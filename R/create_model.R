log_metrics <- function(metrics, run_id) {
  parameter_names <- names(metrics)
  parameter_values <- lapply(metrics, rlang::get_expr)
  for (i in seq_along(metrics)) {
    parameter_name <- parameter_names[[i]]
    parameter_value <- parameter_values[[i]]
    if (!is.null(parameter_value)) {
      mlflow::mlflow_log_metric(parameter_name, parameter_value, run_id = run_id)
    }
  }
  metrics
}

#' Create and evaluate a logistic regression model
#'
#' This function creates a logistic regression model using tidymodels, recipes, and workflows,
#' and evaluates its performance on training and test datasets. The function also logs the experiment
#' information, metrics, and artifacts using the mlflow package (optional).
#'
#' @param train_data A dataframe containing the training dataset.
#' @param test_data A dataframe containing the test dataset.
#' @param target A named list with "id_variable" and "target_variable" specifying the ID and target variable names.
#' @param n_prop A numeric value representing the proportion of data used for each resample in the subsampling process (default: 2/3).
#' @param n_repeats A numeric value specifying the number of times to repeat the subsampling (default: 50).
#' @param log_experiment A logical value indicating whether to log the experiment information using mlflow (default: TRUE).
#' @param explain A logical value indicating whether to create model's explainer using DALEX (default: TRUE).
#'
#' @return A dataframe containing the performance metrics for the created model on the training and test datasets.
#' @export

create_model <- function(train_data, test_data, target, n_prop = 2 / 3, n_repeats = 50, log_experiment = TRUE, explain = TRUE) {

  model_name <- paste(colnames(train_data)[!(colnames(train_data) %in% c(target$id_variable, target$target_variable))], collapse = " + ")

  if (log_experiment) {
    mlflow::mlflow_start_run(nested = T)
    run <- mlflow::mlflow_get_run()
    # log model name
    mlflow::mlflow_log_param("model_name", model_name, run_id = run$run_uuid)
  }

  n_groups <-
    train_data %>%
    group_by(!!rlang::sym(target$target_variable)) %>%
    count() %>%
    spread(target$target_variable, n) %>%
    rename_with(~ paste0("n_", .x))

  if (log_experiment) {
    lapply(colnames(n_groups), function(i) mlflow::mlflow_log_param(i, pull(n_groups[i]), run_id = run$run_uuid))
  }

  # Define model
  data_recipe <-
    recipes::recipe(train_data) %>%
    recipes::update_role(target$id_variable, new_role = "id variable") %>%
    recipes::update_role(target$target_variable, new_role = "outcome") %>%
    recipes::update_role(recipes::has_role(NA), new_role = "predictor")

  model_spec <- # your model specification
    parsnip::logistic_reg() %>% # model type
    parsnip::set_engine(engine = "glm") %>% # model engine
    parsnip::set_mode("classification") # model mode

  # Subsampling
  # resample <- rsample::mc_cv(train_data, prop = n_prop, times = n_repeats, strata = target$target_variable)
  # to try one day
  resample <-
    rsample::vfold_cv(train_data,
                      v = 3,
                      strata = target$target_variable
    )

  if (log_experiment) {
    mlflow::mlflow_log_param("resampling strategy", "mc_cv")
    mlflow::mlflow_log_param("prop", n_prop)
    mlflow::mlflow_log_param("times", n_repeats)
  }

  # Define workflow
  model_wflow <- # new workflow object
    workflows::workflow() %>% # use workflow function
    workflows::add_recipe(data_recipe) %>% # use the recipe
    workflows::add_model(model_spec)

  custom_metrics <-
    yardstick::metric_set(
      yardstick::mcc,
      yardstick::recall,
      yardstick::precision,
      yardstick::accuracy,
      # yardstick::roc_auc,
      yardstick::sens,
      yardstick::spec,
      yardstick::ppv,
      yardstick::npv,
      # yardstick::pr_auc,
      yardstick::f_meas,
    )

  model_res <-
    model_wflow %>%
    tune::fit_resamples(
      resamples = resample,
      metrics = custom_metrics,
      control = tune::control_resamples(
        save_pred = TRUE, allow_par = F
      )
    )

  train_results <-
    model_res %>%
    tune::collect_metrics(summarize = T) %>%
    select(.metric, mean) %>%
    spread(.metric, mean) %>%
    rename_with(~ paste0("train_", .x))

  train_results <- bind_cols(n_groups, train_results)

  # fit model on entire training data
  fitted_model <- fit(model_wflow, train_data)

  if (nrow(test_data) > 0) {
    ## get predictions
    predicted <- predict(fitted_model,
      new_data = test_data,
      type = "class"
    )

    test_results <- test_data %>%
      select(target$target_variable) %>%
      bind_cols(predicted)

    test_metrics <-
      custom_metrics(test_results,
        truth = !!target$target_variable,
        estimate = .pred_class
      ) %>%
      mutate(`.metric` = paste0("test_", `.metric`)) %>%
      select(-`.estimator`) %>%
      spread(`.metric`, `.estimate`)

    test_metrics <-
      bind_cols(
        test_data %>%
          group_by(!!rlang::sym(target$target_variable)) %>%
          count() %>%
          ungroup() %>%
          spread(target$target_variable, n) %>%
          rename_with(~ paste0("test_n_", .x)),
        test_results %>%
          group_by(!!rlang::sym(target$target_variable)) %>%
          count(.pred_class) %>%
          ungroup() %>%
          mutate(guessed = case_when(
            !!rlang::sym(target$target_variable) == `.pred_class` ~ paste0("n_test_", !!rlang::sym(target$target_variable), "_guessed_correctly"),
            TRUE ~ paste0("test_n_", !!rlang::sym(target$target_variable), "_guessed_incorrectly")
          )) %>%
          select(guessed, n) %>%
          spread(guessed, n),
        test_metrics
      )

    results <-
      bind_cols(
        tibble(model_name = model_name),
        train_results,
        test_metrics
      )
  } else {
    results <- train_results
  }

  if (log_experiment) {
    # log metrics
    log_metrics(results, run_id = run$run_uuid)

    # save model
    crated_model <- carrier::crate(
      function(data_to_fit) {
        dplyr::bind_cols(
          workflows:::predict.workflow(fitted_model, as.data.frame(data_to_fit), type = "class"),
          workflows:::predict.workflow(fitted_model, as.data.frame(data_to_fit), type = "prob")
        )
      },
      fitted_model = fitted_model
    )

    # create temporary folder
    dir.create(paste(directory, run$run_uuid, sep = "/"))
    artifact_dir <- paste(directory, run$run_uuid, sep = "/")
    mlflow::mlflow_save_model(crated_model, artifact_dir)
    mlflow::mlflow_log_artifact(artifact_dir, artifact_path = "model")

    rm(crated_model)

    if (explain) {
      # explain prediction
      explainer_lr <-
        DALEXtra::explain_tidymodels(
          fitted_model,
          data = data,
          y = target$target_variable,
          label = "lr",
          verbose = FALSE
        )

      crated_explainer <- carrier::crate(
        function(data_to_fit) {
          DALEX::predict_parts(
            explainer = explainer_lr,
            new_observation = data_to_fit,
            type = "shap"
          )
        },
        explainer_lr = explainer_lr
      )

      mlflow::mlflow_save_model(crated_explainer, artifact_dir)
      mlflow::mlflow_log_artifact(artifact_dir, artifact_path = "explainer")
    }

    # save raw data
    data_dir <- paste0(artifact_dir, "/data.rds")
    ## temporary data save
    saveRDS(data, file = data_dir)
    ## log data to experiment
    mlflow::mlflow_log_artifact(data_dir, run_id = run$run_uuid)
    # remove directiry
    unlink(artifact_dir, recursive = TRUE)
    # end mlflow run
    mlflow::mlflow_end_run()
  }

  # return metrics
  return(results)
}
