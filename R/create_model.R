#' Create and evaluate a logistic regression model
#'
#' This function creates a logistic regression model using tidymodels, recipes, and workflows,
#' and evaluates its performance on training and test datasets. The function also logs the experiment
#' information, metrics, and artifacts (optional) and provides explanation with DALEX package.
#'
#' @param train_data A dataframe containing the training dataset.
#' @param test_data A dataframe containing the test dataset.
#' @param target A named list with "id_variable" and "target_variable" specifying the ID and target variable names.
#' @param validation_method A character string specifying the validation method to be used; either "subsampling" or "cv" (default: "cv").
#' @param n_fold A numeric value representing number of fold for k-fold cross-validation (default: 5).
#' @param n_prop A numeric value representing the proportion of data used for each resample in the subsampling process (e.g. 2/3 for subsampling).
#' @param n_repeats A numeric value specifying the number of times to repeat the validation (default: 5).
#' @param log_experiment A logical value indicating whether to log the experiment details and results. Default is TRUE.
#' @param explain A logical value indicating whether to create model's explainer using DALEX (default: TRUE).
#' @param add_weights A logical value indicating whether to add weights to the training data based on the class imbalance. Default is FALSE.
#' @param directory A character string indicating the directory where experiment logs and models should be saved. Default is the current working directory.
#'
#' @importFrom DALEX explain
#' @return A tibble containing details of the model, training metrics, and test metrics (if test data provided).
#' In case of an error during processing, it returns a tibble with only the 'model_id'.
#' @export

create_model <- function(
    train_data, test_data, target,
    validation_method = "cv",
    n_fold = 5,
    n_prop = 2 / 3,
    model_type = "LR",
    n_repeats = 5,
    log_experiment = TRUE,
    explain = TRUE,
    add_weights = FALSE,
    directory = getwd()) {
  model_name <- paste(colnames(train_data)[!(colnames(train_data) %in% c(target$target_variable))], collapse = " + ")
  # Model names are unique within an analysis
  model_id <- digest::digest(model_name)

  results <-
    tryCatch(
      {
        if (log_experiment) {
          model_dir <- file.path(directory, model_id)
          dir.create(model_dir)

          # Create logger
          log_path <- paste(model_dir, "model_logs.json", sep = "/")
          logger::log_appender(logger::appender_file(log_path))
          logger::log_threshold(logger::DEBUG)
          logger::log_layout(logger::layout_json())

          logger::log_info("Model '{model_name}' started")
        }

        if (log_experiment) {
          model_details <-
            tibble(
              model_name = model_name,
              model_id = model_id,
              model_dir = model_dir
            )
        } else {
          model_details <-
            tibble(
              model_name = model_name,
              model_id = model_id,
              model_dir = NULL
            )
        }

        n_groups <-
          train_data %>%
          count(!!rlang::sym(target$target_variable)) %>%
          pivot_wider(
            names_from = all_of(target$target_variable), names_prefix = "n_", values_from = n
          ) %>%
          lapply(identity)

        if (log_experiment) {
          # save raw data
          saveRDS(
            train_data, file.path(model_dir, "train_data.Rds")
          )
          saveRDS(
            test_data, file.path(model_dir, "test_data.Rds")
          )

          # Log parameters
          list(
            model_name = model_name,
            groups = n_groups,
            validation_method = validation_method,
            n_prop = n_prop,
            n_repeats = n_repeats,
            add_weights = add_weights
          ) %>%
            jsonlite::write_json(file.path(model_dir, "params.json"),
                                 pretty = TRUE, auto_unbox = TRUE
            )
        }

        if (add_weights) {
          counts <- train_data %>%
            group_by(!!rlang::sym(target$target_variable)) %>%
            count() %>%
            arrange(desc(n))
          proportion <- floor(counts$n[1] / counts$n[2])
          train_data <-
            train_data %>%
            mutate(
              sample_weight = ifelse(!!rlang::sym(target$target_variable) == as.character(counts[[1]][2]), proportion, 1),
              sample_weight = parsnip::importance_weights(sample_weight)
            )
        }

        # Define model
        data_recipe <-
          recipes::recipe(train_data) %>%
          recipes::update_role(target$target_variable, new_role = "outcome") %>%
          recipes::update_role(recipes::has_role(NA), new_role = "predictor")

        # Model specification
        if(model_type == "LR"){
        model_spec <-
          parsnip::logistic_reg() %>% # model type
          parsnip::set_engine(engine = "glm") %>% # model engine
          parsnip::set_mode("classification") # model mode
        }

        # Validation
        if (validation_method == "subsampling") {
          resample <-
            rsample::mc_cv(train_data,
                           prop = n_prop,
                           times = n_repeats,
                           strata = target$target_variable
            )
        } else if (validation_method == "cv") {
          resample <-
            rsample::vfold_cv(train_data,
                              v = n_fold,
                              repeats = n_repeats,
                              strata = target$target_variable
            )
        }

        # Define workflow
        model_wflow <-
          workflows::workflow() %>%
          workflows::add_recipe(data_recipe) %>%
          workflows::add_model(model_spec)

        if (add_weights) {
          model_wflow <-
            model_wflow %>%
            workflows::add_case_weights(sample_weight)
        }

        custom_metrics <-
          yardstick::metric_set(
            yardstick::mcc,
            yardstick::recall,
            yardstick::precision,
            yardstick::accuracy,
            yardstick::roc_auc,
            yardstick::sens,
            yardstick::spec,
            yardstick::ppv,
            yardstick::npv,
            yardstick::pr_auc,
            yardstick::f_meas
          )

        model_res <- model_wflow %>%
          tune::fit_resamples(
            resamples = resample,
            metrics = custom_metrics,
            na_rm = TRUE,
            control = tune::control_resamples(
              save_pred = TRUE, allow_par = F
            )
          )

        if (log_experiment) {
          logger::log_eval(
            model_res,
            multiline = TRUE, level = logger::WARN
          )
        }

        train_results <-
          model_res %>%
          tune::collect_metrics(summarize = T) %>%
          select(.metric, mean) %>%
          pivot_wider(names_from = .metric, values_from = mean) %>%
          rename_with(~ paste0("train_", .x))

        conf_matrix <-
          model_res %>%
          tune::collect_predictions(summarize = T) %>%
          yardstick::conf_mat(truth = !!rlang::sym(target$target_variable), estimate = .pred_class) %>%
          .$table %>%
          reshape2::melt() %>%
          mutate(
            Metric = ifelse(Truth == Prediction, "guessed_correctly", "guessed_incorrectly"),
            Variable_Name = paste0("train_n_", Truth, "_", Metric)
          ) %>%
          select(-Prediction, -Truth, -Metric) %>%
          pivot_wider(names_from = Variable_Name, values_from = value)

        ci <-
          model_res |>
          tune::collect_predictions(summarize = T)  |>
          summarise(sum = sum(.pred_class == !!rlang::sym(target$target_variable)), n = n()) |>
          mutate(
            pp = sum/n,
            ci_lower = binom::binom.bayes(sum, n, conf.level = 0.95, type = "central")$lower,
            ci_high = binom::binom.bayes(sum, n, conf.level = 0.95, type = "central")$upper) %>%
          select(-sum, -n)

        train_results <- bind_cols(n_groups, train_results, conf_matrix, ci)

        # fit model on entire training data
        fitted_model <- fit(model_wflow, train_data)

        # log the model coefficients
        if (log_experiment) {
          broom::tidy(fitted_model) %>%
            split(., .$term) %>%
            jsonlite::write_json(file.path(model_dir, "model_coef.json"),
                                 pretty = TRUE, auto_unbox = TRUE
            )

          #log predictions on training data
          fitted_model %>%
            parsnip::augment(new_data = train_data) %>%
            saveRDS(
              file.path(model_dir, "predictions_training.Rds")
            )
        }

        if (!is.null(test_data)) {
          ## get predictions
          test_results <-
            predict(fitted_model, new_data = test_data) %>%
            bind_cols(predict(fitted_model, new_data = test_data, type = "prob")) %>%
            bind_cols(test_data %>%
                        select(target$target_variable))

          custom_metrics <-
            yardstick::metric_set(
              yardstick::mcc,
              yardstick::recall,
              yardstick::precision,
              yardstick::accuracy,
              yardstick::sens,
              yardstick::spec,
              yardstick::ppv,
              yardstick::npv,
              yardstick::f_meas
            )

          test_metrics <-
            custom_metrics(test_results,
                           truth = !!target$target_variable,
                           estimate = .pred_class,
                           na_rm = T
            ) %>%
            bind_rows(
              yardstick::roc_auc(test_results,
                                 truth = !!target$target_variable,
                                 !!paste0(".pred_", target$positive_class),
                                 na_rm = T
              )
            ) %>%
            mutate(`.metric` = paste0("test_", `.metric`)) %>%
            select(-`.estimator`) %>%
            pivot_wider(names_from = `.metric`, values_from = `.estimate`)

          test_metrics <-
            bind_cols(
              test_data %>%
                group_by(!!rlang::sym(target$target_variable)) %>%
                count() %>%
                ungroup() %>%
                pivot_wider(names_from = !!rlang::sym(target$target_variable), values_from = n) %>%
                rename_with(~ paste0("test_n_", .x)),
              test_results %>%
                group_by(!!rlang::sym(target$target_variable)) %>%
                count(.pred_class) %>%
                ungroup() %>%
                mutate(guessed = case_when(
                  !!rlang::sym(target$target_variable) == `.pred_class` ~ paste0("test_n_", !!rlang::sym(target$target_variable), "_guessed_correctly"),
                  TRUE ~ paste0("test_n_", !!rlang::sym(target$target_variable), "_guessed_incorrectly")
                )) %>%
                select(guessed, n) %>%
                pivot_wider(names_from = guessed, values_from = n),
              test_metrics
            )

          results <-
            bind_cols(
              model_details,
              train_results,
              test_metrics
            )
        } else {
          results <-
            bind_cols(
              model_details,
              train_results
            )
        }

        if (log_experiment) {
          # log metrics
          jsonlite::write_json(
            lapply(results, identity),
            file.path(model_dir, "metrics.json"),
            pretty = TRUE, auto_unbox = TRUE
          )

          # log predictions on test data
            saveRDS(
              test_results, file.path(model_dir, "predictions_test.Rds")
            )

          # save model
          saveRDS(
            carrier::crate(
              function(data_to_fit) {
                dplyr::bind_cols(
                  workflows:::predict.workflow(fitted_model, as.data.frame(data_to_fit), type = "class"),
                  workflows:::predict.workflow(fitted_model, as.data.frame(data_to_fit), type = "prob")
                )
              },
              fitted_model = fitted_model
            ),
            file.path(model_dir, "model.Rds")
          )
        }

        if (explain) {
          # explain prediction
          explainer_lr <-
            DALEXtra::explain_tidymodels(
              fitted_model,
              data = train_data,
              y = target$target_variable,
              label = "lr",
              verbose = FALSE
            )
        }

        if (all(log_experiment, explain)) {
          saveRDS(
            carrier::crate(
              function(data_to_fit) {
                DALEX::predict_parts(
                  explainer = explainer_lr,
                  new_observation = data_to_fit,
                  type = "shap"
                )
              },
              explainer_lr = explainer_lr
            ),
            file.path(model_dir, "explainer.Rds")
          )
        }


        logger::log_info("Model '{model_name}' ended")

        # return metrics
        results
      },
      error = function(error_condition) {
        # message(error_condition)
        if (log_experiment) {
          logger::log_error(as.character(error_condition))
        }
        return(tibble(model_id = model_id))
      }
    )

  logger::log_appender(logger::appender_console)
  return(results)
}
