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
#' @param n_prop A numeric value representing the proportion of data used for each resample in the subsampling process (default: 2/3 for subsampling).
#' @param n_repeats A numeric value specifying the number of times to repeat the validation (default: 5).
#' @param log_experiment A logical value indicating whether to log the experiment details and results. Default is TRUE.
#' @param explain A logical value indicating whether to create model's explainer using DALEX (default: TRUE).
#' @param directory A character string indicating the directory where experiment logs and models should be saved. Default is the current working directory.
#'
#' @return A tibble containing details of the model, training metrics, and test metrics (if test data provided).
#' In case of an error during processing, it returns a tibble with only the 'model_id'.
#' @export

create_model <- function(
    train_data, test_data, target,
    validation_method = "cv", n_prop = 2 / 3, n_repeats = 5,
    log_experiment = TRUE,
    explain = TRUE,
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
          # Log parameters
          list(
            model_name = model_name,
            groups = n_groups,
            validation_method = validation_method,
            n_prop = n_prop,
            n_repeats = n_repeats
          ) %>%
            jsonlite::write_json(file.path(model_dir, "params.json"),
              pretty = TRUE, auto_unbox = TRUE
            )
        }

        # Define model
        data_recipe <-
          recipes::recipe(train_data) %>%
          recipes::update_role(target$target_variable, new_role = "outcome") %>%
          recipes::update_role(recipes::has_role(NA), new_role = "predictor")

        # Model specification
        model_spec <-
          parsnip::logistic_reg() %>% # model type
          parsnip::set_engine(engine = "glm") %>% # model engine
          parsnip::set_mode("classification") # model mode

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
              v = n_repeats,
              strata = target$target_variable
            )
        }

        # Define workflow
        model_wflow <-
          workflows::workflow() %>%
          workflows::add_recipe(data_recipe) %>%
          workflows::add_model(model_spec)

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

        # logger::log_eval(
        # model_res <-
        #   model_wflow %>%
        #   tune::fit_resamples(
        #     resamples = resample,
        #     metrics = custom_metrics,
        #     control = tune::control_resamples(
        #       save_pred = TRUE, allow_par = F
        #     )
        #   ),
        # multiline = TRUE, level = logger::WARN
        # )

        train_results <-
          model_res %>%
          tune::collect_metrics(summarize = T) %>%
          select(.metric, mean) %>%
          pivot_wider(names_from = .metric, values_from = mean) %>%
          rename_with(~ paste0("train_", .x))

        train_results <- bind_cols(n_groups, train_results)

        # fit model on entire training data
        fitted_model <- fit(model_wflow, train_data)

        if (nrow(test_data) > 0) {
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
              estimate = .pred_class
            ) %>%
            # bind_rows(
            #   yardstick::roc_auc(test_results,
            #                  truth =  !!target$target_variable,
            #                  estimate = !!paste0(".pred_", target$positive_class)
            #   )
            # ) %>%
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

        if (log_experiment) {
          # save raw data
          saveRDS(
            train_data, file.path(model_dir, "train_data.Rds")
          )
          saveRDS(
            test_data, file.path(model_dir, "test_data.Rds")
          )
          logger::log_info("Model '{model_name}' ended")
        }

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
