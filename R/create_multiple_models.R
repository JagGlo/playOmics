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

#' Create small models
#'
#'
#'
#' @param
#
#' @return
#'
#' @examples
#'
#' @export

create_multiple_models <- function(data_filtered, experiment_name, n_cores = detectCores()/4, n_prop = 2/3, n_repeats = 50){

  directory <- paste0(getwd(), '/', experiment_name)
  if(!dir.exists(directory)){
  dir.create(directory)
  } else {
  logger::log_error("Experiment with given name already exists. Please introduce different name")
  stop("Experiment with given name already exists. Please introduce different name")
  }

  last_run <- Sys.Date()

  data_united <-
    data_filtered %>%
    reduce(full_join, by = c(target$target_variable, target$id_variable)) %>%
    select(-target$id_variable) %>%
    select(-target$target_variable, everything())

  chunks <-
    c(
      combn(ncol(data_united) - 1, 2, simplify = FALSE),
      combn(ncol(data_united) - 1, 3, simplify = FALSE)
    ) %>%
    lapply(function(y) {
      c(y, ncol(data_united))
    }) %>%
    sample()

  # Save
  save(data_united, directory, chunks, last_run, file = paste(directory, "raw_data.RData", sep = "/"))

  mlflow_directory <- paste(directory, "mlflow", sep = "/")
  dir.create(mlflow_directory)
  mlflow::mlflow_set_tracking_uri(mlflow_directory)
  mlflow::mlflow_set_experiment(
    experiment_name = experiment_name
  )
  logger::log_info("Experiment {experiment_name} created")
  cl <- parallel::makeForkCluster(n_cores)

  logger::log_info("Starting modelling experiment")
  models <-
    chunks[1:4] %>%
    parallel::parLapplyLB(cl, ., function(single_model){

      set.seed(sample(1:100000, size = 1))
      data <-
        data_united[, single_model] %>%
        na.omit()

      model_name <- paste(colnames(data[-ncol(data)]), collapse = " + ")

      tryCatch({

        mlflow::mlflow_start_run(nested = T)
        run = mlflow::mlflow_get_run()
        # log model name
        mlflow::mlflow_log_param("model_name", model_name, run_id = run$run_uuid)

        n_groups <-
          data %>%
          group_by(!!rlang::sym(target$target_variable)) %>%
          count() %>%
          spread(target$target_variable, n) %>%
          rename_with( ~ paste0("n_", .x))

        lapply(colnames(n_groups), function(i) mlflow::mlflow_log_param(i, pull(n_groups[i]),  run_id = run$run_uuid))

        # Define model
        data_recipe <-
          recipes::recipe(data) %>%
          recipes::update_role(target$target_variable, new_role = "outcome") %>%
          recipes::update_role(recipes::has_role(NA), new_role = "predictor")
        # step_normalize(all_predictors()) #should the data be normalized?

        model_spec <- # your model specification
          parsnip::logistic_reg() %>%  # model type
          parsnip::set_engine(engine = "glm") %>%  # model engine
          parsnip::set_mode("classification")   # model mode

        # Subsampling
        resample <- rsample::mc_cv(data, prop = n_prop, times = n_repeats, strata = target$target_variable)

        mlflow::mlflow_log_param("resampling strategy", "mc_cv")
        mlflow::mlflow_log_param("prop", n_prop)
        mlflow::mlflow_log_param("times", n_repeats)

        # Define workflow
        model_wflow <- # new workflow object
          workflows::workflow() %>% # use workflow function
          workflows::add_recipe(data_recipe) %>%   # use the recipe
          workflows::add_model(model_spec)

        model_res <-
          model_wflow %>%
          tune::fit_resamples(
            resamples = resample,
            metrics = metric_set(
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
              yardstick::f_meas,
            ),
            control = tune::control_resamples(
              save_pred = TRUE, allow_par = F)
          )

        results <-
          log_res %>%
          tune::collect_metrics(summarize = T) %>%
          tune::select(.metric, mean) %>%
          tune::spread(.metric, mean)

        log_metrics(results,  run_id = run$run_uuid)

        # create final model on complete data
        fitted_model <- fit(log_wflow, data)

        crated_model <- carrier::crate(
          function(data_to_fit) {
            dplyr::bind_cols(
              workflows:::predict.workflow(fitted_model, as.data.frame(data_to_fit), type = "class"),
              workflows:::predict.workflow(fitted_model, as.data.frame(data_to_fit), type = "prob")
            )
          },
          fitted_model = fitted_model
        )

        #create temporary folder
        dir.create(paste(directory,run$run_uuid, sep = "/"))
        artifact_dir <- paste(directory,run$run_uuid, sep = "/")
        mlflow::mlflow_save_model(crated_model, artifact_dir)
        mlflow::mlflow_log_artifact(artifact_dir, artifact_path = "model")

        # save raw data
        data_dir <-paste0(artifact_dir,"/data.rds")
        ## temporary data save
        saveRDS(data, file = data_dir)
        ## log data to experiment
        mlflow::mlflow_log_artifact(data_dir,  run_id = run$run_uuid)
        #remove directiry
        unlink(artifact_dir, recursive=TRUE)
        # end mlflow run
        mlflow::mlflow_end_run()

        # return metrics
        return(results)
        # })
      }, error = function(error_condition) {
        return(NULL)
      })
    })
  logger::log_info("Modelling experiment ended")

  # save models' stats
  save(models,file = paste(directory, "models_stats.RData", sep = "/"))

  # Multicore off
  parallel::stopCluster(cl)
  rm(cl)

  return(models)

}
