log_metrics <- function(metrics, run_id) {
  parameter_names <- names(metrics)
  parameter_values <- lapply(metrics, rlang::get_expr)
  for (i in seq_along(metrics)) {
    parameter_name <- parameter_names[[i]]
    parameter_value <- parameter_values[[i]]
    if (!is.null(parameter_value)) {
      mlflow_log_metric(parameter_name, parameter_value, run_id = run_id)
    }
  }
  metrics
}

#'
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

create_multiple_models <- function(data_filtered, experiment_name, n_cores, n_prop = 2/3, n_repeats = 50){

  #if(!directory exist)
  directory <- paste0(getwd(), '/', experiment_name)
  dir.create(directory)
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
  # browser()

  mlflow_directory <- paste(directory, "mlflow", sep = "/")
  dir.create(mlflow_directory)
  mlflow_set_tracking_uri(mlflow_directory)
  mlflow_set_experiment(
    experiment_name = experiment_name
  )

  # opts <- furrr::furrr_options(
  #   seed = TRUE
  # )

  # nCores <- detectCores()/4
  # future::plan(future::multisession)
  # cl <- parallel::makeCluster(n_cores)
  # doParallel::registerDoParallel(cl)
  cl <- parallel::makeForkCluster(n_cores)
  tictoc::tic()
  models <-
    chunks %>%
    # furrr::future_map(function(single_model) {
    parallel::parLapplyLB(cl, ., function(single_model){
      # lapply(function(single_model) {


      # browser()
      set.seed(sample.int(1:100000, n = 1))
      data <-
        data_united[, single_model] %>%
        na.omit()

      model_name <- paste(colnames(data[-ncol(data)]), collapse = " + ")
      tryCatch({

        mlflow_start_run(nested = T)
        run = mlflow_get_run()
        # log model name
        mlflow_log_param("model_name", model_name, run_id = run$run_uuid)

        n_groups <-
          data %>%
          group_by(!!rlang::sym(target$target_variable)) %>%
          count() %>%
          spread(target$target_variable, n) %>%
          rename_with( ~ paste0("n_", .x))

        lapply(colnames(n_groups), function(i) mlflow_log_param(i, pull(n_groups[i]),  run_id = run$run_uuid))

        # Define model
        data_recipe <-
          recipe(data) %>%
          update_role(target$target_variable, new_role = "outcome") %>%
          # update_role(target$id_variable, new_role = "id variable") %>%
          update_role(has_role(NA), new_role = "predictor") %>%
          step_corr(all_predictors(), threshold = 0.7, method = "spearman") %>%
          step_zv(all_predictors())
        # step_normalize(all_predictors()) #should the data be normalized?

        log_spec <- # your model specification
          logistic_reg() %>%  # model type
          set_engine(engine = "glm") %>%  # model engine
          set_mode("classification")   # model mode

        # Subsampling
        # if else?
        resample <- mc_cv(data, prop = n_prop, times = n_repeats, strata = target$target_variable)

        mlflow_log_param("resampling strategy", "mc_cv")
        mlflow_log_param("prop", n_prop)
        mlflow_log_param("times", n_repeats)

        # Define workflow
        log_wflow <- # new workflow object
          workflow() %>% # use workflow function
          add_recipe(data_recipe) %>%   # use the recipe
          add_model(log_spec)

        log_res <-
          log_wflow %>%
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
          collect_metrics(summarize = T) %>%
          select(.metric, mean) %>%
          spread(.metric, mean)

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
        mlflow_save_model(crated_model, artifact_dir)
        mlflow_log_artifact(artifact_dir, artifact_path = "model")

        # save raw data
        data_dir <-paste0(artifact_dir,"/data.rds")
        ## temporary data save
        saveRDS(data, file = data_dir)
        ## log data to experiment
        mlflow_log_artifact(data_dir,  run_id = run$run_uuid)
        #remove directiry
        unlink(artifact_dir, recursive=TRUE)
        # end mlflow run
        mlflow_end_run()

        # return metrics
        return(results)
        # })
      }, error = function(error_condition) {
        return(NULL)
      })
    })
  tictoc::toc()

  # save models' stats
  save(models,file = paste(directory, "models_stats.RData", sep = "/"))

  # Multicore off
  parallel::stopCluster(cl)
  rm(cl)

  return(models)

}
