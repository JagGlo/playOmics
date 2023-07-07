#' Create and evaluate multiple logistic regression models
#'
#' This function creates a logistic regression model using tidymodels, recipes, and workflows,
#' and evaluates its performance on training and test datasets. The function also logs the experiment
#' information, metrics, and artifacts using the mlflow package. Moreover, it creates an explainer
#' for each model using DALEXtra package.
#'
#' @param train_data A dataframe containing the training dataset.
#' @param test_data A dataframe containing the test dataset.
#' @param n_max Maximum number of variables to be combined into a model. Cannot be lower than 2 (default: 3).
#' @param target A named list with "id_variable" and "target_variable" specifying the ID and target variable names.
#' @param n_prop A numeric value representing the proportion of data used for each resample in the subsampling process (default: 2/3).
#' @return A dataframe containing the performance metrics for the created model on the training and test datasets.
#' @export


create_multiple_models <- function(experiment_name, train_data, test_data, target, n_max = 3, n_cores = parallel::detectCores()/4){

  directory <- paste0(getwd(), '/', experiment_name)

  # if(!dir.exists(directory)){
  #   dir.create(directory)
  # } else {
  #   logger::log_error("Experiment with given name already exists. Please introduce different name")
  #   stop("Experiment with given name already exists. Please introduce different name")
  # }

  # Create directory for mlflow
  # mlflow_directory <- paste(directory, "mlflow", sep = "/")
  # dir.create(mlflow_directory)
  # mlflow::mlflow_set_tracking_uri(mlflow_directory)
  # mlflow::mlflow_set_experiment(
  #     experiment_name = experiment_name
  #   )

  logger::log_info("Experiment {experiment_name} created")

  # prepare train data
  train_data_united <-
    train_data %>%
    reduce(full_join, by = c(target$target_variable, target$id_variable)) %>%
    select(-target$id_variable, -target$target_variable, everything())

  # prepare test data
  test_data_united <-
    prepare_data_for_modelling(test_data, target) %>%
    reduce(full_join, by = c(target$id_variable)) %>%
    select(-target$id_variable, -target$target_variable, everything())

  if(n_max < 2){
    logger::log_error("n_max can't be lower than 2. Please introduce higher number!")
    stop("n_max can't be lower than 2. Please introduce higher number!")
  }

  # Prepare combinations
  chunks <- list()

  for(i in 2:n_max){
    chunks[[i-1]] <- combn(names(train_data_united)[!(names(train_data_united) %in% c(target$target_variable, target$id_variable))], i, simplify = FALSE)
  }

  names(chunks) <- paste0(2:n_max, "-vars models")

  last_run <- Sys.Date()

  # Save data
  save(train_data, test_data, train_data_united, test_data_united, last_run, chunks, file = paste(directory, "data.RData", sep = "/"))

  logger::log_info("Starting modelling experiment")

  n <- 1
  # Model and prune
  lapply(names(chunks), function(chunk_list){

    n <<- n+1
    logger::log_info("There are {length(chunks[[chunk_list]])} {n}-vars models to be constructed")

    # Start parallel computing
    cl <- parallel::makeForkCluster(n_cores)

    # models <- create_and_log_models(chunks[[chunk_list]], data_united, test_data, n_cores = parallel::detectCores()/4, n_prop = 2/3, n_repeats = 50, directory, abbreviations)
    models <-
      chunks[[chunk_list]] %>%
      parallel::parLapplyLB(cl, ., function(single_model){
        # lapply(function(single_model){

          # allow only for non-missing data
          train_data <-
            train_data_united[, c(target$id_variable, single_model, target$target_variable)] %>%
            na.omit()

          test_data <-
            test_data_united[, c(target$id_variable, single_model, target$target_variable)] %>%
            na.omit()

        create_model(train_data, test_data, target, log_experiment = FALSE)

      })

    parallel::stopCluster(cl)
browser()
    # save stats
    save(models, file = paste(directory, paste0(chunk_list, "_stats.RData"), sep = "/"))

    toremove <- na.omit(models[sapply(models,"[[", "train_mcc") <= 0.3])
    toremove <- toremove[lengths(toremove) > 0]
    logger::log_info("There are {length(toremove)} {n}-vars models to be removed")

    if(n < n_max){
      models_tbr <-
        lapply(chunks[[n]], function(x){
          any(
            as.logical(
              lapply(toremove, function(y){
                all(unlist(as.vector(str_split(y$model_name, pattern = "\\s\\+\\s"))) %in% x)
              })
            )
          )
        })

      logger::log_info("There are {sum(unlist(models_tbr))} {n+1}-vars models to be removed")
      logger::log_info("There are {length(chunks[[n]]) - sum(unlist(models_tbr))} {n+1}-vars combinations to be assessed")

      chunks[[n]] <<- chunks[[n]][!unlist(models_tbr)]
    }
  })

  logger::log_info("Modelling experiment ended")

}

