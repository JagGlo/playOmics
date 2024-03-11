# Function to log error message and stop execution if a condition fails
stopIfConditionFails <- function(condition, message) {
  if (!condition) {
    logger::log_error(message)
    stop(message)
  }
}

# prepare test data
prepare_test_data <- function(test_data, target) {
  # Attempt the initial method to create test_data_united
  result <- tryCatch(
    {
      test_data %>%
        reduce(full_join, by = c(target$id_variable)) %>%
        select(-target$id_variable, -target$target_variable, everything())
    },
    # If there's an error with the initial method, this alternative method will be used
    error = function(e) {
      test_data %>%
        reduce(full_join, by = c(target$target_variable, target$id_variable)) %>%
        select(-target$id_variable, -target$target_variable, everything())
    }
  )

  return(result)
}

#' Create multiple models for given datasets
#'
#' This function iterates over combinations of predictor variables in the train data to build
#' multiple models.
#'
#' @details
#' When you dive into using the \emph{create_multiple_models()} function, you're not just making one model — you're orchestrating a symphony of models,
#' each one trying its best to shed light on your data. Among these models, however, not all are equal. Some might not capture
#' the patterns in your data well, and this is where the trim_models, trim_metric, and trim_threshold parameters can be handy.
#'
#' \itemize{
#'   \item \strong{trim_models}: This parameter acts as a switch or flag.
#' If set to TRUE, the function will employ the mechanism to remove or "trim" models based on the specified metric and threshold (detailed below). If set to FALSE, all models will be kept, regardless of their performance.
#'  \item \strong{trim_metric}: This specifies the metric used to evaluate model performance.
#' In the provided function, "train_mcc" (which likely stands for Matthews Correlation Coefficient on the training data) is the default metric. However, users can potentially specify other metrics if they want models to be evaluated and potentially trimmed based on different performance criteria.
#' \item \strong{trim_threshold}: This is the critical value, based on which models will be evaluated for potential trimming.
#' If the performance metric of a model (as specified by trim_metric) is below this threshold, and trim_models is set to TRUE, that model will be removed or "trimmed."
#' The default value provided is 0.3. So, for instance, with default settings, any model with a "train_mcc" less than 0.3 would be removed.
#' }
#' After constructing "n"-variables models for each variable combination, the function checks if the metric (e.g., "train_mcc") for the component of this model in the "n-1"-variable run is
#' below the specified threshold. If it is and if trim_models is TRUE, those underperforming models are removed. This ensures that subsequent
#' combinations of variables don't waste time considering models that are deemed unsatisfactory based on prior results.
#'
#' @param experiment_name A character string denoting the name of the experiment.
#' @param train_data A list containing the training data.
#' @param test_data A list containing the testing data.
#' @param target A list with at least two elements: 'target_variable' (name of the dependent variable) and 'id_variable' (name of the identifier variable); see more under \link[playOmics]{define_target}. 
#' @param n_max An integer specifying the minimum number of predictor variables to consider in combinations. Default is 2.
#' @param n_max An integer specifying the maximum number of predictor variables to consider in combinations. Default is 3.
#' @param n_cores An integer specifying the number of CPU cores to use in parallel processing. Default is one fourth of the available cores.
#' @param directory A character string specifying the path to the working directory. Default is the current working directory.
#' @param trim_models A logical indicating whether to trim models based on a given metric and threshold. Default is TRUE.
#' @param trim_metric A character string specifying the metric to use for trimming models. Default is 'train_mcc'.
#' @param trim_threshold A numeric value specifying the threshold below which models should be trimmed. Default is 0.3.
#' @param validation_method A character string specifying the validation method to be used; either "subsampling" or "cv"; see more under \link[playOmics]{create_model}.
#' @param n_prop A numeric value representing the proportion of data used for each resample in the subsampling process (default: 2/3); see more under \link[playOmics]{create_model}.
#' @param n_repeats A numeric value specifying the number of times to repeat the validation (default: 10); see more under \link[playOmics]{create_model}.
#' @param log_experiment A logical value indicating whether to log the experiment details and results. Default is TRUE; see more under \link[playOmics]{create_model}.
#' @param explain A logical value indicating whether to create model's explainer using DALEX (default: TRUE);see more under \link[playOmics]{create_model}.

#'
#' @return A list of results from the modeling process.
#'
#' @examples
#' # Assuming appropriate data is available
#' \dontrun{
#' # results <-
#' create_multiple_models(
#'   experiment_name = "my_experiment_name",
#'   train_data = train_data_prepared,
#'   test_data = test_data_prepared,
#'   target = my_target,
#'   n_max = 3,
#'   trim_models = TRUE,
#'   trim_metric = "train_mcc",
#'   trim_threshold = 0.3,
#'   # single model settings
#'   validation_method = "cv",
#'   n_prop = NULL,
#'   n_repeats = 5,
#'   log_experiment = TRUE,
#'   explain = TRUE,
#'   # configuration
#'   n_cores = 5,
#'   directory = getwd()
#' )
#'}
#' @export


create_multiple_models <- function(experiment_name,
                                   train_data,
                                   test_data,
                                   target,
                                   n_min = 2,
                                   n_max = 3,
                                   trim_models = TRUE,
                                   trim_metric = "train_mcc",
                                   trim_threshold = 0.3,
                                   # single model settings
                                   validation_method = "cv",
                                   n_prop = 2 / 3,
                                   n_repeats = 5,
                                   log_experiment = TRUE,
                                   explain = TRUE,
                                   add_weights = FALSE,
                                   # configuration
                                   n_cores = parallel::detectCores() / 4,
                                   directory = getwd()) {
  # Set the directory
  directory <- file.path(directory, experiment_name)

  # Check if the directory already exists and stop if it does
  stopIfConditionFails(!dir.exists(directory), "Experiment with given name already exists. Please introduce different name")
  # Create directory if it doesn't exist
  dir.create(directory)

  # Log that the experiment was created
  logger::log_info(sprintf("Experiment %s created", experiment_name))

  # prepare train data
  train_data_united <-
    train_data %>%
    reduce(full_join, by = c(target$target_variable, target$id_variable)) %>%
    select(-target$id_variable, -target$target_variable, everything())

  logger::log_info("Number of features in the final set: {ncol(train_data_united)-2} features.")

  # prepare test data
  test_data_united <- prepare_test_data(test_data, target)

  # Ensure n_max is at least 2
  stopIfConditionFails(n_max >= 2, "n_max can't be lower than 2. Please introduce higher number!")

  # Prepare variables combinations
  chunks <- list()

  for (i in n_min:n_max) {
    chunks[[i - 1]] <- combn(names(train_data_united)[!(names(train_data_united) %in% c(target$target_variable, target$id_variable))], i, simplify = FALSE)
  }

  names(chunks) <- paste0(n_min:n_max, "-vars models")

  experiment_timestamp <- Sys.Date()

  # Save train_data
  save(train_data, file = paste(directory, "train_data.RData", sep = "/"))

  # Save test_data
  save(test_data, file = paste(directory, "test_data.RData", sep = "/"))

  # Save train_data_united
  save(train_data_united, file = paste(directory, "train_data_united.RData", sep = "/"))

  # Save test_data_united
  save(test_data_united, file = paste(directory, "test_data_united.RData", sep = "/"))

  # Save experiment_timestamp
  save(experiment_timestamp, file = paste(directory, "experiment_timestamp.RData", sep = "/"))

  # Save chunks
  save(chunks, file = paste(directory, "chunks.RData", sep = "/"))

  # Log the start of the experiment
  logger::log_info("Starting modelling experiment")

  # Determine the type of cluster based on the operating system
  cluster_type <- ifelse(.Platform$OS.type == "windows", "PSOCK", "FORK")

  # Create a cluster with the appropriate type
  cl <- parallel::makeCluster(n_cores, type = cluster_type)

  if (cluster_type == "PSOCK") {
    parallel::clusterExport(cl = cl, varlist = c("train_data_united", "test_data_united", "target", "directory", "create_model"), envir = environment())
    parallel::clusterEvalQ(cl, {
      library(tidyverse)
      # library(playOmics)
    })
  }

  # Process each chunk
  results <-
    lapply(1:length(chunks), function(n) {

      chunk_list <- names(chunks)[[n]]
      n <- n + 1

      logger::log_info(sprintf("Processing: %d %s to be evaluated.", length(chunks[[chunk_list]]), chunk_list))

      models <-
        chunks[[chunk_list]] %>%
        parallel::parLapply(cl, ., function(single_model) {
          # lapply(function(single_model) {

          # allow only for non-missing data
          # remove identifier so it won't be required when predicting on new data
          train_data <-
            train_data_united[, c(single_model, target$target_variable)] %>%
            na.omit()

          test_data <-
            test_data_united[, c(single_model, target$target_variable)] %>%
            na.omit()

          model_result <-
            create_model(train_data,
                         test_data,
                         target,
                         log_experiment = log_experiment,
                         explain = explain,
                         add_weights = add_weights,
                         directory = directory,
                         validation_method = validation_method,
                         n_prop = n_prop,
                         n_repeats = n_repeats
            )

          model_result
        })

      logger::log_info("Processing {chunk_list} ended.")

      # save stats
      saveRDS(models, file = paste(directory, paste0(chunk_list, "_stats.Rds"), sep = "/"))

      # trim models
      if (n < n_max & trim_models) {
        logger::log_info("===============================================")
        logger::log_info("Initial Setup: Preparing {length(chunks[[n]])} {n+1}-vars models for construction.")
        models_tbr <- models[lengths(models) > 1]
        toremove <- na.omit(models_tbr[sapply(models_tbr, "[[", trim_metric) <= trim_threshold])
        toremove <- toremove[lengths(toremove) > 0]
        extracted_elements <- sapply(toremove, function(y) as.vector(str_split(y$model_name, pattern = "\\s\\+\\s")))
        logger::log_info("Trimming: {length(toremove)} {n}-vars models below defined threshold.")

        models_tbr <-
          parallel::parLapply(cl, chunks[[n]], function(x) {
            any(
              as.logical(
               lapply(extracted_elements, function(y) {
                  all(y %in% x)
                })
              )
            )
          })

        logger::log_info("Removal Notice: There are {sum(unlist(models_tbr))} {n+1}-vars models to be removed.")

        chunks[[n]] <<- chunks[[n]][!unlist(models_tbr)]
      }

      models
    })

  parallel::stopCluster(cl)
  closeAllConnections()

  logger::log_info("Modelling experiment ended")

  return(results)
}
