# Function to safely create a cluster and automatically stop it after use
autoStopCluster <- function(cl) {
  stopifnot(inherits(cl, "cluster"))
  env <- new.env()
  env$cluster <- cl
  attr(cl, "gcMe") <- env
  reg.finalizer(env, function(e) {
    message("Finalizing cluster ...")
    message(capture.output(print(e$cluster)))
    try(parallel::stopCluster(e$cluster), silent = FALSE)
    closeAllConnections()
    message("Finalizing cluster ... done")
  })
  cl
}

# Function to log error message and stop execution if a condition fails
stopIfConditionFails <- function(condition, message) {
  if (!condition) {
    logger::log_error(message)
    stop(message)
  }
}

#' Create multiple models for given datasets
#'
#' This function iterates over combinations of predictor variables in the train data to build
#' multiple models. It also provides the option to validate models using permutations.
#'
#' @details
#' When you dive into using the \emph{create_multiple_models()} function, you're not just making one model — you're orchestrating a symphony of models,
#' each one trying its best to shed light on your data. Among these models, however, not all are equal. Some might not capture
#' the patterns in your data well, and this is where the trim_models, trim_metric, and trim_threshold parameters can be handy.
#'
#'\itemize{
#'   \item \strong{trim_models}: This parameter acts as a switch or flag.
#' If set to TRUE, the function will employ the mechanism to remove or "trim" models based on the specified metric and threshold (detailed below). If set to FALSE, all models will be kept, regardless of their performance.
#'  \item \strong{trim_metric}: This specifies the metric used to evaluate model performance.
#' In the provided function, "train_mcc" (which likely stands for Matthews Correlation Coefficient on the training data) is the default metric. However, users can potentially specify other metrics if they want models to be evaluated and potentially trimmed based on different performance criteria.
#' \item \strong{trim_threshold}: This is the critical value, based on which models will be evaluated for potential trimming.
#' If the performance metric of a model (as specified by trim_metric) is below this threshold, and trim_models is set to TRUE, that model will be removed or "trimmed."
#' The default value provided is 0.3. So, for instance, with default settings, any model with a "train_mcc" less than 0.3 would be removed.
#'}
#' After constructing "n"-variables models for each variable combination, the function checks if the metric (e.g., "train_mcc") for the component of this model in the "n-1"-variable run is
#' below the specified threshold. If it is and if trim_models is TRUE, those underperforming models are removed. This ensures that subsequent
#' combinations of variables don't waste time considering models that are deemed unsatisfactory based on prior results.
#'
#' @param experiment_name A character string denoting the name of the experiment.
#' @param train_data A data frame containing the training data.
#' @param test_data A data frame containing the testing data.
#' @param target A list with two elements: 'target_variable' (name of the dependent variable) and 'id_variable' (name of the identifier variable).
#' @param n_max An integer specifying the maximum number of predictor variables to consider in combinations. Default is 3.
#' @param n_cores An integer specifying the number of CPU cores to use in parallel processing. Default is one fourth of the available cores.
#' @param directory A character string specifying the path to the working directory. Default is the current working directory.
#' @param validate_with_permutation A logical indicating whether to validate models using permutations. Default is FALSE.
#' @param n_perm An integer specifying the number of permutations to be used if 'validate_with_permutation' is TRUE.
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
#' # results <-
#' create_multiple_models(
#'        experiment_name = my_experiment_name,
#'        train_data = train_data_prepared,
#'        test_data = test_data_prepared,
#'        target = my_target,
#'        n_max = 3,
#'        validate_with_permutation = FALSE,
#'        n_perm = NULL,
#'        trim_models = TRUE,
#'        trim_metric = "train_mcc",
#'        trim_threshold = 0.3,
#'        # single model settings
#'        validation_method = "cv",
#'        n_prop = NULL,
#'        n_repeats = 5,
#'        log_experiment = TRUE,
#'        explain = TRUE,
#'        # configuration
#'        n_cores = 5,
#'       directory = here::here()
#'       )
#'
#' @export


create_multiple_models <- function(experiment_name,
                                   train_data,
                                   test_data,
                                   target,
                                   n_max = 3,
                                   validate_with_permutation = FALSE,
                                   n_perm = NULL,
                                   trim_models = TRUE,
                                   trim_metric = "train_mcc",
                                   trim_threshold = 0.3,
                                   # single model settings
                                   validation_method = "subsampling",
                                   n_prop = 2 / 3,
                                   n_repeats = 10,
                                   log_experiment = TRUE,
                                   explain = TRUE,
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

  # prepare test data
  test_data_united <-
    test_data %>%
    reduce(full_join, by = c(target$id_variable)) %>%
    select(-target$id_variable, -target$target_variable, everything())

  # Ensure n_max is at least 2
  stopIfConditionFails(n_max >= 2, "n_max can't be lower than 2. Please introduce higher number!")

  # Prepare variables combinations
  chunks <- list()

  for (i in 2:n_max) {
    chunks[[i - 1]] <- combn(names(train_data_united)[!(names(train_data_united) %in% c(target$target_variable, target$id_variable))], i, simplify = FALSE)
  }

  names(chunks) <- paste0(2:n_max, "-vars models")

  last_run <- Sys.Date()

  # Save data
  save(train_data, test_data, train_data_united, test_data_united, last_run, chunks, file = paste(directory, "data.RData", sep = "/"))

  # Log the start of the experiment
  logger::log_info("Starting modelling experiment")

  # Check if validate_with_permutation is selected, and if so, ensure number of permutations is set
  stopIfConditionFails(!validate_with_permutation || !is.null(n_perm), "Option validate with permutation selected, but number of permutations is zero. Please set number of permutations.")

  # Process each chunk
  results <-
    lapply(1:length(chunks), function(n) {
      chunk_list <- names(chunks)[[n]]
      n <- n + 1
      logger::log_info(sprintf("There are %d %s to be constructed", length(chunks[[chunk_list]]), chunk_list))

      cl <- autoStopCluster(parallel::makeCluster(n_cores, type = "PSOCK"))
      parallel::clusterExport(cl = cl, varlist = c("train_data_united", "test_data_united", "target", "directory", "create_model"), envir = environment())
      parallel::clusterEvalQ(cl, {
        library(tidyverse)
        # library(playOmics)
      })

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
                         directory = directory,
                         validation_method = validation_method,
                         n_prop = n_prop,
                         n_repeats = n_repeats)

          # train & test shuffled separately?
          if (validate_with_permutation) {
            permutation_results <-
              lapply(1:n_perm, function(i) {
                # Shuffle the target variable in train and test data
                train_data_perm <- train_data %>% mutate(!!target$target_variable := sample(!!sym(target$target_variable)))
                test_data_perm <- test_data %>% mutate(!!target$target_variable := sample(!!sym(target$target_variable)))
                # Create a model with shuffled data
                my_result <- test_data_perm(train_data_perm, test_data_perm, target, log_experiment = FALSE, explain = FALSE, directory = directory, validation_method = "cv", n_repeats = 3)
                return(my_result)
              }) %>%
              bind_rows(.id = "permutation_no")

            # Calculate p-values to evaluate the significance of the mode
            pval <- rowSums(apply(permutation_results, 1, function(x) {
              select(model_result, !starts_with("model")) > x[-c(1:2)]
            }, simplify = T)) / nrow(permutation_results)

            # Prepare p-values for merging with the model results
            names(pval) <- names(model_result)
            pval <- pval %>% rename_with(~ paste0("perm_pval_", .x))

            model_result <-
              bind_cols(
                model_result,
                tibble(n_perm = n_perm),
                data.frame(t(pval))
              )
          }

          model_result
        })

      parallel::stopCluster(cl)
      closeAllConnections()

      # save stats
      saveRDS(models, file = paste(directory, paste0(chunk_list, "_stats.Rds"), sep = "/"))

      # trim models
      if (n < n_max & trim_models) {
        models_tbr <- models[lengths(models) > 1]
        toremove <- na.omit(models_tbr[sapply(models_tbr, "[[", trim_metric) <= trim_threshold])
        toremove <- toremove[lengths(toremove) > 0]
        logger::log_info("There are {length(toremove)} {n}-vars models to be removed")

        models_tbr <-
          lapply(chunks[[n]], function(x) {
            any(
              as.logical(
                lapply(toremove, function(y) {
                  all(unlist(as.vector(str_split(y$model_name, pattern = "\\s\\+\\s"))) %in% x)
                })
              )
            )
          })

        logger::log_info("There are {sum(unlist(models_tbr))} {n+1}-vars models to be removed")

        chunks[[n]] <<- chunks[[n]][!unlist(models_tbr)]
      }

      models
    })

  logger::log_info("Modelling experiment ended")

  return(results)
}
