# autoStopCluster <- function(cl) {
#   stopifnot(inherits(cl, "cluster"))
#   env <- new.env()
#   env$cluster <- cl
#   attr(cl, "gcMe") <- env
#   reg.finalizer(env, function(e) {
#     message("Finalizing cluster ...")
#     message(capture.output(print(e$cluster)))
#     try(parallel::stopCluster(e$cluster), silent = FALSE)
#     message("Finalizing cluster ... done")
#   })
#   cl
# }

# # Function to safely create a cluster and automatically stop it after use
# autoStopCluster <- function(cl) {
#   # Check if cl is a cluster
#   stopifnot(inherits(cl, "cluster"))
#   # Add exit hook to stop the cluster when the function ends
#   on.exit({
#     message("Finalizing cluster ...")
#     message(capture.output(print(cl)))
#     tryCatch(
#       parallel::stopCluster(cl),  # Attempt to stop the cluster
#       error = function(e) {
#         message("Error while stopping cluster")  # Error handling
#       },
#       finally = {
#         message("Finalizing cluster ... done")
#       }
#     )
#   }, add = TRUE)
#   cl
# }

# Function to log error message and stop execution if a condition fails
stopIfConditionFails <- function(condition, message) {
  if (!condition) {
    logger::log_error(message)
    stop(message)
  }
}

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


create_multiple_models <- function(experiment_name,
                                   train_data,
                                   test_data,
                                   target,
                                   n_max = 3,
                                   n_cores = parallel::detectCores() / 4,
                                   directory = getwd(),
                                   validate_with_permutation = FALSE,
                                   n_perm = NULL,
                                   trim_models = TRUE,
                                   trim_metric = "train_mcc",
                                   trim_threshold = 0.3
) {

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

      cl <- parallel::makeCluster(n_cores, type = "PSOCK")
      parallel::clusterExport(cl = cl, varlist=c("train_data_united", "test_data_united", "target", "directory", "create_model"),  envir=environment())
      parallel::clusterEvalQ(cl, {
        library(tidyverse)
        library(playOmics)
      })
      tictoc::tic()
      models <-
        chunks[[chunk_list]] %>%
        parallel::parLapply(cl, ., function(single_model){
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
            create_model(train_data, test_data, target, log_experiment = TRUE, explain = TRUE, directory = directory, validation_method = "cv", n_repeats = 5)

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

      on.exit(parallel::stopCluster(cl))
      tictoc::toc()

      # save stats
      saveRDS(models, file = paste(directory, paste0(chunk_list, "_stats.Rds"), sep = "/"))

      if (n < n_max) {
        models_tbr <- models[lengths(models) > 1]
        toremove <- na.omit(models_tbr[sapply(models_tbr, "[[", "train_mcc") <= 0.3])
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
