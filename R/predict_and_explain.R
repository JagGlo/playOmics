#' Load Model and Make Predictions
#'
#' This function loads a saved model from a specified path and makes predictions
#' on the provided data.
#'
#' @param path The directory path where the model is saved.
#' @param data The data on which predictions should be made.
#'
#' @return The model's predictions on the data. If there is an error, the function logs the error message and returns NULL.
#'
#' @examples
#' \dontrun{
#' predicted_values <- load_and_predict("/path/to/model/directory", sample_data)
#' }
#'
#' @importFrom logger log_error
load_and_predict <- function(path, data) {
  tryCatch(
    {
      model_path <- paste(path, "model.Rds", sep = "/")
      model <- readRDS(model_path)
      model(data)
    },
    error = function(e) {
      logger::log_error(e[["message"]])
      NULL
    }
  )
}

#' Load Explainer and Generate Explanations
#'
#' This function loads a saved explainer from a specified path and generates explanations
#' for the provided data.
#'
#' @param path The directory path where the explainer is saved.
#' @param data The data for which explanations are needed.
#'
#' @return The explainer's output for the data. If there is an error, the function logs the error message and returns NULL.
#'
#' @importFrom DALEX explain
#'
#' @examples
#' \dontrun{
#' explanations <- load_and_explain("/path/to/explainer/directory", sample_data)
#' }
#'
#' @importFrom logger log_error
load_and_explain <- function(path, my_data) {
  tryCatch(
    {
      explainer_path <- paste(path, "explainer.Rds", sep = "/")
      my_explainer <- readRDS(explainer_path)
      my_explainer(my_data)
    },
    error = function(e) {
      logger::log_error(e[["message"]])
      NULL
    }
  )
}
