#' Get experiment metrics from stored RDS files
#'
#' This function reads and combines metrics data from RDS files stored in a specified directory.
#'
#' @param experiment_name The name of the experiment.
#' @param directory The directory where the RDS files are stored. Default is the current working directory (\code{getwd()}).
#'
#' @return A data frame containing the combined experiment metrics.
#'
#' @examples
#' \dontrun{
#' # Read experiment metrics from the "my_experiment" directory
#' metrics_data <- read_model_data("my_experiment")
#'}
#'
#' @export

read_model_data <- function(experiment_name, directory = getwd()) {
  # Read and combine metrics data from RDS files
  results <-
    lapply(list.files(path = paste(directory, experiment_name, sep = "/"), pattern = "*.Rds", full.names = T), function(f) {
      readRDS(f)
    }) %>%
    bind_rows()

  if (nrow(results) == 0) {
    stop(paste("No metrics data found in the specified directory. Please verify if the model data has been logged to following directory:", paste(directory, experiment_name, sep = "/")))
  }

  return(results)
}
