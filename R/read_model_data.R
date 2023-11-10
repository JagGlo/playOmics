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

#' Get experiment metrics from logged files
#'
#' This function reads data from files stored in a specified directory.
#'
#' @param experiment_name The name of the experiment.
#' @param file_type The type of file to read; either "metrics", "model_logs" or "params". Default is \code{metrics}.
#' @param directory The directory where the RDS files are stored. Default is the current working directory (\code{getwd()}).
#'
#' @return A data frame containing the combined experiment metrics.
#'
#' @examples
#' \dontrun{
#' # Read experiment metrics from the "my_experiment" directory
#' metrics_data <- read_logged_data("my_experiment", file_type = "metrics")
#'}
#'
#' @export
read_logged_data <- function(experiment_name, file_type = "metrics", directory = getwd()){
  results <-
    lapply(list.dirs(path = paste(directory, experiment_name, sep = "/"), full.names = T)[-1], function(f) {
      params_path <- paste0(f, "/", file_type, ".json")
      tryCatch({
        logged_data <-
          if(file_type == "model_logs"){
            lapply(readLines(params_path), jsonlite::fromJSON) %>% bind_rows()
          } else if(file_type == "metrics") {
            jsonlite::fromJSON(readLines(params_path), simplifyVector = F) %>%
              bind_rows() %>%
              mutate_at(vars(starts_with("train_"), starts_with("test_")), as.numeric)
          } else if(file_type == "params") {
            jsonlite::fromJSON(readLines(params_path), simplifyVector = F)
          }
      }, error = function(e) {
        print(e)
        return(NULL)
      })
    })
}
