#' Combine multiple datasets into a list
#'
#' This function takes in multiple datasets and combines them into a list. By
#' default, the original datasets are removed from the global environment.
#'
#' @param ... names of datasets to be concatenated. Common ID must be placed as a first column for all data.
#' @param remove_original_data A logical value indicating whether or not to
#' remove the original datasets from the global environment. Default is TRUE.
#'
#' @return A list containing the combined datasets.
#'
#' @examples
#' # create two sample datasets
#' df1 <- data.frame(a = 1:5, b = 6:10)
#' df2 <- data.frame(a = 11:15, b = 16:20)
#'
#' # combine the datasets into a list
#' combined <- connect_datasets(df1, df2)
#'
#' @export

connect_datasets <- function(..., remove_original_data = TRUE) {
  varnames <- lapply(substitute(list(...))[-1], deparse) %>% unlist()

  data <- list(...)

  names(data) <- varnames

  if (remove_original_data) {
    rm(list = varnames, envir = .GlobalEnv)
  }

  return(data)
}
