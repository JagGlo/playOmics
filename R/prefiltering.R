#' Filter values below a defined threshold
#'
#' This function filters out columns with percentage of values below a specified numeric threshold
#' in a dataset. The percentage of samples within a variable containing values
#' higher than the threshold is calculated based on non-missing values.
#'
#' @param data A dataset containing numeric values to be filtered.
#' @param numeric_threshold The numeric threshold below which data should be filtered out.
#' @param pcent_of_samples The percentage of values within a variable that should
#' contain values higher than the numeric threshold.
#'
#' @return A filtered dataset.
#'
#' @examples
#' # Filter values below a threshold
#' my_data <- data.frame(SampleID = c(1:10),
#'   Variable1 = c(3, 6, 2, 8, 4, 1, 9, 7, 5, 10),
#'   Variable2 = c(NA, 2, 5, 1, NA, 7, 3, 6, 8, NA)
#'   )
#' filtered_data <- filter_below_threshold(my_data, 5, 0.8)
#'
#' @export
filter_below_threshold <- function(data, numeric_threshold, pcent_of_samples){

  # Filter data based on the numeric threshold
  data_filtered <- data[-1] > numeric_threshold
  rownames(data_filtered) <- data[[1]]

  # Calculate the percentage of samples meeting the threshold
  to_save <-
    as.data.frame(colSums(data_filtered, na.rm = TRUE)) %>%
    tibble::rownames_to_column() %>%
    rename(number_of_positives = 2) %>%
    filter(as.numeric(number_of_positives) >= pcent_of_samples * (nrow(data) - 1))

  # Extract the relevant columns
  data_filtered <- as.data.frame(data[, c(names(data)[1], to_save$rowname)])

  # Give warning if no columns are left
  if (ncol(data_filtered) == 1) {
    warning("No columns left after filtering.")
    names(data_filtered) <- names(data)[1]
  }

  return(data_filtered)
}


#' Filter missing values based on a percentage threshold
#'
#' This function filters out rows with missing values in a dataset. The percentage
#' of non-missing values within a variable that should be present in all samples
#' is specified by the \code{pcent_of_samples} parameter.
#'
#' @param data A dataset containing values to be filtered.
#' @param pcent_of_samples The percentage of non-missing values within a variable
#' that should be present in all samples.
#'
#' @return A filtered dataset.
#'
#' @examples
#' # Filter rows with missing values
#' #' my_data <- data.frame(
#'   SampleID = 1:10,
#'   Variable1 = c(3, 6, 2, 8, 4, 1, 9, 7, 5, 10),
#'   Variable2 = c(NA, 2, 5, 1, NA, 7, 3, 6, 8, NA)
#'   )
#' filtered_data <- filter_missing(my_data, 0.8)
#'
#' @export
filter_missing <- function(data, pcent_of_samples){

  # Filter data based on missing values
  data_non_missing <- !is.na(data[-1])
  rownames(data_non_missing) <- data[[1]]

  # Calculate the percentage of non-missing values
  to_save <-
    as.data.frame(colSums(data_non_missing, na.rm = TRUE)) %>%
    tibble::rownames_to_column() %>%
    rename(number_of_positives = 2) %>%
    filter(as.numeric(number_of_positives) >= pcent_of_samples * (nrow(data) - 1))

  # Extract the relevant columns
  data_filtered <- as.data.frame(data[, c(names(data)[1], to_save$rowname)])

  # Give warning if no columns are left
  if (ncol(data_filtered) == 1) {
    warning("No columns left after filtering.")
    names(data_filtered) <- names(data)[1]
  }

  return(data_filtered)
}
