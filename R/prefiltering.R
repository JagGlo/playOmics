#' Filter values below defined threshold
#'
#'
#'
#' @param numeric_threshold value below which data should be filtered out
#' @param pcent_of_samples indicates % of values within variable which should contain
#' a value higher than the one defined in the numeric_variable parameter; n total is calculated based
#' on all samples (no distinction between missing and non-missing values)
#
#' @return
#'
#' @examples
#'
#' @export

filter_below_threshold <- function(data, numeric_threshold, pcent_of_samples, inside_group = NULL, group_by = NULL){

  data_filtered <- data[-1] > numeric_threshold
  rownames(data_filtered) <- data[[1]]

  to_save <-
  as.data.frame(colSums(data_filtered, na.rm = T)) %>%
    tibble::rownames_to_column() %>%
    rename(number_of_positives = 2) %>%
    filter(as.numeric(number_of_positives) >= pcent_of_samples * (nrow(data)-1))

  data <-data[, c(names(data)[1], to_save$rowname)]

  return(data)

}

#' Filter missing values
#'
#'
#'
#' @param pcent_of_samples indicates % of values within variable which should contain
#' non-missing values in all samples
#
#' @return
#'
#' @examples
#'
#' @export

filter_missing <- function(data, pcent_of_samples, inside_group = NULL){

  data_non_missing <- !is.na(data[-1])
  rownames(data_non_missing) <- data[[1]]

  to_save <-
    as.data.frame(colSums(data_non_missing, na.rm = T)) %>%
    tibble::rownames_to_column() %>%
    rename(number_of_positives = 2) %>%
    filter(as.numeric(number_of_positives) >= pcent_of_samples * (nrow(data)-1))

  data <-data[, c(names(data)[1], to_save$rowname)]

  return(data)

}

# near zero variance
# lapply( LAML_data[["miRNA"]][-1], FUN = var, na.rm = T) %>%
#   cbind() %>% unlist() %>%
#   summary()






