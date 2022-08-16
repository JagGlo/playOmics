#' Summarise data structure
#'
#' Check the data structure for each element of list separately
#'
#' @param data Named list of dataframes
#
#' @return Summary of data in the tabularized format with following positions:
#' number of samples, number of variables, number of numeric/character/factor columns across dataset.
#'
#' @examples
#' data_summary(my_data)
#'
#' @export

data_summary <- function(data){

  data_summary <-
    lapply(seq_along(data), function(i){

      data.frame(
        "Dataset name" = names(data[i]),
        "Number of samples" = nrow(data[[i]]),
        "Number of variables" = ncol(data[[i]]),
        "Numeric columns" =  length(data[[i]][unlist(lapply(data[[i]], is.numeric))]),
        "Character columns" =  length(data[[i]][unlist(lapply(data[[i]], is.character))]),
        "Factor columns" =  length(data[[i]][unlist(lapply(data[[i]], is.factor))])
      )

    }) %>%
    bind_rows()

  return(data_summary)
}

#' Check numeric
#'
#' Basic statistics for numeric data
#'
#' @param dataset Either a dataframe or element of a list
#
#' @return
#'\itemize{
#'   \item A - The letters of the alphabet.
#'   \item B - A vector of numbers.
#' }
#' @examples
#' #For dataframe:
#' check_numeric(dataset)
#' #For element of a list:
#' check_numeric(my_data$dataset)
#'
#' @export

check_data <- function(dataset){
  numeric_data <-
    dataset[unlist(lapply(dataset, is.numeric))] %>%
    gather(variable, value) %>%
    group_by(variable) %>%
    summarise(n_available = sum(!is.na(value)),
              n_missing = sum(is.na(value)),
              n_unique = n_distinct(value),
      min = min(value, na.rm = T),
      `Q1` = quantile(value, 0.25, na.rm = TRUE),
      mean = mean(value, na.rm = T),
      median = median(value, na.rm = T),
      sd = sd(value, na.rm = T),
      variance = var(value, na.rm = T),
      `Q3` = quantile(value, 0.75, na.rm = TRUE),
      max =max(value, na.rm = T)) %>%
    rename(numeric_variables = variable)
  non_numeric_data <-
    dataset[!unlist(lapply(dataset, is.numeric))] %>%
    gather(variable, value) %>%
    group_by(variable) %>%
    summarise(n_available = sum(!is.na(value)),
              n_missing = sum(is.na(value)),
              n_unique = n_distinct(value)) %>%
    rename(non_numeric_variables = variable)
  return(list(non_numeric_data, numeric_data))
}

#' Density plots for numeric data
#'
#' Basic statistics for numeric data
#'
#' @param dataset Either a dataframe or element of a list
#
#' @return
#'
#' @examples
#' For dataframe:
#' check_numeric(dataset)
#' For element of a list:
#' check_numeric(my_data$dataset)
#'
#' @export

plot_density_numeric <- function(dataset){

  dataset[unlist(lapply(dataset, is.numeric))] %>%
    gather(key, value) %>%
    ggplot(aes(value, color = key)) +
    geom_density(show.legend = F) +
    theme_minimal()
}
