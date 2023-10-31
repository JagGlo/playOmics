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
#' data1 <- data.frame(SampleID = c(1:5),Variable1 = c(3, 6, 2, 8, 4),
#' Variable2 = c("A", "B", "A", "C", "B"),Variable3 = factor(c("X", "Y", "Z", "X", "Y")))
#' data2 <- data.frame(SampleID = c(1:5), Variable1 = c(2, 4, 6, 8, 10),
#' Variable2 = c("A", "B", "C", "D", "E"),Variable3 = factor(c("X", "Y", "Z", "X", "Y")))
#' my_data <- list(data1 = data1, data2 = data2)
#' data_summary(my_data)
#'
#' @export

data_summary <- function(data) {
  data_summary <-
    lapply(seq_along(data), function(i) {
      data.frame(
        "Dataset name" = names(data[i]),
        "Number of samples" = nrow(data[[i]]),
        "Number of variables" = ncol(data[[i]]),
        "Numeric columns" = length(data[[i]][unlist(lapply(data[[i]], is.numeric))]),
        "Character columns" = length(data[[i]][unlist(lapply(data[[i]], is.character))]),
        "Factor columns" = length(data[[i]][unlist(lapply(data[[i]], is.factor))])
      )
    }) %>%
    bind_rows()

  return(data_summary)
}

#' Check data
#'
#' This function calculates basic statistics for numeric data within a dataframe or an element
#' of a list. It provides information such as the count of available and missing values, unique values,
#' minimum, first quartile (Q1), mean, median, standard deviation, variance, third quartile (Q3), and maximum.
#'
#' @param dataset Either a dataframe or element of a list.
#
#' @return A list containing two dataframes: 'non_numeric_data' and 'numeric_data'.
#
#' @examples
#' data1 <- data.frame(SampleID = c(1:5),Variable1 = c(3, 6, 2, 8, 4),
#' Variable2 = c("A", "B", "A", "C", "B"),Variable3 = factor(c("X", "Y", "Z", "X", "Y")))
#' data2 <- data.frame(SampleID = c(1:5), Variable1 = c(2, 4, 6, 8, 10),
#' Variable2 = c("A", "B", "C", "D", "E"),Variable3 = factor(c("X", "Y", "Z", "X", "Y")))
#' my_data <- list(data1 = data1, data2 = data2)
#' # For dataframe:
#' check_data(data1)
#' # For element of a list:
#' check_data(my_data$data1)
#'
#' @export

check_data <- function(dataset) {
  numeric_data <-
    dataset[unlist(lapply(dataset, is.numeric))] %>%
    gather(variable, value) %>%
    group_by(variable) %>%
    summarise(
      n_available = sum(!is.na(value)),
      n_missing = sum(is.na(value)),
      n_unique = n_distinct(value),
      min = min(value, na.rm = T),
      `Q1` = quantile(value, 0.25, na.rm = TRUE),
      mean = mean(value, na.rm = T),
      median = median(value, na.rm = T),
      sd = sd(value, na.rm = T),
      variance = var(value, na.rm = T),
      `Q3` = quantile(value, 0.75, na.rm = TRUE),
      max = max(value, na.rm = T)
    ) %>%
    rename(numeric_variables = variable)
  non_numeric_data <-
    dataset[!unlist(lapply(dataset, is.numeric))] %>%
    gather(variable, value) %>%
    group_by(variable) %>%
    summarise(
      n_available = sum(!is.na(value)),
      n_missing = sum(is.na(value)),
      n_unique = n_distinct(value)
    ) %>%
    rename(non_numeric_variables = variable)
  return(list(non_numeric_data = non_numeric_data, numeric_data = numeric_data))
}

#' Plot statistics for numeric data
#'
#' This function generates boxplots to visualize basic statistics for numeric data within a dataframe
#  or an element of a list. It also return metrics such as minimum, median, mean, standard deviation, and maximum in the tabular form.
#'
#' @param dataset Either a dataframe or element of a list
#' @param metric_to_plot The metric to plot (e.g., 'median', 'mean', etc.).
#'
#' @return A boxplot and a table summarizing statistics for the specified metric.
#'
#'
#' @examples
#' \dontrun{
#' # For a dataframe:
#' plot_stats(my_data, "median")
#' # For an element of a list:
#' plot_stats(my_data$dataset, "mean")
#'}
#'
#' @export

plot_stats <- function(dataset, metric_to_plot) {
  summary <- check_data(dataset)

  plot <-
    summary$numeric_data %>%
    ggplot(aes(y = {{ metric_to_plot }}, x = NULL)) +
    geom_boxplot() +
    labs(x = NULL, y = paste0(deparse(substitute(metric_to_plot)), "'s value across all variables")) +
    theme_classic() +
    theme(axis.text.x = element_blank())

  legend <- paste("Stats for", deparse(substitute(metric_to_plot)), "across all variables")

  table <-
    summary$numeric_data %>%
    rename(value = {{ metric_to_plot }}) %>%
    summarise(
      min = min(value, na.rm = T),
      median = median(value, na.rm = T),
      mean = mean(value, na.rm = T),
      sd = sd(value, na.rm = T),
      max = max(value, na.rm = T)
    ) %>%
    mutate_all(round, 3) %>%
    knitr::kable(caption = legend)

  return(list(table, plot))
}
