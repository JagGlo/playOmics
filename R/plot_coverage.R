#' Visualize intersections between different modalities
#'
#' This function takes a named list of connected dataframes as input, where the common ID is placed
#' as the first column for all dataframes. It generates a plot that presents the data coverage
#' between different datasets, showing how many common IDs (samples) are shared across each dataset.
#'
#' @param data named list of connected dataframes. Common ID must be placed as a first column for all data.
#'
#' @return plot presenting data coverage between different datasets.
#' @export
#'
#' @examples
#' data1 <- data.frame(SampleID = 1:5, Variable = c("A", "B", "C", "D", "E"))
#' data2 <- data.frame(SampleID = 3:7, Variable = c("C", "D", "E", "F", "G"))
#' my_data <- list(data1 = data1, data2 = data2)
#' plot_coverage(my_data)
#'
#' @import ggplot2
#' @import ggupset
#'
plot_coverage <- function(data) {
  data_for_plot <-
    lapply(seq_along(data), function(i) {
      df <- data.frame(
        dataset_name = names(data[i]),
        variable_names = pull(data[[i]][1])
      ) # first column as sample ID
    }) %>%
    bind_rows()

  data_for_plot %>%
    group_by(variable_names) %>%
    summarize(dataset_name = list(dataset_name)) %>%
    ggplot(aes(x = dataset_name)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = 0) +
    ggupset::scale_x_upset() +
    theme_minimal() +
    theme(plot.margin = margin(t = 2, l = 2, unit = "cm"))
}
