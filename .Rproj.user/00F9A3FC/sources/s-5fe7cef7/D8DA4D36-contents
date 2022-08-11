#' Visualize intersections between different modalities/
#'
#' @param data named list of connected dataframes. Common ID must be placed as a first column for all data.
#'
#' @return plot presenting data coverage between different datasets
#' @export
#'
#' @examples
#' plot_coverage(my_data)
#'
plot_coverage <- function(data){

  data_for_plot <-
  lapply(seq_along(data), function(i){
    df <- data.frame(dataset_name = names(data[i]),
                     variable_names = pull(data[[i]][1])) # first column as sample ID

    }) %>%
    bind_rows()

  data_for_plot %>%
    group_by(variable_names) %>%
    summarize(dataset_name = list(dataset_name)) %>%
    ggplot(aes(x=dataset_name)) +
    geom_bar() +
    geom_text(stat='count', aes(label=after_stat(count)), vjust=0) +
    ggupset::scale_x_upset() +
    theme_minimal() +
    theme(plot.margin = margin(t = 2, l = 2, unit = "cm"))


}

