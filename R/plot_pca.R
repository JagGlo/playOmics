#' Plot PCA
#'
#'
#'
#' @param data_to_plot data for PCA analysis
#' @param variable_to_color name of variable for grouping; must be categorical
#' @param legend_title
#' @param plot_title
#'
#' @return None
#'
#' @examples
#' data_to_plot <- data$RNA_data %>% left_join(data$clinical_data)
#' plot_PCA(data_to_plot, variable_to_color = sex, legend_title = "Sex", title = "RNA")
#'
#' @export
#'
#'

convert2matrix<-function(x) {
  m<-as.matrix(x[,-1])
  rownames(m)<-x[[1]]
  m
}

plot_PCA <- function(data_to_plot, variable_to_color, legend_title = "Groups", plot_title = NULL, dim_1 = 1, dim_2 = 2){

  variable_to_color <- enquo(variable_to_color)
  color_indicator <- pull(data_to_plot %>% select(!!variable_to_color))
  data_to_plot <- data_to_plot %>% select(!! -variable_to_color)
  data_matrix <- convert2matrix(data_to_plot)
  data_matrix <- data_matrix[ , colSums(is.na(data_matrix)) == 0]
  data_matrix <- data_matrix[,!is.infinite(colSums(data_matrix))]

  res.pca <- prcomp(data_matrix, scale = F)

  factoextra::fviz_pca_ind(res.pca,
               col.ind = as.factor(color_indicator),
               axes = c(dim_1, dim_2),
               legend.title = legend_title,
               geom = "point"
  ) +
    labs(title = plot_title) +
    theme_classic() +
    theme(legend.position = "bottom",
          text=element_text(size=12,  family="Helvetica", colour = "black"),
          plot.title = element_text(size=12, colour = "black"),
          axis.text = element_text(size=12, colour = "black"))

}
