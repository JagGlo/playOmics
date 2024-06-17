visualize_ranking <- function(ranking, top_n_feat = 20){
  ranking %>%
    lapply(function(dataset){
      p1 <-
        dataset %>%
        ggplot(aes(y = mean_score)) +
        ggdist::stat_halfeye(
          aes(fill = NULL),
          adjust = 0.5,
          justification = -.25,
          .width = 0,
          point_colour = NA,
          scale = 0.65
        ) +
        geom_boxplot(
          aes(fill = NULL),
          width = 0.2,
          # outlier.colour = NA,
          alpha = 0.5
        ) +
        theme_bw() +
        theme(
          legend.position = "none",
          axis.text.x= element_blank(),
          # text=element_text(size=12,  family="Helvetica", colour = "black"),
          # axis.text = element_text(size = 12, colour = "black"),
          # plot.title = element_text(size=12, colour = "black"), strip.text.x = element_text(size = 10),
          # strip.text.y = element_blank(), panel.grid.minor = element_blank(),
          # strip.background.x = element_rect(fill = "white")
        )
      p2 <- dataset %>%
        slice_max(order_by = mean_score, n = top_n_feat, with_ties = F) %>%
        rename(x = original_name, y = mean_score) %>%
        arrange(y) %>%
        mutate(x = as.factor(x) %>% fct_inorder()) %>%
        ggplot(aes(x = x, y = y)) +
        geom_segment( aes(x=x, xend=x, y=0, yend=y), color="skyblue") +
        geom_point( color="blue", size=4, alpha=0.6) +
        theme_light() +
        coord_flip() +
        theme_classic()
      # theme(
      # legend.position = "none",
      # axis.text.x= element_text(angle=60, hjust=1),
      # text=element_text(size=12,  family="Helvetica", colour = "black"),
      # axis.text = element_text(size = 12, colour = "black"),
      # plot.title = element_text(size=12, colour = "black"), strip.text.x = element_text(size = 10),
      # strip.text.y = element_blank(), panel.grid.minor = element_blank(),
      # strip.background.x = element_rect(fill = "white"))
      ggpubr::ggarrange(p1, p2, widths = c(1,3))
    })
}
