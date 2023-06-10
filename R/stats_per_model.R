# data
#
# molecules_to_count <-
#   raw_values[,c("seizures_any", molecules_to_count)] %>%
#   mutate_at(vars(starts_with("rs")), as.factor) %>%
#   mutate_at(vars(starts_with("TSC")), as.factor)
#
# count(distinct(data))

count_stats_per_model <- function(data){

  unique_counts <-
    data %>%
    summarise_all(n_distinct)

  names_to_fct <- names(unique_counts[which(unique_counts == 2)])

  data <-
  data %>%
    mutate_at(names_to_fct, as.factor)

  data %>%
    group_by(!!rlang::sym(target$target_variable)) %>%
    summarise(n = n()) %>%
    left_join(
      data %>%
        group_by(!!rlang::sym(target$target_variable)) %>%
        summarise(across(where(is.numeric),
                         list(median = ~ median(.x, na.rm = TRUE),
                              Q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
                              Q3 = ~ quantile(.x, 0.75, na.rm = TRUE)
                              ))) %>%
        mutate_if(is.numeric, round, 2) %>%
        gather(key, value, -!!rlang::sym(target$target_variable)) %>%
        separate(key, c("key", "what"), sep = "_(?=Q|m|n)") %>%
        spread(what, value) %>%
        mutate(numbers = paste0(median, " (", Q1, "-", Q3, ")")) %>%
        select(-median, -Q1, -Q3) %>%
        spread(key, numbers),
      by = target$target_variable
    ) %>%
    # left_join(
    #   data %>%
    #     select(where(is.factor)) %>%
    #     pivot_longer(!(!!rlang::sym(target$target_variable)), names_to = "variable", values_to = "value") %>%
    #     group_by(!!rlang::sym(target$target_variable), variable, value) %>%
    #     count() %>%
    #     ungroup() %>%
    #     left_join(
    #     data %>%
    #       group_by(!!rlang::sym(target$target_variable)) %>%
    #       count() %>%
    #       rename(n_total = 2),
    #     by = target$target_variable
    #     ) %>%
    #     mutate(pcent =  scales::percent(n/n_total, accuracy =1)) %>%
    #     mutate(numbers = paste0(value, ":", pcent, "(n=", n, ")")) %>%
    #     select(-value, -n, -n_total, -pcent) %>%
    #     group_by(variable, !!rlang::sym(target$target_variable)) %>%
    #     summarise(stats = paste(numbers, collapse = "/\n")) %>%
    #     spread(variable, stats)
    # ) %>%
    mutate(!!rlang::sym(target$target_variable) := paste0(!!rlang::sym(target$target_variable), "\n(n=", n, ")")) %>%
    select(-n) %>%
    gather(variable, value, -!!rlang::sym(target$target_variable)) %>%
    spread(!!rlang::sym(target$target_variable), value)
}


bar_plot <- function(dataset, variable, condition = treatment, scale_name = variable) {
  variable <- enquo(variable)
  condition <- enquo(condition)

  dataset %>%
    group_by(!! condition) %>%
    count(!! variable) %>%
    add_column(agreement = "all", .before = 1) %>%
    mutate_if(is.numeric, funs(pcent = (. / sum(.)))) %>%
    rename(value = n) %>%
    ggplot(aes_(x = condition, y = ~pcent, label = ~value, fill = variable)) +
    geom_bar(stat = "identity") +
    geom_text(position = position_stack(vjust = 0.5)) +
    facet_wrap(~agreement) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "top") +
    xlab(NULL) +
    ylab("proportion") +
    scale_fill_discrete(scale_name) +
    scale_y_continuous(labels = scales::percent)
}

raincloud_plot <- function(data, title = NULL){

  ggplot(aes(x = get(names(data)[2]), y = get(names(data)[1]), fill = get(names(data)[2])), data = data)+
    # ggdist::stat_halfeye(
    #   adjust = 0.5,
    #   justification = -.2,
    #   .width = 0,
    #   point_colour = NA,
    #   scale = 0.65
    # ) +
    geom_point(aes(color = get(names(data)[2])), position = position_jitter(width = .15, height = 0), size = .25) +
    geom_boxplot(
      aes(fill = NULL),
      width = 0.2,
      outlier.colour = NA,
      alpha = 0.5
    ) +
    # scale_color_manual(values = pal_nejm("default")(5)[2:3]) +
    # scale_fill_manual(values = pal_nejm("default")(5)[2:3]) +
    labs(x = NULL, title = title) +
    theme_classic() +
    theme(legend.position = "none",
          axis.text.x= element_blank(),#element_text(angle=60, hjust=1),
          text=element_text(size=12,  family="Helvetica", colour = "black"), axis.text = element_text(size = 12, colour = "black"), plot.title = element_text(size=12, colour = "black"), strip.text.x = element_text(size = 10), strip.text.y = element_blank(), panel.grid.minor = element_blank(), strip.background.x = element_rect(fill = "white"))
}
