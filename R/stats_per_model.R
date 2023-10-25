#' Count Statistics per Model
#'
#' This function calculates statistics for the given dataset per target level.
#' It first detects binary columns and converts them to factors.
#' Then, for numeric columns, it calculates the median, first quartile, and third quartile.
#' For factor columns, it calculates percentages and counts per factor level.
#'
#' @param data A dataframe containing the data for which statistics should be calculated.
#' @param target A list containing the target variable details. This list should contain:
#'   `$target_variable` which specifies the target variable name as a string.
#'
#' @return A dataframe with calculated statistics per target level.
#'   - For numeric variables: Median, Q1, Q3 values.
#'   - For factor variables: Level percentages and counts.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(target_var = c("A", "A", "B", "B"),
#'                  num_var = c(1, 2, 2.5, 3),
#'                  fct_var = c("Yes", "No", "Yes", "No"))
#' target_list <- list(target_variable = "target_var")
#' count_stats_per_model(df, target_list)
#' }
#'
#' @export

count_stats_per_model <- function(data, target) {

  unique_counts <-
    data %>%
    summarise_all(n_distinct)

  fct_names <- names(unique_counts[which(unique_counts == 2)])

  data <-
    data %>%
    mutate_at(fct_names, as.factor)

  data %>%
    group_by(!!rlang::sym(target$target_variable)) %>%
    summarise(n = n()) %>%
    left_join(
      data %>%
        group_by(!!rlang::sym(target$target_variable)) %>%
        summarise(across(
          where(is.numeric),
          list(
            median = ~ median(.x, na.rm = TRUE),
            Q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
            Q3 = ~ quantile(.x, 0.75, na.rm = TRUE)
          )
        )) %>%
        mutate_if(is.numeric, round, 2) %>%
        gather(key, value, -!!rlang::sym(target$target_variable)) %>%
        separate(key, c("key", "what"), sep = "_(?=Q|m|n)") %>%
        spread(what, value) %>%
        mutate(numbers = paste0(median, " (", Q1, "-", Q3, ")")) %>%
        select(-median, -Q1, -Q3) %>%
        spread(key, numbers),
      by = target$target_variable
    ) %>%
    left_join(
      data %>%
        select(where(is.factor)) %>%
        pivot_longer(!(!!rlang::sym(target$target_variable)), names_to = "variable", values_to = "value") %>%
        group_by(!!rlang::sym(target$target_variable), variable, value) %>%
        count() %>%
        ungroup() %>%
        left_join(
          data %>%
            group_by(!!rlang::sym(target$target_variable)) %>%
            count() %>%
            rename(n_total = 2),
          by = target$target_variable
        ) %>%
        mutate(pcent = scales::percent(n / n_total, accuracy = 1)) %>%
        mutate(numbers = paste0(value, ": ", pcent, " (n=", n, ")")) %>%
        select(-value, -n, -n_total, -pcent) %>%
        group_by(variable, !!rlang::sym(target$target_variable)) %>%
        summarise(stats = paste(numbers, collapse = " | ")) %>%
        ungroup() %>%
        pivot_wider(names_from = variable, values_from = stats)
    ) %>%
    mutate(!!rlang::sym(target$target_variable) := paste0(!!rlang::sym(target$target_variable), "\n(n=", n, ")")) %>%
    select(-n) %>%
    gather(variable, value, -!!rlang::sym(target$target_variable)) %>%
    spread(!!rlang::sym(target$target_variable), value)
}
