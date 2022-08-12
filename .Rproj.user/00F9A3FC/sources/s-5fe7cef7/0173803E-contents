#' Select variables
#'
#' Select variables related to a target. Features are selected based on ranking that is created depending of selected method.
#' Then, three methods of selection can be chosen:
#'
#' @param data Name of a dataframe name containing the phenotype/clinical data
#' @param target Name of a column with statuses
#' @param filter_name Name of a filter to be applied (https://mlr.mlr-org.com/articles/tutorial/filter_methods.html#current-methods, column "Classif")
#' @param cutoff_method One of the following: top_n, percentage, threshold
#' @param cutoff_treshold Depending of a cutoff method, a number of features to be selected, % of variables to be selected, a threshold above which features are selected.
#' @param n_threads Number of threads for feature selection (as default set to 1)
#'
#' @return
#'
#' @examples
#' @export
#'
nested_filtering <- function(data, target, filter_name = "auc", cutoff_method, cutoff_treshold, n_threads = 1, nfold = 5) {
  sapply(names(data), function(dataframe) {
    logger::log_info("Ranking {dataframe} data")
    resample <-
      rsample::vfold_cv(data[[dataframe]],
        v = nfold,
        strata = target$target_variable
      )
    training_data <-
      lapply(1:nfold, function(i) resample$splits[[i]]$data[resample$splits[[i]]$in_id, ])

    names(training_data) <- paste0("split", 1:nfold)

    ranked_features <-
      rank_features(training_data, target, filter_name = filter_name, n_threads = n_threads)

    ranked_features$selected_features <-
      select_features(data[[dataframe]], ranked_features$ranking, target, cutoff_method, cutoff_treshold)
  }, USE.NAMES = TRUE, simplify = F)
}

rank_features <- function(data, target, filter_name, n_threads = 1) {

  ranked_features <-
    sapply(names(data), function(dataframe) {
      mydata <- data[[dataframe]] %>% select(-target$id_variable)

      task <- mlr3::as_task_classif(mydata, target = target$target_variable, positive = target$positive_class)
      filter <- mlr3filters::flt(filter_name)

      # set threads for all filters which support it
      mlr3::set_threads(filter, n_threads)

      ranked_features <- data.table::as.data.table(filter$calculate(task))
    }, USE.NAMES = TRUE, simplify = F)

  ranked_features <-
    lapply(1:length(ranked_features), function(i) {
      iteration_name <- names(ranked_features[i])
      ranked_features[[i]] %>% add_column(id = iteration_name)
    }) %>%
    bind_rows()

  # different selection methods? weighted ranks etc?
  ranking <-
    ranked_features %>%
    group_by(!!.[[1]]) %>%
    summarise(
      mean_score = mean(score, na.rm = T),
      variance = var(score, na.rm = T)
    ) %>%
    arrange(desc(mean_score))

  return(list(ranked_features = spread(ranked_features, id, score), ranking = ranking))
}


select_features <- function(data, ranking, target, cutoff_method, cutoff_treshold) {

  # select
  selected_features <-
    if (cutoff_method == "top_n") {
      na.omit(ranking[1:cutoff_treshold, 1])
    } else if (cutoff_method == "percentage") {
      na.omit(ranked_features[1:(nrow(ranked_features) * cutoff_treshold / 100), 1])
    } else if (cutoff_method == "threshold") {
      na.omit(ranked_features[ranked_features$score > cutoff_treshold, 1])
    }

  filtered_data <- data[, c(target$id_variable, target$target_variable, pull(selected_features))]
}
