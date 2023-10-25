rank_features <- function(data, target, filter_name, n_threads = 1) {

  ranked_features <-
    sapply(names(data), function(dataframe) {
      mydata <- data[[dataframe]] %>% select(-target$id_variable)
      names_dictionary <-
        data.frame(
          original_name = names(mydata),
          valid_name = make.names(colnames(mydata))
        )
      colnames(mydata) <- make.names(colnames(mydata))

      task <- mlr3::as_task_classif(mydata, target = target$target_variable, positive = target$positive_class)
      filter <- mlr3filters::flt(filter_name)

      # set threads for all filters which support it
      mlr3::set_threads(filter, n_threads)

      ranked_features <- data.table::as.data.table(filter$calculate(task)) %>% left_join(names_dictionary, by = c("feature" = "valid_name"))
    }, USE.NAMES = TRUE, simplify = F)

  ranked_features <-
    lapply(1:length(ranked_features), function(i) {
      iteration_name <- names(ranked_features[i])
      ranked_features[[i]] %>% add_column(id = iteration_name)
    }) %>%
    bind_rows()

  ranking <-
    ranked_features %>%
    group_by(original_name) %>%
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
      na.omit(ranking[1:ceiling(nrow(ranking) * cutoff_treshold / 100), 1])
    } else if (cutoff_method == "threshold") {
      na.omit(ranking[ranking$mean_score > cutoff_treshold, 1])
    } else {
      stop("Cutoff method not found!")
    }

  filtered_data <- data[, c(target$id_variable, target$target_variable, pull(selected_features))]
}

#' Perform nested filtering on multiple datasets
#'
#' This function performs nested filtering on multiple datasets. It takes a
#' target dataset and a list of datasets as an input, adds target to each dataset
#' and removes missing target values. It then performs n-times feature ranking on
#' each fold. Nextly, ranking among all folds is created and mean metric value is used for variable selection.
#' The top features are selected according to a specified cutoff method.
#'
#' It consists of two underlying functions:
#'
#'    * The rank_features function ranks the features in a dataset based on their importance
#' for predicting a binary classification target variable. It takes in a dataset and a target
#' variable along with the filter method to be used for feature ranking. It applies the specified
#' filter method on each dataset and creates a ranked list of features along with their scores. It then
#' calculates the mean score and variance for each feature and returns the ranked features along with
#' the ranking table. The function also allows for multithreading.
#'
#'    * The select_features function selects the top features from a dataset based on the ranking
#' calculated by the rank_features function. The function then selects the top features based on the specified
#' cutoff method and threshold. The selected features are then used to create a filtered dataset that only
#' includes the target variable and the selected features. The resulting filtered dataset is returned.
#'
#' @param data A list of datasets to be filtered.
#' @param target A list defining the ID and target variables names to be
#' used for filtering.
#' @param filter_name A character value specifying the feature filter method (https://mlr.mlr-org.com/articles/tutorial/filter_methods.html#current-methods, column "Classif").
#' Default is "auc".
#' @param cutoff_method  A character value specifying the cutoff method to be
#' used for selecting features; one of the following:
#'    * "top_n",
#'    * "percentage",
#'    * "threshold".
#' Default is "top_n".
#' @param cutoff_treshold Depending of a cutoff method, a number of features to be selected (for "top_n" method),
#'percentage of variables to be selected (for "percentage" method), a threshold above which features are selected (for "percentage" method). Default is 1.
#' @param n_threads An integer value specifying the number of threads to be
#' used for feature ranking. Default is 1.
#' @param n_fold An integer value specifying the number of folds for
#' cross-validation. Cross-validation is used for overfitting prevention. A ranking of  Default is 5.
#'
#' @return A list containing the filtered datasets.
#'
#' @examples
#' filtered_data <- nested_filtering(data = data_prepared, target = target, filter_name = "auc",
#'     cutoff_method = "top_n", cutoff_treshold = 10, n_fold = 5 n_threads = 10)
#' @export

nested_filtering <- function(data, target, filter_name = "auc", cutoff_method = "top_n", cutoff_treshold = 10, n_threads = 1, n_fold = 5) {

  # Extract target data from the input data
  target_data <-
    data[[target$phenotype_df]] %>%
    select(target$id_variable, target$target_variable)

  sapply(names(data), function(dataframe) {
    logger::log_info("Ranking {dataframe} data")

    # For each element in the input data, join with the target data and remove missing target values
    if (dataframe == target$phenotype_df) { # Don't change the phenotype df
      data[[dataframe]]
    } else {
      data[[dataframe]] <-
        data[[dataframe]] %>%
        left_join(target_data, by = target$id_variable) %>%
        filter(!is.na(!!rlang::sym(target$target_variable)))
    }

    # Remove missing target values
    data[[dataframe]] <-
      data[[dataframe]] %>%
      filter(!is.na(!!rlang::sym(target$target_variable)))

    # Perform stratified cross-validation using vfold_cv from the rsample package
    resample <-
      rsample::vfold_cv(data[[dataframe]],
                        v = n_fold,
                        strata = target$target_variable
      )

    # Create a list to store the training data for each fold
    training_data <-
      lapply(1:n_fold, function(i) resample$splits[[i]]$data[resample$splits[[i]]$in_id, ])

    names(training_data) <- paste0("split", 1:n_fold)

    # Rank the features using the rank_features function
    ranked_features <-
      rank_features(training_data, target, filter_name = filter_name, n_threads = n_threads)

    # Select the top features based on the ranking using the select_features function
    ranked_features$selected_features <-
      select_features(data[[dataframe]], ranked_features$ranking, target, cutoff_method = cutoff_method, cutoff_treshold = cutoff_treshold)
  }, USE.NAMES = TRUE, simplify = F)
}
