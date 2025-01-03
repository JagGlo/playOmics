rank_features <- function(data, target, filter_name, n_threads = 1) {

  if(is.data.frame(data)){
    data <- list(data = data)
  }

  ranked_features <-
    sapply(names(data), function(dataframe) {

      mydata <- data[[dataframe]] %>% select(-all_of(target$id_variable))
      names_dictionary <-
        data.frame(
          original_name = names(mydata),
          valid_name = make.names(colnames(mydata))
        )
      if(filter_name %in% c("auc", "information_gain", "variance")){
        colnames(mydata) <- make.names(colnames(mydata))
        task <- mlr3::as_task_classif(mydata, target = target$target_variable, positive = target$positive_class)
        filter <- mlr3filters::flt(filter_name)

        # set threads for all filters which support it
        mlr3::set_threads(filter, n_threads)

        ranked_features <- data.table::as.data.table(filter$calculate(task)) %>% left_join(names_dictionary, by = c("feature" = "valid_name"))

      } else if (filter_name == "mrmr" & ncol(mydata) > 1){
        X <- mydata %>% select(-all_of(target$target_variable))
        Y <- mydata %>% pull(all_of(target$target_variable))
        results <- praznik::MRMR(X, Y, k = ncol(mydata)-1, threads = n_threads)
        ranked_features <- data.frame(original_name=names(results$score), score=results$score, row.names=NULL) %>% arrange(desc(score))
      } else if (filter_name == "jmim"& ncol(mydata) > 1){
        X <- mydata %>% select(-all_of(target$target_variable))
        Y <- mydata %>% pull(all_of(target$target_variable))
        results <- praznik::JMIM(X, Y, k = ncol(mydata)-1, threads = n_threads)
        ranked_features <- data.frame(original_name=names(results$score), score=results$score, row.names=NULL) %>% arrange(desc(score))
      } else {
        ranked_features <- data.frame(original_name = NA, score = NA)
      }

    }, USE.NAMES = TRUE, simplify = FALSE)

  ranked_features <- bind_rows(ranked_features)

  ranking <-
    ranked_features %>%
    group_by(original_name) %>%
    summarise(
      mean_score = mean(score, na.rm = TRUE),
      variance = var(score, na.rm = TRUE)
    ) %>%
    arrange(desc(mean_score))

  return(list(ranking = ranking))
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

  filtered_data <- as.data.frame(data)[, c(target$id_variable, target$target_variable, pull(selected_features))]
  return(filtered_data)
}

rank_and_select_features <- function(data, target, filter_name = "auc", cutoff_method = "top_n", cutoff_treshold = 10,
                                     selection_type = "combined", return_ranking_list = FALSE, n_fold = 5, n_threads = 1) {

  if(selection_type == "separate"){
    # Process each dataframe separately
    # Initialize filtered_data and ranking_list
    filtered_data <- list()
    ranking_list <- list()
    
    #Extract the dataset assignment from the column names
    dataset_assignments <- colnames(data) %>%
      str_extract_all("\\[.*?\\]") %>%
      unlist() %>%
      str_replace_all("[\\[\\]]", "")

    # Create a named list where each element is a dataframe for one dataset
    split_data <- map(unique(dataset_assignments), function(dataset) {
      selected_columns <- grep(paste0("\\[", dataset, "\\]"), colnames(data), value = TRUE)
      data %>%
        select(my_target$id_variable, all_of(selected_columns))
    })

    # Name each element of the list with the dataset assignment
    names(split_data) <- unique(dataset_assignments)
    
    split_data[[target$phenotype_df]] <- data %>% select(my_target$id_variable, my_target$target_variable)
    
    data <- split_data
    
    # Perform ranking
    for(dataframe in names(data)){
      logger::log_info("Ranking {dataframe} data")
      # For each dataframe, process as before
      # Extract target data
      target_data <- data[[target$phenotype_df]] %>% select(all_of(c(target$id_variable, target$target_variable)))
      if (dataframe == target$phenotype_df) { # Don't change the phenotype df
        data[[dataframe]]
      } else {
        data[[dataframe]] <- data[[dataframe]] %>% left_join(target_data, by = target$id_variable) %>% filter(!is.na(!!rlang::sym(target$target_variable)))
      }
      data[[dataframe]] <- data[[dataframe]] %>% filter(!is.na(!!rlang::sym(target$target_variable)))
      # Perform stratified cross-validation
      if(!is.null(n_fold)){
        resample <- rsample::vfold_cv(data[[dataframe]], v = n_fold, strata = target$target_variable)
        training_data <- lapply(1:n_fold, function(i)resample$splits[[i]]$data[resample$splits[[i]]$in_id, ])
        names(training_data) <- paste0("split", 1:n_fold)
        # Rank features
        ranked_features <- rank_features(training_data, target, filter_name = filter_name, n_threads = n_threads)
      } else {
        # Perform one time feature selection
        ranked_features <- rank_features(data[[dataframe]], target, filter_name = filter_name, n_threads = n_threads)
      }
      # Select features
      ranked_features$selected_features <- select_features(data[[dataframe]], ranked_features$ranking, target, cutoff_method = cutoff_method, cutoff_treshold = cutoff_treshold)
      filtered_data[[dataframe]] <- ranked_features$selected_features
      ranking_list[[dataframe]] <- ranked_features$ranking
    }
    
    filtered_data <-
      c(filtered_data, data[target$phenotype_df]) %>%
      reduce(full_join, by = c(target$target_variable, target$id_variable)) %>%
      select(-target$id_variable, -target$target_variable, everything())
    
    if (return_ranking_list) {
      return(list(filtered_data = filtered_data, ranking_list = ranking_list))
    } else {
      return(filtered_data)
    }
  } else if (selection_type == "combined"){
    # Perform stratified cross-validation
    if(!is.null(n_fold)){
      resample <- rsample::vfold_cv(data, v = n_fold, strata = target$target_variable)
      training_data <- lapply(1:n_fold, function(i) resample$splits[[i]]$data[resample$splits[[i]]$in_id, ])
      names(training_data) <- paste0("split", 1:n_fold)
      # Rank features
      ranked_features <- rank_features(training_data, target, filter_name = filter_name, n_threads = n_threads)
    } else {
      # Perform one time feature selection
      ranked_features <- rank_features(data, target, filter_name = filter_name, n_threads = n_threads)
    }
    # Select features
    ranked_features$selected_features <- select_features(data, ranked_features$ranking, target, cutoff_method = cutoff_method, cutoff_treshold = cutoff_treshold)
    # Return the filtered data
    filtered_data <- ranked_features$selected_features
    if (return_ranking_list) {
      return(list(filtered_data = filtered_data, ranking_list = ranked_features$ranking))
    } else {
      return(filtered_data)
    }
  } else {
    stop("Selection method not found!")
  }
  
}
