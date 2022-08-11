

nested_filtering <- function(data, target, nfold = 5, filter_name = "auc", n_threads = 1){
  # cl <- parallel::makeCluster(parallel::detectCores()/4)
  # browser()

    # parallel::mclapply(cl, 1:length(data), function(i) {
    sapply(names(data), function(dataframe){

      resample <-  rsample::vfold_cv(data[[dataframe]], v = nfold, strata = target$target_variable)
      training_data <- lapply(1:nfold, function(i) resample$splits[[i]]$data[resample$splits[[i]]$in_id,])
      names(training_data) <- paste0("split", 1:nfold)
      ranked_features <- rank_features(training_data, target, filter_name = filter_name, n_threads = n_threads)
      ranked_features$selected_features <- select_features(data[[dataframe]], ranked_features$ranking, target, "top_n", 20)

    }, USE.NAMES = TRUE, simplify = F)
  # parallel::stopCluster(cl)
  # ans <- Reduce("cbind", matrix_of_rankings)

}
