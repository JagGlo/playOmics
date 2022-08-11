read_all_data_for_dir <- function(dir){
  dirs <- list.dirs(dir, full.names = F, recursive = F)
  tryCatch({
  my_results <-
    lapply(dirs, function(dir_name){
      if(dir_name == "params"){
        files <- list.files(paste(dir, dir_name, sep = "/"))
        read <-
          lapply(1:length(files), function(i){
            read_file(paste(dir, dir_name, files[i], sep = "/"))
          })
        names(read) <- files
        df <-
          read %>%
          unlist(recursive = FALSE) %>%
          enframe() %>%
          unnest() %>%
          spread(name, value)
        return(df)
      } else if(dir_name == "metrics"){
        files <- list.files(paste(dir, dir_name, sep = "/"))
        read <-
          lapply(1:length(files), function(i){
            file <- read_file(paste(dir, dir_name, files[i], sep = "/"))
            result <- strsplit(file, " ")[[1]][2]
            return(result)
          })
        names(read) <- files
        df <-
          read %>%
          unlist(recursive = FALSE) %>%
          enframe() %>%
          unnest() %>%
          spread(name, value)
        return(df)
      } else if(dir_name == "artifacts"){
        directory <- as.list(list.files(paste(dir, dir_name, sep = "/"), full.names = T))
        names(directory) <- list.files(paste(dir, dir_name, sep = "/")) %>% stringr::str_remove("\\.rds")
        return(directory)
      } else{
        return(NULL)
      }
    })
  names(my_results) <- dirs
  res <- my_results[c("params", "metrics", "artifacts")] %>% flatten() %>% bind_rows()
  return(res)
  }, error = function(error_condition) {
    return(NULL)
  })
}

#' Get experiment metrics
#'
#'
#'
#' @param
#
#' @return
#'
#' @examples
#' @export

get_metrics_for_all_data <- function(experiment_name, n_cores = detectCores()/4){

  cl <- parallel::makeForkCluster(n_cores)

  models_to_read <- list.dirs(paste(here::here(experiment_name), "mlflow", "0", sep ="/"), recursive = F)

  results <-
    pbapply::pblapply(1:length(models_to_read), function(model){
    read_all_data_for_dir(models_to_read[model])
  }, cl = cl) %>%
    bind_rows()

  parallel::stopCluster(cl)
  rm(cl)

  return(results)
}
