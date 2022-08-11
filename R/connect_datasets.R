#' Connect datasets
#'
#' @param ... names of datasets that should be concatenated
#' @param remove_original_data If set to TRUE [default], it will remove all dataframes passed as arguments.
#'
#' @return Named list; dataset names are further used as a list's elements names. Common ID must be placed as a first column for all data.
#'
#' @examples
#' my_data <-connect_datasets(clinical_data, eeg_data, proteomics_data, miRNA_data, RNA_data)
#'
#' @export

connect_datasets <- function(..., remove_original_data = TRUE){

  varnames <- lapply(substitute(list(...))[-1], deparse) %>% unlist()

  data <- list(...)

  names(data) <- varnames

  if(remove_original_data){
    rm(list= varnames, envir = .GlobalEnv)
  }

  return(data)
}


