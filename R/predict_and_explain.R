#' @importFrom logger log_error
load_and_predict <- function(path, data) {
  tryCatch({
    model_path <- paste(path, "model.Rds", sep = "/")
    model <- readRDS(model_path)
    model(data)
  }, error = function(e) {
    logger::log_error(e[["message"]])
    NULL
  })
}

#' @importFrom logger log_error
load_and_explain <- function(path, data) {
  tryCatch({
    explainer_path <- paste(path, "explainer.Rds", sep = "/")
    explainer <- readRDS(explainer_path)
    explainer(data)
  }, error = function(e) {
    logger::log_error(e[["message"]])
    NULL
  })
}
