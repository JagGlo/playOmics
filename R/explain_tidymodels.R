#' Overwrite the explain function for tidymodels from: https://github.com/ModelOriented/DALEXtra/blob/HEAD/R/explain_tidymodels.R

explain_tidymodels <-
  function(model,
           data = NULL,
           y = NULL,
           weights = NULL,
           predict_function = NULL,
           predict_function_target_column = NULL,
           residual_function = NULL,
           ...,
           label = NULL,
           verbose = TRUE,
           precalculate = TRUE,
           colorize = !isTRUE(getOption('knitr.in.progress')),
           model_info = NULL,
           type = NULL) {

    if (inherits(model, "workflow") && !model$trained) {
      stop("Only trained workflows can be passed to explain function")
    }

    # for classification models do not calculate residuals (default)
    # as this my rise some not needed warnings
    if (is.null(residual_function) && isTRUE(model$spec$mode == "classification")) {
      residual_function <- function(m, d, y, predict_function) 0
    }

    if (!is.null(type) && type == "class") {
      type = "classification"
      predict_function <- function(X.model, newdata, ...)
        predict(X.model, newdata, type = "class")
    }

    DALEX::explain(
      model,
      data = data,
      y = y,
      weights = weights,
      predict_function = predict_function,
      predict_function_target_column = predict_function_target_column,
      residual_function = residual_function,
      ...,
      label = label,
      verbose = verbose,
      precalculate = precalculate,
      colorize = colorize,
      model_info = model_info,
      type = type
    )



  }
