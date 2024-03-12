.onAttach <- function(libname, pkgname) {
  # Ensure the 'conflicted' package is available
  if (!requireNamespace("conflicted", quietly = TRUE)) {
    stop("The 'conflicted' package is not available but is required. Please install it.", call. = FALSE)
  }

  # Attach the 'conflicted' package without adding it to the search path
  # This avoids the package attaching message
  requireNamespace("conflicted", quietly = TRUE)
  conflicted::conflict_prefer("explain", "DALEX")

  # Optionally, notify the user
  message("Using 'conflicted' to prefer 'DALEX::explain()' over others.")
}
