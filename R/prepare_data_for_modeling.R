#' Prepare data for modelling
#'
#' This function preprocesses the data for modelling, including one-hot encoding
#' of non-numeric columns, translating logical columns into numbers, converting the ID variable to a character
#' and the target variable to a factor. It also adds the dataset name to
#' each variable to distinguish data and relevels the target variable to set the positive class.
#' Additionally, it removes highly correlated predictors (default).
#'
#' @param data A named list of data.frames containing the data to be prepared for modelling.
#' @param target A named list with the following elements:
#'   * phenotype_df: A character string specifying the dataframe containing the target variable.
#'   * id_variable: A character string specifying the ID variable.
#'   * target_variable: A character string specifying the target variable.
#'   * positive_class: (Optional) The value of the target variable representing the positive class.
#' @param remove_correlated_features A logical value indicating whether to remove correlated features (default is FALSE).
#' @return A named list of processed dataframes.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- list(
#'   phenotype_df = data.frame(id = 1:100, target = sample(c(0, 1), 100, replace = TRUE),
#'       feature1 = rnorm(100)),
#'   another_df = data.frame(id = 1:100, col1 = rnorm(100),
#'       col2 = factor(sample(letters[1:3], 100, replace = TRUE)))
#' )
#'
#' target <- list(
#'   phenotype_df = "phenotype_df",
#'   id_variable = "id",
#'   target_variable = "target",
#'   positive_class = 1
#' )
#'
#' prepared_data <- prepare_data_for_modelling(data, target)
#' }
# Define a function that prepares data for modeling
prepare_data_for_modelling <- function(data, target, remove_correlated_features = FALSE) {
  data <-
    sapply(names(data), function(dataframe) {
      logger::log_info("Preparing {dataframe} dataframe")

      df <- data[[dataframe]]

      not_unique <- function(x) {
        x <- x[!is.na(x)]
        length(unique(x)) >= 2
      }
      non_numeric_vars <- setdiff(names(df %>% select(intersect(which(sapply(., class) != "numeric"), which(sapply(., not_unique))))), c(target$id_variable, target$target_variable))
      # If there are any non-numeric columns, perform one-hot encoding
      if (length(non_numeric_vars) > 0) {
        df <-
          df %>%
          # all input variables for dummy steps must be factors
          mutate_if(is.logical, as.factor) %>%
          # Set up a recipe for one-hot encoding
          recipes::recipe() %>%
          recipes::update_role(everything()) %>%
          recipes::update_role(target$target_variable, new_role = "outcome") %>%
          recipes::update_role(target$id_variable, new_role = "id variable") %>%
          recipes::step_dummy(all_of(non_numeric_vars), one_hot = TRUE) %>%
          recipes::prep() %>%
          # Apply the recipe to the data
          recipes::bake(df) %>%
          # Convert logical columns to integer columns
          mutate_if(is.logical, as.integer)
      }
      # Remove highly correlated predictors
      if (remove_correlated_features){
        df <-
          df %>%
          # Set up a recipe for one-hot encoding
          recipes::recipe() %>%
          recipes::update_role(everything()) %>%
          # recipes::update_role(target$target_variable, new_role = "outcome") %>%
          recipes::update_role(target$id_variable, new_role = "id variable") %>%
          # Remove highly correlated predictors
          recipes::step_corr(recipes::all_numeric_predictors(), threshold = 0.9) %>%
          recipes::prep() %>%
          # Apply the recipe to the data
          recipes::bake(df)
        logger::log_info("Removed highly correlated predictors")
      }

      # Convert ID variable to a character
      df <- df %>% mutate(!!target$id_variable := as.character(!!sym(target$id_variable)))
      # Convert target variable to a factor
      if (dataframe == target$phenotype_df && !is.null(target$target_variable)) {
        df <- df %>%
          mutate(!!target$target_variable := as.factor(!!sym(target$target_variable))) %>%
          relocate(target$id_variable, target$target_variable)

        # Relevel target variable
        if (!is.null(target$positive_class) && !is.null(target$target_variable)) {
          df <- df %>% mutate(!!target$target_variable := fct_relevel(!!sym(target$target_variable), as.character(target$positive_class)))
        }
      } else {
        df <- df %>% relocate(target$id_variable)
      }

      # Add dataset name for each variable to distinct data
      col_names <- c(target$id_variable, setdiff(names(df), target$id_variable))
      col_names <- sapply(col_names, function(x){
        # if target_variable is not null and x is not target_variable, return x
        if(!is.null(target$target_variable))
           if(x == target$target_variable) return(x)
        if(!is.null(target$id_variable))
          if(x == target$id_variable) return(x)

          paste0(x, " [", dataframe, "]")

               })
      setNames(df, col_names)
    }, simplify = FALSE, USE.NAMES = TRUE)
}
