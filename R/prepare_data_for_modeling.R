#' Prepare data for modelling
#'
#' This function preprocesses the data for modelling, including one-hot encoding
#' of non-numeric columns, converting the ID variable to a character, and
#' converting the target variable to a factor. It also adds the dataset name to
#' each variable to distinguish data.
#'
#' @param data A named list of data.frames containing the data to be prepared for modelling.
#' @param target A named list with the following elements:
#'   * phenotype_df: A character string specifying the dataframe containing the target variable.
#'   * id_variable: A character string specifying the ID variable.
#'   * target_variable: A character string specifying the target variable.
#'   * positive_class: (Optional) The value of the target variable representing the positive class.
#' @return A named list of processed dataframes.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- list(
#'   phenotype_df = data.frame(id = 1:100, target = sample(c(0, 1), 100, replace = TRUE), feature1 = rnorm(100)),
#'   another_df = data.frame(id = 1:100, col1 = rnorm(100), col2 = factor(sample(letters[1:3], 100, replace = TRUE)))
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
prepare_data_for_modelling <- function(data, target) {

  data <-
    sapply(names(data), function(dataframe) {

      logger::log_info("Preparing {dataframe} dataframe")

      df <- data[[dataframe]]

      non_numeric_vars <- setdiff(names(df %>% select(which(sapply(., class) != "numeric"))), c(target$id_variable, target$target_variable))
      # If there are any non-numeric columns, perform one-hot encoding
      if (length(non_numeric_vars) > 0) {
        df <-
          df %>%
          # Set up a recipe for one-hot encoding
          recipes::recipe(~.) %>%
          recipes::step_dummy(non_numeric_vars, one_hot = T) %>%
          recipes::prep() %>%
          # Apply the recipe to the data
          recipes::bake(df) %>%
          # Convert logical columns to integer columns
          mutate_if(is.logical, as.integer)
      }

      # Convert ID variable to a character
      df <- df %>% mutate(!!target$id_variable := as.character(!!sym(target$id_variable)))
      # Convert target variable to a factor
      if (dataframe == target$phenotype_df && !is.null(target$target_variable)) {
        df <- df %>%
          mutate(!!target$target_variable := as.factor(!!sym(target$target_variable))) %>%
          relocate(target$id_variable, target$target_variable)

          #Relevel target variable
          if (!is.null(target$positive_class)) {
            df <- df %>% mutate(!!target$target_variable := fct_relevel(!!sym(target$target_variable), as.character(target$positive_class)))
          }

      } else {
        df <- df %>% relocate(target$id_variable)
      }

      # Add dataset name for each variable to distinct data
      col_names <- c(target$id_variable, setdiff(names(df), target$id_variable))
      col_names <- ifelse(col_names == target$target_variable | col_names == target$id_variable, col_names, paste0(col_names, " [", dataframe, "]"))
      setNames(df, col_names)

    }, simplify = FALSE, USE.NAMES = TRUE)

}
