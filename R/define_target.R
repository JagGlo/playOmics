#' Define a target for a classification problem
#'
#' Given the names of the input data frame, id variable, target variable,
#' and positive class variable, this function creates a list containing
#' these objects for use in a classification task.
#'
#' @param phenotype_df_name Name of a dataframe containing the phenotype/clinical data.
#' @param id_variable_name Name of the ID variable in the phenotype data frame.
#' @param target_variable_name Name of the target variable in the phenotype data frame.
#' @param positive_class_name Name of the positive class indication in the target variable.
#'
#' @return A list containing the most important information about defined target
#'
#' @examples
#' my_target <- define_target(phenotype_df_name = "clinical_data", id_variable_name = "patient_id",
#'     target_variable_name = "survived", positive_class_name = "1")
#'
#' @export

define_target <- function(phenotype_df_name, id_variable_name, target_variable_name, positive_class_name = NULL) {
  target <- list(
    phenotype_df = phenotype_df_name,
    id_variable = id_variable_name,
    target_variable = target_variable_name,
    positive_class = positive_class_name
  )

  return(target)
}
