#' Split Data into Training and Testing Sets
#'
#' This function splits the given data into training and testing sets using stratified sampling if a target variable is provided.
#' It also splits other dataframes in the list based on the same train and test ID subsets.
#'
#' @param data A named list of dataframes to be split, where one of the dataframes should be named as the target$phenotype_df.
#' @param target A named list with elements 'phenotype_df', 'id_variable', and 'target_variable' (optional).
#'        'phenotype_df' should match one of the names in the 'data' list, 'id_variable' should be a column in that dataframe,
#'        and 'target_variable' should be a column for stratification (if required).
#' @param prop A numeric value between 0 and 1 representing the proportion of the data to be used for the training set (default is 4/5).
#'
#' @return A list with two named elements, 'train_data' and 'test_data', each containing the corresponding split dataframes.
#' @export
#'
#' @examples
#' # Test data preparation
#' sample_data <- list(
#'   phenotype_df = data.frame(
#'     id = 1:100,
#'     target_variable = sample(c(0, 1), 100, replace = TRUE),
#'     feature1 = rnorm(100),
#'     feature2 = rnorm(100)
#'   ),
#'   another_df = data.frame(
#'     id = 1:100,
#'     col1 = rnorm(100),
#'     col2 = rnorm(100)
#'   )
#' )
#' target <- list(
#'   phenotype_df = "phenotype_df",
#'   id_variable = "id",
#'   target_variable = "target_variable"
#' )
#' split_result <- split_data_into_train_test(sample_data, target, prop = 4/5)
split_data_into_train_test <- function(data, target, prop = 4 / 5) {
  # Initialize empty lists for train and test data
  train_data <- list()
  test_data <- list()

  # Check if target variable is specified
  if (!is.null(target$target_variable)) {
    # Perform stratified sampling based on the target variable
    data_split <- rsample::initial_split(data[[target$phenotype_df]], strata = target$target_variable, prop = prop)
    # Extract the training and the testing data from phenotype df
    train_data[[target$phenotype_df]] <- rsample::training(data_split)
    test_data[[target$phenotype_df]] <- rsample::testing(data_split)
  } else {
    # Perform random sampling without stratification
    data_split <- rsample::initial_split(data[[target$phenotype_df]], strata = NULL, prop = prop)
    # Extract the training and the testing data from phenotype df
    train_data[[target$phenotype_df]] <- rsample::training(data_split)
    test_data[[target$phenotype_df]] <- rsample::testing(data_split)
  }

  # Get the IDs of the training and testing data
  train_id <- pull(train_data[[target$phenotype_df]][target$id_variable])
  test_id <- pull(test_data[[target$phenotype_df]][target$id_variable])

  # Iterate over the data and select training/testing data from each dataframe
  lapply(names(data), function(dataframe) {
    if (dataframe != target$phenotype_df) { # Don't change the phenotype df
      # Subset the training data based on train_id
      train_data[[dataframe]] <<- data[[dataframe]][data[[dataframe]][[target$id_variable]] %in% train_id, ]
      # Subset the testing data based on test_id
      test_data[[dataframe]] <<- data[[dataframe]][data[[dataframe]][[target$id_variable]] %in% test_id, ]
    }
  })

  # Return a list containing train_data and test_data
  return(list(
    train_data = train_data,
    test_data = test_data
  ))
}
