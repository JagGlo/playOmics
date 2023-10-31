library(testthat)

# Define the test case
test_that("define_target function creates a target list with correct attributes", {
  # Set up test inputs
  target_variable_name <- "diabetes"
  positive_class_name <- "Yes"
  phenotype_df_name <- "clinical_data"
  id_variable_name <- "ID"

  # Run the function
  target1 <- define_target(phenotype_df_name, id_variable_name, target_variable_name, positive_class_name)

  # Check that the returned object is a list
  expect_true(is.list(target1))

  # Check that the list contains the correct attributes
  expect_true("phenotype_df" %in% names(target1))
  expect_true("id_variable" %in% names(target1))
  expect_true("target_variable" %in% names(target1))
  expect_true("positive_class" %in% names(target1))

  # Check that the attribute values are correct
  expect_identical(target1$phenotype_df, "clinical_data")
  expect_equal(target1$id_variable, "ID")
  expect_equal(target1$target_variable, "diabetes")
  expect_equal(target1$positive_class, "Yes")

  # test without positive class
  target2 <- define_target(phenotype_df_name, id_variable_name, target_variable_name, positive_class_name = NULL)
  expect_identical(target2$phenotype_df, "clinical_data")
  expect_equal(target2$id_variable, "ID")
  expect_equal(target2$target_variable, "diabetes")
  expect_null(target2$positive_class)
})
