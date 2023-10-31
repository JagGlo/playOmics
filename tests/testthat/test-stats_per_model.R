library(testthat)
library(dplyr)

test_that("count_stats_per_model works correctly", {

  # Test data
  df <- data.frame(target_var = c("A", "A", "B", "B"),
                   num_var = c(1, 2, 2.5, 3),
                   fct_var = c("Yes", "No", "Yes", "No"))

  target_list <- list(target_variable = "target_var")

  result <- count_stats_per_model(df, target_list)

  # Check if the result is a dataframe
  expect_s3_class(result, "data.frame")

  # Check if the result has the correct number of rows
  expect_equal(nrow(result), 2)

  # Check if the result has the correct number of columns
  expect_equal(ncol(result), 3)

  # Check if the result has the correct row names
  expect_equal(result$variable, c("fct_var", "num_var"))

})
