library(testthat)

# Test connect_datasets function
test_that("connect_datasets function works as expected", {

  # Create some example datasets
  df1 <- data.frame(x = rnorm(10), y = rnorm(10))
  df2 <- data.frame(x = rnorm(10), y = rnorm(10), z = rnorm(10))

  # Call the function with the example datasets
  data <- connect_datasets(df1, df2, remove_original_data = F)

  # Check that the data object is a list
  expect_type(data, "list")

  # Check that the list contains the expected datasets
  expect_equal(names(data), c("df1", "df2"))
  expect_equal(data$df1, df1)
  expect_equal(data$df2, df2)
})

test_that("connect_datasets removes original data when remove_original_data is TRUE", {

  # Create some example datasets
  df1 <- data.frame(x = rnorm(10), y = rnorm(10))
  df2 <- data.frame(x = rnorm(10), y = rnorm(10), z = rnorm(10))

  # Call connect_datasets with remove_original_data = TRUE
  data <- connect_datasets(df1, df2, remove_original_data = TRUE)

  # Check that original data has been removed from global environment
  expect_false(exists("df1"))
  expect_false(exists("df2"))

})

