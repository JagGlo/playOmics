library(testthat)

# Sample data for testing
set.seed(123)
my_data <- data.frame(
  SampleID = 1:10,
  Variable1 = c(3, 6, 2, 8, 4, 1, 9, 7, 5, 10),
  Variable2 = c(NA, 2, 5, 1, NA, 7, 3, 6, 8, NA)
)

# Define unit tests
test_that("filter_below_threshold filters values below threshold", {
  # Filter values below a threshold of 5
  filtered_data <- filter_below_threshold(my_data, 5, 0.5)

  # Check that the expected columns are present
  expect_true("SampleID" %in% names(filtered_data))
  expect_true("Variable1" %in% names(filtered_data))
})

test_that("filter_missing filters rows with missing values", {
  # Filter rows with missing values using a threshold of 0.8
  filtered_data <- filter_missing(my_data, 0.8)

  # Check that the expected columns are present
  expect_true("SampleID" %in% names(filtered_data))
  expect_true("Variable1" %in% names(filtered_data))
  expect_false("Variable2" %in% names(filtered_data))
})
