library(testthat)
library(dplyr)

# Sample data for testing
set.seed(123)
data1 <- data.frame(
  SampleID = 1:5,
  Variable1 = c(3, 6, 2, 8, 4),
  Variable2 = c("A", "B", "A", "C", "B"),
  Variable3 = factor(c("X", "Y", "Z", "X", "Y"))
)

data2 <- data.frame(
  SampleID = 1:5,
  Variable1 = c(2, 4, 6, 8, 10),
  Variable2 = c("A", "B", "C", "D", "E"),
  Variable3 = factor(c("X", "Y", "Z", "X", "Y"))
)

my_data <- list(data1 = data1, data2 = data2)

# Define unit tests
test_that("data_summary summarizes data structure", {
  # Get the summary of data structure
  summary <- data_summary(my_data)

  # Check if the result is a data frame
  expect_s3_class(summary, "data.frame")

  # Check if the summary contains expected columns
  expect_true("Dataset.name" %in% names(summary))
  expect_true("Number.of.samples" %in% names(summary))
  expect_true("Number.of.variables" %in% names(summary))
  expect_true("Numeric.columns" %in% names(summary))
  expect_true("Character.columns" %in% names(summary))
  expect_true("Factor.columns" %in% names(summary))

  # Check the content of the summary
  expect_equal(nrow(summary), 2) # Two datasets
  expect_equal(summary$Dataset.name, c("data1", "data2"))
})

test_that("check_data calculates basic statistics for numeric data", {
  # Check basic statistics for numeric data in data1
  numeric_summary <- check_data(data1)

  # Check if the result is a list
  expect_type(numeric_summary, "list")

  # Check if the list contains two data frames
  expect_equal(length(numeric_summary), 2)
  expect_s3_class(numeric_summary$non_numeric_data, "data.frame")
  expect_s3_class(numeric_summary$numeric_data, "data.frame")

  # Check the content of the numeric data summary
  expect_true("numeric_variables" %in% names(numeric_summary$numeric_data))
  expect_length(numeric_summary$numeric_data, 12)
})

test_that("plot_stats generates boxplots for numeric data", {
  # Generate boxplots for 'median' metric in data1
  plots <- plot_stats(data1, median)

  # Check if the result is a list containing a plot and a table
  expect_type(plots, "list")
  expect_length(plots, 2)

  # Check if the plot is a ggplot object
  expect_true(is.ggplot(plots[[2]]))
})
