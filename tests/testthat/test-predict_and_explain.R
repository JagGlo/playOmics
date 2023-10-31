library(testthat)
library(dplyr)
library(DALEXtra)

# Sample data
set.seed(123)
train_data <- data.frame(
  id = 1:100,
  x1 = rnorm(100),
  x2 = runif(100),
  target = as.factor(rbinom(100, 1, 0.5))
)

test_data <- data.frame(
  id = 101:150,
  x1 = rnorm(50),
  x2 = runif(50),
  target = as.factor(rbinom(50, 1, 0.5))
)

target <- define_target(train_data, id_variable = "id", target_variable = "target")

results <- create_model(train_data, test_data, target, validation_method = "cv")

library(testthat)

test_that("load_and_predict function works correctly", {
  test_data <- data.frame(id = 1, x1 = rnorm(1), x2 = runif(1))
  predictions <- load_and_predict(results$model_dir, test_data)

  # expect that predictions are a data frame with one row and three columns
  expect_equal(nrow(predictions), 1)
  expect_equal(ncol(predictions), 3)

  # expect that the first column is the ".pred_class"
  expect_equal(colnames(predictions)[1], ".pred_class")
})

test_that("load_and_explain function works correctly", {
  test_data <- data.frame(id = 1, x1 = rnorm(1), x2 = runif(1))
  explanations <- load_and_explain(results$model_dir, test_data)

  # expect that explanations is not null
  expect_true(!is.null(explanations))

  # expect that the explanations dataframe contains "variable" and "contribution" columns
  expect_true("variable" %in% colnames(explanations))
  expect_true("contribution" %in% colnames(explanations))
})

# Clean up
unlink(results$model_dir, recursive = TRUE)
