library(testthat)
library(tidyverse)

# Sample data
set.seed(123)
train_data_1 <- data.frame(
  id = 1:100,
  x1 = rnorm(100),
  x2 = runif(100),
  target = as.factor(rbinom(100, 1, 0.5))
)

train_data_2 <- data.frame(
  id = 1:80,
  y1 = rnorm(80),
  y2 = runif(80),
  target = as.factor(rbinom(80, 1, 0.5))
)

train_data <- connect_datasets(train_data_1, train_data_2)

test_data_1 <- data.frame(
  id = 101:150,
  x1 = rnorm(50),
  x2 = runif(50),
  target = as.factor(rbinom(50, 1, 0.5))
)

test_data_2 <- data.frame(
  id = 101:130,
  y1 = rnorm(30),
  y2 = runif(30),
  target = as.factor(rbinom(30, 1, 0.5))
)

test_data <- connect_datasets(test_data_1, test_data_2)

target <- define_target(train_data, id_variable = "id", target_variable = "target")

test_that("create_multiple_models produces correct output", {
  # Execute the function
  results <- create_multiple_models(
    experiment_name = "test_experiment",
    train_data = train_data,
    test_data = test_data,
    target = target,
    n_max = 2,
    trim_models = TRUE,
    trim_metric = "train_mcc",
    trim_threshold = 0.3,
    n_cores = 2,
    directory = getwd()
  )

  # Test assertions
  expect_type(results, "list")
  expect_gt(length(results), 0)
  expect_type(results[[1]], "list")
  expect_length(results[[1]], 6)

  # Clean up the directory
  unlink(paste(getwd(), "test_experiment", sep = "/"), recursive = TRUE)
})
