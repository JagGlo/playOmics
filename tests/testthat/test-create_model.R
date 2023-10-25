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

# 1. Test basic functionality with CV
test_that("create_model works with CV validation", {
  results <- create_model(train_data, test_data, target, validation_method = "cv")
  expect_s3_class(results, "tbl_df")
  expect_true("train_accuracy" %in% names(results))
  expect_true("test_accuracy" %in% names(results))
  unlink(results$model_dir, recursive = TRUE)
})

# 2. Test basic functionality with subsampling
test_that("create_model works with subsampling validation", {
  results <- create_model(train_data, test_data, target, validation_method = "subsampling")
  expect_s3_class(results, "tbl_df")
  expect_true("train_accuracy" %in% names(results))
  expect_true("test_accuracy" %in% names(results))
  unlink(results$model_dir, recursive = TRUE)
})

# 3. Test with empty test_data
test_that("create_model works with empty test_data", {
  results <- create_model(train_data, data.frame(), target, validation_method = "cv")
  expect_s3_class(results, "tbl_df")
  expect_true("train_accuracy" %in% names(results))
  expect_false("test_accuracy" %in% names(results))
  unlink(results$model_dir, recursive = TRUE)
})

# 4. Test error handling
test_that("create_model handles errors gracefully", {
  faulty_data <- train_data
  faulty_data$target <- as.numeric(faulty_data$target) # target should be character or factor
  results <- create_model(faulty_data, test_data, target, validation_method = "cv")
  expect_s3_class(results, "tbl_df")
  expect_true("model_id" %in% names(results))
  expect_false("train_accuracy" %in% names(results))
  unlink(paste(getwd(), results$model_id, sep = "/"), recursive = TRUE)
})

# 5. Test logging
test_that("create_model handles logging correctly", {
  results <- create_model(train_data, test_data, target, validation_method = "cv", log_experiment = TRUE, directory = getwd())
  expect_true(file.exists(paste0(getwd(), "/", results$model_id[1], "/model_logs.json")))
  unlink(results$model_dir, recursive = TRUE)
})

# 6. Test with logging set to FALSE
test_that("Function should work with logging set to FALSE", {
  results <- create_model(train_data, test_data, target, validation_method = "cv", log_experiment = FALSE, directory = getwd())
  expect_false(file.exists(paste0(getwd(), "/", results$model_id[1], "/model_logs.json")))

  # Assert: Check the structure of the result
  expect_true("model_name" %in% names(results))
  expect_true("train_mcc" %in% names(results))
  expect_true("train_recall" %in% names(results))
})

# 7. Test DALEX explainer
test_that("create_model handles DALEX explainer correctly", {
  results <- create_model(train_data, test_data, target, validation_method = "cv", explain = TRUE, directory = getwd())
  expect_true(file.exists(paste0(getwd(), "/", results$model_id[1], "/explainer.Rds")))
  unlink(results$model_dir, recursive = TRUE)
})
