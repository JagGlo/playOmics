library(testthat)
library(rsample)

# Test data preparation
sample_data <- list(
  phenotype_df = data.frame(
    id = 1:100,
    target_variable = sample(c(0, 1), 100, replace = TRUE),
    feature1 = rnorm(100),
    feature2 = rnorm(100)
  ),
  another_df = data.frame(
    id = 1:100,
    col1 = rnorm(100),
    col2 = rnorm(100)
  )
)

target <- list(
  phenotype_df = "phenotype_df",
  id_variable = "id",
  target_variable = "target_variable"
)

target_no_target <- list(
  phenotype_df = "phenotype_df",
  id_variable = "id",
  target_variable = NULL
)

prop <- 4/5

test_that("split_data_into_train_test works correctly", {
  split_result <- split_data_into_train_test(sample_data, target, prop)
  expect_type(split_result, "list")
  expect_named(split_result, c("train_data", "test_data"))

  expect_s3_class(split_result$train_data$phenotype_df, "data.frame")
  expect_s3_class(split_result$test_data$phenotype_df, "data.frame")
  expect_s3_class(split_result$train_data$another_df, "data.frame")
  expect_s3_class(split_result$test_data$another_df, "data.frame")

  expect_equal(nrow(split_result$train_data$phenotype_df), prop * nrow(sample_data$phenotype_df), tolerance = 0.05 * prop * nrow(sample_data$phenotype_df))
  expect_equal(nrow(split_result$test_data$phenotype_df), (1 - prop) * nrow(sample_data$phenotype_df), tolerance = 0.05 * (1 - prop) * nrow(sample_data$phenotype_df))

  expect_equal(nrow(split_result$train_data$another_df), prop * nrow(sample_data$another_df), tolerance = 0.05 * prop * nrow(sample_data$another_df))
  expect_equal(nrow(split_result$test_data$another_df), (1 - prop) * nrow(sample_data$another_df), tolerance = 0.05 * (1 - prop) * nrow(sample_data$another_df))

  train_id <- split_result$train_data$phenotype_df[, target$id_variable]
  test_id <- split_result$test_data$phenotype_df[, target$id_variable]

  expect_equal(length(intersect(train_id, test_id)), 0)
  expect_equal(sort(union(train_id, test_id)), sort(sample_data$phenotype_df$id))

  # Test for target variable NULL case
  split_result <- split_data_into_train_test(sample_data, target_no_target, prop)
  expect_equal(nrow(split_result$train_data$phenotype_df), prop * nrow(sample_data$phenotype_df), tolerance = 0.05 * prop * nrow(sample_data$phenotype_df))
  expect_equal(nrow(split_result$test_data$phenotype_df), (1 - prop) * nrow(sample_data$phenotype_df), tolerance = 0.05 * (1 - prop) * nrow(sample_data$phenotype_df))
  expect_equal(length(intersect(train_id, test_id)), 0)
  expect_equal(sort(union(train_id, test_id)), sort(sample_data$phenotype_df$id))
})

