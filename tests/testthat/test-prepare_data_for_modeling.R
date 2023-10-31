library(testthat)
library(dplyr)

# Define test data and target information
testdata <- list(
  phenotype_df = data.frame(id = 1:100, my_target = sample(c(0, 1), 100, replace = TRUE), feature1 = rnorm(100)),
  another_df = data.frame(id = 1:100, col1 = rnorm(100), col2 = factor(sample(letters[1:3], 100, replace = TRUE)))
)

target <- list(
  phenotype_df = "phenotype_df",
  id_variable = "id",
  target_variable = "my_target"
)

result <- prepare_data_for_modelling(testdata, target)

test_that("prepare_data_for_modelling returns a list", {
  expect_true(is.list(result))
})

test_that("prepare_data_for_modelling correctly prepares the data", {
  expect_true(all(sapply(result, function(x) is.data.frame(x))))
  expect_true(all(sapply(result, function(x) "id" %in% colnames(x))))
  expect_true(all(sapply(result, function(x) is.character(x$id))))

  expect_true("my_target" %in% colnames(result$phenotype_df))
  expect_true(is.factor(result$phenotype_df$my_target))
})

test_that("prepare_data_for_modelling performs one-hot encoding", {
  expect_true("col2_a [another_df]" %in% colnames(result$another_df))
  expect_true("col2_b [another_df]" %in% colnames(result$another_df))
  expect_true("col2_c [another_df]" %in% colnames(result$another_df))

  expect_true(is.double(result$another_df$`col2_a [another_df]`))
  expect_true(is.double(result$another_df$`col2_b [another_df]`))
  expect_true(is.double(result$another_df$`col2_c [another_df]`))
})

test_that("prepare_data_for_modelling relevels the target variable", {
  target <- list(phenotype_df = "phenotype_df", id_variable = "id", target_variable = "my_target", positive_class = 1)
  result <- prepare_data_for_modelling(testdata, target)

  expect_equal(levels(result$phenotype_df$my_target)[1], as.character(target$positive_class))
})
