library(testthat)

testdata <- list(
  phenotype_df = data.frame(id = 1:100, target = sample(c(0, 1), 100, replace = TRUE), feature1 = rnorm(100)),
  another_df = data.frame(id = 1:100, col1 = rnorm(100), col2 = rnorm(100))
)

# Tests for nested_filtering
test_that("nested_filtering works as expected", {
  target <- list(
    phenotype_df = "phenotype_df",
    id_variable = "id",
    target_variable = "target"
  )

  # Test 1: Basic functionality
  result <- nested_filtering(testdata, target)
  expect_true(is.list(result))
  expect_true(length(result) == 2)

  # Test 2: Test with different filter_name
  result2 <- nested_filtering(testdata, target, filter_name = "anova")
  expect_true(is.list(result2))
  expect_true(length(result2) == 2)

  # Test 3: Test with different cutoff_method
  result3 <- nested_filtering(testdata, target, cutoff_method = "percentage", cutoff_treshold = 20)
  expect_true(is.list(result3))
  expect_true(length(result3) == 2)

  # Test 4: Test with different cutoff_threshold
  result4 <- nested_filtering(testdata, target, cutoff_treshold = 5)
  expect_true(is.list(result4))
  expect_true(length(result4) == 2)

  # Test 5: Test with filter_name not found in the mlr package
  expect_error(nested_filtering(testdata, target, filter_name = "another_filter"), "Element with key 'another_filter' not found in DictionaryFilter!")

  # Test 6: Test with cutoff_method not valid
  expect_error(nested_filtering(testdata, target, cutoff_method = "another_method"), "Cutoff method not found!")

  # Test 7: Test with different n_fold
  result7 <- nested_filtering(testdata, target, n_fold = 3)
  expect_true(is.list(result7))
  expect_true(length(result7) == 2)
})
