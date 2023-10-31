library(testthat)

# Assuming you have some sample RDS files in a directory named "my_experiment"
# For this example, you can create two dummy RDS files to represent the metrics data

# Create dummy RDS files for testing
experiment_name <- "test_experiment"
temp_dir <- paste(getwd(), experiment_name, sep = "/")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}
dummy_data1 <- data.frame(experiment = "A", metric1 = 10, metric2 = 20)
dummy_data2 <- data.frame(experiment = "B", metric1 = 15, metric2 = 25)
saveRDS(dummy_data1, file.path(temp_dir, "experimentA_metrics.Rds"))
saveRDS(dummy_data2, file.path(temp_dir, "experimentB_metrics.Rds"))

# Define a unit test
test_that("read_model_data reads and combines metrics data", {
  # Call the function to read and combine the metrics data
  metrics_data <- read_model_data("test_experiment", directory = getwd())

  # Check if the result contains the expected columns
  expect_true("metric1" %in% names(metrics_data))
  expect_true("metric2" %in% names(metrics_data))

  # Check the number of rows in the result
  expect_equal(nrow(metrics_data), 2)

  # Check the content of the result (assuming the metrics data is as expected)
  expect_equal(metrics_data$experiment, c("A", "B"))
  expect_equal(metrics_data$metric1, c(10, 15))
  expect_equal(metrics_data$metric2, c(20, 25))

  # Clean up: remove temporary files
  unlink(temp_dir, recursive = TRUE)
})
