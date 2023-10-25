library(testthat)

# Sample data for testing
set.seed(123)
data1 <- data.frame(
  SampleID = 1:5,
  Variable1 = c("A", "B", "C", "D", "E")
)

data2 <- data.frame(
  SampleID = 3:7,
  Variable1 = c("C", "D", "E", "F", "G")
)

my_data <- list(data1 = data1, data2 = data2)

# Define a unit test
test_that("plot_coverage generates a plot for data coverage", {
  # Generate a plot for data coverage
  plot <- plot_coverage(my_data)

  # Check if the result is a ggplot object
  expect_true(is.ggplot(plot))
})
