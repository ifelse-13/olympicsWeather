test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_data <- tibble(
  hourly = list(
    list("2022-01-01T00:00:00Z", "2022-01-01T01:00:00Z", "2022-01-01T02:00:00Z", "2022-01-01T03:00:00Z", "2022-01-01T04:00:00Z"),
    list(20, 21, 19, 22, 18),
    list(18, 20, 17, 21, 16),
    list(0.8, 0.5, 0.2, 0.9, 0.3),
    list(5, 3, 0, 8, 2)
  )
)
library(testthat)

testthat::test_that("unnest_response returns the correct number of rows", {
  result <- unnest_response(test_data)
  expect_equal(nrow(result), 5)
})

test_that("unnest_response returns correct values in temperature_celsius column", {
  result <- unnest_response(test_data)
  expected_values <- list(20, 21, 19, 22, 18)
  expect_equal(result$temperature_celsius, expected_values)
})

test_that("unnest_response returns correct column names", {
  result <- unnest_response(test_data)
  expected_column_names <- c("date_heure", "temperature_celsius", "temperature_ressentie_celsius", "precipitation_proba", "precipitation")
  expect_identical(colnames(result), expected_column_names)
})


test_that("unnest_response returns the correct number of columns", {
  result <- unnest_response(test_data)
  expected_num_columns <- 5
  expect_equal(ncol(result), expected_num_columns)
})
