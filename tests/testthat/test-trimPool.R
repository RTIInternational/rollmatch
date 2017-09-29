
# Load data
load("output.rda")
load("comparison_pool.rda")

lr_result <- output$lr_result
trimmed_pool <- trimPool(caliper = .2, data_pool = comparison_pool,
                            lr_result = lr_result)
# To save for next test: save(trimmed_pool, file = "trimmed_pool.rda")

# Run Tests!
test_that("trimPool should run without errors", {
  expect_equal(dim(trimmed_pool)[1], 30050)
})
test_that("trimPool 'caliper' should be numeric", {
  expect_error(trimPool(".2", comparison_pool, lr_result),
               "non-numeric argument to binary operator")
})
test_that("trimPool 'data_pool' & 'treatment_data' must be dataframes", {
  expect_error(trimPool(.2, "comparison_pool", lr_result),
               "argument is of length zero")
})
