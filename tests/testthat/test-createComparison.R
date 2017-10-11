# Set Variables 
tm <- "quarter"
entry <- "entry_q"
id <- "indiv_id"

# Load data
load("model_output.rda")
lr_result <- model_output$lr_result

comparison_pool <- createComparison(lr_result, tm, entry, id)
# To save for the next test: save(comparison_pool, file = "comparison_pool.rda")

test_that("createComparison should run without error", {
  expect_error(createComparison(lr_result, tm, entry, id), NA)
})
test_that("createComparison should not return an empty dataframe", {
  expect_equal(dim(createComparison(lr_result, tm, entry, id))[1] > 0, TRUE)
})
