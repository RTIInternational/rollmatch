context("Testing reduce_data() functionality")

treat <- "treat"
tm <- "quarter"
entry <- "entry_q"
id <- "indiv_id"
lookback <- 1

data(package = "rollmatch", "rem_synthdata_small")

# TEST THE INPUTS: full set of tests for most inputs: test-rollmatch.R script.

# Testing: data
test_that("reduce_data 'data' is a data.frame", {
  expect_error(reduce_data(data = list(), treat = treat, tm = tm,
                           entry = entry, id = id, lookback = lookback),
               "'data' input parameter must be a data.frame")
  expect_error(reduce_data(data = "data.frame", treat = treat, tm = tm,
                           entry = entry, id = id, lookback = lookback),
               "'data' input parameter must be a data.frame")
})
# Testing: treat
test_that("rollmatch 'treat' is a string and is in the dataset", {
  expect_error(reduce_data(data = rem_synthdata_small, treat = 12,
                           tm = list(), entry = entry, id = id, lookback = 1),
               "'treat' parameter must be a string")
})
# Testing: tm
test_that("reduce_data 'tm' is a string", {
  expect_error(reduce_data(data = rem_synthdata_small, treat = treat,
                           tm = list(), entry = entry, id = id, lookback = 1),
               "'tm' parameter must be a string")
})
# Testing: entry
test_that("reduce_data 'entry' is a string and is in the dataset", {
  expect_error(reduce_data(data = rem_synthdata_small, treat = treat,
                           tm = tm, entry = 12, id = id, lookback = 1),
               "'entry' parameter must be a string")
})
# Testing: id
test_that("rollmatch 'id' is a string and must be a column in the data.frame", {
  expect_error(reduce_data(data = rem_synthdata_small, treat = treat,
                           tm = tm, entry = entry, id = 12, lookback = 1),
               "'id' parameter must be a string")
})
# Testing: lookback
test_that("rollmatch 'lookback' is of type numeric", {
  expect_error(reduce_data(data = rem_synthdata_small, treat = treat,
                           tm = tm, entry = entry, id = id, lookback = "fizz"),
               "'lookback' parameter must be of type numeric")
})


# Test the output
reduced_data <- reduce_data(data = rem_synthdata_small, treat = "treat",
                            tm = "quarter", entry = "entry_q",
                            id = "indiv_id", lookback = lookback)

test_that("reduce_data produces desired results for practice dataset.", {
  expect_equal(dim(reduced_data), c(4030, 20))
  expect_equal(reduced_data$indiv_id[1], 75722)
})
