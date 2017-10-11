# Set variables
formula <- as.formula(treat ~ qtr_pmt + yr_pmt + age)
tm <- "quarter"
entry <- "entry_q"
id <- "indiv_id"

# Load data
load("rem_synthdata_small.RData")
synthdata_clone <- rem_synthdata_small
synthdata_clone["tm_neg"] <- -synthdata_clone[tm]
synthdata_clone["entry_neg"] <- -synthdata_clone[entry]
synthdata_clone["id_neg"] <- -synthdata_clone[id]
synthdata_clone["entry_short"] <- 1

# Run Tests!
test_that("rollmatch 'formula' is of type language", {
  expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id), NA)
	expect_error(rollmatch(treat ~ qtr_pmt + yr_pmt + age, rem_synthdata_small,
	                       tm, entry, id), NA)
	expect_error(rollmatch("treat ~ qtr_pmt + yr_pmt + age", rem_synthdata_small,
	                       tm, entry, id),
	             "'formula' input parameter must be of type 'language'")
	expect_error(rollmatch(12, rem_synthdata_small, tm, entry, id),
	             "'formula' input parameter must be of type 'language'")
	})

test_that("rollmatch 'data' is a data.frame", {
	expect_error(rollmatch(formula, as.list(rem_synthdata_small), tm, entry, id),
	             "'data' input parameter must be a data.frame")
	expect_error(rollmatch(formula, 12, tm, entry, id),
	             "'data' input parameter must be a data.frame")
	expect_error(rollmatch(formula, "data.frame", tm, entry, id),
	             "'data' input parameter must be a data.frame")
	})

test_that("rollmatch 'tm' is a string", {
	expect_error(rollmatch(formula, rem_synthdata_small, 12, entry, id),
	             "'tm' input parameter must be a string")
	expect_error(rollmatch(formula, rem_synthdata_small, "fizz", entry, id),
	             "'tm' input parameter must be a column in the data.frame")
	})

test_that("rollmatch 'tm' is a column in the data.frame", {
	expect_error(rollmatch(formula, rem_synthdata_small, "buzz", entry, id),
	             "'tm' input parameter must be a column in the data.frame")
	})

test_that("rollmatch 'tm' must resolve to a vector of whole numbers", {
	expect_error(rollmatch(formula, rem_synthdata_small, "qtr_pmt", entry, id),
	             "'tm' input variable must resolve to a vector of whole numbers")
	})

test_that("rollmatch 'tm' must resolve to a vector of positive numbers", {
	expect_error(rollmatch(formula, synthdata_clone, "tm_neg", entry, id),
	           "'tm' input variable must resolve to a vector of positive numbers")
	})

test_that("rollmatch 'entry' is a string", {
	expect_error(rollmatch(formula, rem_synthdata_small, tm, 12, id),
	             "'entry' input parameter must be a string")
	expect_error(rollmatch(formula, rem_synthdata_small, tm, "fizz", id),
	             "'entry' input parameter must be a column in the data.frame")
	})

test_that("rollmatch 'entry' is a column in the data.frame", {
	expect_error(rollmatch(formula, rem_synthdata_small, tm, "buzz", id),
	             "'entry' input parameter must be a column in the data.frame")
	})

test_that("rollmatch 'entry' must resolve to a vector of whole numbers", {
	expect_error(rollmatch(formula, rem_synthdata_small, tm, "qtr_pmt", id),
	           "'entry' input variable must resolve to a vector of whole numbers")
	})

test_that("rollmatch 'entry' must resolve to a vector of positive numbers", {
	expect_error(rollmatch(formula, synthdata_clone, tm, "entry_neg", id),
	       "'entry' input variable must resolve to a vector of positive numbers")
	})

test_that("rollmatch 'id' is a string", {
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, 12),
	             "'id' input parameter must be a string")
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, "fizz"),
	             "'id' input parameter must be a column in the data.frame")
	})

test_that("rollmatch 'id' is a column in the data.frame", {
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, "buzz"),
	             "'id' input parameter must be a column in the data.frame")
	})

test_that("rollmatch 'lookback' is of type numeric", {
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       lookback = 2), NA)
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       lookback = "fizzbuzz"),
  	             "'lookback' input parameter must be of type numeric")
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
	                       lookback = TRUE),
	             "'lookback' input parameter must be of type numeric")
    })

test_that("rollmatch 'lookback' is a whole number", {
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
	                       lookback = 8.3),
	             "'lookback' input parameter must be a whole number")
	})

test_that("rollmatch 'lookback' must be between 1 and 10", {
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
	                       lookback = 1), NA)
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
	                       lookback = 4), NA)
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
	                       lookback = -10),
	             "'lookback' input parameter must be between 1 and 10")
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
	                       lookback = 15),
	             "'lookback' input parameter must be between 1 and 10")
	})

test_that("rollmatch 'lookback' is less than number of time periods in data", {
	expect_error(rollmatch(formula, synthdata_clone, tm, "entry_short", id,
	                       lookback = 2),
	             "'lookback' is greater than number of time periods in data set")
	})

test_that("rollmatch 'caliper' is of type numeric", {
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       caliper = 2), NA)
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       caliper = "fizzbuzz"),
  	             "'caliper' input parameter must be numeric")
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
	                       caliper = TRUE),
	             "'caliper' input parameter must be numeric")
    })

test_that("rollmatch 'caliper' is a positive number", {
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       caliper = 0), NA)
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       caliper = -2),
  	             "'caliper' input parameter must be a positive number")
    })

test_that("rollmatch 'weighted_pooled_stdev' is of type logical", {
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       weighted_pooled_stdev = TRUE), NA)
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       weighted_pooled_stdev = "fizzbuzz"),
  	       "'weighted_pooled_stdev' input parameter must be of type 'logical'")
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
	                       weighted_pooled_stdev = 12),
	         "'weighted_pooled_stdev' input parameter must be of type 'logical'")
    })

test_that("rollmatch 'num_matches' is of type numeric", {
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       num_matches = 2), NA)
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       num_matches = "fizzbuzz"),
  	             "'num_matches' input parameter must be numeric")
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
	                       num_matches = TRUE),
	             "'num_matches' input parameter must be numeric")
    })

test_that("rollmatch 'num_matches' is a whole number", {
	expect_error(rollmatch(formula, synthdata_clone, tm, "entry_short", id,
	                       num_matches = 2.2),
	             "'num_matches' input parameter must be a whole number")
	})

test_that("rollmatch 'num_matches' is a positive number", {
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       num_matches = 0), NA)
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       num_matches = -2),
  	             "'num_matches' input parameter must be a positive number")
    })

test_that("rollmatch 'replacement' is of type logical", {
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       replacement = TRUE), NA)
  	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
  	                       replacement = "fizzbuzz"),
  	             "'replacement' input parameter must be of type 'logical'")
	expect_error(rollmatch(formula, rem_synthdata_small, tm, entry, id,
	                       replacement = 12),
	             "'replacement' input parameter must be of type 'logical'")
    })
