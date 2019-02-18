context("Testing rollmatch()")

fm <- as.formula(treat ~ qtr_pmt + yr_pmt + age)
vars <- all.vars(fm)
treat <- "treat"
tm <- "quarter"
entry <- "entry_q"
id <- "indiv_id"
lookback <- 1
alpha <- .2
standard_deviation <- "average"
num_matches <- 3
match_on <- "logit"
model_type <- "logistic"
replacement <- TRUE

data(package = "rollmatch", "rem_synthdata_small")
synthdata_clone <- rem_synthdata_small
synthdata_clone["tm_neg"] <- -synthdata_clone[tm]
synthdata_clone["entry_neg"] <- -synthdata_clone[entry]
synthdata_clone["id_neg"] <- -synthdata_clone[id]
synthdata_clone["entry_short"] <- 1

reduced_data <- reduce_data(data = rem_synthdata_small, treat = treat,
                            tm = tm, entry = entry,
                            id = id, lookback = lookback)
scored_data <- score_data(model_type = model_type, match_on = match_on, fm = fm,
                          reduced_data = reduced_data, treat = treat,
                          tm = tm, entry = entry, id = id)


# Testing: tm
test_that("rollmatch 'tm' is a string", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = 12, entry = entry, id = id, vars = vars,
                         lookback = lookback),
               "'tm' parameter must be a string")
})
test_that("rollmatch 'tm' is in the data frame", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = "buzz", entry = entry, id = id, vars = vars,
                         lookback = lookback),
               "'tm' parameter must be a column in the data.frame")
})
test_that("rollmatch 'tm' must resolve to a vector of whole numbers", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = "qtr_pmt", entry = entry, id = id, vars = vars,
                         lookback = lookback),
               "'tm' variable must resolve to vector of whole numbers")
})
test_that("rollmatch 'tm' must resolve to a vector of positive numbers", {
  expect_error(rollmatch(synthdata_clone, synthdata_clone, treat = treat,
                         tm = "tm_neg", entry = entry, id = id, vars = vars,
                         lookback = lookback),
               "'tm' variable must resolve to vector of positive numbers")
})

# Testing: treat
test_that("rollmatch 'treat' is a string and is in the dataset", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = 12,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback),
               "'treat' parameter must be a string")
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = "treat2",
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback),
               "'treat' parameter must be a column in the data.frame")
})

# Testing: entry
test_that("rollmatch 'entry' is a string and is in the dataset", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = 12, id = id, vars = vars,
                         lookback = lookback),
               "'entry' parameter must be a string")
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = "fizz", id = id, vars = vars,
                         lookback = lookback),
               "'entry' parameter must be a column in the data.frame")
})
test_that("rollmatch 'entry' must resolve to a vector of whole numbers", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = "qtr_pmt", id = id, vars = vars,
                         lookback = lookback),
               "'entry' variable must resolve to vector of whole numbers")
})
test_that("rollmatch 'entry' must resolve to a vector of positive numbers", {
  expect_error(rollmatch(synthdata_clone, synthdata_clone, treat = treat,
                         tm = tm, entry = "tm_neg", id = id, vars = vars,
                         lookback = lookback),
               "'entry' variable must resolve to vector of positive numbers")
})

# Testing: id
test_that("rollmatch 'id' is a string and must be a column in the data.frame", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = 12, vars = vars,
                         lookback = lookback),
               "'id' parameter must be a string")
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = "fizz", vars = vars,
                         lookback = lookback),
               "'id' parameter must be a column in the data.frame")
})

# Testing: lookback
test_that("rollmatch 'lookback' is of type numeric", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = 2), NA)
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = "fizz"),
               "'lookback' parameter must be of type numeric")
})
test_that("rollmatch 'lookback' is a whole number", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = 8.3),
               "'lookback' parameter must be a whole number")
})
test_that("rollmatch 'lookback' must be between 1 and 10", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = 1), NA)
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = 4), NA)
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = -10),
               "'lookback' parameter must be between 1 and 10")
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                          tm = tm, entry = entry, id = id, vars = vars,
                          lookback = 15),
               "'lookback' parameter must be between 1 and 10")
})
test_that("rollmatch 'lookback' is less than number of time periods in data", {
  expect_error(rollmatch(synthdata_clone, synthdata_clone, treat = treat,
                         tm = tm, entry = "entry_short", id = id, vars = vars,
                         lookback = 2),
               "'lookback' is greater than number of time periods in data set")
})

# Testing: alpha
test_that("rollmatch 'alpha' is of type numeric", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback, alpha = 2), NA)
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback, alpha = "fizz"),
               "'alpha' parameter must be numeric")
})
test_that("rollmatch 'alpha' is a positive number", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback, alpha = 0), NA)
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback, alpha = -2),
               "'alpha' parameter must be a positive number")
})

# Testing: standard_deviation
test_that("rollmatch 'standard_deviation' is of type logical", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback, standard_deviation = "average"),
               NA)
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback, standard_deviation = "averag"),
               "'standard_deviation' must be 'average', 'weighted', or 'none'")
})

# Testing: num_matches
test_that("rollmatch 'num_matches' is of type numeric", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback, num_matches = 2), NA)
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback, num_matches = "fizz"),
               "'num_matches' parameter must be numeric")
})
test_that("rollmatch 'num_matches' is a whole number", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback, num_matches = 2.2),
               "'num_matches' parameter must be a whole number")
})
test_that("rollmatch 'num_matches' is a positive number", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback, num_matches = -2),
               "'num_matches' parameter must be a positive number")
})

# Testing: replacement
test_that("rollmatch 'replacement' is of type logical", {
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback, replacement = TRUE), NA)
  expect_error(rollmatch(scored_data, rem_synthdata_small, treat = treat,
                         tm = tm, entry = entry, id = id, vars = vars,
                         lookback = lookback, replacement = "TRUE"),
               "'replacement' parameter must be of type 'logical'")
})
