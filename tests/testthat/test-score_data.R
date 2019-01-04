context("Testing score_data() functionality")

fm <- as.formula(treat ~ qtr_pmt + yr_pmt + age)
vars <- all.vars(fm)
treat <- "treat"
tm <- "quarter"
entry <- "entry_q"
id <- "indiv_id"
lookback <- 1

data(package = "rollmatch", "rem_synthdata_small")
reduced_data <- reduce_data(data = rem_synthdata_small, treat = treat,
                            tm = tm, entry = entry,
                            id = id, lookback = lookback)

# TEST THE INPUTS: full set of tests for most inputs: test-rollmatch.R script.

# Testing: formula
test_that("score_data 'formula' is of type language", {
  expect_error(score_data(model_type = model_type, match_on = match_on,
                          fm = "no formula", reduced_data = reduced_data,
                          tm = tm, entry = entry, id = id),
               "'formula' parameter must be of type 'language'")
  expect_error(score_data(model_type = model_type, match_on = match_on,
                          fm = "treat ~ qtr_pmt + yr_pmt + age",
                          reduced_data = reduced_data,
                          tm = tm, entry = entry, id = id),
               "'formula' parameter must be of type 'language'")
})

# Testing: treat
test_that("rollmatch 'treat' is a string and is in the dataset", {
  expect_error(score_data(model_type = model_type, match_on = match_on,
                          fm = fm, reduced_data = reduced_data, treat = 12,
                          tm = tm, entry = entry, id = id),
               "'treat' parameter must be a string")
})
# Testing: tm
test_that("reduce_data 'tm' is a string", {
  expect_error(score_data(model_type = model_type, match_on = match_on,
                          fm = fm, reduced_data = reduced_data, treat = treat,
                          tm = list(), entry = entry, id = id),
               "'tm' parameter must be a string")
})
# Testing: entry
test_that("reduce_data 'entry' is a string and is in the dataset", {
  expect_error(score_data(model_type = model_type, match_on = match_on,
                          fm = fm, reduced_data = reduced_data, treat = treat,
                          tm = tm, entry = 12, id = id),
               "'entry' parameter must be a string")
})
# Testing: id
test_that("rollmatch 'id' is a string and must be a column in the data.frame", {
  expect_error(score_data(model_type = model_type, match_on = match_on,
                          fm = fm, reduced_data = reduced_data, treat = treat,
                          tm = tm, entry = entry, id = 12),
               "'id' parameter must be a string")
})


# Test the output
scored_data <-
  score_data(model_type = "logistic", match_on = "logit", fm = fm,
             reduced_data = reduced_data, treat = "treat", tm = "quarter",
             entry = "entry_q", id = "indiv_id")

test_that("reduce_data produces desired results for practice dataset.", {
  expect_equal(dim(scored_data), c(4030, 21))
  expect_equal(round(scored_data$score[1], 2), round(-2.586431, 2))
})
