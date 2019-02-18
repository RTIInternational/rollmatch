context("Testing Utility Functions")

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

# ----- Run reduce data and score data to create a dataframe to test
data(package = "rollmatch", "rem_synthdata_small")
reduced_data <- reduce_data(data = rem_synthdata_small, treat = treat,
                            tm = tm, entry = entry,
                            id = id, lookback = lookback)
scored_data <- score_data(model_type = model_type, match_on = match_on, fm = fm,
                          reduced_data = reduced_data, treat = treat,
                          tm = tm, entry = entry, id = id)


# Testing: create_comparison
comparison_pool <- compare_pool(scored_data = scored_data, treat = treat,
                                tm = tm, entry = entry, id = id)

test_that("create_comparison should create the correct sized dataframe", {
  expect_equal(dim(comparison_pool), c(15000, 8))
})
test_that("create_comparison calculated the correct difference value", {
  expect_equal(comparison_pool$difference[1], 7.308659976)
})



# Testing: trim_pool
trimmed_pool <- trim_pool(alpha = alpha, comparison_pool = comparison_pool,
                          scored_data = scored_data, treat = treat, tm = tm)

test_that("An error should print. trim_pool 'alpha' should be numeric", {
  expect_error(trim_pool(".2", comparison_pool, scored_data, treat, tm),
               "non-numeric argument to binary operator")
})
test_that("An error is printed: 'comparison_pool must be a dataframe", {
  expect_error(trim_pool(.2, "comparison_pool", scored_data, treat, tm),
               "argument is of length zero")
})
test_that("An error is printed: 'scored_data must be a dataframe", {
  expect_error(trim_pool(.2, comparison_pool, "scored_data", treat, tm),
               "argument is of length zero")
})
test_that("trim_pool should create the correct sized dataframe", {
  expect_equal(dim(trimmed_pool), c(414, 8))
})
test_that("trim_pool's first observation is correctly identified", {
  expect_equal(trimmed_pool$difference[1], 0.044658855)
})

# Testing: create_matches
matches_true <- create_matches(trimmed_pool = trimmed_pool, tm = tm,
                               num_matches = 3, replacement = TRUE)
test_that("Create matches algorithm works when replacment = TRUE", {
  expect_equal(dim(matches_true)[1], 79)
  expect_equal(matches_true$treat_id[1], 78756)
  expect_equal(matches_true$control_id[1], 64319)
})

matches_false <- create_matches(trimmed_pool = trimmed_pool, tm = tm,
                                num_matches = 3, replacement = FALSE)
test_that("Create matches algorithm works when replacment = TRUE", {
  expect_equal(dim(matches_false)[1], 75)
  expect_equal(matches_false$control_id[1], 26404)
})

# Testing: add_matches_columns
matches <- add_matches_columns(matches_true)
test_that("add_matches_columns added the right columns", {
  expect_equal(dim(matches)[2], 13)
  expect_true(all(c("match_rank", "total_matches", "treatment_weight",
                    "control_matches") %in% names(matches)))
})

# Testing: make_output --- TODO - Need to add checks here
combined_output <- make_output(scored_data = scored_data,
                               data = rem_synthdata_small,
                               matches = matches,
                               treat = "treat", tm = "quarter", id = "indiv_id",
                               entry = "entry_q", lookback = 1)

# Testing: add_balance_table --- TODO - Need to add checks here
output <- add_balance_table(scored_data = scored_data, vars = vars,
                            tm = "quarter", id = "indiv_id",
                            combined_output = combined_output,
                            treat = "treat", matches = matches)


# Testing: change_to_factor
# Create Data
df <- data.frame(x = 1:3, y = 3:1, z = c("a", "b", "c"),
                 stringsAsFactors = FALSE)
vars <- names(df)

# Run Tests
test_that("change_to_factor 'df' only accepts a dataframe", {
  expect_error(change_to_factor(vars, vars), "Input data must a dataframe")
  expect_error(change_to_factor(df, vars), NA)
})

test_that("change_to_factor returns a dataframe the same size as the input", {
  expect_equal(dim(change_to_factor(df, vars)), dim(df))
})

test_that("change_to_factor returns a dataframe without character vectors", {
  expect_equal("character" %in% sapply(change_to_factor(df, vars), class),
               FALSE)
})

test_that("change_to_factor 'vars' is a vector", {
  expect_error(change_to_factor(df, df), "Input variable list must be a vector")
})
