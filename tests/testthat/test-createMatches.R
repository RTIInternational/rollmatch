# Parameters
num_matches <- 3
replacement <- TRUE

# Load data
load("trimmed_pool.rda")

matches <- createMatches(trimmed_pool, num_matches, replacement)
# To save for the next test: save(matches, file = "matches.rda")


# Run Tests!
test_that("create_matches should run without error", {
  expect_equal(dim(matches)[1], 1697)
})
test_that("create_matches should not make more matches than num_matches", {
  expect_equal(max(table(matches$treat_id)) <= num_matches, TRUE)
})
test_that("create_matches should not make more matches than num_matches", {
  expect_equal(max(table(matches$treat_id)) <= num_matches, TRUE)
})
test_that("entry quarter and time should be exactly 'lookback' apart", {
  expect_equal(sum(matches$treat_entry_q - matches$time), dim(matches)[1])
})
