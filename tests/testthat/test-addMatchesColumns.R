
# Load data
load("matches.rda")

# Run Function
newmatches <- addMatchesColumns(matches)

# Run Tests!
test_that("addMatchesColumns should run without error", {
  expect_equal(dim(newmatches)[1], 1697)
})
test_that("create_matches should add 5 new columns", {
  expect_equal(dim(matches)[2] + 5, dim(newmatches)[2])
})
test_that("Column row_weight should sum to the number of unique treat ids", {
  expect_equal(sum(newmatches$row_weight), length(unique(matches$treat_id)))
})
