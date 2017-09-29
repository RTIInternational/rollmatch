# Set Variables 
id <- "indiv_id"

# Load data
load("rem_synthdata.RData")
load("matches.rda")
data <- rem_synthdata

# Update Matches
matches <- addMatchesColumns(matches)
# Run function
out_list <- createWeights(matches, data, id)


# Run Tests!
test_that("createWeights should run without error", {
  expect_equal(length(out_list), 2)
})
test_that("createWeights should add 1 new column to matches", {
  expect_equal(dim(matches)[2] + 1, dim(out_list$matches)[2])
})
test_that("createWeights should add 4 columns to data", {
  expect_equal(dim(data)[2] + 4, dim(out_list$data_full)[2])
})
