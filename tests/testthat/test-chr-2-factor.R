# Create Data
df <- data.frame(x = 1:3, y = 3:1, z = c("a", "b", "c"),
                 stringsAsFactors = FALSE)
vars <- names(df)

# Run Tests
test_that("chr_2_factor 'df' only accepts a dataframe", {
  expect_error(chr_2_factor(vars, vars), "Input data must a dataframe")
  expect_error(chr_2_factor(df, vars), NA)
})

test_that("chr_2_factor returns a dataframe the same size as the input", {
  expect_equal(dim(chr_2_factor(df, vars)), dim(df))
})

test_that("chr_2_factor returns a dataframe without character vectors", {
  expect_equal("character" %in% sapply(chr_2_factor(df, vars), class), FALSE)
})

test_that("chr_2_factor 'vars' is a vector", {
  expect_error(chr_2_factor(df, df), "Input variable list must be a vector")
})
