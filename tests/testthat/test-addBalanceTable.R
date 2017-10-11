# Set Variables 
formula <- as.formula(treat ~ qtr_pmt + yr_pmt + age + is_male + is_white +
                   is_disabled + is_esrd + months_dual + chron_num + lq_ed +
                   yr_ed2 + lq_ip + yr_ip2)
vars <- all.vars(formula)
tm <- "quarter"
id <- "indiv_id"
treat <- vars[1]

# Load Data
load("reduced_data.rda")
load("matches.rda")
load("combined_output.rda")

# Add balance table to the output
finalout <- addBalanceTable(reduced_data, vars, tm, id, combined_output,
                            treat, matches)

# Run Tests!
test_that("addBalanceTable should run without error. There should be 9 items", {
  expect_equal(length(finalout), 9)
})
test_that("addBalanceTable should run without error. There should be 9 items", {
  expect_equal(class(finalout$balance), "data.frame")
})
test_that("addBalanceTable should have 13 rows - one for each item in fm", {
  expect_equal(dim(finalout$balance)[1], 13)
})
test_that("addBalanceTable should have 8 columns", {
  expect_equal(dim(finalout$balance)[2], 8)
})
