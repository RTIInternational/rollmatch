# Set Variables 
formula <- as.formula(treat ~ qtr_pmt + yr_pmt + age + is_male + is_white +
                   is_disabled + is_esrd + months_dual + chron_num + lq_ed +
                   yr_ed2 + lq_ip + yr_ip2)
vars <- all.vars(formula)
treat <- vars[1]
tm <- "quarter"
entry <- "entry_q"
id <- "indiv_id"
model_type <- "logistic"
match_on <- "logit"

# Load data
load("reduced_data.rda")

# Run Tests!
test_that("runModel should run without error", {
  expect_error(runModel(model_type, match_on, reduced_data, id, treat, entry,
                        tm, formula), NA)
})
test_that("runModel 'match_on' only accepts logit or pscore", {
  expect_error(runModel(model_type, "logicX", reduced_data, id, treat, entry,
                        tm, formula))
})
test_that("runModel 'model_type' only accepts logistic or probit", {
  expect_error(runModel("logisticX", match_on, reduced_data, id, treat, entry,
                        tm, formula))
})

test_that("runModel should return as many predictions as rows in the data", {
  expect_equal(dim(runModel(model_type, match_on, reduced_data, id, treat,
                            entry, tm, formula)$lr_result)[1],
               dim(reduced_data)[1])
})
