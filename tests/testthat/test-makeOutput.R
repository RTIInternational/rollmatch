# Set Variables 
orig.call <- "Ignore"
formula <- as.formula(treat ~ qtr_pmt + yr_pmt + age + is_male + is_white +
                   is_disabled + is_esrd + months_dual + chron_num + lq_ed +
                   yr_ed2 + lq_ip + yr_ip2)
tm <- "quarter"
entry <- "entry_q"
lookback <- 1

# Load data
load("model_output.rda")
pred_model <- model_output$pred_model
lr_result <- model_output$lr_result
load("out_list.rda")
data_full <- out_list$data_full
matches <- out_list$matches

# Run Function
out <- makeOutput(pred_model, lr_result, data_full, matches, orig.call,
                  formula, tm, entry, lookback)

# Run Tests!
test_that("makeOutput should run without error", {
  expect_equal(length(out), 8)
})
