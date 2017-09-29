rem_synthdata <- read.csv('data-raw/sythndata.csv', header = TRUE, sep = ',')

#rem_synthdata[is.na(rem_synthdata$yr_pmt),'yr_pmt'] <- 0

devtools::use_data(rem_synthdata,overwrite=TRUE)
