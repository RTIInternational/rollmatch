
# Read data
set.seed(2181)
load('data/datasets.rda')

head(rem_synthdata)
# Grab 5% of the treat ids
ids = unique(rem_synthdata$indiv_id[rem_synthdata$treat ==1])
ids_30 <- ids[sample(1:600,30)]
# Reduce Treatment Set
treat_set = rem_synthdata[rem_synthdata$indiv_id %in% ids_30, ]
# Each unique ID had one entry per quarter
table(treat_set$quarter)
# Go through each quarter and keep 5% of the data
control_set <- rem_synthdata[rem_synthdata$treat ==0, ]
control_small <- control_set[0, ]
for(i in 1:24){
  dataset <- control_set[control_set$quarter == i, ]
  dataset <- dataset[sample(1:10000,500),]
  control_small <- dplyr::bind_rows(control_small, dataset)
}
# Create and Save Final Dataset
rem_synthdata_small <- dplyr::bind_rows(treat_set, control_small)
save(rem_synthdata, rem_synthdata_small, file = "data/datasets.rda")

write.csv(rem_synthdata_small, file = 'data-raw/synth_dataset_small.csv')

# Does is create enough data? 
treat_test <- 
  rem_synthdata_small[rem_synthdata_small$treat == 1 & 
                        (rem_synthdata_small$quarter == 
                           rem_synthdata_small$entry_q - 1), ]
dim(treat_test)
comp_test <- rem_synthdata_small[rem_synthdata_small$treat == 0 & 
                                   (rem_synthdata_small$quarter %in% 
                                      unique(treat_test$quarter)), ]
dim(comp_test)



