
## run all the things 
trial_data_dir <- config::get("trial_data_dir") %>% here()
pt_data_file <- config::get("pt_data_file") %>% here()

trialspath <- trial_data_dir
resultfile <- fs::path(trialspath, "result.RData")
#writeTrialJson(trialspath) 

#result <- aggregateTrials(trialspath) 
#save(result, file = resultfile)  

load(resultfile)





