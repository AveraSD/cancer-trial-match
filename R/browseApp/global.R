
source(here("R", "curateApp", "queryNCT.R"))
source(here("R", "nct_to_json.R"))

## run all the things 
trialspath <- here("data", "trials")
resultfile <- fs::path(trialspath, "result.RData")
#writeTrialJson(trialspath) 

#result <- aggregateTrials(trialspath) 
#save(result, file = resultfile)  

load(resultfile)


source(here("R", "browseApp", "panel_browse.R"))
source(here("R", "browseApp", "panel_match_gen.R"))


