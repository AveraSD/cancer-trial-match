
outSubmit <- function() {
  #tr2 <- isolate(disAd$resultsdf)
  # print(tr)
  tr2 <- isolate(disAd$rsdf)
  #print(tr2)
  outjson <- here(trial_data_dir, 
                  paste0(tr2 %>% unnest(c(info, disease, query)) %>% select(NCT) %>% as.character(), ".ndjson"))
  # writeLines(tr %>% toJSON(pretty = F), outjson)
  # writeLines(tr %>% toJSON(pretty = T), here("data", "trials", paste0(tr$NCT, ".full.ndjson")))
  writeLines(tr2 %>% toJSON(pretty = T), outjson)
  message(paste0("Written to file: ", outjson))
}


# args <- commandArgs(TRUE)
# db_access <- args[1]
#db_access <- "host.docker.internal"
#db_access <- "127.0.0.1"
#db_url <- glue("mongodb://", db_access, ":27017")

#db_url <- "mongodb://host.docker.internal:27017,127.0.0.1:27018" # mogodb running from a docker image
# db <- mongo(collection = "ClinicalTrials", 
#             db = "aci", 
#             url = db_url)


# db$insert(tr)

#curate_data <- result[5:6,]
#m$insert(curate_data)

#curate_data[1,] %>% toJSON(pretty = F) %>% json_validate(schema, verbose = T)

##########
# write trial.full.ndjson to use as tte final output from curate
# fullfiles <- dir(path = trialspath, pattern = "[0-9].full.json", full.names = T)
# writeLines(fullfiles[4] %>%
#              fromJSON() %>%
#              toJSON(pretty = F),
#            "/Users/aanu/Documents/repos/trial-match/data/trials/04.full.ndjson")

# add in trials with trial.full.ndjson
# db$import(file(here("data/trials/01.full.ndjson")))
# db$import(file(here("data/trials/02.full.ndjson")))
# db$import(file(here("data/trials/03.full.ndjson")))
# db$import(file(here("data/trials/04.full.ndjson")))