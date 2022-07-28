## read in manually added trial information from trial.json 
## only needed to get NCT ID in queryTrialJson(jsonfile)

parseManTrials <- function(jsonfile) {
  #jsonfile <- "data/trials/01.json"
  trial <- fromJSON(jsonfile)
  
  tr <- tibble(
    Protocol = trial$info$protocol_no,
    NCT = trial$info$NCT,
    JIT = trial$info$jit,
    Name = trial$info$trial_name
  )
  return(tr)

}


## extract NCT ID from manually added trial information and run the query against the clinicaltrials API
## and write the queried information to trial.auto.json

queryTrialJson <- function(jsonfile) {
  # jsonfile <- "data/trials/01.json"

  parsed_json <- parseManTrials(jsonfile)
  query <- queryNct(nct_id = parsed_json$NCT)
  
  t <- fromJSON(jsonfile)
  t$query <- query
  queried_json <- toJSON(t, pretty = T)
  # queried_json
  
  outjson <- gsub(".json", ".auto.json", jsonfile)
  writeLines(queried_json, outjson) # do not use write_json; adds string literal characters
}


## parse all trial.full.json and curate relevant info for a basic summary table

parseFullTrials <- function(autojsonfile) {
  # autojsonfile <- "data/trials/04.full.json"
  autojsonfile <- "data/trials/NCT04461600.full.ndjson"
  autotrial <- fromJSON(autojsonfile)
  
  # function to create (1 line of) biomarker per cohort
  processBiomarker <- function(x) {
    b <- arm_groups[x,]$biomarker[[1]] %>% 
      select(summary) %>% 
      unlist() %>% 
      glue_collapse(sep = " | ")
    return(b)
  }
  
  # pulling out trial arms
  arm_groups = tibble(cohortlabel = autotrial$query$arm[[1]]$cohortlabel,
                      drug = autotrial$query$arm[[1]]$drug,
                      arm_type = autotrial$query$arm[[1]]$arm_type,
                      biomarker = autotrial$query$arm[[1]]$biomarker)
  
  autotr <- tibble(
    
    # info
    Protocol = autotrial$info$protocol_no,
    NCT = autotrial$info$NCT,
    JIT = autotrial$info$jit,
    Name = autotrial$info$trial_name,

    # disease
    Disease = autotrial$disease$summary,

    
    # query - general
    Status = autotrial$query$current_status,
    StatusUpdate = autotrial$query$status_verif_date,
    Sponsor = autotrial$query$sponsor,
    Summary = autotrial$query$brief_summary,
    Conditions = autotrial$query$conditiions,
    Phase = autotrial$query$phase,
    StudyType = autotrial$query$type,
    InclExclCriteria = autotrial$query$criteria,
    MinAge = if(autotrial$query$min_age %>% is_empty()) {
      min_age = "Not Available"
      } else 
        {
          autotrial$query$min_age
          },
    Gender = autotrial$query$gender,
    Link = autotrial$query$link,
    LastUpdate = autotrial$query$last_update_date,
    
    # query - cohorts only for display table
    disp_cohorts = list(disp_cohorts = bind_cols(arm_groups %>% select(-biomarker), 
                                                 biomarker = lapply(1:nrow(arm_groups), function(x) processBiomarker(x)) %>% 
                                                   unlist())),
    
    # query - cohorts w/ drug and biomarker information
    arm_groups = list(arm_groups),
    
    # query - biomarkers only for display table
    disp_biomarkers = autotrial$query$arm[[1]]$biomarker %>% 
      bind_rows() %>% 
      select(summary) %>% 
      unlist() %>%
      unique() %>% 
      glue_collapse(sep = " | ")
    
  )
  return(autotr)
  
}


## get all trial.json files in the data/trials subfolder and generate a summary table and write updated trial.auto.json

writeTrialJson <- function(path) {
  
  # actions on trial.json
  files <- dir(path = path, pattern = "[0-9].json", full.names = T)
  #files <- dir(path = trialspath, pattern = "[0-9].test.json", full.names = T)
  files %>% walk(queryTrialJson)
}

## read in all trial.full.json files and create a result df for use in the browse section
aggregateTrials <- function(path) {
  
  # actions on trial.full.json
  fullfiles <- dir(path = trialspath, pattern = "[0-9].full.json", full.names = T)
  result <- fullfiles %>% map(parseFullTrials) %>% bind_rows
  return(result)
}

