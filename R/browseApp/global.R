
## run all the things 
trial_data_dir <- config::get("trial_data_dir") %>% here()
pt_data_file <- config::get("pt_data_file") %>% here()

trialspath <- trial_data_dir
resultfile <- fs::path(trialspath, "result.RData")
#writeTrialJson(trialspath) 

#result <- aggregateTrials(trialspath) 
#save(result, file = resultfile)  

load(resultfile)





## parse all trial.full.json and curate relevant info for a basic summary table

parseTrials <- function(jsonfile) {
  jsonfile <- "data/trials/NCT04461600.full.ndjson"
  trial <- fromJSON(jsonfile)
  
  # function to create (1 line of) biomarker per cohort
  processBiomarker <- function(x) {
    b <- arm_groups[x,]$biomarker[[1]] %>% 
      select(summary) %>% 
      unlist() %>% 
      glue_collapse(sep = " | ")
    return(b)
  }
  
  # pulling out trial arms
  arm_groups = tibble(cohortlabel = trial$query$arm[[1]]$cohortlabel,
                      drug = trial$query$arm[[1]]$drug,
                      arm_type = trial$query$arm[[1]]$arm_type,
                      biomarker = trial$query$arm[[1]]$biomarker)
  
  parsedTrial <- tibble(
    
    # info
    #Protocol = trial$info$protocol_no,
    NCT = trial$info$NCT,
    JIT = trial$info$jit,
    Name = trial$info$trial_name,
    
    # disease
    Disease = trial$disease$summary,
    disp_disease = list(disp_disease = trial$disease %>% unnest(details)), 
    
    # query - general
    Status = trial$query$current_status,
    StatusUpdate = trial$query$status_verif_date,
    Sponsor = trial$query$sponsor,
    Summary = trial$query$brief_summary,
    Conditions = trial$query$conditions,
    Phase = trial$query$phase,
    StudyType = trial$query$type,
    MinAge = if(trial$query$min_age %>% is_empty()) {
      min_age = "Not Available"
    } else 
    {
      trial$query$min_age
    },
    Gender = trial$query$gender,
    Link = trial$query$link,
    LastUpdate = trial$query$last_update_date,
    
    # query - cohorts w/ drug and biomarker information
    arm_groups = list(arm_groups = trial$query$arm[[1]] %>% unnest(biomarker)),
    
    # query - cohorts only for display table
    disp_cohorts = list(disp_cohorts = bind_cols(arm_groups %>% select(-biomarker), 
                                                 biomarker = lapply(1:nrow(arm_groups), function(x) processBiomarker(x)) %>% 
                                                   unlist())),
    
    # query - biomarkers only for display table
    disp_biomarkers = trial$query$arm[[1]]$biomarker %>% 
      bind_rows() %>% 
      select(summary) %>% 
      unlist() %>%
      unique() %>% 
      glue_collapse(sep = " | ")
    
  )
  return(parsedTrial)
  
}