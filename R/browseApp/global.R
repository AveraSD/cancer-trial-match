

## parse all trial.full.json and curate relevant info for a basic summary table

parseTrials <- function(jsonfile) {
  #jsonfile <- "data/trials/01.full.ndjson"
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
                      line_of_therapy = trial$query$arm[[1]]$line_of_therapy,
                      arm_hold_status = trial$query$arm[[1]]$arm_hold_status,
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
    #arm_groups = list(arm_groups = trial$query$arm[[1]] %>% unnest(biomarker)),
    #arm_groups = trial$query$arm[[1]] %>% unnest(biomarker),
    
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
      glue_collapse(sep = " | "),
    
    HoldStatus = trial$query$trial_hold_status,
    Documentation = trial$query$docs
    
  )
  return(parsedTrial)
  
}




###############################################

## read in trials from database to use for browse
# create the equivalent of the 'result' tibble


loadDbData <- function() {
 
  db <- mongo(collection = "ClinicalTrials", 
              db = "aci", 
              url = db_url)
  
  # add in trials with trial.full.ndjson
  # db$import(file(here("data/trials/01.full.ndjson")))
  # db$import(file(here("data/trials/02.full.ndjson")))
  # db$import(file(here("data/trials/03.full.ndjson")))
  # db$import(file(here("data/trials/04.full.ndjson")))
  # db$import(file(here("data/trials/05.full.ndjson")))
  # db$import(file(here("data/trials/06.full.ndjson")))
  
  
  # aggregate tibble
  db_tbl <- db$aggregate()[,2:4] %>% 
    unnest(cols = c(info, disease, query))
  
  
  db_tbl <- db_tbl %>% rename(
    # info
    "Protocol" = protocol_no,
    "JIT" = jit,
    "Name" = trial_name,
    
    # disease
    "Disease" = summary,
    
    # query - general
    "Status" = current_status,
    "StatusUpdate" = status_verif_date,
    "Sponsor" = sponsor,
    "Summary" = brief_summary,
    "Conditions" = conditiions,
    "Phase" = phase,
    "StudyType" = type,
    "InclExclCriteria" = criteria,
    "MinAge" = min_age,
    "Gender" = gender,
    "Link" = link,
    "LastUpdate" = last_update_date,
    "HoldStatus" = trial_hold_status,
    "Documentation" = docs
  )
  
  
  # create a 1 line summary for biomarkers for each trial
  biom <- function(tb) {
    tb %>% 
      unnest(biomarker) %>% 
      select(summary) %>% 
      unlist() %>%
      unique() %>% 
      glue_collapse(sep = " | ")
  }
  
  db_tbl$disp_biomarkers <- db_tbl$arm %>% map(biom)
  
  return(db_tbl)
  
}
