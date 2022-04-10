## server side
## display all trials

## read in trials from database to use for browse
# create the equivalent of the 'result' tibble

#db_access <- "host.docker.internal" # run mongod from a docker image
db_access <- "127.0.0.1" # local mongod process
db_url <- glue("mongodb://", db_access, ":27017")

#db_url <- "mongodb://0.0.0.0:27017,host.docker.internal:27017,127.0.0.1:27017" 

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

# browse table
display_browse_db <- reactable(db_tbl %>% 
                                 select(Link, Name, Disease, disp_biomarkers, Documentation) %>% 
                                 rename("Trial" = Link,
                                        "Biomarker" = disp_biomarkers) , 
                               filterable = TRUE,
                               #searchable = TRUE,
                               resizable = TRUE,
                               fullWidth = TRUE,
                               defaultColDef = colDef(align = "center"), 
                               striped = TRUE, 
                               showSortable = TRUE, 
                               style = list(minWidth = 800), 
                               columns = list(Trial = colDef(html = TRUE)),
                               details = function(index) {
                                 
                                 # create table for cohort level information
                                 processBiomarker <- function(x) {
                                   b <- arm_groups[x,]$biomarker[[1]] %>% 
                                     select(summary) %>% 
                                     unlist() %>% 
                                     glue_collapse(sep = " | ")
                                   return(b)
                                 }
                                 
                                 arm_groups <- db_tbl$arm[[index]] %>% 
                                   select(cohortlabel, drug, arm_type, line_of_therapy, arm_hold_status, biomarker)
                                 
                                 disp_cohorts = bind_cols(arm_groups %>% select(-biomarker), 
                                                          biomarker = lapply(1:nrow(arm_groups), function(x) processBiomarker(x)) %>% 
                                                            unlist())
                                 
                                 coh <- disp_cohorts
                                 coh$drug <- gsub(" \\| NA$", "", coh$drug)
                                 
                                 
                                 # create tables to be displayed if nested rows are expanded
                                 htmltools::div(
                                   
                                   # group1: general info
                                   reactable(db_tbl[index, ] %>%
                                               select(JIT, Sponsor, Phase, StudyType, Status, HoldStatus)),
                                   
                                   
                                   # group2: cohort info
                                   reactable(coh, 
                                             columns = list(cohortlabel = colDef(name = "Cohort Label"),
                                                            drug = colDef(name = "Drug(s)"),
                                                            arm_type = colDef(name = "Arm Type"),
                                                            biomarker = colDef(name = "Biomarker(s)"), 
                                                            line_of_therapy = colDef(name = "Line of Tx"), 
                                                            arm_hold_status = colDef(name = "Arm HoldStatus"))),
                                   
                                   
                                   # group 3: summary
                                   reactable(db_tbl[index, ] %>% 
                                               select(Summary)),
                                   
                                   
                                   # group 4: trial conditions
                                   reactable(db_tbl[index, ] %>% 
                                               select(Conditions, Gender, MinAge, StatusUpdate, LastUpdate), 
                                             defaultColDef = colDef(align = "center"),
                                             columns = list(MinAge = colDef(name = "Minimum Age"), 
                                                            StatusUpdate = colDef(name = "Status Verification Date"),
                                                            LastUpdate = colDef(name = "Last Update")))
                                 )
                               })
