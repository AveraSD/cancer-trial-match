library(mongolite)
library(jsonlite)

# murl = "mongodb://127.0.0.1:27017"
# murl = "mongodb://localhost"

#m <- mongo(collection = "trials", 
#                    db = "aci", 
#                    url = murl)


#m <- mongo("trials")

#m$import(file("data/trials/01.auto.ndjson"))
#m$insert(result[1:4,])

#m$aggregate()
# m$drop() # to remove the collection associated with m

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


###############
# read in trials from database to use for browse
# create the equivalent of the 'result' tibble
db_url = "mongodb://127.0.0.1:27017"
db <- mongo(collection = "ClinicalTrials", 
            db = "aci", 
            url = db_url)

# add in trials with trial.full.ndjson
# db$import(file(here("data/trials/01.full.ndjson")))
# db$import(file(here("data/trials/02.full.ndjson")))
# db$import(file(here("data/trials/03.full.ndjson")))
# db$import(file(here("data/trials/04.full.ndjson")))


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
  "LastUpdate" = last_update_date
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
                              select(Link, Name, Disease, disp_biomarkers) %>% 
                              rename("Trial" = Link,
                                     "Biomarker" = disp_biomarkers) , 
                            filterable = TRUE,
                            searchable = TRUE,
                            resizable = TRUE,
                            fullWidth = TRUE,
                            defaultColDef = colDef(align = "center"), 
                            striped = TRUE, 
                            showSortable = TRUE, 
                            style = list(minWidth = 800), 
                            columns = list(Trial = colDef(html = TRUE)),
                            details = function(index) {
                              
                              # create table for cohort level information - try to do this outside of reactable if possible
                              processBiomarker <- function(x) {
                                b <- arm_groups[x,]$biomarker[[1]] %>% 
                                  select(summary) %>% 
                                  unlist() %>% 
                                  glue_collapse(sep = " | ")
                                return(b)
                              }
                              
                              arm_groups <- db_tbl$arm[[index]] %>% 
                                select(cohortlabel, drug, arm_type, biomarker)
                              
                              disp_cohorts = bind_cols(arm_groups %>% select(-biomarker), 
                                                                                  biomarker = lapply(1:nrow(arm_groups), function(x) processBiomarker(x)) %>% 
                                                                                    unlist())
                              
                              coh <- disp_cohorts
                              coh$drug <- gsub(" \\| NA$", "", coh$drug)
                              
                              
                              # create tables to be displayed if nested rows are expanded
                              htmltools::div(
                                
                                # group1: general info
                                reactable(db_tbl[index, ] %>%
                                            select(JIT, Sponsor, Phase, StudyType, Status)),
                                
                                
                                # group2: cohort info
                                reactable(coh, 
                                          columns = list(cohortlabel = colDef(name = "Cohort Label"),
                                                         drug = colDef(name = "Drug(s)"),
                                                         arm_type = colDef(name = "Arm Type"),
                                                         biomarker = colDef(name = "Biomarker(s)"))),
                                
                                
                                # group 3: summary
                                reactable(db_tbl[index, ] %>% 
                                            select(Summary)),
                                
                                
                                # group 4: trial conditions
                                reactable(db_tbl[index, ] %>% 
                                            select(Conditions, Gender, MinAge), # add min age here
                                          defaultColDef = colDef(align = "center"),
                                          columns = list(MinAge = colDef(name = "Minimum Age"))),
                                
                                
                                # group 6: dates of last status update and any trial update
                                reactable(db_tbl[index, ] %>% 
                                            select(StatusUpdate, LastUpdate),
                                          columns = list(StatusUpdate = colDef(name = "Status Verification Date"),
                                                         LastUpdate = colDef(name = "Last Update")))
                              )
                            })
