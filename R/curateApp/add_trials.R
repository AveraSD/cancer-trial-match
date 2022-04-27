
# function to write trial.ndjson after final Submit button is clicked

outSubmit <- function() {
  #tr2 <- isolate(disAd$resultsdf)
  # print(tr)
  tr2 <- isolate(disAd$rsdf)
  #print(tr2)
  
  outjson <- paste0(here(trial_data_dir), 
                    paste0(tr2 %>% unnest(c(info, disease, query)) %>% select(NCT) %>% as.character(), ".ndjson"))
  writeLines(tr2 %>% toJSON(pretty = T), outjson)
  message(paste0("Written to file: ", outjson))
}

