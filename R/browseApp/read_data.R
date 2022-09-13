
### read in config parameters

## trials data location
t_d_d <- config::get("trial_data_dir")
trial_data_dir <- if_else(t_d_d %>% fs::is_absolute_path(), t_d_d, t_d_d %>% here())

## patient data location
p_d_f <- config::get("pt_data_file") 
pt_data_file <- if_else(p_d_f %>% fs::is_absolute_path(), p_d_f, p_d_f %>% here())

## trial data storage format
storage <- config::get("storage")

if (storage == "json") {
  # create a combined tibble
  trialsfiles <- dir(path = trial_data_dir, pattern = "*.full.ndjson", full.names = T)
  #trialsfiles = trialsfiles[7:10]
  #data <- trialsfiles %>% map_df(~fromJSON(file.path(trialsfiles, .), flatten = TRUE))
  result <- trialsfiles %>% map(parseTrials) %>% bind_rows()
  browse_tbl <<- result
}

if (storage == "db") {
  # look for active mongod process based on docker status
  docker <- config::get("docker")
  
  if (docker == "yes") {
    db_url <<- "host.docker.internal:27017,127.0.0.1:27017" 
  }
  
  if (docker == "no") {
    db_url <<- "mongodb://0.0.0.0:27017" 
  }
  
  browse_tbl <<- loadDbData()
}


