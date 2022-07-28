## server side
# display all available biomarker + disease based matches for patients with Tempus results

# read in patient summary results
pt_file <- here("data", "pt", "ptdata.tsv")
pt_data <- read.delim(pt_file, header = TRUE, sep = "\t", quote = "")

# format trial results tibble
result_ext <- result %>% unnest(disp_cohorts) 
  # unnest(arm_groups) %>% 
  # unnest(biomarker)

# Note: when using the column names function, it is absolutely necessary to quote function in backticks to ensure that dplyr::filter reads it as a variable name and not something in R itself

matchcols <- c("test_id", "patient_id", "matchVal", "matchKey", "Protocol", 'NCT', "JIT", "Name", "Disease", "Status", "StatusUpdate", "Sponsor", "Summary", "Phase", "StudyType", "MinAge", "Gender", "Link", "LastUpdate", "cohortlabel", "drug", "arm_type", "summary")

### 1: TMB - high

matchTmbHigh <- function(tr, pt){
  
  # A: trial side
  matched_trials <- tr %>% 
    filter(type == "TMB" & `function` == "high" & selection == "include")
  
  # B: patient side 
  # Tempus xT: TMB-high threshold as defined by Tempus in their paper 
  # (PMID: 31040929)
  matched_pts <- pt_data %>% 
    filter(matchType == "msi_tmb") %>% 
    filter(!is.na(tmb)) %>% 
    mutate(tmb = as.numeric(tmb)) %>% 
    filter(tmb > 9) %>%
    select(-msi) %>%
    mutate(matchKey = "TMB") %>%
    mutate(tmb = as.character(tmb)) %>%
    rename(matchVal = tmb)
  
  out <- bind_cols(matched_pts, matched_trials) %>% 
    select(all_of(matchcols))
  
  return(out)
} 


### 2: Gene Fusion

matchFusion <- function(tr, pt){
  
  # A: trial side
  matched_trials <- tr %>% 
    filter(type == "fusion" & selection == "include")  %>%
    mutate(gene1 = gene) %>% # remove after correcting JSON fields
    rename(gene2 = gene)
  
  # B: patient side
  matchOut <- function(gene1, gene2){
    a <- pt_data %>% 
      filter(matchType == "fusion") %>% 
      filter(gene5 %in% c(gene1, gene2) | gene3 %in% c(gene1, gene2))
    return(a)
  }
  
  out <- matched_trials %>% 
    rowwise() %>% 
    mutate(mat = list(matchOut(gene1, gene2))) %>% 
    unnest(mat) %>%
    mutate(matchKey = "fusion", matchVal = paste0(gene5, "_", gene3)) %>%
    select(all_of(matchcols))
  
  return(out)
}


### 3: Gene Mutation

matchMutation <- function(tr, pt){
  
  # A: trial side
  matched_trials <- tr %>% 
    filter(type == "mutation" & selection == "include")
  
  # B: patient side
  matchVariant <- function(gene, variant){
    if (variant == "") { # match on gene name only
      a <- pt_data %>% 
        filter(matchType == "mut") %>% 
        rename(pt_gene = gene) %>%
        mutate(mutationEffect = gsub("p.|c.", "", mutationEffect)) %>%
        filter(pt_gene == gene)
    } else { # match on gene + specific mutation
      a <- pt_data %>% 
        filter(matchType == "mut") %>% 
        rename(pt_gene = gene) %>%
        mutate(mutationEffect = gsub("p.|c.", "", mutationEffect)) %>%
        filter(pt_gene == gene & mutationEffect == variant)
    }
    
    return(a)
  }
  
  matchMultiple <- function(gene, variant, `function`){
    a <- matchVariant(gene, variant)
    if (`function` == "multiple"){
      b <- a %>%
        group_by(test_id, pt_gene) %>%
        summarise(Count = n()) %>%
        filter(Count > 1)
      
      a <- a %>%
        filter(test_id %in% b$test_id)
    }
    return(a)
  }
  
  out <- matched_trials %>% 
    rowwise() %>% 
    mutate(mat = list(matchMultiple(gene, variant, `function`))) %>% 
    unnest(mat) %>%
    mutate(matchKey = "mutation", matchVal = paste0(pt_gene, "_", mutationEffect)) %>%
    select(all_of(matchcols))
  
  return(out)
}

### 4: Gene Copy Number Alteration

matchCNA <- function(tr, pt){
  
  # A: trial side
  matched_trials <- tr %>% 
    filter((type == "amplification" | type == "deletion") & selection == "include")  
  
  # B: patient side
  matchOut <- function(gene, type){
    a <- pt_data %>% 
      filter(matchType == "cna") %>% 
      rename(pt_gene = gene) %>%
      filter(pt_gene == gene & variantType == type)
    return(a)
  }
  
  out <- matched_trials %>% 
    rowwise() %>% 
    mutate(mat = list(matchOut(gene, type))) %>% 
    unnest(mat) %>%
    mutate(matchKey = "CNA", matchVal = paste0(pt_gene, "_", variantType)) %>%
    select(all_of(matchcols))
  
  return(out)
}

##### evaluate all match criteria across all patients
matchdf <- function(tr, pt) {
  o <- matchTmbHigh(tr, pt) %>% 
    #bind_rows(matchFusion(tr, pt)) %>% 
    bind_rows(matchMutation(tr,pt)) %>%
    bind_rows(matchCNA(tr, pt))
  return(o)
}

match_table <- matchdf(tr = result_ext, pt = pt_data)

min_match <- match_table %>% 
  select(test_id, patient_id, matchVal, matchKey, Name, cohortlabel, drug, summary, Link) %>% 
  mutate(patient_id = replace(patient_id, patient_id == "" | is.na(patient_id), "Not Available"))


##### display table
display_match_gen <- reactable(min_match %>% 
                             select(patient_id, test_id, summary, Name, Link, cohortlabel, matchVal) %>%
                             rename("Test ID" = test_id,
                                    "Patient ID" = patient_id,
                                    "Trial" = Link,
                                    "Cohort" = cohortlabel,
                                    "Match criteria" = summary,
                                    "Match Value" = matchVal),
                           filterable = TRUE,
                           #searchable = TRUE,
                           resizable = TRUE,
                           fullWidth = TRUE,
                           defaultColDef = colDef(align = "center"),
                           defaultPageSize = 100,
                           showSortable = TRUE, 
                           showPageSizeOptions = TRUE,
                           style = list(minWidth = 800),
                           defaultSorted = c("Patient ID", "Test ID", "Match criteria"), 
                           defaultSortOrder = "desc",
                           columns = list(
                             Trial = colDef(html = TRUE),
                             `Patient ID` = colDef(
                               name = "PatientID",
                               style = JS("function(rowInfo, colInfo, state) {
        const firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by Patient ID
        if (!firstSorted || firstSorted.id === 'Patient ID') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.row['Patient ID'] === prevRow['Patient ID']) {
            return { visibility: 'hidden' }
          }
        }
      }")
      #style = group_merge_sort("Patient ID")
                             )), 
      rowStyle = JS("
    function(rowInfo, state) {
      // Ignore padding rows
      if (!rowInfo) return

      // Add horizontal separators between groups 
      const firstSorted = state.sorted[0]
      if (firstSorted && firstSorted.id === 'Patient ID') {
        const nextRow = state.pageRows[rowInfo.viewIndex + 1]
        if (nextRow && rowInfo.row['Patient ID'] !== nextRow['Patient ID']) {
          // Use box-shadow to add a Npx border without taking extra space
          return { boxShadow: 'inset 0 -0.2px 0 rgba(54, 69, 79, 0.5)' }
        }
      }
    }
  "))



