
# get path to trials directory
t_d_d <- config::get("trial_data_dir")
trial_data_dir <- if_else(t_d_d %>% fs::is_absolute_path(), t_d_d, t_d_d %>% here())
trialspath <- trial_data_dir

# get oncotree data
oncotree <- read.delim2(file = here("data", "oncotree", "oncotree.tsv"), 
                        header = TRUE, 
                        sep = "\t", 
                        quote = "", 
                        na.strings = 'NA')

###add All Cancers and Solid Tumor entries

oncotree_dup<-oncotree
#oncotree_addrows<- oncotree_dup %>% add_row(level_1  = "All Cancers",level_2 = "NA",level_3 = "NA", level_4 = "NA", level_5 = "NA", level_6 = "NA", level_7 = "NA")
oncotree_addrows<- oncotree_dup %>% add_row(level_1  = "All Cancers")
#oncotree_addrows1<- oncotree_addrows %>% add_row(level_1  = "Solid Tumors",level_2 = "NA",level_3 = "NA", level_4 = "NA", level_5 = "NA", level_6 = "NA", level_7 = "NA")
oncotree_addrows1<- oncotree_addrows %>% add_row(level_1  = "Solid Tumors")
### ends

# get gene and variant lists
allgeneR <- here("data", "metadata", "allgenes.txt")
allvarR <- here("data", "metadata", "allvariants.txt")

allgenes <- read.delim2(file = allgeneR, 
                        header = TRUE, 
                        sep="\t", 
                        quote = "", 
                        na.strings = 'NA') %>% arrange(x) %>% distinct()
allgenes <- bind_rows(tibble(x = "Not available"), allgenes)

allVar <- read.delim2(file = allvarR, 
                      header = TRUE, 
                      sep="\t", 
                      quote = "", 
                      na.strings = 'NA') %>% arrange(x) %>% distinct() 
allVar <- bind_rows(tibble(x = "Not available"), allVar)


