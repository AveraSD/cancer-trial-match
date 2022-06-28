
# get path to trials directory
trial_data_dir <- config::get("trial_data_dir")
trialspath <- trial_data_dir %>% here()


# get oncotree data
oncotree <- read.delim2(file = here("data", "oncotree", "oncotree.tsv"), 
                        header = TRUE, 
                        sep = "\t", 
                        quote = "", 
                        na.strings = 'NA')


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


