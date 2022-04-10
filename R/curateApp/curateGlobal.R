



trialspath <- here("data", "trials")
resultfile <- fs::path(trialspath, "result.RData")
load(resultfile)
#writeTrialJson(trialspath) # toggling on and off for dev 
#result <- aggregateTrials(trialspath) 
#save(result, file = resultfile) # toggling on and off for dev 

# oncoT = here("data", "oncotree")
# oncotree = fs::path(oncoT, "oncotree.tsv") 
oncotree <- read.delim2(file = fs::path(here("data", "oncotree"), "oncotree.tsv"), 
                        header = TRUE, 
                        sep = "\t", 
                        quote = "", 
                        na.strings = 'NA')
#oncotree= oncotree %>% mutate(across(where(~ anyNA(.) & is.character(.)), ~ replace_na(., "Nothing"))) 


readfileT = here("data", "metadata")
allgeneR = fs::path(readfileT, "allgenes.txt")
allvarR = fs::path(readfileT, "allvariants.txt")
allgenes = read.delim2(file = allgeneR, header = TRUE, sep="\t", quote = "", na.strings = 'NA')
allVar = read.delim2(file = allvarR, header = TRUE, sep="\t", quote = "", na.strings = 'NA')




