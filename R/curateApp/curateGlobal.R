
# get config params
trial_data_dir <- config::get("trial_data_dir")
pt_data_file <- config::get("pt_data_file") %>% here()


# get existing trial data
trialspath <- trial_data_dir %>% here()
resultfile <- fs::path(trialspath, "result.RData")
load(resultfile)


# get oncotree data
oncotree <- read.delim2(file = fs::path(here("data", "oncotree"), "oncotree.tsv"), 
                        header = TRUE, 
                        sep = "\t", 
                        quote = "", 
                        na.strings = 'NA')


# get gene and variant lists
readfileT = here("data", "metadata")
allgeneR = fs::path(readfileT, "allgenes.txt")
allvarR = fs::path(readfileT, "allvariants.txt")
allgenes = read.delim2(file = allgeneR, header = TRUE, sep="\t", quote = "", na.strings = 'NA')
allVar = read.delim2(file = allvarR, header = TRUE, sep="\t", quote = "", na.strings = 'NA')




