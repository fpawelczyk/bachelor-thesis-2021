### Prepare basic/useful objects ###
### Author: Fabian Pawelczyk ###
### Date: 17/09/2021 ###


setwd("./replication_data_fp")
df_repec <- load(file = "df_repec_v1.4_author_clean.RData")
df_repec <- df_repec_v1.4_author_clean
rm(df_repec_v1.4_author_clean)

# Packages ####
require(quanteda)
library(stringr)

# Create Corpus ####
corp_repec <- corpus(df_repec, text_field = "abstract")


## Edit docnames
docid <- paste(df_repec$year,  
               df_repec$title, 
               df_repec$author_clean,
               sep = " ")

docnames(corp_repec) <- docid
rm(docid)

save(corp_repec, file = "corp_repec.RData")


# tokenize text corpus and remove various features ####
corp_sent <- corpus_reshape(corp_repec, to =  "sentences")
toks_sent <- corp_sent %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(stopwords("en", source = "marimo")) %>%
  tokens_remove(c("*-time", "*-timeUpdated", "GMT", "BST", "*.com")) 

### Select words with Capital Letters

toks_deb_cap <- tokens_select(toks_sent, 
                              pattern = "^[A-Z]",
                              valuetype = "regex",
                              case_insensitive = FALSE, 
                              padding = TRUE)


tstat_col_cap <- textstat_collocations(toks_deb_cap, min_count = 10, 
                                       tolower = FALSE, size = 2:3)

#head(tstat_col_cap, 200)
#tail(tstat_col_cap, 200)

toks_sent <- tokens_compound(toks_sent, pattern = tstat_col_cap[tstat_col_cap$z > 4], 
                             case_insensitive = FALSE)

### Create own multi word expressions

multiword <- c("budget deficit", "budget deficits","crowding in",
               "consumption multiplier","consumption multipliers",
               "fiscal deficit", "fiscal deficits", "fiscal multiplier",
               "fiscal multipliers", "fiscal stimulus", "fiscal stimuli",
               "government debt", "government spending", "government consumption",
               "government intervention", "government interventions",
               "public spending", "new keynesian", "neo-keynesian",
               "neo-keynesians", "spending multiplier", "spending multipliers", 
               "keynesian multiplier", "keynesian multipliers", "state spending",
               "state spendings", "state intervention", "state interventions",
               "public debt", "policy intervention", "policy interventions", 
               "deficit spending","regulatory policy", "regulatory policies", 
               "balanced budget", "crowding out", "debt brake","neo classical",
               "neo classic","free market", "free markets", "fiscal consolidation", 
               "fiscal consolidations", "fiscal discipline", "fiscal prudence", 
               "laissez faire", "spending cut", "spending cuts",
               "deficits reduction", "tax cuts", "tax cut", "free trade",
               "ricardian equivalence", "rational agent",
               "rational agents",
               "consumer spending", "fiscal policy", "fiscal policies")


toks_sent <- tokens_compound(toks_sent, pattern = phrase(multiword))

rm(toks_deb_cap)
rm(tstat_col_cap)





# create a document feature matrix from the tokens object

dfmat_sent <- toks_sent %>% 
  dfm() %>% 
  dfm_remove(pattern = "") %>% 
  dfm_trim(min_termfreq = 5) ### Here I lose some words

# Save dfmat and toks

save(dfmat_sent, file ="dfmat_repec.RData")
save(toks_sent, file ="toks_repec.RData")


# Construct a dfmat for STM ####

# Remove Stopwords etc

repec_dfm_forstm <- toks_sent %>% 
  tokens(remove_punct   = TRUE,
         remove_numbers = TRUE,
         remove_symbols = TRUE) %>% 
  dfm() %>% 
  dfm_remove(stopwords("english"))


save(repec_dfm_forstm, file = "repec_dfm_forstm.RData")

# Remove all objects

rm(list = ls())
