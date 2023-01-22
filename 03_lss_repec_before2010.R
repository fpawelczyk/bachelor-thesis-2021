### LSS before 2010 ###
### Author: Fabian Pawelczyk ###
### Date: 02/09/2021 ###
### RePeC Latent Semantic Scaling ###

setwd("./replication_data_fp")
load(file = "df_repec_v1.4_author_clean.RData")
df_repec <- df_repec_v1.4_author_clean
rm(df_repec_v1.4_author_clean)

### load Packages
require(quanteda)
require(quanteda.corpora)
require(quanteda.textstats)
require(LSX)


# Create Corpus
corp_repec <- corpus(df_repec, text_field = "abstract")


## Edit docnames
docid <- paste(df_repec$year,  
               df_repec$title, 
               df_repec$author_clean,
               sep = " ")

docnames(corp_repec) <- docid
rm(docid)


corp_repec <- corpus_subset(corp_repec, year <= 2009 & year >= 1999) ### before 
sort(corp_repec$year, decreasing = T)


# tokenize text corpus and remove various features
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


tstat_col_cap <- textstat_collocations(toks_deb_cap, min_count = 10, tolower = FALSE, size = 2:3)

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
               "public debt", "public spending", "new keynesian", "neo-keynesian",
               "neo-keynesians", "spending multiplier", "spending multipliers", 
               "keynesian multiplier", "keynesian multipliers", "state spending",
               "state spendings", "state intervention", "state interventions",
               "policy intervention", "policy interventions", 
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
  dfm_trim(min_termfreq = 5)

#topfeatures(dfmat_sent, 20)


seed <- as.seedwords(data_dictionary_sentiment)



# run before LSS model for Keynesian Dict ####

tmod_lss_before <- textmodel_lss(dfmat_sent, seeds = seed,
                          terms =c("budget_deficit", "budget_deficits","bailout",
                                   "bailouts", "crowding_in",
                                   "consumption_multiplier",
                                   "consumption_multipliers", "countercyclical",
                                   "fiscal_deficit", "fiscal_deficits", 
                                   "fiscal_multiplier",
                                   "fiscal_multipliers", "fiscal_stimulus", 
                                   "fiscal_stimuli",
                                   "government_debt", "government_spending", 
                                   "government_consumption",
                                   "government_intervention", 
                                   "government_interventions",
                                   "public_spending", 
                                   "new_keynesian", "neo-keynesian",
                                   "neo-keynesians", "keynes", "keynesian", 
                                   "keynesians", "keynesianism",
                                   "spending_multiplier", "spending_multipliers",
                                   "keynesian_multiplier",
                                   "keynesian_multipliers", "state_spending",
                                   "state_spendings", "stimulus",
                                   "stimuli", "state_intervention",
                                   "state_interventions", "public_debt", 
                                   "policy_intervention", "policy_interventions",
                                   "deficit_spending",
                                   "regulation",
                                   "regulatory_policy", "regulatory_policies",
                                   "nationalization",
                                   "nationalizations", "nationalisation", "HANK",
                                   "fiscal policy",
                                   "fiscal policies", "inflation"
                                   ), k = 300, cache = TRUE)





save(tmod_lss_before, file ="tmod_lss_before.RData")

### Remove everything but one
rm(list=ls()[! ls() %in% c("dfmat_sent","seed")])


# Run 'Before-LSS for Neo-Classical Dict' ####

tmod_lss_before_nclassic <- textmodel_lss(dfmat_sent, seeds = seed,
                                 terms =c("austere", "austerity", 
                                          "balanced budget", "crowding-out", 
                                          "crowding_out", "debt_brake",
                                          "deregulation", "neo-classical", 
                                          "neo_classical", "neo-classic",
                                          "neo_classic", "neoclassical", 
                                          "neoclassic", "monetarist", 
                                          "monetarists",
                                          "monetarism", "monetarists", 
                                          "monetarism", "monetarisms", 
                                          "monetaristic",
                                          "free_market", "free_markets", 
                                          "fiscal_consolidation", 
                                          "fiscal_consolidations", 
                                          "fiscal_discipline", 
                                          "fiscal_prudence", 
                                          "laissez-faire", "laissez_faire", 
                                          "spending_cut", "spending_cuts",
                                          "deficits_reduction", "tax_cuts", 
                                          "tax_cut", "free_trade", "privatization", 
                                          "privatisation", 
                                          "ricardian_equivalence"), k = 300, cache = TRUE)

save(tmod_lss_before_nclassic, file ="tmod_lss_before_nclassic.RData")

#rm(list=ls()[! ls() %in% c("tmod_lss_before","tmod_lss_before_nclassic")])
rm(list=ls())


