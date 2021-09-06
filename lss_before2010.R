### LSS before 2010 ###
### Author: Fabian Pawelczyk ###
### Date: 02/09/2021 ###
### RePeC Latent Semantic Scaling ###

setwd("C:/Users/fpawe/OneDrive - Universität zu Köln/UzK/BA Thesis")
load(file = "df_repec_v1.4_author_clean.RData")
df_repec <- df_repec_v1.4_author_clean
rm(df_repec_v1.4_author_clean)

### load Packages
require(quanteda)
require(quanteda.corpora)
require(quanteda.textstats)
require(LSX)


start <- Sys.time()


# Create Corpus
corp_repec <- corpus(df_repec, text_field = "abstract")


## Edit docnames
docid <- paste(df_repec$year,  
               df_repec$title, 
               df_repec$author_clean,
               sep = " ")

docnames(corp_repec) <- docid
rm(docid)

corp_repec <- corpus_subset(corp_repec, year <= 2009) ### before 



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

multiword <- c("fiscal multiplier*", "fiscal multipliers", "government debt", 
               "government spending","new keynesian", "new keynes", "new keynesians",
               "spending multiplier", "spending multipliers", "keynesian multiplier", 
               "keynesian multipliers", "lagrange mutliplier", "lagrange multipliers",
               "state spending", "state spendings", "government spending", "government
               spendings", "public spending", "public spendings", "fiscal stimulus",
               "debt financed", "public debt", "government debt", "free market", 
               "free markets", "spending cut", "spending cuts", "fiscal deficit", 
               "fiscal deficits", "fiscal consolidation", "fiscal discipline", 
               "consumer spending", "budget deficit", "budget deficits", "fiscal policy",
               "deficit financed")

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

# identify context words 

eco <- char_context(toks_sent, pattern =c("fiscal multiplier*", "fiscal multipliers", "government debt", 
                                          "government spending","new keynesian", "new keynes", "new keynesians",
                                          "spending multiplier", "spending multipliers", "keynesian multiplier", 
                                          "keynesian multipliers",
                                          "state spending", "state spendings", "government spending",
                                          "government spendings", "public spending", "public spendings", "fiscal stimulus",
                                          "debt financed", "public debt", "government debt", 
                                          "spending cut", "fiscal deficit", 
                                          "fiscal deficits", "fiscal consolidation",
                                          "fiscal discipline", 
                                          "consumer spending", "budget deficit", "budget deficits",
                                          "keynes*", "countercyclical",
                                          "fiscal policy",
                                          "neo_keynes*",
                                          "fiscal deficits",
                                          "state-intervention", "interventionsism", 
                                          "debt-financed", "crowding-in", "friction", "frictions",
                                          "bailout", "bailouts", "regulation"), p = 0.05)

head(eco)
tail(eco)

# run LSS model

tmod_lss_before <- textmodel_lss(dfmat_sent, seeds = seed,
                          terms =c("fiscal_multiplier", "fiscal_multipliers", "government_debt", 
                                   "government_spending","new_keynesian", "new_keynes*", "new_keynesians",
                                   "spending_multiplier", "spending_multipliers", "keynesian_multiplier", 
                                   "keynesian_multipliers",
                                   "state_spending", "state_spendings", "government_spending",
                                   "government_spendings", "public_spending", "public_spendings", "fiscal_stimulus",
                                   "debt_financed", "public_debt", "government_debt", 
                                   "spending_cut", "fiscal_deficit", 
                                   "fiscal_deficits", "fiscal_consolidation",
                                   "fiscal_discipline", 
                                   "consumer_spending", "budget_deficit", "budget_deficits",
                                   "keynes*", "keynesian", "keynesians", "keynesianism", "countercyclical",
                                   "fiscal_policy",
                                   "neo_keynes*",
                                   "fiscal_deficits",
                                   "state-intervention", "interventionsism", 
                                   "debt-financed", "crowding-in", "friction", "frictions",
                                   "bailout", "bailouts", "regulation",
                                   "multiplier*", "deficit_financed", "monetarism", "inflation"
                                   ), k = 300, cache = TRUE)



tmod_lss_before <- textmodel_lss(dfmat_sent, seeds = seed,
                          terms = eco, k = 300, cache = TRUE)


save(tmod_lss_before, file ="tmod_lss_before.RData")


head(coef(tmod_lss_before), 50) # most positive words

tail(coef(tmod_lss_before), 20) # most negative words

### Typical negative words are visable. We see many negative words as negative
### but not few as positive as well

textplot_terms(tmod_lss_before, data_dictionary_LSD2015["negative"])



dfmat_doc <- dfm_group(dfmat_sent)
dat <- docvars(dfmat_doc)
dat$fit <- predict(tmod_lss_before, newdata = dfmat_doc)

dat$date <- NA
dat$date <- dat$year
dat$date <- as.Date(dat$date, origin = "1970-01-01") 
dat$date <- as.Date(as.character(dat$year), format = "%Y")


dat_smooth <- smooth_lss(dat, date_var = "date", engine = "locfit")

head(dat_smooth)

plot(dat$date, dat$fit, col = rgb(0, 0, 0, 0.05), pch = 16, ylim = c(-0.8, 0.8),
     xlab = "Time", ylab = "Economic sentiment")
lines(dat_smooth$date, dat_smooth$fit, type = "l")
lines(dat_smooth$date, dat_smooth$fit + dat_smooth$se.fit * 1.96, type = "l", lty = 3)
lines(dat_smooth$date, dat_smooth$fit - dat_smooth$se.fit * 1.96, type = "l", lty = 3)
abline(h = 0, lty = c(1, 2))


end <- Sys.time()
end-start
