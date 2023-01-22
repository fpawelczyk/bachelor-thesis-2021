### Author: Fabian Pawelczyk ###
### Date: 10/08/2021 ###
### STM and Visualisation ###

#options(java.parameters = "-Xmx8g")

setwd("./replication_data_fp")
load("budget_data_clean.RData")



### Packages ####

require(quanteda)
#require(Rcpp)
require(stm)
require(quanteda.textstats)
library(xlsx)


### Remove Columns for saving storage

FINAL2$chamber <- NULL
FINAL2$country <- NULL
FINAL2$presidency <- NULL

### Create a corpus
corp_debates <- corpus(FINAL2, text_field = "content")

## Edit docnames
docid <- paste(FINAL2$party,  
               FINAL2$speaker, 
               FINAL2$year,
               FINAL2$sessionnumber,
               FINAL2$year_dummy, sep = " ")

docnames(corp_debates) <- docid
rm(docid)

#summary(corp_debates)

### create toks and remove punct and numbers (and maybe names?)

toks_debates <- tokens(corp_debates, remove_punct = T, remove_numbers = T, include_docvars = TRUE)

toks_debates <- tokens_remove(toks_debates, pattern = c(stopwords("de"),
                                                        pattern = c("peter", 
                                                                    "johannes", 
                                                                    "jürgen", 
                                                                    "fricke",
                                                                    "r","n", "s",
                                                                    "en", "kampeter",
                                                                    "kindler",
                                                                    "herr", "dm",
                                                                    "steffen", "thomas",
                                                                    "schmidt", "volker",
                                                                    "drucksache", "kahrs",
                                                                    "friedrich", "kauder", 
                                                                    "inzwischen", "de",
                                                                    "müller", "christian",
                                                                    "göring-eckardt",
                                                                    "ie", "nd", "wollten",
                                                                    "kelber", "e", "ch",
                                                                    "t", "as", "wolfgang",
                                                                    "michael", 
                                                                    "abgeordneten_eckert",
                                                                    "herren_beilfall",
                                                                    "afd_die", "afd_der",
                                                                    "wern", "menen",
                                                                    "frau_schavan", 
                                                                    "zurufe", "hören",
                                                                    "stimmen",
                                                                    "änderungsantrag",
                                                                    "abstimmung", 
                                                                    "angenommen", "abgelehnt",
                                                                    "dr", "uwe", "markus"
                                                        )))

### Select words with Capital Letters

toks_deb_cap <- tokens_select(toks_debates, 
                              pattern = "^[A-Z]",
                              valuetype = "regex",
                              case_insensitive = FALSE, 
                              padding = TRUE)



### Compound tokens again

tstat_col_cap <- textstat_collocations(toks_deb_cap, min_count = 10, tolower = FALSE, size = 2)
#head(tstat_col_cap, 100)


toks_debates <- tokens_compound(toks_debates, pattern = tstat_col_cap[tstat_col_cap$z > 4], 
                                case_insensitive = FALSE)
rm(toks_deb_cap)
rm(tstat_col_cap)

### Construct a document feature matrix

dfmat_debates <- dfm(toks_debates) %>%
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")

#topfeatures(dfmat_debates, 50) ### remove non important words later (see above)

### Delete empty rows in dfmat
dfmat_debates <- dfm_subset(dfmat_debates, ntoken(dfmat_debates) > 0)

#tail(dfmat_debates$content)
#dfm_wordstem(dfmat_debates, language = quanteda_options("de"))

### Check if that works
#ntoken(dfmat_debates)

### Change corp so it does fit to dfmat
corp_debates_vec <- as.character(corp_debates)[docnames(dfmat_debates)]
corp_debates <- corpus(corp_debates_vec)
rm(corp_debates_vec)
#save(corp_debates, file = "corp_debates.RData")

### Run STM and delete other Objects in Enviroment before
rm(FINAL2)
rm(toks_debates)






#NOT RUN!

stm_debates_40 <- stm(dfmat_debates, K = 40,
 data = docvars(corp_debates), seed=123)



#save(dfmat_debates, file = "dfmat_debates.RData")
#save(stm_debates_30, file ="stm_debates_30_leon.RData")
save(stm_debates_40, file ="stm_dbts_40.RData")
#write.csv(test1, "stm_debates_30_leon.csv")



#gc(reset = TRUE)


plot(stm_debates_40, xlim = c(0, .3), n=4)
labelTopics(stm_debates_40)

### Export plot manually here



# K = 40 ####

#load(file ="stm_dbts_40.RData")
test1 <- labelTopics(stm_debates_40)

#proportion_40 <- stm_debates_40$theta
freq_40 <- test1$prob
frex_40 <- test1$frex




#write.xlsx(proportion_40, file="debates_BA_40_topics.xlsx", sheetName="Topic Proportion", row.names=FALSE)
write.xlsx(freq_40, file="debates_BA_40_topics.xlsx", sheetName="FREQ", append=TRUE, row.names=FALSE)
write.xlsx(frex_40, file="debates_BA_40_topics.xlsx", sheetName="FREX", append=TRUE, row.names=FALSE)


# Create STM change plot ####
out <- NA
meta <- dfmat_debates@docvars
out$meta <- meta
rm(meta)
out$meta

prep <- estimateEffect(1:40 ~ year_dummy, stm_debates_40, meta=out$meta, 
                       uncertainty="Global")


plot(prep, covariate = "year_dummy", model = stm_debates_40, topics=c(8, 14, 15, 33),
     method = "difference", cov.value1="0", cov.value2="1", xlim=c(-.075,.075),
     xlab="Before 2012 ... After 2011", labeltype ="custom", 
     custom.labels=c('spending cuts', 'schwarze null','employment',
                     'macroeconomic Budgeting'
                    ))


# Whats the proportion of observations after 2010? ####

mean(as.numeric(levels(FINAL2$year_dummy))[FINAL2$year_dummy]) # -> 0.6292

rm(list=ls())
