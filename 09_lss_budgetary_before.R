### LSS Budgetary Debate BEFORE 2010 ###
### Author: Fabian Pawelczyk ###
### Date: 21/10/2021 ###

setwd("./replication_data_fp")

load("budget_data_clean.RData")


### load Packages

require(quanteda)
require(quanteda.corpora)
#require(quanteda.textstats)
require(LSX)



# Create Corpus, Toks, Dfmat for LSS ####

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

# Create Corpus before 2011
corp_debates <- corpus_subset(corp_debates, year <= 2011 & year >= 1999) ### before

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



# Create important multiword expressions and add them


multiword <- c("schwarze null", "ausgeglichene haushalte","ausgeglichenen haushalt",
               "ausgeglichener haushalt", "neue Schulden", "ausgeglichene budgets", 
               "ausgeglichene budget", 
               "ausgeglichenes budget")

toks_debates <- tokens_compound(toks_debates, pattern = phrase(multiword))


### Construct a document feature matrix

dfmat_debates <- dfm(toks_debates) %>%
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")





# Create own dictionary ####

dict_test <- c("gut", "perfekt", "ausgezeichnet", "positiv", "erfreulich",
               "erfreulicherweise", "glücklich", "besser", "effektiv",
               "schlecht", "übel", "negativ", "falsch", "schlechter", 
               "fehlerhaft", "schwach", "schädlich", "schlimm")

dict_numb <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
dict_list <- list(dict_test, dict_numb)
seed <- as.seedwords(dict_list)

rm(dict_numb)
rm(dict_list)

# Run LSS for debates BEFORE crisis ####

tmod_lss_before_dbts <- textmodel_lss(dfmat_debates, seeds = seed,
                                 terms =c("multiplier", "schuldenfinanziertes",  
                                      "schuldenfinanzierte", "schuldenfinanziert",
         "keynes", "keynsianer", "keynesianismus", "keynesianer", "keynesianisch",
         "keynesianische","austeritätsprogramme", "austerität", "austeritätspolitik", 
         "reguliert",
         "regulierung", "regulierungen", "regulierten", "reguliertes", "subventionen",
         "subventioniert", "subvention", "subventionierung", "subventionsabbau",
         "subventionskürzungen", "schulden", "neuverschuldung", "nettoneuverschuldung",
         "kreditaufnahme", "nettokreditaufnahme", "defizite", "defizit", "expansiv",
         "expansive", "expansiven", "expansiver", "nettokreditaufnahme", "kreditfinanziert",
         "kreditfinanzierte", "kreditfinanzierung", "staatsverschuldung", "verschuldung", 
         "kredite", "ausgeglichen_haushalt", "ausgeglichene_haushalt","ausgeglichenen_haushalt",
         "ausgeglichener_haushalt", "kürzen", "gekürzt", "schuldenstand", "sparen",
         "schwarze_null", "konsolidieren", "konsoldierung", "Schuldenbremse"), k = 300, cache = TRUE)


save(tmod_lss_before_dbts, file ="tmod_lss_before_dbts.RData")

### Apply Latent Semantic Scaling ####

#tmod_lss <- textmodel_lss(dfmat_debates, seeds = seed,
                          #terms =c("multiplier", "schuldenfinanziertes",  
                                  # "schuldenfinanzierte", "schuldenfinanziert",
                                   #"keynes", "keynsianer", "keynesianismus", 
                                   #"keynesianer", "keynesianisch", "keynesianische",
                                   #"austeritätsprogramme", "austerität", 
                                   #"austeritätspolitik", "reguliert",
                                   #"regulierung", "regulierungen", "regulierten",
                                   #"reguliertes", "subventionen",
                                   #"subventioniert", "subvention", 
                                   #"subventionierung", "subventionsabbau",
                                   #"subventionskürzungen", "schulden", 
                                   #"neuverschuldung", "nettoneuverschuldung",
                                   #"kreditaufnahme", "nettokreditaufnahme", 
                                   #"defizite", "defizit", "expansiv",
                                   #"expansive", "expansiven", "expansiver", 
                                   #"nettokreditaufnahme", "kreditfinanziert",
                                   #"kreditfinanzierte", "kreditfinanzierung",
                                   #"staatsverschuldung", "verschuldung", 
                                   #"kredite", "ausgeglichen_haushalt",
                                   #"ausgeglichene_haushalt","ausgeglichenen_haushalt",
                                  # "ausgeglichener_haushalt", "kürzen", "gekürzt",
                                  # "schuldenstand", "sparen",
                                  # "schwarze_null", "konsolidieren", 
                                  # "konsoldierung", "schuldenbremse"
                         # ), k = 300, cache = TRUE)
rm(list=ls())




