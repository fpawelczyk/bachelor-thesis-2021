### Author: Fabian Pawelczyk ###
### Date 09/08/2021 ###
### Add Dummy Years to Datasets ###



setwd("./replication_data_fp")
load("budget_data_clean.RData")

# Add year-dummy to Plenary Text Data ####

FINAL2$year <- NA
FINAL2$year <- as.numeric(FINAL2$year)
FINAL2$sessionnumber <- as.numeric(FINAL2$sessionnumber)

FINAL2$year[FINAL2$sessionnumber < 8] <- 2000
FINAL2$year[FINAL2$sessionnumber >= 8] <- 2001
FINAL2$year[FINAL2$sessionnumber >= 16] <- 2002
FINAL2$year[FINAL2$sessionnumber >= 18] <- 2003
FINAL2$year[FINAL2$sessionnumber >= 29] <- 2004
FINAL2$year[FINAL2$sessionnumber >= 37] <- 2006
FINAL2$year[FINAL2$sessionnumber >= 57] <- 2007
FINAL2$year[FINAL2$sessionnumber >= 69] <- 2009
FINAL2$year[FINAL2$sessionnumber >= 81] <- 2010
FINAL2$year[FINAL2$sessionnumber >= 105] <- 2011
FINAL2$year[FINAL2$sessionnumber >= 117] <- 2012
FINAL2$year[FINAL2$sessionnumber >= 129] <- 2014
FINAL2$year[FINAL2$sessionnumber >= 153] <- 2015
FINAL2$year[FINAL2$sessionnumber >= 157] <- 2016
FINAL2$year[FINAL2$sessionnumber >= 169] <- 2018
FINAL2$year[FINAL2$sessionnumber >= 192] <- 2019
FINAL2$year[FINAL2$sessionnumber >= 203] <- 2020

### Add Dummy before and after 2010

FINAL2$dummy_year <- NA
FINAL2$dummy_year <- as.numeric(FINAL2$dummy_year)
FINAL2$dummy_year[FINAL2$year > 2010] <- 1
FINAL2$dummy_year[FINAL2$year <= 2010] <- 0
FINAL2$year_dummy <- as.factor(FINAL2$dummy_year)




head(FINAL2$year, 3000)
FINAL2 <- FINAL2[,c("speaker", "party", "presidency", "content", "year", "chamber", "country", "sessionnumber", "year_dummy")]
FINAL2$year <- as.character(FINAL2$year)
FINAL2$sessionnumber <- as.character(FINAL2$sessionnumber)
save(FINAL2, file ="Budget_data_clean.RData")


# Add year-dummy to RePEc Data #### -> Copy Paste to Prep Script

load(file = "dfmat_repec.RData")
dfmat_repec <- dfmat_sent
rm(dfmat_sent)

dfmat_repec$dummy_year <- NA
dfmat_repec$dummy_year <- as.numeric(dfmat_repec$dummy_year)
dfmat_repec$dummy_year[dfmat_repec$year > 2010] <- 1
dfmat_repec$dummy_year[dfmat_repec$year <= 2010] <- 0
dfmat_repec$year_dummy <- as.factor(dfmat_repec$dummy_year)

save(dfmat_repec, file = "repec_dfmat.RData")

load(file = "repec_dfm_forstm.RData")

repec_dfm_forstm$dummy_year <- NA
repec_dfm_forstm$dummy_year <- as.numeric(repec_dfm_forstm$dummy_year)
repec_dfm_forstm$dummy_year[repec_dfm_forstm$year > 2010] <- 1
repec_dfm_forstm$dummy_year[repec_dfm_forstm$year <= 2010] <- 0
repec_dfm_forstm$year_dummy <- as.factor(repec_dfm_forstm$dummy_year)

save(repec_dfm_forstm, file = "repec_dfm_forstm.RData")

rm(list=ls())
