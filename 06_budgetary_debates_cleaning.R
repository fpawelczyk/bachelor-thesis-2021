#### Separation of Bundestag speeches from pdfs ###
### Author: Pit Rieger ###
### Edited by: Fabian Pawelczyk ###
### 21.08.2021 ###
library(stringr)
library(pdftools)
library(openxlsx)



## Create file paths for all pdfs
setwd("./replication_data_fp")
#sessionmax <- as.numeric(readline("What is the most recent plenary session number? ")) # Type 215 into console
sessionmax <- as.numeric(215)
session <- c(1:sessionmax)
session <- as.character(session)
session[1:9] <- paste0("00", session[1:9]) ### Add two zeros before first nine numbers
session[10:min(99,sessionmax)] <- paste0("0", session[10:min(99,sessionmax)])
filepaths <- paste0("./replication_data_fp/Parlamentsdebatten/", session, ".pdf")
rm(session)     

# START OF BIG LOOP through all pdfs
FINAL <- data.frame()                  
for (h in c(1:sessionmax)){
  text <- pdf_text(filepaths[h]) ## IMPORT
  startpage <- grep("Beginn:[[:space:]]", text) ## First Page number of speeches
  endpage <- grep("\\Q(\\ESchluss.+Uhr\\Q)\\E", text) ## Last Page number of speeches
  meta <- text[1:startpage-1] ## list of speakers, etc.
  text2 <- text[startpage:endpage] ## Speech part of document
  rm(startpage, endpage, text)
  
  text2 <- strsplit(text2, "\n") # split into strings by new line
  
  ## FIRST PAGE EXTRA WITHOUT GREPL ":" and removing header
  text2[[1]] <- text2[[1]][6:length(text2[[1]])] #starting with string 6
  text_c1 <- substring(text2[[1]], 5, 67) # extract 1st column
  text_c2 <- substring(text2[[1]], 74, 131) # extract 2nd column
  text2[[1]] <- c(text_c1,text_c2) # bind columns
  text2[[1]] <- gsub("[[:blank:]]{2,}", "", text2[[1]]) #get rid of unnecessary blanks
  text2[[1]] <- gsub("^[[:blank:]]{1}", "", text2[[1]]) 
  #Check to see whether page actually starts with string 6 -> 1st string should now be the name of the president
  # text2[1]
  
  ## remaining pages 2+
  for (i in c(2:length(text2))){
    text2[[i]] <- text2[[i]][2:length(text2[[i]])] #remove headline
    text_c1 <- substring(text2[[i]], 5, 67) #extract 1st column
    text_c2 <- substring(text2[[i]], 74, 131) #extract 2nd column
    text2[[i]] <- c(text_c1,text_c2) # bind columns
    if (grepl(":", text2[[i]][1])==F){
      text2[[i]] <- text2[[i]][2:length(text2[[i]])] # if a speaker already started on the previous page, the name is repeated without ":". hence in that case, the first string is deleted
    }
    text2[[i]] <- gsub("[[:blank:]]{2,}", "", text2[[i]]) #remove unnecessary blanks
    text2[[i]] <- gsub("^[[:blank:]]{1}", "", text2[[i]]) 
    rm(text_c1, text_c2)
  }
  rm(i)  
  protocol <- unlist(text2) ## bind all pages together
  
  ## regular expressions for speakers
  ## Must include parties, presidents and generic identifiers for ministers / secretaries of state
  party_speaker <- c("\\Q(\\ECDU/CSU\\Q):\\E$", "\\Q(\\ESPD\\Q):\\E$", "\\Q(\\EFDP\\Q):\\E$", "\\Q(\\EDIE[[:blank:]]LINKE\\Q):\\E$", "\\Q(\\EBÃNDNIS 90/DIE GRÃNEN\\Q):\\E$", "\\Q(\\EAfD\\Q):\\E$", 
                     "AltersprÃ¤sident Dr. Hermann Otto Solms\\Q:\\E$",  "PrÃ¤sident Dr. Wolfgang SchÃ¤uble\\Q:\\E$", "VizeprÃ¤sident Thomas Oppermann\\Q:\\E$","VizeprÃ¤sidentin Petra Pau\\Q:\\E$", 
                     "VizeprÃ¤sident Dr. Hans-Peter Friedrich\\Q:\\E$", "VizeprÃ¤sidentin Claudia Roth\\Q:\\E$", "VizeprÃ¤sident Wolfgang Kubicki\\Q:\\E$",
                     "^[^t\\(].+, Bundesminister\\>|^[^t\\(].+, Bundesministerin\\>", "^[^\\Q(\\E].+, Parl. StaatssekretÃ¤r", "^[^mH\\(].+, Staatsminister") # For "Bundesminister", the letter "t" needs to be excluded at the beginning of the string
  # because otherwise it includes one occurance that is not a speaker. same for "mH" in "Staatsminister"
  start <- grep(paste0(party_speaker, collapse =  "|"), protocol) # find starting positions of new interventions
  
  ## Split in individual speeches
  speeches <- data.frame() # empty dataframe for all interventions within session
  for (k in c(1:length(start))){
    speaker <- protocol[start[k]] 
    if (k!=length(start)){ 
      content <- paste0(protocol[(start[k]+1):(start[k+1]-1)], collapse = " ") # go from the string following the string identifying speaker k to the string before speaker k+1
    }else {content <- paste0(protocol[(start[k]+1):length(protocol)], collapse = " ")} # if k is the last speaker, go to the end of the document
    a <- data.frame(speaker, content) # create temporary in-loop dataframe
    speeches <- rbind(speeches, a) # bind with session dataframe
    rm(a, speaker, content)
  }
  speeches$content <- gsub("- ", "", speeches$content) # fix remaining formatting errors
  
  ##META INFO 
  meta <- strsplit(meta, "\n")
  speeches$date <- meta[[1]][5]
  speeches$sessionnumber <- as.character(h)
  speeches$chamber <- "Bundestag"
  speeches$country <- "Germany"
  ## bind with final dataframe
  FINAL <- rbind(FINAL, speeches) 
  rm(start, text2, speeches, meta, protocol, party_speaker, k)
}
rm(h, sessionmax, filepaths)

### Try to delete dublicate data


save(FINAL, file ="FINAL.RData")
#load(file = "FINAL.RData")





# Clean data, add parties, presidency
partyreg <- c("\\Q(\\ECDU/CSU\\Q)\\E", "\\Q(\\ESPD\\Q)\\E", "\\Q(\\EFDP\\Q)\\E", "\\Q(\\EDIE[[:blank:]]LINKE\\Q)\\E", "\\Q(\\EBÃNDNIS 90/DIE GRÃNEN\\Q)\\E", "\\Q(\\EAfD\\Q)\\E") #party of speakers
presidencyreg <- c("AltersprÃ¤sident Dr. Hermann Otto Solms\\Q:\\E$",  "PrÃ¤sident Dr. Wolfgang SchÃ¤uble\\Q:\\E$", "VizeprÃ¤sident Thomas Oppermann\\Q:\\E$","VizeprÃ¤sidentin Petra Pau\\Q:\\E$", 
                   "VizeprÃ¤sident Dr. Hans-Peter Friedrich\\Q:\\E$", "VizeprÃ¤sidentin Claudia Roth\\Q:\\E$", "VizeprÃ¤sident Wolfgang Kubicki\\Q:\\E$") # presidency 
FINAL$presidency <- NA

for (i in c(1:length(FINAL$content))){
  FINAL$date2cleanDE[i] <- substring(FINAL$date[i], str_locate(FINAL$date[i], "[[:digit:]]{1,2}")[1], 1000)  
  FINAL$party[i] <- substring(FINAL$speaker[i], str_locate(FINAL$speaker[i], paste0(partyreg, collapse = "|"))[1]+1, str_locate(FINAL$speaker[i], paste0(partyreg, collapse = "|"))[2]-1)
  if (grepl(paste0(presidencyreg, collapse = "|"), FINAL$speaker[i])==T){
    FINAL$presidency[i] <- "Yes"
  } else {FINAL$presidency[i] <- "No"}
  FINAL$day[i] <- substring(FINAL$date2cleanDE[i], 1, str_locate(FINAL$date2cleanDE[i], "[[:digit:]]{1,2}")[2])                      
  FINAL$monthword[i] <- substring(FINAL$date2cleanDE[i], str_locate(FINAL$date2cleanDE[i], "[[:alpha:]]+")[1], str_locate(FINAL$date2cleanDE[i], "[[:alpha:]]+")[2])
  FINAL$year[i] <- substring(FINAL$date2cleanDE[i], str_locate(FINAL$date2cleanDE[i], "[[:digit:]]{4}")[1], str_locate(FINAL$date2cleanDE[i], "[[:digit:]]{4}")[2])
}
print(FINAL$party)
head(FINAL$party, 1000)
tail(FINAL$party, 1000)
summary(FINAL$party)


# CREATE Date  DD/MM/YY
FINAL$month <- NA
FINAL$month[FINAL$monthword=="Januar"]    <- "01"
FINAL$month[FINAL$monthword=="Februar"]   <- "02"
FINAL$month[FINAL$monthword=="MÃ¤rz"]      <- "03"
FINAL$month[FINAL$monthword=="April"]     <- "04"
FINAL$month[FINAL$monthword=="Mai"]       <- "05"
FINAL$month[FINAL$monthword=="Juni"]      <- "06"
FINAL$month[FINAL$monthword=="Juli"]      <- "07"
FINAL$month[FINAL$monthword=="August"]    <- "08"
FINAL$month[FINAL$monthword=="September"] <- "09"
FINAL$month[FINAL$monthword=="Oktober"]   <- "10"
FINAL$month[FINAL$monthword=="November"]  <- "11"
FINAL$month[FINAL$monthword=="Dezember"]  <- "12"
FINAL$date2clean <- paste0(FINAL$day, "/", FINAL$month, "/", FINAL$year)
rm(partyreg, presidencyreg, i)

# identify party of ministers and presidents of BT
a <- FINAL$speaker[is.na(FINAL$party)==T & FINAL$presidency=="No"]
a <- a[duplicated(a)==F]
b <- a[grepl("^[^t\\(].+, Bundesminister\\>|^[^t\\(].+, Bundesministerin\\>",a)==T]
minis <- substring(b, 1, regexpr(",", b)-1)
minis <- minis[duplicated(minis)==F]
minis <- c(minis, "AltersprÃ¤sident Dr. Hermann Otto Solms",  "PrÃ¤sident Dr. Wolfgang SchÃ¤uble", "VizeprÃ¤sident Thomas Oppermann","VizeprÃ¤sidentin Petra Pau", 
           "VizeprÃ¤sident Dr. Hans-Peter Friedrich", "VizeprÃ¤sidentin Claudia Roth", "VizeprÃ¤sident Wolfgang Kubicki")
print(minis)

minisparty <- c("SPD", 
                "BÃNDNIS 90/DIE GRÃNEN",
                "BÃNDNIS 90/DIE GRÃNEN", 
                "SPD",
                "BÃNDNIS 90/DIE GRÃNEN",
                "SPD", 
                "SPD",
                "SPD", 
                "SPD",
                "BÃNDNIS 90/DIE GRÃNEN",
                "SPD",
                "SPD",
                "SPD",
                "SPD",
                "SPD", 
                "SPD",
                "SPD",
                "BÃNDNIS 90/DIE GRÃNEN",
                "SPD", 
                "BÃNDNIS 90/DIE GRÃNEN",
                "SPD",
                "SPD", 
                "SPD",
                "SPD",
                "SPD",
                "SPD",
                "SPD",
                "SPD",
                "SPD",
                "CDU",
                "SPD",
                "SPD",
                "CDU/CSU",
                "SPD",
                "CDU/CSU",
                "SPD",
                "CDU/CSU",
                "CDU/CSU",
                "CDU/CSU",
                "SPD",
                "CDU/CSU",
                "CDU/CSU",
                "FDP",
                "FDP",
                "FDP",
                "SPD",
                "CDU/CSU",
                "CDU/CSU",
                "FDP",
                "CDU/CSU", # 50
                "SPD", 
                "CDU/CSU",
                "CDU/CSU",
                "FDP",
                "FDP", 
                "CDU/CSU",
                "SPD",
                "CDU/CSU",
                "CDU/CSU",
                "CDU/CSU",
                "SPD",
                "CDU/CSU",
                "SPD",
                "SPD",
                "CDU/CSU",
                "SPD",
                "SPD",
                "CDU/CSU",
                "SPD",
                "SPD",
                "SPD",
                "CDU/CSU",
                "SPD",
                "CDU/CSU",
                "CDU/CSU",
                "CDU/CSU",
                "CDU/CSU",
                "CDU/CSU",
                "SPD",
                "SPD",
                "SPD",
                "CDU/CSU",
                "CDU/CSU",
                "SPD",
                "CDU/CSU",
                "SPD",
                "SPD",
                "SPD", # 3 times giffey
                "FDP",
                "CDU/CSU",
                "SPD",
                "Die Linke",
                "CDU/CSU",
                "BÃNDNIS 90/DIE GRÃNEN",
                "FDP")

ministers <- data.frame(minis, minisparty) 
ministers$minis <- as.character(ministers$minis)
ministers$minisparty <- as.character(ministers$minisparty)
FINAL$speaker <- as.character(FINAL$speaker)
for (i in c(1:length(ministers$minis))){
  for(h in c(1:length(FINAL$content))){
    if (grepl(ministers$minis[i], FINAL$speaker[h])==T){
      FINAL$party[h] <- ministers$minisparty[i]
    }
  }
  cat("*")
}
rm(minis, minisparty, ministers, a, b, i, h)

##Clean names
for (i in c(1:length(FINAL$content))){
  FINAL$speaker[i] <- sub("AltersprÃ¤sident ", "",FINAL$speaker[i])
  FINAL$speaker[i] <- sub("PrÃ¤sident ", "",FINAL$speaker[i])
  FINAL$speaker[i] <- sub("VizeprÃ¤sident |VizeprÃ¤sidentin", "",FINAL$speaker[i])
  FINAL$speaker[i] <- sub("AltersprÃ¤sident", "",FINAL$speaker[i])
  FINAL$speaker[i] <- sub("\\(.+\\)", "",FINAL$speaker[i])
  FINAL$speaker[i] <- sub(":", "", FINAL$speaker[i])
  FINAL$speaker[i] <- sub(", .*", "", FINAL$speaker[i])
}

#subset
FINAL2 <- FINAL[,c("speaker", "party", "presidency", "content", "date2clean", "chamber", "country", "sessionnumber")]
FINAL2$date <- FINAL2$date2clean
FINAL2 <- FINAL2[,c("speaker", "party", "presidency", "content", "date", "chamber", "country", "sessionnumber")]
rm(FINAL)
rm(i)


#EXPORT
write.xlsx(FINAL2, "DE_budgetary_debates_2000-2021.xlsx")
write.csv(FINAL2, "DE_budgetary_debates_2000-2021.csv", sep = ",")
save(FINAL2, file ="budget_data_clean.RData")
#save(FINAL2, file ="Budget_data.RData")

rm(FINAL2)


