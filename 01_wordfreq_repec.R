### Word Frequencies RePEc Data ###
### Author: Fabian Pawelczyk ###
### Date: 17/09/2021 ###

setwd("./replication_data_fp")
load(file = "dfmat_repec.RData")
load(file = "toks_repec.RData")

repec_dfmat <- dfmat_sent
repec_toks <- toks_sent
rm(dfmat_sent)
rm(toks_sent)

# Packages ####
require(quanteda)
require(quanteda.textstats)
require(ggplot2)
require(lubridate)

### Show most frequent features

#topfeatures(repec_dfmat, 30)

### create wordcloud

#set.seed(132)
#textplot_wordcloud(repec_dfmat, max_words = 100)


## Simple frequency analysis 

freq_grouped <- textstat_frequency(repec_dfmat, 
                                   groups = repec_dfmat$year)


freq_countercyclical <- subset(freq_grouped, freq_grouped$feature %in% "countercyclical")
freq_new_keynesian <- subset(freq_grouped, freq_grouped$feature %in% "new_keynesian")
freq_multiplier <- subset(freq_grouped, freq_grouped$feature %in% "multiplier")
freq_multipliers <- subset(freq_grouped, freq_grouped$feature %in% "multipliers")
freq_fiscal_multiplier <- subset(freq_grouped, freq_grouped$feature %in% "fiscal_multiplier")
freq_fiscal_multipliers <- subset(freq_grouped, freq_grouped$feature %in% "fiscal_multipliers")
#freq_consolidation <- subset(freq_grouped, freq_grouped$feature %in% "consolidation")
freq_keynes <- subset(freq_grouped, freq_grouped$feature %in% "keynes")
freq_keynesian <- subset(freq_grouped, freq_grouped$feature %in% "keynesian")
#freq_ricardian <- subset(freq_grouped, freq_grouped$feature %in% "ricardian")
freq_state <- subset(freq_grouped, freq_grouped$feature %in% "state")
freq_intervention <- subset(freq_grouped, freq_grouped$feature %in% "orthodox")
freq_stimulus <- subset(freq_grouped, freq_grouped$feature %in% "stimulus")
#freq_keynesianism <- subset(freq_grouped, freq_grouped$feature %in% "fiscal_policy")
freq_bailout <- subset(freq_grouped, freq_grouped$feature %in% "bailout")
freq_bailouts <- subset(freq_grouped, freq_grouped$feature %in% "bailouts")
freq_crowdingin <- subset(freq_grouped, freq_grouped$feature %in% "crowding-in")
freq_fiscalstimulus <- subset(freq_grouped, freq_grouped$feature %in% "fiscal_stimulus")
freq_governmentinter <- subset(freq_grouped, freq_grouped$feature %in% "government_intervention")
freq_governmentcons <- subset(freq_grouped, freq_grouped$feature %in% "government_consumption")
freq_governmentspen <- subset(freq_grouped, freq_grouped$feature %in% "government_spending")
freq_fiscalpolicy<- subset(freq_grouped, freq_grouped$feature %in% "fiscal_policy")

# Check some words individually

#kw_multiword <- kwic(repec_toks, pattern = phrase(c("new keynes*", "fiscal multipl*")))
#kw_multiword <- kwic(repec_toks, pattern = phrase(c("government spending", "fiscal spending", "public spending", "state spending", "governement debt", "public debt", )))
#kw_multiword <- kwic(repec_toks, pattern = phrase(c("deficit_spending*")))






# Create Plots for: Frequency Analysis ####

colors <- c("multipliers" = "blue", "multiplier" = "red", "state" = "orange", "intervention" = "black")

ggplot(freq_multiplier, aes(x = as.numeric(group))) +
  ggtitle("Word frequencies: multiplier, multipliers, state, intervention") +
  labs(y="Frequency", x = "Year", color = "Legend", caption = "The graphs show the total word frequency in 
       the repec data set for following words: multiplier (red); multipliers (blue); state (orange); intervention (black)") +
  theme_bw() + 
  theme(plot.caption = element_text(size=8)) + 
  geom_line(aes(y = frequency), color = "red") +
  geom_line(data = freq_state, aes(y = frequency), color = "orange") +
  geom_line(data = freq_intervention, aes(y = frequency)) +
  geom_line(data = freq_multipliers, aes(y = frequency), color = "blue")+
scale_y_continuous(limits = c(0, 400), breaks = c(seq(0, 14, 2))) +
  xlab(NULL) + 
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Plot total frequencies with for selected words

as.numeric(gg_wordfreq$group)

gg_wordfreq <- ggplot(freq_multiplier, aes(x = as.numeric(group))) +
  labs(y="Frequency", x = "Year", color = "Legend") +
  theme_bw() + 
  theme(plot.caption = element_text(size=8)) + 
  geom_line(aes(y = frequency, color = "multiplier")) +
  geom_line(data = freq_keynes, aes(y = frequency, colour = "keynes")) +
  geom_line(data = freq_countercyclical, aes(y = frequency, color = "countercyclical")) +
  geom_line(data = freq_fiscal_multiplier, aes(y = frequency, color = "fiscal multiplier")) +
  geom_line(data = freq_fiscal_multipliers, aes(y = frequency,  color = "fiscal multipliers")) +
  geom_line(data = freq_governmentinter, aes(y = frequency,  color = "government intervention")) +
  geom_line(data = freq_keynesian, aes(y = frequency, color = "keynesian")) +
  geom_line(data = freq_new_keynesian, aes(y = frequency,  color = "new keynesian")) +
  geom_line(data = freq_stimulus, aes(y = frequency, color = "stimulus")) +
  geom_vline(xintercept = 2008) +
  xlim(1999, 2020) +
  scale_color_manual(name="", 
                     values = c("keynes"="green", "multiplier"="red",
                                "countercyclical"="coral4",
                                "fiscal multiplier"="darkseagreen4",
                                "fiscal multipliers"="hotpink3",
                                "government intervention"="cyan2",
                                "keynesian"="tomato2",
                                "new keynesian"="greenyellow",
                                "stimulus" = "black"))

gg_wordfreq
ggsave("./Plots/totfreq_repec99.png", width = 26, height = 12, units = "cm")






# Rank Graph Pro State Interventionsm starting 1999 ####
# Rows removed because of observation before 1999

gg_rank_keynes <- ggplot(freq_multiplier, aes(x = as.numeric(group))) +
  labs(y="Rank", x = "Year", color = "Legend") +
  theme_bw() + 
  theme(plot.caption = element_text(size=8)) + 
  geom_line(aes(y = rank, color = "multiplier")) +
  geom_line(data = freq_keynes, aes(y = rank, colour = "keynes")) +
  geom_line(data = freq_countercyclical, aes(y = rank, color = "countercyclical")) +
  geom_line(data = freq_fiscal_multiplier, aes(y = rank, color = "fiscal multiplier")) +
  geom_line(data = freq_fiscal_multipliers, aes(y = rank,  color = "fiscal multipliers")) +
  geom_line(data = freq_governmentinter, aes(y = rank,  color = "government intervention")) +
  geom_line(data = freq_keynesian, aes(y = rank, color = "keynesian")) +
  geom_line(data = freq_new_keynesian, aes(y = rank,  color = "new keynesian")) +
  geom_line(data = freq_stimulus, aes(y = rank, color = "stimulus")) +
  geom_vline(xintercept = 2008) +
  xlim(1999, 2020) +
  scale_color_manual(name="", 
                     values = c("keynes"="green", "multiplier"="red",
                                "countercyclical"="coral4",
                                "fiscal multiplier"="darkseagreen4",
                                "fiscal multipliers"="hotpink3",
                                "government intervention"="cyan2",
                                "keynesian"="tomato2",
                                "new keynesian"="greenyellow",
                                "stimulus" = "black"))

gg_rank_keynes
ggsave("./Plots/rank_repec99.png", width = 26, height = 12, units = "cm")

rm(list = ls())






