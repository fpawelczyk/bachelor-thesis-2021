### Compare LSS before and after ###
### Author: Fabian Pawelczyk ###
### Date: 10/09/2021 ###

setwd("./replication_data_fp")
load(file = "tmod_lss_after.RData")
load(file = "tmod_lss_before.RData")
load(file ="tmod_lss_after_nclassic.RData")
load(file ="tmod_lss_before_nclassic.RData")


# Packages ####
require(LSX)
require(ggplot2)


# Calculate difference of vectors for Keynesian Dict ####
# Try to get two vectors to calculate the differences

before_coef_vec <- head(coef(tmod_lss_before), 50)
#print(before_coef_vec)


after_coef_vec <- head(coef(tmod_lss_after), 50)
#print(after_coef_vec)






#test_vec <- names(before_coef_vec) %in% names(after_coef_vec)



after_coef_vec <- head(coef(tmod_lss_after), 50) # most positive words

name_vector <- names(before_coef_vec)

### Create an index for order of names
reorder_idx <- match(names(before_coef_vec), names(after_coef_vec))
#reorder_idx


### Reordering 'after_vector'
after_coef_vec
after_coef_vec <- after_coef_vec[reorder_idx]

### Create a df with two numerical vectors and the word vector

df_coef <- data.frame(name_vector,before_coef_vec, after_coef_vec,
                      after_coef_vec - before_coef_vec)



# Textplots Keynesian Dict ####
# Marked words are words who get more positive sentiments over time

# Sort the data
sort(df_coef$after_coef_vec - before_coef_vec, decreasing = T)
#edit(df_coef) # Show Data in another window

#tp_more_positive_keynes <- textplot_terms(tmod_lss_before, 
              # highlighted = c("fiscal deficits", "keynesian", "new_keynesian",
                              # "state_intervention", "fiscal_deficit", "inflation",
                               #"bailouts", "government_spending", "fiscal_multipliers",
                               #"regulatory_policies", "government_intervention", 
                              # "regulation", "policy_intervention", "bailout"
                              # ))


extract_1<- textplot_terms(tmod_lss_before)
df_1 <- extract_1$data

df_1$hl <- ifelse(df_1$word == "fiscal_deficits"|df_1$word == "keynesian"|
                    df_1$word =="new_keynesian"|df_1$word == "state_intervention"|
                    df_1$word == "fiscal_deficit"|df_1$word == "inflation"|
                    df_1$word == "bailouts" |df_1$word == "government_spending"|
                    df_1$word == "fiscal_multipliers"| df_1$word == "regulatory_policies"|
                    df_1$word == "government_intervention"|df_1$word == "regulation"|
                    df_1$word == "policy_intervention"|df_1$word == "bailout", 1, 0)

# GGPlot more positive (keynes) over time before 2010 ####
ggplot(data = df_1, aes(x=beta, y=frequency))+
  geom_text(aes(label = word),color = ifelse(df_1$hl==0,
  "grey40", "orange2"), size = 3.5, check_overlap = F) +
  labs(x = "Polarity", y = "Frequency(log)")+
  ylim(1, 10)+
  xlim(-0.07,0.07)+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(family = "serif"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("./Plots/pos_kyns_before.png", width = 26, height = 12, units = "cm")


# GGPlot more POSITIVE (keynes) over time AFTER 2010 ####

extract_2<- textplot_terms(tmod_lss_after)
df_2 <- extract_2$data

df_2$hl <- ifelse(df_2$word == "fiscal_deficits"|df_2$word == "keynesian"|
                    df_2$word =="new_keynesian"| df_2$word == "state_intervention"|
                    df_2$word == "fiscal_deficit"|df_2$word == "inflation"|
                    df_2$word == "bailouts" |df_2$word == "government_spending"|
                    df_2$word == "fiscal_multipliers"| df_2$word == "regulatory_policies"|
                    df_2$word == "government_intervention"|df_2$word == "regulation"|
                    df_2$word == "policy_intervention"|df_2$word == "bailout",
                  1, 0)

ggplot(data = df_2, aes(x=beta, y=frequency))+
  geom_text(aes(label = word),color = ifelse(df_2$hl==0,
            "grey40", "orange2"), size = 3.5, check_overlap = F) +
  labs(x = "Polarity", y = "Frequency(log)")+
  ylim(1, 10)+
  xlim(-0.07,0.07)+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(family = "serif"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("./Plots/pos_kyns_after.png", width = 26, height = 12, units = "cm")


# GGPlot more negative (keynes) over time before 2010 ####

extract_3<- textplot_terms(tmod_lss_before)
df_3 <- extract_3$data

df_3$hl <- ifelse(df_3$word == "fiscal_stimulus"|df_3$word == "government_interventions"|
                    df_3$word =="budget_deficit"| df_3$word == "government_debt"|
                    df_3$word == "keynes"|df_3$word == "regulatory_policy"|
                    df_3$word == "government_consumption" |df_3$word == "stimulus"|
                    df_3$word == "public_debt"| df_3$word == "budget_deficts"|
                    df_3$word == "countercyclical"|df_3$word == "policy_interventions"|
                    df_3$word == "public_spending", 1, 0)


ggplot(data = df_3, aes(x=beta, y=frequency))+
  geom_text(aes(label = word),color = ifelse(df_3$hl==0,
                                             "grey40", "orange2"), size = 3.5, check_overlap = F) +
  labs(x = "Polarity", y = "Frequency(log)")+
  ylim(1, 10)+
  xlim(-0.07,0.07)+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(family = "serif"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("./Plots/neg_kyns_before.png", width = 26, height = 12, units = "cm")

# GGPlot more negative (keynes) over time AFTER 2010 ####

extract_4<- textplot_terms(tmod_lss_after)
df_4 <- extract_4$data

df_4$hl <- ifelse(df_4$word == "fiscal_stimulus"|df_4$word == "government_interventions"|
                    df_4$word =="budget_deficit"| df_4$word == "government_debt"|
                    df_4$word == "keynes"|df_4$word == "regulatory_policy"|
                    df_4$word == "government_consumption" |df_4$word == "stimulus"|
                    df_4$word == "public_debt"| df_4$word == "budget_deficts"|
                    df_4$word == "countercyclical"|df_4$word == "policy_interventions"|
                    df_4$word == "public_spending", 1, 0)


ggplot(data = df_4, aes(x=beta, y=frequency))+
  geom_text(aes(label = word),color = ifelse(df_4$hl==0,
                                             "grey40", "orange2"), size = 3.5, check_overlap = F) +
  labs(x = "Polarity", y = "Frequency(log)")+
  ylim(1, 10)+
  xlim(-0.07,0.07)+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(family = "serif"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("./Plots/neg_kyns_after.png", width = 26, height = 12, units = "cm")


# Calculate difference of vectors for Classical Dict ####

# Try to get two vectors to calculate the differences

before_coef_vec_nclassic <- head(coef(tmod_lss_before_nclassic), 50)
#print(before_coef_vec)


after_coef_vec_nclassic <- head(coef(tmod_lss_after_nclassic), 50)
#print(after_coef_vec)






#test_vec <- names(before_coef_vec) %in% names(after_coef_vec)



after_coef_vec_nclassic <- head(coef(tmod_lss_after_nclassic), 50) # most positive words

name_vector <- names(before_coef_vec_nclassic)

### Create an index for order of names
reorder_idx <- match(names(before_coef_vec_nclassic), names(after_coef_vec_nclassic))
#reorder_idx


### Reordering the after_vector
after_coef_vec_nclassic
after_coef_vec_nclassic <- after_coef_vec_nclassic[reorder_idx]

### Create a df with two numerical vectors and the word vector

df_coef <- data.frame(name_vector,before_coef_vec_nclassic, after_coef_vec_nclassic,
                      after_coef_vec_nclassic - before_coef_vec_nclassic)

# Textplots Classical Dict ####
# Marked words are words who get more positive sentiments over time

#edit(df_coef)

textplot_terms(tmod_lss_before_nclassic, 
               highlighted = c("tax_cuts", "fiscal_discipline", "austerity", 
                               "ricardian_equivalence", "neoclassical",
                               "fiscal_consolidations", "free_market", 
                               "tax_cut", "free_trade", "privatization",
                               "deregulation"))

# GGPlot more positive (nclassic) over time BEFORE 2010 ####

extract_5<- textplot_terms(tmod_lss_before_nclassic)
df_5 <- extract_5$data

df_5$hl <- ifelse(df_5$word == "tax_cuts"|df_5$word == "fiscal_discipline"|
                    df_5$word =="austerity"| df_5$word == "ricardian_equivalence"|
                    df_5$word == "neoclassical"|df_5$word == "fiscal_consolidations"|
                    df_5$word == "free_market" |df_5$word == "tax_cut"|
                    df_5$word == "free_trade"| df_5$word == "privatization"|
                    df_5$word == "deregulation", 1, 0)


ggplot(data = df_5, aes(x=beta, y=frequency))+
  geom_text(aes(label = word),color = ifelse(df_5$hl==0,
                                             "grey40", "orange2"), size = 3.5, check_overlap = F) +
  labs(x = "Polarity", y = "Frequency(log)")+
  ylim(1, 10)+
  xlim(-0.07,0.07)+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(family = "serif"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("./Plots/pos_nclssc_before.png", width = 26, height = 12, units = "cm")

#edit(df_coef)

textplot_terms(tmod_lss_after_nclassic, 
               highlighted = c("tax_cuts", "fiscal_discipline", "austerity", 
                               "ricardian_equivalence", "neoclassical",
                               "fiscal_consolidations", "free_market", 
                               "tax_cut", "free_trade", "privatization",
                               "deregulation"))

# GGPlot more positive (nclassic) over time after 2010 ####

extract_6<- textplot_terms(tmod_lss_after_nclassic)
df_6 <- extract_6$data

df_6$hl <- ifelse(df_6$word == "tax_cuts"|df_6$word == "fiscal_discipline"|
                    df_6$word =="austerity"| df_6$word == "ricardian_equivalence"|
                    df_6$word == "neoclassical"|df_6$word == "fiscal_consolidations"|
                    df_6$word == "free_market" |df_6$word == "tax_cut"|
                    df_6$word == "free_trade"| df_6$word == "privatization"|
                    df_6$word == "deregulation", 1, 0)


ggplot(data = df_6, aes(x=beta, y=frequency))+
  geom_text(aes(label = word),color = ifelse(df_6$hl==0,
                                             "grey40", "orange2"), size = 3.5, check_overlap = F) +
  labs(x = "Polarity", y = "Frequency(log)")+
  ylim(1, 10)+
  xlim(-0.07,0.07)+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(family = "serif"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("./Plots/pos_nclssc_after.png", width = 26, height = 12, units = "cm")


# Marked words are words who get more negative sentiments over time


# GGPlot more negative (nclassic) over time BEFORE 2010 ####

extract_7<- textplot_terms(tmod_lss_before_nclassic)
df_7 <- extract_7$data

df_7$hl <- ifelse(df_7$word == "monetarism"|df_7$word == "monetarist"|
                    df_7$word =="crowding-out"| df_7$word == "laissez-faire"|
                    df_7$word == "fiscal_consolidation", 1, 0)


ggplot(data = df_7, aes(x=beta, y=frequency))+
  geom_text(aes(label = word),color = ifelse(df_7$hl==0,
                                             "grey40", "orange2"), size = 3.5, check_overlap = F) +
  labs(x = "Polarity", y = "Frequency(log)")+
  ylim(1, 10)+
  xlim(-0.07,0.07)+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(family = "serif"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("./Plots/neg_nclssc_before.png", width = 26, height = 12, units = "cm")

# GGPlot more negative (nclassic) over time AFTER 2010 ####

extract_8<- textplot_terms(tmod_lss_after_nclassic)
df_8 <- extract_8$data

df_8$hl <- ifelse(df_8$word == "monetarism"|df_8$word == "monetarist"|
                    df_8$word =="crowding-out"| df_8$word == "laissez-faire"|
                    df_8$word == "fiscal_consolidation", 1, 0)


ggplot(data = df_8, aes(x=beta, y=frequency))+
  geom_text(aes(label = word),color = ifelse(df_8$hl==0,
                                             "grey40", "orange2"), size = 3.5, check_overlap = F) +
  labs(x = "Polarity", y = "Frequency(log)")+
  ylim(1, 10)+
  xlim(-0.07,0.07)+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(family = "serif"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("./Plots/neg_nclssc_after.png", width = 26, height = 12, units = "cm")
               



rm(list=ls()) # clear
# END
