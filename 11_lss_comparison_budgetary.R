### Compare LSS for Budgetary Debate before and after ###
### Author: Fabian Pawelczyk ###
### Date: 23/10/2021 ###

setwd("./replication_data_fp")
load(file = "tmod_lss_after_dbts.RData")
load(file = "tmod_lss_before_dbts.RData")



# Packages ####
require(LSX)
require(ggplot2)


# Calculate difference of vectors for Keynesian Dict ####
# Try to get two vectors to calculate the differences

before_coef_vec <- head(coef(tmod_lss_before_dbts), 50)
#print(before_coef_vec)


after_coef_vec <- head(coef(tmod_lss_after_dbts), 50)
#print(after_coef_vec)






#test_vec <- names(before_coef_vec) %in% names(after_coef_vec)



after_coef_vec <- head(coef(tmod_lss_after_dbts), 50) # most positive words

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

extract_1<- textplot_terms(tmod_lss_before_dbts)
df_1 <- extract_1$data
#edit(df_coef)

df_1$hl <- ifelse(df_1$word == "regulierungen"|df_1$word == "subventionierung"|
                    df_1$word =="subventioniert"|df_1$word == "kredite"|
                    df_1$word == "gekürzt"|df_1$word == "ausgeglichener_haushalt"|
                    df_1$word == "keynes" |df_1$word == "defizite"|
                    df_1$word == "subventionen"| df_1$word == "neuverschuldung"|
                    df_1$word == "konsolidieren"|df_1$word == "defizit", 1, 0)

# GGPlot more all words over time before 2012 ####
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

ggsave("./Plots/budgetary_positive_before.png", width = 26, height = 12, units = "cm")


# GGPlot more POSITIVE (keynes) over time AFTER 2010 ####

extract_2<- textplot_terms(tmod_lss_after_dbts)
df_2 <- extract_2$data

df_2$hl <- ifelse(df_2$word == "regulierungen"|df_2$word == "subventionierung"|
                    df_2$word =="subventioniert"|df_2$word == "kredite"|
                    df_2$word == "gekürzt"|df_2$word == "ausgeglichener_haushalt"|
                    df_2$word == "keynes" |df_2$word == "defizite"|
                    df_2$word == "subventionen"| df_2$word == "neuverschuldung"|
                    df_2$word == "konsolidieren"|df_2$word == "defizit", 1, 0)

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

ggsave("./Plots/budgetary_posi_after.png", width = 26, height = 12, units = "cm")









rm(list=ls()) # clear
# END
