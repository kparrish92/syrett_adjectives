# ------------------------------------------------------
# Author: Kyle Parrish
# Date 5/2/22
# This script creates many of the plots used in the manuscript 
# including descriptive data etc.
# -------------------------------------------------------


# load data 
source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "03_load_data.R"))

# make summary tables for plots and tables 
desc_df_eng = english_l1_pct %>% 
  group_by(phoneme, stim_language, choice) %>% 
  summarize(n = n(), mean_rating = mean(slider.response)) %>% 
  mutate(L1 = "English") %>% 
  filter(stim_language == "french") %>% 
  mutate(percentage = n/550) 

desc_df_eng$pct = round(desc_df_eng$percentage, digits = 2)*100

desc_df_eng_g = english_l1_pct_g %>% 
  group_by(choice, phoneme, stim_language) %>% 
  summarize(n = n(), mean_rating = mean(slider.response)) %>% 
  mutate(L1 = "English") %>% 
  filter(stim_language == "German") %>% 
  mutate(percentage = n/550) 

desc_df_eng_g$pct = round(desc_df_eng_g$percentage, digits = 2)*100

desc_df_span = spanish_l1_pct %>% 
  group_by(choice, phoneme, language_chosen) %>% 
  summarize(n = n(), mean_rating = mean(slider.response)) %>% 
  mutate(L1 = "Spanish") %>% 
  mutate(stim = "French") %>% 
  mutate(percentage = n/570)

desc_df_span$pct = round(desc_df_span$percentage, digits = 2)*100

desc_df_span_g = spanish_l1_pct_g %>% 
  group_by(choice, phoneme, language_chosen) %>% 
  summarize(n = n(), mean_rating = mean(slider.response)) %>% 
  mutate(L1 = "Spanish") %>% 
  mutate(stim = "German") %>% 
  mutate(percentage = n/570)

desc_df_span_g$pct = round(desc_df_span_g$percentage, digits = 2)*100


# Age of onset

eng_blp$L1_group <- "English"
span_blp$L1_group <- "Spanish"

plot_blp = rbind(eng_blp, span_blp) %>% 
  mutate(l2_ao = str_replace(l2_ao, "Since birth", "0"),
         l2_ao = str_replace(l2_ao, "Desde el nacimiento", 
                             "0"),
         l2_aoa = str_replace(l2_aoa, 
                              "Tan pronto como recuerdo.", 
                              "0"))


plot_blp$l2_ao <- factor(plot_blp$l2_ao, 
                         levels = 
                           c("0",
                             "1", 
                             "2", 
                             "3",
                             "4",
                             "5",
                             "6",
                             "7",
                             "8",
                             "9",
                             "10",
                             "11",
                             "12",
                             "13",
                             "14",
                             "15",
                             "16",
                             "17",
                             "18",
                             "19",
                             "20+"))

ao = plot_blp %>% 
  ggplot(aes(y = l2_ao, fill = L1_group)) + 
  geom_bar(color = "black") + 
  xlim(0,25) +
  facet_wrap(~L1_group) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ylab("Age of Onset") + xlab("Count") 



# Age of acquisition

plot_blp$l2_aoa <- factor(plot_blp$l2_aoa, 
                          levels = 
                            c("0",
                              "1", 
                              "2", 
                              "3",
                              "4",
                              "5",
                              "6",
                              "7",
                              "8",
                              "9",
                              "10",
                              "11",
                              "12",
                              "13",
                              "14",
                              "15",
                              "16",
                              "17",
                              "18",
                              "19",
                              "20+"))

aoa = plot_blp %>% 
  ggplot(aes(y = l2_aoa, fill = L1_group)) + 
  geom_bar(color = "black") + 
  xlim(0,25) +
  facet_wrap(~L1_group) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ylab("Age of Acquisition") + xlab("Count") 


ggarrange(ao, aoa, labels = c('A', 'B')) +
  ggsave(here("MDPI_template", "figs", "ao_aoa_combined.png"),
         dpi = 1200)

# Self-rated Production proficiency
spoken = plot_blp %>% 
  ggplot(aes(x = l2_prof_production, fill = L1_group)) + 
  geom_bar(color = "black") + 
  xlim(0,7) + facet_wrap(~L1_group) +
  ylab("Count") + xlab("Self-rating") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ggtitle("Spoken proficiency") 


mean(eng_blp$l2_prof_production)
sd(eng_blp$l2_prof_production)

mean(span_blp$l2_prof_production) 
sd(eng_blp$l2_prof_production)


# Self-rated perception proficiency
perception = plot_blp %>% 
  ggplot(aes(x = l2_prof_perception, fill = L1_group)) + 
  geom_bar(color = "black") + 
  xlim(0,7) + facet_wrap(~L1_group) +
  ylab("Count") + xlab("Self-rating") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ggtitle("Perceptual Ability") 

ggarrange(perception, spoken, labels = c('A', 'B')) +
  ggsave(here("MDPI_template", "figs", "proficiency_combined.png"),
         dpi = 1200)


### Stim data plot 

### manually inserting values from PRAAT textgrid
### F1 and F2 values are means over the duration of the vowel
### The measurements were taken from the fricatives 
### though the vowels were the same sources


stim_data = data.frame(phoneme = c("i", "o", "^", "y"),
                       f1 = c(508, 598.39, 601.57, 444.18), 
                       f2 = c(1966.73, 1267.28, 1755.68, 1776.17), 
                       Language = "French Stimulus")

stim_data_german = data.frame(phoneme = c("o", "i", "y", "^"),
                              f1 = c(417, 322, 411, 736), 
                              f2 = c(849, 2533, 2053, 1304), 
                              Language = "German choices")


stim_data_eng = data.frame(phoneme = c("i", "a", "^", "u"),
                           f1 = c(390, 752, 718, 479), 
                           f2 = c(2487, 1215, 1259, 870), 
                           Language = "English choices")


stim_data_span = data.frame(phoneme = c("i", "o", "u"),
                            f1 = c(331, 369, 320), 
                            f2 = c(2475, 1430, 1639), 
                            Language = "Spanish choices")



comb = rbind(stim_data, stim_data_eng, stim_data_span, stim_data_german)

comb %>% 
  ggplot(aes(x = f2, y = f1, 
             label = phoneme, color = Language)) + 
  geom_text() + scale_y_reverse() + scale_x_reverse()  + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.05, 
                                        linetype = 'dashed', colour = "red")) + 
  ggtitle("Formant values of the stimuli and choices") + 
  theme(legend.position="bottom") +
  ggsave(here("MDPI_template", "figs", "stimuli.png"),
         dpi = 1200)



### Tables 
desc_df_eng %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()



# Spanish raw data 
span_desc_plot = rbind(desc_df_span, desc_df_span_g)  %>% 
  ggplot(aes(x = choice, y = percentage, 
             fill = mean_rating)) +  
  geom_col(color = "black") +
  scale_x_discrete(limits=c("fin", "su", "son",
                            "fool", "fought", "fun",
                            "feel")) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.text.x = element_text(color="black",
                                   size=8, angle=90)) +
  theme(text=
          element_text(
            size=10,
            family="Times New Roman")) +
  ylim(0, 1) +
  facet_grid(stim~phoneme) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.position = "bottom") +
  ggsave(here("MDPI_template", "figs", "span_desc_plot.png"),
         dpi = 1200)

## Descriptive tables of spanish 

### French
span_fr = desc_df_span %>% 
  mutate(percentage = round(percentage, digits = 2)) %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()
### German 

span_ger = desc_df_span_g %>% 
  mutate(percentage = round(percentage, digits = 2)) %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()

# English raw data 
eng_desc_plot = rbind(desc_df_eng, desc_df_eng_g)  %>% 
  ggplot(aes(x = choice, y = percentage, 
             fill = mean_rating)) +  
  geom_col(color = "black") +
  scale_x_discrete(limits=c("fin", "su", "son",
                            "fool", "fought", "fun",
                            "feel")) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.text.x = element_text(color="black",
                                   size=8, angle=90)) +
  theme(text=
          element_text(
            size=10,
            family="Times New Roman")) +
  ylim(0, 1) +
  facet_grid(stim_language~phoneme) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.position = "bottom") +
  ggsave(here("MDPI_template", "figs", "eng_desc_plot.png"),
         dpi = 1200)


# Descriptive tables of English 

## French 

eng_fr = desc_df_eng %>% 
  mutate(percentage = round(percentage, digits = 2)) %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()

## German 

eng_ger = desc_df_eng_g %>% 
  mutate(percentage = round(percentage, digits = 2)) %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()

