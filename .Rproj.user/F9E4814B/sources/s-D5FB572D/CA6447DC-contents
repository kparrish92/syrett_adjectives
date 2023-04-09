# ------------------------------------------------------
# Author: Kyle Parrish
# Date 5/2/22
# This script tidies the model output and creates plots
#
# -------------------------------------------------------


# Load 
mod_span = readRDS(here("data", "perception", "models", "ns_span.rds"))
mod_span_g = readRDS(here("data", "perception", "models", "ns_span_g.rds"))
mod_eng = readRDS(here("data", "perception", "models", "ns_eng.rds"))
mod_eng_g = readRDS(here("data", "perception", "models", "ns_eng_g.rds"))

# Tidy
f = conditional_effects(mod_span, categorical = TRUE) 
f_g = conditional_effects(mod_span_g, categorical = TRUE) 

e = conditional_effects(mod_eng, categorical = TRUE) 
e_g = conditional_effects(mod_eng_g, categorical = TRUE) 

# Spanish L1 - French stim 
eff_df = f[["phoneme:cats__"]] %>% 
  mutate(language = case_when(
    cats__ == "feel" ~ "English",
    cats__ == "fool" ~ "English",
    cats__ == "fought" ~ "English",
    cats__ == "fun" ~ "English",
    cats__ == "son" ~ "Spanish",
    cats__ == "su" ~ "Spanish",         
    cats__ == "fin" ~ "Spanish")) %>% 
  mutate(stim_language = "French") %>% 
  mutate(L1 = "Spanish")

eff_df_g = f_g[["phoneme:cats__"]] %>% 
  mutate(language = case_when(
    cats__ == "feel" ~ "English",
    cats__ == "fool" ~ "English",
    cats__ == "fought" ~ "English",
    cats__ == "fun" ~ "English",
    cats__ == "son" ~ "Spanish",
    cats__ == "su" ~ "Spanish",         
    cats__ == "fin" ~ "Spanish")) %>% 
  mutate(stim_language = "German") %>% 
  mutate(L1 = "Spanish")

eff_df_e = e[["phoneme:cats__"]] %>% 
  mutate(language = case_when(
    cats__ == "feel" ~ "English",
    cats__ == "fool" ~ "English",
    cats__ == "fought" ~ "English",
    cats__ == "fun" ~ "English",
    cats__ == "son" ~ "Spanish",
    cats__ == "su" ~ "Spanish",         
    cats__ == "fin" ~ "Spanish")) %>% 
  mutate(stim_language = "French") %>% 
  mutate(L1 = "English")

eff_df_e_g = e_g[["phoneme:cats__"]] %>% 
  mutate(language = case_when(
    cats__ == "feel" ~ "English",
    cats__ == "fool" ~ "English",
    cats__ == "fought" ~ "English",
    cats__ == "fun" ~ "English",
    cats__ == "son" ~ "Spanish",
    cats__ == "su" ~ "Spanish",         
    cats__ == "fin" ~ "Spanish")) %>% 
  mutate(stim_language = "German") %>% 
  mutate(L1 = "English")


cond_df = rbind(eff_df, eff_df_g, eff_df_e, eff_df_e_g) %>% 
  write.csv(here("data", "tidy", "cond_df.csv"))

# Reorder levels to make English and Spanish words occur
# next to each other in the plots 
eff_df$cats__ <- factor(eff_df$cats__, 
                        levels = 
                          c("feel", 
                            "fool", 
                            "fought",
                            "fun",
                            "son",
                            "su",
                            "fin"))

eff_df_g$cats__ <- factor(eff_df_g$cats__, 
                          levels = 
                            c("feel", 
                              "fool", 
                              "fought",
                              "fun",
                              "son",
                              "su",
                              "fin"))

eff_df_e$cats__ <- factor(eff_df_e$cats__, 
                          levels = 
                            c("feel", 
                              "fool", 
                              "fought",
                              "fun",
                              "son",
                              "su",
                              "fin"))

eff_df_e_g$cats__ <- factor(eff_df_e_g$cats__, 
                            levels = 
                              c("feel", 
                                "fool", 
                                "fought",
                                "fun",
                                "son",
                                "su",
                                "fin"))


span_full = rbind(eff_df, eff_df_g) %>% 
  mutate(L1 = "Spanish L1 group")
eng_full = rbind(eff_df_e, eff_df_e_g) %>% 
  mutate(L1 = "English L1 group")


# Plot 
eff_df %>% 
  ggplot(aes(y = phoneme, x = estimate__, fill = cats__)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) + 
  scale_fill_discrete(name = "Choice") +
  xlim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue")) + facet_wrap(~language) +
  xlab("Probability") + 
  labs(title = "Probabilty of word choice by phoneme",
       subtitle = "Spanish L1 group")  + 
  ggsave(here("data", "plots", "new_sounds", "prob_per_word_spl1.png"),
         dpi = 1200) 



eff_df_e %>% 
  ggplot(aes(y = phoneme, x = estimate__, fill = cats__)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) + 
  scale_fill_discrete(name = "Choice") +
  xlim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue")) + facet_wrap(~language) +
  xlab("Probability") + 
  labs(title = "Probabilty of word choice by phoneme",
       subtitle = "English L1 group")  + 
  ggsave(here("data", "plots", "new_sounds", "prob_per_word_enl1.png"),
         dpi = 1200) 


# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")


# y plots 
rbind(eng_full, span_full) %>% 
  filter(phoneme == "y") %>% 
  ggplot(aes(y = phoneme, x = estimate__, fill = cats__)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) + 
  scale_fill_discrete(name = "Choice") +
  scale_fill_manual(values=cbPalette) +
  xlim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = .05, 
          linetype = 'solid',
          colour = "lightblue")) +
  theme(axis.text.x = element_text(color="black",
                                   size=8, angle=25)) +
  theme(axis.text.y = element_text(face="bold", color="black",
                                   size=14, angle=0)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text=element_text(size=8),
        legend.position = "bottom") +
  facet_grid(vars(stim_language), vars(L1)) +
  geom_vline(xintercept = .5, linetype = "dashed") + 
  theme(strip.text.x = element_text(size=8),
        strip.background = element_rect(colour="lightblue", fill="white")) +
  guides(fill = guide_legend(nrow = 1)) + theme(text=
                                                  element_text(
                                                    size=10,
                                                    family="Times New Roman")) +
  theme(legend.title=element_blank()) + 
  guides(colour = guide_legend(nrow = 1)) +
  geom_vline(xintercept = .17, linetype = "longdash", 
             color = "red", alpha = .5) +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1)) + 
  ggsave(here("sections", "figs", "y_full.png"))



# i plots
rbind(eng_full, span_full) %>% 
  filter(phoneme == "i") %>% 
  ggplot(aes(y = phoneme, x = estimate__, fill = cats__)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) + 
  scale_fill_discrete(name = "Choice") +
  scale_fill_manual(values=cbPalette) +
  xlim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = .05, 
          linetype = 'solid',
          colour = "lightblue")) +
  theme(axis.text.x = element_text(color="black",
                                   size=8, angle=25)) +
  theme(axis.text.y = element_text(face="bold", color="black",
                                   size=14, angle=0)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text=element_text(size=8),
        legend.position = "bottom") +
  facet_grid(vars(stim_language), vars(L1)) +
  geom_vline(xintercept = .5, linetype = "dashed") + 
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1)) +
  theme(strip.text.x = element_text(size=8),
        strip.background = element_rect(colour="lightblue", fill="white")) +
  guides(fill = guide_legend(nrow = 1)) + theme(text=
                                                  element_text(
                                                    size=10,
                                                    family="Times New Roman")) +
  theme(legend.title=element_blank()) + 
  guides(colour = guide_legend(nrow = 1)) +
  geom_vline(xintercept = .17, linetype = "longdash", 
             color = "red", alpha = .5) +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1)) + 
  ggsave(here("MDPI_template", "figs", "i_full.png"),
         dpi = 1200)



# o plots 
rbind(eng_full, span_full) %>% 
  filter(phoneme == "o") %>% 
  ggplot(aes(y = phoneme, x = estimate__, fill = cats__)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) + 
  scale_fill_discrete(name = "Choice") +
  scale_fill_manual(values=cbPalette) +
  xlim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = .05, 
          linetype = 'solid',
          colour = "lightblue")) +
  theme(axis.text.x = element_text(color="black",
                                   size=8, angle=25)) +
  theme(axis.text.y = element_text(face="bold", color="black",
                                   size=14, angle=0)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text=element_text(size=8),
        legend.position = "bottom") +
  facet_grid(vars(stim_language), vars(L1)) +
  geom_vline(xintercept = .5, linetype = "dashed") + 
  theme(strip.text.x = element_text(size=8),
        strip.background = element_rect(colour="lightblue", fill="white")) +
  guides(fill = guide_legend(nrow = 1)) + theme(text=
                                                  element_text(
                                                    size=10,
                                                    family="Times New Roman")) +
  theme(legend.title=element_blank()) + 
  guides(colour = guide_legend(nrow = 1)) +
  geom_vline(xintercept = .17, linetype = "longdash", 
             color = "red", alpha = .5) +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1)) + 
  ggsave(here("MDPI_template", "figs", "o_full.png"),
         dpi = 1200)


# schwa plot
rbind(eng_full, span_full) %>% 
  filter(phoneme == "schwa") %>% 
  ggplot(aes(y = phoneme, x = estimate__, fill = cats__)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) + 
  scale_fill_discrete(name = "Choice") +
  scale_fill_manual(values=cbPalette) +
  xlim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = .05, 
          linetype = 'solid',
          colour = "lightblue")) +
  theme(axis.text.x = element_text(color="black",
                                   size=8, angle=25)) +
  theme(axis.text.y = element_text(face="bold", color="black",
                                   size=14, angle=0)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text=element_text(size=8),
        legend.position = "bottom") +
  facet_grid(vars(stim_language), vars(L1)) +
  geom_vline(xintercept = .5, linetype = "dashed") + 
  theme(strip.text.x = element_text(size=8),
        strip.background = element_rect(colour="lightblue", fill="white")) +
  guides(fill = guide_legend(nrow = 1)) + theme(text=
                                                  element_text(
                                                    size=10,
                                                    family="Times New Roman")) +
  theme(legend.title=element_blank()) + 
  guides(colour = guide_legend(nrow = 1)) +
  geom_vline(xintercept = .17, linetype = "longdash", 
             color = "red", alpha = .5) +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1)) +
  ggsave(here("MDPI_template", "figs", "schwa_full.png"),
         dpi = 1200)



# Load 
mod_span = readRDS(here("data", "models", "ns_span.rds"))
mod_span_g = readRDS(here("data", "models", "ns_span_g.rds"))
mod_eng = readRDS(here("data", "models", "ns_eng.rds"))
mod_eng_g = readRDS(here("data", "models", "ns_eng_g.rds"))

ex = tab_model(mod_span)



