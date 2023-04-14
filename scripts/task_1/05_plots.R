library(here)
library(tidyverse)
library(janitor)
library(bayestestR)
library(bayesplot)
library(tidybayes)
library(modelr)
library(brms)

source(here::here("scripts", "task_1", "03_load_data.R"))

# Add color blind palette - The palette with grey:
cbPalette <- c("#009E73", "#ff6242", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

rep_df = em_adj_df %>%
  data_grid(animacy_condtion,frame_content_condition) %>%
  add_fitted_draws(mod_b_1, dpar = TRUE, category = "answers",
                   re_formula = NA) 

a_df = rep_df %>% filter(animacy_condtion == "animate") 

round(mean(a_df$.value), digits = 2) # effect for animate
round(hdi(a_df$.value), digits = 2)[1] # hdi lo for animate
round(hdi(a_df$.value), digits = 2)[2] # hdi hi for animate


## Forest Plot
em_adj_df %>%
  data_grid(animacy_condtion,frame_content_condition) %>%
  add_fitted_draws(mod_b_1, dpar = TRUE, category = "answers",
                   re_formula = NA) %>%
  ggplot(aes(y = animacy_condtion, x = .value, fill = frame_content_condition)) +
  stat_halfeye(alpha = .5) +
  scale_size_continuous(guide = "none") +
  coord_cartesian(x = c(0,1), clip = "off") +
  theme_minimal() +
  xlab("Probability") + ylab("Condition") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Response")) +
  ggsave(here("docs", "plots", "task_1_bayesian.png"))

em_adj_df %>%
  data_grid(animacy_condtion,frame_content_condition) %>%
  add_fitted_draws(mod_b_1, dpar = TRUE, category = "answers",
                   re_formula = NA) %>%
  ggplot(aes(y = frame_content_condition, x = .value, fill = animacy_condtion)) +
  stat_halfeye(alpha = .5) +
  scale_size_continuous(guide = "none") +
  coord_cartesian(x = c(0,1), clip = "off") +
  theme_minimal() +
  xlab("Probability") + ylab("Condition") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Response")) +
  ggsave(here("docs", "plots", "task_1_bayesian_frame.png"))


## ES plots

data_grid_m = em_adj_df %>%
  data_grid(animacy_condtion,frame_content_condition) %>%
  add_fitted_draws(mod_b_1, dpar = TRUE, category = "answers",
                   re_formula = NA) 

## Create a function to not have to copy this kind of complex pipeline
plot_logistic = function(effect)
{
  
  ab = data_grid_m %>% 
    filter(animacy_condtion == effect & frame_content_condition == "bleached") %>% 
    rename("ab_est" = ".value")
  
  al = data_grid_m %>% 
    filter(animacy_condtion == effect & frame_content_condition == "lexical") %>% 
    rename("al_est" = ".value")
  
  c = left_join(ab, al, by = c("animacy_condtion", ".draw")) %>% 
    mutate(es_l_b = al_est - ab_est) %>% 
    mutate(effect = effect)
  
  pct_pos_df = c %>% 
    mutate(is_positve = case_when(
      es_l_b > 0 ~ 1,
      es_l_b < 0 ~ 0
    )) %>% 
    group_by(effect) %>% 
    summarise(qty_positive = sum(is_positve)/4000)
  
  es_l_b_hdi = data.frame(hdi_low = round(hdi(c$es_l_b)[,1], digits = 3),
                          hdi_hi = round(hdi(c$es_l_b)[,2], digits = 3),
                          es_l_b = round(mean(c$es_l_b), digits = 3)) %>% 
    mutate(effect = effect)  %>% 
    left_join(pct_pos_df, by = c("effect"))
  
  
  plot = c %>% 
    ggplot(aes(x = es_l_b, y = effect, fill = after_stat(x < 0))) + 
    stat_halfeye() +
    geom_text(data = mutate_if(es_l_b_hdi, is.numeric, round, 2),
              aes(label = paste0(es_l_b, " [", `hdi_low`, " - ", `hdi_hi`, "]")), 
              hjust = .5, vjust = 2, size = 2.5, family = "sans") +
    geom_text(data = mutate_if(es_l_b_hdi, is.numeric, round, 2),
              aes(label = paste0(qty_positive)), 
              hjust = .5, vjust = -2.5, size = 2.5,
              family = "sans") +
    geom_vline(xintercept = 0, linetype = "dashed", 
               alpha = .4) +
    coord_cartesian(x = c(-1,1), clip = "off") +
    theme_minimal() +
    scale_fill_manual(values=cbPalette) + theme(legend.position = "none") +
    theme(axis.line = element_line(colour = "black", 
                                   size = .1, linetype = "solid")) +
    xlab("Difference in Probability") + 
    ylab("Effect") + ggtitle(str_wrap(paste0("The difference in the probability in the ", effect, " condition going from bleached to lexical frames"), width = 35))
  return(plot)
}


# Run function and save plots
plot_logistic("animate") +
  ggsave(here("docs", "plots", "animate_es_1.png"))

plot_logistic("inanimate") +
  ggsave(here("docs", "plots", "inanimate_es_1.png"))


describe_posterior(mod_b_1, rope_range = c(-0.18, 0.18)) %>% 
  as_tibble() %>% 
  select(-c("CI", "ROPE_CI", "ROPE_low", "ROPE_high")) %>% 
  mutate(Parameter = case_when(
    Parameter == "b_Intercept" ~ "Intercept",                                                              
    Parameter == "b_animacy_condtioninanimate" ~ "animate",                                                            
    Parameter == "b_frame_content_conditionlexical" ~ "lexical",                                                          
    Parameter == "b_animacy_condtioninanimate:frame_content_conditionlexical" ~ "animate:lexical"                                                      
  )) %>% 
  mutate(across(-c("Parameter", "ESS"), specify_decimal, k = 2)) %>% 
  mutate(ESS = round(ESS)) %>% 
  mutate(HDI = glue::glue("[{CI_low}, {CI_high}]")) %>% 
  select(Parameter, Median, HDI, `% in ROPE` = ROPE_Percentage, MPE = pd, Rhat, ESS) %>% 
  write_csv(here("docs", "tables", "study_1_model.csv"))



rating_draws = em_adj_df_ratings %>% 
  data_grid(animacy_condtion,frame_content_condition) %>%
  add_fitted_draws(ord_mod, dpar = TRUE, category = "Confidence",
                   re_formula = NA) 


rating_draws %>%
  ggplot(aes(y = frame_content_condition, x = .value, fill = Confidence)) +
  stat_halfeye(alpha = .5) +
  scale_size_continuous(guide = "none") +
  coord_cartesian(x = c(0,1), clip = "off") +
  theme_minimal() +
  facet_wrap(~animacy_condtion, ncol = 1) +
  xlab("Probability") + ylab("Condition") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Response")) +
  ggsave(here("docs", "plots", "ratings_mod.png"))

em_adj_df_ratings %>%
  ggplot(aes(Confidence, fill = as.factor(Confidence))) +
  geom_histogram(stat = "count", color = "black") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_grid(animacy_condtion~frame_content_condition) +
  guides(fill=guide_legend("Response")) +
  ggsave(here("docs", "plots", "ratings_desc.png"))



describe_posterior(ord_mod, rope_range = c(-0.18, 0.18)) %>% 
  as_tibble() %>% 
  select(-c("CI", "ROPE_CI", "ROPE_low", "ROPE_high")) %>% 
  mutate(Parameter = case_when(
    Parameter == "b_Intercept[1]" ~ "Intercept[1]", 
    Parameter == "b_Intercept[2]" ~ "Intercept[2]", 
    Parameter == "b_Intercept[3]" ~ "Intercept[3]", 
    Parameter == "b_animacy_condtioninanimate" ~ "animate", 
    Parameter == "b_frame_content_conditionlexical" ~ "lexical", 
    Parameter == "b_animacy_condtioninanimate:frame_content_conditionlexical" ~ 
      "animate:lexical"
  )) %>% 
  mutate(across(-c("Parameter", "ESS"), specify_decimal, k = 2)) %>% 
  mutate(ESS = round(ESS)) %>% 
  mutate(HDI = glue::glue("[{CI_low}, {CI_high}]")) %>% 
  select(Parameter, Median, HDI, `% in ROPE` = ROPE_Percentage, MPE = pd, Rhat, ESS) %>% 
  write_csv(here("docs", "tables", "study_2_model_ord.csv"))


ws_mod_3 = read_rds(here("data", "models", "ws_model-3.RDS"))

ws_draws = em_adj_df_ratings %>% 
  data_grid(word_col ,animacy_condtion,frame_content_condition) %>%
  add_fitted_draws(ws_mod_3, dpar = TRUE, category = "answers",
                   re_formula = NA)  %>% 
  mutate(condition = paste(animacy_condtion,frame_content_condition)) %>% 
  mutate(frame_type = case_when(
    word_col == "DAXY" ~ "about DP",  
    word_col == "WILPY"  ~ "of DP because S",  
    word_col == "PILKY" ~ "for DP",
    word_col == "SPOOVY" ~ "that S",  
    word_col == "TROBY" ~ "about gerund",
    word_col == "BRISPY" ~ "at DP because S",
    word_col == "LARPY" ~"to VP"
  ))




ws_draws %>% 
  # filter(frame_type == "to VP") %>% 
  ggplot(aes(y = frame_type, x = .value, point_color = condition)) +
  stat_halfeye(alpha = .5) +
  scale_fill_manual(values = c("#1338BE", "#63c5da", "red", "#b90e0a")) +
  scale_size_continuous(guide = "none") +
  coord_cartesian(x = c(0,1), clip = "off") +
  theme_minimal() +
  #  facet_grid(~word_col) +
  xlab("Probability") + ylab("Condition") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Response"))

ws_draws %>% 
  #  filter(frame_type == "to VP") %>% 
  ggplot(aes(y = frame_type, x = .value, fill = condition,
             color = condition)) +
  #  stat_slab(alpha = .3) +
  scale_fill_manual(values = c("#1338BE", "#63c5da", "red", "#b90e0a")) +
  scale_color_manual(values = c("#1338BE", "#63c5da", "red", "#b90e0a")) +
  stat_pointinterval(position = position_dodge(width = 0, preserve = "single")) +
  theme_minimal() +
  theme(legend.position="bottom") +
  xlab("Probability") + ylab("Condition") + 
  guides(fill = guide_legend(nrow = 2))
ggsave(here("docs", "plots", "ws_plot.png"))



vp_p = ws_draws %>% 
  filter(frame_type == "to VP") %>% 
  ggplot(aes(y = frame_type, x = .value, fill = condition)) +
  stat_halfeye(alpha = .5) +
  scale_fill_manual(values = c("#1338BE", "#63c5da", "red", "#b90e0a")) +
  scale_size_continuous(guide = "none") +
  coord_cartesian(x = c(0,1), clip = "off") +
  theme_minimal() +
  #  facet_grid(~word_col) +
  xlab("Probability") + ylab("Condition") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Response")) +
  ylab("") + (xlab(""))

that_s = ws_draws %>% 
  filter(frame_type == "that S") %>% 
  ggplot(aes(y = frame_type, x = .value, fill = condition)) +
  stat_halfeye(alpha = .5) +
  scale_fill_manual(values = c("#1338BE", "#63c5da", "red", "#b90e0a")) +
  scale_size_continuous(guide = "none") +
  coord_cartesian(x = c(0,1), clip = "off") +
  theme_minimal() +
  #  facet_grid(~word_col) +
  xlab("Probability") + ylab("Condition") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Response"))

ggarrange(vp_p, vp_p, vp_p, vp_p,
          vp_p, vp_p, vp_p, vp_p, ncol = 1, common.legend = TRUE)


rating_df_1 = read.csv(here("data", "tidy", "task_1_rating.csv")) %>% 
  filter(Confidence.1 == 1 | Confidence.1 == 2 | Confidence.1 == 3 | 
           Confidence.1 == 4) %>% 
  filter(Confidence.2 == 1 | Confidence.2 == 2 | Confidence.2 == 3 | 
           Confidence.2 == 4) %>% 
  filter(Confidence.3 == 1 | Confidence.3 == 2 | Confidence.3 == 3 | 
           Confidence.3 == 4) %>% 
  filter(Confidence.4 == 1 | Confidence.4 == 2 | Confidence.4 == 3 | 
           Confidence.4 == 4) %>% 
  mutate(condition = paste0(frame, "_", animacy))
  

## 182 down to 137 
rating_df_all_1 = rating_df_1 %>% 
  filter(adjective_1 == 1 | adjective_1 == 0 & 
           adjective_2 == 1 | adjective_2 == 0 & 
           adjective_3 == 1 | adjective_3 == 0 &
           adjective_4 == 1 | adjective_4 == 0) %>% 
  filter(emotion_adj_1 != "n/a") %>% 
  filter(emotion_adj_2 != "n/a") %>% 
  filter(emotion_adj_3 != "n/a") %>% 
  filter(emotion_adj_4 != "n/a") 


unique(rating_df_all_1$emotion_adj_3)

rating_df_adjective_1 = rating_df_1 %>% 
  filter(adjective_1 == 1 & adjective_2 == 1 & adjective_3 == 1 & 
           adjective_4 == 1) 
  

unique(rating_df_adjective_1$emotion_adj_4)


d_1 = rating_df_all_1 %>% 
  group_by(condition) %>% 
  summarize(prop_em = sum(as.numeric(emotion_adj_1))/n()) %>% 
  mutate(guess_no = 1)

d_2 = rating_df_all_1 %>% 
  group_by(condition) %>% 
  summarize(prop_em = sum(as.numeric(emotion_adj_2))/n()) %>% 
  mutate(guess_no = 2)

d_3 = rating_df_all_1 %>% 
  group_by(condition) %>% 
  summarize(prop_em = sum(as.numeric(emotion_adj_3))/n()) %>% 
  mutate(guess_no = 3)

d_4 = rating_df_all_1 %>% 
  group_by(condition) %>% 
  summarize(prop_em = sum(as.numeric(emotion_adj_4))/n()) %>% 
  mutate(guess_no = 4)

  
rating_df_1$Confidence.1 = as.numeric(rating_df_1$Confidence.1)

d_1_a = rating_df_adjective_1 %>% 
  group_by(condition) %>% 
  summarize(prop_em = sum(as.numeric(emotion_adj_1))/n()) %>% 
  mutate(guess_no = 1)

d_2_a = rating_df_adjective_1 %>% 
  group_by(condition) %>% 
  summarize(prop_em = sum(as.numeric(emotion_adj_2))/n()) %>% 
  mutate(guess_no = 2)

d_3_a = rating_df_adjective_1 %>% 
  group_by(condition) %>% 
  summarize(prop_em = sum(as.numeric(emotion_adj_3))/n()) %>% 
  mutate(guess_no = 3)

d_4_a = rating_df_adjective_1 %>% 
  group_by(condition) %>% 
  summarize(prop_em = sum(as.numeric(emotion_adj_4))/n()) %>% 
  mutate(guess_no = 4)

rbind(d_1, d_2, d_3, d_4) %>% 
  ggplot(aes(x = guess_no, 
             y = prop_em, 
             color = condition, group = condition)) + 
  geom_point() + geom_line() +
  theme_minimal() +
  ggsave(here("docs", "plots", "task-1-rating-guess-1.png"))

rbind(d_1_a, d_2_a, d_3_a, d_4_a) %>% 
  ggplot(aes(x = guess_no, 
             y = prop_em, 
             color = condition, group = condition)) + 
  geom_point() + geom_line() +
  theme_minimal() +
  ggsave(here("docs", "plots", "task-1-rating-guess-1-adj.png"))


rbind(d_1, d_2, d_3, d_4) %>% 
  ggplot(aes(y = prop_em, x = condition, fill = as.factor(guess_no))) + 
  geom_col(color = "black", position = "dodge") 

d_1 %>% 
  ggplot(aes(y = prop_em, x = condition)) + 
  geom_col(color = "black", position = "dodge", fill = "seagreen") + ylim(0,1) +
  theme_minimal() + ylab("Proportion Emotion Adjectives") +
  ggsave(here("docs", "plots", "prop_em.png"))

d_1_a %>% 
  ggplot(aes(y = prop_em, x = condition)) + 
  geom_col(color = "black", position = "dodge", fill = "seagreen") + ylim(0,1) +
  theme_minimal() + ylab("Proportion Emotion Adjectives") +
  ggsave(here("docs", "plots", "prop_em_adj.png"))

rating_df_1 %>% 
  ggplot(aes(x = Confidence.1, fill = as.factor(Confidence.1))) + 
 # scale_fill_manual(values = c("#1338BE", "#63c5da", "red", "#b90e0a")) +
  geom_bar(color = "black", position = "dodge") + 
  facet_wrap(~condition, nrow = 2) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ylab("Count") + xlab("Confidence Rating") +
  ggsave(here("docs", "plots", "task-1-rating-1.png"))

rating_df_adjective_1 %>% 
  ggplot(aes(x = Confidence.1, fill = Confidence.1)) + 
  # scale_fill_manual(values = c("#1338BE", "#63c5da", "red", "#b90e0a")) +
  geom_bar(color = "black", position = "dodge") + 
  facet_wrap(~condition, nrow = 2) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ylab("Count") + xlab("Confidence Rating") +
  ggsave(here("docs", "plots", "task-1-rating-1-adj.png"))



# compare final confidence rating for target trials across conditions

rating_df_adjective_1 %>% 
  ggplot(aes(x = Confidence.4, fill = Confidence.4)) + 
  # scale_fill_manual(values = c("#1338BE", "#63c5da", "red", "#b90e0a")) +
  geom_bar(color = "black", position = "dodge") + 
  facet_wrap(~condition, nrow = 2) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ylab("Count") + xlab("Confidence Rating") + 
  ggsave(here("docs", "plots", "confidence-4-1.png"))



c_1 = rating_df_all_1 %>% 
  group_by(condition) %>% 
  summarize(mean_c = mean(as.numeric(Confidence.1)),
            sd_c = sd(as.numeric(Confidence.1))) %>% 
  mutate(trial = 1)
    

c_4 = rating_df_all_1 %>% 
  group_by(condition) %>% 
  summarize(mean_c = mean(as.numeric(Confidence.4)),
            sd_c = sd(as.numeric(Confidence.4))) %>% 
  mutate(trial = 4)


rbind(c_1, c_4) %>% 
  ggplot(aes(x = trial, y = mean_c, color = condition, group = 
               condition, ymin = mean_c-sd_c, 
             ymax = mean_c+sd_c)) + geom_pointrange(position = position_dodge(width = .2)) + 
  geom_line() + theme_minimal() + ylab("Mean Confidence Rating") +
  ggsave(here("docs", "plots", "confidence-comp.png"))


