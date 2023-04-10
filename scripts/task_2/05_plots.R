library(here)
library(tidyverse)
library(janitor)
library(bayestestR)
library(bayesplot)
library(tidybayes)
library(modelr)
library(brms)

source(here::here("scripts", "task_2", "03_load_data.R"))

# Add color blind palette - The palette with grey:
cbPalette <- c("#009E73", "#ff6242", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

rep_df = em_adj_df %>%
  data_grid(animacy_condtion,frame_content_condition) %>%
  add_fitted_draws(mod_b_2, dpar = TRUE, category = "answers",
                   re_formula = NA) 

a_df = rep_df %>% filter(animacy_condtion == "animate") 

round(mean(a_df$.value), digits = 2) # effect for animate
round(hdi(a_df$.value), digits = 2)[1] # hdi lo for animate
round(hdi(a_df$.value), digits = 2)[2] # hdi hi for animate


## Forest Plot
em_adj_df %>%
  data_grid(animacy_condtion,frame_content_condition) %>%
  add_fitted_draws(mod_b_2, dpar = TRUE, category = "answers",
                   re_formula = NA) %>%
  ggplot(aes(y = animacy_condtion, x = .value, fill = frame_content_condition)) +
  stat_halfeye(alpha = .5) +
  scale_size_continuous(guide = "none") +
  coord_cartesian(x = c(0,1), clip = "off") +
  theme_minimal() +
  xlab("Probability") + ylab("Condition") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Response")) +
  ggsave(here("docs", "plots", "task_2_bayesian.png"))

## Forest Plot - animacy
em_adj_df %>%
  data_grid(animacy_condtion,frame_content_condition) %>%
  add_fitted_draws(mod_b_2, dpar = TRUE, category = "answers",
                   re_formula = NA) %>%
  ggplot(aes(y = animacy_condtion, x = .value, fill = animacy_condtion)) +
  stat_halfeye(alpha = .5) +
  scale_size_continuous(guide = "none") +
  coord_cartesian(x = c(0,1), clip = "off") +
  theme_minimal() +
  xlab("Probability") + ylab("Condition") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Response"))

em_adj_df %>%
  data_grid(animacy_condtion,frame_content_condition) %>%
  add_fitted_draws(mod_b_2, dpar = TRUE, category = "answers",
                   re_formula = NA) %>%
  ggplot(aes(y = frame_content_condition, x = .value, fill = frame_content_condition)) +
  stat_halfeye(alpha = .5) +
  scale_size_continuous(guide = "none") +
  coord_cartesian(x = c(0,1), clip = "off") +
  theme_minimal() +
  xlab("Probability") + ylab("Condition") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Response"))

em_adj_df %>%
  data_grid(animacy_condtion,frame_content_condition) %>%
  add_fitted_draws(mod_b_2, dpar = TRUE, category = "answers",
                   re_formula = NA) %>%
  ggplot(aes(y = frame_content_condition, x = .value, fill = animacy_condtion)) +
  stat_halfeye(alpha = .5) +
  scale_size_continuous(guide = "none") +
  coord_cartesian(x = c(0,1), clip = "off") +
  theme_minimal() +
  xlab("Probability") + ylab("Condition") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Response")) +
  ggsave(here("docs", "plots", "task_2_bayesian_frame.png"))


## ES plots

data_grid_m = em_adj_df %>%
  data_grid(animacy_condtion,frame_content_condition) %>%
  add_fitted_draws(mod_b_2, dpar = TRUE, category = "answers",
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
  ggsave(here("docs", "plots", "animate_es.png"))
 
plot_logistic("inanimate") +
  ggsave(here("docs", "plots", "inanimate_es.png"))


describe_posterior(mod_b_2, rope_range = c(-0.18, 0.18)) %>% 
  as_tibble() %>% 
  select(-c("CI", "ROPE_CI", "ROPE_low", "ROPE_high")) %>% 
  mutate(Parameter = case_when(
    Parameter == "b_Intercept" ~ "Intercept",                                                              
    Parameter == "b_word_colDAXY" ~ "about DP",                                                            
    Parameter == "b_word_colLARPY" ~ "to VP",                                                          
    Parameter == "b_word_colSPOOVY" ~ "that S",                                                          
    Parameter == "b_word_colTROBY" ~ "about gerund",                                                           
    Parameter == "b_word_colWILPY" ~ "of DP because S",                                                          
    Parameter == "b_animacy_condtioninanimate" ~ "animate",                                              
    Parameter == "b_frame_content_conditionlexical" ~ "lexical",                                         
    Parameter == "b_word_colDAXY:animacy_condtioninanimate" ~ "about DP:animate",                                 
    Parameter == "b_word_colLARPY:animacy_condtioninanimate" ~ "to VP: animate",                                
    Parameter == "b_word_colSPOOVY:animacy_condtioninanimate" ~ "that S: animate",                               
    Parameter == "b_word_colTROBY:animacy_condtioninanimate" ~ "about gerund:animate",                                
    Parameter == "b_word_colWILPY:animacy_condtioninanimate" ~ "of DP because S: animate",                                
    Parameter == "b_word_colDAXY:frame_content_conditionlexical" ~ "about DP:lexical",                            
    Parameter == "b_word_colLARPY:frame_content_conditionlexical" ~ "to VP:lexical",                           
    Parameter == "b_word_colSPOOVY:frame_content_conditionlexical" ~ "that S:lexical",                          
    Parameter == "b_word_colTROBY:frame_content_conditionlexical" ~ "about gerund:lexical",                           
    Parameter == "b_word_colWILPY:frame_content_conditionlexical" ~ "of DP because S:lexical",                           
    Parameter == "b_animacy_condtioninanimate:frame_content_conditionlexical" ~ "animate:lexical",               
    Parameter == "b_word_colDAXY:animacy_condtioninanimate:frame_content_conditionlexical" ~ "about DP:animate:lexical",  
    Parameter == "b_word_colLARPY:animacy_condtioninanimate:frame_content_conditionlexical" ~ "to VP:animate:lexical", 
    Parameter == "b_word_colSPOOVY:animacy_condtioninanimate:frame_content_conditionlexical" ~ "that S:animate:lexical",
    Parameter == "b_word_colTROBY:animacy_condtioninanimate:frame_content_conditionlexical" ~ "about gerund:animate:lexical", 
    Parameter == "b_word_colWILPY:animacy_condtioninanimate:frame_content_conditionlexical" ~ "of DP because S:animate:lexical"
  )) %>% 
  mutate(across(-c("Parameter", "ESS"), specify_decimal, k = 2)) %>% 
  mutate(ESS = round(ESS)) %>% 
  mutate(HDI = glue::glue("[{CI_low}, {CI_high}]")) %>% 
  select(Parameter, Median, HDI, `% in ROPE` = ROPE_Percentage, MPE = pd, Rhat, ESS) %>% 
  write_csv(here("docs", "tables", "study_2_model.csv"))



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

