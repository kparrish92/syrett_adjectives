source(here::here("scripts", "task_2", "00_libs.R"))


target_words = c("DAXY", "WILPY", "SPOOVY", "TROBY", "BRIPSY", "LARPY")

## load data 
task_2 = read.csv(here("data", "tidy", "task_2_tidy.csv")) %>% 
  filter(word_col %in% target_words)

em_adj_df = read.csv(here("data", "tidy", "em_adj_df.csv"))

em_adj_df_ratings = read.csv(here("data", "tidy", "em_adj_df_ratings.csv"))


target_words = c("DAXY", "WILPY", "SPOOVY", "TROBY", "BRIPSY", "LARPY")

task_2 = read.csv(here("data", "tidy", "task_2_tidy.csv")) %>% 
  filter(word_col %in% target_words)

em_adj_df = read.csv(here("data", "tidy", "em_adj_df.csv"))

mod_b_2 = read_rds(here("data", "models", "ws_model-3.RDS"))

total_adjective_guesses = nrow(task_2 %>% filter(type == "Adjective?" & answers == 1))/nrow(task_2 %>% filter(type == "Adjective?"))

prop_emotional = nrow(task_2 %>% filter(type == "Emotion Adj?-Conserv" & answers == 1))/nrow(task_2 %>% filter(type == "Adjective?" & answers == 1))

rep_df = em_adj_df %>%
  data_grid(animacy_condtion,frame_content_condition, word_col) %>%
  add_fitted_draws(mod_b_2, dpar = TRUE, category = "answers",
                   re_formula = NA) 

# animacy 
a_df = rep_df %>% filter(animacy_condtion == "animate") 
ia_df = rep_df %>% filter(animacy_condtion == "inanimate") 

# frame

b_df = rep_df %>% filter(frame_content_condition == "bleached") 
l_df = rep_df %>% filter(frame_content_condition == "lexical") 

# animacy-frame interaction

## animate bleached 
ab_df = rep_df %>% filter(animacy_condtion == "animate" & frame_content_condition == "bleached")
## animate lexical
al_df = rep_df %>% filter(animacy_condtion == "animate" & frame_content_condition == "lexical")

## inanimate bleached 
ib_df = rep_df %>% filter(animacy_condtion == "inanimate" & frame_content_condition == "bleached")
## inanimate lexical
il_df = rep_df %>% filter(animacy_condtion == "inanimate" & frame_content_condition == "lexical")


ord_mod = read_rds(here("data", "models", "study_2_b_ratings.RDS"))


ws_mod = read_rds(here("data", "models", "ws_model.RDS"))

ws_mod_3 = read_rds(here("data", "models", "ws_model-3.RDS"))


