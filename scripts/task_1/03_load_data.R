source(here::here("scripts", "task_1", "00_libs.R"))

target_words = c("DAXY", "WILPY", "SPOOVY", "TROBY", "BRISPY", "LARPY")

## load data 
task_1 = read.csv(here("data", "tidy", "task_1_tidy.csv")) %>% 
  mutate(word_col = str_remove(word_col, ".1")) %>% 
  mutate(word_col = str_remove(word_col, ".1")) %>%
  mutate(word_col = str_remove(word_col, ".2")) %>% 
  mutate(word_col = str_remove(word_col, ".2")) %>% 
  mutate(word_col = str_remove(word_col, ".3")) %>% 
  mutate(word_col = str_remove(word_col, ".3")) %>% 
  mutate(word_col = str_remove(word_col, ".4")) %>% 
  mutate(word_col = str_remove(word_col, ".4")) %>% 
  filter(word_col %in% target_words)

em_adj_df = read.csv(here("data", "tidy", "em_adj_df_1.csv"))

em_adj_df_ratings = read.csv(here("data", "tidy", "em_adj_df_ratings_1.csv"))

mod_b_1 = read_rds(here("data", "models", "study_1_b.RDS"))

total_adjective_guesses = nrow(task_1 %>% filter(type == "Adjective?" & answers == 1))/nrow(task_1 %>% filter(type == "Adjective?"))

prop_emotional = nrow(task_1 %>% filter(type == "Emotion Adj?" & answers == 1))/nrow(task_1 %>% filter(type == "Adjective?" & answers == 1))

rep_df = em_adj_df %>%
  data_grid(animacy_condtion,frame_content_condition,) %>%
  add_fitted_draws(mod_b_1, dpar = TRUE, category = "answers",
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


# ord_mod = read_rds(here("data", "models", "study_2_b_ratings.RDS"))


# ws_mod = read_rds(here("data", "models", "ws_model.RDS"))

# ws_mod_3 = read_rds(here("data", "models", "ws_model-3.RDS"))


