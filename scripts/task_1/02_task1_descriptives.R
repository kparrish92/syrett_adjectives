library(lme4)


target_words = c("DAXY", "WILPY", "SPOOVY", "TROBY", "BRISPY", "LARPY")

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
  
#a) proportion of emotion adjective guesses (out of ALL guesses - sum adj and non-adj guesses)
# -compare guesses for target trials across conditions (look for main effects and interactions)

### State the total adjective guesses out of ALL trials

total_adjective_guesses = nrow(task_1 %>% filter(type == "Adjective?" & answers == 1))/nrow(task_1 %>% filter(type == "Adjective?"))

### State the total guesses that were emotional adjectives out of ALL trials 

total_emotion_adjective_guesses = nrow(task_1 %>% filter(type == "Emotion Adj?" & answers == 1))/nrow(task_1 %>% filter(type == "Emotion Adj?"))

# b) proportion of emotion adjective guesses (out of all ADJ guesses)
# -compare guesses for target trials across conditions (look for main effects and interactions)
# -compare guesses between target trials within each condition (Chi-sq analysis?)

prop_emotional = nrow(task_1 %>% filter(type == "Emotion Adj?" & answers == 1))/nrow(task_1 %>% filter(type == "Adjective?" & answers == 1))

# Do we want to examine the probability of selecting an emotional adjective as a 
# function of animacy, frame and their interaction?

## removed 1224 - 1208 nas  
em_adj_df = task_1 %>% filter(type == "Emotion Adj?") %>% 
  filter(answers == 1 | answers == 0) %>% 
  write.csv(here("data", "tidy", "em_adj_df_1.csv"))


## ordinal model for ratings

em_adj_df_ratings = task_1 %>% 
  select(-X) %>% 
  filter(type == "Confidence 1" | type == "Confidence 2" |
           type == "Confidence 3" | type == "Confidence 4" |
           type == "Emotion Adj?") %>% 
  filter(answers == 1 | answers == 2 | answers == 3 | answers == 4) %>% 
  pivot_wider(names_from = "type", values_from = "answers") %>% 
  write.csv(here("data", "tidy", "em_adj_df_ratings_1.csv"))





  