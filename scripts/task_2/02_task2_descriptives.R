library(lme4)


target_words = c("DAXY", "WILPY", "SPOOVY", "TROBY", "BRISPY", "LARPY")

task_2 = read.csv(here("data", "tidy", "task_2_tidy.csv")) %>% 
  filter(word_col %in% target_words)
  
#a) proportion of emotion adjective guesses (out of ALL guesses - sum adj and non-adj guesses)
# -compare guesses for target trials across conditions (look for main effects and interactions)

### State the total adjective guesses out of ALL trials

total_adjective_guesses = nrow(task_2 %>% filter(type == "Adjective?" & answers == 1))/nrow(task_2 %>% filter(type == "Adjective?"))

### State the total guesses that were emotional adjectives out of ALL trials 

total_emotion_adjective_guesses = nrow(task_2 %>% filter(type == "Emotion Adj?-Conserv" & answers == 1))/nrow(task_2 %>% filter(type == "Emotion Adj?-Conserv"))

# b) proportion of emotion adjective guesses (out of all ADJ guesses)
# -compare guesses for target trials across conditions (look for main effects and interactions)
# -compare guesses between target trials within each condition (Chi-sq analysis?)

prop_emotional = nrow(task_2 %>% filter(type == "Emotion Adj?-Conserv" & answers == 1))/nrow(task_2 %>% filter(type == "Adjective?" & answers == 1))

# Do we want to examine the probability of selecting an emotional adjective as a 
# function of animacy, frame and their interaction?

## removed 1224 - 1208 nas  
em_adj_df=task_2 %>% filter(type == "Emotion Adj?-Conserv") %>% 
  filter(answers == 1 | answers == 0) %>% 
  write.csv(here("data", "tidy", "em_adj_df.csv"))


## ordinal model for ratings


em_adj_df_ratings = task_2 %>% 
  select(-X) %>% 
  filter(type == "Confidence" | answers == 1 & type == "Emotion Adj?-Conserv") %>% 
  pivot_wider(names_from = "type", values_from = "answers") %>% 
  filter(`Emotion Adj?-Conserv` == 1) %>% 
  filter(Confidence == 1 | Confidence == 2 | Confidence == 3 | Confidence == 4) %>%
  write.csv(here("data", "tidy", "em_adj_df_ratings.csv"))







  