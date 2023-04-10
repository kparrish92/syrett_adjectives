source(here::here("scripts", "task_2", "00_libs.R"))

em_adj_df = read.csv(here("data", "tidy", "em_adj_df.csv"))

em_adj_df_ratings = read.csv(here("data", "tidy", "em_adj_df_ratings.csv"))


### LOG MOD
brm(as.numeric(answers) ~ animacy_condtion*frame_content_condition, family = bernoulli(link = "logit"), data = em_adj_df, 
          file = here("data", "models", "study_2_b.RDS"))

### ORD MOD

brm(as.integer(Confidence) ~ animacy_condtion*frame_content_condition,
               data = em_adj_df_ratings,
               family = cumulative(),
               cores = 4,
               file = here("data", "models", "study_2_b_ratings.RDS"))

### within-subj mod

brm(as.numeric(answers) ~ word_col*animacy_condtion*frame_content_condition + 
            (1 | prolific_id), family = bernoulli(link = "logit"), data = em_adj_df, 
          file = here("data", "models", "ws_model-3.RDS"))
