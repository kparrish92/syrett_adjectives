source(here::here("scripts", "task_1", "00_libs.R"))

em_adj_df = read.csv(here("data", "tidy", "em_adj_df_1.csv"))

em_adj_df_ratings = read.csv(here("data", "tidy", "em_adj_df_ratings_1.csv"))


### LOG MOD
mod_b_1 = brm(as.numeric(answers) ~ animacy_condtion*frame_content_condition + (1 | word_col) + (1 | prolific_id), 
              family = binomial(link = "logit"), 
              data = em_adj_df, 
    file = here("data", "models", "study_1_b.RDS"))


### ORD MOD


#brm(as.integer(Confidence) ~ animacy_condtion*frame_content_condition,
#    data = em_adj_df_ratings,
#    family = cumulative(),
#    cores = 4,
#    file = here("data", "models", "study_2_b_ratings.RDS"))


### FREQ MOD 


### LOG MOD



full_mod = lme4::glmer(as.numeric(answers) ~ animacy_condtion*frame_content_condition + (1 | word_col) + (1 | prolific_id), 
                       family = binomial(link = "logit"), 
                       data = em_adj_df)

full_mod = lme4::glmer(as.numeric(answers) ~ animacy_condtion*frame_content_condition + (1 | word_col) + (1 | prolific_id), 
              family = binomial(link = "logit"), 
              data = em_adj_df)