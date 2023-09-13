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


## Logistic regression 
null_mod = lme4::glmer(as.numeric(answers) ~ 1 + (1 | word_col) + (1 | prolific_id), 
                       family = binomial(link = "logit"), 
                       data = em_adj_df)


ac_mod = lme4::glmer(as.numeric(answers) ~ animacy_condtion + (1 | word_col) + (1 | prolific_id), 
                     family = binomial(link = "logit"), 
                     data = em_adj_df)


fc_mod = lme4::glmer(as.numeric(answers) ~ animacy_condtion + frame_content_condition + (1 | word_col) + (1 | prolific_id), 
                     family = binomial(link = "logit"), 
                     data = em_adj_df)

int_mod = lme4::glmer(as.numeric(answers) ~ animacy_condtion + frame_content_condition + animacy_condtion:frame_content_condition + 
                        (1 | word_col) + (1 | prolific_id), 
                      family = binomial(link = "logit"), 
                      data = em_adj_df)


res_df = anova(null_mod, ac_mod, fc_mod, int_mod) %>% 
  write.csv(here("data", "tidy", "task_2_nmc.csv"))


null_mod = lme4::glmer(as.numeric(answers) ~ 1 + (1 | word_col) + (1 | prolific_id), 
                       family = binomial(link = "logit"), 
                       data = em_adj_df)


ac_mod = lme4::glmer(as.numeric(answers) ~ animacy_condtion + (1 | word_col) + (1 | prolific_id), 
                     family = binomial(link = "logit"), 
                     data = em_adj_df)


fc_mod = lme4::glmer(as.numeric(answers) ~ animacy_condtion + frame_content_condition + (1 | word_col) + (1 | prolific_id), 
                     family = binomial(link = "logit"), 
                     data = em_adj_df)

int_mod = lme4::glmer(as.numeric(answers) ~ animacy_condtion + frame_content_condition + animacy_condtion:frame_content_condition + 
                        (1 | word_col) + (1 | prolific_id), 
                      family = binomial(link = "logit"), 
                      data = em_adj_df)


res_df = anova(null_mod, ac_mod, fc_mod, int_mod) %>% 
  write.csv(here("data", "tidy", "task_3_nmc.csv"))

library(brglm2)
library(multcomp)


#(X(df) = Chisq; p < value)


#(X(df) = Chisq; p < value)

