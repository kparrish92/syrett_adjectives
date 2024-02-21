
library(brglm2)
library(multcomp)

library(coda)
library(insight)
library(report)

# mod_b_2.rds, logistic regression 
# Task 2 is the ordinal regression 

ws_mod_3 = read_rds(here("data", "models", "ws_model-3.RDS")) # 3.2.2
ord_mod = read_rds(here("data", "models", "study_2_b_ratings.RDS")) # 3.4.4

report(ws_mod_3)

report(ws_mod_3, verbose = FALSE)

ws_mod_3
### Reporting convergence 

mod_3_3_2 = as.mcmc(ws_mod_3)
gelman.diag(mod_3_3_2)

mod_3_4_4 = as.mcmc(ord_mod)

gelman.diag(as.mcmc.list(mod_3_4_4))


as.mcmc.list(mod_3_4_4)



re = ranef(ws_mod_3) %>% 
  as.data.frame()


summary(ws_mod_3)

sqrt(var(re$prolific_id.Estimate.Intercept))

hist(re$prolific_id.Estimate.Intercept)

# Was there bias? 
b_df = em_adj_df %>% 
  group_by(prolific_id) %>% 
  summarize(bias = sum(answers)/n())

hist(b_df$bias)

em_adj_df %>% 
  group_by(prolific_id) %>% 
  summarize(n = n())

em_adj_df %>% 
  group_by(answers) %>% 
  summarize(n = n())





