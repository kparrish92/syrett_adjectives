
fct = read.csv(here("data", "03_referent_selection.csv")) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  pivot_longer(cols = c(2:9), names_to = "condition",
               values_to = "is_correct")


library(readr)
fct <- read_csv("data/03_referent_selection.csv", 
                                   col_types = cols(X10 = col_skip())) %>% 
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(cols = c(`blickets (count N)`:`roaking (transitive)`), 
               names_to = "condition",
               values_to = "is_correct") %>% 
  rename(sona_id = `SONA ID`)

fct$`SONA ID`




### LOG MOD
mod_b_int = brm(as.numeric(is_correct) ~ 1 + (1 | sona_id), 
                family = binomial(link = "logit"), 
                data = fct, 
                file = here("data", "models", "fct_mod_int.RDS"))



### LOG MOD
mod_b_fct = brm(as.numeric(is_correct) ~ condition + (1 | sona_id), 
              family = binomial(link = "logit"), 
              data = fct, 
              file = here("data", "models", "fct_mod.RDS"))


mod_b_df = mod_b_int %>% 
  as.data.frame() %>%
  mutate(aggregate_prob = plogis(b_Intercept)) %>% 
  mutate(case = "Aggregate")

mod_b_df %>% 
  ggplot(aes(y = case, x = aggregate_prob)) + stat_halfeye(fill = "seagreen") + 
  theme_minimal() +
  ylab("") + xlab("Probability") +
  xlim(0,1) + geom_vline(xintercept = .5, linetype = "dashed", alpha = .5) +
  ggsave(here("docs", "plots", "aggregate_prob.png"))


round(mean(mod_b_df$aggregate_prob), digits = 2)
hdi(mod_b_df$aggregate_prob), digits = 2)
round(hdi(mod_b_df$aggregate_prob), digits = 2)[1]
round(hdi(mod_b_df$aggregate_prob), digits = 2)[2]

plogis(mod_b_df 

rep_df = fct %>%
  data_grid(condition) %>%
  add_fitted_draws(mod_b_fct, dpar = TRUE, category = "is_correct",
                   re_formula = NA) 

conditional_effects(mod_b_fct)


rep_df %>% 
  ggplot(aes(y = condition, x = .value)) + stat_halfeye(fill = "seagreen") + 
  theme_minimal() +
  ylab("") + xlab("Probability") +
  xlim(0,1) + geom_vline(xintercept = .5, linetype = "dashed", alpha = .5) +
  ggsave(here("docs", "plots", "ind_prob.png"))
  



