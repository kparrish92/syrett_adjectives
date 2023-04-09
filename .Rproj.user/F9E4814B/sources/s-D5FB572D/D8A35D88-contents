
describe_posterior(eng_mod, rope_range = c(-0.1, 0.1)) %>% 
  as_tibble() %>% 
  select(-c("CI", "ROPE_CI", "ROPE_low", "ROPE_high")) %>% 
  mutate(Parameter = case_when(
    Parameter == "b_Intercept" ~ "Intercept", 
    Parameter == "b_phonemeo" ~ "/o/", 
    Parameter == "b_phonemeschwa_a" ~ "schwa, /a/", 
    Parameter == "b_phonemeu" ~ "/u/, /y/", 
    Parameter == "b_modeproduction" ~ "Production", 
    Parameter == "b_phonemeo:modeproduction" ~ "/o/: Production", 
    Parameter == "b_phonemeschwa_a:modeproduction" ~ "schwa, /a/: Production", 
    Parameter == "b_phonemeu:modeproduction" ~ "/u/, /y/: Production"
  )) %>% 
  mutate(across(-c("Parameter", "ESS"), specify_decimal, k = 2)) %>% 
  mutate(ESS = round(ESS)) %>% 
  mutate(HDI = glue::glue("[{CI_low}, {CI_high}]")) %>% 
  select(Parameter, Median, HDI, `% in ROPE` = ROPE_Percentage, MPE = pd, Rhat, ESS) %>% 
  write_csv(here("data", "both", "tables", "eng_model_table.csv"))



describe_posterior(span_mod, rope_range = c(-0.1, 0.1)) %>% 
  as_tibble() %>% 
  select(-c("CI", "ROPE_CI", "ROPE_low", "ROPE_high")) %>% 
  mutate(Parameter = case_when(
    Parameter == "b_Intercept" ~ "Intercept", 
    Parameter == "b_phonemeo" ~ "/o/", 
    Parameter == "b_phonemeschwa_a" ~ "schwa, /a/", 
    Parameter == "b_phonemeu" ~ "/u/, /y/", 
    Parameter == "b_modeproduction" ~ "Production", 
    Parameter == "b_phonemeo:modeproduction" ~ "/o/: Production", 
    Parameter == "b_phonemeschwa_a:modeproduction" ~ "schwa, /a/: Production", 
    Parameter == "b_phonemeu:modeproduction" ~ "/u/, /y/: Production"
  )) %>% 
  mutate(across(-c("Parameter", "ESS"), specify_decimal, k = 2)) %>% 
  mutate(ESS = round(ESS)) %>% 
  mutate(HDI = glue::glue("[{CI_low}, {CI_high}]")) %>% 
  select(Parameter, Median, HDI, `% in ROPE` = ROPE_Percentage, MPE = pd, Rhat, ESS) %>% 
  write_csv(here("data", "both", "tables", "span_model_table.csv"))


