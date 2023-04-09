library(here)
library(tidyverse)

mf_guesses = read.csv(here("data", "04_multiframe_guesses.csv"))

sf_version = read.csv(here("data", "02_singleframe_version.csv"))

## Trying to make loop 

#### prol id 

df_tidy_whole = tibble()
  
for (i in 3:155) {
  
df_tidy = data.frame(prolific_id = rep(sf_version$X[i], times = ncol(sf_version)-3),
           animacy_condtion = rep(sf_version$X.1[i], times = ncol(sf_version)-3),
           frame_content_condition = rep(sf_version$X.2[i], times = ncol(sf_version)-3))

word_col = sf_version[1, 4:43] %>% 
as_tibble() %>%
  unlist(., use.names=FALSE)

type = sf_version[2, 4:43] %>% 
  as_tibble() %>%
  unlist(., use.names=FALSE)

answers = sf_version[i, 4:43] %>% 
  as_tibble() %>%
  unlist(., use.names=FALSE)

df_tidy$type = type
df_tidy$word_col = word_col
df_tidy$answers = answers

df_tidy_whole = rbind(df_tidy, df_tidy_whole)

}

df_tidy_whole %>% 
  write.csv(here("data", "tidy", "task_2_tidy.csv"))

control_words = c("blicket", "pilky", "gormy", "roak")
target_words = c("daxy", "wilpy", "spoovy", "troby", "brispy", "larpy")

#Control (NON-adj, N): blicket – Frame: count list (1, 2, 3 Ns) 
# Target (emotion adj):  – Frame: about DP  
# Target (emotion adj):  – Frame: of DP because S  
# Control (NON-emotion adj): pilky – Frame: Gerundive subject ... for DP
# Target (emotion adj):  – Frame: that S  
# Target (emotion adj):  – Frame: about gerund  
# Control (NON-emotion adj):  – Frame: Expletive 'it' subject ... to VP  
# Target (emotion adj):  – Frame: at DP because S
# Target (emotion adj):  – Frame: to VP
#0 Control (NON-adj, V): roak – Frame:  