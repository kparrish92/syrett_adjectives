
# compare 1st guess for target trials across conditions 

em_adj_df_ratings

task_1 %>% 
  filter(type == "Emotion Adj?" | "Confidence 1")

task_1_wide = task_1 %>% 
  select(-X) %>% 

  pivot_wider(names_from = type, values_from = answers) %>% 
  tidyr::unnest(cols = c("Adjective?", "Emotion Adj?"))



em_adj_df_ratings = task_1 %>% 
  select(-X) %>% 
  filter(type == "Confidence 1" | type == "Confidence 2" |
           type == "Confidence 3" | type == "Confidence 4" |
           answers == 1 & type == "Emotion Adj?") %>% 
  filter(answers == 1 | answers == 2 | answers == 3 | answers == 4) %>% 
  #  pivot_wider(names_from = "type", values_from = "answers") %>% 
  write.csv(here("data", "tidy", "em_adj_df_ratings_1.csv"))


p_id = prol_ids[5]

task_1 %>% 
  group_by(frame_content_condition, animacy_condtion) %>% 
  summarize(n = n())

tidy_df(prol_ids[190]) 



tidy_df = function(p_id)
{

df_sample = task_1 %>% 
  filter(prolific_id == p_id)

temp_1 = c(df_sample$type[1:4])
temp_1[2] = "adjective_1"
temp_1[3] = "emotion_adj_1"

temp_2 = c(df_sample$type[5:8])
temp_2[2] = "adjective_2"
temp_2[3] = "emotion_adj_2"

temp_3 = c(df_sample$type[9:12])
temp_3[2] = "adjective_3"
temp_3[3] = "emotion_adj_3"

temp_4 = c(df_sample$type[13:16])
temp_4[2] = "adjective_4"
temp_4[3] = "emotion_adj_4"


df_sample$answers[1:16]
df_sample$frame_content_condition[1]
df_sample$animacy_condtion[1]

df_fixed = data.frame(prolific_id = df_sample$prolific_id[1], 
           type = c(temp_1,
                    temp_2,
                    temp_3,
                    temp_4),
           answers = c(df_sample$answers[1:16]),
           frame = df_sample$frame_content_condition[1],
           animacy = df_sample$animacy_condtion[1]) %>% 
  pivot_wider(names_from = type, values_from = answers)
 
return(df_fixed) 
}

prol_ids = unique(task_1$prolific_id)

tidy_df_comb = tibble()

for (i in 1:length(prol_ids)) {
  t = tidy_df(prol_ids[i])
  tidy_df_comb = rbind(tidy_df_comb, t)
}

tidy_df_comb %>% 
  write.csv(here("data", "tidy", "task_1_rating.csv"))
  


