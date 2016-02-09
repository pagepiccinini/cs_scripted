## LOAD PACKAGES ####
library(purrr)
library(dplyr)


## READ IN DATA ####
vot = list.files(path = "data/vot", full.names = TRUE) %>%
  map(read.table, header=T, "\t") %>%
  reduce(rbind)


## CLEAN DATA ####
vot_clean = vot %>%
  # Add column for condition
  mutate(condition = as.numeric(gsub("sub_","", speaker)) %% 8) %>%
  mutate(condition = ifelse(condition==0, 8, condition)) %>%
  # Remove disfluencies
  filter(is.na(sent_comment)) %>%
  filter(is.na(word_comment))
  

## SUMMARIZE DATA ####
vot_sum = vot_clean %>%
  group_by(language, context, word_number) %>%
  summarise(duration = mean(duration_ms))
  
