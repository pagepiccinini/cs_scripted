## LOAD REQUIRED PACKAGES ####
library(purrr)
library(dplyr)


## READ IN DATA ####
lclar = list.files(path = "data/l_clarity", pattern = "l.txt$", full.names = TRUE) %>%
  map(read.table, header=T, "\t") %>%
  reduce(rbind)


## CLEAN DATA ####
lclar_clean = lclar %>%
  # Add column for condition
  mutate(condition = as.numeric(gsub("sub_","", speaker)) %% 8) %>%
  mutate(condition = ifelse(condition==0, 8, condition)) %>%
  # Remove disfluencies
  filter(is.na(sent_comment)) %>%
  filter(is.na(word_comment))


## SUMMARIZE DATA ####
lclar_sum = lclar_clean %>%
  group_by(language, context, word_number, l_position) %>%
  summarise(f3_f2 = mean(f3_f2))



