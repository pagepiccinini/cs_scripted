## LOAD PACKAGES ####
library(purrr)
library(dplyr)


## READ IN DATA ####
lenition = list.files(path = "data/lenition", full.names = TRUE) %>%
  map(read.table, header=T, "\t") %>%
  reduce(rbind)


## CLEAN DATA ####
lenition_clean = lenition %>%
  # Add column for condition
  mutate(condition = as.numeric(gsub("sub_","", speaker)) %% 8) %>%
  mutate(condition = ifelse(condition==0, 8, condition)) %>%
  # Remove disfluencies
  filter(is.na(sent_comment)) %>%
  filter(is.na(word_comment) | word_comment=="fric" | word_comment=="stop")


## SUMMARIZE DATA ####
lenition_sum = lenition_clean %>%
  group_by(language, context, word_number) %>%
  summarise(fricative_realization = mean(realization, na.rm=T))

