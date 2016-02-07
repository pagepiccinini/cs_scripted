## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Scripted/Results/cs_scripted_analysis/data/l_clarity/")


## LOAD REQUIRED PACKAGES ####
library(purrr)
library(dplyr)


## READ IN DATA ####
lclar = list.files(pattern = "l.txt$") %>%
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


