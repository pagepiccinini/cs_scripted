## LOAD PACKAGES ####
library(dplyr)
library(tidyr)
library(ggplot2)


## READ IN DATA AND ORGANIZE ####
mint = read.table("data/demographics/mint.txt", header=T, sep="\t", na.strings="") %>%
  mutate(correct_uncued = ifelse(!is.na(uncued), 1, 0)) %>%
  mutate(correct_semantic = ifelse(!is.na(uncued) | !is.na(cue_semantic_correct), 1, 0)) %>%
  mutate(correct_phonemic = ifelse(!is.na(uncued) | !is.na(cue_semantic_correct) | !is.na(cue_phonemic_correct),
                                   1, 0))


## SUMMARISE DATA ####
# By subject and language
mint_sum = mint %>%
  group_by(speaker, language) %>%
  summarise(score_uncued = mean(correct_uncued),
            score_semantic = mean(correct_semantic),
            score_phonemic = mean(correct_phonemic)) %>%
  ungroup()

# Make table for later comparision
mint_uncued_sum = mint_sum %>%
  select(speaker, language, score_uncued) %>%
  spread(language, score_uncued) %>%
  rename(mint_english = english) %>%
  rename(mint_spanish = spanish)

# Differences between languages
mint_uncued_diff = mint_sum %>%
  select(c(speaker, language, score_uncued)) %>%
  spread(language, score_uncued) %>%
  mutate(diff_uncued = english - spanish) %>%
  select(c(speaker, diff_uncued))

mint_semantic_diff = mint_sum %>%
  select(c(speaker, language, score_semantic)) %>%
  spread(language, score_semantic) %>%
  mutate(diff_semantic = english - spanish) %>%
  select(c(speaker, diff_semantic))

mint_phonemic_diff = mint_sum %>%
  select(c(speaker, language, score_phonemic)) %>%
  spread(language, score_phonemic) %>%
  mutate(diff_phonemic = english - spanish) %>%
  select(c(speaker, diff_phonemic))

mint_diff = mint_uncued_diff %>%
  inner_join(mint_semantic_diff) %>%
  inner_join(mint_phonemic_diff)


## PLOT SCORES ####
# Uncued
uncued.plot = ggplot(mint_sum, aes(x = language, y = score_uncued)) +
  geom_boxplot(aes(fill = language))

uncued.plot

# Semantic
semantic.plot = ggplot(mint_sum, aes(x = language, y = score_semantic)) +
  geom_boxplot(aes(fill = language))

semantic.plot

# Phonemic
phonemic.plot = ggplot(mint_sum, aes(x = language, y = score_phonemic)) +
  geom_boxplot(aes(fill = language))

phonemic.plot

# Uncued difference
uncued_diff = ggplot(mint_diff, aes(x = diff_uncued)) +
  geom_histogram(bin=0.1) +
  geom_vline(x = 0, col="red", lwd=2)

uncued_diff










