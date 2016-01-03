## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Scripted/Results/cs_scripted_analysis/scripts/lenition/")


## LOAD REQUIRED PACKAGES
library(dplyr)
library(lme4)


## RUN CLEANING SCRIPT TO GET DATA
source("lenition_cleaning.R")


## PERPARE DATA FOR ANALYSIS
lenition_stats = lenition_clean %>%
  # Filter out English tokens
  filter(language == "spanish") %>%
  # Set monolingual to baseline with contrast coding
  mutate(contextContrast = ifelse(context == "ml", -0.5, 0.5)) %>%
  # Set word 1 to baseline with contrast coding
  mutate(word_numberContrast = ifelse(word_number == "one", -0.5, 0.5))


## BUILD MODELS
# Full model
lenition.glmer = glmer(realization ~ contextContrast * word_numberContrast +
                         (1|speaker) +
                         (0+contextContrast+word_numberContrast|speaker) +
                         (1|word) +
                         (0+word_numberContrast|word) +
                         (1|sentence), family=binomial, data=lenition_stats)
lenition.glmer_sum = summary(lenition.glmer)

# Test for significant effect of context
lenition_nocont.glmer = glmer(realization ~ contextContrast * word_numberContrast - contextContrast +
                                (1|speaker) +
                                (0+contextContrast+word_numberContrast|speaker) +
                                (1|word) +
                                (0+word_numberContrast|word) +
                                (1|sentence), family=binomial, data=lenition_stats)
lenition_nocont.anova = anova(lenition.glmer, lenition_nocont.glmer)

# Test for significant effect of word number
lenition_nown.glmer = glmer(realization ~ contextContrast * word_numberContrast - word_numberContrast +
                              (1|speaker) +
                              (0+contextContrast+word_numberContrast|speaker) +
                              (1|word) +
                              (0+word_numberContrast|word) +
                              (1|sentence), family=binomial, data=lenition_stats)
lenition_nown.anova = anova(lenition.glmer, lenition_nown.glmer)

# Test for significant 2-way interaction of context x word number
lenition_nocontxwn.glmer = glmer(realization ~ contextContrast * word_numberContrast - contextContrast:word_numberContrast +
                                   (1|speaker) +
                                   (0+contextContrast+word_numberContrast|speaker) +
                                   (1|word) +
                                   (0+word_numberContrast|word) +
                                   (1|sentence), family=binomial, data=lenition_stats)
lenition_nocontxwn.anova = lenition_nocontxwn.anova = anova(lenition.glmer, lenition_nocontxwn.glmer)

