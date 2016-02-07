## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Scripted/Results/cs_scripted_analysis/scripts/lenition/")


## LOAD REQUIRED PACKAGES
library(dplyr)
library(lme4)


## RUN CLEANING SCRIPT TO GET DATA
source("lenition_cleaning.R")


## PERPARE DATA FOR ANALYSIS
lenition_stats = lenition_data %>%
  # Set English to baseline with contrast coding
  mutate(languageContrast = ifelse(language == "english", -0.5, 0.5)) %>%
  # Set monolingual to baseline with contrast coding
  mutate(contextContrast = ifelse(context == "ml", -0.5, 0.5)) %>%
  # Set word 1 to baseline with contrast coding
  mutate(word_numberContrast = ifelse(word_number == "one", -0.5, 0.5))


## BUILD MODELS
# Full model
lenition.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast +
                  (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                  (1+contextContrast*word_numberContrast|word) +
                  (1|sentence), family=binomial, data=lenition_stats)
summary(lenition.glmer)

# Test for significant effect of language
lenition_nolg.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - languageContrast +
                       (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                       (1+contextContrast*word_numberContrast|word) +
                       (1|sentence), family=binomial, data=lenition_stats)
anova(lenition.glmer, lenition_nolg.glmer)

# Test for significant effect of context
lenition_nocont.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - contextContrast +
                         (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                         (1+contextContrast*word_numberContrast|word) +
                         (1|sentence), family=binomial, data=lenition_stats)
anova(lenition.glmer, lenition_nocont.glmer)

# Test for significant effect of word number
lenition_nown.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - word_numberContrast +
                       (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                       (1+contextContrast*word_numberContrast|word) +
                       (1|sentence), family=binomial, data=lenition_stats)
anova(lenition.glmer, lenition_nown.glmer)

# Test for significant 2-way interaction of language x context
lenition_nolgxcont.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - languageContrast:contextContrast +
                            (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                            (1+contextContrast*word_numberContrast|word) +
                            (1|sentence), family=binomial, data=lenition_stats)
anova(lenition.glmer, lenition_nolgxcont.glmer)

# Test for significant 2-way interaction of language x word number
lenition_nolgxwn.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - languageContrast:word_numberContrast +
                          (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                          (1+contextContrast*word_numberContrast|word) +
                          (1|sentence), family=binomial, data=lenition_stats)
anova(lenition.glmer, lenition_nolgxwn.glmer)

# Test for significant 2-way interaction of context x word number
lenition_nocontxwn.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - contextContrast:word_numberContrast +
                            (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                            (1+contextContrast*word_numberContrast|word) +
                            (1|sentence), family=binomial, data=lenition_stats)
anova(lenition.glmer, lenition_nocontxwn.glmer)

# Test for significant 3-way interaction of language x context x word number
lenition_nolgxcontxwn.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - languageContrast:contextContrast:word_numberContrast +
                               (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                               (1+contextContrast*word_numberContrast|word) +
                               (1|sentence), family=binomial, data=lenition_stats)
anova(lenition.glmer, lenition_nolgxcontxwn.glmer)