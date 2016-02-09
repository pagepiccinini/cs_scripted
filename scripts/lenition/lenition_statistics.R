###########################################################################################
#### Note: This script was not used in the final analysis as there were too few tokens ####
#### of English fricative realizations.                                                ####
###########################################################################################

## READ IN DATA ####
source("scripts/lenition/lenition_cleaning.R")


## LOAD PACKAGES ####
library(lme4)


## ORGANIZE DATA ####
lenition_stats = lenition_clean %>%
  # Set English to baseline with contrast coding
  mutate(languageContrast = ifelse(language == "english", -0.5, 0.5)) %>%
  # Set monolingual to baseline with contrast coding
  mutate(contextContrast = ifelse(context == "ml", -0.5, 0.5)) %>%
  # Set word 1 to baseline with contrast coding
  mutate(word_numberContrast = ifelse(word_number == "one", -0.5, 0.5))


## BUILD MODELS ####
# Full model
lenition.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast +
                  (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                  (1+contextContrast*word_numberContrast|word) +
                  (1|sentence), family=binomial, data=lenition_stats)

lenition.glmer_sum = summary(lenition.glmer)

# Test for significant effect of language
lenition_nolg.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - languageContrast +
                       (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                       (1+contextContrast*word_numberContrast|word) +
                       (1|sentence), family=binomial, data=lenition_stats)

lenition_nolg.anova = anova(lenition.glmer, lenition_nolg.glmer)

# Test for significant effect of context
lenition_nocont.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - contextContrast +
                         (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                         (1+contextContrast*word_numberContrast|word) +
                         (1|sentence), family=binomial, data=lenition_stats)

lenition_nocont.anova = anova(lenition.glmer, lenition_nocont.glmer)

# Test for significant effect of word number
lenition_nown.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - word_numberContrast +
                       (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                       (1+contextContrast*word_numberContrast|word) +
                       (1|sentence), family=binomial, data=lenition_stats)

lenition_nown.anova = anova(lenition.glmer, lenition_nown.glmer)

# Test for significant 2-way interaction of language x context
lenition_nolgxcont.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - languageContrast:contextContrast +
                            (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                            (1+contextContrast*word_numberContrast|word) +
                            (1|sentence), family=binomial, data=lenition_stats)

lenition_nolgxcont.anova = anova(lenition.glmer, lenition_nolgxcont.glmer)

# Test for significant 2-way interaction of language x word number
lenition_nolgxwn.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - languageContrast:word_numberContrast +
                          (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                          (1+contextContrast*word_numberContrast|word) +
                          (1|sentence), family=binomial, data=lenition_stats)

lenition_nolgxwn.anova = anova(lenition.glmer, lenition_nolgxwn.glmer)

# Test for significant 2-way interaction of context x word number
lenition_nocontxwn.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - contextContrast:word_numberContrast +
                            (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                            (1+contextContrast*word_numberContrast|word) +
                            (1|sentence), family=binomial, data=lenition_stats)

lenition_nocontxwn.anova = anova(lenition.glmer, lenition_nocontxwn.glmer)

# Test for significant 3-way interaction of language x context x word number
lenition_nolgxcontxwn.glmer = glmer(realization ~ languageContrast * contextContrast * word_numberContrast - languageContrast:contextContrast:word_numberContrast +
                               (1+languageContrast*contextContrast*word_numberContrast|speaker) +
                               (1+contextContrast*word_numberContrast|word) +
                               (1|sentence), family=binomial, data=lenition_stats)

lenition_nolgxcontxwn.anova = anova(lenition.glmer, lenition_nolgxcontxwn.glmer)