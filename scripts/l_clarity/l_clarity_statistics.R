## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Scripted/Results/cs_scripted_analysis/scripts/l_clarity/")


## LOAD REQUIRED PACKAGES
library(dplyr)
library(lme4)


## RUN CLEANING SCRIPT TO GET DATA
source("l_clarity_cleaning.R")


## PERPARE DATA FOR ANALYSIS
# General organization
lclar_stats = lclar_clean %>%
  # Set monolingual to baseline with contrast coding
  mutate(contextContrast = ifelse(context == "ml", -0.5, 0.5)) %>%
  # Set word 1 to baseline with contrast coding
  mutate(word_numberContrast = ifelse(word_number == "one", -0.5, 0.5)) %>%
  # Set onset to baseline with contrast coding
  mutate(l_positionContrast = ifelse(l_position == "onset", -0.5, 0.5))

# Separate for English
lclar_eng_stats = filter(lclar_stats, language=="english")

# Separate for Spanish
lclar_sp_stats = filter(lclar_stats, language=="spanish")


## BUILD MODELS FOR ENGLISH
# Full model
lclar_eng.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast +
                         (1|speaker) +
                         (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                         (1|word) +
                         (0+contextContrast+word_numberContrast|word) +
                         (1|sentence), REML=F, data=lclar_eng_stats)
lclar_eng.lmer_sum = summary(lclar_eng.lmer)

# Test for effect of context
lclar_eng_nocont.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - contextContrast +
                        (1|speaker) +
                        (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                        (1|word) +
                        (0+contextContrast+word_numberContrast|word) +
                        (1|sentence), REML=F, data=lclar_eng_stats)
lclar_eng_nocont.anova = anova(lclar_eng.lmer, lclar_eng_nocont.lmer)

# Test for effect of word number
lclar_eng_nown.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - word_numberContrast +
                               (1|speaker) +
                               (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                               (1|word) +
                               (0+contextContrast+word_numberContrast|word) +
                               (1|sentence), REML=F, data=lclar_eng_stats)
lclar_eng_nown.anova = anova(lclar_eng.lmer, lclar_eng_nown.lmer)

# Test for effect of /l/ position
lclar_eng_nolpos.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - l_positionContrast +
                             (1|speaker) +
                             (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                             (1|word) +
                             (0+contextContrast+word_numberContrast|word) +
                             (1|sentence), REML=F, data=lclar_eng_stats)
lclar_eng_nolpos.anova = anova(lclar_eng.lmer, lclar_eng_nolpos.lmer)

# Test for interaction of context x word number
lclar_eng_nocontxwn.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - contextContrast:word_numberContrast +
                               (1|speaker) +
                               (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                               (1|word) +
                               (0+contextContrast+word_numberContrast|word) +
                               (1|sentence), REML=F, data=lclar_eng_stats)
lclar_eng_nocontxwn.anova = anova(lclar_eng.lmer, lclar_eng_nocontxwn.lmer)

# Test for interaction of context x /l/ position
lclar_eng_nocontxlpos.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - contextContrast:l_positionContrast +
                                  (1|speaker) +
                                  (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                                  (1|word) +
                                  (0+contextContrast+word_numberContrast|word) +
                                  (1|sentence), REML=F, data=lclar_eng_stats)
lclar_eng_nocontxlpos.anova = anova(lclar_eng.lmer, lclar_eng_nocontxlpos.lmer)

# Test for interaction of word number x /l/ position
lclar_eng_nownxlpos.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - word_numberContrast:l_positionContrast +
                                    (1|speaker) +
                                    (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                                    (1|word) +
                                    (0+contextContrast+word_numberContrast|word) +
                                    (1|sentence), REML=F, data=lclar_eng_stats)
lclar_eng_nownxlpos.anova = anova(lclar_eng.lmer, lclar_eng_nownxlpos.lmer)

# Test for interaction of context x word number x /l/ position
lclar_eng_nocontxwnxlpos.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - contextContrast:word_numberContrast:l_positionContrast +
                                  (1|speaker) +
                                  (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                                  (1|word) +
                                  (0+contextContrast+word_numberContrast|word) +
                                  (1|sentence), REML=F, data=lclar_eng_stats)
lclar_eng_nocontxwnxlpos.anova = anova(lclar_eng.lmer, lclar_eng_nocontxwnxlpos.lmer)


## BUILD MODELS FOR SPANISH
# Full model
lclar_sp.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast +
                        (1|speaker) +
                        (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                        (1|word) +
                        (0+contextContrast+word_numberContrast|word) +
                        (1|sentence), REML=F, data=lclar_sp_stats)
lclar_sp.lmer_sum = summary(lclar_sp.lmer)

# Test for effect of context
lclar_sp_nocont.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - contextContrast +
                               (1|speaker) +
                               (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                               (1|word) +
                               (0+contextContrast+word_numberContrast|word) +
                               (1|sentence), REML=F, data=lclar_sp_stats)
lclar_sp_nocont.anova = anova(lclar_sp.lmer, lclar_sp_nocont.lmer)

# Test for effect of word number
lclar_sp_nown.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - word_numberContrast +
                             (1|speaker) +
                             (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                             (1|word) +
                             (0+contextContrast+word_numberContrast|word) +
                             (1|sentence), REML=F, data=lclar_sp_stats)
lclar_sp_nown.anova = anova(lclar_sp.lmer, lclar_sp_nown.lmer)

# Test for effect of /l/ position
lclar_sp_nolpos.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - l_positionContrast +
                               (1|speaker) +
                               (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                               (1|word) +
                               (0+contextContrast+word_numberContrast|word) +
                               (1|sentence), REML=F, data=lclar_sp_stats)
lclar_sp_nolpos.anova = anova(lclar_sp.lmer, lclar_sp_nolpos.lmer)

# Test for interaction of context x word number
lclar_sp_nocontxwn.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - contextContrast:word_numberContrast +
                                  (1|speaker) +
                                  (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                                  (1|word) +
                                  (0+contextContrast+word_numberContrast|word) +
                                  (1|sentence), REML=F, data=lclar_sp_stats)
lclar_sp_nocontxwn.anova = anova(lclar_sp.lmer, lclar_sp_nocontxwn.lmer)

# Test for interaction of context x /l/ position
lclar_sp_nocontxlpos.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - contextContrast:l_positionContrast +
                                    (1|speaker) +
                                    (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                                    (1|word) +
                                    (0+contextContrast+word_numberContrast|word) +
                                    (1|sentence), REML=F, data=lclar_sp_stats)
lclar_sp_nocontxlpos.anova = anova(lclar_sp.lmer, lclar_sp_nocontxlpos.lmer)

# Test for interaction of word number x /l/ position
lclar_sp_nownxlpos.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - word_numberContrast:l_positionContrast +
                                  (1|speaker) +
                                  (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                                  (1|word) +
                                  (0+contextContrast+word_numberContrast|word) +
                                  (1|sentence), REML=F, data=lclar_sp_stats)
lclar_sp_nownxlpos.anova = anova(lclar_sp.lmer, lclar_sp_nownxlpos.lmer)

# Test for interaction of context x word number x /l/ position
lclar_sp_nocontxwnxlpos.lmer = lmer(f3_f2 ~ contextContrast * word_numberContrast * l_positionContrast - contextContrast:word_numberContrast:l_positionContrast +
                                       (1|speaker) +
                                       (0+contextContrast*word_numberContrast+l_positionContrast|speaker) +
                                       (1|word) +
                                       (0+contextContrast+word_numberContrast|word) +
                                       (1|sentence), REML=F, data=lclar_sp_stats)
lclar_sp_nocontxwnxlpos.anova = anova(lclar_sp.lmer, lclar_sp_nocontxwnxlpos.lmer)


## FOLLOW-UP LINEAR REGRESSIONS ON SPANISH CONTEXT X WORD NUMBER INTERACTION
# English
lclar_eng_wd1.lm = lm(f3_f2 ~ contextContrast, data=subset(lclar_eng_stats, word_number=="one"))
lclar_eng_wd1.lm_sum = summary(lclar_eng_wd1.lm)

lclar_eng_wd2.lm = lm(f3_f2 ~ contextContrast, data=subset(lclar_eng_stats, word_number=="two"))
lclar_eng_wd2.lm_sum = summary(lclar_eng_wd2.lm)

# Spanish
lclar_sp_wd1.lm = lm(f3_f2 ~ contextContrast, data=subset(lclar_sp_stats, word_number=="one"))
lclar_sp_wd1.lm_sum = summary(lclar_sp_wd1.lm)

lclar_sp_wd2.lm = lm(f3_f2 ~ contextContrast, data=subset(lclar_sp_stats, word_number=="two"))
lclar_sp_wd2.lm_sum = summary(lclar_sp_wd2.lm)


