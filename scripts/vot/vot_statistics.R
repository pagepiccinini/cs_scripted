## READ IN DATA ####
source("scripts/vot/vot_cleaning.R")


## LOAD PACKAGES ####
library(lme4)


## ORGANIZE DATA ####
vot_stats = vot_clean %>%
  # Do log10 transform on VOT durations
  mutate(duration_ms_log10 = log10(duration_ms)) %>%
  # Set English to baseline with contrast coding
  mutate(languageContrast = ifelse(language == "english", -0.5, 0.5)) %>%
  # Set monolingual to baseline with contrast coding
  mutate(contextContrast = ifelse(context == "ml", -0.5, 0.5)) %>%
  # Set word 1 to baseline with contrast coding
  mutate(word_numberContrast = ifelse(word_number == "one", -0.5, 0.5))


## BUILD MODELS ####
# Full model
vot.lmer = lmer(duration_ms_log10 ~ languageContrast * contextContrast * word_numberContrast +
                  (1|speaker) +
                  (0+languageContrast*contextContrast*word_numberContrast|speaker) +
                  (1|word) +
                  (0+contextContrast+word_numberContrast|word) +
                  (1|sentence), REML=F, data=vot_stats)

vot.lmer_sum = summary(vot.lmer)

# Test for significant effect of language
vot_nolg.lmer = lmer(duration_ms_log10 ~ languageContrast * contextContrast * word_numberContrast - languageContrast +
                  (1|speaker) +
                  (0+languageContrast*contextContrast*word_numberContrast|speaker) +
                  (1|word) +
                  (0+contextContrast+word_numberContrast|word) +
                  (1|sentence), REML=F, data=vot_stats)

vot_nolg.anova = anova(vot.lmer, vot_nolg.lmer)

# Test for significant effect of context
vot_nocont.lmer = lmer(duration_ms_log10 ~ languageContrast * contextContrast * word_numberContrast - contextContrast +
                         (1|speaker) +
                         (0+languageContrast*contextContrast*word_numberContrast|speaker) +
                         (1|word) +
                         (0+contextContrast+word_numberContrast|word) +
                         (1|sentence), REML=F, data=vot_stats)

vot_nocont.anova = anova(vot.lmer, vot_nocont.lmer)

# Test for significant effect of word number
vot_nown.lmer = lmer(duration_ms_log10 ~ languageContrast * contextContrast * word_numberContrast - word_numberContrast +
                       (1|speaker) +
                       (0+languageContrast*contextContrast*word_numberContrast|speaker) +
                       (1|word) +
                       (0+contextContrast+word_numberContrast|word) +
                       (1|sentence), REML=F, data=vot_stats)

vot_nown.anova = anova(vot.lmer, vot_nown.lmer)

# Test for significant 2-way interaction of language x context
vot_nolgxcont.lmer = lmer(duration_ms_log10 ~ languageContrast * contextContrast * word_numberContrast - languageContrast:contextContrast +
                            (1|speaker) +
                            (0+languageContrast*contextContrast*word_numberContrast|speaker) +
                            (1|word) +
                            (0+contextContrast+word_numberContrast|word) +
                            (1|sentence), REML=F, data=vot_stats)

vot_nolgxcont.anova = anova(vot.lmer, vot_nolgxcont.lmer)

# Test for significant 2-way interaction of language x word number
vot_nolgxwn.lmer = lmer(duration_ms_log10 ~ languageContrast * contextContrast * word_numberContrast - languageContrast:word_numberContrast +
                          (1|speaker) +
                          (0+languageContrast*contextContrast*word_numberContrast|speaker) +
                          (1|word) +
                          (0+contextContrast+word_numberContrast|word) +
                          (1|sentence), REML=F, data=vot_stats)

vot_nolgxwn.anova = anova(vot.lmer, vot_nolgxwn.lmer)

# Test for significant 2-way interaction of context x word number
vot_nocontxwn.lmer = lmer(duration_ms_log10 ~ languageContrast * contextContrast * word_numberContrast - contextContrast:word_numberContrast +
                            (1|speaker) +
                            (0+languageContrast*contextContrast*word_numberContrast|speaker) +
                            (1|word) +
                            (0+contextContrast+word_numberContrast|word) +
                            (1|sentence), REML=F, data=vot_stats)

vot_nocontxwn.anova = anova(vot.lmer, vot_nocontxwn.lmer)

# Test for significant 3-way interaction of language x context x word number
vot_nolgxcontxwn.lmer = lmer(duration_ms_log10 ~ languageContrast * contextContrast * word_numberContrast - languageContrast:contextContrast:word_numberContrast +
                               (1|speaker) +
                               (0+languageContrast*contextContrast*word_numberContrast|speaker) +
                               (1|word) +
                               (0+contextContrast+word_numberContrast|word) +
                               (1|sentence), REML=F, data=vot_stats)

vot_nolgxcontxwn.anova = anova(vot.lmer, vot_nolgxcontxwn.lmer)


## FOLLOW-UP REGRESSIONS ON 3-WAY INTERACTION OF LANGUAGE x CONTEXT x WORD NUMBER ####
vot_engwn1.lm = lm(duration_ms_log10 ~ contextContrast, data=subset(vot_stats, language=="english" & word_number=="one"))
vot_engwn1.lm_sum = summary(vot_engwn1.lm)

vot_engwn2.lm = lm(duration_ms_log10 ~ contextContrast, data=subset(vot_stats, language=="english" & word_number=="two"))
vot_engwn2.lm_sum = summary(vot_engwn2.lm)

vot_spwn1.lm = lm(duration_ms_log10 ~ contextContrast, data=subset(vot_stats, language=="spanish" & word_number=="one"))
vot_spwn1.lm_sum = summary(vot_spwn1.lm)

vot_spwn2.lm = lm(duration_ms_log10 ~ contextContrast, data=subset(vot_stats, language=="spanish" & word_number=="two"))
vot_spwn2.lm_sum = summary(vot_spwn2.lm)
