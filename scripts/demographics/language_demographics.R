## LOAD MINT SCRIPT ####
source("scripts/demographics/mint.R")


## READ IN NEW DATA AND ORGANIZE ####
lang_demo_full = read.table("data/demographics/language_demographics.txt", header=T, sep="\t") %>%
  inner_join(mint_uncued_sum)

lang_demo = lang_demo_full %>%
  filter(keeping == "yes")


## GET SUMMARY INFORMATION ####
# Gender
xtabs(~gender, lang_demo)

# Age
mean(lang_demo$age, na.rm=T)
min(lang_demo$age)
max(lang_demo$age)

# First / second language
xtabs(~english_native, lang_demo)
xtabs(~spanish_native, lang_demo)

# Language dominance
xtabs(~dominant_lg, lang_demo)

# Ethnicity
xtabs(~ethnicity, lang_demo)

# Ages of acquisition
aoa_l1_sum = lang_demo %>%
  select(speaker, l1, l1_aoa, l1_percentage) %>%
  rename(language = l1) %>%
  rename(age = l1_aoa) %>%
  rename(percentage = l1_percentage)

aoa_l2_sum = lang_demo %>%
  select(speaker, l2, l2_aoa, l2_percentage) %>%
  rename(language = l2) %>%
  rename(age = l2_aoa) %>%
  rename(percentage = l2_percentage)

aoa_sum = bind_rows(aoa_l1_sum, aoa_l2_sum) %>%
  group_by(language) %>%
  summarise(age_mean = mean(age),
            age_min = min(age),
            age_max = max(age),
            perc_mean = mean(percentage),
            perc_min = min(percentage),
            perc_max = max(percentage)) %>%
  ungroup()

# Proficiency
lang_demo %>%
  select(mint_english, english_prof_speaking, english_prof_understanding, english_prof_reading,
         mint_spanish, spanish_prof_speaking, spanish_prof_understanding, spanish_prof_reading) %>%
  summarise_each(funs(mean))

lang_demo %>%
  select(mint_english, english_prof_speaking, english_prof_understanding, english_prof_reading,
         mint_spanish, spanish_prof_speaking, spanish_prof_understanding, spanish_prof_reading) %>%
  summarise_each(funs(min))

lang_demo %>%
  select(mint_english, english_prof_speaking, english_prof_understanding, english_prof_reading,
         mint_spanish, spanish_prof_speaking, spanish_prof_understanding, spanish_prof_reading) %>%
  summarise_each(funs(max))




