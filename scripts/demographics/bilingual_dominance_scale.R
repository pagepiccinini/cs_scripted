## LOAD MINT SCRIPT ####
source("scripts/demographics/language_demographics.R")


## ORGANIZE DATA FOR BDS AND COMPUTE SCORES ####
bds = lang_demo %>%
  select(speaker, age_spanish, age_english, comfort_spanish, comfort_english,
        use_predominantly, use_math, accent, use_life,
        school_spanish, school_english, lost_fluency, lost_fluency_lg, lost_fluency_age,
        current_country) %>%
  mutate(comfort_english = as.numeric(as.character(comfort_english))) %>%
  mutate(comfort_spanish = as.numeric(as.character(comfort_spanish))) %>%
  # Questions 1 and 2
  mutate(bds_english = ifelse(age_english <= 5, 5,
                       ifelse(age_english <= 9, 3,
                       ifelse(age_english <= 15, 1, 0))),
         bds_spanish = ifelse(age_spanish <= 5, 5,
                       ifelse(age_spanish <= 9, 3,
                       ifelse(age_spanish <= 15, 1, 0)))) %>%
  # Questions 3 and 4
  mutate(bds_english = ifelse(comfort_english <= 5, bds_english + 5,
                       ifelse(comfort_english <= 9, bds_english + 3,
                       ifelse(comfort_english <= 15, bds_english + 1, bds_english + 0))),
         bds_spanish = ifelse(comfort_spanish <= 5, bds_spanish + 5,
                       ifelse(comfort_spanish <= 9, bds_spanish + 3,
                       ifelse(comfort_spanish <= 15, bds_spanish + 1, bds_spanish + 0)))) %>%
  # Question 5
  mutate(bds_english = ifelse(use_predominantly == "english", bds_english + 5,
                       ifelse(use_predominantly == "both", bds_english + 3, bds_english + 0)),
         bds_english = ifelse(use_predominantly == "spanish", bds_spanish + 5,
                       ifelse(use_predominantly == "both", bds_spanish + 3, bds_spanish + 0))) %>%
  # Question 6
  mutate(bds_english = ifelse(use_math == "english", bds_english + 3, bds_english + 0),
         bds_spanish = ifelse(use_math == "spanish", bds_spanish + 3, bds_spanish + 0)) %>%
  # Question 7
  mutate(bds_english = ifelse(accent == "spanish", bds_english + 5,
                       ifelse(accent == "both", bds_english + 3, bds_english + 0)),
         bds_spanish = ifelse(accent == "english", bds_spanish + 5,
                       ifelse(accent == "both", bds_spanish + 3, bds_spanish + 0))) %>%
  # Question 8
  mutate(bds_english = ifelse(use_life == "english", bds_english + 2, bds_english + 0),
         bds_spanish = ifelse(use_life == "spanish", bds_spanish + 2, bds_spanish + 0)) %>%
  # Questions 9 and 10
  mutate(bds_english = ifelse(school_english == 0, bds_english + 0,
                       ifelse(school_english <= 6, bds_english + 1, bds_english + 2)),
         bds_spanish = ifelse(school_spanish == 0, bds_spanish + 0,
                       ifelse(school_spanish <= 6, bds_spanish + 1, bds_spanish + 2))) %>%
  # Question 11
  mutate(bds_english = ifelse(lost_fluency == "yes" & lost_fluency_lg == "english", bds_english - 3, bds_english + 0),
         bds_spanish = ifelse(lost_fluency == "yes" & lost_fluency_lg == "spanish", bds_spanish - 3, bds_spanish + 0)) %>%
  # Question 12
  mutate(bds_english = bds_english + 4) %>%
  # BDS score
  mutate(bds_score = bds_english - bds_spanish) %>%
  mutate(bds_dom = ifelse(bds_score > 5, "english_dom",
                   ifelse(bds_score < -5, "spanish_dom", "balanced")))


## GET SUMMARY INFORMATION ####
mean(bds$bds_score, na.rm = T)
sd(bds$bds_score, na.rm = T)
