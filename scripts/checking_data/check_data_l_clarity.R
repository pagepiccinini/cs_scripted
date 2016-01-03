## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Scripted/Results/cs_scripted_analysis/data/l_clarity")


## LOAD LIBRARIES
library(dplyr)


## READ IN DATA
sub_56_l = read.table("sub_56_l_preout.txt", header=T, sep="\t")


## CHECK FOR OUTLIERS
# Summarise data
sub_56_l_sum = sub_56_l %>%
  filter(is.na(sent_comment)) %>%
  filter(is.na(word_comment)) %>%
  group_by(language) %>%
  summarise(f3_f2_mean = mean(f3_f2),
            f3_f2_sd = sd(f3_f2)) %>%
  mutate(f3_f2_min = f3_f2_mean - (2 * f3_f2_sd)) %>%
  mutate(f3_f2_max = f3_f2_mean + (2 * f3_f2_sd)) %>%
  ungroup()

# Find non-outliers
sub_56_l_noout = sub_56_l %>%
  inner_join(sub_56_l_sum) %>%
  filter(f3_f2 > f3_f2_min & f3_f2 < f3_f2_max)

# Find outliers
sub_56_l_out = sub_56_l %>%
  inner_join(sub_56_l_sum) %>%
  filter(f3_f2 <= f3_f2_min | f3_f2 >= f3_f2_max)

# Look at first three non-outliers and all outliers
sub_56_l_noout[1:3,]
dim(sub_56_l_out)
sub_56_l_out


## REPLACE OUTLIERS <speaker specific>
sub_56_l_fixed = sub_56_l %>%
  # Outlier #1
  mutate(f1 = ifelse(time_stamp == sub_56_l_out$time_stamp[1], 399.60407296133116, f1),
         f2 = ifelse(time_stamp == sub_56_l_out$time_stamp[1], 975.7857819590062, f2),
         f3 = ifelse(time_stamp == sub_56_l_out$time_stamp[1], 2435.0013232437313, f3)) %>%
  # Outlier #2
  mutate(f1 = ifelse(time_stamp == sub_56_l_out$time_stamp[2], 351.7167728400676, f1),
         f2 = ifelse(time_stamp == sub_56_l_out$time_stamp[2], 1881.7307522788353, f2),
         f3 = ifelse(time_stamp == sub_56_l_out$time_stamp[2], 2555.9713524352505, f3)) %>%
  # Outlier #3
  mutate(f1 = ifelse(time_stamp == sub_56_l_out$time_stamp[3], 354.11514092276883, f1),
         f2 = ifelse(time_stamp == sub_56_l_out$time_stamp[3], 2068.4294165071046, f2),
         f3 = ifelse(time_stamp == sub_56_l_out$time_stamp[3], 2453.5476488359423, f3)) %>%
  # Outlier #4
  mutate(f1 = ifelse(time_stamp == sub_56_l_out$time_stamp[4], 319.32901136848653, f1),
         f2 = ifelse(time_stamp == sub_56_l_out$time_stamp[4], 1187.036059388009, f2),
         f3 = ifelse(time_stamp == sub_56_l_out$time_stamp[4], 2533.4552810981245, f3)) %>%
  # Outlier #5
  mutate(f1 = ifelse(time_stamp == sub_56_l_out$time_stamp[5], 299.8016476056404, f1),
         f2 = ifelse(time_stamp == sub_56_l_out$time_stamp[5], 1186.7229077499308, f2),
         f3 = ifelse(time_stamp == sub_56_l_out$time_stamp[5], 2390.978451400307, f3)) %>%
  # Outlier #6
  mutate(f1 = ifelse(time_stamp == sub_56_l_out$time_stamp[6], 313.1138264594691, f1),
         f2 = ifelse(time_stamp == sub_56_l_out$time_stamp[6], 1124.095465197938, f2),
         f3 = ifelse(time_stamp == sub_56_l_out$time_stamp[6], 2269.264710665814, f3)) %>%
  # Outlier #7
  #mutate(f1 = ifelse(time_stamp == sub_56_l_out$time_stamp[7], 437.027997621583, f1),
  #       f2 = ifelse(time_stamp == sub_56_l_out$time_stamp[7], 1103.2224843701017, f2),
  #       f3 = ifelse(time_stamp == sub_56_l_out$time_stamp[7], 3531, f3)) %>%
  # Outlier #8
  #mutate(f1 = ifelse(time_stamp == sub_56_l_out$time_stamp[8], 589.5133930422727, f1),
  #       f2 = ifelse(time_stamp == sub_56_l_out$time_stamp[8], 1417.318258187188, f2),
  #       f3 = ifelse(time_stamp == sub_56_l_out$time_stamp[8], 3325, f3)) %>%
  # Outlier #9
  #mutate(f1 = ifelse(time_stamp == sub_56_l_out$time_stamp[9], 363.4254290384839, f1),
  #       f2 = ifelse(time_stamp == sub_56_l_out$time_stamp[9], 2056.7755337882168, f2),
  #       f3 = ifelse(time_stamp == sub_56_l_out$time_stamp[9], 3000.4707747896878, f3)) %>%
  # Outlier #10
  #mutate(f1 = ifelse(time_stamp == sub_56_l_out$time_stamp[10], 557.4305316805066, f1),
  #       f2 = ifelse(time_stamp == sub_56_l_out$time_stamp[10], 1317.4574123684251, f2),
  #       f3 = ifelse(time_stamp == sub_56_l_out$time_stamp[10], 3340.7752212785576, f3)) %>%
  # Redo column for difference
  mutate(f3_f2 = f3 - f2)


## SEE NUMBER OF OUTLIERS WITH CORRECTED DATA
# Summarise data
sub_56_l_sum_fixed = sub_56_l_fixed %>%
  filter(is.na(sent_comment)) %>%
  filter(is.na(word_comment)) %>%
  group_by(language) %>%
  summarise(f3_f2_mean = mean(f3_f2),
            f3_f2_sd = sd(f3_f2)) %>%
  mutate(f3_f2_min = f3_f2_mean - (2 * f3_f2_sd)) %>%
  mutate(f3_f2_max = f3_f2_mean + (2 * f3_f2_sd)) %>%
  ungroup()

# Find outliers
sub_56_l_out_fixed = sub_56_l_fixed %>%
  inner_join(sub_56_l_sum_fixed) %>%
  filter(f3_f2 <= f3_f2_min | f3_f2 >= f3_f2_max)
dim(sub_56_l_out_fixed)
sub_56_l_out_fixed


## WRITE TO TABLE
write.table(sub_56_l_fixed, "sub_56_l.txt", sep="\t", eol="\n")







