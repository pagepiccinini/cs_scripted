## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Scripted/Results/cs_scripted_analysis/data")


## LOAD LIBRARIES
library(dplyr)
library(tidyr)

## READ IN DATA
word_info = read.table("word_information.txt", header=T, sep="\t", stringsAsFactors=F)
sub_45_sent = read.table("raw_data/sub_45_sentences_INITIAL.txt", header=T, sep="\t", stringsAsFactors=F)


## ORGANIZE DATA
# Add comments for sentences
sub_45_sent_comments = sub_45_sent %>%
  separate(sentence, into=c("prefix", "number", "sent_comment"), sep="_") %>%
  mutate(sentence=paste(prefix, number, sep="_")) %>%
  select(-prefix, -number)

# Combine sentence and word files and remove 'copa' tokens
sub_45 = inner_join(sub_45_sent_comments, word_info)
sub_45 = filter(sub_45, word!="copa")


## CHECK IF SHOULD BE EXCLUDED
# Remove disfluencies
sub_45_sub = sub_45 %>%
  filter(is.na(sent_comment))

# Separte out analysis with different variables
sub_45_stops = filter(sub_45_sub, feature!="l_clarity")
sub_45_lclar = filter(sub_45_sub, feature=="l_clarity")

# Check for outliers
  # Need 12 per cell
xtabs(~language+context+word_number+feature, sub_45_stops)

  # Need 4 per cell
xtabs(~language+context+word_number+l_position, sub_45_lclar)
















