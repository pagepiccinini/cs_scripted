## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Scripted/Results/cs_scripted_analysis/data")


## LOAD LIBRARIES
library(dplyr)
library(tidyr)


## READ IN DATA
word_info = read.table("word_information.txt", header=T, sep="\t", stringsAsFactors=F)
sub_13_sent = read.table("raw_data/sub_13_sentences.txt", header=T, sep="\t", stringsAsFactors=F)
sub_13_word = read.table("raw_data/sub_13_words.txt", header=T, sep="\t", stringsAsFactors=F)
sub_13_l_data = read.table("raw_data/sub_13_l_data.txt", header=T, sep="\t", stringsAsFactors=F)


## ORGANIZE DATA
# Add comments for sentences
sub_13_sent_comments = sub_13_sent %>%
	separate(sentence, into=c("prefix", "number", "sent_comment"), sep="_") %>%
	mutate(sentence=paste(prefix, number, sep="_")) %>%
	select(-prefix, -number)
	
# Add comments for words
sub_13_word_comments = sub_13_word %>%
	separate(word, into=c("phoneme", "word", "word_comment"), sep="_")
	
# Combine sentence and word files and remove 'copa' tokens
sub_13 = cbind(sub_13_sent_comments, sub_13_word_comments)
sub_13 = filter(sub_13, word!="copa")

# Combine with other word information
sub_13_full = inner_join(sub_13, word_info)


## CHECK IF SHOULD BE EXCLUDED
sub_13_sub = sub_13_full %>%
	filter(is.na(sent_comment)) %>%
	filter(is.na(word_comment) | word_comment=="stop" | word_comment=="fric")
	
xtabs(~language+context+word_number+feature, subset(sub_13_sub, feature!="l_clarity")) # 12 per cell
xtabs(~language+context+word_number+l_position, subset(sub_13_sub, feature=="l_clarity")) # 4 per cell


## MAKE ANALYSIS SPECIFIC DATA FRAMES
# Separate into different analyses
sub_13_vot = filter(sub_13_full, feature=="VOT")
sub_13_l_temp = filter(sub_13_full, feature=="l_clarity")
sub_13_lenition_temp = filter(sub_13_full, feature=="lenition")

# Oganize l-clarity data
sub_13_l = cbind(sub_13_l_temp, sub_13_l_data[,2:4])
sub_13_l$f3_f2 = sub_13_l$f3 - sub_13_l$f2

# Organize lenition data with 'stop' is 0 and 'fric' is 1
sub_13_lenition = sub_13_lenition_temp %>%
	mutate(realization = ifelse(word_comment=="stop", 0, ifelse(word_comment=="fric", 1, NA)))
	

## WRITE DATA FILES 
write.table(sub_13_vot, "vot/sub_13_vot.txt", sep="\t", eol="\n")
write.table(sub_13_l, "l_clarity/sub_13_l_preout.txt", sep="\t", eol="\n")
write.table(sub_13_lenition, "lenition/sub_13_lenition.txt", sep="\t", eol="\n")












