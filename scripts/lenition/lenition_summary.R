## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Scripted/Results/data/lenition")


## LOAD LIBRARIES
library(dplyr)


## READ IN DATA
all_my_files = list.files(full.names=T)
my_files = grep("sub_[0-9][0-9]_lenition.txt", all_my_files, value=T)
dataframes = lapply(my_files, read.delim)
data = do.call(rbind, dataframes)


## SUMMARIZE
data_summ = data %>%
	group_by(language, context, word_number) %>%
	summarise(fricative_realization = mean(realization, na.rm=T))