## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Scripted/Results/cs_scripted_analysis/scripts/lenition/")


## LOAD REQUIRED PACKAGES
library(dplyr)
library(ggplot2)


## RUN CLEANING SCRIPT TO GET DATA
source("lenition_cleaning.R")


## PREPARE DATA FOR FIGURES
lenition_figs = lenition_clean %>%
  mutate(language = factor(language, levels=c("english", "spanish"), labels=c("English", "Spanish"))) %>%
  mutate(context = factor(context, levels=c("ml", "cs"), labels=c("monolingual", "code-switching"))) %>%
  group_by(speaker, language, context, word_number) %>%
  summarise(fric_realization = mean(realization) * 100) %>%
  ungroup()


## MAKE FIGURES
# Monolingual versus code-switching
lenition.fig = ggplot(lenition_figs, aes(x=language, y=fric_realization)) +
  geom_boxplot(aes(fill=context)) +
  #scale_fill_manual(values=c("white", "grey")) +
  ggtitle("Lenition in English and Spanish\nSeparated by Context") +
  xlab("Language") +
  ylab("Percentage of time realized as a fricative") +
  guides(fill=guide_legend(title="context")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
lenition.fig

# Within code-switching, language x word number
lenition_cs.fig = ggplot(subset(lenition_figs, context=="code-switching"), aes(x=language, y=fric_realization)) +
  geom_boxplot(aes(fill=word_number)) +
  #scale_fill_manual(values=c("white", "grey")) +
  ggtitle("Lenition in Code-switching English and Spanish\nSeparated by Target Word Number") +
  xlab("Language") +
  ylab("Percentage of time realized as a fricative") +
  guides(fill=guide_legend(title="word number")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
lenition_cs.fig

# Within English, context x word number
lenition_eng.fig = ggplot(subset(lenition_figs, language=="English"), aes(x=context, y=fric_realization)) +
  geom_boxplot(aes(fill=word_number)) +
  #scale_fill_manual(values=c("white", "grey")) +
  ggtitle("Lenition in English\nSeparated by Context and Target Word Number") +
  xlab("Context") +
  ylab("Percentage of time realized as a fricative") +
  guides(fill=guide_legend(title="word number")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
lenition_eng.fig

# Within Spanish, context x word number
lenition_sp.fig = ggplot(subset(lenition_figs, language=="Spanish"), aes(x=context, y=fric_realization)) +
  geom_boxplot(aes(fill=word_number)) +
  #scale_fill_manual(values=c("white", "grey")) +
  ggtitle("Lenition in Spanish Separated\nby Context and Target Word Number") +
  xlab("Context") +
  ylab("Percentage of time realized as a fricative") +
  guides(fill=guide_legend(title="word number")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
lenition_sp.fig

# Monolingual versus code-switching by word number by language
lenition_lgxcontxwn.fig = ggplot(lenition_figs, aes(x=word_number, y=fric_realization)) +
  geom_boxplot(aes(fill=context)) +
  facet_wrap(~language) +
  scale_fill_manual(values=c("white", "grey")) +
  ggtitle("Lenition in English and Spanish\nby Context and Target Word Number") +
  xlab("Word number") +
  ylab("Percentage of time realized as a fricative") +
  guides(fill=guide_legend(title="context")) +
  ylim(0, 100) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
pdf("../../figures/lenition_lgxcontxwn.pdf")
lenition_lgxcontxwn.fig
dev.off()
