## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Scripted/Results/cs_scripted_analysis/scripts/vot/")


## LOAD REQUIRED PACKAGES
library(dplyr)
library(ggplot2)


## RUN CLEANING SCRIPT TO GET DATA
source("vot_cleaning.R")


## PREPARE DATA FOR FIGURES
vot_figs = vot_clean %>%
  mutate(language = factor(language, levels=c("english", "spanish"), labels=c("English", "Spanish"))) %>%
  mutate(context = factor(context, levels=c("ml", "cs"), labels=c("monolingual", "code-switching")))
  

## MAKE FIGURES
# Monolingual versus code-switching
vot.fig = ggplot(vot_figs, aes(x=language, y=duration_ms)) +
  geom_boxplot(aes(fill=context)) +
  #scale_fill_manual(values=c("white", "grey")) +
  ggtitle("VOTs in English and Spanish\nSeparated by Context") +
  xlab("Language") +
  ylab("VOT in milliseconds") +
  guides(fill=guide_legend(title="context")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
vot.fig

# Within code-switching, language x word number
vot_cs.fig = ggplot(subset(vot_figs, context=="code-switching"), aes(x=language, y=duration_ms)) +
  geom_boxplot(aes(fill=word_number)) +
  #scale_fill_manual(values=c("white", "grey")) +
  ggtitle("VOTs in Code-switching English and Spanish\nSeparated by Target Word Number") +
  xlab("Language") +
  ylab("VOT in milliseconds") +
  guides(fill=guide_legend(title="word number")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
vot_cs.fig

# Within English, context x word number
vot_eng.fig = ggplot(subset(vot_figs, language=="English"), aes(x=word_number, y=duration_ms)) +
  geom_boxplot(aes(fill=context)) +
  #scale_fill_manual(values=c("white", "grey")) +
  ggtitle("VOTs in English\nSeparated by Context and Target Word Number") +
  xlab("Context") +
  ylab("VOT in milliseconds") +
  guides(fill=guide_legend(title="word number")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
vot_eng.fig

# Within Spanish, context x word number
vot_sp.fig = ggplot(subset(vot_figs, language=="Spanish"), aes(x=word_number, y=duration_ms)) +
  geom_boxplot(aes(fill=context)) +
  #scale_fill_manual(values=c("white", "grey")) +
  ggtitle("VOTs in Spanish\nSeparated by Context and Target Word Number") +
  xlab("Context") +
  ylab("VOT in milliseconds") +
  guides(fill=guide_legend(title="word number")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
vot_sp.fig

# Monolingual versus code-switching by word number by language
vot_lgxcontxwn.fig = ggplot(vot_figs, aes(x=word_number, y=duration_ms)) +
  geom_boxplot(aes(fill=context)) +
  facet_wrap(~language) +
  scale_fill_manual(values=c("white", "grey")) +
  ggtitle("VOTs in English and Spanish\nby Context and Target Word Number") +
  xlab("Word number") +
  ylab("VOT in milliseconds") +
  guides(fill=guide_legend(title="context")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
  pdf("../../figures/vot_lgxcontxwn.pdf")
vot_lgxcontxwn.fig
  dev.off()
  
# Monolingual versus code-switching by word number by language (log-transform)
vot_lgxcontxwn_log.fig = ggplot(vot_figs, aes(x=word_number, y=log10(duration_ms))) +
    geom_boxplot(aes(fill=context)) +
    facet_wrap(~language) +
    scale_fill_manual(values=c("white", "grey")) +
    ggtitle("VOTs in English and Spanish\nby Context and Target Word Number") +
    xlab("Word number") +
    ylab("VOT in milliseconds\n(log-transformed)") +
    guides(fill=guide_legend(title="context")) +
    theme_bw() +
    theme(text=element_text(size=18), title=element_text(size=18),
          panel.border = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="top", legend.key=element_blank())
pdf("../../figures/vot_lgxcontxwn_log.pdf")
vot_lgxcontxwn_log.fig
dev.off()
