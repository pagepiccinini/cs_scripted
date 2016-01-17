## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(RColorBrewer)


## RUN CLEANING SCRIPT TO GET DATA ####
source("scripts/l_clarity/l_clarity_cleaning.R")


## PREPARE DATA FOR FIGURES ####
lclar_figs = lclar_clean %>%
  mutate(language = factor(language, levels=c("english", "spanish"), labels=c("English", "Spanish"))) %>%
  mutate(context = factor(context, levels=c("ml", "cs"), labels=c("monolingual", "code-switching"))) %>%
  mutate(l_position = factor(l_position, levels=c("onset", "coda"))) %>%
  mutate(word_number = factor(word_number, levels=c("one", "two"), labels=c("one\n(pre-switch)", "two\n(post-switch)"))) %>%
  mutate(context_full =  ifelse(language == "English" & context == "monolingual", "Eng. ML",
                                ifelse(language == "English" & context == "code-switching", "Eng. CS",
                                       ifelse(language == "Spanish" & context == "monolingual", "Sp. ML", "Sp. CS"))))


## SET COLORS ####
cols = brewer.pal(5, "PRGn")
col_eng = cols[5]
col_sp = cols[1]
col_cses = cols[4]  
col_csse = cols[2]


## MAKE FIGURES ####
# Monolingual versus code-switching
lclar.fig = ggplot(lclar_figs, aes(x=language, y=f3_f2)) +
  geom_boxplot(aes(fill=context)) +
  #scale_fill_manual(values=c("white", "grey")) +
  ggtitle("F3 Minus F2 in English and Spanish\nSeparated by Context") +
  xlab("Language") +
  ylab("F3-F2 in Hz\nlight to dark") +
  guides(fill=guide_legend(title="context")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
lclar.fig

# Within code-switching, language x word number
lclar_cs.fig = ggplot(subset(lclar_figs, context=="code-switching"), aes(x=language, y=f3_f2)) +
  geom_boxplot(aes(fill=word_number)) +
  #scale_fill_manual(values=c("white", "grey")) +
  ggtitle("F3 Minus F2 in Code-switching English and Spanish\nSeparated by Target Word Number") +
  xlab("Language") +
  ylab("F3-F2 in Hz\nlight to dark") +
  guides(fill=guide_legend(title="word number")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
lclar_cs.fig

# Within English, context x word number
lclar_eng.fig = ggplot(subset(lclar_figs, language=="English"), aes(x=context, y=f3_f2)) +
  geom_boxplot(aes(fill=word_number)) +
  #scale_fill_manual(values=c("white", "grey")) +
  ggtitle("F3 Minus F2 in English\nSeparated by Context and Target Word Number") +
  xlab("Language") +
  ylab("F3-F2 in Hz\nlight to dark") +
  guides(fill=guide_legend(title="word number")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
lclar_eng.fig

# Within Spanish, context x word number
lclar_sp.fig = ggplot(subset(lclar_figs, language=="Spanish"), aes(x=context, y=f3_f2)) +
  geom_boxplot(aes(fill=word_number)) +
  #scale_fill_manual(values=c("white", "grey")) +
  ggtitle("F3 Minus F2 in Spanish Separated\nby Context and Target Word Number") +
  xlab("Language") +
  ylab("F3-F2 in Hz\nlight to dark") +
  guides(fill=guide_legend(title="word number")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
lclar_sp.fig

# English monolingual versus code-switching by word number by /l/ position
lclar_eng_contxwnxlpos.fig = ggplot(subset(lclar_figs, language=="English"), aes(x=word_number, y=f3_f2)) +
  geom_boxplot(aes(fill=context)) +
  facet_wrap(~l_position) +
  scale_fill_manual(values=c("white", "grey")) +
  ggtitle("F3 minus F2 in English Separated by\nContext, Word Number, and /l/-position") +
  xlab("Word number") +
  ylab("F3-F2 in Hz\nlight to dark") +
  guides(fill=guide_legend(title="context")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
#pdf("../../figures/lclar_eng_contxwnxlpos.pdf")
lclar_eng_contxwnxlpos.fig
#dev.off()

# Spanish monolingual versus code-switching by word number by /l/ position
lclar_sp_contxwnxlpos.fig = ggplot(subset(lclar_figs, language=="Spanish"), aes(x=word_number, y=f3_f2)) +
  geom_boxplot(aes(fill=context)) +
  facet_wrap(~l_position) +
  scale_fill_manual(values=c("white", "grey")) +
  ggtitle("F3 minus F2 in Spanish Separated by\nContext, Word Number, and /l/-position") +
  xlab("Word number") +
  ylab("F3-F2 in Hz\nlight to dark") +
  guides(fill=guide_legend(title="context")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())
#pdf("../../figures/lclar_sp_contxwnxlpos.pdf")
lclar_sp_contxwnxlpos.fig
#dev.off()

# Monolingual versus code-switching by language, word number, and /l/ position
lclar_lgxcontxwnxlpos.fig = ggplot(lclar_figs, aes(x=word_number, y=f3_f2)) +
  geom_boxplot(aes(fill=context)) +
  facet_grid(l_position ~ language) +
  #scale_fill_manual(values=c("white", "grey")) +
  scale_fill_manual(values=c("white", "black")) +
  ggtitle("F3 minus F2 in English and Spanish\nby Context, Target Word Number, and Position") +
  xlab("Word number") +
  ylab("F3-F2 in Hz\nlight to dark") +
  guides(fill=guide_legend(title="context")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank(),
        strip.background = element_rect(color="white", fill="white"))
pdf("../../figures/lclar_lgxcontxwnxlpos.pdf")
lclar_lgxcontxwnxlpos.fig
dev.off()

# Monolingual versus code-switching by language and word number
lclar_lgxcontxwn.fig = ggplot(lclar_figs, aes(x=word_number, y=f3_f2)) +
  geom_boxplot(aes(fill=context_full)) +
  facet_wrap(~ language) +
  aes(fill = language) +
  #scale_fill_manual(values=c("white", "grey")) +
  scale_fill_manual(values=c(col_eng, col_cses, col_sp, col_csse)) +
  ggtitle("F3 minus F2 in English and Spanish\nby Context and Target Word Number") +
  xlab("Word number") +
  ylab("F3-F2 in Hz\nlight to dark") +
  guides(fill=guide_legend(title="context")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank(),
        strip.background = element_rect(color="white", fill="white"))
pdf("../../figures/lclar_lgxcontxwn.pdf")
lclar_lgxcontxwn.fig
dev.off()
