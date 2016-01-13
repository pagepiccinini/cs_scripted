## LOAD PACKAGES ####
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)


## MAKE FAKE DATA ####
eng_ml = data.frame(time = seq(1, 100)) %>%
  mutate(English = rnorm(100, 90, 2)) %>%
  mutate(Spanish = rnorm(100, 10, 2)) %>%
  gather(language, activation, c(English, Spanish))

sp_ml = data.frame(time = seq(1, 100)) %>%
  mutate(Spanish = rnorm(100, 90, 2)) %>%
  mutate(English = rnorm(100, 10, 2)) %>%
  gather(language, activation, c(English, Spanish))

cses_dom = data.frame(time = seq(1, 100)) %>%
  mutate(English = c(rep(90, 40), seq(90, 10, length.out = 20), rep(10, 40)) + rnorm(100, 0, 2)) %>%
  mutate(Spanish = c(rep(10, 25), seq(10, 90, length.out = 20), rep(90, 55)) + rnorm(100, 0, 2)) %>%
  gather(language, activation, c(English, Spanish))

cses_l1 = data.frame(time = seq(1, 100)) %>%
  mutate(English = c(rep(90, 40), seq(90, 10, length.out = 20), rep(10, 40)) + rnorm(100, 0, 2)) %>%
  mutate(Spanish = c(rep(10, 25), seq(10, 90, length.out = 40), rep(90, 35)) + rnorm(100, 0, 2)) %>%
  gather(language, activation, c(English, Spanish))

cses_sudden = data.frame(time = seq(1, 100)) %>%
  mutate(English = c(rep(90, 50), seq(90, 10, length.out = 0), rep(10, 50)) + rnorm(100, 0, 2)) %>%
  mutate(Spanish = c(rep(10, 50), seq(10, 90, length.out = 0), rep(90, 50)) + rnorm(100, 0, 2)) %>%
  gather(language, activation, c(English, Spanish))

cses_equal = data.frame(time = seq(1, 100)) %>%
  mutate(English = c(rep(90, 40), seq(90, 10, length.out = 20), rep(10, 40)) + rnorm(100, 0, 2)) %>%
  mutate(Spanish = c(rep(10, 40), seq(10, 90, length.out = 20), rep(90, 40)) + rnorm(100, 0, 2)) %>%
  gather(language, activation, c(English, Spanish))

csse_dom = data.frame(time = seq(1, 100)) %>%
  mutate(Spanish = c(rep(90, 40), seq(90, 10, length.out = 20), rep(10, 40)) + rnorm(100, 0, 2)) %>%
  mutate(English = c(rep(10, 25), seq(10, 90, length.out = 40), rep(90, 35)) + rnorm(100, 0, 2)) %>%
  gather(language, activation, c(English, Spanish))

csse_l1 = data.frame(time = seq(1, 100)) %>%
  mutate(Spanish = c(rep(90, 40), seq(90, 10, length.out = 20), rep(10, 40)) + rnorm(100, 0, 2)) %>%
  mutate(English = c(rep(10, 25), seq(10, 90, length.out = 20), rep(90, 55)) + rnorm(100, 0, 2)) %>%
  gather(language, activation, c(English, Spanish))

csse_equal = data.frame(time = seq(1, 100)) %>%
  mutate(Spanish = c(rep(90, 40), seq(90, 10, length.out = 20), rep(10, 40)) + rnorm(100, 0, 2)) %>%
  mutate(English = c(rep(10, 40), seq(10, 90, length.out = 20), rep(90, 40)) + rnorm(100, 0, 2)) %>%
  gather(language, activation, c(English, Spanish))


## MAKE COLORS FOR FIGURES ####
colors = brewer.pal(5, "PRGn")
col_eng = colors[5]
col_sp = colors[1]


## MAKE PLOTS ####
# English monolingual utterance
eng_act.plot = ggplot(eng_ml, aes(x = time, y = activation, color=language)) +
  geom_line(lwd=2) +
  #geom_area(fill=col_eng) +
  xlim(0, 100) + 
  ylim(0, 100) +
  #scale_color_manual(values = c("black", "darkgrey")) +
  scale_color_manual(values = c(col_eng, col_sp)) +
  #ggtitle("Language Activation During\nMonolingual English Utterance") +
  #xlab("The woman is very tired") +
  #ylab("Amount of activation") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pdf("figures/eng_act.pdf")
eng_act.plot
dev.off()

# Spanish monolingual utterance
sp_act.plot = ggplot(sp_ml, aes(x = time, y = activation, color=language)) +
  geom_line(lwd=2) +
  #geom_area(fill=col_eng) +
  xlim(0, 100) + 
  ylim(0, 100) +
  #scale_color_manual(values = c("black", "darkgrey")) +
  scale_color_manual(values = c(col_eng, col_sp)) +
  #ggtitle("Language Activation During\nMonolingual Spanish Utterance") +
  #xlab(expression(paste(italic("La mujer está muy cansada")))) +
  #ylab("Amount of activation") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pdf("figures/sp_act.pdf")
sp_act.plot
dev.off()

# Code-switching English to Spanish utterance
cses_act.plot = ggplot(cses, aes(x = time, y = activation, color=language)) +
  geom_line(lwd=2) +
  #geom_area(fill=col_eng) +
  geom_vline(x = 50, lwd=1.5) +
  xlim(0, 100) + 
  ylim(0, 100) +
  #scale_color_manual(values = c("black", "darkgrey")) +
  scale_color_manual(values = c(col_eng, col_sp)) +
  #ggtitle("Language Activation During\nCode-switching English to Spanish Utterance") +
  #xlab(expression(paste("The woman is |", italic(" muy cansada")))) +
  #ylab("Amount of activation") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pdf("figures/cses_act.pdf")
cses_act.plot
dev.off()

# Code-switching English to Spanish utterance (sudden H0)
cses_act_sudden.plot = ggplot(cses_sudden, aes(x = time, y = activation, color=language)) +
  geom_line(lwd=2) +
  #geom_area(fill=col_eng) +
  geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color="black", lwd=1.5) +
  xlim(0, 100) + 
  ylim(0, 100) +
  #scale_color_manual(values = c("black", "darkgrey")) +
  scale_color_manual(values = c(col_eng, col_sp)) +
  #ggtitle("Language Activation During\nCode-switching English to Spanish Utterance") +
  #xlab(expression(paste("The woman is |", italic(" muy cansada")))) +
  #ylab("Amount of activation") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pdf("figures/cses_act_sudden.pdf")
cses_act_sudden.plot
dev.off()

# Code-switching English to Spanish utterance (equal H1)
cses_act_equal.plot = ggplot(cses_equal, aes(x = time, y = activation, color=language)) +
  geom_line(lwd=2) +
  #geom_area(fill=col_eng) +
  geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color="black", lwd=1.5) +
  xlim(0, 100) + 
  ylim(0, 100) +
  #scale_color_manual(values = c("black", "darkgrey")) +
  scale_color_manual(values = c(col_eng, col_sp)) +
  #ggtitle("Language Activation During\nCode-switching English to Spanish Utterance") +
  #xlab(expression(paste("The woman is |", italic(" muy cansada")))) +
  #ylab("Amount of activation") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pdf("figures/cses_act_equal.pdf")
cses_act_equal.plot
dev.off()

# Code-switching English to Spanish utterance (l1 H2-0)
cses_act_l1.plot = ggplot(cses_l1, aes(x = time, y = activation, color=language)) +
  geom_line(lwd=2) +
  #geom_area(fill=col_eng) +
  geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color="black", lwd=1.5) +
  xlim(0, 100) + 
  ylim(0, 100) +
  #scale_color_manual(values = c("black", "darkgrey")) +
  scale_color_manual(values = c(col_eng, col_sp)) +
  #ggtitle("Language Activation During\nCode-switching English to Spanish Utterance") +
  #xlab(expression(paste("The woman is |", italic(" muy cansada")))) +
  #ylab("Amount of activation") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pdf("figures/cses_act_l1.pdf")
cses_act_l1.plot
dev.off()

# Code-switching English to Spanish utterance (dom H2-1)
cses_act_dom.plot = ggplot(cses_dom, aes(x = time, y = activation, color=language)) +
  geom_line(lwd=2) +
  #geom_area(fill=col_eng) +
  geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color="black", lwd=1.5) +
  xlim(0, 100) + 
  ylim(0, 100) +
  #scale_color_manual(values = c("black", "darkgrey")) +
  scale_color_manual(values = c(col_eng, col_sp)) +
  #ggtitle("Language Activation During\nCode-switching English to Spanish Utterance") +
  #xlab(expression(paste("The woman is |", italic(" muy cansada")))) +
  #ylab("Amount of activation") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pdf("figures/cses_act_dom.pdf")
cses_act_dom.plot
dev.off()


# Code-switching Spanish to English utterance
csse_act.plot = ggplot(csse, aes(x = time, y = activation, color=language)) +
  geom_line(lwd=2) +
  #geom_area(fill=col_eng) +
  geom_vline(x = 50, lwd=1.5) +
  xlim(0, 100) + 
  ylim(0, 100) +
  #scale_color_manual(values = c("black", "darkgrey")) +
  scale_color_manual(values = c(col_eng, col_sp)) +
  ggtitle("Language Activation During\nCode-switching Spanish to English Utterance") +
  xlab(expression(paste(italic("La mujer está "), "| very sad        "))) +
  ylab("Amount of activation") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pdf("figures/csse_act.pdf")
csse_act.plot
dev.off()

# Code-switching Spanish to English utterance (equal H1)
csse_act_equal.plot = ggplot(csse_equal, aes(x = time, y = activation, color=language)) +
  geom_line(lwd=2) +
  #geom_area(fill=col_eng) +
  geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color="black", lwd=1.5) +
  xlim(0, 100) + 
  ylim(0, 100) +
  #scale_color_manual(values = c("black", "darkgrey")) +
  scale_color_manual(values = c(col_eng, col_sp)) +
  #ggtitle("Language Activation During\nCode-switching English to Spanish Utterance") +
  #xlab(expression(paste("The woman is |", italic(" muy cansada")))) +
  #ylab("Amount of activation") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pdf("figures/csse_act_equal.pdf")
csse_act_equal.plot
dev.off()

# Code-switching Spanish to English utterance (l1 H2-0)
csse_act_l1.plot = ggplot(csse_l1, aes(x = time, y = activation, color=language)) +
  geom_line(lwd=2) +
  #geom_area(fill=col_eng) +
  geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color="black", lwd=1.5) +
  xlim(0, 100) + 
  ylim(0, 100) +
  #scale_color_manual(values = c("black", "darkgrey")) +
  scale_color_manual(values = c(col_eng, col_sp)) +
  #ggtitle("Language Activation During\nCode-switching English to Spanish Utterance") +
  #xlab(expression(paste("The woman is |", italic(" muy cansada")))) +
  #ylab("Amount of activation") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pdf("figures/csse_act_l1.pdf")
csse_act_l1.plot
dev.off()

# Code-switching Spanish to English utterance (dom H2-1)
csse_act_dom.plot = ggplot(csse_dom, aes(x = time, y = activation, color=language)) +
  geom_line(lwd=2) +
  #geom_area(fill=col_eng) +
  geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color="black", lwd=1.5) +
  xlim(0, 100) + 
  ylim(0, 100) +
  #scale_color_manual(values = c("black", "darkgrey")) +
  scale_color_manual(values = c(col_eng, col_sp)) +
  #ggtitle("Language Activation During\nCode-switching English to Spanish Utterance") +
  #xlab(expression(paste("The woman is |", italic(" muy cansada")))) +
  #ylab("Amount of activation") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none", legend.key=element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

pdf("figures/csse_act_dom.pdf")
csse_act_dom.plot
dev.off()

