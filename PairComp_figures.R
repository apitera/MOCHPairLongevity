# Making figures for Pair Composition project :) This script will use categories that do not include new adults???
# Use data file PairCompData_RevisedAnalysis.RData for this code
# lmer(J.FIRST.EGG ~ New.Pair + ELEVATION + (1|YEAR), data = all.adults, REML = F, na.action = na.omit)
# Libraries used: --------
library(emmeans)
library(ggforce)
library(ggplot2)
library(ggpubr)



# Functions used------
nDF <- function(x) {
  a <- summary(lsmeans(x, pairwise~New.Pair, mode = "tukey"))
  b <- a$lsmeans
  b
}

sumDF <- function(x) {
  a <-summary(lsmeans(x, pairwise~PAIR.CLASS, mode = "tukey"))
  b <- a$lsmeans
  b
}

fDF <- function(x) {
  a <- summary(lsmeans(x, pairwise~F.AGE, mode = "tukey"))
  b <- a$lsmeans
  b
}

#mDF <- function(x) {
#  a <- summary(lsmeans(x, pairwise~M.AGE, mode = "tukey"))
#  b <- a$lsmeans
#  b
#}


# New v. old ---------
n.jegg   <- lm(J.FIRST.EGG ~ New.Pair + ELEVATION + YEAR, data = all.adults,na.action = na.omit)

n.a <- ggplot(all.adults, aes(x = New.Pair, y = J.FIRST.EGG), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.15, maxwidth = 0.5, na.rm = T, aes(shape = ELEVATION, group = New.Pair), colour = "black", size = 1.5) +
  geom_pointrange(data = nDF(n.jegg), aes(x = New.Pair, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  scale_x_discrete(labels = c('Old pair', 'New pair')) +
  scale_y_continuous(limits = c(130,185), breaks = seq(130,180,10))+
  ylab("First egg date\n(day of year)") +
  xlab("") +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# clutch---
n.clutch <- lm(CLUTCH ~ New.Pair + ELEVATION + YEAR, data = all.adults, na.action = na.omit)

n.b <- ggplot(all.adults, aes(x = New.Pair, y = CLUTCH), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.15, maxwidth = 0.5, na.rm = T, aes(shape = ELEVATION, group = New.Pair), colour = "black", size = 1.5) +
  geom_pointrange(data = nDF(n.clutch), aes(x = New.Pair, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  scale_y_continuous(limits = c(2,10), breaks = seq(2,10,2))+
  scale_x_discrete(labels = c('Old pair', 'New pair')) +
  ylab("\nClutch size") +
  xlab("") +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# brood ---
n.brood  <- lm(BROOD ~ New.Pair + ELEVATION + YEAR, data = all.adults, na.action = na.omit)

n.c <- ggplot(all.adults, aes(x = New.Pair, y = BROOD), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.15, maxwidth = 0.5, na.rm = T, aes(shape = ELEVATION, group = New.Pair), colour = "black", size = 1.5) +
  geom_pointrange(data = nDF(n.brood), aes(x = New.Pair, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  ylab("\nBrood size") +
  xlab("") +
  scale_y_continuous(limits = c(2,9), breaks = seq(1,9,2))+
  scale_x_discrete(labels = c('Old pair', 'New pair')) +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))


# mass ---
n.mass   <- lm(MEANMASS ~ New.Pair + ELEVATION + YEAR, data = all.adults, na.action = na.omit)

n.d <- ggplot(all.adults, aes(x = New.Pair, y = MEANMASS), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.15, maxwidth = 0.5, na.rm = T, aes(shape = ELEVATION, group = New.Pair), colour = "black", size = 1.5) +
  geom_pointrange(data = nDF(n.mass), aes(x = New.Pair, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  ylab("\nMean nestling mass (g)") +
  xlab("") +
  scale_y_continuous(limits = c(10,14), breaks = seq(10,14,1)) +
  scale_x_discrete(labels = c('Old pair', 'New pair')) +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

ggarrange(n.a, n.b, n.c, n.d, nrow = 2, ncol = 2, labels = c("a", "b", "c", "d"), align = "hv")

ggsave(filename = "./Results/RerunStats/Fig1_YrFxd.eps", units = "mm", width = 174, height = 140, device = cairo_ps)

# 3 classes ----
pairs$PAIR.CLASS[which(pairs$PAIR.CLASS == "Adults")]    <- '0 Adults'
pairs$PAIR.CLASS[which(pairs$PAIR.CLASS == "Mixed")]     <- '1 Mixed'
pairs$PAIR.CLASS[which(pairs$PAIR.CLASS == "Juveniles")] <- '2 Juveniles'

# Lay date ---
jegg <- lm(J.FIRST.EGG ~ PAIR.CLASS + ELEVATION + YEAR, data = pairs, na.action = na.omit)

t.a <- ggplot(pairs, aes(x = PAIR.CLASS, y = J.FIRST.EGG), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.15, maxwidth = 0.5, na.rm = T, aes(shape = ELEVATION, group = PAIR.CLASS), colour = "black", size = 1.5) +
  geom_pointrange(data = sumDF(jegg), aes(x = PAIR.CLASS, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  scale_x_discrete(labels = c('Experienced', 'Mixed', 'Inexperienced')) +
  scale_y_continuous(limits = c(130,190), breaks = seq(130,180,10))+
  geom_point(x = 1.5, y = 187, size = 1) +
  geom_text(label = "*", x = 2, y = 190.5, size = 5) +
  geom_segment(aes(x = 0.85, xend = 3.15, y = 190, yend = 190), size = 0.25) +
  geom_segment(aes(x = 0.85, xend = 2.15, y = 185, yend = 185), size = 0.25) +
  ylab("First egg date\n(day of year)") +
  xlab("") +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# clutch---
clutch <- lm(CLUTCH ~ PAIR.CLASS + ELEVATION + YEAR, data = pairs, na.action = na.omit)

t.b <- ggplot(pairs, aes(x = PAIR.CLASS, y = CLUTCH), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.15, maxwidth = 0.5, na.rm = T, aes(shape = ELEVATION, group = PAIR.CLASS), colour = "black", size = 1.5) +
  geom_pointrange(data = sumDF(clutch), aes(x = PAIR.CLASS, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  scale_y_continuous(limits = c(2,10), breaks = seq(2,10,2))+
  scale_x_discrete(labels = c('Experienced', 'Mixed', 'Inexperienced')) +
  ylab("\nClutch size") +
  xlab("") +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# brood---

brood <- lm(BROOD ~ PAIR.CLASS + ELEVATION + YEAR, data = pairs, na.action = na.omit)

t.c <- ggplot(pairs, aes(x = PAIR.CLASS, y = BROOD), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.15, maxwidth = 0.5, na.rm = T, aes(shape = ELEVATION, group = PAIR.CLASS), colour = "black", size = 1.5) +
  geom_pointrange(data = sumDF(brood), aes(x = PAIR.CLASS, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  ylab("\nBrood size") +
  xlab("") +
  scale_y_continuous(limits = c(1,9), breaks = seq(1,9,2))+
  scale_x_discrete(labels = c('Experienced', 'Mixed', 'Inexperienced')) +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# mean mass---
mass <- lm(MEANMASS ~ PAIR.CLASS + ELEVATION + YEAR, data = pairs, na.action = na.omit)

t.d <- ggplot(pairs, aes(x = PAIR.CLASS, y = MEANMASS), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.15, maxwidth = 0.5, na.rm = T, aes(shape = ELEVATION, group = PAIR.CLASS), colour = "black", size = 1.5) +
  geom_pointrange(data = sumDF(mass), aes(x = PAIR.CLASS, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  geom_point(x = 2, y = 15, size = 1) +
  geom_text(label = "**", x = 1.5, y = 14.35, size = 5) +
  geom_segment(aes(x = 0.85, xend = 3.15, y = 14.75, yend = 14.75), size = 0.25) +
  geom_segment(aes(x = 0.85, xend = 2.15, y = 14.25, yend = 14.25), size = 0.25) +
  ylab("\nMean nestling mass (g)") +
  xlab("") +
  scale_y_continuous(limits = c(9,14.9), breaks = seq(9,14,1)) +
  scale_x_discrete(labels = c('Experienced', 'Mixed', 'Inexperienced')) +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

ggarrange(t.a, t.b, t.c, t.d, nrow = 2, ncol = 2, labels = c("a", "b", "c", "d"), align = "hv")

ggsave(filename = "./Results/RerunStats/Fig2_YrFxd.eps", units = "mm", width = 174, height = 140, device = cairo_ps)

# Females -----
# Lay date ---
f.jegg <- lm(J.FIRST.EGG ~ F.AGE + ELEVATION + YEAR, data = pairs, na.action = na.omit)

f_a <- 
  ggplot(pairs, aes(x = F.AGE, y = J.FIRST.EGG), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.15, maxwidth = 0.7, na.rm = T, size = 1.5, aes(shape = ELEVATION, group = F.AGE)) +
  geom_pointrange(data = fDF(f.jegg), aes(x = F.AGE, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), fatten = 2, size = 0.3) +
  scale_shape_manual( values = c(17,2)) +
  geom_text(label = "***", x = 1.5, y = 186, size = 5) +
  geom_segment(aes(x = 0.85, xend = 2.15, y = 185, yend = 185), size = 0.25) +
  ylab("First egg date (day of year)") +
  xlab("") +
  scale_x_discrete(labels = c("Experienced \nfemale", "Inexperienced \nfemale")) +
  scale_y_continuous(limits = c(130,187), breaks = seq(130,180,10))+
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 11), 
        axis.title.x = element_text(vjust = -2, size = 11), 
        axis.title.y = element_text(vjust = 3, size = 12),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# clutch ---

f.clutch <- lm(CLUTCH ~ F.AGE + ELEVATION + YEAR, data = pairs, na.action = na.omit)

f_b <- ggplot(pairs, aes(x = F.AGE, y = CLUTCH), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.15, maxwidth = 0.7, na.rm = T, size = 1.5, aes(shape = ELEVATION, group = F.AGE)) +
  geom_pointrange(data = fDF(f.clutch), aes(x = F.AGE, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), fatten = 2, size = 0.3) +
  scale_shape_manual( values = c(17,2)) +
  geom_text(label = "*", x = 1.5, y = 10.9, size = 5) +
  geom_segment(aes(x = 0.85, xend = 2.15, y = 10.7, yend = 10.7), size = 0.25) +
  scale_y_continuous(limits = c(2,11), breaks = seq(2,10,2)) +
  ylab("Clutch size") +
  xlab("") +
  scale_x_discrete(labels = c('Experienced \nfemale','Inexperienced \nfemale'))+
  theme(plot.margin = margin(0.25,0.5,0.5,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 11), 
        axis.title.x = element_text(vjust = -2, size = 11), 
        axis.title.y = element_text(vjust = 3, size = 12),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

ggarrange(f_a, f_b, nrow = 2, ncol = 1, labels = c("a", "b"), align = "hv")

ggsave(filename = "./Results/RerunStats/Fig3_YrFxd.eps", units = "mm", width = 84, height = 220, device = cairo_ps)

# brood---

#f.brood <- lmer(BROOD ~ F.AGE + ELEVATION + (1|YEAR), data = pairs, REML = F, na.action = na.omit)


#f_c <- 
#ggplot(pairs, aes(x = F.AGE, y = BROOD), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.3, maxwidth = 0.5, na.rm = T, aes(shape = ELEVATION, group = F.AGE), colour = "black", size = 1.5) +
  geom_pointrange(data = fDF(f.brood), aes(x = F.AGE, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  ylab("Brood size") +
  xlab("") +
  scale_y_continuous(limits = c(1,9), breaks = seq(1,9,2)) +
  scale_x_discrete(labels = c('Experienced \nfemale','Inexperienced \nfemale')) +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))


# Mean mass ---
#f.mass <- lmer(MEANMASS ~ F.AGE + ELEVATION + (1|YEAR), data = pairs, REML = F, na.action = na.omit)

# f_d <- 
ggplot(pairs, aes(x = F.AGE, y = MEANMASS), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.3, maxwidth = 0.5, na.rm = T, aes(shape = ELEVATION, group = F.AGE), colour = "black", size = 1.5) +
  geom_pointrange(data = fDF(f.mass), aes(x = F.AGE, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  ylab("Mean nestling mass (g)") +
  xlab("") +
  scale_y_continuous(limits = c(9,14.9), breaks = seq(9,14,1)) +
  scale_x_discrete(labels = c('Experienced \nfemale','Inexperienced \nfemale'))+
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

#ggarrange(One_a, One_b, One_c, One_d, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"), align = "hv")


# Males -------
# Lay date ---
m.jegg <- lm(J.FIRST.EGG ~ M.AGE + ELEVATION + YEAR, data = pairs, na.action = na.omit)

# m.a <- 
ggplot(pairs, aes(x = M.AGE, y = J.FIRST.EGG), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.3, maxwidth = 0.7, na.rm = T, size = 1.5, aes(shape = ELEVATION, group = M.AGE)) +
  geom_pointrange(data = mDF(m.jegg), aes(x = M.AGE, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), size = 0.3) +
  scale_shape_manual( values = c(17,2)) +
  ylab("First egg date (day of year)") +
  xlab("") +
  scale_x_discrete(labels = c("Experienced \nmale", "Inexperienced \nmale")) +
  scale_y_continuous(limits = c(130,187), breaks = seq(130,180,10))+
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 11), 
        axis.title.x = element_text(vjust = -2, size = 11), 
        axis.title.y = element_text(vjust = 3, size = 12),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# Clutch ---

m.clutch <- lmer(CLUTCH ~ M.AGE + ELEVATION + (1|YEAR), data = pairs, REML = F, na.action = na.omit)

# m.b <- 
ggplot(pairs, aes(x = M.AGE, y = CLUTCH), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.3, maxwidth = 0.7, na.rm = T, size = 1.5, aes(shape = ELEVATION, group = M.AGE)) +
  geom_pointrange(data = mDF(m.clutch), aes(x = M.AGE, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), size = 0.3) +
  scale_shape_manual( values = c(17,2)) +
  scale_y_continuous(limits = c(2,11), breaks = seq(2,10,2)) +
  ylab("Clutch size") +
  xlab("") +
  scale_x_discrete(labels = c('Experienced \nmale','Inexperienced \nmale'))+
  theme(plot.margin = margin(0.25,0.5,0.5,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 11), 
        axis.title.x = element_text(vjust = -2, size = 11), 
        axis.title.y = element_text(vjust = 3, size = 12),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))


# Brood ---

m.brood <- lmer(BROOD ~ M.AGE + ELEVATION + (1|YEAR), data = pairs, REML = F, na.action = na.omit)

# m.c <-
ggplot(pairs, aes(x = M.AGE, y = BROOD), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.3, maxwidth = 0.5, na.rm = T, aes(shape = ELEVATION, group = M.AGE), colour = "black", size = 1.5) +
  geom_pointrange(data = mDF(m.brood), aes(x = M.AGE, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  ylab("Brood size") +
  xlab("") +
  scale_y_continuous(limits = c(1,9), breaks = seq(1,9,2)) +
  scale_x_discrete(labels = c('Experienced \nmale','Inexperienced \nmale')) +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# Mean mass --- 
mass <- lm(MEANMASS ~ M.AGE + ELEVATION + YEAR, data = pairs, na.action = na.omit)

# m.d <- 
ggplot(pairs, aes(x = M.AGE, y = MEANMASS), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.3, maxwidth = 0.5, na.rm = T, aes(shape = ELEVATION, group = M.AGE), colour = "black", size = 1.5) +
  geom_pointrange(data = mDF(m.mass), aes(x = M.AGE, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  ylab("Mean nestling mass (g)") +
  xlab("") +
  scale_y_continuous(limits = c(9,14.9), breaks = seq(9,14,1)) +
  scale_x_discrete(labels = c('Experienced \nmale','Inexperienced \nmale'))+
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

#ggarrange(m.a, m.b, m.c, m.d, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"), align = "hv")







# Let's look at between adult-adult pair classes-----
all.adults <- pairs[which(!is.na(pairs$ADULT.CLASSES)),]
all.adults$Ad.Level <- ""
all.adults$Ad.Level[which(all.adults$ADULT.CLASSES == "Old.Pair")] <- "1"
all.adults$Ad.Level[which(all.adults$ADULT.CLASSES == "Not.From.Divorce")] <- "2"
all.adults$Ad.Level[which(all.adults$ADULT.CLASSES == "From.Divorce")] <- "3"

# Mean Mass ---

Three_d <- ggplot(all.adults, aes(x = all.adults$Ad.Level, y = all.adults$MEANMASS), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.3, maxwidth = 0.5, na.rm = T, aes(shape = all.adults$ELEVATION, group = all.adults$Ad.Level), colour = "black", size = 1.5) +
  stat_summary(fun.y = mean, geom = "point", size = 2, na.rm = T) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.5, na.rm = T, ) +
  scale_shape_manual(name = "", values = c(17,2)) +
  ylab("Mean nestling mass (g)") +
  xlab("") +
  scale_x_discrete(labels = c('Old pair', 'New pair:\nmate loss','New pair:\ndivorce')) +
  scale_y_continuous(limits = c(9,14), breaks = seq(9,14,1))+
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 11), 
        axis.title.x = element_text(vjust = -2, size = 11), 
        axis.title.y = element_text(vjust = 3, size = 12),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent")) 


# Brood size ---

Three_c <- ggplot(all.adults, aes(x = all.adults$Ad.Level, y = all.adults$BROOD), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.3, maxwidth = 0.5, na.rm = T, aes(shape = all.adults$ELEVATION, group = all.adults$Ad.Level), colour = "black", size = 1.5) +
  stat_summary(fun.y = mean, geom = "point", size = 2, na.rm = T) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.5, na.rm = T, ) +
  scale_shape_manual(name = "", values = c(17,2)) +
  ylab("Brood size") +
  xlab("") +
  scale_x_discrete(labels = c('Old pair', 'New pair:\nmate loss','New pair:\ndivorce')) +
  scale_y_continuous(limits = c(3,9), breaks = seq(3,9,2))+
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 11), 
        axis.title.x = element_text(vjust = -2, size = 11), 
        axis.title.y = element_text(vjust = 3, size = 12),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))


# Clutch ---
Three_b <- ggplot(all.adults, aes(x = all.adults$Ad.Level, y = all.adults$CLUTCH), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.3, maxwidth = 0.5, na.rm = T, aes(shape = all.adults$ELEVATION, group = all.adults$Ad.Level), colour = "black", size = 1.5) +
  stat_summary(fun.y = mean, geom = "point", size = 2, na.rm = T) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.5, na.rm = T, ) +
  scale_shape_manual(name = "", values = c(17,2)) +
  ylab("Clutch size") +
  xlab("") +
  scale_x_discrete(labels = c('Old pair', 'New pair:\nmate loss','New pair:\ndivorce')) +#  scale_y_continuous(limits = c(3,9), breaks = seq(3,9,1))+
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 11), 
        axis.title.x = element_text(vjust = -2, size = 11), 
        axis.title.y = element_text(vjust = 3, size = 12),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))


# Julian first egg ---

Three_a <- ggplot(all.adults, aes(x = all.adults$Ad.Level, y = all.adults$J.FIRST.EGG), na.rm = T) +
  theme_classic() +
  geom_sina(alpha = 0.3, maxwidth = 0.5, na.rm = T, aes(shape = all.adults$ELEVATION, group = all.adults$Ad.Level), colour = "black", size = 1.5) +
  stat_summary(fun.y = mean, geom = "point", size = 2, na.rm = T) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.5, na.rm = T, ) +
  scale_shape_manual(name = "", values = c(17,2)) +
  geom_point(x = 1.5, y = 14.4, size = 1) +
  geom_text(label = "**", x = 2, y = 14.85, size = 5) +
  geom_segment(aes(x = 0.85, xend = 3.15, y = 14.75, yend = 14.75), size = 0.35) +
  geom_segment(aes(x = 0.85, xend = 2.15, y = 14.25, yend = 14.25), size = 0.35) +
  ylab("Julian first egg date") +
  xlab("") +
  scale_x_discrete(labels = c('Old pair', 'New pair:\nmate loss','New pair:\ndivorce')) +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 5, size = 11), 
        axis.title.x = element_text(vjust = -2, size = 11), 
        axis.title.y = element_text(vjust = 3, size = 12),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# let's make a panel of our experienced pairs figures ---
ggarrange(Three_a, Three_b, Three_c, Three_d, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"), align = "hv")



# w/divorce --------
jegg <- lmer(J.FIRST.EGG ~ ADULT.CLASSES + ELEVATION + (1|YEAR), data = all.adults, REML = F, na.action = na.omit)

#geom_pointrange(data = sumDF(mass.1), aes(x = PAIR.CLASS, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), size = 0.3) +

# clutch ---
clutch <- lmer(CLUTCH ~ ADULT.CLASSES + ELEVATION + (1|YEAR), data = all.adults, REML = F, na.action = na.omit)

# geom_pointrange(data = sumDF(mass.1), aes(x = PAIR.CLASS, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), size = 0.3) +

# brood ---
brood <- lmer(BROOD ~ ADULT.CLASSES + ELEVATION + (1|YEAR), data = all.adults, REML = F, na.action = na.omit)

#geom_pointrange(data = sumDF(mass.1), aes(x = PAIR.CLASS, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), size = 0.3) +

# mass ---
mass <- lmer(MEANMASS ~ ADULT.CLASSES + ELEVATION + (1|YEAR), data = all.adults, REML = F, na.action = na.omit)

#geom_pointrange(data = sumDF(mass.1), aes(x = PAIR.CLASS, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), size = 0.3) +

