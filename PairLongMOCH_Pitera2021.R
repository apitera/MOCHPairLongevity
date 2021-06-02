# This code accompanies analyses for Pitera et al. 2021??? "Reproduction is affected by individual breeding experience but not pair longevity in a socially monogamous bird"

# Overview: ----

# This code the file PairLongMOCH_2016_18.RData 

# We have 5 sections for our analyses: Pair longevity, pt. I, pair longevity, pt. II, pair composition, female experience, & male experience.

# The first 3 use the same four dependent variables as measures of reproductive investment while the last 2 are more sex-specific. 

# All models use elevation and year as independent variables to control for variation across years and between elevation sites (Kozlovsky et al. 2018). Note that these are NOT the main independent variables of interest!

# Pseudocode: ----

# Part I: Analyses 
# i) Subset data to only include experienced pairs for parts 1a & 1b
# 1a) Pair longevity, pt. I
# 1b) Pair longevity, pt. II
# 2)  Pair composition
# 3)  Female experience
# 4)  Male experience

# Part II: Plots/visualization
# 5) Fig. 1 for pair longevity, pt. I
# 6) Fig. 2 for pair composition
# 7) Fig. 3 for female experience

# Libraries used: ------
library(car)
library(emmeans)
library(ggforce)
library(ggplot2)
library(ggpubr)

# Functions used (probably inefficient, all for plots in Pt. II): ----

nDF <- function(x) {
  a <- summary(lsmeans(x, pairwise~NEW.PAIR, mode = "tukey"))
  b <- a$lsmeans
  b
}

sumDF <- function(x) {
  a <-summary(lsmeans(x, pairwise~PAIR.CLASS, mode = "tukey"))
  b <- a$lsmeans
  b
}

fDF <- function(x) {
  a <- summary(lsmeans(x, pairwise~F.EXP, mode = "tukey"))
  b <- a$lsmeans
  b
}

# ======================== Pt. I Analyses =============================
# i) Load in & subset data to only include experienced pairs for parts 1a & 1b: -----

# load in .RData file however you'd like
load("./PairLongMOCH_2016_18.RData")

all.expd <- pairs[which(!is.na(pairs$EXPD.CLASSES)),] #subset for pair longevity analyses (only including pairs that we know have stayed together or are newly formed in a given year)


# 1a) Reproductive investment and pair longevity, pt I: Old vs. new pairs of experienced breeders -------
# Here 'NEW.PAIR' is our only independent variable of interest

# First look at clutch initiation date:
n.jegg <- lm(J.FIRST.EGG ~ NEW.PAIR + ELEVATION + YEAR, data = all.expd)
Anova(n.jegg, type = 3) 

# below not presented in MS:
#summary(n.jegg)$adj.r.squared # pull adjusted r2 value for our model
#summary(lsmeans(n.jegg, pairwise~ELEVATION, mode = "tukey"))
#summary(lsmeans(n.jegg, pairwise~YEAR, mode = "tukey")) 

# Clutch size:
n.clutch <- lm(CLUTCH ~ NEW.PAIR + ELEVATION + YEAR, data = all.expd)
Anova(n.clutch, type = 3) 

# below not presented in MS:
#summary(n.clutch)$adj.r.squared
#summary(lsmeans(n.clutch, pairwise~ELEVATION, mode = "tukey"))

# Brood size:
n.brood <- lm(BROOD ~ NEW.PAIR + ELEVATION + YEAR, data = all.expd)

Anova(n.brood, type = 3)

#below not presented in MS:
#summary(n.brood)$adj.r.squared
#summary(lsmeans(n.brood, pairwise~ELEVATION, mode = "tukey")) 
#summary(lsmeans(n.brood, pairwise~YEAR, mode = "tukey")) # Not of interest here

# Mean nestling mass:
n.mass <- lm(MEAN.MASS ~ NEW.PAIR + ELEVATION + YEAR, data = all.expd)
Anova(n.mass, type = 3) 

#below not presented in MS:
#summary(n.mass)$adj.r.squared 
#summary(lsmeans(n.mass, pairwise~YEAR, mode = "tukey")) # Not of interest here


# 1b) Reproductive investment and pair longevity, pt II: Old vs.new pairs of experienced breeders where new pairs are subset by whether they formed from divorce or mate loss -------

# Clutch initiation date:
d.jegg <- lm(J.FIRST.EGG ~ EXPD.CLASSES + ELEVATION + YEAR, data = all.expd)
Anova(d.jegg, type = 3) 

# below not presented in MS:
#summary(d.jegg)$adj.r.squared
#summary(lsmeans(d.jegg, pairwise~ELEVATION, mode = "tukey"))
#summary(lsmeans(d.jegg, pairwise~YEAR, mode = "tukey")) # Not of interest here

# Clutch size:

d.clutch <- lm(CLUTCH ~ EXPD.CLASSES + ELEVATION + YEAR, data = all.expd)
Anova(d.clutch, type = 3) 

# below not presented in MS:
#summary(d.clutch)$adj.r.squared
#summary(lsmeans(d.clutch, pairwise~ELEVATION, mode = "tukey"))

# Brood size:
d.brood <- lm(BROOD ~ EXPD.CLASSES + ELEVATION + YEAR, data = all.expd) 
Anova(d.brood, type = 3) 

# below not presented in MS:
#summary(d.brood)$adj.r.squared
#summary(lsmeans(d.brood, pairwise~ELEVATION, mode = "tukey"))

# Mean nestling mass:
d.mass <- lm(MEAN.MASS ~ EXPD.CLASSES + ELEVATION + YEAR, data = all.expd)
Anova(d.mass, type = 3) 

# below not presented in MS:
#summary(d.mass)$adj.r.squared
#summary(lsmeans(d.mass, pairwise~ELEVATION, mode = "tukey"))

# 2) Reproductive investment and pair composition: comparing reproductive investment between pair classes of experienced, inexperienced, and mixed-experience breeders -----
# These data are in fig. 2

# Clutch initiation date:
t.jegg <- lm(J.FIRST.EGG ~ PAIR.EXP + ELEVATION + YEAR, data = pairs)

Anova(t.jegg, type = 3)
summary(t.jegg)$adj.r.squared # here our independent variable of interest, 'PAIR.EXP' is significant (alpha = 0.05), so we reported adjusted R^2
summary(lsmeans(t.jegg, pairwise~PAIR.EXP, mode = "tukey")) # see Fig. 2a

# below not presented in MS:
#summary(lsmeans(t.jegg, pairwise~ELEVATION, mode = "tukey"))
#summary(lsmeans(t.jegg, pairwise~YEAR, mode = "tukey"))

# Clutch size: 
t.clutch <- lm(CLUTCH ~ PAIR.EXP + ELEVATION + YEAR, data = pairs)
Anova(t.clutch, type = 3) 

# below not presented in MS:
#summary(t.clutch)$adj.r.squared
#summary(lsmeans(t.clutch, pairwise~ELEVATION, mode = "tukey"))
#summary(lsmeans(t.clutch, pairwise~YEAR, mode = "tukey"))

# Brood size: 
t.brood <- lm(BROOD ~ PAIR.EXP + ELEVATION + YEAR, data = pairs)
Anova(t.brood, type = 3)

# below not presented in MS:
#summary(t.brood)$adj.r.squared
#summary(lsmeans(t.brood, pairwise~ELEVATION, mode = "tukey"))
#summary(lsmeans(t.brood, pairwise~YEAR, mode = "tukey"))

# Mean nestling mass:
t.mass <- lm(MEAN.MASS ~ PAIR.EXP + ELEVATION + YEAR, data = pairs)

Anova(t.mass, type = 3)
summary(t.mass)$adj.r.squared
summary(lsmeans(t.mass, pairwise~PAIR.EXP, mode = "tukey"))

# below not presented in MS:
#summary(lsmeans(t.mass, pairwise~ELEVATION, mode = "tukey"))
#summary(lsmeans(t.mass, pairwise~YEAR, mode = "tukey"))

# 3) Reproductive investment and female breeding experience: comparing experienced and inexperienced females----

# ** Here we are only looking at 2 of our reproductive metrics: clutch initiation and clutch size as we considered these to have the potential to be driven by females (egg laying is part of female biology) and could result in detectable differences based on female breeding experience

# Clutch initiation date:
f.jegg <- lm(J.FIRST.EGG ~ F.EXP + ELEVATION + YEAR, data = pairs)

Anova(f.jegg, type = 3)
summary(f.jegg)$adj.r.squared

# Clutch size:

f.clutch <- lm(CLUTCH ~ F.EXP + ELEVATION + YEAR, data = pairs)

Anova(f.clutch, type = 3) 
summary(f.clutch)$adj.r.squared

# 4) Reproductive investment and male reproductive experience: comparing experienced and inexperienced males ----

# ** Here we only looked at 1 of our reproductive metrics: mean nestling mass as previous work (Grundel 1984) suggests that males, but not females, may adjust nestling provisioning in this species, suggesting that this metric may be a more relevant measure of male reproductive investment. Furthermore, we may expect there to be little variation across females in this behavior, regardless of experience. 

# Mean nestling mass:
m.mass <- lm(MEAN.MASS ~ M.EXP + ELEVATION + YEAR, data = pairs)

Anova(m.mass, type = 3) 

# below not presented in MS:
#summary(m.mass)$adj.r.squared

















# ======================== Pt. II Visualization with sina plots!=================-------

# *** These use linear models above to draw points and lines, code will break if they are not in the global environment!

# 5) Fig. 1 for pair longevity, pt. I ----

# Initiation of egg laying:
n.a <- 
ggplot(all.expd, aes(x = NEW.PAIR, y = J.FIRST.EGG), na.rm = T) +
  geom_sina(aes(shape = ELEVATION, group = NEW.PAIR),
            alpha = 0.15, maxwidth = 0.5, colour = "black", size = 1.5,  na.rm = T) +
  geom_pointrange(data = nDF(n.jegg), 
                  aes(x = NEW.PAIR, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), 
                  fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  scale_x_discrete(labels = c('Old pair', 'New pair')) +
  scale_y_continuous(limits = c(130,185), breaks = seq(130,180,10))+
  ylab("First egg date\n(day of year)") +
  xlab("") +
  theme_classic() +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title   = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x  = element_text(color = "black", size = 10),
        axis.text.y  = element_text(color = "black", size = 10),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# Clutch size 

n.b <- 
ggplot(all.expd, aes(x = NEW.PAIR, y = CLUTCH), na.rm = T) +
  theme_classic() +
  geom_sina(aes(shape = ELEVATION, group = NEW.PAIR), 
            colour = "black", size = 1.5, alpha = 0.15, maxwidth = 0.5, na.rm = T) +
  geom_pointrange(data = nDF(n.clutch), 
                  aes(x = NEW.PAIR, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE),
                  fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  scale_y_continuous(limits = c(2,10), breaks = seq(2,10,2))+
  scale_x_discrete(labels = c('Old pair', 'New pair')) +
  ylab("\nClutch size") +
  xlab("") +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title   = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x  = element_text(color = "black", size = 10),
        axis.text.y  = element_text(color = "black", size = 10),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# Brood size--

n.c <- 
ggplot(all.expd, aes(x = NEW.PAIR, y = BROOD), na.rm = T) +
  geom_sina(aes(shape = ELEVATION, group = NEW.PAIR),
            alpha = 0.15, maxwidth = 0.5, colour = "black", size = 1.5, na.rm = T) +
  geom_pointrange(data = nDF(n.brood), 
                  aes(x = NEW.PAIR, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE),
                  fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  scale_y_continuous(limits = c(2,9), breaks = seq(1,9,2))+
  scale_x_discrete(labels = c('Old pair', 'New pair')) +
  ylab("\nBrood size") +
  xlab("") +
  theme_classic() +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title   = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x  = element_text(color = "black", size = 10),
        axis.text.y  = element_text(color = "black", size = 10),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))


# Mean nestling mass ---

n.d <- 
ggplot(all.expd, aes(x = NEW.PAIR, y = MEAN.MASS), na.rm = T) +
  geom_sina(aes(shape = ELEVATION, group = NEW.PAIR), 
            alpha = 0.15, maxwidth = 0.5, colour = "black", size = 1.5, na.rm = T) +
  geom_pointrange(data = nDF(n.mass), 
                  aes(x = NEW.PAIR, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE),
                  fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  scale_y_continuous(limits = c(10,14), breaks = seq(10,14,1)) +
  scale_x_discrete(labels = c('Old pair', 'New pair')) +
  ylab("\nMean nestling mass (g)") +
  xlab("") +
  theme_classic() +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title   = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x  = element_text(color = "black", size = 10),
        axis.text.y  = element_text(color = "black", size = 10),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

ggarrange(n.a, n.b, n.c, n.d, nrow = 2, ncol = 2, labels = c("a", "b", "c", "d"), align = "hv")

# 6) Fig. 2 for pair composition --------

# I'd like our pair types to show up in the following order: "Experieced", "Mixed", "Inexperienced" on our figure.
# R/ggplot2 default would put "Mixed" last, so we need to reorder these.
# There are many solutions to the same problem and gosh darn this is just the fastest solution for me  
# I apologize if you find this cringeworthy
pairs$PAIR.CLASS <- ""
pairs$PAIR.CLASS[which(pairs$PAIR.EXP == "Experienced")]    <- '0 Experienced'
pairs$PAIR.CLASS[which(pairs$PAIR.EXP == "Mixed")]     <- '1 Mixed'
pairs$PAIR.CLASS[which(pairs$PAIR.EXP == "Inexperienced")] <- '2 Inexperienced'

# Clutch initiation date ---
jegg <- lm(J.FIRST.EGG ~ PAIR.CLASS + ELEVATION + YEAR, data = pairs, na.action = na.omit)

t.a <- 
ggplot(pairs, aes(x = PAIR.CLASS, y = J.FIRST.EGG), na.rm = T) +
  geom_sina(aes(shape = ELEVATION, group = PAIR.CLASS), 
            alpha = 0.15, maxwidth = 0.5, na.rm = T, colour = "black", size = 1.5) +
  geom_pointrange(data = sumDF(jegg), 
                  aes(x = PAIR.CLASS, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), 
                  fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  geom_point(x = 1.5, y = 187, size = 1) +
  geom_text(label = "*", x = 2, y = 190.5, size = 5) +
  geom_segment(aes(x = 0.85, xend = 3.15, y = 190, yend = 190), size = 0.25) + # L354-357 significance marks & lines
  geom_segment(aes(x = 0.85, xend = 2.15, y = 185, yend = 185), size = 0.25) +
  scale_x_discrete(labels = c('Experienced', 'Mixed', 'Inexperienced')) +
  scale_y_continuous(limits = c(130,190), breaks = seq(130,180,10))+
  ylab("First egg date\n(day of year)") +
  xlab("") +
  theme_classic() +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title   = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x  = element_text(color = "black", size = 10),
        axis.text.y  = element_text(color = "black", size = 10),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# Clutch size---
clutch <- lm(CLUTCH ~ PAIR.CLASS + ELEVATION + YEAR, data = pairs, na.action = na.omit)

t.b <- 
ggplot(pairs, aes(x = PAIR.CLASS, y = CLUTCH), na.rm = T) +
  geom_sina(aes(shape = ELEVATION, group = PAIR.CLASS),
            alpha = 0.15, maxwidth = 0.5, colour = "black", size = 1.5, na.rm = T) +
  geom_pointrange(data = sumDF(clutch), 
                  aes(x = PAIR.CLASS, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), 
                  fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  scale_y_continuous(limits = c(2,10), breaks = seq(2,10,2))+
  scale_x_discrete(labels = c('Experienced', 'Mixed', 'Inexperienced')) +
  ylab("\nClutch size") +
  xlab("") +
  theme_classic() +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title   = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x  = element_text(color = "black", size = 10),
        axis.text.y  = element_text(color = "black", size = 10),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# Brood size---

brood <- lm(BROOD ~ PAIR.CLASS + ELEVATION + YEAR, data = pairs, na.action = na.omit)

t.c <- 
ggplot(pairs, aes(x = PAIR.CLASS, y = BROOD), na.rm = T) +
  geom_sina(aes(shape = ELEVATION, group = PAIR.CLASS), 
            alpha = 0.15, maxwidth = 0.5, colour = "black", size = 1.5, na.rm = T) +
  geom_pointrange(data = sumDF(brood), 
                  aes(x = PAIR.CLASS, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), 
                  fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  scale_y_continuous(limits = c(1,9), breaks = seq(1,9,2))+
  scale_x_discrete(labels = c('Experienced', 'Mixed', 'Inexperienced')) +
  ylab("\nBrood size") +
  xlab("") +
  theme_classic() +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title   = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x  = element_text(color = "black", size = 10),
        axis.text.y  = element_text(color = "black", size = 10),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# Mean nestling mass---
mass <- lm(MEAN.MASS ~ PAIR.CLASS + ELEVATION + YEAR, data = pairs, na.action = na.omit)

t.d <- 
ggplot(pairs, aes(x = PAIR.CLASS, y = MEAN.MASS), na.rm = T) +
  geom_sina(aes(shape = ELEVATION, group = PAIR.CLASS), 
            alpha = 0.15, maxwidth = 0.5, colour = "black", size = 1.5, na.rm = T) +
  geom_pointrange(data = sumDF(mass), 
                  aes(x = PAIR.CLASS, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), 
                  fatten = 2, size = 0.3) +
  scale_shape_manual(name = "", values = c(17,2)) +
  geom_point(x = 2, y = 15, size = 1) +
  geom_text(label = "**", x = 1.5, y = 14.35, size = 5) +
  geom_segment(aes(x = 0.85, xend = 3.15, y = 14.75, yend = 14.75), size = 0.25) +
  geom_segment(aes(x = 0.85, xend = 2.15, y = 14.25, yend = 14.25), size = 0.25) +
  scale_y_continuous(limits = c(9,14.9), breaks = seq(9,14,1)) +
  scale_x_discrete(labels = c('Experienced', 'Mixed', 'Inexperienced')) +
  ylab("\nMean nestling mass (g)") +
  xlab("") +
  theme_classic() +
  theme(plot.margin = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title   = element_text(hjust = 0.5, vjust = 5, size = 10), 
        axis.title.x = element_text(vjust = -2, size = 10), 
        axis.title.y = element_text(vjust = 3, size = 11),
        axis.text.x  = element_text(color = "black", size = 10),
        axis.text.y  = element_text(color = "black", size = 10),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

ggarrange(t.a, t.b, t.c, t.d, nrow = 2, ncol = 2, labels = c("a", "b", "c", "d"), align = "hv") # Makes a nice panel


# 7) Fig. 3 for female experience ---------

# Clutch initiation date:
f_a <- 
ggplot(pairs, aes(x = F.EXP, y = J.FIRST.EGG), na.rm = T) +
  geom_sina(aes(shape = ELEVATION, group = F.EXP),
             alpha = 0.15, maxwidth = 0.7, size = 1.5, na.rm = T) +
  geom_pointrange(data = fDF(f.jegg), 
                  aes(x = F.EXP, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE),
                  fatten = 2, size = 0.3) +
  scale_shape_manual( values = c(17,2)) +
  geom_text(label = "***", x = 1.5, y = 186, size = 5) +
  geom_segment(aes(x = 0.85, xend = 2.15, y = 185, yend = 185), size = 0.25) +
  scale_x_discrete(labels = c("Experienced \nfemale", "Inexperienced \nfemale")) +
  scale_y_continuous(limits = c(130,187), breaks = seq(130,180,10)) +
  ylab("First egg date (day of year)") +
  xlab("") +
  theme_classic() +
  theme(plot.margin     = margin(0.5,0.5,0.25,0.5,"cm"), 
        legend.position = "none",
        plot.title   = element_text(hjust = 0.5, vjust = 5, size = 11), 
        axis.title.x = element_text(vjust = -2, size = 11), 
        axis.title.y = element_text(vjust = 3, size = 12),
        axis.text.x  = element_text(color = "black", size = 11),
        axis.text.y  = element_text(color = "black", size = 11),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# Clutch size ---

f_b <- 
ggplot(pairs, aes(x = F.EXP, y = CLUTCH), na.rm = T) +
  geom_sina(aes(shape = ELEVATION, group = F.EXP),
            alpha = 0.15, maxwidth = 0.7, size = 1.5, na.rm = T) +
  geom_pointrange(data = fDF(f.clutch), 
                  aes(x = F.EXP, y = lsmean, ymax = lsmean + SE, ymin = lsmean - SE), 
                  fatten = 2, size = 0.3) +
  scale_shape_manual( values = c(17,2)) +
  geom_text(label = "*", x = 1.5, y = 10.9, size = 5) +
  geom_segment(aes(x = 0.85, xend = 2.15, y = 10.7, yend = 10.7), size = 0.25) +
  scale_y_continuous(limits = c(2,11), breaks = seq(2,10,2)) +
  scale_x_discrete(labels = c('Experienced \nfemale','Inexperienced \nfemale')) +
  ylab("Clutch size") +
  xlab("") +
  theme_classic() +
  theme(plot.margin = margin(0.25,0.5,0.5,0.5,"cm"), 
        legend.position = "none",
        plot.title   = element_text(hjust = 0.5, vjust = 5, size = 11), 
        axis.title.x = element_text(vjust = -2, size = 11), 
        axis.title.y = element_text(vjust = 3, size = 12),
        axis.text.x  = element_text(color = "black", size = 11),
        axis.text.y  = element_text(color = "black", size = 11),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

ggarrange(f_a, f_b, nrow = 2, ncol = 1, labels = c("a", "b"), align = "hv")

