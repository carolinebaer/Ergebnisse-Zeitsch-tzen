##### Auswertung Experiment 8 (Drahtermuedung) #################################

## Pakete laden:
library(agricolae)
library(xtable)
library(readxl)
library(ggplot2)
library(patchwork)

##### VERSUCHSPLAN #####========================================================
## Erstellung des Versuchsplans:
plan <- design.ab(4, trt = c(2,3,2), seed = 12322)
names(plan$book) <- c("nummer", "block", "Drahtsorte", "Richtung", "Haendigkeit")
plan$book

plan$book$Drahtsorte[which(plan$book$Drahtsorte == "1")] <- "rot"
plan$book$Drahtsorte[which(plan$book$Drahtsorte == "2")] <- "silber"
plan$book$Richtung[which(plan$book$Richtung == "1")] <- "mo-mu"
plan$book$Richtung[which(plan$book$Richtung == "2")] <- "lo-ru"
plan$book$Richtung[which(plan$book$Richtung == "3")] <- "ro-lu"
plan$book$Haendigkeit[which(plan$book$Haendigkeit == "1")] <- "rechts"
plan$book$Haendigkeit[which(plan$book$Haendigkeit == "2")] <- "links"

plan$book


##### DATENSATZ #####===========================================================

## Laden des Datensatzes:
erg <- read_excel("Versuchsplan - Exp. 8 Drahtermüdung.xlsx")

erg$Tag <- as.factor(erg$Tag)
erg$Drahtsorte  <- as.factor(erg$Drahtsorte)
erg$Biegerichtung <- as.factor(erg$Biegerichtung)
erg$Händigkeit <- as.factor(erg$Händigkeit)
names(erg) <- c("Nummer", "Tag", "Block", "Drahtsorte", "Biegerichtung", 
                "Haendigkeit", "Biegungsanzahl", "Vorkommnisse")


## Erstellung der Unterdatensaetze:
## --> nach Tag, Drahtsorte, Biegerichtung, Haendigkeit

tag_eins <- erg[1:24, ]
tag_zwei <- erg[25:48, ]

draht_rot <- subset(erg, erg$Drahtsorte == "rot")
draht_silber <- subset(erg, erg$Drahtsorte == "silber")

biegericht_mo_mu <- subset(erg, erg$Biegerichtung == "mo-mu")
biegericht_lo_ru <- subset(erg, erg$Biegerichtung == "lo-ru")
biegericht_ro_lu <- subset(erg, erg$Biegerichtung == "ro-lu")

hand_links <- subset(erg, erg$Haendigkeit == "links")
hand_rechts <- subset(erg, erg$Haendigkeit == "rechts")


##### BOXPLOTS #####============================================================

# insg - Drahtsorte
# insg - Biegerichtung
# insg - Haendigkeit
# Tag  - Drahtsorte
# Tag - Biegerichtung
# (Tag - Haendigkeit)
# Draht - Biegerichtung
# Haendigkeit - Biegerichtung
# Draht - Haendigkeit



# insgesamt - Drahtsorte
ggplot(erg, aes(Drahtsorte, Biegungsanzahl)) +
  geom_boxplot(aes(group = Drahtsorte), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3)


# insgesamt - Biegerichtung
ggplot(erg, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3)


# insgesamt - Haendigkeit
ggplot(erg, aes(Haendigkeit, Biegungsanzahl)) +
  geom_boxplot(aes(group = Haendigkeit), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  xlab("Händigkeit")


# Tag - Drahtsorte
ggplot(tag_eins, aes(Drahtsorte, Biegungsanzahl)) +
  geom_boxplot(aes(group = Drahtsorte), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) + 
  ylim(0, 40) +
  ggtitle("Tag 1") +
ggplot(tag_zwei, aes(Drahtsorte, Biegungsanzahl)) +
  geom_boxplot(aes(group = Drahtsorte), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ylim(0, 40) +
  ggtitle("Tag 2")


# Tag - Biegerichtung
ggplot(tag_eins, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ylim(0, 40) +
  ggtitle("Tag 1") +
ggplot(tag_zwei, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ylim(0, 40) +
  ggtitle("Tag 2")


# Tag - Haendigkeit
ggplot(tag_eins, aes(Haendigkeit, Biegungsanzahl)) +
  geom_boxplot(aes(group = Haendigkeit), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ylim(0, 40) +
  ggtitle("Tag 1") +
ggplot(tag_zwei, aes(Haendigkeit, Biegungsanzahl)) +
  geom_boxplot(aes(group = Haendigkeit), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ylim(0, 40) +
  ggtitle("Tag 2")


# Draht - Biegerichtung
ggplot(draht_rot, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ylim(0, 40) +
  ggtitle("roter Draht") +
  ggplot(draht_silber, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ylim(0, 40) +
  ggtitle("silberner Draht")


# Haendigkeit - Biegerichtung
ggplot(hand_links, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ylim(0, 40) +
  ggtitle("Linkshändigkeit") +
ggplot(hand_rechts, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ylim(0, 40) +
  ggtitle("Rechtshändigkeit")


# Draht - Haendigkeit
ggplot(draht_rot, aes(Haendigkeit, Biegungsanzahl)) +
  geom_boxplot(aes(group = Haendigkeit), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ylim(0, 40) +
  ggtitle("roter Draht") +
  ggplot(draht_silber, aes(Haendigkeit, Biegungsanzahl)) +
  geom_boxplot(aes(group = Haendigkeit), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 40), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ylim(0, 40) +
  ggtitle("silberner Draht")



##### VORTESTS - SHAPIRO-WILK #####=============================================

# ab eine Stichprobengroesse von 3 moeglich 


### roter Draht: ___________
##  Linkshaenderin: -----

# mo-mu
rot_links_mo_mu <- subset(draht_rot, draht_rot$Biegerichtung == "mo-mu" 
                          & draht_rot$Haendigkeit == "links")
shapiro.test(rot_links_mo_mu$Biegungsanzahl)
# Shapiro-Wilk normality test
# 
# data:  rot_links_mo_mu$Biegungsanzahl
# W = 0.89495, p-value = 0.4064


# lo-ru
rot_links_lo_ru <- subset(draht_rot, draht_rot$Biegerichtung == "lo-ru" 
                          & draht_rot$Haendigkeit == "links")
shapiro.test(rot_links_lo_ru$Biegungsanzahl)
# Shapiro-Wilk normality test
# 
# data:  rot_links_lo_ru$Biegungsanzahl
# W = 0.99291, p-value = 0.9719


# ro-lu
rot_links_ro_lu <- subset(draht_rot, draht_rot$Biegerichtung == "ro-lu" 
                          & draht_rot$Haendigkeit == "links")
shapiro.test(rot_links_ro_lu$Biegungsanzahl)
# Shapiro-Wilk normality test
# 
# data:  rot_links_ro_lu$Biegungsanzahl
# W = 0.80056, p-value = 0.1032



## Rechtshaenderin: -----

# mo-mu
rot_rechts_mo_mu <- subset(draht_rot, draht_rot$Biegerichtung == "mo-mu" 
                           & draht_rot$Haendigkeit == "rechts")
shapiro.test(rot_rechts_mo_mu$Biegungsanzahl)
# Shapiro-Wilk normality test
# 
# data:  rot_rechts_mo_mu$Biegungsanzahl
# W = 0.86337, p-value = 0.2725


# lo-ru
rot_rechts_lo_ru <- subset(draht_rot, draht_rot$Biegerichtung == "lo-ru" 
                           & draht_rot$Haendigkeit == "rechts")
shapiro.test(rot_rechts_lo_ru$Biegungsanzahl)
# Shapiro-Wilk normality test
# 
# data:  rot_rechts_lo_ru$Biegungsanzahl
# W = 0.72863, p-value = 0.02386

## kleiner Niveau 0.05


# ro-lu
rot_rechts_ro_lu <- subset(draht_rot, draht_rot$Biegerichtung == "ro-lu" 
                           & draht_rot$Haendigkeit == "rechts")
shapiro.test(rot_rechts_ro_lu$Biegungsanzahl)
# Shapiro-Wilk normality test
# 
# data:  rot_rechts_ro_lu$Biegungsanzahl
# W = 0.86337, p-value = 0.2725



### silberner Draht: ________
##  Linkshaenderin: -----

# mo-mu
silber_links_mo_mu <- subset(draht_silber, draht_silber$Biegerichtung == "mo-mu" 
                             & draht_silber$Haendigkeit == "links")
shapiro.test(silber_links_mo_mu$Biegungsanzahl)
# Shapiro-Wilk normality test
# 
# data:  silber_links_mo_mu$Biegungsanzahl
# W = 0.75295, p-value = 0.04114

## kleiner als Niveau 0.05


# lo-ru
silber_links_lo_ru <- subset(draht_silber, draht_silber$Biegerichtung == "lo-ru" 
                             & draht_silber$Haendigkeit == "links")
shapiro.test(silber_links_lo_ru$Biegungsanzahl)
# Shapiro-Wilk normality test
# 
# data:  silber_links_lo_ru$Biegungsanzahl
# W = 0.93153, p-value = 0.6034


# ro-lu
silber_links_ro_lu <- subset(draht_silber, draht_silber$Biegerichtung == "ro-lu" 
                             & draht_silber$Haendigkeit == "links")
shapiro.test(silber_links_ro_lu$Biegungsanzahl)
# Shapiro-Wilk normality test
# 
# data:  silber_links_ro_lu$Biegungsanzahl
# W = 0.91409, p-value = 0.5044


## Rechtshaenderin: -----

# mo-mu
silber_rechts_mo_mu <- subset(draht_silber, draht_silber$Biegerichtung == "mo-mu" 
                              & draht_silber$Haendigkeit == "rechts")
shapiro.test(silber_rechts_mo_mu$Biegungsanzahl)
# Shapiro-Wilk normality test
# 
# data:  silber_rechts_mo_mu$Biegungsanzahl
# W = 0.89495, p-value = 0.4064


# lo-ru
silber_rechts_lo_ru <- subset(draht_silber, draht_silber$Biegerichtung == "lo-ru" 
                              & draht_silber$Haendigkeit == "rechts")
shapiro.test(silber_rechts_lo_ru$Biegungsanzahl)
# Shapiro-Wilk normality test
# 
# data:  silber_rechts_lo_ru$Biegungsanzahl
# W = 0.88952, p-value = 0.3809


# ro-lu
silber_rechts_ro_lu <- subset(draht_silber, draht_silber$Biegerichtung == "ro-lu" 
                              & draht_silber$Haendigkeit == "rechts")
shapiro.test(silber_rechts_ro_lu$Biegungsanzahl)
# Shapiro-Wilk normality test
# 
# data:  silber_rechts_ro_lu$Biegungsanzahl
# W = 0.89519, p-value = 0.4075



##### VORTESTS - BARTLETT #####=================================================

bartlett.test(Biegungsanzahl ~ Drahtsorte, data = erg)
# Bartlett test of homogeneity of variances
# 
# data:  Biegungsanzahl by Drahtsorte
# Bartlett's K-squared = 48.376, df = 1, p-value = 3.518e-12


bartlett.test(Biegungsanzahl ~ Biegerichtung , data = erg)
# Bartlett test of homogeneity of variances
# 
# data:  Biegungsanzahl by Biegerichtung
# Bartlett's K-squared = 0.36327, df = 2, p-value = 0.8339


bartlett.test(Biegungsanzahl ~ Haendigkeit , data = erg)
# Bartlett test of homogeneity of variances
# 
# data:  Biegungsanzahl by Haendigkeit
# Bartlett's K-squared = 1.9018, df = 1, p-value = 0.1679




##### LINEARES MODELL #####=====================================================
modell <- aov(Biegungsanzahl ~ Drahtsorte + Biegerichtung + Haendigkeit, 
              data = erg)
summary(modell)
anova(modell)
# Analysis of Variance Table
# 
# Response: Biegungsanzahl
#               Df  Sum Sq Mean Sq F value    Pr(>F)    
# Drahtsorte     1 1938.02 1938.02 65.3105 3.635e-10 ***
# Biegerichtung  2   17.79    8.90  0.2998    0.7425    
# Haendigkeit    1   31.69   31.69  1.0679    0.3072    
# Residuals     43 1275.98   29.67                      
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

