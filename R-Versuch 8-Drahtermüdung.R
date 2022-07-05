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

plan$book$Drahtsorte[which(plan$book$Drahtsorte == "1")] <- "Eisendraht"
plan$book$Drahtsorte[which(plan$book$Drahtsorte == "2")] <- "Stahldraht"
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

draht_rot <- subset(erg, erg$Drahtsorte == "Eisen")
draht_silber <- subset(erg, erg$Drahtsorte == "Stahl")

biegericht_mo_mu <- subset(erg, erg$Biegerichtung == "mo-mu")
biegericht_lo_ru <- subset(erg, erg$Biegerichtung == "lo-ru")
biegericht_ro_lu <- subset(erg, erg$Biegerichtung == "ro-lu")

hand_links <- subset(erg, erg$Haendigkeit == "links")
hand_rechts <- subset(erg, erg$Haendigkeit == "rechts")


##### BOXPLOTS #####============================================================

# insg - Drahtsorte
# insg - Biegerichtung
# (insg - Haendigkeit)
# Tag 1  - Drahtsorte
# Tag 1 - Biegerichtung
# (Tag 1 - Haendigkeit)
# Tag 2 - Drahtsorte
# Tag 2 - Biegerichtung
# (Tag 2 - Haendigkeit)
# Draht - Biegerichtung
# Haendigkeit - Biegerichtung
# Draht - Haendigkeit



# insgesamt - Drahtsorte
ggplot(erg, aes(Drahtsorte, Biegungsanzahl)) +
  geom_boxplot(aes(group = Drahtsorte), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) # Ueberlappung mancher Punkte


# insgesamt - Biegerichtung
ggplot(erg, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
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
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3)


# Tag 1 - Drahtsorte
ggplot(tag_eins, aes(Drahtsorte, Biegungsanzahl)) +
  geom_boxplot(aes(group = Drahtsorte), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) # Ueberlappung mancher Punkte


# Tag 1 - Biegerichtung
ggplot(tag_eins, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3)


# Tag 1 - Haendigkeit
ggplot(tag_eins, aes(Haendigkeit, Biegungsanzahl)) +
  geom_boxplot(aes(group = Haendigkeit), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3)


# Tag 2 - Drahtsorte
ggplot(tag_zwei, aes(Drahtsorte, Biegungsanzahl)) +
  geom_boxplot(aes(group = Drahtsorte), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) 


# Tag 2 - Biegerichtung
ggplot(tag_zwei, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3)


# Tag 2 - Haendigkeit
ggplot(tag_zwei, aes(Haendigkeit, Biegungsanzahl)) +
  geom_boxplot(aes(group = Haendigkeit), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3)


# Draht - Biegerichtung
ggplot(draht_rot, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ggtitle("Bei Eisendraht (rot)") +
  ggplot(draht_silber, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ggtitle("Bei Stahldraht (silber)")


# Haendigkeit - Biegerichtung
ggplot(hand_links, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ggtitle("Bei Linkshändigkeit") +
  ggplot(hand_rechts, aes(Biegerichtung, Biegungsanzahl)) +
  geom_boxplot(aes(group = Biegerichtung), fill = c("lightcoral", "cadetblue2", 
                                                    "lightgreen"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ggtitle("Bei Rechtshandigkeit")


# Draht - Haendigkeit
ggplot(draht_rot, aes(Haendigkeit, Biegungsanzahl)) +
  geom_boxplot(aes(group = Haendigkeit), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ggtitle("Bei Eisendraht (rot)") +
  ggplot(draht_silber, aes(Haendigkeit, Biegungsanzahl)) +
  geom_boxplot(aes(group = Haendigkeit), fill = c("lightcoral", "cadetblue2"), 
               alpha = 0.7) +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  geom_point(pch = 16, size = 3) +
  ggtitle("Bei Stahldraht (silber)")


##### LINEARES MODELL #####=====================================================




##### VORTESTS #####============================================================
#shapiro.test()


