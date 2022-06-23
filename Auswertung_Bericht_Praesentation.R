# Funktionen fuer Praesentation und Bericht

library(readxl)
library(ggplot2)
library(patchwork)
library(xtable)

# Einlesen des Versuchsprotokolls:
versuchsprotokoll <- read_excel("versuchsprotokoll.xlsx")

names(versuchsprotokoll) <- c("einheit", "teiln_nr", "geschlecht", 
                              "tag", "geplant_start", "zeit_durchf",
                              "ziel_schaetzung", "ziel_schaetzung_hdtsek",
                              "erg_schaetzung", "erg_schaetzung_hdtsek",
                              "rel_abweichung", "rel_abweichung_abs", 
                              "Stoerungen")
versuchsprotokoll$ziel_schaetzung <- as.factor(versuchsprotokoll$ziel_schaetzung)


x <- versuchsprotokoll$ziel_schaetzung_hdtsek/100
y1 <- versuchsprotokoll$rel_abweichung_abs
y2 <- versuchsprotokoll$rel_abweichung


# Vortests:
shapiro.test(residuals(lm(y1 ~ x)))
# W = 0.9318, p-value = 0.1196
shapiro.test(residuals(lm(y2 ~ x)))
# W = 0.9801, p-value = 0.9078
shapiro.test(residuals(lm(y1 ~ x + I(x^2))))
# W = 0.95089, p-value = 0.3053
shapiro.test(residuals(lm(y2 ~ x + I(x^2))))
# W = 0.98195, p-value = 0.9365


# Regression/t-Test:
summary(lm(y1 ~ x))$coef
#                 Estimate   Std. Error  t value   Pr(>|t|)
# (Intercept) 0.0920359149 0.0487235435 1.888941 0.07278938
# x           0.0009214062 0.0007097218 1.298264 0.20827441
summary(lm(y2 ~ x))$coef
#                 Estimate  Std. Error   t value  Pr(>|t|)
# (Intercept) -0.189042708 0.072342075 -2.613178 0.0162401
# x            0.002330156 0.001053756  2.211285 0.0382408
summary(lm(y1 ~ x + I(x^2)))$coef
#                  Estimate   Std. Error    t value   Pr(>|t|)
# (Intercept)  1.662731e-01 9.398105e-02  1.7692199 0.09210502
# x           -2.660670e-03 3.937724e-03 -0.6756871 0.50697758
# I(x^2)       2.985063e-05 3.227322e-05  0.9249350 0.36602610
summary(lm(y2 ~ x + I(x^2)))$coef
#                  Estimate   Std. Error    t value  Pr(>|t|)
# (Intercept) -9.997626e-02 1.405699e-01 -0.7112211 0.4851602
# x           -1.967455e-03 5.889756e-03 -0.3340470 0.7418214
# I(x^2)       3.581343e-05 4.827189e-05  0.7419106 0.4667655

# p-Werte:
# lineare Regression, fuer theta_1 
2*(1- pt(1.298264, 21))     # mit 2 nur multipliziert, um die Werte mit R-Output
# [1] 0.2082744             # zu kontrollieren
1- pt(1.298264, 21)
# [1] 0.1041372

2*(1- pt(2.211285, 21))
# [1] 0.03824083
1- pt(2.211285, 21)
# [1] 0.01912042


# Datensatz ohne Drop-Out:
zeiten <- subset(versuchsprotokoll, !is.na(versuchsprotokoll$rel_abweichung))

# Datensatz differenziert nach Tag:
tag1 <- subset(zeiten, zeiten$tag == "Tag 1" | zeiten$teiln_nr == "7")
tag2 <- subset(zeiten, zeiten$tag == "Tag 2" & zeiten$teiln_nr != "7")

# Grafiken
# Boxplot: betragsm. rel. Abweichung 
ggplot(zeiten, aes(ziel_schaetzung_hdtsek/100, rel_abweichung_abs)) + 
  geom_boxplot(aes(group = ziel_schaetzung),
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral"), 
               alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  xlab("zu schätzende Zeit (in Sekunden)") +
  ylab("betragsmäßige relative Abweichung") +
  # ggtitle("Betragsmäßige relative Abweichungen der Schätzungen") +
  geom_point(pch = 16, size = 3) +
  scale_x_continuous(breaks = c(20, 60, 100), limits = c(5, 115)) 


# Boxplot: rel. Abweichung 
ggplot(zeiten, aes(ziel_schaetzung_hdtsek/100, rel_abweichung)) + 
  geom_boxplot(aes(group = ziel_schaetzung),
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral"), 
               alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  xlab("zu schätzende Zeit (in Sekunden)") +
  ylab("relative Abweichung") +
  # ggtitle("Relative Abweichungen der Schätzungen") +
  geom_point(pch = 16, size = 3) +
  scale_x_continuous(breaks = c(20, 60, 100), limits = c(5, 115))


# Boxplot: Relative Abweichung - Diff. nach Tag
ggplot(tag1, aes(ziel_schaetzung_hdtsek/100, rel_abweichung)) + 
  geom_boxplot(aes(group = ziel_schaetzung),
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral"), 
               alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  xlab("zu schätzende Zeit (in Sekunden)") +
  ylab("relative Abweichung") +
  ggtitle("Tag 1") +
  ylim(-0.32, 0.42) +
  geom_point(pch = 16, size = 3) +
  scale_x_continuous(breaks = c(20, 60, 100), limits = c(5, 115)) +
  ggplot(tag2, aes(ziel_schaetzung_hdtsek/100, rel_abweichung)) + 
  geom_boxplot(aes(group = ziel_schaetzung),
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral"), 
               alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(text = element_text(size = 25), 
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA), 
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE) +
  xlab("zu schätzende Zeit (in Sekunden)") +
  ylab("relative Abweichung") +
  ggtitle("Tag 2") +
  ylim(-0.32, 0.42) +
  geom_point(pch = 16, size = 3) +
  scale_x_continuous(breaks = c(20, 60, 100), limits = c(5, 115))


# Tabellen-Erstellung:
# Median, Interquartilsabstand, 1. und 3. Quartil, Maximum, Minimum, Spannweite
erstell_tabelle <- function(daten){
  speicher <- numeric(7)
  speicher[1] <- median(daten)
  speicher[2] <- IQR(daten)
  speicher[3] <- quantile(daten, 1/4)
  speicher[4] <- quantile(daten, 3/4)
  speicher[5] <- max(daten)
  speicher[6] <- min(daten)
  speicher[7] <- speicher[5] - speicher[6]
  names(speicher) <- c("Median", "Interquartilsabstand", 
                       "1.Quartil", "3.Quartil", "Maximum", "Minimum", 
                       "Spannweite")
  return(speicher)
}

# Datensaetze geordnet nach Zeitspanne:
zeit20 <- subset(zeiten, zeiten$ziel_schaetzung == "20 sek")
zeit60 <- subset(zeiten, zeiten$ziel_schaetzung == "60 sek")
zeit100 <- subset(zeiten, zeiten$ziel_schaetzung == "100 sek")

erster_tag20 <- subset(tag1, tag1$ziel_schaetzung == "20 sek")
erster_tag60 <- subset(tag1, tag1$ziel_schaetzung == "60 sek")
erster_tag100 <- subset(tag1, tag1$ziel_schaetzung == "100 sek")

zweiter_tag20 <- subset(tag2, tag2$ziel_schaetzung == "20 sek")
zweiter_tag60 <- subset(tag2, tag2$ziel_schaetzung == "60 sek")
zweiter_tag100 <- subset(tag2, tag2$ziel_schaetzung == "100 sek")

# Tabelle: Betragsm. rel. Abweichungen - insgesamt
tab_betrag_insg <- cbind(erstell_tabelle(zeit20$rel_abweichung_abs),
                         erstell_tabelle(zeit60$rel_abweichung_abs),
                         erstell_tabelle(zeit100$rel_abweichung_abs))

colnames(tab_betrag_insg) <- c("20 Sek", "60 Sek", "100 Sek")

tab_betrag_insg
#                        20 Sek     60 Sek 100 Sek
# Median               0.111000 0.04683333 0.20575
# Interquartilsabstand 0.170375 0.13691667 0.13505
# 1.Quartil            0.024250 0.04366667 0.12095
# 3.Quartil            0.194625 0.18058333 0.25600
# Maximum              0.319000 0.28083333 0.40310
# Minimum              0.000500 0.02250000 0.02030
# Spannweite           0.318500 0.25833333 0.38280


# Tabelle: Relative Abweichungen - insgesamt
tab_relativ_insg <- cbind(erstell_tabelle(zeit20$rel_abweichung),
                          erstell_tabelle(zeit60$rel_abweichung),
                          erstell_tabelle(zeit100$rel_abweichung))

colnames(tab_relativ_insg) <- c("20 Sek", "60 Sek", "100 Sek")

tab_relativ_insg
#                         20 Sek       60 Sek   100 Sek
# Median               -0.111000 -0.046666667  0.051650
# Interquartilsabstand  0.170375  0.189666667  0.396300
# 1.Quartil            -0.194625 -0.180583333 -0.143375
# 3.Quartil            -0.024250  0.009083333  0.252925
# Maximum              -0.000500  0.046833333  0.403100
# Minimum              -0.319000 -0.280833333 -0.242900
# Spannweite            0.318500  0.327666667  0.646000


# Tabelle: Rel. Abweichungen - differenziert nach Tag
tab_relativ_tag <- cbind(erstell_tabelle(erster_tag20$rel_abweichung),
                         erstell_tabelle(erster_tag60$rel_abweichung),
                         erstell_tabelle(erster_tag100$rel_abweichung),
                         erstell_tabelle(zweiter_tag20$rel_abweichung),
                         erstell_tabelle(zweiter_tag60$rel_abweichung),
                         erstell_tabelle(zweiter_tag100$rel_abweichung))

colnames(tab_relativ_tag) <- c("Tag 1 - 20 Sek", "Tag 1 - 60 Sek", 
                               "Tag 1 - 100 Sek", "Tag 2 - 20 Sek",
                               "Tag 2 - 60 Sek", "Tag 2 - 100 Sek")

tab_relativ_tag
#                      Tag 1 - 20 Sek Tag 1 - 60 Sek Tag 1 - 100 Sek
# Median                    -0.135500   -0.118833333         -0.1336
# Interquartilsabstand       0.238625    0.226416667          0.4680
# 1.Quartil                 -0.260875   -0.231583333         -0.1727
# 3.Quartil                 -0.022250   -0.005166667          0.2953
# Maximum                   -0.000500    0.046833333          0.4031
# Minimum                   -0.319000   -0.280833333         -0.2429
# Spannweite                 0.318500    0.327666667          0.6460
# 
#                      Tag 2 - 20 Sek Tag 2 - 60 Sek Tag 2 - 100 Sek
# Median                    -0.111000    -0.04666667         0.08300
# Interquartilsabstand       0.060625     0.09333333         0.10925
# 1.Quartil                 -0.137000    -0.09633333         0.05165
# 3.Quartil                 -0.076375    -0.00300000         0.16090
# Maximum                   -0.008500     0.04066667         0.23880
# Minimum                   -0.179000    -0.14600000         0.02030
# Spannweite                 0.170500     0.18666667         0.21850


# Tabelle: 20 Sekunden (insgesamt, Tag 1, Tag 2)
tab_20_sek <- cbind(erstell_tabelle(zeit20$rel_abweichung),
                    erstell_tabelle(erster_tag20$rel_abweichung),
                    erstell_tabelle(zweiter_tag20$rel_abweichung))
colnames(tab_20_sek) <- c("insgesamt", "Tag 1", "Tag 2")

tab_20_sek
#                      insgesamt     Tag 1     Tag 2
# Median               -0.111000 -0.135500 -0.111000
# Interquartilsabstand  0.170375  0.238625  0.060625
# 1.Quartil            -0.194625 -0.260875 -0.137000
# 3.Quartil            -0.024250 -0.022250 -0.076375
# Maximum              -0.000500 -0.000500 -0.008500
# Minimum              -0.319000 -0.319000 -0.179000
# Spannweite            0.318500  0.318500  0.170500


# Tabelle: 60 Sekunden (insgesamt, Tag 1, Tag 2)
tab_60_sek <- cbind(erstell_tabelle(zeit60$rel_abweichung),
                    erstell_tabelle(erster_tag60$rel_abweichung),
                    erstell_tabelle(zweiter_tag60$rel_abweichung))
colnames(tab_60_sek) <- c("insgesamt", "Tag 1", "Tag 2")

tab_60_sek
#                         insgesamt        Tag 1       Tag 2
# Median               -0.046666667 -0.118833333 -0.04666667
# Interquartilsabstand  0.189666667  0.226416667  0.09333333
# 1.Quartil            -0.180583333 -0.231583333 -0.09633333
# 3.Quartil             0.009083333 -0.005166667 -0.00300000
# Maximum               0.046833333  0.046833333  0.04066667
# Minimum              -0.280833333 -0.280833333 -0.14600000
# Spannweite            0.327666667  0.327666667  0.18666667


# Tabelle: 100 Sekunden (insgesamt, Tag 1, Tag 2)
tab_100_sek <- cbind(erstell_tabelle(zeit100$rel_abweichung),
                    erstell_tabelle(erster_tag100$rel_abweichung),
                    erstell_tabelle(zweiter_tag100$rel_abweichung))
colnames(tab_100_sek) <- c("insgesamt", "Tag 1", "Tag 2")

tab_100_sek
#                      insgesamt   Tag 1   Tag 2
# Median                0.051650 -0.1336 0.08300
# Interquartilsabstand  0.396300  0.4680 0.10925
# 1.Quartil            -0.143375 -0.1727 0.05165
# 3.Quartil             0.252925  0.2953 0.16090
# Maximum               0.403100  0.4031 0.23880
# Minimum              -0.242900 -0.2429 0.02030
# Spannweite            0.646000  0.6460 0.21850

xtable(tab_betrag_insg, digits = 4)
xtable(tab_relativ_insg, digits = 4)
xtable(tab_relativ_tag, digits = 4)

xtable(tab_20_sek, digits = 4)
xtable(tab_60_sek, digits = 4)
xtable(tab_100_sek, digits = 4)
