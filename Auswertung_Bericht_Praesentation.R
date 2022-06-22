# Funktionen fuer Praesentation und Bericht

library(readxl)
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


# Vortests
shapiro.test(residuals(lm(y1 ~ x)))
# W = 0.9318, p-value = 0.1196
shapiro.test(residuals(lm(y2 ~ x)))
# W = 0.9801, p-value = 0.9078
shapiro.test(residuals(lm(y1 ~ x + I(x^2))))
# W = 0.95089, p-value = 0.3053
shapiro.test(residuals(lm(y2 ~ x + I(x^2))))
# W = 0.98195, p-value = 0.9365


# Regression/t-Test
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