## Auswertung von Versuch3-Zeitschaetzen ##

#### KOMPLETT AUSWERTUNG #######################################################

library(readxl)
library(ggplot2)
library(patchwork)

# Laden des Versuchsprotokolls:
versuchsprotokoll <- read_excel("versuchsprotokoll.xlsx")

names(versuchsprotokoll) <- c("einheit", "teiln_nr", "geschlecht", 
                              "tag", "geplant_start", "zeit_durchf",
                              "ziel_schaetzung", "ziel_schaetzung_hdtsek",
                              "erg_schaetzung", "erg_schaetzung_hdtsek",
                              "rel_abweichung", "rel_abweichung_abs", 
                              "Stoerungen")

versuchsprotokoll$geschlecht <- as.factor(versuchsprotokoll$geschlecht)
versuchsprotokoll$tag <- as.factor(versuchsprotokoll$tag)
versuchsprotokoll$ziel_schaetzung <- as.factor(versuchsprotokoll$ziel_schaetzung)

zeiten <- versuchsprotokoll

zeit20 <- subset(zeiten, zeiten$ziel_schaetzung == "20 sek")
zeit60 <- subset(zeiten, zeiten$ziel_schaetzung == "60 sek")
zeit100 <- subset(zeiten, zeiten$ziel_schaetzung == "100 sek")

boxplot(zeit20$rel_abweichung, zeit60$rel_abweichung, zeit100$rel_abweichung, 
        col = c("darkolivegreen3", "cadetblue2", "lightcoral"),
        main = "rel. Abweichung der Schätzung nach Zeitintervall", 
        names = c("20 sek", "60 sek", "100 sek"),
        xlab = "Zeitintervall",
        ylab = "relative Abweichung")
abline(h=0, col = "darkblue", lty = 5)


t.test(zeiten$rel_abweichung_abs, alternative = "greater", 
       mu = 0, conf.level = 0.95)
# One Sample t-test
# 
# data:  zeiten$rel_abweichung_abs
# t = 6.1271, df = 22, p-value = 1.813e-06
# alternative hypothesis: true mean is greater than 0
# 95 percent confidence interval:
#   0.1060332       Inf
# sample estimates:
#   mean of x 
# 0.1473203

x <- zeiten$ziel_schaetzung_hdtsek
y <- zeiten$rel_abweichung_abs

lin_modell <- lm(y ~ x + I(x^2))
summary(lin_modell)
# Call:
#   lm(formula = y ~ x + I(x^2))
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.17841 -0.08251 -0.02600  0.07529  0.20439 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  1.663e-01  9.398e-02   1.769   0.0921 .
# x           -2.661e-05  3.938e-05  -0.676   0.5070  
# I(x^2)       2.985e-09  3.227e-09   0.925   0.3660  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1139 on 20 degrees of freedom
# (1 Beobachtung als fehlend gelöscht)
# Multiple R-squared:  0.1123,	Adjusted R-squared:  0.0235 
# F-statistic: 1.265 on 2 and 20 DF,  p-value: 0.304


x_sek <- zeiten$ziel_schaetzung_hdtsek/100

lin_modell_sek <- lm(y ~ x_sek + I((x_sek)^2))
summary(lin_modell_sek)
# Call:
#   lm(formula = y ~ x_sek + I((x_sek)^2))
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.17841 -0.08251 -0.02600  0.07529  0.20439 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   1.663e-01  9.398e-02   1.769   0.0921 .
# x_sek        -2.661e-03  3.938e-03  -0.676   0.5070  
# I((x_sek)^2)  2.985e-05  3.227e-05   0.925   0.3660  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1139 on 20 degrees of freedom
# (1 Beobachtung als fehlend gelöscht)
# Multiple R-squared:  0.1123,	Adjusted R-squared:  0.0235 
# F-statistic: 1.265 on 2 and 20 DF,  p-value: 0.304



mean(zeit20$rel_abweichung, na.rm = TRUE) 
#[1] -0.125
mean(zeit60$rel_abweichung, na.rm = TRUE) 
#[1] -0.08909524
mean(zeit100$rel_abweichung, na.rm = TRUE)
#[1] 0.0614125


# Spannweite nach Zeitintervall:
max(zeit20$rel_abweichung) - min(zeit20$rel_abweichung)
#[1] 0.3185
max(zeit60$rel_abweichung, na.rm = TRUE) - 
  min(zeit60$rel_abweichung, na.rm = TRUE)
#[1] 0.3276667
max(zeit100$rel_abweichung) - min(zeit100$rel_abweichung)
#[1] 0.646

## Die Spannweiten werden mit wachsender Zeitspanne groesser.
## Die Spannweite von 100 sek ist mehr als doppelt so gross als die 
## anderen.



# Interquartilsabstand:
quantile(zeit20$rel_abweichung, 3/4) - quantile(zeit20$rel_abweichung, 1/4)
# 0.170375 
quantile(zeit60$rel_abweichung, na.rm = TRUE, 3/4) - 
  quantile(zeit60$rel_abweichung, na.rm = TRUE, 1/4)
# 0.1896667 
quantile(zeit100$rel_abweichung, 3/4) - quantile(zeit100$rel_abweichung, 1/4)
# 0.3963 

## Die Interquartilsabstaende werden mit wachsender Zeitspanne groesser.
## Der Interquartilsabstand von 100 sek ist mehr als doppelt so gross als die 
## anderen.


# Mediane:
median(zeit20$rel_abweichung)
#[1] -0.111
median(zeit60$rel_abweichung, na.rm = TRUE)
#[1] -0.04666667
median(zeit100$rel_abweichung)
#[1] 0.05165

werte <- subset(zeiten, !is.na(zeiten$rel_abweichung_abs))
plot(werte$rel_abweichung_abs)

# Plot der betragsmaessigen Abweichung in Hundertstelsekunden:
ggplot(werte, aes(ziel_schaetzung_hdtsek, rel_abweichung_abs))+
  geom_point() +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("Betrag der relativen Abweichung") +
  ggtitle("betragsmäßigen rel. Abweichungen der Schätzungen")

# Plot der betragsmaessigen Abweichung in Sekunden:
ggplot(werte, aes(ziel_schaetzung_hdtsek/100, rel_abweichung_abs))+
  geom_point() +
  theme_bw() +
  xlab("zu schätzende Zeit (in Sekunden)") +
  ylab("Betrag der relativen Abweichung") +
  ggtitle("betragsmäßigen rel. Abweichungen der Schätzungen")

# Plot der Abweichung in Hundertstelsekunden:
ggplot(werte, aes(ziel_schaetzung_hdtsek, rel_abweichung))+
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("relative Abweichung") +
  ggtitle("rel. Abweichungen der Schätzungen")

# Plot der Abweichung in Sekunden:
ggplot(werte, aes(ziel_schaetzung_hdtsek/100, rel_abweichung))+
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Sekunden)") +
  ylab("relative Abweichung") +
  ggtitle("rel. Abweichungen der Schätzungen")



# Unterdatensaetze fuer die Untersuchung nach einem Geschlechstspezifischem 
# Unterschied in den Schaetzungen:

weibl <- subset(werte, werte$geschlecht== "weiblich")
maennl <- subset(werte, werte$geschlecht== "maennlich")

par(mfrow = c(1,2))
plot1 <- ggplot(weibl, aes(ziel_schaetzung_hdtsek, rel_abweichung))+
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("relative Abweichung") +
  ggtitle("rel. Abweichungen der Schätzungen (Frauen)")

plot2 <- ggplot(maennl, aes(ziel_schaetzung_hdtsek, rel_abweichung))+
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("relative Abweichung") +
  ggtitle("rel. Abweichungen der Schätzungen (Männer)")

plot1 + plot2
