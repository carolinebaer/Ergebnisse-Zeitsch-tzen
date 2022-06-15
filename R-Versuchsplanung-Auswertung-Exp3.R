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
# (1 Beobachtung als fehlend geloescht)
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
# (1 Beobachtung als fehlend geloescht)
# Multiple R-squared:  0.1123,	Adjusted R-squared:  0.0235 
# F-statistic: 1.265 on 2 and 20 DF,  p-value: 0.304

summary(lm(zeiten$rel_abweichung ~ x_sek))$coef
#                 Estimate  Std. Error   t value  Pr(>|t|)
# (Intercept) -0.189042708 0.072342075 -2.613178 0.0162401
# x_sek        0.002330156 0.001053756  2.211285 0.0382408
plot(x_sek,zeiten$rel_abweichung)
abline(lm(zeiten$rel_abweichung ~ x_sek))
# Q-Q-Plot
plot(lm(zeiten$rel_abweichung ~ x_sek), which = 2)
# Normalverteilungsannahme scheint zu passen

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



#  Auswertung nach Geschlecht ##################################################

# Unterdatensaetze fuer die Untersuchung nach einem Geschlechstspezifischem 
# Unterschied in den Schaetzungen:

weibl <- subset(werte, werte$geschlecht== "weiblich")
maennl <- subset(werte, werte$geschlecht== "maennlich")

# Plot der betragsm. rel. Abweichung stratifiziert nach Geschlecht
plot1 <- ggplot(weibl, aes(ziel_schaetzung_hdtsek, rel_abweichung_abs)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("betragsmäßige relative Abweichung") +
  ggtitle("rel. Abweichungen der Schätzungen (Frauen)") +
  geom_smooth(method = "lm") +
  ylim(-0.1, 0.5)

plot2 <- ggplot(maennl, aes(ziel_schaetzung_hdtsek, rel_abweichung_abs)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("betragsmäßige relative Abweichung") +
  ggtitle("rel. Abweichungen der Schätzungen (Männer)") +
  geom_smooth(method = "lm") +
  ylim(-0.1, 0.5)

plot1 + plot2


# Boxplot der rel. Abweichungen stratifiziert nach Geschlecht
p1 <- ggplot(weibl, aes(ziel_schaetzung_hdtsek, rel_abweichung)) +
  geom_boxplot(aes(group = ziel_schaetzung), 
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("relative Abweichung") +
  ggtitle("rel. Abweichungen der Schätzungen (Frauen)") +
  ylim(-0.3, 0.45) +
  geom_point()

p2 <- ggplot(maennl, aes(ziel_schaetzung_hdtsek, rel_abweichung)) +
  geom_boxplot(aes(group = ziel_schaetzung), 
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("relative Abweichung") +
  ggtitle("rel. Abweichungen der Schätzungen (Männer)") +
  ylim(-0.3, 0.45) +
  geom_point()

p1 + p2



t.test(weibl$rel_abweichung_abs, alternative = "greater", 
       mu = 0, conf.level = 0.95)
# One Sample t-test
# 
# data:  weibl$rel_abweichung_abs
# t = 5.0024, df = 11, p-value = 0.0002005
# alternative hypothesis: true mean is greater than 0
# 95 percent confidence interval:
#   0.1047461       Inf
# sample estimates:
#   mean of x 
# 0.1634111 

t.test(maennl$rel_abweichung_abs, alternative = "greater", 
       mu = 0, conf.level = 0.95)
# One Sample t-test
# 
# data:  maennl$rel_abweichung_abs
# t = 3.572, df = 10, p-value = 0.002539
# alternative hypothesis: true mean is greater than 0
# 95 percent confidence interval:
#   0.06392269        Inf
# sample estimates:
#   mean of x 
# 0.1297667


# Boxplot der betragsm. rel. Abweichungen stratifiziert nach Geschlecht
plot_1 <- ggplot(weibl, aes(ziel_schaetzung_hdtsek, rel_abweichung_abs)) +
  geom_boxplot(aes(group = ziel_schaetzung), 
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("betragsmäßige relative Abweichung") +
  ggtitle("betragsmäßige rel. Abweichungen der Schätzungen (Frauen)") +
  ylim(-0.01, 0.45) +
  geom_point()

plot_2 <- ggplot(maennl, aes(ziel_schaetzung_hdtsek, rel_abweichung_abs)) +
  geom_boxplot(aes(group = ziel_schaetzung), 
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("betragsmäßige relative Abweichung") +
  ggtitle("betragsmäßige rel. Abweichungen der Schätzungen (Männer)") +
  ylim(-0.01, 0.45) +
  geom_point()

plot_1 + plot_2

# Grafik-1 fuer Praesentation:
ggplot(werte, aes(ziel_schaetzung_hdtsek, rel_abweichung)) + 
  geom_boxplot(aes(group = ziel_schaetzung),
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral"), 
               alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("relative Abweichung") +
  ggtitle("relative Abweichungen der Schätzungen") +
  geom_point(pch = 16, size = 3)

# Grafik-2 fuer Praesentation:
ggplot(werte, aes(ziel_schaetzung_hdtsek, rel_abweichung_abs)) + 
  geom_boxplot(aes(group = ziel_schaetzung),
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral"), 
               alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("betragsmäßige relative Abweichung") +
  ggtitle("betragsmäßige relative Abweichungen der Schätzungen") +
  geom_point(pch = 16, size = 3)

# Grafik-3 fuer Praesentation:
w <- ggplot(weibl, aes(ziel_schaetzung_hdtsek, rel_abweichung)) + 
  geom_boxplot(aes(group = ziel_schaetzung),
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral"), 
               alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("relative Abweichung") +
  ggtitle("relative Abweichungen der Schätzungen (Frauen)") +
  ylim(-0.3, 0.45) +
  geom_point(pch = 16, size = 3)

m <- ggplot(maennl, aes(ziel_schaetzung_hdtsek, rel_abweichung)) + 
  geom_boxplot(aes(group = ziel_schaetzung),
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral"), 
               alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("relative Abweichung") +
  ggtitle("relative Abweichungen der Schätzungen (Männer)") +
  ylim(-0.3, 0.45) +
  geom_point(pch = 16, size = 3)

w + m

# Grafik-4 fuer Praesentation:
w_abs <- ggplot(weibl, aes(ziel_schaetzung_hdtsek, rel_abweichung_abs)) + 
  geom_boxplot(aes(group = ziel_schaetzung),
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral"), 
               alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("betragsmäßige relative Abweichung") +
  ggtitle("betragsm. rel. Abweichungen der Schätzungen (Frauen)") +
  ylim(-0.01, 0.45) +
  geom_point(pch = 16, size = 3)

m_abs <- ggplot(maennl, aes(ziel_schaetzung_hdtsek, rel_abweichung_abs)) + 
  geom_boxplot(aes(group = ziel_schaetzung),
               fill = c("darkolivegreen3", "cadetblue2", "lightcoral"), 
               alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("zu schätzende Zeit (in Hundertstelsekunden)") +
  ylab("betragsmäßige relative Abweichung") +
  ggtitle("betragsm. rel. Abweichungen der Schätzungen (Männer)") +
  ylim(-0.01, 0.45) +
  geom_point(pch = 16, size = 3)

w_abs + m_abs

#  Auswertung nach Tag ##################################################

# relative Abweichungen
median(zeit20$rel_abweichung[zeit20$tag=="Tag 1"])
# [1] -0.1355
median(zeit20$rel_abweichung[zeit20$tag=="Tag 2"])
# [1] -0.111

median(zeit60$rel_abweichung[zeit60$tag=="Tag 1"])
# [1] -0.1188333
median(zeit60$rel_abweichung[zeit60$tag=="Tag 2"], na.rm = TRUE)
# [1] -0.04666667

median(zeit100$rel_abweichung[zeit100$tag=="Tag 1"])
# [1] -0.15315
median(zeit100$rel_abweichung[zeit100$tag=="Tag 2"])
# [1] 0.1609

# relative Absolut-Abweichungen
median(zeit20$rel_abweichung_abs[zeit20$tag=="Tag 1"])
# [1] 0.1355
median(zeit20$rel_abweichung_abs[zeit20$tag=="Tag 2"])
# [1] 0.111

median(zeit60$rel_abweichung_abs[zeit60$tag=="Tag 1"])
# [1] 0.131
median(zeit60$rel_abweichung_abs[zeit60$tag=="Tag 2"], na.rm = TRUE)
# [1] 0.04666667

median(zeit100$rel_abweichung_abs[zeit100$tag=="Tag 1"])
# [1] 0.2078
median(zeit100$rel_abweichung_abs[zeit100$tag=="Tag 2"])
# [1] 0.1609


boxplot(zeit20$rel_abweichung[zeit20$tag=="Tag 1"], 
        zeit60$rel_abweichung[zeit20$tag=="Tag 1"], 
        zeit100$rel_abweichung[zeit20$tag=="Tag 1"], 
        col = c("darkolivegreen3", "cadetblue2", "lightcoral"),
        main = "rel. Abweichung der Schaetzung nach Zeitintervall an Tag 1", 
        names = c("20 sek", "60 sek", "100 sek"),
        xlab = "Zeitintervall",
        ylab = "relative Abweichung",
        ylim = c(-0.4, 0.4))
abline(h=0, col = "darkblue", lty = 5)

boxplot(zeit20$rel_abweichung[zeit20$tag=="Tag 2"], 
        zeit60$rel_abweichung[zeit20$tag=="Tag 2"], 
        zeit100$rel_abweichung[zeit20$tag=="Tag 2"], 
        col = c("darkolivegreen3", "cadetblue2", "lightcoral"),
        main = "rel. Abweichung der Schaetzung nach Zeitintervall an Tag 2", 
        names = c("20 sek", "60 sek", "100 sek"),
        xlab = "Zeitintervall",
        ylab = "relative Abweichung",
        ylim = c(-0.4, 0.4))
abline(h=0, col = "darkblue", lty = 5)

boxplot(zeit20$rel_abweichung_abs[zeit20$tag=="Tag 1"], 
        zeit60$rel_abweichung_abs[zeit20$tag=="Tag 1"], 
        zeit100$rel_abweichung_abs[zeit20$tag=="Tag 1"], 
        col = c("darkolivegreen3", "cadetblue2", "lightcoral"),
        main = "betragsmaessige rel. Abweichung der Schaetzung nach Zeitintervall an Tag 1", 
        names = c("20 sek", "60 sek", "100 sek"),
        xlab = "Zeitintervall",
        ylab = "betragsmaessige relative Abweichung",
        ylim = c(-0.01, 0.45))
abline(h=0, col = "darkblue", lty = 5)

boxplot(zeit20$rel_abweichung_abs[zeit20$tag=="Tag 2"], 
        zeit60$rel_abweichung_abs[zeit20$tag=="Tag 2"], 
        zeit100$rel_abweichung_abs[zeit20$tag=="Tag 2"], 
        col = c("darkolivegreen3", "cadetblue2", "lightcoral"),
        main = "betragsmaessige rel. Abweichung der Schaetzung nach Zeitintervall an Tag 2", 
        names = c("20 sek", "60 sek", "100 sek"),
        xlab = "Zeitintervall",
        ylab = "betragsmaessige relative Abweichung",
        ylim = c(-0.01, 0.45))
abline(h=0, col = "darkblue", lty = 5)


# Vortest mittels Shapiro-Wilk-Test:___________________________________
shapiro.test(zeiten$rel_abweichung_abs)
# Shapiro-Wilk normality test
# 
# data:  zeiten$rel_abweichung_abs
# W = 0.93477, p-value = 0.1386

## da der p-Wert groesser als 0.05 ist, koennen wir Normalverteilung annehmen

# Vortest auf Residuen der lm-Modelle
shapiro.test(residuals(lm(zeiten$rel_abweichung ~ x_sek)))
# Shapiro-Wilk normality test
# 
# data:  residuals(lm(zeiten$rel_abweichung ~ x_sek))
# W = 0.9801, p-value = 0.9078
shapiro.test(residuals(lm(zeiten$rel_abweichung_abs ~ x_sek)))
# Shapiro-Wilk normality test
# 
# data:  residuals(lm(zeiten$rel_abweichung_abs ~ x_sek))
# W = 0.9318, p-value = 0.1196

shapiro.test(residuals(lm(zeiten$rel_abweichung ~ x_sek + I(x_sek^2))))
# Shapiro-Wilk normality test
# 
# data:  residuals(lm(zeiten$rel_abweichung ~ x_sek + I(x_sek^2)))
# W = 0.98195, p-value = 0.9365
shapiro.test(residuals(lm(zeiten$rel_abweichung_abs ~ x_sek + I(x_sek^2))))
# Shapiro-Wilk normality test
# 
# data:  residuals(lm(zeiten$rel_abweichung_abs ~ x_sek + I(x_sek^2)))
# W = 0.95089, p-value = 0.3053

# t-Test:____________________________________________________________
t.test(zeiten$rel_abweichung_abs, alternative = "greater", mu = 0, 
       conf.level = 0.95)
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


## Teststatistik: T = 6.1271
qt(0.95, 22)
# [1] 1.717144
## Kritischer Wert: t_(22; 1-0.05) = 1.717144
## p-Wert: 1.813e-06

## Teststatistik groesser als kritischer Wert, p-Wert < 0.05
## ==> lehne H0 ab
## die Steigung ist >0


summary(lm(zeiten$rel_abweichung_abs ~ x))
xtable(summary(lm(zeiten$rel_abweichung_abs ~ x)))
