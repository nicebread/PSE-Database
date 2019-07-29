## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2018 Felix Schönbrodt

## ======================================================================
## Explore the difference between OLS correction for word count
# (current standard practice) and the newly recommended robust regression
## ======================================================================

library(dplyr)
library(purrr)
library(robustbase) 

load(file="processed_data/person.RData")

# ---------------------------------------------------------------------
# OLS residualization, within study

person$aff.resid <- person %>%
  split(.$study_id) %>% # from base R
  map(~ lm(aff.sum ~ wc.person.z, data = ., na.action=na.omit)) %>%
  map(resid) %>% 
	unlist()

person$ach.resid <- person %>%
  split(.$study_id) %>% # from base R
  map(~ lm(ach.sum ~ wc.person.z, data = ., na.action=na.omit)) %>%
  map(resid) %>% 
	unlist()

person$pow.resid <- person %>%
  split(.$study_id) %>% # from base R
  map(~ lm(pow.sum ~ wc.person.z, data = ., na.action=na.omit)) %>%
  map(resid) %>% 
	unlist()


# ---------------------------------------------------------------------
# Robust residualization, within study
# Apply the recommended procedure

person$aff.resid.robust <- person %>%
  split(.$study_id) %>% # from base R
  map(~ lmrob(aff.sum ~ wc.person.z, data = ., na.action=na.omit, setting="KS2014")) %>%
  map(resid) %>% 
	unlist()

person$ach.resid.robust <- person %>%
  split(.$study_id) %>% # from base R
  map(~ lmrob(ach.sum ~ wc.person.z, data = ., na.action=na.omit, setting="KS2014")) %>%
  map(resid) %>% 
	unlist()

person$pow.resid.robust <- person %>%
  split(.$study_id) %>% # from base R
  map(~ lmrob(pow.sum ~ wc.person.z, data = ., na.action=na.omit, setting="KS2014")) %>%
  map(resid) %>% 
	unlist()


sel <- person %>% select(contains("aff"), contains("ach"), contains("pow"))
C1 <- cor(sel) %>% round(3)
C1

plot(sel$aff.resid, sel$aff.resid.robust)
plot(sel$ach.resid, sel$ach.resid.robust)
plot(sel$pow.resid, sel$pow.resid.robust)

# --> OK: robust and regular regression does not make a real difference in our data sets ...
# But maybe they already have been screened for outliers.

## ======================================================================
## Check outlier influences
## ======================================================================

# use a part of real data set, then add some outliers
df <- person %>% filter(study_id=="OCS_smofee8") %>% slice(1:80)

cor(df$wc.person, df$aff.sum)
lm1 <- lm(aff.sum~wc.person, df)
rlm1 <- lmrob(aff.sum~wc.person, df, setting="KS2014")

plot(df$wc.person, df$aff.sum)
abline(a=coef(lm1)[1], b=coef(lm1)[2])
abline(a=coef(rlm1)[1], b=coef(rlm1)[2], col="red")


df2 <- df
# change two data points into outliers
df2[1, "wc.person"] <- 1800
df2[1, "aff.sum"] <- 0
df2[2, "wc.person"] <- 1900
df2[2, "aff.sum"] <- 0

cor.test(df2$wc.person, df2$aff.sum)
lm2 <- lm(aff.sum~wc.person, df2)
rlm2 <- lmrob(aff.sum~wc.person, df2, setting="KS2014")

plot(df2$wc.person, df2$aff.sum)
abline(a=coef(lm2)[1], b=coef(lm2)[2])
abline(a=coef(rlm2)[1], b=coef(rlm2)[2], col="red")

par(mfcol=c(3, 1))
hist(df2$aff.sum, breaks=24, main="aff raw score")
hist(resid(rlm2), breaks=24, main="robust residuals")
hist(resid(lm2), breaks=24, main="OLS residuals")

# ---------------------------------------------------------------------
#  Try Oliver's suggestion for correction

# >   1. Sind Motivrohwerte normalverteilt (Shapiro-Wilk + visuelle Inspektion des
# > Werte-Histogramms)?

shapiro.test(df2$aff.sum)
hist(df2$aff.sum)

# >   2. Ist die Wortanzahl normalverteilt (Shapiro-Wilk + visuelle Inspektion des
# > Werte-Histogramms)?

shapiro.test(df2$wc.person)
hist(df2$wc.person)

# >   3. Wenn zu 1 UND 2 nein, überprüfe, ob Outlier-Werte der Wortanzahl Outlier-Werte der
# > Motivscores „generiert“ (ob also z.B eine Probandin, die außergewöhnlich viel geschrieben
# > hat, auch außergewöhnlich hohe Werte in einem Motiv hat). Das erkennt man am besten an
# > einem Regressionsplot: wenn Outlier- & Leverage-Punkte sehr nah an der Regressionsgerade
# > liegen, stellen diese Punkte vermutlich Extrapolationen des Zusammenhangs im Rest der
# > Datenwolke dar, und eine Korrektur ist nicht nötig. In diesem Fall wird das Residuum
# > völlig unauffällig sein.

# --> not leverage points, real outliers

# >   4. Wenn zu 1 ODER 2 nein, transformiere (schwach: Quadratwurzel; stark: Log; immer nach
# > Addition der Konstante 1) die entsprechende Variable & überprüfe auf Normalverteilung.

# try sqrt-trans:
df2$aff.sum.sqrt <- sqrt(df2$aff.sum)
shapiro.test(df2$aff.sum.sqrt)
hist(df2$aff.sum.sqrt)

df2$aff.sum.log <- log(df2$aff.sum+1)
shapiro.test(df2$aff.sum.log)
hist(df2$aff.sum.log)


df2$wc.person.sqrt <- sqrt(df2$wc.person)
shapiro.test(df2$wc.person.sqrt)
hist(df2$wc.person.sqrt)

df2$wc.person.log <- log(df2$wc.person)
shapiro.test(df2$wc.person.log)
hist(df2$wc.person.log)


# >   5. Partialisiere (transformierten) Motivwert für (transformierten) Wortanzahlwert per
# > OLS, speichere Residuen.

df2$aff.resid.OCS <- resid(lm(aff.sum.sqrt~wc.person.log, df2))
df2$aff.resid.OCS2 <- resid(lm(aff.sum.log~wc.person.log, df2))

# >   6. Teste Residuen auf Normalverteilung (Shapiro-Wilk + visuelle Inspektion des
# > Werte-Histogramms) -- normalverteilt: Transformiere zu z-Werten; nicht-normalverteilt,
# > zurück zu 4 mit stärkerer/anderer Transformation

shapiro.test(df2$aff.resid.OCS)
hist(df2$aff.resid.OCS)

shapiro.test(df2$aff.resid.OCS2)
hist(df2$aff.resid.OCS2)



# >   7. Wenn Residuen trotz allem nicht mit Normalverteilung in Einklang gebracht werden
# > können, verwende ROBREG für Partialisierung und berichte Ergebnisse aller Analysen mit
# > und ohne Outlier
# >



## ======================================================================
## Different example: Construct a data point that is an bivariate outlier
## (considering word count), but not univariate
## ======================================================================


# use a real data set, then add some outliers
df2 <- person %>% filter(study_id=="OCS_smofee8") %>% slice(1:80)

# change one data point into outliers
df2[1, "wc.person"] <- 1100
df2[1, "aff.sum"] <- 2
df2[2, "wc.person"] <- 1800
df2[2, "aff.sum"] <- 24

rlm2 <- lmrob(aff.sum~wc.person, df2, setting="KS2014")

plot(df2$wc.person, df2$aff.sum)
points(1100, 2, col="red", pch=20)
points(1800, 24, col="green", pch=20)
abline(a=coef(rlm2)[1], b=coef(rlm2)[2], col="red")

hist(df2$aff.sum, breaks=20)
points(2, 0, col="red", pch=20, cex=2)
points(24, 0, col="green", pch=20, cex=2)

hist(resid(rlm2))
points(resid(rlm2)[1], 0, col="red", pch=20, cex=2)
points(resid(rlm2)[2], 0, col="green", pch=20, cex=2)