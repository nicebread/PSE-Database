## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2018 Felix Schönbrodt

# ---------------------------------------------------------------------
# Predicting motive raw scores (which are count data) actually requires
# non-Gaussian models. However, for simplicity we'd like to stay with OLS
# regressions. This script explores the impact of this model misspecification
# compared to proper count models.

# Analysis is done on person level.

library(dplyr)
library(fitdistrplus)

# load the data
load(file="cache/person.RData")
df <- person	# assign shorter variable name

## ======================================================================
## First: Look at distribution of the DV: Count data!
## ======================================================================

funcNames <- c("Poisson", "Negative Binomial")

# ---------------------------------------------------------------------
# Aff

descdist(df$aff.sum, discrete=TRUE, boot=500)

fit.aff.pois <- fitdist(df$aff.sum, "pois", discrete=TRUE)
fit.aff.nbinom <- fitdist(df$aff.sum, "nbinom", discrete=TRUE)

gofstat(list(fit.aff.pois, fit.aff.nbinom), fitnames = funcNames)
cdfcomp(list(fit.aff.pois, fit.aff.nbinom), legendtext = funcNames, discrete=TRUE)

# --> clearly negative binomial!


# ---------------------------------------------------------------------
# Ach

descdist(df$ach.sum, discrete=TRUE, boot=500)

fit.ach.pois <- fitdist(df$ach.sum, "pois", discrete=TRUE)
fit.ach.nbinom <- fitdist(df$ach.sum, "nbinom", discrete=TRUE)

gofstat(list(fit.ach.pois, fit.ach.nbinom), fitnames = funcNames)
cdfcomp(list(fit.ach.pois, fit.ach.nbinom), legendtext = funcNames, discrete=TRUE)

# --> clearly negative binomial!



# ---------------------------------------------------------------------
# Pow

descdist(df$pow.sum, discrete=TRUE, boot=500)

fit.pow.pois <- fitdist(df$pow.sum, "pois", discrete=TRUE)
fit.pow.nbinom <- fitdist(df$pow.sum, "nbinom", discrete=TRUE)

gofstat(list(fit.pow.pois, fit.pow.nbinom), fitnames = funcNames)
cdfcomp(list(fit.pow.pois, fit.pow.nbinom), legendtext = funcNames, discrete=TRUE)

# --> clearly negative binomial!

# Summary: The negative binomial has an excellent fit to the data


## ======================================================================
## Single level modeling with negative binomial, compare with Gaussian model
## (for the moment: only linear main effects)
## ======================================================================

# ---------------------------------------------------------------------
# Compute classical linear model

df$lm.aff.r <- resid(lm(aff.sum ~ sc.person + wc.person, data=person))
df$lm.ach.r <- resid(lm(ach.sum ~ sc.person + wc.person, data=person))
df$lm.pow.r <- resid(lm(pow.sum ~ sc.person + wc.person, data=person))


# ---------------------------------------------------------------------
# Compute negative binomial regression

glm.pow <- glm.nb(pow.sum ~ sc.person, data=person, init.theta=10)
summary(glm.pow)

df$glm.aff.r <- resid(glm.nb(aff.sum ~ sc.person, data=person))
df$glm.ach.r <- resid(glm.nb(ach.sum ~ sc.person, data=person))
df$glm.pow.r <- resid(glm.nb(pow.sum ~ sc.person, data=person))

df %>% 
	ungroup() %>% 
	dplyr::select(sc.person, wc.person, contains("aff"), contains("ach"), contains("pow")) %>% 
	cor() %>% 
	round(2)

# --> Summary: Correlations between lm-residuals and glm.nb-residuals are > .90 (.92, .95, .90)
# For practical purposes, we stay with the Gaussian linear model.