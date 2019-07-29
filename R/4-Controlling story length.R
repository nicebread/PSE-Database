## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2018 Felix Schönbrodt

##-----------------------------------------------------------------------------
## Purpose: Analyses for how to control for story length
##-----------------------------------------------------------------------------

# person.RData is computed in 2-Story length descriptives.R
source("0-start.R")
load(file="processed_data/person.RData")

# compute scales variables for better fits
person$sc.person.z <- scale(person$sc.person)
person$wc.person.z <- scale(person$wc.person)
person$sc.person2.z <- person$sc.person.z^2
person$wc.person2.z <- person$wc.person.z^2
# ---------------------------------------------------------------------
# Models including the squared term
# Random effects are uncorrelated to avoid onvergence problems and singular fits

mlm.aff2 <- lmer(aff.sum ~ sc.person.z + wc.person.z + sc.person2.z + wc.person2.z + (1 + sc.person.z + wc.person.z || study_id), data=person, control=lmerControl(optCtrl=list(maxfun=35000)))
summary(mlm.aff2)

mlm.ach2 <- lmer(ach.sum ~ sc.person.z + wc.person.z + sc.person2.z + wc.person2.z + (1 + sc.person.z + wc.person.z || study_id), data=person)
summary(mlm.ach2)

mlm.pow2 <- lmer(pow.sum ~ sc.person.z + wc.person.z + sc.person2.z + wc.person2.z + (1 + sc.person.z + wc.person.z || study_id), data=person)
summary(mlm.pow2)


# ---------------------------------------------------------------------
# Models with only linear terms: These are reported in the published table

mlm.aff <- lmer(aff.sum ~ sc.person.z + wc.person.z + (1 + sc.person.z + wc.person.z || study_id), data=person)
summary(mlm.aff)

mlm.ach <- lmer(ach.sum ~ sc.person.z + wc.person.z + (1 + sc.person.z + wc.person.z || study_id), data=person)
summary(mlm.ach)

mlm.pow <- lmer(pow.sum ~ sc.person.z + wc.person.z + (1 + sc.person.z + wc.person.z || study_id), data=person)
summary(mlm.pow)


# LR test: Do we need the squared terms?
(LR.aff <- anova(mlm.aff2, mlm.aff))
(LR.ach <- anova(mlm.ach2, mlm.ach))
(LR.pow <- anova(mlm.pow2, mlm.pow))

save(mlm.aff2, mlm.ach2, mlm.pow2, mlm.aff, mlm.ach, mlm.pow, LR.aff, LR.ach, LR.pow, file="processed_data/MLMs.RData")

# get marginal R^2 for all models
library(MuMIn)

r.squaredGLMM(mlm.aff2)
r.squaredGLMM(mlm.aff)

r.squaredGLMM(mlm.ach2)
r.squaredGLMM(mlm.ach)

r.squaredGLMM(mlm.pow2)
r.squaredGLMM(mlm.pow)

# extract random slope variance
SD=attr(VarCorr(mlm.aff)$study_id.1, "stddev")["sc.person.z"];paste0(f2(SD^2, 2), " (", f2(SD, 2), ")")
SD=attr(VarCorr(mlm.ach)$study_id.1, "stddev")["sc.person.z"];paste0(f2(SD^2, 2), " (", f2(SD, 2), ")")
SD=attr(VarCorr(mlm.pow)$study_id.1, "stddev")["sc.person.z"];paste0(f2(SD^2, 2), " (", f2(SD, 2), ")")

# extract fixed effects (SE)
paste0(f2(fixef(mlm.aff)["sc.person.z"], 2), " (", f2(sqrt(diag(vcov(mlm.aff))[2]), 2),")")
paste0(f2(fixef(mlm.aff)["wc.person.z"], 2), " (", f2(sqrt(diag(vcov(mlm.aff))[3]), 2),")")

# exemplary range of random effects
range(coef(mlm.aff)$study_id$sc.person.z)




## ======================================================================
## Compute commonality analysis in MLM; use only model with main effects
## ======================================================================

CA.MLM <- function(DV="aff.sum", data=person) {
	print("Computing 'sc only' model ...")
	MLM.only_sc <- lmer(formula(paste0(DV, " ~ sc.person.z + (1 + sc.person.z || study_id)")), data=data)	
	
	print("Computing 'wc only' model ...")
	MLM.only_wc <- lmer(formula(paste0(DV, " ~ wc.person.z + (1 + wc.person.z || study_id)")), data=data)	
	
	print("Computing 'sc and wc' model ...")
	MLM.both <- lmer(formula(paste0(DV, " ~ sc.person.z + wc.person.z + (1 + sc.person.z || study_id)")), data=data)	
	
	full.R2 <- r.squaredGLMM(MLM.both)[1, "R2m"]
	unique.R2.sc <- full.R2 - r.squaredGLMM(MLM.only_wc)[1, "R2m"]
	unique.R2.wc <- full.R2 - r.squaredGLMM(MLM.only_sc)[1, "R2m"]
	common.R2 <- full.R2 - unique.R2.sc - unique.R2.wc
	
	unique.R2.sc.perc <- unique.R2.sc/full.R2
	unique.R2.wc.perc <- unique.R2.wc/full.R2
	common.R2.perc <- common.R2/full.R2
	
	return(list(unique.R2.sc.perc=unique.R2.sc.perc, unique.R2.wc.perc=unique.R2.wc.perc, common.R2.perc=common.R2.perc, full.R2=full.R2))
}

CA.aff <- CA.MLM("aff.sum", data=person)
CA.ach <- CA.MLM("ach.sum", data=person)
CA.pow <- CA.MLM("pow.sum", data=person)

save(CA.aff, CA.ach, CA.pow, file="processed_data/CAs.RData")



## ======================================================================
## Bayesian approach (4c):
## - Analyze our big data base starting with uninformative priors
## - Use the posteriors for intercept and slope as input into subsequent single-study regressions
## - use a t-distributed prior for a more robust slope estimation (which accomodates outliers, see Kruschke, )
## ======================================================================


person$wc.person.1000 <- person$wc.person/1000

mlm.aff.simple <- lmer(aff.sum ~ wc.person.1000 + (1 + wc.person.1000 | study_id), data=person)
summary(mlm.aff.simple)

mlm.ach.simple <- lmer(ach.sum ~ wc.person.1000 + (1 + wc.person.1000 | study_id), data=person)
summary(mlm.ach.simple)

# uncorrelated random effects to avoid singular fit
mlm.pow.simple <- lmer(pow.sum ~ wc.person.1000 + (1 + wc.person.1000 || study_id), data=person)
summary(mlm.pow.simple)

brm.aff <- brm(
	aff.sum ~ wc.person.1000 + (1 + wc.person.1000 | study_id), 
	prior = c(prior(normal(0, 10), class = Intercept),
	          prior(normal(0, 20), class = b)),
	data=person)
summary(brm.aff)

x <- posterior_samples(brm.aff)

# combine samples from the fixed effect and the random draws from the random slope samples
# to get a posterior sample that combines both sources of variance
set.seed(0xBEEF)

slope_samples <- x$b_wc.person.1000
slope_samples_aug <- slope_samples + sample(unlist(x[, 22:36]), length(slope_samples))
plot(density(slope_samples), col="black", xlim=c(-5, 30))
lines(density(slope_samples_aug), col="red")

# find parameters of slope posterior samples
mean(slope_samples_aug)
sd(slope_samples_aug)

intercept_samples <- x$b_Intercept
intercept_samples_aug <- intercept_samples + sample(unlist(x[, 7:21]), length(intercept_samples))
plot(density(intercept_samples), col="black", xlim=c(-4, 8))
lines(density(intercept_samples_aug), col="red")

# find parameters of intercept posterior samples
mean(intercept_samples_aug)
sd(intercept_samples_aug)

# TODO: This must be done for all three motives separately

# ---------------------------------------------------------------------
# for each single data set: Compute Bayesian wc regression

res <- data.frame()
for (s in unique(person$study_id)) {
	print(s)
	dat0 <- person %>% filter(study_id == s)
	
	l1 <- lm(aff.sum ~ wc.person.1000, dat0)
	r1 <- lmrob(aff.sum ~ wc.person.1000, dat0, setting="KS2014")
	
	# brm.aff <- brm(
	# 	aff.sum ~ wc.person.1000,
	# 	prior = c(prior(normal(1.35, 1.05), class = Intercept),
	# 	          prior(normal(11.7, 3.80), class = b)),
	# 	data=dat0, silent=TRUE)
	# #print(summary(brm.aff))
	
	b1 <- stan_glm(aff.sum ~ wc.person.1000, data = dat0, 
	                           chains = 1, seed = 12345, iter = 250, # for speed only
	                           prior = normal(11.7, 3.80), # prior is for slopes
	                           prior_intercept = normal(1.35, 1.05))
	
	res <- rbind(res, data.frame(
		studyID = s,
		intercept.lm = coef(l1)[1],
		intercept.rlm = coef(r1)[1],
		intercept.Bayes = fixef(b1)[1],
		slope.lm = coef(l1)[2],		
		slope.rlm = coef(r1)[2],		
		slope.Bayes = fixef(b1)[2],
		n = nrow(dat0)
	))
	
	print(res)
}

