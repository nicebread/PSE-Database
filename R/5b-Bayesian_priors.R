# Purpose: Compute Bayesian posteriors for word count correction in all cases that used the standardSix picture set
# We compute a Bayesian prediction interval that incorporates the between-study variance, and therefore generalizes to new studies.

source("1-start.R")
load(file="processed_data/story.RData")

person.s6 <- story %>% 
	filter(pic_id %in% standardSix) %>% 
	group_by(study_id, participant_id) %>% 
	mutate(nPic = length(unique(pic_id))) %>% 
	filter(nPic == 6) %>% 
	summarise(
		gender = gender[1],
		scoring_type = scoring_type[1],
		n.pic_id = length(unique(pic_id)),
		aff.sum2 = sum(aff.sum),
		ach.sum2 = sum(ach.sum),
		pow.sum2 = sum(pow.sum),
		overall.sum = aff.sum2 + ach.sum2 + pow.sum2,
		sc.person = sum(sc.story),
		wc.person = sum(wc.story),
		wc.person.1000 = wc.person/1000
	) %>% 
	ungroup()

## ======================================================================
## Affiliation
## ======================================================================

mlm.aff.simple <- lmer(aff.sum2 ~ wc.person.1000 + (1 + wc.person.1000 | study_id), data=person.s6)
summary(mlm.aff.simple)

# fit Bayesian multilevel model with broad priors on the slope
brm.aff <- brm(
	aff.sum2 ~ wc.person.1000 + (1 + wc.person.1000 | study_id), 
	prior = c(prior(normal(0, 10), class = Intercept),
	          prior(normal(0, 20), class = b)),
	data=person.s6)
summary(brm.aff)

post <- posterior_samples(brm.aff)

# combine samples from the fixed effect and the random draws from the random slope samples
# to get a posterior sample that combines both sources of variance
set.seed(0xBEEF)

slope_samples_aff <- post$b_wc.person.1000
slope_samples_aff_aug <- unlist(slope_samples_aff + post[, 20:32])
plot(density(slope_samples_aff), col="black", xlim=c(-5, 30))
lines(density(slope_samples_aff_aug), col="red")

# find parameters of slope posterior samples
(mean_slope_aff <- mean(slope_samples_aff_aug))
(sd_slope_aff <- sd(slope_samples_aff_aug))

intercept_samples_aff <- post$b_Intercept
intercept_samples_aff_aug <- unlist(intercept_samples_aff + post[, 7:19])
plot(density(intercept_samples_aff), col="black", xlim=c(-4, 8))
lines(density(intercept_samples_aff_aug), col="red")


# find parameters of intercept posterior samples
(mean_intercept_aff <- mean(intercept_samples_aff_aug))
(sd_intercept_aff <- sd(intercept_samples_aff_aug))


pp <- posterior_predict(brm.aff)


## ======================================================================
## Achievement
## ======================================================================

mlm.ach.simple <- lmer(ach.sum2 ~ wc.person.1000 + (1 + wc.person.1000 | study_id), data=person.s6)
summary(mlm.ach.simple)

# fit Bayesian multilevel model with broad priors on the slope
brm.ach <- brm(
	ach.sum2 ~ wc.person.1000 + (1 + wc.person.1000 | study_id), 
	prior = c(prior(normal(0, 10), class = Intercept),
	          prior(normal(0, 20), class = b)),
	data=person.s6)
summary(brm.ach)

post <- posterior_samples(brm.ach)

# combine samples from the fixed effect and the random draws from the random slope samples
# to get a posterior sample that combines both sources of variance
set.seed(0xBEEF)

slope_samples_ach <- post$b_wc.person.1000
slope_samples_ach_aug <- unlist(slope_samples_ach + post[, 20:32])
plot(density(slope_samples_ach), col="black", xlim=c(-5, 30))
lines(density(slope_samples_ach_aug), col="red")

# find parameters of slope posterior samples
(mean_slope_ach <- mean(slope_samples_ach_aug))
(sd_slope_ach <- sd(slope_samples_ach_aug))

intercept_samples_ach <- post$b_Intercept
intercept_samples_ach_aug <- unlist(intercept_samples_ach + post[, 7:19])
plot(density(intercept_samples_ach), col="black", xlim=c(-4, 8))
lines(density(intercept_samples_ach_aug), col="red")

# find parameters of intercept posterior samples
(mean_intercept_ach <- mean(intercept_samples_ach_aug))
(sd_intercept_ach <- sd(intercept_samples_ach_aug))


## ======================================================================
## Power
## ======================================================================

mlm.pow.simple <- lmer(pow.sum2 ~ wc.person.1000 + (1 + wc.person.1000 | study_id), data=person.s6)
summary(mlm.pow.simple)

# fit Bayesian multilevel model with broad priors on the slope
brm.pow <- brm(
	pow.sum2 ~ wc.person.1000 + (1 + wc.person.1000 | study_id), 
	prior = c(prior(normal(0, 10), class = Intercept),
	          prior(normal(0, 20), class = b)),
	data=person.s6)
summary(brm.pow)

post <- posterior_samples(brm.pow)

# combine samples from the fixed effect and the random draws from the random slope samples
# to get a posterior sample that combines both sources of variance
set.seed(0xBEEF)

slope_samples_pow <- post$b_wc.person.1000
slope_samples_pow_aug <- unlist(slope_samples_pow + post[, 20:32])

# plausibility check:
BLUPs <- ranef(mlm.pow.simple)$study_id[, 2, drop=FALSE]
BLUPs$post <- as.vector(colMeans(post[, 20:32]))
plot(BLUPs); abline(a=0, b=1)

plot(density(slope_samples_pow), col="black", xlim=c(-5, 30))
lines(density(slope_samples_pow_aug), col="red")

# find parameters of slope posterior samples
(mean_slope_pow <- mean(slope_samples_pow_aug))
(sd_slope_pow <- sd(slope_samples_pow_aug))

intercept_samples <- post$b_Intercept
intercept_samples_aug <- unlist(intercept_samples + post[, 7:19])
plot(density(intercept_samples), col="black", xlim=c(-4, 8))
lines(density(intercept_samples_aug), col="red")

# find parameters of intercept posterior samples
(mean_intercept_pow <- mean(intercept_samples_aug))
(sd_intercept_pow <- sd(intercept_samples_aug))

