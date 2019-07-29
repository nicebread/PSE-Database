
## ======================================================================
## OLD APPROACH
## ======================================================================

# ---------------------------------------------------------------------
# Compute z.standardized variables for model stability

df$sc.story.z <- scale(df$sc.story)
df$sc.story2.z <- scale(df$sc.story^2)
df$sc.story3.z <- scale(df$sc.story^3)

df$wc.story.z <- scale(df$wc.story)
df$wc.story2.z <- scale(df$wc.story^2)
df$wc.story3.z <- scale(df$wc.story^3)


## ======================================================================
## Model with Gaussian MLM
## ======================================================================

library(lmerTest)

# maxfun is calibrated to the bobyqa recommendation of "10 * length(par)^2"
# (56 parameters with both random effects pic_id and study_id)

# ---------------------------------------------------------------------
# Modes including the squared term

system.time({
mlm.aff2 <- lmer(aff.sum ~ sc.story.z + wc.story.z + sc.story2.z + wc.story2.z + (1 + sc.story.z + wc.story.z | pic_id) + (1 + sc.story.z + wc.story.z | study_id), data=df, control=lmerControl(optCtrl=list(maxfun=35000)), verbose=1)
})
summary(mlm.aff2)

# have to remove random slope for wc|study_id due to convergence problems
system.time({
mlm.ach2 <- lmer(ach.sum ~ sc.story.z + wc.story.z + sc.story2.z + wc.story2.z + (1 + sc.story.z + wc.story.z  | pic_id) + (1 + sc.story.z | study_id), data=df, control=lmerControl(optCtrl=list(maxfun=35000)), verbose=1)
})
summary(mlm.ach2)

system.time({
mlm.pow2 <- lmer(pow.sum ~ sc.story.z + wc.story.z + sc.story2.z + wc.story2.z + (1 + sc.story.z + wc.story.z | pic_id) + (1 + sc.story.z + wc.story.z | study_id), data=df, control=lmerControl(optCtrl=list(maxfun=35000)), verbose=1)
})
summary(mlm.pow2)




system.time({
mlm.aff <- lmer(aff.sum ~ sc.story.z + wc.story.z + (1 + sc.story.z + wc.story.z | pic_id) + (1 + sc.story.z + wc.story.z | study_id), data=df, control=lmerControl(optCtrl=list(maxfun=35000)), verbose=1)
})
summary(mlm.aff)

system.time({
mlm.ach <- lmer(ach.sum ~ sc.story.z + wc.story.z + (1 + sc.story.z + wc.story.z | pic_id) + (1 + sc.story.z | study_id), data=df, control=lmerControl(optCtrl=list(maxfun=35000)), verbose=1)
})
summary(mlm.ach)

system.time({
mlm.pow <- lmer(pow.sum ~ sc.story.z + wc.story.z + (1 + sc.story.z + wc.story.z | pic_id) + (1 + sc.story.z + wc.story.z | study_id), data=df, control=lmerControl(optCtrl=list(maxfun=35000)), verbose=1)
})
summary(mlm.pow)

# LR test: Do we need the squared terms?
(LR.aff <- anova(mlm.aff2, mlm.aff))
(LR.ach <- anova(mlm.ach2, mlm.ach))
(LR.pow <- anova(mlm.pow2, mlm.pow))

save(mlm.aff2, mlm.ach2, mlm.pow2, mlm.aff, mlm.ach, mlm.pow, LR.aff, LR.ach, LR.pow, file="processed_data/MLMs.RData")

# get marginal R^2 for all models
library(MuMIn)

r.squaredGLMM(mlm.aff2)
r.squaredGLMM(mlm.ach2)
r.squaredGLMM(mlm.pow2)
r.squaredGLMM(mlm.aff)
r.squaredGLMM(mlm.ach)
r.squaredGLMM(mlm.pow)

# extract random slope variance
summary(mlm.aff2)
VarCorr(mlm.aff2)

SD=attr(VarCorr(mlm.aff)$pic_id, "stddev")["sc.story.z"];paste0(f2(SD^2), " (", f2(SD), ")")
SD=attr(VarCorr(mlm.ach)$pic_id, "stddev")["sc.story.z"];paste0(f2(SD^2), " (", f2(SD), ")")
SD=attr(VarCorr(mlm.pow)$pic_id, "stddev")["sc.story.z"];paste0(f2(SD^2), " (", f2(SD), ")")

# extract fixed effects (SE)
paste0(f2(fixef(mlm.aff)["sc.story.z"]), " (", f2(sqrt(diag(vcov(mlm.aff))[2])),")")
paste0(f2(fixef(mlm.aff)["wc.story.z"]), " (", f2(sqrt(diag(vcov(mlm.aff))[3])),")")

# exemplary range of random effects
range(coef(mlm.aff)$pic_id$sc.story.z)
range(coef(mlm.aff)$study_id$sc.story.z)



## ======================================================================
## 
## ======================================================================

## ======================================================================
## Probability of motive coding depending on overall sentence count in story
## ======================================================================

storylength$aff.wc.resid <- resid(lm(aff.sum ~ wc.story, data=storylength))
storylength$ach.wc.resid <- resid(lm(ach.sum ~ wc.story, data=storylength))
storylength$pow.wc.resid <- resid(lm(pow.sum ~ wc.story, data=storylength))
storylength$overall.wc.resid <- resid(lm(overall.sum ~ wc.story, data=storylength))

ST <- storylength %>% 
	group_by(sc.story) %>% 
	summarise(
		aff.sum.mean=mean(aff.sum),
		ach.sum.mean=mean(ach.sum),
		pow.sum.mean=mean(pow.sum),
		overall.mean=mean(overall.sum),
		aff.wc.resid.mean=mean(aff.wc.resid),
		ach.wc.resid.mean=mean(ach.wc.resid),
		pow.wc.resid.mean=mean(pow.wc.resid),
		overall.wc.resid.mean=mean(overall.wc.resid),
		wc.mean=mean(wc.story)
	)

ST.long <- ST %>% gather(variable, value, -sc.story)

ggplot(ST.long, aes(x=sc.story, y=value, color=variable)) + geom_point() + geom_path() + facet_wrap(~variable, scales="free", ncol=2) + xlab("Sentence count in story") + ylab("Average motive score / Word count")





## ======================================================================
## Probability of motive coding depending on sentence position
## ======================================================================

ST <- PSE %>% 
	filter(scoring_type == "eachSentence", unit <= 17) %>% 
	group_by(unit) %>% 
	summarise(
		aff.mean=mean(aff),
		ach.mean=mean(ach),
		pow.mean=mean(pow),
		overall.mean=aff.mean + ach.mean + pow.mean,
		wc.mean=mean(wc)
	)

ST.long <- ST %>% gather(variable, value, -unit)

ggplot(ST.long, aes(x=unit, y=value, color=variable)) + geom_point() + geom_path() + facet_wrap(~variable, scales="free") + xlab("Sentence position") + ylab("Probability of motive score / Word count")



## ======================================================================
## Relations of word count, sentence count, and motive raw scores
# We want these correlations on person level, as this is the level of analysis
# in practice. 
# First approach: Ignore nesting in studies; Problem: differing number of pictures; sum
# DO NOT USE THIS APPROACH
## ======================================================================


person <- PSE %>% 
	filter(scoring_type == "eachSentence" & !grepl("newpic", pic_id)) %>% 
	group_by(study_id, participant_id) %>% 
	summarise(
		sc = n(),
		wc = sum(wc),
		n.pic_id = length(unique(pic_id)),
		aff.sum = sum(aff),
		ach.sum = sum(ach),
		pow.sum = sum(pow),
		overall.sum = sum(aff) + sum(ach) + sum(pow)
	) %>% 
	ungroup() %>% 
	mutate(
		aff.wc.resid = resid(lm(aff.sum ~ wc)),
		aff.sc.resid = resid(lm(aff.sum ~ sc)),
		aff.wc.dens = aff.sum / (wc/1000),
		aff.sc.dens = aff.sum / sc,
		
		ach.wc.resid = resid(lm(ach.sum ~ wc)),
		ach.sc.resid = resid(lm(ach.sum ~ sc)),
		ach.wc.dens = ach.sum / (wc/1000),
		ach.sc.dens = ach.sum / sc,
		
		pow.wc.resid = resid(lm(pow.sum ~ wc)),
		pow.sc.resid = resid(lm(pow.sum ~ sc)),
		pow.wc.dens = pow.sum / (wc/1000),
		pow.sc.dens = pow.sum / sc		
	)


# overview plot
ggplot(person, aes(x=sc, y=overall.sum)) + geom_jitter(alpha=.4) + geom_smooth()
ggplot(person, aes(x=wc, y=overall.sum)) + geom_jitter(alpha=.4) + geom_smooth()

storylength.table <- person %>% ungroup() %>% select(
	aff.sum, aff.wc.resid, aff.sc.resid, aff.wc.dens, aff.sc.dens, 
	ach.sum, ach.wc.resid, ach.sc.resid, ach.wc.dens, ach.sc.dens, 
	pow.sum, pow.wc.resid, pow.sc.resid, pow.wc.dens, pow.sc.dens, 
	#overall.sum, overall.wc.resid, overall.sc.resid, overall.wc.dens, overall.sc.dens, 
	wc, sc)

# build the descriptives/ correlation table
C1 <- cor(storylength.table, use="p")

C1 <- f2(C1, digits=2, skipZero=TRUE)
C1[lower.tri(C1)] <- ""
diag(C1) <- "-"

# Add means and SDs
tab.sent <- data.frame(
	M = f2(colMeans(storylength.table, na.rm=TRUE)), 
	SD = f2(apply(storylength.table, 2, sd, na.rm=TRUE)),
	C1)

rownames(tab.sent) <- c(
	"(1) Aff motive score",
	"(2) Aff motive score, word count resid.",
	"(3) Aff motive score, sentence count resid.",
	"(4) Aff motive density (per 1000 words)",
	"(5) Aff motive density (per sentence)",
	"(6) Ach motive score",
	"(7) Ach motive score, word count resid.",
	"(8) Ach motive score, sentence count resid.",
	"(9) Ach motive density (per 1000 words)",
	"(10) Ach motive density (per sentence)",
	"(11) Pow motive score",
	"(12) Pow motive score, word count resid.",
	"(13) Pow motive score, sentence count resid.",
	"(14) Pow motive density (per 1000 words)",
	"(15) Pow motive density (per sentence)",	
	"(16) Word count per story",
	"(17) Sentence count per story"
)

colnames(tab.sent) <- c(
	"Mean",
	"SD",
	"(1)",
	"(2)",
	"(3)",
	"(4)",
	"(5)",
	"(6)",
	"(7)",
	"(8)",
	"(9)",
	"(10)",
	"(11)",
	"(12)",
	"(13)",
	"(14)",
	"(15)",
	"(16)",
	"(17)"
)

xtable(tab.sent, caption="Descriptive statistics for overall motive, word, and sentence count.")



## ======================================================================
## Word count predicts mortive scores
# Exploration: Use a negative binomial distirbution
## ======================================================================


dat0 <- person %>% filter(study_id == "OCS_smofee8")
b1 <- stan_glm(aff.sum ~ wc.person.1000, family=gaussian(), data = dat0, 
                           chains = 1, seed = 12345, iter = 250, # for speed only
                           prior = normal(11.7, 3.80), # prior is for slopes
                           prior_intercept = normal(1.35, 1.05))

r1 <- lmrob(aff.sum ~ wc.person.1000, dat0, setting="KS2014")

b2 <- stan_glm.nb(aff.sum ~ wc.person.1000, data = dat0, 
                          chains = 1, seed = 12345, iter = 250, # for speed only
                          prior = normal(11.7, 3.80), # prior is for slopes
                          prior_intercept = normal(1.35, 1.05))


summary(b1, digits=2)
summary(b2, digits=2)

plot(resid(b1), resid(b2))
plot(resid(b1), resid(r1))