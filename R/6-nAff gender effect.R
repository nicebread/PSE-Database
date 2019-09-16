## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2018 Felix Schönbrodt

##-----------------------------------------------------------------------------
## Purpose: Compare different ways to control for story length on the well-known gender effect of nAff
##-----------------------------------------------------------------------------

library(metafor)

source("1-start.R")
load(file="processed_data/person.RData")

getGroupDiff <- function(df, DV){
	x <- df[df$gender=="f", ] %>% pull(DV)
	y <- df[df$gender=="m", ] %>% pull(DV)
	n1 <- length(x)
	n2 <- length(y)
	
	if (n1<2 | n2<2) return(data.frame(
		p.value=NA,
		m1=NA,
		m2=NA,
		sd1=NA,
		sd2=NA,
		n1=NA,
		n2=NA
	))
	
	df <- n1+n2-2
  S = sqrt( ((n1 - 1)*var(x) + (n2 - 1)*var(y)) / df )
  
	t1 <- t.test(x, y)
	
  #calculate d, the variance of d, the p-value, the t-stat, and n.
  d = (mean(x, na.rm=TRUE) - mean(y, na.rm=TRUE))/S
  d_v = (n1 + n2)/(n1 * n2) + (d^2 / (2*(n1+n2)))
	return(data.frame(
		p.value=t1$p.value,
		m1=mean(x, na.rm=TRUE),
		m2=mean(y, na.rm=TRUE),
		sd1=sd(x, na.rm=TRUE),
		sd2=sd(y, na.rm=TRUE),
		n1=length(na.omit(x)),
		n2=length(na.omit(y))
	))
}



# ---------------------------------------------------------------------
# OLS residualization, within study

person$aff.resid <- person %>%
  split(.$study_id) %>% # from base R
  map(~ lm(aff.sum ~ wc.person, data = ., na.action=na.omit)) %>%
  map(resid) %>% 
	unlist()

person$ach.resid <- person %>%
  split(.$study_id) %>% # from base R
  map(~ lm(ach.sum ~ wc.person, data = ., na.action=na.omit)) %>%
  map(resid) %>% 
	unlist()

person$pow.resid <- person %>%
  split(.$study_id) %>% # from base R
  map(~ lm(pow.sum ~ wc.person, data = ., na.action=na.omit)) %>%
  map(resid) %>% 
	unlist()


# ---------------------------------------------------------------------
# Robust residualization, within study
# Apply the recommended procedure

person$aff.resid.robust <- person %>%
  split(.$study_id) %>% # from base R
  map(~ lmrob(aff.sum ~ wc.person, data = ., na.action=na.omit, setting="KS2014")) %>%
  map(resid) %>% 
	unlist()

person$ach.resid.robust <- person %>%
  split(.$study_id) %>% # from base R
  map(~ lmrob(ach.sum ~ wc.person, data = ., na.action=na.omit, setting="KS2014")) %>%
  map(resid) %>% 
	unlist()

person$pow.resid.robust <- person %>%
  split(.$study_id) %>% # from base R
  map(~ lmrob(pow.sum ~ wc.person, data = ., na.action=na.omit, setting="KS2014")) %>%
  map(resid) %>% 
	unlist()
	
# # ---------------------------------------------------------------------
# # Robust residualization + squared wc, within study
# # Apply the recommended procedure
#
# person$aff.resid.robust2 <- person %>%
#   split(.$study_id) %>% # from base R
#   map(~ lmrob(aff.sum ~ wc.person + I(wc.person^2), data = ., na.action=na.omit, setting="KS2014")) %>%
#   map(resid) %>%
# 	unlist()
#
# person$ach.resid.robust2 <- person %>%
#   split(.$study_id) %>% # from base R
#   map(~ lmrob(ach.sum ~ wc.person + I(wc.person^2), data = ., na.action=na.omit, setting="KS2014")) %>%
#   map(resid) %>%
# 	unlist()
#
# person$pow.resid.robust2 <- person %>%
#   split(.$study_id) %>% # from base R
#   map(~ lmrob(pow.sum ~ wc.person + I(wc.person^2), data = ., na.action=na.omit, setting="KS2014")) %>%
#   map(resid) %>%
# 	unlist()


# ---------------------------------------------------------------------
#  density scores are already computed in 2-Descriptives.R

sel <- person %>% select(contains("aff"), contains("ach"), contains("pow"), wc.person)
C1 <- cor(sel) %>% round(3)
C1


# ---------------------------------------------------------------------
# Test gender effect in nAff

res.aff.resid <- person %>%
	filter(!is.na(gender)) %>%
	group_by(study_id) %>% 
	group_modify(~ getGroupDiff(.x, "aff.resid")) %>% 
	mutate(correction="resid") %>% 
	ungroup()

res.aff.robust <- person %>%
	filter(!is.na(gender)) %>% 
	group_by(study_id) %>% 
	group_modify(~ getGroupDiff(.x, "aff.resid.robust")) %>% 
	mutate(correction="robust") %>% 
	ungroup()

res.aff.density <- person %>%
	filter(!is.na(gender)) %>% 
	group_by(study_id) %>% 
	group_modify(~ getGroupDiff(.x, "aff.wc.dens")) %>% 
	mutate(correction="density") %>% 
	ungroup()
	

res.ach.resid <- person %>%
	filter(!is.na(gender)) %>%
	group_by(study_id) %>% 
	group_modify(~ getGroupDiff(.x, "ach.resid")) %>% 
	mutate(correction="resid") %>% 
	ungroup()

res.ach.robust <- person %>%
	filter(!is.na(gender)) %>% 
	group_by(study_id) %>% 
	group_modify(~ getGroupDiff(.x, "ach.resid.robust")) %>% 
	mutate(correction="robust") %>% 
	ungroup()

res.ach.density <- person %>%
	filter(!is.na(gender)) %>% 
	group_by(study_id) %>% 
	group_modify(~ getGroupDiff(.x, "ach.wc.dens")) %>% 
	mutate(correction="density") %>% 
	ungroup()	

res.pow.resid <- person %>%
	filter(!is.na(gender)) %>%
	group_by(study_id) %>% 
	group_modify(~ getGroupDiff(.x, "pow.resid")) %>% 
	mutate(correction="resid") %>% 
	ungroup()

res.pow.robust <- person %>%
	filter(!is.na(gender)) %>% 
	group_by(study_id) %>% 
	group_modify(~ getGroupDiff(.x, "pow.resid.robust")) %>% 
	mutate(correction="robust") %>% 
	ungroup()

res.pow.density <- person %>%
	filter(!is.na(gender)) %>% 
	group_by(study_id) %>% 
	group_modify(~ getGroupDiff(.x, "pow.wc.dens")) %>% 
	mutate(correction="density") %>% 
	ungroup()	




res.aff <- bind_rows(res.aff.density, res.aff.resid, res.aff.robust) %>% 
	data.frame() %>% 
	filter(!is.na(p.value))

res.ach <- bind_rows(res.ach.density, res.ach.resid, res.ach.robust) %>% 
	data.frame() %>% 
	filter(!is.na(p.value))

res.pow <- bind_rows(res.pow.density, res.pow.resid, res.pow.robust) %>% 
	data.frame() %>% 
	filter(!is.na(p.value))

# compute effect sizes for group differences
ES.aff <- escalc(measure="SMD", m1i=res.aff$m1, m2i=res.aff$m2, sd1i=res.aff$sd1, sd2i=res.aff$sd2, n1i=res.aff$n1, n2i=res.aff$n2)

ES.ach <- escalc(measure="SMD", m1i=res.ach$m1, m2i=res.ach$m2, sd1i=res.ach$sd1, sd2i=res.ach$sd2, n1i=res.ach$n1, n2i=res.ach$n2)

ES.pow <- escalc(measure="SMD", m1i=res.pow$m1, m2i=res.pow$m2, sd1i=res.pow$sd1, sd2i=res.pow$sd2, n1i=res.pow$n1, n2i=res.pow$n2)

# combine ES estimates and original data frame
res.aff <- data.frame(res.aff, ES.aff)
res.ach <- data.frame(res.ach, ES.ach)
res.pow <- data.frame(res.pow, ES.pow)

# get meta-analytic effect size for each correction method
(MA.resid.aff <- rma(yi=res.aff %>% filter(correction=="resid") %>% pull("yi"), vi=res.aff %>% filter(correction=="resid") %>% pull("vi"), method="FE"))
(MA.robust.aff <- rma(yi=res.aff %>% filter(correction=="robust") %>% pull("yi"), vi=res.aff %>% filter(correction=="robust") %>% pull("vi"), method="FE"))
(MA.density.aff <- rma(yi=res.aff %>% filter(correction=="density") %>% pull("yi"), vi=res.aff %>% filter(correction=="density") %>% pull("vi"), method="FE"))

(MA.resid.ach <- rma(yi=res.ach %>% filter(correction=="resid") %>% pull("yi"), vi=res.ach %>% filter(correction=="resid") %>% pull("vi"), method="FE"))
(MA.robust.ach <- rma(yi=res.ach %>% filter(correction=="robust") %>% pull("yi"), vi=res.ach %>% filter(correction=="robust") %>% pull("vi"), method="FE"))
(MA.density.ach <- rma(yi=res.ach %>% filter(correction=="density") %>% pull("yi"), vi=res.ach %>% filter(correction=="density") %>% pull("vi"), method="FE"))

(MA.resid.pow <- rma(yi=res.pow %>% filter(correction=="resid") %>% pull("yi"), vi=res.pow %>% filter(correction=="resid") %>% pull("vi"), method="FE"))
(MA.robust.pow <- rma(yi=res.pow %>% filter(correction=="robust") %>% pull("yi"), vi=res.pow %>% filter(correction=="robust") %>% pull("vi"), method="FE"))
(MA.density.pow <- rma(yi=res.pow %>% filter(correction=="density") %>% pull("yi"), vi=res.pow %>% filter(correction=="density") %>% pull("vi"), method="FE"))



# ---------------------------------------------------------------------
# Result table

RE_to_string <- function(obj, het=FALSE) {
	res <- paste0("$g = ", f2(obj$b, 2),"$ (\\emph{SE} = ", f2(obj$se, 2), ", $p$ = ", p0(obj$pval, latex=TRUE))
	if (het==FALSE) {
		res <- paste0(res, ")")
	} else {
		res <- paste0(res, "; Q(", obj$k-1,") = ", f2(obj$QE, 2), ", ", f2(obj$QEp, 3, skipZero=TRUE), "; $I^2$ = ", round(obj$I2),"\\%)")
	}	
	return(res)
}

FE_to_string <- function(obj) {
	paste0("$g = ", f2(obj$b, 2),"$ (\\emph{SE} = ", f2(obj$se, 2), ", ", p0(obj$pval, latex=TRUE), ")")
}

# a shorter version
FE_to_string2 <- function(obj) {
	paste0(f2(obj$b, 2)," (", f2(obj$se, 2), ", ", p0(obj$pval, latex=TRUE), ")")
}


tab.gender_MA <- data.frame(
	correction = c("Density scores", "OLS residuals", "Robust residuals"),
	aff.MA = c(FE_to_string2(MA.density.aff), FE_to_string2(MA.resid.aff), FE_to_string2(MA.robust.aff)),
	ach.MA = c(FE_to_string2(MA.density.ach), FE_to_string2(MA.resid.ach), FE_to_string2(MA.robust.ach)),
	pow.MA = c(FE_to_string2(MA.density.pow), FE_to_string2(MA.resid.pow), FE_to_string2(MA.robust.pow))
)

colnames(tab.gender_MA) <- c("Correction", "aff", "ach", "pow")



library(pwr)

curve(pwr.t.test(n=x, d=MA.robust.aff$b[1], sig.level=.05, type = "two.sample", alternative = "greater")$power, 
      from=20, to=100, col="black", xlab="Sample size per group", ylab="Statistical power", 
			lwd=2, lty="solid")
curve(pwr.t.test(n=x, d=MA.resid.aff$b[1], sig.level=.05, type = "two.sample", alternative = "greater")$power, 
      from=20, to=100, col="grey40", lwd=2, lty="dashed", add=TRUE)
curve(pwr.t.test(n=x, d=MA.density.aff$b[1], sig.level=.05, type = "two.sample", alternative = "greater")$power, 
      from=20, to=100, col="grey60", lwd=2, lty="dotted", add=TRUE)			
legend("bottomright", legend=c(
		paste0("d = ", f2(MA.robust.aff$b[1], 3)," (robust residuals)"),
		paste0("d = ", f2(MA.resid.aff$b[1], 3)," (OLS residuals)"),
		paste0("d = ", f2(MA.density.aff$b[1], 3)," (density scores)")
	), lty=c("solid", "dashed", "dotted"), col=c("black", "grey40", "grey60"), lwd=2
)	

pwr.t.test(n=60, d=MA.robust.aff$b[1], sig.level=.05, type = "two.sample", alternative = "greater")$power
pwr.t.test(n=60, d=MA.resid.aff$b[1], sig.level=.05, type = "two.sample", alternative = "greater")
pwr.t.test(n=60, d=MA.density.aff$b[1], sig.level=.05, type = "two.sample", alternative = "greater")

save(MA.resid.aff, MA.resid.ach, MA.resid.pow, MA.robust.aff, MA.robust.ach, MA.robust.pow, MA.density.aff, MA.density.ach, MA.density.pow, tab.gender_MA, file="processed_data/gender_MA.RData")