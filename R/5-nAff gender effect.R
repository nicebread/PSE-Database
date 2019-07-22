## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2018 Felix Schönbrodt

##-----------------------------------------------------------------------------
## Purpose: Compare different ways to control for story length on the well-known gender effect of nAff
##-----------------------------------------------------------------------------

library(metafor)

source("0-start.R")

getGroupDiff <- function(df, DV){
	x <- df[df$sex=="f", ] %>% pull(DV)
	y <- df[df$sex=="m", ] %>% pull(DV)
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


person <- PSE %>% 
	group_by(study_ID, person_ID) %>% 
	summarise(
		sex = sex[1],
		sc = n(),
		wc = sum(wc),
		n.pic_ID = length(unique(pic_ID)),
		aff.sum = sum(aff),
		ach.sum = sum(ach),
		pow.sum = sum(pow),
		n.pics = length(unique(pic_ID)),
		aff.perPic = aff.sum / n.pics,
		ach.perPic = ach.sum / n.pics,
		pow.perPic = pow.sum / n.pics,
		overall.sum = sum(aff) + sum(ach) + sum(pow),
		sc.person = n(),
		wc.person = sum(wc)
	) %>% 
	ungroup()

# ---------------------------------------------------------------------
# OLS residualization, within study

person$aff.resid <- person %>%
  split(.$study_ID) %>% # from base R
  map(~ lm(aff.sum ~ wc.person, data = ., na.action=na.omit)) %>%
  map(resid) %>% 
	unlist()

person$ach.resid <- person %>%
  split(.$study_ID) %>% # from base R
  map(~ lm(ach.sum ~ wc.person, data = ., na.action=na.omit)) %>%
  map(resid) %>% 
	unlist()

person$pow.resid <- person %>%
  split(.$study_ID) %>% # from base R
  map(~ lm(pow.sum ~ wc.person, data = ., na.action=na.omit)) %>%
  map(resid) %>% 
	unlist()


# ---------------------------------------------------------------------
# Robust residualization, within study
# Apply the recommended procedure

person$aff.resid.robust <- person %>%
  split(.$study_ID) %>% # from base R
  map(~ lmrob(aff.sum ~ wc.person, data = ., na.action=na.omit, setting="KS2014")) %>%
  map(resid) %>% 
	unlist()

person$ach.resid.robust <- person %>%
  split(.$study_ID) %>% # from base R
  map(~ lmrob(ach.sum ~ wc.person, data = ., na.action=na.omit, setting="KS2014")) %>%
  map(resid) %>% 
	unlist()

person$pow.resid.robust <- person %>%
  split(.$study_ID) %>% # from base R
  map(~ lmrob(pow.sum ~ wc.person, data = ., na.action=na.omit, setting="KS2014")) %>%
  map(resid) %>% 
	unlist()
	
# # ---------------------------------------------------------------------
# # Robust residualization + squared wc, within study
# # Apply the recommended procedure
#
# person$aff.resid.robust2 <- person %>%
#   split(.$study_ID) %>% # from base R
#   map(~ lmrob(aff.sum ~ wc.person + I(wc.person^2), data = ., na.action=na.omit, setting="KS2014")) %>%
#   map(resid) %>%
# 	unlist()
#
# person$ach.resid.robust2 <- person %>%
#   split(.$study_ID) %>% # from base R
#   map(~ lmrob(ach.sum ~ wc.person + I(wc.person^2), data = ., na.action=na.omit, setting="KS2014")) %>%
#   map(resid) %>%
# 	unlist()
#
# person$pow.resid.robust2 <- person %>%
#   split(.$study_ID) %>% # from base R
#   map(~ lmrob(pow.sum ~ wc.person + I(wc.person^2), data = ., na.action=na.omit, setting="KS2014")) %>%
#   map(resid) %>%
# 	unlist()


# ---------------------------------------------------------------------
#  density scores

person$aff.dens <- person$aff.sum/person$wc

sel <- person %>% select(contains("aff"), contains("ach"), contains("pow"))
C1 <- cor(sel) %>% round(3)
C1


# ---------------------------------------------------------------------
# Test gender effect in nAff

res1 <- person %>%
	filter(!is.na(sex)) %>%
	group_by(study_ID) %>% 
	group_map(~ getGroupDiff(.x, "aff.resid")) %>% 
	mutate(correction="resid") %>% 
	ungroup()

res2 <- person %>%
	filter(!is.na(sex)) %>% 
	group_by(study_ID) %>% 
	group_map(~ getGroupDiff(.x, "aff.resid.robust")) %>% 
	mutate(correction="robust") %>% 
	ungroup()

res3 <- person %>%
	filter(!is.na(sex)) %>% 
	group_by(study_ID) %>% 
	group_map(~ getGroupDiff(.x, "aff.dens")) %>% 
	mutate(correction="density") %>% 
	ungroup()

res4 <- person %>%
	filter(!is.na(sex)) %>% 
	group_by(study_ID) %>% 
	group_map(~ getGroupDiff(.x, "aff.resid.robust2")) %>% 
	mutate(correction="robust2") %>% 
	ungroup()

res <- bind_rows(res1, res2, res3) %>% 
	data.frame() %>% 
	filter(!is.na(p.value))

# compute effect sizes for group differences
ES <- escalc(measure="SMD", m1i=res$m1, m2i=res$m2, sd1i=res$sd1, sd2i=res$sd2, n1i=res$n1, n2i=res$n2)
res <- data.frame(res, ES)

# get meta-analytic effect size for each correction method
library(metafor)
(MA.resid <- rma(yi=res %>% filter(correction=="resid") %>% pull("yi"), vi=res %>% filter(correction=="resid") %>% pull("vi")))
(MA.robust <- rma(yi=res %>% filter(correction=="robust") %>% pull("yi"), vi=res %>% filter(correction=="resid") %>% pull("vi")))
(MA.density <- rma(yi=res %>% filter(correction=="density") %>% pull("yi"), vi=res %>% filter(correction=="resid") %>% pull("vi")))
#(MA.robust2 <- rma(yi=res %>% filter(correction=="robust2") %>% pull("yi"), vi=res %>% filter(correction=="resid") %>% pull("vi")))

funnel(MA.robust)

library(pwr)

curve(pwr.t.test(n=x, d=MA.robust$b[1], sig.level=.05, type = "two.sample", alternative = "greater")$power, 
      from=20, to=100, col="black", xlab="Sample size per group", ylab="Statistical power", 
			lwd=2, lty="solid")
curve(pwr.t.test(n=x, d=MA.resid$b[1], sig.level=.05, type = "two.sample", alternative = "greater")$power, 
      from=20, to=100, col="grey40", lwd=2, lty="dashed", add=TRUE)
curve(pwr.t.test(n=x, d=MA.density$b[1], sig.level=.05, type = "two.sample", alternative = "greater")$power, 
      from=20, to=100, col="grey60", lwd=2, lty="dotted", add=TRUE)			
legend("bottomright", legend=c(
		paste0("d = ", f2(MA.robust$b[1], 3)," (robust residuals)"),
		paste0("d = ", f2(MA.resid$b[1], 3)," (OLS residuals)"),
		paste0("d = ", f2(MA.density$b[1], 3)," (density scores)")
	), lty=c("solid", "dashed", "dotted"), col=c("black", "grey40", "grey60"), lwd=2
)	

pwr.t.test(n=60, d=MA.robust$b[1], sig.level=.05, type = "two.sample", alternative = "greater")
pwr.t.test(n=60, d=MA.resid$b[1], sig.level=.05, type = "two.sample", alternative = "greater")
pwr.t.test(n=60, d=MA.density$b[1], sig.level=.05, type = "two.sample", alternative = "greater")
