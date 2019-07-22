## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2018 Felix Schönbrodt

## ======================================================================
## Relations of word count, sentence count, and motive raw scores
# We want these correlations on person level, as this is the level of analysis
# in practice. 
# Problem: Nested in studies with differing number of pictures
# Solution: Do the analysis within each study, do a meta-analysis to aggregate
#
# This script computes Table 6: "Descriptive Statistics for Raw Motive Scores, 
# Word Count, and Sentence Count per Picture Story, and Correlations on Person Level."
## ======================================================================

source("0-start.R")

## ======================================================================
## Meta-analytic approach
## Compute the residuals and the correlation table in each study, and meta-analytically aggregate the tables
## ======================================================================

person <- PSE %>% 
	filter(scoring_type == "eachSentence" & !grepl("newpic", pic_ID)) %>% 
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
	
person$sc.person.z <- scale(person$sc.person)	
person$wc.person.z <- scale(person$wc.person)	
person$sc.person2.z <- scale(person$sc.person^2)	
person$wc.person2.z <- scale(person$wc.person^2)	


studies <- unique(person$study_ID)
corTable <- matrix(NA, nrow=289, ncol=length(studies))
ns <- c()
for (s in 1:length(studies)) {
	print(s)
	df <- person[person$study_ID == studies[s], ] %>% 
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
		) %>% 
		select(
			aff.sum, aff.wc.resid, aff.sc.resid, aff.wc.dens, aff.sc.dens, 
			ach.sum, ach.wc.resid, ach.sc.resid, ach.wc.dens, ach.sc.dens, 
			pow.sum, pow.wc.resid, pow.sc.resid, pow.wc.dens, pow.sc.dens, 
			wc, sc
		)

	# store the ns for each study; necessary to compute the standard error for meta-analysis
	ns <- c(ns, nrow(df))
	
	# store the correlation table; serialized as a vector
	C1 <- cor(df, use="p")
	
	corTable[, s] <- as.vector(C1)
}

# ---------------------------------------------------------------------
#  do the meta-analysis

r2Z <- function(r) {
	return(0.5 * log((1 + r)/(1 - r)))
}

# Helper: REcode Fisher's Z to correlation
Z2r <- function(Z) {
	return((exp(2*Z)-1)/(exp(2*Z)+1))
}

corMeta.vec <- p.values <- corTable[, 1]

library(metafor)
for (i in 1:nrow(corTable)) {
	yi <- r2Z(corTable[i, ])
	if (any(is.infinite(yi))) {
		corMeta.vec[i] <- NA
		p.values[i] <- NA
	} else {
		vi <- 1 / (ns-3)
		M <- rma(yi, vi=vi, method="REML")
		corMeta.vec[i] <- as.numeric(Z2r(M$b))
		p.values[i] <- M$pval
	}
}

# convert serialized correlation vector back to a matrix
corMeta.string <- matrix(f2(corMeta.vec, digits=2, skipZero=TRUE, trimToZero=.005), nrow=nrow(C1))
corMeta.string[lower.tri(corMeta.string)] <- ""
diag(corMeta.string) <- "-"

# ---------------------------------------------------------------------
#  Add means and SDs

storylength.table <- person %>% ungroup() %>% 
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
		pow.sc.dens = pow.sum / sc,
		
		wc.perPic = wc / n.pic_ID,
		sc.perPic = sc / n.pic_ID
	) %>% 
	select(
	aff.perPic, aff.wc.resid, aff.sc.resid, aff.wc.dens, aff.sc.dens, 
	ach.perPic, ach.wc.resid, ach.sc.resid, ach.wc.dens, ach.sc.dens, 
	pow.perPic, pow.wc.resid, pow.sc.resid, pow.wc.dens, pow.sc.dens, 
	wc.perPic, sc.perPic)
	
	
tab.sent <- data.frame(
	M = f2(colMeans(storylength.table, na.rm=TRUE), 2, trimToZero=.005), 
	SD = f2(apply(storylength.table, 2, sd, na.rm=TRUE), 2, trimToZero=.005),
	corMeta.string)

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


save(person, file="cache/person.RData")
save(tab.sent, file="cache/tab.sent.RData")