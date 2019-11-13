## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2018 Felix Schönbrodt

## ======================================================================
## Relations of word count, sentence count, and motive raw scores
# We want these correlations on person level, as this is the level of analysis
# in practice. 
# Problem: Nested in studies with differing number of pictures
# Solution: Do the analysis within each study, do a meta-analysis to aggregate
#
# We do not include study FS_newPic, because to few persons/stories per pic are present.
# We only include studies with the eachSentence coding style.
#
# This script computes Table 6: "Descriptive Statistics for Raw Motive Scores, 
# Word Count, and Sentence Count per Picture Story, and Correlations on Person Level."
## ======================================================================

source("1-start.R")

## ======================================================================
## Meta-analytic approach
## Compute the residuals and the correlation table in each study, and meta-analytically aggregate the tables
## ======================================================================


studies <- unique(person.gt50.each$study_id)

# corTable store the correlations of each study; 
# each row is one study, the columns contain the serialized correlation  matrix of this study
corTable <- matrix(NA, nrow=length(studies), ncol=289)
ns <- c()
resids <- tibble()
for (s in 1:length(studies)) {
	print(s)
	df <- person.gt50.each[person.gt50.each$study_id == studies[s], ] %>% 
		ungroup() %>% 
		mutate(
			aff.wc.resid = resid(lm(aff.sum ~ wc.person)),
			aff.sc.resid = resid(lm(aff.sum ~ sc.person)),
			aff.wc.dens = aff.sum / (wc.person/1000),
			aff.sc.dens = aff.sum / sc.person,
	
			ach.wc.resid = resid(lm(ach.sum ~ wc.person)),
			ach.sc.resid = resid(lm(ach.sum ~ sc.person)),
			ach.wc.dens = ach.sum / (wc.person/1000),
			ach.sc.dens = ach.sum / sc.person,
	
			pow.wc.resid = resid(lm(pow.sum ~ wc.person)),
			pow.sc.resid = resid(lm(pow.sum ~ sc.person)),
			pow.wc.dens = pow.sum / (wc.person/1000),
			pow.sc.dens = pow.sum / sc.person		
		) %>% 
		select(
			study_id, participant_id,
			aff.sum, aff.wc.resid, aff.sc.resid, aff.wc.dens, aff.sc.dens, 
			ach.sum, ach.wc.resid, ach.sc.resid, ach.wc.dens, ach.sc.dens, 
			pow.sum, pow.wc.resid, pow.sc.resid, pow.wc.dens, pow.sc.dens, 
			wc.person, sc.person
		)

	# we need to save the residuals for the descriptive statistics
	resids <- rbind(resids, df)

	# store the ns for each study; necessary to compute the standard error for meta-analysis
	ns <- c(ns, nrow(df))
	
	# store the correlation table; serialized as a vector
	C1 <- cor(df[, -c(1:2)], use="p")
	
	corTable[s, ] <- as.vector(C1)
}

# ---------------------------------------------------------------------
#  do the meta-analysis

corMeta.vec <- p.values <- corTable[, 1]

library(metafor)
# compute for each correlation, compute a meta-analysis across the 17 studies
# (i.e., for each column of corTable)
for (i in 1:ncol(corTable)) {
	print(i)
	ri <- corTable[, i]
	if (any(ri == 1)) {
		corMeta.vec[i] <- NA
		p.values[i] <- NA
	} else {
		ES <- escalc(ri=ri, ni=ns, measure="UCOR", vtype = "UB")
		M <- rma(yi=ES$yi, vi=ES$vi, method="REML")
		corMeta.vec[i] <- as.numeric(M$b)
		p.values[i] <- M$pval
	}
}

# convert serialized correlation vector back to a matrix
corMeta.string <- matrix(f2(corMeta.vec, digits=2, skipZero=TRUE, trimToZero=.005), nrow=nrow(C1))
corMeta.string[lower.tri(corMeta.string)] <- ""
diag(corMeta.string) <- "-"

# ---------------------------------------------------------------------
#  Add means and SDs

# add residual scores to the person.gt50.each object
person.gt50.each2 <- left_join(person.gt50.each, resids %>% select(study_id, participant_id, contains(".resid")), by=c("study_id", "participant_id"))

storylength.table <- person.gt50.each2 %>% ungroup() %>% 
	mutate(
		wc.perPic = wc.person / n.pic_id,
		sc.perPic = sc.person / n.pic_id,
		aff.perPic = aff.sum / n.pics,
		ach.perPic = ach.sum / n.pics,
		pow.perPic = pow.sum / n.pics
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

# ---------------------------------------------------------------------
# Save processed object

save(tab.sent, file="processed_data/tab.sent.RData")