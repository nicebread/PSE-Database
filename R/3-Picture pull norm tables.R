## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2019 Felix Schönbrodt

## ======================================================================
## Picture pulls: Descriptive table and Ternary Plot
## using only studies where the second-sentence-rule has *not* been applied
## ======================================================================

library(ggtern)
source("0-start.R")

# summarise PSE sentences and codings on story level
stories <- PSE %>% 
	filter(scoring_type == "eachSentence") %>% 
	group_by(USID, pic_ID) %>% 
	summarise(
		aff = sum(aff),
		ach = sum(ach),
		pow = sum(pow),
		aff.present = as.numeric(any(aff > 0)),	# is there any aff coding present? We need it for bolding the table cells
		ach.present = as.numeric(any(ach > 0)),
		pow.present = as.numeric(any(pow > 0)),
		sc = n(),				# sentence count
		wc = sum(wc),		# word count
		fullText = paste(text, collapse="  ")		
	) %>% 
	mutate(
		AI = str_count(fullText, regex("\\bnicht\\b", ignore_case = TRUE))		# AI = activity inhibition, count the word "nicht"
	) %>% 
	select(-fullText)


# even more aggregation: summarise stories across picture stimuli
pics <- stories %>% group_by(pic_ID) %>% summarise(
	aff.mean = mean(aff),
	aff.sd = sd(aff, na.rm=TRUE),
	aff.meanPresent = mean(aff.present),
	ach.mean = mean(ach),
	ach.sd = sd(ach, na.rm=TRUE),
	ach.meanPresent = mean(ach.present),
	pow.mean = mean(pow),
	pow.sd = sd(pow, na.rm=TRUE),
	pow.meanPresent = mean(pow.present),
	overallPull = aff.mean + ach.mean + pow.mean,
	sc.mean = mean(sc),
	sc.sd = sd(sc, na.rm=TRUE),
	wc.mean = mean(wc),
	wc.sd = sd(wc, na.rm=TRUE),
	AI.mean = mean(AI, na.rm=TRUE),
	AI.sd = sd(AI, na.rm=TRUE),
	n.stories=n()
)

pics %>% arrange(-overallPull) %>% print(n=100)


# final table use only pictures with at least 50 stories in the data set
# order by overall pull

pics %>% filter(n.stories > 50) %>% arrange(-overallPull) %>% print(n=100)

pics %>% filter(pic_ID %in% standardSix)

pics.round <- pics
pics.round[, -1] <- round(pics.round[, -1], 2)

# which new pictures have the highest motive pull?
pics %>% filter(grepl("newpic", pic_ID)) %>% arrange(-overallPull) %>% print(n=100)

library(rio)
export(pics.round, file="export/picturePulls.xlsx")


# ---------------------------------------------------------------------
# Produce the final norms table

t1dat <- pics %>% 
	filter(n.stories > 50) %>% 
	arrange(-overallPull) %>% 
	mutate(
		picNumber = 1:n(),
		# strings for the norm table
		aff.output = paste0(f2bold.y(aff.mean, aff.meanPresent, 2), " (", f2(aff.sd, 2), ")"),
		ach.output = paste0(f2bold.y(ach.mean, ach.meanPresent, 2), " (", f2(ach.sd, 2), ")"),
		pow.output = paste0(f2bold.y(pow.mean, pow.meanPresent, 2), " (", f2(pow.sd, 2), ")"),
		overall.output = f2(overallPull, 2),
		sc.output =  paste0(f2(sc.mean, 1), " (", f2(sc.sd, 1), ")"),
		wc.output =  paste0(f2(wc.mean, 0), " (", f2(wc.sd, 0), ")"),
		AI.output =  paste0(f2(AI.mean, 2), " (", f2(AI.sd, 2), ")"),
		
		aff.norm = aff.mean/overallPull,	# we need this for the ternary plot
		ach.norm = ach.mean/overallPull,	# we need this for the ternary plot
		pow.norm = pow.mean/overallPull		# we need this for the ternary plot
	) 

save(t1dat, file="cache/t1dat.RData")

	
# select columns for table in publication
t1 <- t1dat	%>% select(picNumber, pic_ID, aff.output, ach.output, pow.output, overall.output, AI.output, wc.output, n.stories)

# mark standard six pictures with a *
t1$pic_ID[t1$pic_ID %in% standardSix] <- paste0("*", t1$pic_ID[t1$pic_ID %in% standardSix])

# sanitize picture names for Latex:
t1$pic_ID <- gsub("_", "\\_", t1$pic_ID, fixed=TRUE)
t1$pic_ID <- gsub("&", "\\&", t1$pic_ID, fixed=TRUE)

colnames(t1) <- c("", "Pic ID", "Aff", "Ach", "Pow", "Overall", "Activity Inhib.", "Word count", "\\emph{n}")



tab.norm <- xtable(t1, 
	caption = "Means (SDs) of Motive Raw Scores and Activity Inhibition for Picture Stimuli.", 
	label = "tab:norms",
	)
	
	
	
# ---------------------------------------------------------------------
# Produce the picture norm table for all pictures 
# (for the online appendix at https://osf.io/pqckn/wiki/Norm%20values%20for%20each%20picture/), including sc and wc


library(knitr)

wikitable <- pics %>% 
	arrange(-overallPull) %>% 
	mutate(
		# strings for the norm table
		aff.output = paste0(f2(aff.mean, 2), " (", f2(aff.sd, 2), ")"),
		ach.output = paste0(f2(ach.mean, 2), " (", f2(ach.sd, 2), ")"),
		pow.output = paste0(f2(pow.mean, 2), " (", f2(pow.sd, 2), ")"),
		sc.output = paste0(f2(sc.mean, digits=1), " (", f2(sc.sd, digits=1), ")"),
		wc.output = paste0(f2(wc.mean, digits=0), " (", f2(wc.sd, digits=0), ")"),
		overall.output = f2(overallPull, 2),
		AI.output =  paste0(f2(AI.mean, 2), " (", f2(AI.sd, 2), ")")
	) 

# select columns for table in wiki
wikitable <- wikitable	%>% select(pic_ID, aff.output, ach.output, pow.output, overall.output, AI.output, n.stories, sc.output, wc.output)

colnames(wikitable) <- c("Pic ID", "Aff", "Ach", "Pow", "Overall", "Activity Inhibition", "n", "Sentence count", "Word count")

kable(wikitable, format="markdown")	

# ---------------------------------------------------------------------
# Compare norms to Schultheiss norms (not printed in paper)

t1.compare <- pics %>% 
	filter(pic_ID %in% standardSix) %>% 
	mutate(
		# strings for the norm table
		aff.new = paste0(f2(aff.mean, 2), " (", f2(aff.sd, 2), ")"),
		aff.SB = c("1.29 (1.08)", "0.19 (0.48)", "1.84 (1.05)", "--", "0.45 (0.71)", "0.20 (0.53)"),
		
		ach.new = paste0(f2(ach.mean, 2), " (", f2(ach.sd, 2), ")"),
		ach.SB = c("0.09 (0.31)", "0.66 (0.77)", "0.03 (0.17)", "--", "0.78 (0.84)", "0.11 (0.37)"),
		
		pow.new = paste0(f2(pow.mean, 2), " (", f2(pow.sd, 2), ")"),
		pow.SB = c("0.86 (0.83)", "0.80 (0.84)", "0.43 (0.72)", "--", "0.79 (0.85)", "1.16 (0.92)")
	)  %>% 
	select(pic_ID, aff.new, aff.SB, ach.new, ach.SB, pow.new, pow.SB)
	
t1.compare	
	


## ======================================================================
## Ternary Plot
## ======================================================================

load(file="cache/t1dat.RData")

# hmm, currently needs an older version of ggplot because of incompability
#devtools::install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org")

# some tweaking of the label position
t1dat <- t1dat %>% mutate(
	aff.label = aff.norm,
	ach.label = ach.norm,
	pow.label = pow.norm
)

# manually adjust labels that they do not overlap
t1dat$pow.label[22] <- t1dat$pow.norm[22] - .02
t1dat$ach.label[22] <- t1dat$ach.norm[22] + .02

t1dat$pow.label[20] <- t1dat$pow.norm[20] + .04
t1dat$ach.label[20] <- t1dat$ach.norm[20] + .01

t1dat$pow.label[5] <- t1dat$pow.norm[5] - .03
t1dat$pow.label[9] <- t1dat$pow.norm[9] - .03
t1dat$ach.label[5] <- t1dat$ach.norm[5] + .02
t1dat$ach.label[9] <- t1dat$ach.norm[9] + .02

t1dat$pow.label[2] <- t1dat$pow.norm[2] + .03


ternary.plot <- ggtern(data=t1dat, aes(x=ach.norm, y=aff.norm, z=pow.norm, size=overallPull, color=overallPull)) + 
	geom_point() + 
	geom_text(aes(x=ach.label, y=aff.label, z=pow.label, label=picNumber), hjust=0.5, size=3, color="black") + 
	scale_colour_gradient(low="lightgreen", high="red") + 
	labs(x="ACH", y="AFF", z="POW") + theme_rgbw()
ternary.plot

save(t1dat, ternary.plot, tab.norm, file="cache/picture_norms.RData")	

