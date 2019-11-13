## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2018 Felix Schönbrodt

# ---------------------------------------------------------------------
# This script looks for a balanced picture set, choosing only from 
# the "newpic" pictures which have a free license.
# Be careful: There are only very few stories for these pictures
# in the database, hence this is really preliminary.

library(dplyr)
library(stringr)

load(file="data/PSE.RData")

standardSix <- c("women in laboratory", "boxer", "trapeze artists", "couple by river", "ship captain", "nightclub scene")


stories <- PSE %>% 
	filter(scoring_type == "eachSentence") %>% 
	group_by(USID, pic_id) %>% 
	summarise(
		aff = sum(aff),
		ach = sum(ach),
		pow = sum(pow),
		sc = n(),
		wc = sum(wc),
		fullText = paste(text, collapse="  ")		
	) %>% 
	mutate(
		AI = str_count(fullText, regex("\\bnicht\\b", ignore_case = TRUE))
	) %>% 
	select(-fullText)


pics <- stories %>% group_by(pic_id) %>% summarise(
	aff.sum = sum(aff),
	ach.sum = sum(ach),
	pow.sum = sum(pow),
	
	aff.mean = mean(aff),
	aff.sd = sd(aff, na.rm=TRUE),
	ach.mean = mean(ach),
	ach.sd = sd(ach, na.rm=TRUE),
	pow.mean = mean(pow),
	pow.sd = sd(pow, na.rm=TRUE),
	overallPull = aff.mean + ach.mean + pow.mean,
	sc.mean = mean(sc),
	sc.sd = sd(sc, na.rm=TRUE),
	wc.mean = mean(wc),
	wc.sd = sd(wc, na.rm=TRUE),
	AI.mean = mean(AI, na.rm=TRUE),
	AI.sd = sd(AI, na.rm=TRUE),
	n.stories=n()
)

# new 6 pics: This is our suggestion for a balanced set.
pics %>% 
filter(pic_id %in% c("newpic12", "applause", "newpic10", "newpic9", "newpic22", "newpic1")) %>% 
select(aff.mean, ach.mean, pow.mean, overallPull) %>% colMeans
	
pics %>% 
filter(pic_id %in% standardSix) %>% 
select(aff.mean, ach.mean, pow.mean, overallPull) %>% colMeans


# locate in ternary plot

pic_comp <- pics %>% filter(pic_id %in% c("newpic12", "applause", "newpic10", "newpic9", "newpic22", "newpic1", standardSix))

pic_comp <- pic_comp %>% mutate(
	aff.norm = aff.sum / overallPull,
	ach.norm = ach.sum / overallPull,
	pow.norm = pow.sum / overallPull,
	aff.label = aff.norm,
	ach.label = ach.norm,
	pow.label = pow.norm
)

library(ggtern)
ternary.plot <- ggtern(data=pic_comp, aes(x=ach.norm, y=aff.norm, z=pow.norm, size=overallPull, color=overallPull)) + 
	geom_point() + 
	geom_text(aes(x=ach.label, y=aff.label, z=pow.label, label=pic_id), hjust=0.5, size=3, color="black") + 
	scale_colour_gradient(low="lightgreen", high="red") + 
	labs(x="ACH", y="AFF", z="POW") + theme_rgbw()
ternary.plot