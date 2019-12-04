## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2019 Felix Schönbrodt
##-----------------------------------------------------------------------------
## Purpose: This file computes summary statistics and some tables
## displayed in the paper.
##-----------------------------------------------------------------------------

source("1-start.R")

# ---------------------------------------------------------------------
# Story descriptives
# USID = unique story ID
# Each row is one story

story <- PSE %>% 
	group_by(study_id, participant_id, pic_id, USID) %>% 
	summarise(
		sc.story = n(),
		wc.story = sum(wc),
		aff.sum = sum(aff),
		ach.sum = sum(ach),
		pow.sum = sum(pow),
		overall.sum = sum(aff) + sum(ach) + sum(pow),
		newpic = grepl("newpic", pic_id[1]),
		scoring_type = scoring_type[1],
		coding_lab = coding_lab[1],
		pic_position = pic_position[1],
		pic_order = pic_order[1],
		gender = gender[1]
	) %>% 
	group_by(pic_id) %>% 
	mutate(
		n.stories = n()
	) %>% 
	ungroup() %>% 
	arrange(study_id, participant_id, pic_position)

# how many stories per pic_id?
sort(table(story$pic_id))

# % of stories from pictures with <= 50 stories (all of them are "newpic" pictures)
round(prop.table(table(story$n.stories <= 50))["TRUE"]*100, 1)

# The following descriptive stats are based on the pics with > 50 stories only
story.gt50 <- story %>% filter(n.stories>50)
	
# show sentence counts	
table(story.gt50$sc.story)
table(story.gt50$sc.story) %>% prop.table %>% round(., 2)

# sanity check: Print the longest story with 41 sentences
PSE %>% filter(sc==41) %>% pull("text") %>% paste()

# sentence count, split by pic_id
sentenceCountByPic <- story.gt50 %>% 
	group_by(pic_id) %>% 
	summarise(
		sc.story.mean=mean(sc.story),
		wc.story.mean=mean(wc.story)
	) %>% 
	arrange(sc.story.mean)
	
sentenceCountByPic %>% print(n=100)

# How many stories are coded according to the 2nd-sentence-rule?
prop.table(table(story$scoring_type))

# ---------------------------------------------------------------------
#  number of pictures
length(unique(PSE$pic_id))
table(PSE$pic_id)

# number of stories
nrow(story)

# number of sentences
nrow(PSE)

# number of persons
length(unique(PSE$participant_id))

# number of included studies
length(unique(PSE$study_id))

# ---------------------------------------------------------------------
# TABLE motive categories
#  motive coding on sentence level; use only studies that did not apply the second-sentence rule!!

tab.mot.dat <- data.frame(prop.table(table(PSE %>% filter(scoring_type == "eachSentence") %>% pull("motclassfull")))*100) %>% arrange(-Freq)

colnames(tab.mot.dat) <- c("Motive category", "Frequency")
tab.mot.dat$Frequency <- paste0(f2(tab.mot.dat$Frequency, digits=1), "%")
tab.mot.dat

# ---------------------------------------------------------------------


# average length (M and SD) of stories, in terms of words and sentences
mean(story$sc.story)
sd(story$sc.story)
mean(story$wc.story)
sd(story$wc.story)

# number of stories per person
person.desc <- story %>% 
	group_by(participant_id, study_id) %>% 
	summarise(
		n.stories = n()
	)
	
summary(person.desc$n.stories)
prop.table(table(person.desc$n.stories))
table(person.desc$n.stories, person.desc$study_id) %>% prop.table(margin=2)*100 %>% round(., 1)



## ======================================================================
## Some demo sentences
## ======================================================================

start <- which(str_detect(PSE$text, fixed("der reporter im bild")))
demosentences0 <- PSE %>%
	slice(start:(start+12)) %>% 
	select(text, ach, aff, pow, motclassfull)
	

# correct some typos for the publication
demosentences0$text <- c("der reporter im bild versucht sich einen eindruck vom leben der beschäftigten der schifffahrt in der vergangenheit zu machen.",
"als er erfährt, dass dieser kapitän bei einem unwetter über 100 leben gerettet hat, beginnt er aufgeregt der sache auf den grund zu gehen.",
"immerhin könnte das die geschichte sein, auf die er seit langem wartet.",
"zwei freundinnen treffen sich um eine party vorzubereiten.",
"dazu sitzen auf der terasse in einem restaurant und sammeln ideen für ein motto.",
"außerdem wollen kurz aufteilen wer welche aufgaben bei der vorbereitung übernimmt.",
"hinzu kommt ein weiterer freund, der die beiden erkannt hat.",
"er möchte kurz eine minute aufmerksamkeit der beiden haben um hallo zu sagen.",
"die beiden sind so vertieft in ihre arbeit, dass sie ihn gar nicht erst wahrnehmen.",
"da er scheinbar schon länger steht ist er bereits etwas genervt.",
"wir befinden uns im zirkus rogalli.",
"die zwei akrobaten im bild sind bekannt für ihre gefährlichen kunststücke am trapez.",
"mit ihrer neuen nummer gehen sie noch ein stück weiter.")

# translation
demosentences0$translation <- c(
"The reporter in this picture is trying to get a sense of how ship employees lived in the past.",
"When he finds out that this captain saved more than 100 lives during a storm, he excitedly begins to investigate the matter.",
"After all, this could be the story he has been waiting for for a long time.",
"Two friends get together and prepare a party.",
"For this purpose, they are sitting on the terrace of a restaurant collecting ideas for the party’s theme.",
"Besides, they want to divvy up what needs to be done in preparation.",
"Another friend, who has recognized them, joins.",
"He wants to get the girls’ attention for a bit to say hello.",
"Both girls are so absorbed in their work that they do not even notice him.",
"It looks like he has been standing there for a while now and he is already somewhat annoyed.",
"We are at circus Rogalli.",
"The two acrobats in the picture are famous for their dangerous feats on the trapeze.",
"They go one step further with their new stunt."
)

# rearrange and define new column names
demosentences <- demosentences0 %>% select(
	"Text (original)" = text,
	"Text (translation)" = translation,
	ach, aff, pow, motclassfull
)



## ======================================================================
## Descriptive tables on study level
## ======================================================================

study.desc <- story %>% 
	group_by(study_id) %>% 
	summarise(
		n.story = n(),
		n.persons = length(unique(participant_id)),
		n.pictures = length(unique(pic_id)),
		scoring_type = scoring_type[1],
		coding_lab = coding_lab[1],
		pic_order = pic_order[1],
		gender_ratio = sum(gender=="f", na.rm=TRUE) / sum(!is.na(gender))
	)


print(study.desc, n=100)

# enrich the table with the meta-information from data/PSE-Database_ Study level descriptives.xlsx
library(rio)
study.meta <- import("raw_data/source_Data/PSE-Database_Study_level_descriptives.xlsx")[, 1:12]
colnames(study.meta)[c(1, 8:12)] <- c("study_id", "date_of_collection", "location", "administration", "test_setting", "population")
study.desc2 <- merge(study.desc, study.meta[, c(1, 8:10, 12)], by="study_id") %>% arrange(study_id)
	
# format gender ratio
study.desc2$gender_ratio <- paste0(round(study.desc2$gender_ratio*100), "%")
study.desc2$gender_ratio[study.desc2$gender_ratio == "NaN%"] <- "-"
	
## format as nice Latex table	
# escape underscores for Latex

# study.desc2$study_id <- gsub("_", "\\_", study.desc2$study_id, fixed=TRUE)
# study.desc2$scoring_type <- gsub("_", "\\_", study.desc2$scoring_type, fixed=TRUE)
# study.desc2$gender_ratio <- gsub("%", "\\%", study.desc2$gender_ratio, fixed=TRUE)
# study.desc2$administration <- gsub("&", "\\&", study.desc2$administration, fixed=TRUE)

colnames(study.desc2) <- c("Study ID", "# stories", "n", "# pic", "Scoring type", "Coding lab", "Pic. order", "% female", "Date", "Location", "Admin.", "Population")

tab.study.desc2 <- xtable(study.desc2, 
	caption = "Descriptives of Studies in the Database.", 
	label = "tab:studies")
	
	
## ======================================================================
## Person level aggregation
## ======================================================================

# all persons

person <- PSE %>% 
	group_by(study_id, participant_id) %>% 
	summarise(
		gender = gender[1],
		scoring_type = scoring_type[1],
		n.pic_id = length(unique(pic_id)),
		aff.sum = sum(aff),
		ach.sum = sum(ach),
		pow.sum = sum(pow),
		overall.sum = sum(aff) + sum(ach) + sum(pow),
		sc.person = n(),
		wc.person = sum(wc),
		n.pics = length(unique(pic_id)),
		
		# density scores per 1000 words / per sentence
		aff.wc.dens = aff.sum / (wc.person/1000),
		aff.sc.dens = aff.sum / sc.person,
		ach.wc.dens = ach.sum / (wc.person/1000),
		ach.sc.dens = ach.sum / sc.person,
		pow.wc.dens = pow.sum / (wc.person/1000),
		pow.sc.dens = pow.sum / sc.person
	) %>% 
	ungroup()

	
# persons with stories >= 50, and eachSentence coding style	
person.gt50.each <- person %>% filter(study_id != "FS_newpic", scoring_type == "eachSentence")
		
# ---------------------------------------------------------------------
# Save the stuff
		
#save.image("processed_data/1-Descriptives.RData")	
save(story, story.gt50, file="processed_data/story.RData")
save(person, person.gt50.each, person.desc, file="processed_data/person.RData")
save(tab.study.desc2, person.gt50.each, demosentences, sentenceCountByPic, tab.mot.dat, file="processed_data/Descriptives.RData")
