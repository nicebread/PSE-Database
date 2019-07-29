## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2018 Felix Schönbrodt

## ======================================================================
## Decline effect at later pictures?
## Descriptive plot and multilevel models.
## ======================================================================

source("0-start.R")
load(file="processed_data/story.RData")

story.random <- story %>% 
	filter(scoring_type == "eachSentence", pic_order=="variable") %>% 
	filter(!grepl("newpic", pic_id))

# special treatment for MK3: This study had a large break between pic_id 1-4 and 5-8. Therefore we only use 1-4
story.random <- story.random[!(story.random$study_id == "MK3" & story.random$pic_position > 4), ]

# show which picture appears on which position
table(story.random$pic_id, story.random$pic_position)

# More or less motive codings in later pictures?
# We do not use random effects for study_id, because this has only 7 levels
picPos.overall <- lmer(overall.sum ~ 1 + study_id + pic_position + (1 | pic_id) + (1 |participant_id), data=story.random)
summary(picPos.overall)

picPos.aff <- lmer(aff.sum ~ 1 + study_id + pic_position + (1 | pic_id) + (1 |participant_id), data=story.random)
summary(picPos.aff)

picPos.ach <- lmer(ach.sum ~ 1 + study_id + pic_position + (1 | pic_id) + (1 |participant_id), data=story.random)
summary(picPos.ach)

picPos.pow <- lmer(pow.sum ~ 1 + study_id + pic_position + (1 | pic_id) + (1 |participant_id), data=story.random)
summary(picPos.pow)

# More or less sentences in later pictures?
picPos.sc <- lmer(sc.story ~ 1 + study_id + pic_position + (1 | pic_id) + (1 |participant_id), data=story.random, control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun=35000)))
summary(picPos.sc)

# More or less words in later pictures?
picPos.wc <- lmer(wc.story ~ 1 + pic_position + (1 | pic_id) + (1 | participant_id), data=story.random, control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun=35000)))
summary(picPos.wc)


# convert to long format data frame for plotting
story.random.long <- story.random %>%
	ungroup() %>% 
	select(pic_id, pic_position, overall.sum, aff.sum, ach.sum, pow.sum, sc.story, wc.story) %>% 
	gather(variable, value, -pic_id, -pic_position)
	
story.random.long$variable <- factor(story.random.long$variable, labels=c("Overall motive score", "Aff score", "Ach score", "Pow score", "Sentence count", "Word count"))
	
picPosPlot <- ggplot(story.random.long, aes(x=pic_position, y=value)) + stat_summary(fun.data=mean_cl_normal) + facet_wrap(~variable, scales="free") + ylab("") + xlab("Story position in PSE task") + theme_bw()
picPosPlot

save(story.random, picPos.overall, picPos.aff, picPos.ach, picPos.pow, picPos.sc, picPos.wc, picPosPlot, file="processed_data/Decline.RData")