### As per usual, lets start by getting tables setup ###
pilot<-read.csv("study2_pilot_clean_summer.csv")

#we're gonna start w/ gtsummary, since that's how we closed it out originally
library("lme4")
library("texreg")
library("ggplot2")
library("tidyverse")
library("gtable")
library("gtsummary")


#lets bust out some simple graphs to see how the different conditions stack up

### UHC

# Belief Change
plot1a<-ggplot(pilot, aes(x=condition, y=uhc_belief_c, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Openness to Belief Change on Support for UHC", 
    colour = "Moral Conviction Manipulation",
    title = "Effect of Moral Conviction Manipulation on Openness to Change Beliefs on UHC"
  )
plot1a + scale_color_brewer(palette = "Set1")

plot2a<-ggplot(pilot, aes(x=condition, y=uhc_moral_c, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Moral Conviction on Support for UHC", 
    colour = "Moral Conviction Manipulation",
    title = "Manipulation on Moral Conviction of UHC Beliefs"
  )
plot2a + scale_color_brewer(palette = "Set1")

plot3a<-ggplot(pilot, aes(x=condition, y=uhc_persuadable, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Persuasiveness of Essay", 
    colour = "Moral Conviction Manipulation",
    title = "Persuasiveness of Essay on UHC by Moral Conviction Manipulation"
  )
plot3a + scale_color_brewer(palette = "Set1")

plot4a<-ggplot(pilot, aes(x=condition, y=uhc_support, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Support for UHC", 
    colour = "Moral Conviction Manipulation",
    title = "Support of UHC by Moral Conviction Manipulation"
  )
plot4a + scale_color_brewer(palette = "Set1")

### Death

# Belief Change
plot1b<-ggplot(pilot, aes(x=condition, y=death_belief_c, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Openness to Belief Change on Support for Capital Punishment", 
    colour = "Moral Conviction Manipulation",
    title = "Effect of Moral Conviction Manipulation on Openness to Change Beliefs on Capital Punishment"
  )
plot1b + scale_color_brewer(palette = "Set1")

plot2b<-ggplot(pilot, aes(x=condition, y=death_moral_c, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Moral Conviction on Support for Capital Punishment", 
    colour = "Moral Conviction Manipulation",
    title = "Manipulation on Moral Conviction of Capital Punishment Beliefs"
  )
plot2b + scale_color_brewer(palette = "Set1")

plot3b<-ggplot(pilot, aes(x=condition, y=death_persuadable, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Persuasiveness of Essay", 
    colour = "Moral Conviction Manipulation",
    title = "Persuasiveness of Essay on Capital Punishment by Moral Conviction Manipulation"
  )
plot3b + scale_color_brewer(palette = "Set1")

plot4b<-ggplot(pilot, aes(x=condition, y=death_support, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Support for Capital Punishment", 
    colour = "Moral Conviction Manipulation",
    title = "Support of Capital Punishment by Moral Conviction Manipulation"
  )
plot4b + scale_color_brewer(palette = "Set1")


### Exercise

# Belief Change
plot1c<-ggplot(pilot, aes(x=condition, y=exercise_belief_c, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Openness to Belief Change on Support for Exercise", 
    colour = "Moral Conviction Manipulation",
    title = "Effect of Moral Conviction Manipulation on Openness to Change Beliefs on Exercise"
  )
plot1c + scale_color_brewer(palette = "Set1")

plot2c<-ggplot(pilot, aes(x=condition, y=exercise_moral_c, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Moral Conviction on Support for Exercise", 
    colour = "Moral Conviction Manipulation",
    title = "Manipulation on Moral Conviction of Exercise Beliefs"
  )
plot2c + scale_color_brewer(palette = "Set1")

plot3c<-ggplot(pilot, aes(x=condition, y=exercise_persuadable, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Persuasiveness of Essay", 
    colour = "Moral Conviction Manipulation",
    title = "Persuasiveness of Essay on Exercise by Moral Conviction Manipulation"
  )
plot3c + scale_color_brewer(palette = "Set1")

plot4c<-ggplot(pilot, aes(x=condition, y=exercise_support, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Support for Exercise", 
    colour = "Moral Conviction Manipulation",
    title = "Support of Exercise by Moral Conviction Manipulation"
  )
plot4c + scale_color_brewer(palette = "Set1")

### Climate

# Belief Change
plot1d<-ggplot(pilot, aes(x=condition, y=climate_belief_c, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Openness to Belief Change on Climate Change", 
    colour = "Moral Conviction Manipulation",
    title = "Effect of Moral Conviction Manipulation on Openness to Change Beliefs on Climate Change"
  )
plot1d + scale_color_brewer(palette = "Set1")

plot2d<-ggplot(pilot, aes(x=condition, y=climate_moral_c, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Moral Conviction on Support for Climate Change", 
    colour = "Moral Conviction Manipulation",
    title = "Manipulation on Moral Conviction of Climate Change Beliefs"
  )
plot2d + scale_color_brewer(palette = "Set1")

plot3d<-ggplot(pilot, aes(x=condition, y=climate_persuadable, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Persuasiveness of Essay", 
    colour = "Moral Conviction Manipulation",
    title = "Persuasiveness of Essay on Climate Change Beliefs by Moral Conviction Manipulation"
  )
plot3d + scale_color_brewer(palette = "Set1")

plot4d<-ggplot(pilot, aes(x=condition, y=climate_support, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Support for Climate Change", 
    colour = "Moral Conviction Manipulation",
    title = "Support of Climate Change Beliefs by Moral Conviction Manipulation"
  )
plot4d + scale_color_brewer(palette = "Set1")

##################
## ANCOVA CHECK ##
##################
# Cloning some earlier code on running an ANCOVA analysis, indicating that there is indeed
# a significant effect of condition on support for [topic] controlling for 
# [conviction?] or [openness to belief change] 

# Likewise, see if there's significant effect of condition on [moral conviction]
# controlling for [support] and [openness to belief change]

library(dplyr)

# UHC
anc_supp_uhc <- aov(uhc_support ~ condition * uhc_belief_c, data = pilot)
summary(anc_supp_uhc)

anc_supp_uhc <- aov(uhc_support ~ condition + uhc_belief_c, data = pilot)
summary(anc_supp_uhc)

# differences in openess to belief change = support for UHC
anc_conv_uhc <- aov(uhc_moral_c ~ condition + uhc_belief_c, data = pilot)
summary(anc_conv_uhc)
# no seeming differences in conviction by condition moderated by belief change

# death penalty
anc_supp_death <- aov(death_support ~ condition + death_belief_c, data = pilot)
summary(anc_supp_death)
anc_conv_death <- aov(death_moral_c ~ condition + death_belief_c, data = pilot)
summary(anc_conv_death)
#no differences RE: death penalty

anc_supp_climate <- aov(climate_support ~ condition + climate_belief_c, data = pilot)
summary(anc_supp_climate)
anc_conv_climate <- aov(climate_moral_c ~ condition + climate_belief_c, data = pilot)
summary(anc_conv_climate)
anc_conv_climate <- aov(climate_moral_c ~ condition * climate_belief_c, data = pilot)
summary(anc_conv_climate)


#differences in moral conviction change based on opennness to belief change!


anc_supp_exercise <- aov(exercise_support ~ condition + exercise_belief_c, data = pilot)
summary(anc_supp_exercise)
# differences in openess to belief change = support for exercise!!
anc_conv_exercise <- aov(exercise_moral_c ~ condition + exercise_belief_c, data = pilot)
summary(anc_conv_exercise)
#no changes in moral conviction

#we see belief change affect differences in support for UHC and Exercise
## interaction is NOT significant for exercise belief change x condition
## however... interaction IS significant for uhc belief change x condition

#we see belief change affect differences in moral conviction change as well for climate
## interaction is NOT significant for climate belief change x condition

#thus... given that this assumption is violated, we re-examine using a multiple regression model instead

#UHC
lm_supp_uhc<-lm(uhc_support ~ condition * uhc_belief_c, data = pilot)
summary(lm_supp_uhc)
#significant effect of openness to belief change
#significant effect of 'pragmatic condition'
#significant intrxn of belief change and piggybacking, moral responsibility, and pragmatic
lm_conv_uhc<-lm(uhc_moral_c ~ condition * uhc_belief_c, data = pilot)
summary(lm_conv_uhc)
#significant effect of 'pragmatic condition'
#significant intrxn of belief change and piggybacking and pragmatic

#Death Penalty
lm_supp_death<-lm(death_support ~ condition * death_belief_c, data = pilot)
summary(lm_supp_death)
#no significant effects of condition or belief change openness
lm_conv_death<-lm(death_moral_c ~ condition * death_belief_c, data = pilot)
summary(lm_conv_death)
#no significant effects of condition or belief change openness


#Climate
lm_supp_climate<-lm(climate_support ~ condition * climate_belief_c, data = pilot)
summary(lm_supp_climate)
#significant intrxn of belief change and hedonic condition
lm_conv_climate<-lm(climate_moral_c ~ condition * climate_belief_c, data = pilot)
summary(lm_conv_climate)
#significant intrxn of belief change and piggybacking/pragmatic


#Exercise
lm_supp_exercise<-lm(exercise_support ~ condition * exercise_belief_c, data = pilot)
summary(lm_supp_exercise)
#significant effect of 'moral piggybacking'
lm_conv_exercise<-lm(exercise_moral_c ~ condition * exercise_belief_c, data = pilot)
summary(lm_conv_exercise)
#no significant effects

#should we check as an exploratory measure, differences in baseline
#openness to belief change/baseline conviction by type of exercise?
## create a data subset to do this
pilot_baselines<-pilot[,c(4,14,24,34,44:47)]
pilot_baselines$ID<-1:length(pilot_baselines$uhc_belief_c)
#reshape wide to long
library(tidyr)
pilot_base_compare1 <-pivot_longer(pilot_baselines, cols=1:4, names_to ="topic", values_to = "value")
pilot_base_compare2 <-pivot_longer(pilot_baselines, cols=5:8, names_to ="topic", values_to = "value")

#for belief change
res.aov <- aov(value ~ topic, data = pilot_base_compare1)
summary(res.aov)
# yes, each topic has differing levels of baseline openness to belief change
# lets throw up a basic graph showing these differences?
baseline_belief_c<-ggplot(pilot_base_compare1, aes(x=topic, y=value, color=topic))
#labels for the x-axis

baseline_xlabs<-c("Climate Change", "Capital Punishment", "Exercise", "Universal Health Care")
baseline_belief_c+geom_boxplot() +labs(
  x = "Topic", 
  y = "Openness to Belief Change", 
  colour = "Topic",
  title = "Baseline Differences in Openness to Belief Change by Topic"
) + scale_color_brewer(palette = "Set1", labels = c("Climate Change", "Capital Punishment", "Exercise", "Universal Health"))+
  scale_x_discrete(labels = baseline_xlabs) + theme_bw()
#greater openness to belief change for UHC!


#for moral conviction
res.aov2 <- aov(value ~ topic, data = pilot_base_compare2)
summary(res.aov2)
#we see the same thing again with baseline differences in moral conviction across topics.

baseline_moral_c<-ggplot(pilot_base_compare2, aes(x=topic, y=value, color=topic))
baseline_moral_c+geom_boxplot() +labs(
  x = "Topic", 
  y = "Moral Conviction", 
  colour = "Topic",
  title = "Baseline Differences in Moral Conviction by Topic"
) + scale_color_brewer(palette = "Set1", labels = c("Climate Change", "Capital Punishment", "Exercise", "Universal Health"))+
  scale_x_discrete(labels = baseline_xlabs) + theme_bw()
# as expected, we see essentially no moral conviction for exercise!

###############
# Tukey's HSD #
###############
install.packages("multcomp")
library(multcomp)

#openness to belief change
TukeyHSD(res.aov)

#moral conviction
TukeyHSD(res.aov)




#we can assess pre-planned pairwise comparisons using contrast coding
#first, ensure that R sees our conditions as factors
pilot$condition<-as.factor(pilot$condition)
levels(pilot$condition)
#making contrasts
contrasts <- rbind(
  "Control vs. Hedonic+Pragmatic" = c(1, -1/2, 0, 0, -1/2),
  "Control vs Moral Conditions" = c(1, 0, -1/2, -1/2, 0),
  "H+P vs Moral Conditions" = c(0, 1/2, -1/2,-1/2, 1/2)
)

#for UHC

#fit a VERY basic model
model <- lm(uhc_moral_c ~ factor(condition), data = pilot)

#get emms
library(emmeans)
emm<-emmeans(model, ~factor(condition))


contrasts <- list(
  "Control vs. Hedonic+Pragmatic" = c(1, -1/2, 0, 0, -1/2),
  "Control vs Moral Conditions" = c(1, 0, -1/2, -1/2, 0),
  "H+P vs Moral Conditions" = c(0, 1/2, -1/2,-1/2, 1/2)
)
#make longer list of contrasts to compare against?
levels(pilot$condition)
contrasts <- list(
  "Control vs. Hedonic" = c(1, -1, 0, 0, 0),
  "Control vs Moral Piggybacking" = c(1, 0, -1, 0, 0),
  "Control vs Moral Responsibility" = c(1, 0, 0,-1, 0),
  "Control vs Pragmatic" = c(1, 0, 0, 0, -1))
########## type up more stuff later

contrast(emm, contrasts, adjust = "bonferroni")

#try again with death penalty
model <- lm(death_moral_c ~ factor(condition), data = pilot)
#get emms
emm<-emmeans(model, ~factor(condition))
contrast(emm, contrasts, adjust = "bonferroni")

#try again with exercise
model <- lm(exercise_moral_c ~ factor(condition), data = pilot)
#get emms
emm<-emmeans(model, ~factor(condition))
contrast(emm, contrasts, adjust = "bonferroni")

#try finally w/ climate change
model <- lm(climate_moral_c ~ factor(condition), data = pilot)
#get emms
emm<-emmeans(model, ~factor(condition))
contrast(emm, contrasts, adjust = "bonferroni")

#looks like the grouped contrasts are NOT IT - however... we can assess individual condition differences
#compared against control as well.

#needing to make our tables for our pre-planned pairwise comparisions
pairwise.t.test(pilot$uhc_moral_c, pilot$condition)
