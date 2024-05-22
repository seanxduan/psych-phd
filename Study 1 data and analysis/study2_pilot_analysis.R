### As per usual, lets start by getting tables setup ###
pilot<-read.csv("study2_pilot_clean.csv")

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
