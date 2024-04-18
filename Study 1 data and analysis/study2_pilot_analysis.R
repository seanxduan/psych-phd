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

