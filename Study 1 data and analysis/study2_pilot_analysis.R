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

### UHC

# Belief Change
plot1b<-ggplot(pilot, aes(x=condition, y=death_belief_c, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Openness to Belief Change on Support for death", 
    colour = "Moral Conviction Manipulation",
    title = "Effect of Moral Conviction Manipulation on Openness to Change Beliefs on death"
  )
plot1b + scale_color_brewer(palette = "Set1")

plot2b<-ggplot(pilot, aes(x=condition, y=death_moral_c, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Moral Conviction on Support for death", 
    colour = "Moral Conviction Manipulation",
    title = "Manipulation on Moral Conviction of death Beliefs"
  )
plot2b + scale_color_brewer(palette = "Set1")

plot3b<-ggplot(pilot, aes(x=condition, y=death_persuadable, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Persuasiveness of Essay", 
    colour = "Moral Conviction Manipulation",
    title = "Persuasiveness of Essay on death by Moral Conviction Manipulation"
  )
plot3b + scale_color_brewer(palette = "Set1")

plot4b<-ggplot(pilot, aes(x=condition, y=death_support, color=condition)) +
  geom_boxplot() +labs(
    x = "Moral Conviction Manipulation", 
    y = "Support for death", 
    colour = "Moral Conviction Manipulation",
    title = "Support of death by Moral Conviction Manipulation"
  )
plot4b + scale_color_brewer(palette = "Set1")


#nice we see exactly what we want to see :)

#lets see how the prescore looks?

plot1_A<-ggplot(pilot, aes(x=utilitarian, y=Pre_P2)) +
  geom_point() + geom_smooth(method = "lm")
plot1_A + scale_color_brewer(palette = "Set1")

#how about the final score?

plot1_B<-ggplot(pilot, aes(x=utilitarian, y=P2)) +
  geom_point() + geom_smooth(method = "lm")
plot1_B + scale_color_brewer(palette = "Set1")

#look @ it for deont.

plot1_C<-ggplot(pilot, aes(x=deontological, y=P2)) +
  geom_point() + geom_smooth(method = "lm")
plot1_C + scale_color_brewer(palette = "Set1")

#now lets see total change and deont/util?

plot1_D<-ggplot(pilot, aes(x=deontological, y=UHC)) +
  geom_point() + geom_smooth(method = "lm")
plot1_D + scale_color_brewer(palette = "Set1")

plot1_E<-ggplot(pilot, aes(x=deontological, y=UHC)) +
  geom_point() + geom_smooth(method = "lm")
plot1_E + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")+ 
  labs(
    x = "Deontological Orientation", 
    y = "Support for Universal Health Care", 
    title = "Effect of Deontological Orientation on Change in Support for Universal Health Care by Social Consensus Condition",
    subtitle = "There is no significant interaction between deontological leaning and condition"
  )

plot1_F<-ggplot(pilot, aes(x=utilitarian, y=UHC)) +
  geom_point() + geom_smooth(method = "lm")
plot1_F + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")+ 
  labs(
    x = "Utilitarian Orientation", 
    y = "Support for Universal Health Care", 
    title = "Effect of Utilitarian Orientation on Change in Support for Universal Health Care by Social Consensus Condition",
    subtitle = "There is a significant interaction between utilitarian leaning and condition"
  )

#SMELLS LIKE AN INTERACTION!!?!?!?!!!
#in the high social consensus situation, you are relatively less influenced by
#social pressure, the MORE utilitarian your baseline orientation is (this is NOT what the lit says?!?)

#in the LOW social consensus we do not see an effect of greater utilitarianism.

#so, the more utiltarian you are, the more likely you are to ORIGINALLY support UHC regardless of 
#anything else

#THUS, if you are likely to support UHC more, you are less likely to be influenced by things

plot1_G<-ggplot(pilot, aes(x=utilitarian, y=P2)) +
  geom_point() + geom_smooth(method = "lm")
plot1_G + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")

plot1_H<-ggplot(pilot, aes(x=utilitarian, y=Pre_P2)) +
  geom_point() + geom_smooth(method = "lm")
plot1_H + 
  labs(
    x = "Utilitarian Orientation", 
    y = "Support for Universal Health Care", 
    title = "Effect of Utilitarian Orientation on Initial Support for Universal Health Care",
    subtitle = "Utilitarian leaning predicts initial support for UHC"
  )

plot1_H2<-ggplot(pilot, aes(x=deontological, y=Pre_P2)) +
  geom_point() + geom_smooth(method = "lm")
plot1_H2 + 
  labs(
    x = "Deontological Orientation", 
    y = "Support for Universal Health Care", 
    title = "Effect of Deontological Orientation on Initial Support for Universal Health Care",
    subtitle = "Deontological leaning predicts initial support for UHC"
  )

##### Is it plausible that we're hitting a ceiling effect?
#e.g. for those that support UHC, our intervention doesn't do very much?

plot2<-ggplot(pilot_model_long, aes(x=condition, y=CLIM_SUP, color=condition)) +
  geom_boxplot() 
plot2 + facet_wrap(~ Time)+ scale_color_brewer(palette = "Set1") + 
  labs(
    x = "Social Consensus Condition", 
    y = "Support for Anthropoegenic Climate Change", 
    colour = "Social Consensus Condition",
    title = "Effect of Social Consensus on Support for Anthropoegenic Climate Change",
    subtitle = "Social consensus has no effect on support for Anthropoegenic Climate Change"
  )
plot2_E<-ggplot(pilot, aes(x=deontological, y=climate)) +
  geom_point() + geom_smooth(method = "lm")
plot2_E + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")+ 
  labs(
    x = "Deontological Orientation", 
    y = "Support for Anthropoegenic Climate Change", 
    title = "Effect of Deontological Orientation on Change in Support for Anthropoegenic Climate Change by Social Consensus Condition",
    subtitle = "There is no significant interaction between deontological leaning and condition"
  )

plot2_F<-ggplot(pilot, aes(x=utilitarian, y=climate)) +
  geom_point() + geom_smooth(method = "lm")
plot2_F + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")+ 
  labs(
    x = "Utilitarian Orientation", 
    y = "Support for Anthropoegenic Climate Change", 
    title = "Effect of Utilitarian Orientation on Change in Support for Anthropoegenic Climate Change by Social Consensus Condition",
    subtitle = "There is a significant interaction between Utilitarian leaning and condition"
  )

plot2_H<-ggplot(pilot, aes(x=utilitarian, y=Pre_P1)) +
  geom_point() + geom_smooth(method = "lm")
plot2_H + 
  labs(
    x = "Utilitarian Orientation", 
    y = "Support for Anthropoegenic Climate Change", 
    title = "Effect of Utilitarian Orientation on Initial Support for Anthropoegenic Climate Change",
    subtitle = "Utilitarian leaning predicts initial support for Anthropoegenic Climate Change"
  )

plot2_H2<-ggplot(pilot, aes(x=deontological, y=Pre_P1)) +
  geom_point() + geom_smooth(method = "lm")
plot2_H2 + 
  labs(
    x = "Deontological Orientation", 
    y = "Support for Anthropoegenic Climate Change", 
    title = "Effect of Deontological Orientation on Initial Support for Anthropoegenic Climate Change",
    subtitle = "Deontological leaning predicts initial support for Anthropoegenic Climate Change"
  )

###################

plot3<-ggplot(pilot_model_long, aes(x=condition, y=DEATH_SUP, color=condition)) +
  geom_boxplot() 
plot3 + facet_wrap(~ Time)+ scale_color_brewer(palette = "Set1")+ 
  labs(
    x = "Social Consensus Condition", 
    y = "Support for Capital Punishment", 
    colour = "Social Consensus Condition",
    title = "Effect of Social Consensus on Support for Capital Punishment",
    subtitle = "Social consensus has a significant effect on support for Capital Punishment"
  )



plot3_H<-ggplot(pilot, aes(x=utilitarian, y=Pre_P3)) +
  geom_point() + geom_smooth(method = "lm")
plot3_H + 
  labs(
    x = "Utilitarian Orientation", 
    y = "Support for Capital Punishment", 
    title = "Effect of Utilitarian Orientation on Initial Support for Capital Punishment",
    subtitle = "Utilitarian leaning predicts initial support for Capital Punishment"
  )

plot3_H2<-ggplot(pilot, aes(x=deontological, y=Pre_P3)) +
  geom_point() + geom_smooth(method = "lm")
plot3_H2 + 
  labs(
    x = "Deontological Orientation", 
    y = "Support for Capital Punishment", 
    title = "Effect of Deontological Orientation on Initial Support for Capital Punishment",
    subtitle = "Deontological leaning predicts initial support for Capital Punishment"
  )

plot3_E<-ggplot(pilot, aes(x=deontological, y=death)) +
  geom_point() + geom_smooth(method = "lm")
plot3_E + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")+ 
  labs(
    x = "Deontological Orientation", 
    y = "Support for Capital Punishment", 
    title = "Effect of Deontological Orientation on Change in Support for Capital Punishment by Social Consensus Condition",
    subtitle = "There is no significant interaction between deontological leaning and condition"
  )

plot3_F<-ggplot(pilot, aes(x=utilitarian, y=death)) +
  geom_point() + geom_smooth(method = "lm")
plot3_F + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")+ 
  labs(
    x = "Utilitarian Orientation", 
    y = "Support for Anthropoegenic Climate Change", 
    title = "Effect of Utilitarian Orientation on Change in Support for Capital Punishment by Social Consensus Condition",
    subtitle = "There is a significant interaction between Utilitarian leaning and condition"
  )

###################


plot4<-ggplot(pilot_model_long, aes(x=condition, y=SLAVE_SUP, color=condition)) +
  geom_boxplot() 
plot4 + facet_wrap(~ Time)+ scale_color_brewer(palette = "Set1")+ 
  labs(
    x = "Social Consensus Condition", 
    y = "Support for Slavery", 
    colour = "Social Consensus Condition",
    title = "Effect of Being in a Social Consensus Condition manipulating other topics on Support for Slavery",
    subtitle = "Social consensus was not directly manipulated for Slavery"
  )
plot4_H<-ggplot(pilot, aes(x=utilitarian, y=Pre_P4)) +
  geom_point() + geom_smooth(method = "lm")
plot4_H + 
  labs(
    x = "Utilitarian Orientation", 
    y = "Support for Slavery", 
    title = "Effect of Utilitarian Orientation on Initial Support for Slavery",
    subtitle = "Utilitarian leaning predicts initial support for Capital Punishment"
  )

plot4_H2<-ggplot(pilot, aes(x=deontological, y=Pre_P4)) +
  geom_point() + geom_smooth(method = "lm")
plot4_H2 + 
  labs(
    x = "Deontological Orientation", 
    y = "Support for Slavery", 
    title = "Effect of Deontological Orientation on Initial Support for Slavery",
    subtitle = "Deontological leaning does not predict initial support for Slavery"
  )

plot4_E<-ggplot(pilot, aes(x=deontological, y=slave)) +
  geom_point() + geom_smooth(method = "lm")
plot4_E + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")+ 
  labs(
    x = "Deontological Orientation", 
    y = "Support for Slavery", 
    title = "Effect of Deontological Orientation on Change in Support for Slavery by Social Consensus Condition",
    subtitle = "There is no significant interaction between deontological leaning and condition"
  )

plot4_F<-ggplot(pilot, aes(x=utilitarian, y=slave)) +
  geom_point() + geom_smooth(method = "lm")
plot4_F + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")+ 
  labs(
    x = "Utilitarian Orientation", 
    y = "Support for Slavery", 
    title = "Effect of Utilitarian Orientation on Change in Support for Slavery by Social Consensus Condition",
    subtitle = "There is no significant interaction between Utilitarian leaning and condition"
  )
