### As per usual, lets start by getting tables setup ###

#doing it now WITHOUT the dupes
pilot_long<-read.csv("study_3_long_clean_nd.csv")
pilot<-read.csv("study_3_clean_nd.csv")

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
plot1a<-ggplot(pilot, aes(x=conv_cond, y=in_uhc_change, color=consen_cond)) +
  geom_boxplot() +labs(
      )
plot1a + scale_color_brewer(palette = "Set1")

#not a lot of value in looking at belief change outright?

#persuasiveness?
#actually - what we want to do is compare how belief change shifts ACROSS the conditions
#same for the other things actually!
#thus... we need a melt for this by condition?
#actually yes, more data reshifting needed

#lets get this data re-shifted?

library(reshape2)
library(data.table)

pilot_xtra_long<-data.table::melt(setDT(pilot_long),measure=patterns("_familiar", "_change", "_support", "_mconv"), 
                      value.name=c("topic_familiarity", "topic_changeable", "topic_support", "topic_moral_c"),
                      variable.name="topic")
#subset out the 'pre' results so we can see baseline differences

#rename the topics lol

pilot_xtra_long$topic<-str_replace_all(pilot_xtra_long$topic, c("1"= "uhc", "2" = "cap", "3" = "ai"))

pilot_xtra_long<-subset(pilot_xtra_long,time == 'pre')

#lets try our original graph but w/ a couple of baseline changes!

plot1a<-ggplot(pilot_xtra_long, aes(x=topic, y=topic_familiarity, color=topic)) +
  geom_boxplot() +labs(
  )
plot1a + scale_color_brewer(palette = "Set1")

plot1b<-ggplot(pilot_xtra_long, aes(x=topic, y=topic_changeable, color=topic)) +
  geom_boxplot() +labs(
  )
plot1b + scale_color_brewer(palette = "Set1")

# lets look at baseline levels of moral conviction and support!

plot1c<-ggplot(pilot_xtra_long, aes(x=topic, y=topic_support, color=topic)) +
  geom_boxplot() +labs(
  )
plot1c + scale_color_brewer(palette = "Set1")
#a priori support is nice and high for uhc, but p strongly against the other two!

plot1d<-ggplot(pilot_xtra_long, aes(x=topic, y=topic_moral_c, color=topic)) +
  geom_boxplot() +labs(
  )
plot1d + scale_color_brewer(palette = "Set1")

##################################################################
## lets see how topic support shifts as a product of condition? ## 
##################################################################

##### UHC ######

#subset of post results only?
plot2a<-ggplot(pilot_long, aes(x=time, y=uhc_support, color=conv_cond)) +
  geom_boxplot() +labs(
  )

plot2a + scale_color_brewer(palette = "Set1")

plot2a<-ggplot(pilot_long, aes(x=time, y=uhc_support, color=consen_cond)) +
  geom_boxplot() +labs(
  )

plot2a + scale_color_brewer(palette = "Set1")
#consensus seems to impact, but NOT conviction? for at least general support

plot2b<-ggplot(pilot_long, aes(x=time, y=uhc_mconv, color=conv_cond)) +
  geom_boxplot() +labs(
  )

plot2b + scale_color_brewer(palette = "Set1")

plot2b<-ggplot(pilot_long, aes(x=time, y=uhc_mconv, color=consen_cond)) +
  geom_boxplot() +labs(
  )

plot2b + scale_color_brewer(palette = "Set1")

###############

#### Death ####

plot3a<-ggplot(pilot_long, aes(x=time, y=cap_support, color=conv_cond)) +
  geom_boxplot() +labs(
  )

plot3a + scale_color_brewer(palette = "Set1")

plot3a<-ggplot(pilot_long, aes(x=time, y=cap_support, color=consen_cond)) +
  geom_boxplot() +labs(
  )

plot3a + scale_color_brewer(palette = "Set1")
#consensus seems to impact, but NOT conviction? for at least general support

plot3b<-ggplot(pilot_long, aes(x=time, y=cap_mconv, color=conv_cond)) +
  geom_boxplot() +labs(
  )

plot3b + scale_color_brewer(palette = "Set1")

plot3b<-ggplot(pilot_long, aes(x=time, y=cap_mconv, color=consen_cond)) +
  geom_boxplot() +labs(
  )

plot3b + scale_color_brewer(palette = "Set1")

############

#### AI ####


plot4a<-ggplot(pilot_long, aes(x=time, y=ai_support, color=conv_cond)) +
  geom_boxplot() +labs(
  )

plot4a + scale_color_brewer(palette = "Set1")

plot4a<-ggplot(pilot_long, aes(x=time, y=ai_support, color=consen_cond)) +
  geom_boxplot() +labs() +facet_wrap(vars(conv_cond)) #added facet_wrap, implies that there is no interaction
#since there is no actual change - no interaction, HOWEVER, the base differences jump out as worthwhile for
#a pre-post effect.

plot4a + scale_color_brewer(palette = "Set1")
#consensus seems to impact, but NOT conviction? for at least general support

plot4b<-ggplot(pilot_long, aes(x=time, y=ai_mconv, color=conv_cond)) +
  geom_boxplot() +labs(
  )

plot4b + scale_color_brewer(palette = "Set1")

plot4b<-ggplot(pilot_long, aes(x=time, y=ai_mconv, color=consen_cond)) +
  geom_boxplot() +labs(
  )

plot4b + scale_color_brewer(palette = "Set1")

#############################
#############################
#############################

#time to do basic models!

#UHC
sup_uhc<-lm(fn_uhc_supp ~ conv_cond*consen_cond+in_uhc_supp+in_uhc_change+in_uhc_familiar+utilitarian+deontological, data = pilot)
summary(sup_uhc)

### secret bonus for testing if mc level actually affects stuff)
sup_uhc3<-lm(fn_uhc_supp ~ conv_cond+fn_uhc_conviction*consen_cond+in_uhc_supp+in_uhc_change+in_uhc_familiar+utilitarian+deontological, data = pilot)
summary(sup_uhc3)

#huh, conviction plots tightly to final support
#lets see if this holds up for initial or final as well
cor(pilot$in_uhc_supp,pilot$in_uhc_conviction)
cor(pilot$fn_uhc_supp,pilot$fn_uhc_conviction)

cor(pilot$in_uhc_supp,pilot$fn_uhc_supp)
cor(pilot$in_uhc_conviction,pilot$fn_uhc_conviction)

plot(pilot$in_uhc_supp,pilot$in_uhc_conviction)

plot(pilot$fn_uhc_supp,pilot$fn_uhc_conviction)

#very... interesting
###


conv_uhc<-lm(fn_uhc_conviction ~ conv_cond*consen_cond+in_uhc_conviction+in_uhc_change+in_uhc_familiar+utilitarian+deontological, data = pilot)
summary(conv_uhc)

mini_conv_uhc<-lm(fn_uhc_conviction ~ conv_cond+in_uhc_supp+in_uhc_conviction, data = pilot)
summary(mini_conv_uhc)

#death
sup_cap<-lm(fn_cap_supp ~ conv_cond*consen_cond+in_cap_supp+in_cap_change+in_cap_familiar+utilitarian+deontological, data = pilot)
summary(sup_cap)

###
sup_cap2<-lm(fn_cap_supp ~ conv_cond+fn_cap_conviction*consen_cond+in_cap_supp+in_cap_change+in_cap_familiar+utilitarian+deontological, data = pilot)
summary(sup_cap2)


cor(pilot$in_cap_supp,pilot$in_cap_conviction)
cor(pilot$fn_cap_supp,pilot$fn_cap_conviction)

cor(pilot$in_cap_supp,pilot$fn_cap_supp)
cor(pilot$in_cap_conviction,pilot$fn_cap_conviction)

plot(pilot$in_cap_supp,pilot$in_cap_conviction)

plot(pilot$fn_cap_supp,pilot$fn_cap_conviction)
###



conv_cap<-lm(fn_cap_conviction ~ conv_cond*consen_cond+in_cap_conviction+in_cap_change+in_cap_familiar+utilitarian+deontological, data = pilot)
summary(conv_cap)

#ai
sup_ai<-lm(fn_ai_supp ~ conv_cond*consen_cond+in_ai_supp+in_ai_change+in_ai_familiar+utilitarian+deontological, data = pilot)
summary(sup_ai)

sup_ai2<-lm(fn_ai_supp ~ conv_cond+fn_ai_conviction*consen_cond+in_ai_supp+in_ai_change+in_ai_familiar+utilitarian+deontological, data = pilot)
summary(sup_ai2)


cor(pilot$in_ai_supp,pilot$in_ai_conviction)
cor(pilot$fn_ai_supp,pilot$fn_ai_conviction)

cor(pilot$in_ai_supp,pilot$fn_ai_supp)
cor(pilot$in_ai_conviction,pilot$fn_ai_conviction)

plot(pilot$in_ai_supp,pilot$in_ai_conviction)

plot(pilot$fn_ai_supp,pilot$fn_ai_conviction)

### very interdasting - good that we looked at it regardless


conv_ai<-lm(fn_ai_conviction ~ conv_cond*consen_cond+in_ai_conviction+in_ai_change+in_ai_familiar+utilitarian+deontological, data = pilot)
summary(conv_ai)

#### Thinking about what other tests and graphs we want to assess?
#lets look at some of our similar analysis from earlier works



#####some expansion style graphs#######

#scatterplot for final AI support as affected by openness to AI change#

plot6a<-ggplot(pilot, aes(x=in_ai_change, y=fn_ai_supp, color=consen_cond)) +
  geom_point() +labs(
  ) + geom_smooth( method = "lm")

plot6a + scale_color_brewer(palette = "Set1")


#scatterplot for final capital punishment support as affected by openness to cap change#

plot6b<-ggplot(pilot, aes(x=in_cap_change, y=fn_cap_supp, color=consen_cond)) +
  geom_point() +labs(
  ) + geom_smooth( method = "lm")

plot6b + scale_color_brewer(palette = "Set1")


#histograms of change
hist(pilot$fn_ai_conviction-pilot$in_ai_conviction)
hist(pilot$fn_uhc_conviction-pilot$in_uhc_conviction)
hist(pilot$fn_cap_conviction-pilot$in_cap_conviction)
#moral conviction did NOT seem to change across any of our conditions?

#we can test for amount of change pre-post by conviction and consensus condition as our NEXT step

#can we see the same on support?

hist(pilot$fn_ai_supp-pilot$in_ai_supp)
#can we sep this hist by soc consensus?
ai_hi<-subset(pilot, consen_cond=="SocialConsensus-High")
ai_lo<-subset(pilot, consen_cond=="SocialConsensus-Low")
hist(ai_hi$fn_ai_supp-ai_hi$in_ai_supp)
mean(ai_hi$fn_ai_supp-ai_hi$in_ai_supp)

hist(ai_lo$fn_ai_supp-ai_lo$in_ai_supp)
mean(ai_lo$fn_ai_supp-ai_lo$in_ai_supp)

hist(pilot$fn_uhc_supp-pilot$in_uhc_supp)
hist(pilot$fn_cap_supp-pilot$in_cap_supp)
#there does seem to be a difference... maybe best way to check is... some sort of t-test?
#just want to confirm that there are differences in support change between our conditions
#maybe try it again w/ all 4 'blocks' and see if there are differences?

#do a 

pilot$comb_cond<-paste(pilot$consen_cond,pilot$conv_cond)
#subset first
#

pilot_diff_compare<-pivot_longer(pilot, cols=c(32:34), names_to ="topic", values_to = "value")

#we did it backwards, split first then subset by topic

#we're going to do it again so we can have the same assessment of changes in moral conviction!!!################!!!


#######################################
### UHC DIFFERENCE SCORES BY TOPIC  ###
#######################################

uhc_diff<-subset(pilot_diff_compare, topic=="uhc_diff")
#differences in UHC change score by condition
res.aov <- aov(value ~ comb_cond, data = uhc_diff)

summary(res.aov)
#groups don't seem to have differences in UHC change scores.

# lets throw up a basic graph showing these differences?
uhc_diff_gfx<-ggplot(uhc_diff, aes(x=comb_cond, y=value, color=comb_cond))

baseline_xlabs<-c("SC-High + Moral", "SC-High + Prag.", "SC-Low + Moral", "SC-Low + Prag.")

uhc_diff_gfx+geom_boxplot() +labs(
  x = "Manipulation Condition", 
  y = "Change in Support for UHC", 
  colour = "Topic",
  title = "Changes in Support for Universal Health Care Across All Manipulation Conditions"
) + scale_color_brewer(palette = "Set1", labels = c("High Social Consensus + Moral Essay", "High Social Consensus + Pragmatic Essay",
                                                    "Low Social Consensus + Moral Essay", "Low Social Consensus + Pragmatic Essay"))+
  scale_x_discrete(labels = baseline_xlabs) + theme_bw()+scale_y_continuous(limits = c(-50, 50))
#need to relabel the scale but mostly there
#clearly shows that there isn't really big differences in 'difference scores' between our four conditions'
#but also shows that there is significant change in value regardless

#do it again for the other factors?


## do changes for support by 'openness to change'

uhc_diff_open<-ggplot(uhc_diff, aes(x=in_uhc_change, y=value))

uhc_diff_open+geom_jitter(width = .2, height = .2, alpha = 0.5, size = 2) + geom_smooth(method = lm, linetype = "dashed", se = FALSE) +labs(
  x = "Openness to Belief Change for UHC", 
  y = "Change in Support for UHC", 
  colour = "Topic",
  title = "Changes in Support for Universal Health Care by Openness to Belief Change")+
  scale_color_brewer(palette = "Set1")+ theme_bw()+scale_y_continuous(limits = c(-50, 50))











#################################################
### DEATH PENALTY DIFFERENCE SCORES BY TOPIC  ###
#################################################

cap_diff<-subset(pilot_diff_compare, topic=="cap_diff")
#differences in UHC change score by condition
res.aov <- aov(value ~ comb_cond, data = cap_diff)

summary(res.aov)
#groups don't seem to have differences in UHC change scores.


# lets throw up a basic graph showing these differences?
cap_diff_gfx<-ggplot(cap_diff, aes(x=comb_cond, y=value, color=comb_cond))

baseline_xlabs<-c("SC-High + Moral", "SC-High + Prag.", "SC-Low + Moral", "SC-Low + Prag.")

cap_diff_gfx+geom_boxplot() +labs(
  x = "Manipulation Condition", 
  y = "Change in Support for Capital Punishment", 
  colour = "Topic",
  title = "Changes in Support for Capital Punishment Across All Manipulation Conditions"
) + scale_color_brewer(palette = "Set1", labels = c("High Social Consensus + Moral Essay", "High Social Consensus + Pragmatic Essay",
                                                    "Low Social Consensus + Moral Essay", "Low Social Consensus + Pragmatic Essay"))+
  scale_x_discrete(labels = baseline_xlabs) + theme_bw()+scale_y_continuous(limits = c(-50, 50))



cap_diff_open<-ggplot(cap_diff, aes(x=in_cap_change, y=value))

cap_diff_open+geom_jitter(width = .2, height = .2, alpha = 0.5, size = 2) + geom_smooth(method = lm, linetype = "dashed", se = FALSE) +labs(
  x = "Openness to Belief Change for Capital Punishment", 
  y = "Change in Support for Capital Punishment", 
  colour = "Topic",
  title = "Changes in Support for Capital Punishment by Openness to Belief Change")+
  scale_color_brewer(palette = "Set1")+ theme_bw()+scale_y_continuous(limits = c(-50, 50))


######################################
### AI DIFFERENCE SCORES BY TOPIC  ###
######################################

ai_diff<-subset(pilot_diff_compare, topic=="ai_diff")
#differences in UHC change score by condition
res.aov <- aov(value ~ comb_cond, data = ai_diff)

summary(res.aov)
#groups don't seem to have differences in UHC change scores.


# lets throw up a basic graph showing these differences?
ai_diff_gfx<-ggplot(ai_diff, aes(x=comb_cond, y=value, color=comb_cond))

baseline_xlabs<-c("SC-High + Moral", "SC-High + Prag.", "SC-Low + Moral", "SC-Low + Prag.")

ai_diff_gfx+geom_boxplot() +labs(
  x = "Manipulation Condition", 
  y = "Change in Support for Usage of AI in the Workplace", 
  colour = "Topic",
  title = "Changes in Support for Usage of Artificial Intelligence in the Workplace Across All Manipulation Conditions"
) + scale_color_brewer(palette = "Set1", labels = c("High Social Consensus + Moral Essay", "High Social Consensus + Pragmatic Essay",
                                                    "Low Social Consensus + Moral Essay", "Low Social Consensus + Pragmatic Essay"))+
  scale_x_discrete(labels = baseline_xlabs) + theme_bw()+scale_y_continuous(limits = c(-50, 50))

ai_diff_open<-ggplot(ai_diff, aes(x=in_ai_change, y=value))

ai_diff_open+geom_jitter(width = .2, height = .2, alpha = 0.5, size = 2) + geom_smooth(method = lm, linetype = "dashed", se = FALSE) +labs(
  x = "Openness to Belief Change for Usage of AI in the Workplace", 
  y = "Change in Support for Usage of AI in the Workplace", 
  colour = "Topic",
  title = "Changes in Support for Usage of AI in the Workplace by Openness to Belief Change")+
  scale_color_brewer(palette = "Set1")+ theme_bw()+scale_y_continuous(limits = c(-50, 50))


#####
#TL:DR
# all four condition combinations improved support
# no differences by condition.... because all of them increased support a comparable amount
#####

#also! perhaps scale the y-axis and x-axis to fixed values so we can easily compare across the three sets of graphs

###################################################
# Differences in Moral Conviction Change by Topic #
###################################################

pilot$uhc_conv_diff<-(pilot$fn_uhc_conviction-pilot$in_uhc_conviction)
pilot$ai_conv_diff<-(pilot$fn_ai_conviction-pilot$in_ai_conviction)
pilot$cap_conv_diff<-(pilot$fn_cap_conviction-pilot$in_cap_conviction)

pilot_conv_compare<-pivot_longer(pilot, cols=c(36:38), names_to ="topic", values_to = "value")


######## for UHC #########

uhc_mc<-subset(pilot_conv_compare, topic=="uhc_conv_diff")
#differences in UHC change score by condition
res.aov <- aov(value ~ comb_cond, data = uhc_mc)

summary(res.aov)
#hmm seems like some real fucking close to differences here.
#lets look at the graph to compare?

uhc_conv_gfx<-ggplot(uhc_mc, aes(x=comb_cond, y=value, color=comb_cond))

baseline_xlabs<-c("SC-High + Moral", "SC-High + Prag.", "SC-Low + Moral", "SC-Low + Prag.")

uhc_conv_gfx+geom_boxplot() +labs(
  x = "Manipulation Condition", 
  y = "Change in Moral Conviction", 
  colour = "Topic",
  title = "Changes in Percieved Moral Conviction regarding Universal Health Care Across All Manipulation Conditions"
) + scale_color_brewer(palette = "Set1", labels = c("High Social Consensus + Moral Essay", "High Social Consensus + Pragmatic Essay",
                                                    "Low Social Consensus + Moral Essay", "Low Social Consensus + Pragmatic Essay"))+
  scale_x_discrete(labels = baseline_xlabs) + theme_bw()+scale_y_continuous(limits = c(-50, 50))

#running a quick tukey HSD to see what's up
library(multcomp)
TukeyHSD(res.aov)
#nope, tukey says no significant differences.

#run the same graph but for utilitarian orientation

uhc_conv_util<-ggplot(uhc_mc, aes(x=utilitarian, y=value))

uhc_conv_util+geom_jitter(width = .2, height = .2, alpha = 0.5, size = 2) +
  geom_smooth(method = lm, linetype = "dashed", se = FALSE) +
  labs(
  x = "Utiltarian Orientation", 
  y = "Change in Moral Conviction", 
  colour = "Topic",
  title = "Moral Conviction Change for UHC by Utilitarian Orientation")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+scale_y_continuous(limits = c(-50, 50))+
  scale_x_continuous(limits = c(1, 5))

#deontological gfx

uhc_conv_deon<-ggplot(uhc_mc, aes(x=deontological, y=value))

uhc_conv_deon+geom_jitter(width = .2, height = .2, alpha = 0.5, size = 2) +
  geom_smooth(method = lm, linetype = "dashed", se = FALSE) +
  labs(
    x = "Deontological Orientation", 
    y = "Change in Moral Conviction", 
    colour = "Topic",
    title = "Moral Conviction Change for UHC by Deontological Orientation")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+scale_y_continuous(limits = c(-50, 50))+
  scale_x_continuous(limits = c(1, 5))



######## for capital punishment #########

cap_mc<-subset(pilot_conv_compare, topic=="cap_conv_diff")
#differences in UHC change score by condition
res.aov <- aov(value ~ comb_cond, data = cap_mc)
summary(res.aov)
#no major differences.

cap_conv_gfx<-ggplot(cap_mc, aes(x=comb_cond, y=value, color=comb_cond))

cap_conv_gfx+geom_boxplot() +labs(
  x = "Manipulation Condition", 
  y = "Change in Moral Conviction", 
  colour = "Topic",
  title = "Changes in Percieved Moral Conviction regarding Capital Punishment Across All Manipulation Conditions"
) + scale_color_brewer(palette = "Set1", labels = c("High Social Consensus + Moral Essay", "High Social Consensus + Pragmatic Essay",
                                                    "Low Social Consensus + Moral Essay", "Low Social Consensus + Pragmatic Essay"))+
  scale_x_discrete(labels = baseline_xlabs) + theme_bw()+scale_y_continuous(limits = c(-50, 50))

cap_conv_util<-ggplot(cap_mc, aes(x=utilitarian, y=value))

cap_conv_util+geom_jitter(width = .2, height = .2, alpha = 0.5, size = 2) +
  geom_smooth(method = lm, linetype = "dashed", se = FALSE) +
  labs(
    x = "Utiltarian Orientation", 
    y = "Change in Moral Conviction", 
    colour = "Topic",
    title = "Changes in Percieved Moral Conviction regarding Capital Punishment by Utilitarian Orientation")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+scale_y_continuous(limits = c(-50, 50))+
  scale_x_continuous(limits = c(1, 5))



cap_conv_deon<-ggplot(cap_mc, aes(x=deontological, y=value))

cap_conv_deon+geom_jitter(width = .2, height = .2, alpha = 0.5, size = 2) +
  geom_smooth(method = lm, linetype = "dashed", se = FALSE) +
  labs(
    x = "Deontological Orientation", 
    y = "Change in Moral Conviction", 
    colour = "Topic",
    title = "Moral Conviction Change for Capital Punishment by Deontological Orientation")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+scale_y_continuous(limits = c(-50, 50))+
  scale_x_continuous(limits = c(1, 5))


####### for AI in the workplace #######

ai_mc<-subset(pilot_conv_compare, topic=="ai_conv_diff")
#differences in UHC change score by condition
res.aov <- aov(value ~ comb_cond, data = ai_mc)
summary(res.aov)
#differences, mild but there?

ai_conv_gfx<-ggplot(ai_mc, aes(x=comb_cond, y=value, color=comb_cond))

ai_conv_gfx+geom_boxplot() +labs(
  x = "Manipulation Condition", 
  y = "Change in Moral Conviction", 
  colour = "Topic",
  title = "Changes in Percieved Moral Conviction regarding usage of Artificial Intelligence in the Workforce Across All Manipulation Conditions"
) + scale_color_brewer(palette = "Set1", labels = c("High Social Consensus + Moral Essay", "High Social Consensus + Pragmatic Essay",
                                                    "Low Social Consensus + Moral Essay", "Low Social Consensus + Pragmatic Essay"))+
  scale_x_discrete(labels = baseline_xlabs) + theme_bw()+scale_y_continuous(limits = c(-50, 50))

TukeyHSD(res.aov)

ai_conv_util<-ggplot(ai_mc, aes(x=utilitarian, y=value))

ai_conv_util+geom_jitter(width = .2, height = .2, alpha = 0.5, size = 2) +
  geom_smooth(method = lm, linetype = "dashed", se = FALSE) +
  labs(
    x = "Utiltarian Orientation", 
    y = "Change in Moral Conviction", 
    colour = "Topic",
    title = "Changes in Percieved Moral Conviction regarding AI by Utilitarian Orientation")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+scale_y_continuous(limits = c(-50, 50))+
  scale_x_continuous(limits = c(1, 5))


ai_conv_deon<-ggplot(ai_mc, aes(x=deontological, y=value))

ai_conv_deon+geom_jitter(width = .2, height = .2, alpha = 0.5, size = 2) +
  geom_smooth(method = lm, linetype = "dashed", se = FALSE) +
  labs(
    x = "Deontological Orientation", 
    y = "Change in Moral Conviction", 
    colour = "Topic",
    title = "Moral Conviction Change for Usage of AI in the Workplace by Deontological Orientation")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+scale_y_continuous(limits = c(-50, 50))+
  scale_x_continuous(limits = c(1, 5))



############ maybe one more set of graphs showing improvement pre-post?








#labels for the x-axis
#####################
#####################


#run a quick test to see how AI support is affected by things
sup_ai2<-lm(ai_support ~ conv_cond*consen_cond+in_ai_change+in_ai_familiar+utilitarian+deontological+time, data = pilot_long)
summary(sup_ai2)
#ok, so we do see an affect of pre-post, both interventions DID increase support

#try an interaction w/ time?
sup_ai3<-lm(ai_support ~ conv_cond*consen_cond*time+in_ai_change+in_ai_familiar+utilitarian+deontological, data = pilot_long)
summary(sup_ai3)


# we can test similar things for conviction?
mconv_ai2<-lm(ai_mconv ~ conv_cond*consen_cond+in_ai_change+in_ai_familiar+utilitarian+deontological+time, data = pilot_long)
summary(mconv_ai2)
#same for here as well!

#see how this holds up for UHC and cap?

## uhc
sup_uhc2<-lm(uhc_support ~ conv_cond*consen_cond+in_uhc_change+in_uhc_familiar+utilitarian+deontological+time, data = pilot_long)
summary(sup_uhc2)

#our two interventions were not significantly different from EACH OTHER
#however... the interventions DID have a significant different from baseline.

mconv_uhc2<-lm(uhc_mconv ~ conv_cond*consen_cond+in_uhc_change+in_uhc_familiar+utilitarian+deontological+time, data = pilot_long)
summary(mconv_uhc2)
## cap
sup_cap2<-lm(cap_support ~ conv_cond*consen_cond+in_cap_change+in_cap_familiar+utilitarian+deontological+time, data = pilot_long)
summary(sup_cap2)
mconv_cap2<-lm(cap_mconv ~ conv_cond*consen_cond+in_cap_change+in_cap_familiar+utilitarian+deontological+time, data = pilot_long)
summary(mconv_cap2)


#########
# lets try some additional tests as ethan recommended
#########

#first y = difference b/w pre and post treatment scores 
#e.g. "compare the differences between pre and post treatments across the treatments!"

#create a new y that indicates that!

pilot$uhc_diff<-(pilot$fn_uhc_supp - pilot$in_uhc_supp)
pilot$cap_diff<-(pilot$fn_cap_supp - pilot$in_cap_supp)
pilot$ai_diff<-(pilot$fn_ai_supp - pilot$in_ai_supp)

#then run the SAME analysis using the diff as the outcome var
diff_uhc<-lm(uhc_diff ~ conv_cond*consen_cond+in_uhc_change+in_uhc_familiar+utilitarian+deontological, data = pilot)
summary(diff_uhc)

diff_cap<-lm(cap_diff ~ conv_cond*consen_cond+in_cap_change+in_cap_familiar+utilitarian+deontological, data = pilot)
summary(diff_cap)

diff_ai<-lm(ai_diff ~ conv_cond*consen_cond+in_ai_change+in_ai_familiar+utilitarian+deontological, data = pilot)
summary(diff_ai)
#we see that none of the shit looks good lmao
#well - at least our initial understanding is that this ISN"T the way to examine it!

#ok - facts, we need to get our demographic table up and running too lol
#lets get it happening!

library("gtsummary")

#peel out only the demographic info?

d1<-pilot[,c(17:21)]
d1$Age_High<-as.numeric(d1$Age_High)
d1$Pol_High_1<-as.numeric(d1$Pol_High_1)

d1 %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / ({p}%)"),
    digits = all_continuous() ~ 2,
    label = c(School_High ~ "School Year",
              Race_High ~ "Race",
              Gender_High ~"Gender",
              Age_High ~ "Age"),
    missing_text = "(Missing)",
    sort = list(everything() ~ "frequency")
  ) %>% bold_labels() %>% modify_header(label = "**Social Consensus Condition**")
str(d1)

d1 %>%
  tbl_summary(label = c(School_High ~ "School Year",
                         Race_High ~ "Race",
                         Gender_High ~"Gender",
                         Age_High ~ "Age",
                        Pol_High_1 ~ "Political Orientation"),
              type = c(Age_High, Pol_High_1) ~ "continuous",
              digits = all_continuous() ~ 2,
              sort = list(all_categorical() ~ "frequency"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / ({p}%)"))%>%
  bold_labels() %>% modify_header(label = "**Demographic Information**")



#### additional material copied from earlier work on study #####
#### 2 to generate baseline graphs and formula, also it's kinda easy? ####

#should we check as an exploratory measure, differences in baseline
#openness to belief change/baseline conviction by type of exercise?
## create a data subset to do this

#selecting the baseline elements here
pilot_baselines<-pilot[,c(3:4,6:7,9:10,26:28)]
pilot_baselines$ID<-1:length(pilot_baselines$in_uhc_change)

#reshape wide to long
library(tidyr)
pilot_base_compare1 <-pivot_longer(pilot_baselines, cols=c(1,3,5), names_to ="topic", values_to = "value")
#code for familiarity
#lmao this code is janky af but yes it does indeed get there
pilot_base_compare2 <-pivot_longer(pilot_baselines, cols=c(2,4,6), names_to ="topic", values_to = "value")
# for openness to change
pilot_base_compare3 <-pivot_longer(pilot_baselines, cols=c(7:9), names_to ="topic", values_to = "value")
#for initial conviction


#######################
### topic familiarity
#######################

res.aov <- aov(value ~ topic, data = pilot_base_compare1)
summary(res.aov)
# yes, each topic has differing levels of baseline openness to belief change
# lets throw up a basic graph showing these differences?
baseline_belief_c<-ggplot(pilot_base_compare1, aes(x=topic, y=value, color=topic))
#labels for the x-axis

baseline_xlabs<-c("AI", "Capital Punishment", "UHC")
baseline_belief_c+geom_boxplot() +labs(
  x = "Topic", 
  y = "Familiarity with Topic", 
  colour = "Topic",
  title = "Baseline Differences in Topic Familiarity"
) + scale_color_brewer(palette = "Set1", labels = c("Usage of AI in the Workplace", "Capital Punishment", "Universal Health Care"))+
  scale_x_discrete(labels = baseline_xlabs) + theme_bw()


####################
######belief change
####################

res.aov2 <- aov(value ~ topic, data = pilot_base_compare2)
summary(res.aov2)

baseline_belief_c<-ggplot(pilot_base_compare2, aes(x=topic, y=value, color=topic))
#labels for the x-axis

baseline_xlabs<-c("AI", "Capital Punishment", "UHC")
baseline_belief_c+geom_boxplot() +labs(
  x = "Topic", 
  y = "Openness to Belief Change", 
  colour = "Topic",
  title = "Baseline Differences in Openness to Belief Change"
) + scale_color_brewer(palette = "Set1", labels = c("Usage of AI in the Workplace", "Capital Punishment", "Universal Health Care"))+
  scale_x_discrete(labels = baseline_xlabs) + theme_bw()
#greater openness to belief change for UHC!

#############################
#initial moral conviction
#############################

res.aov3 <- aov(value ~ topic, data = pilot_base_compare3)
summary(res.aov3)

baseline_belief_c<-ggplot(pilot_base_compare3, aes(x=topic, y=value, color=topic))
#labels for the x-axis

baseline_xlabs<-c("AI", "Capital Punishment", "UHC")
baseline_belief_c+geom_boxplot() +labs(
  x = "Topic", 
  y = "Initial Moral Conviction", 
  colour = "Topic",
  title = "Baseline Differences in Initial Moral Conviction"
) + scale_color_brewer(palette = "Set1", labels = c("Usage of AI in the Workplace", "Capital Punishment", "Universal Health Care"))+
  scale_x_discrete(labels = baseline_xlabs) + theme_bw()
#greater openness to belief change for UHC!



###############
# Tukey's HSD #
###############
library(multcomp)
#topic familiarity
TukeyHSD(res.aov)
#openness to change
TukeyHSD(res.aov2)
#initial moral conviction
TukeyHSD(res.aov3)



####### we can do a simple t-test to determine if we even have change pre-post #####

#maybe do t-tests first lol
t.test(pilot$uhc_diff)
t.test(pilot$cap_diff)
t.test(pilot$ai_diff)


#mirror density charts for each variable showing pre-post improvement regardless of condition
library(ggplot2)
library(hrbrthemes)


p_density_top <-
  pilot_long %>%
  filter(time == "pre") %>%
  ggplot(aes(uhc_support)) +
  geom_density(fill = "purple") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank()
  )
p_density_top

#this simple split density code seems to work?

#holy shit i'm overthiking this - literally just use our original dataset (we dont need it to be 'long')
#fucking hilarious

#lets try to rework the original code again now

p <- ggplot(pilot, aes(x=x) ) +
  # Top
  geom_density( aes(x = in_uhc_supp, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=4.5, y=0.02, label="Initial Support for UHC"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = fn_uhc_supp, y = -..density..), fill= "#404080") +
  geom_label( aes(x=4.5, y=-0.02, label="Final Support for UHC"), color="#404080") +
  labs(
    x = "Support for UHC", 
    y = "Density", 
    title = "Changes in Support for Universal Health Care After Both Manipulations") +
  theme_minimal()+
  scale_y_continuous(limits = c(-0.025, 0.025))
p

#graph is good, t-tests are good too.
#lets see if we like this MORE or LESS than the stacked histogram/density plot?

p2 <- ggplot(pilot, aes(x=x) ) +
  # Top
  geom_density( aes(x = in_uhc_supp, y = ..density.., alpha = 0.3), fill="#69b3a2" ) +
  geom_label( aes(x=8.5, y=0.02, label="Initial Support for UHC"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = fn_uhc_supp, y = ..density.., alpha = 0.3), fill= "#404080") +
  geom_label( aes(x=25.5, y=0.023, label="Final Support for UHC"), color="#404080") +
  labs(
    x = "Support for UHC", 
    y = "Density", 
    title = "Changes in Support for Universal Health Care After Both Manipulations") +
  theme_minimal()+ theme(legend.position="none") +
  scale_y_continuous(limits = c(0, 0.025))
p2


p3 <- ggplot(pilot, aes(x=x) ) +
  # Top
  geom_density( aes(x = in_cap_supp, y = ..density.., alpha = 0.3), fill="#69b3a2" ) +
  geom_label( aes(x=2.5, y=0.015, label="Initial Support for Capital Punishment"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = fn_cap_supp, y = ..density.., alpha = 0.3), fill= "#404080") +
  geom_label( aes(x=23.5, y=0.017, label="Final Support for Capital Punishment"), color="#404080") +
  labs(
    x = "Support for Capital Punishment", 
    y = "Density", 
    title = "Changes in Support for Capital Punishment After Both Manipulations") +
  theme_minimal()+ theme(legend.position="none") +
  scale_y_continuous(limits = c(0, 0.025))
p3

p4 <- ggplot(pilot, aes(x=x) ) +
  # Top
  geom_density( aes(x = in_ai_supp, y = ..density.., alpha = 0.3), fill="#69b3a2" ) +
  geom_label( aes(x=-23, y=0.014, label="Initial Support for AI"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = fn_ai_supp, y = ..density.., alpha = 0.3), fill= "#404080") +
  geom_label( aes(x=17, y=0.017, label="Final Support for AI"), color="#404080") +
  labs(
    x = "Support for Usage of AI in the Workplace", 
    y = "Density", 
    title = "Changes in Support for Usage of AI in the Workplace After Both Manipulations") +
  theme_minimal()+ theme(legend.position="none") +
  scale_y_continuous(limits = c(0, 0.025))
p4


########### New graphs for Lab Meeting for Victoria ###

###########################################################################
#'time interaction w/ main effects, social consensus and moral conviction'#
###########################################################################

#using pilot_long, trying to have different groups by pre-post
pilot_long$comb_cond<-paste(pilot_long$consen_cond,pilot_long$conv_cond)
pilot_long$time <- factor(pilot_long$time, levels = c("pre", "post"))

#effect of WHAT on SUPPORT
p5<-ggplot(pilot_long, aes(x=time, y=uhc_support, color = consen_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.7, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = comb_cond))
#maybe have a combined condition graph instead?

p5 + facet_wrap(~ consen_cond)
#this is literally... something, but not what victora wants, I think...

#how about... time with JUST the social consensus, and JUST the moral conviction?
p5 + facet_wrap(~conv_cond, labeller = labeller(conv_cond = 
                                                c("MORALRESPONSIBILITYBLOCK" = "Moral Condition",
                                                  "PRAGMATIC/PRACTICALBLOCK" = "Pragmatic Condition")
                                                ))+
  labs(
    x = "Time", 
    y = "Support for UHC", 
    title = "Changes in Support for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Set2", labels = c("High Social Consensus",
                                                                   "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())

#lets work on this some more?
#this is good... we can try this for our other three items?

p5b<-ggplot(pilot_long, aes(x=time, y=cap_support, color = consen_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.7, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = comb_cond))
p5b + facet_wrap(~conv_cond, labeller = labeller(conv_cond = 
                                                  c("MORALRESPONSIBILITYBLOCK" = "Moral Condition",
                                                    "PRAGMATIC/PRACTICALBLOCK" = "Pragmatic Condition")))+
  labs(
    x = "Time", 
    y = "Support for UHC", 
    title = "Changes in Support for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Set1", labels = c("High Social Consensus",
                                                                   "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))

## and now for AI
p5c<-ggplot(pilot_long, aes(x=time, y=ai_support, color = consen_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.7, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = comb_cond))
p5c + facet_wrap(~conv_cond, labeller = labeller(conv_cond = 
                                                   c("MORALRESPONSIBILITYBLOCK" = "Moral Condition",
                                                     "PRAGMATIC/PRACTICALBLOCK" = "Pragmatic Condition")))+
  labs(
    x = "Time", 
    y = "Support for UHC", 
    title = "Changes in Support for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Set1", labels = c("High Social Consensus",
                                                                   "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))


######## do we need to do the same thing... but facet by social consensus?
#yeah lets try it
#or... is it overly complicated to have the colors split by the other condition?

p5d<-ggplot(pilot_long, aes(x=time, y=uhc_support, color = consen_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.7, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = consen_cond))+
  labs(
    x = "Time", 
    y = "Support for UHC", 
    title = "Changes in Support for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Set1", labels = c("High Social Consensus",
                                                                   "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())

p5d

#try our version of p5 but w/ facet by sc instead?
p5e<-ggplot(pilot_long, aes(x=time, y=uhc_support, color = conv_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = comb_cond))

p5e + facet_wrap(~consen_cond, labeller = labeller(consen_cond = 
                                                  c("SocialConsensus-High" = "High Social Consensus",
                                                    "SocialConsensus-Low" = "Low Social Consensus")
))+
  labs(
    x = "Time", 
    y = "Support for UHC", 
    title = "Changes in Support for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Pastel1", labels = c("Moral Condition",
                                                                   "Pragmatic Condition"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())
#try to have facet by social consensus condition and still have pre-post

p5f<-ggplot(pilot_long, aes(x=time, y=uhc_support)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.5, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = consen_cond))+
  labs(
    x = "Time", 
    y = "Support for UHC", 
    title = "Changes in Support for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Set1", labels = c("High Social Consensus",
                                                                   "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())

p5f+facet_wrap(~consen_cond, labeller = labeller(consen_cond = c("SocialConsensus-High" = "High Social Consensus",
                                                     "SocialConsensus-Low" = "Low Social Consensus")))

### essentially do our original p5 graphs but for.... moral conviction

p6a<-ggplot(pilot_long, aes(x=time, y=uhc_mconv, color = conv_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.7, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = conv_cond))+
  labs(
    x = "Time", 
    y = "Percieved Moral Conviction", 
    title = "Changes in Moral Conviction for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Set1", labels = c("Moral Condition",
                                                                   "Pragmatic Condition"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())

p6a

###
p6b<-ggplot(pilot_long, aes(x=time, y=uhc_mconv)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.5, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = conv_cond))+
  labs(
    x = "Time", 
    y = "Percieved Moral Conviction", 
    title = "Changes in Moral Conviction for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Set1", labels = c("Moral Condition",
                                                                   "Pragmatic Condition"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())

p6b+facet_wrap(~conv_cond, labeller = labeller(conv_cond = c("MORALRESPONSIBILITYBLOCK" = "Moral Condition",
                                                             "PRAGMATIC/PRACTICALBLOCK" = "Pragmatic Condition")))
##########
p6c<-ggplot(pilot_long, aes(x=time, y=uhc_mconv, color = conv_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = comb_cond))

p6c + facet_wrap(~consen_cond, labeller = labeller(consen_cond = 
                                                     c("SocialConsensus-High" = "High Social Consensus",
                                                       "SocialConsensus-Low" = "Low Social Consensus")
))+
  labs(
    x = "Time", 
    y = "Percieved Moral Conviction", 
    title = "Changes in Moral Conviction for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Pastel1", labels = c("Moral Condition",
                                                                      "Pragmatic Condition"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())
#####
p6d<-ggplot(pilot_long, aes(x=time, y=uhc_mconv, color = consen_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.7, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = comb_cond))


p6d + facet_wrap(~conv_cond, labeller = labeller(conv_cond = 
                                                  c("MORALRESPONSIBILITYBLOCK" = "Moral Condition",
                                                    "PRAGMATIC/PRACTICALBLOCK" = "Pragmatic Condition")
))+
  labs(
    x = "Time", 
    y = "Percieved Moral Conviction", 
    title = "Changes in Moral Conviction for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Set2", labels = c("High Social Consensus",
                                                                   "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())





#what about a single graph that just shows time by social consensus?

#try something like our overlaid density graphs... but faceted by soc/moral manipulation?

p6 <- ggplot(pilot, aes(x=x) ) +
  # Top
  geom_density( aes(x = in_uhc_supp, y = ..density.., alpha = 0.3), fill="#69b3a2" ) +
  geom_label( aes(x=8.5, y=0.02, label="Initial Support for UHC"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = fn_uhc_supp, y = ..density.., alpha = 0.3), fill= "#404080") +
  geom_label( aes(x=25.5, y=0.023, label="Final Support for UHC"), color="#404080") +
  labs(
    x = "Support for UHC", 
    y = "Density", 
    title = "Changes in Support for Universal Health Care After Both Manipulations") +
  theme_minimal()+ theme(legend.position="none") +
  scale_y_continuous(limits = c(0, 0.025))
p6

#going to figure out how to make columns for each value... with each time set aside separately?
#lets try it w/ consent first
pilot_spc<-subset(pilot,consen_cond == 'SocialConsensus-High')
pilot_spc2<-subset(pilot,consen_cond == 'SocialConsensus-Low')
pilot_spc[,c(2,11)]
pilot_spc2[,c(2,11)]

pilot_p6<-cbind(pilot_spc[,c(2,11)],pilot_spc2[,c(2,11)])
#try merge
p6<-merge(pilot_spc[,c(2,11)],pilot_spc2[,c(2,11)])
#nope...
#try new package? cbindx?
install.packages("gdata")
library(gdata)
pilot6<-cbindX(pilot_spc[,c(2,11)],pilot_spc2[,c(2,11)])


#first two rows are sc high, next two are sc low
#rename the columns ig
colnames(pilot6)<-c("ISHI","FSHI","ISLO","FSLO")


p6 <- ggplot(pilot6, aes(x=x) ) +
  # Top
  geom_density( aes(x = ISHI, y = ..density.., alpha = 0.3), fill="#06ba24" ) +
  geom_label( aes(x=8.5, y=0.02, label="Initial Support for UHC - High SC"), color="#06ba24") +
  geom_density( aes(x = FSHI, y = ..density.., alpha = 0.3), fill= "#e67e22") +
  geom_label( aes(x=32, y=0.023, label="Final Support for UHC - High SC"), color="#e67e22") +
  # Bottom
  geom_density( aes(x = ISLO, y = -..density.., alpha = 0.3), fill="#af7ac5" ) +
  geom_label( aes(x=8, y=-0.0225, label="Initial Support for UHC - Low SC"), color="#af7ac5") +
  # Bottom
  geom_density( aes(x = FSLO, y = -..density.., alpha = 0.3), fill= "#2874a6") +
  geom_label( aes(x=30, y=-0.025, label="Final Support for UHC - Low SC"), color="#2874a6") +
  labs(
    x = "Support for UHC", 
    y = "Density", 
    title = "Changes in Support for Universal Health Care by Social Consensus Condition") +
  theme_minimal()+ theme(legend.position="none") +
  scale_y_continuous(limits = c(-0.025, 0.025))
p6
#we can split this into 2 graphs instead?
p6a<-ggplot(pilot6, aes(x=x) ) +
  # Top
  geom_density( aes(x = ISHI, y = ..density.., alpha = 0.3), fill="#06ba24" ) +
  geom_label( aes(x=8.5, y=0.02, label="Initial Support for UHC - High SC"), color="#06ba24") +
  geom_density( aes(x = FSHI, y = ..density.., alpha = 0.3), fill= "#e67e22") +
  geom_label( aes(x=32, y=0.023, label="Final Support for UHC - High SC"), color="#e67e22") +
  labs(
    x = "Support for UHC", 
    y = "Density", 
    title = "Changes in Support for Universal Health Care By Time - High Social Consensus Only") +
  theme_minimal()+ theme(legend.position="none") +
  scale_y_continuous(limits = c(0, 0.025))

p6a

p6b<-ggplot(pilot6, aes(x=x) ) +
  geom_density( aes(x = ISLO, y = ..density.., alpha = 0.3), fill="#af7ac5" ) +
  geom_label( aes(x=8, y=0.0225, label="Initial Support for UHC - Low SC"), color="#af7ac5") +
  # Bottom
  geom_density( aes(x = FSLO, y = ..density.., alpha = 0.3), fill= "#2874a6") +
  geom_label( aes(x=30, y=0.025, label="Final Support for UHC - Low SC"), color="#2874a6") +
  labs(
    x = "Support for UHC", 
    y = "Density", 
    title = "Changes in Support for Universal Health Care By Time - Low Social Consensus Only") +
  theme_minimal()+ theme(legend.position="none") +
  scale_y_continuous(limits = c(0, 0.025))

p6b

#fiddle with colors and such but... this looks roughly right?
#this is JUST the graph for social consensus 
#lets try it again for moral conviction?


pilot_spc<-subset(pilot,conv_cond == 'MORALRESPONSIBILITYBLOCK')
pilot_spc2<-subset(pilot,conv_cond == 'PRAGMATIC/PRACTICALBLOCK')
pilot_spc[,c(2,11)]
pilot_spc2[,c(2,11)]
pilot7<-cbindX(pilot_spc[,c(2,11)],pilot_spc2[,c(2,11)])
colnames(pilot7)<-c("ISMR","FSMR","ISPR","FSPR")

p7 <- ggplot(pilot7, aes(x=x) ) +
  # Top
  geom_density( aes(x = ISMR, y = ..density.., alpha = 0.3), fill="#06ba24" ) +
  geom_label( aes(x=8.5, y=0.022, label="Initial Support for UHC - Moral Condition"), color="#06ba24") +
  geom_density( aes(x = FSMR, y = ..density.., alpha = 0.3), fill= "#e67e22") +
  geom_label( aes(x=32, y=0.025, label="Final Support for UHC - Moral Condition"), color="#e67e22") +
  # Bottom
  geom_density( aes(x = ISPR, y = -..density.., alpha = 0.3), fill="#af7ac5" ) +
  geom_label( aes(x=8, y=-0.0225, label="Initial Support for UHC - Practical Condition"), color="#af7ac5") +
  # Bottom
  geom_density( aes(x = FSPR, y = -..density.., alpha = 0.3), fill= "#2874a6") +
  geom_label( aes(x=30, y=-0.025, label="Final Support for UHC - Practical Condition"), color="#2874a6") +
  labs(
    x = "Support for UHC", 
    y = "Density", 
    title = "Changes in Support for Universal Health Care by Moral Conviction Condition") +
  theme_minimal()+ theme(legend.position="none") +
  scale_y_continuous(limits = c(-0.025, 0.025))
p7

#we can try and see how it splits out for our other conditions?

#lets try a density chart plainly for amount of 'change' by condition
pilot7$mr_diff<-(pilot7$FSMR-pilot7$ISMR)
#bind the extra material to it's own df
pilot8<-cbindX(pilot6[,c(5,6)],pilot7[,c(5,6)])

p8<- ggplot(pilot8, aes(x=x) ) +
  # Top
  geom_density( aes(x = hi_diff, y = ..density.., alpha = 0.3), fill="#06ba24" ) +
  geom_label( aes(x=35, y=0.025, label="Change in Support for UHC - High Consensus"), color="#06ba24") +
  geom_density( aes(x = lo_diff, y = ..density.., alpha = 0.3), fill= "#e67e22") +
  geom_label( aes(x=-35, y=0.025, label="Change in Support for UHC - Low Consensus"), color="#e67e22") +
  # Bottom
  geom_density( aes(x = mr_diff, y = -..density.., alpha = 0.3), fill="#af7ac5" ) +
  geom_label( aes(x=38, y=-0.0225, label="Change in Support for UHC - Moral Condition"), color="#af7ac5") +
  # Bottom
  geom_density( aes(x = pr_diff, y = -..density.., alpha = 0.3), fill= "#2874a6") +
  geom_label( aes(x=-35, y=-0.025, label="Change in Support for UHC - Pragmatic Condition"), color="#2874a6") +
  labs(
    x = "Support for UHC", 
    y = "Density", 
    title = "Amount of Change in Support for UHC Across All Conditions") +
  theme_minimal()+ theme(legend.position="none") +
  scale_y_continuous(limits = c(-0.05, 0.05))
p8

#######
#initial support predicts final support?
#######


p9<-ggplot(pilot, aes(x=in_uhc_supp, y=fn_uhc_supp)) + 
  geom_point(position ="jitter")+geom_smooth(method="lm" )+
  labs(
    x = "Initial Support for UHC", 
    y = "Final Support for UHC", 
    title = "Association between Initial and Final levels of Support for UHC") +
  theme_minimal()

p9


###utilitarian orientation predicts support for AI###
p10<-ggplot(pilot, aes(x=utilitarian, y=fn_uhc_supp)) + 
  geom_point(position ="jitter")+geom_smooth(method="lm" )+
  labs(
    x = "Utilitarian Orientation", 
    y = "Final Support for UHC", 
    title = "Association between Utilitarian Orientation and Support for UHC") +
  theme_minimal()
p10

#ancillary graphs for moral conviction##

#Social Consensus Manipulation associated with increased Moral Conviction for AI#

p11<-ggplot(pilot_long, aes(x=time, y=ai_mconv, color = consen_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.7, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = consen_cond))+
  labs(
    x = "Time", 
    y = "Percieved Moral Conviction", 
    title = "Changes in Moral Conviction for AI in the Workplace by Social Consensus Condition") +
  theme_minimal()+ scale_color_brewer(palette = "Set1", labels = c("High Consensus",
                                                                   "Low Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())

p11

p12<-ggplot(pilot_long, aes(x=time, y=ai_mconv)) +
  geom_jitter(size=1, alpha=0.7, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = consen_cond))+
  labs(
    x = "Time", 
    y = "Percieved Moral Conviction", 
    title = "Changes in Moral Conviction for AI in the Workplace by Social Consensus Condition") +
  theme_minimal()+ scale_color_brewer(palette = "Set1", labels = c("High Consensus",
                                                                   "Low Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())

p12+facet_wrap(~consen_cond,labeller = labeller(consen_cond =c("SocialConsensus-High" = "High Social Consensus",
                                                               "SocialConsensus-Low" = "Low Social Consensus")))

#try a 3rd factor (initially in support of/initially against)
#see how this washes out, and if people in one bucket move more than in others

#first shot is to check the histogram
hist(pilot$in_uhc_supp)
hist(pilot$in_cap_supp)
hist(pilot$in_ai_supp)

hist(pilot$fn_cap_supp)

#maybe would try splitting each into... 3 groups?
#against/neutral/for

#do what we did for supp by condition...
#however, add in a facet by category
#have category facet be split by... what
pilot$uhc_cat<-cut(pilot$in_uhc_supp, breaks=c(-51, -5, 5, 51),
    labels=c('Negative', 'Neutral', 'Positive'))

#whoops do it for pilot_long
pilot_long$uhc_cat<-cut(pilot_long$in_uhc_supp, breaks=c(-51, -5, 5, 51),
                   labels=c('Negative', 'Neutral', 'Positive'))
#uhh... we actually need to add this BEFORE the melt whoops.
#maybe do a fresh melt here?

#lets try ripping out the column in pilot and using that as a ... match?
pilot[,c(21,36)]

pilot_long$time <- factor(pilot_long$time, levels = c("pre", "post"))

p13<-ggplot(pilot_long, aes(x=time, y=uhc_support, color = consen_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.7, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = consen_cond))

p13 + facet_wrap(~uhc_cat)+
  labs(
    x = "Time", 
    y = "Support for UHC", 
    title = "Changes in Support for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Set2", labels = c("High Social Consensus",
                                                                   "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())

#we can see a bit of a 'ceiling' effect here....?

#we can try this again w/ the other two things
table(pilot_long$uhc_cat)
p14<-ggplot(pilot_long, aes(x=time, y=cap_support, color = consen_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.7, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = consen_cond))

p14 + facet_wrap(~cap_cat)+
  labs(
    x = "Time", 
    y = "Support for Capital Punishment", 
    title = "Changes in Support for Capital Punishment by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Dark2", labels = c("High Social Consensus",
                                                                   "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())

p15<-ggplot(pilot_long, aes(x=time, y=ai_support, color = consen_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.7, width = .05) +labs() +
  geom_smooth(method = "lm", se = FALSE, aes(group = consen_cond))

p15 + facet_wrap(~ai_cat)+
  labs(
    x = "Time", 
    y = "Support for AI", 
    title = "Changes in Support for AI by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Accent", labels = c("High Social Consensus",
                                                                    "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())



###########################################
######### final set of graphs for draft  ##
###########################################

#graph for effect of social consensus on support for topic.
# e.g., NO RESULT

s3g1<-ggplot(pilot_long, aes(x=time, y=uhc_support, color = consen_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, alpha=0.9, width = .05) +labs() +
  geom_violin(alpha=0.2,position="identity", trim = FALSE)+
  geom_smooth(method = "lm", se = FALSE, aes(group = comb_cond))
#maybe have a combined condition graph instead?

s3g1+ facet_wrap(~conv_cond, labeller = labeller(conv_cond = 
                                                      c("MORALRESPONSIBILITYBLOCK" = "Moral Condition",
                                                        "PRAGMATIC/PRACTICALBLOCK" = "Pragmatic Condition")
))+
  labs(
    x = "Time", 
    y = "Support for UHC", 
    title = "Changes in Support for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Set2", labels = c("High Social Consensus",
                                                                   "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(-65, 65))

#pretty close th the right graph for what we want? overlaid the violin

## our pre-post graphs
#we can split this into 2 graphs instead?
p6a<-ggplot(pilot6, aes(x=x) ) +
  # Top
  geom_density( aes(x = ISHI, y = ..density.., alpha = 0.3), fill="#06ba24" ) +
  geom_label( aes(x=8.5, y=0.02, label="Initial Support for UHC - High SC"), color="#06ba24") +
  geom_density( aes(x = FSHI, y = ..density.., alpha = 0.3), fill= "#e67e22") +
  geom_label( aes(x=32, y=0.023, label="Final Support for UHC - High SC"), color="#e67e22") +
  labs(
    x = "Support for UHC", 
    y = "Density", 
    title = "Changes in Support for Universal Health Care By Time - High Social Consensus Only") +
  theme_minimal()+ theme(legend.position="none") +
  scale_y_continuous(limits = c(0, 0.025))

p6a

p6b<-ggplot(pilot6, aes(x=x) ) +
  geom_density( aes(x = ISLO, y = ..density.., alpha = 0.3), fill="#af7ac5" ) +
  geom_label( aes(x=8, y=0.0225, label="Initial Support for UHC - Low SC"), color="#af7ac5") +
  # Bottom
  geom_density( aes(x = FSLO, y = ..density.., alpha = 0.3), fill= "#2874a6") +
  geom_label( aes(x=30, y=0.025, label="Final Support for UHC - Low SC"), color="#2874a6") +
  labs(
    x = "Support for UHC", 
    y = "Density", 
    title = "Changes in Support for Universal Health Care By Time - Low Social Consensus Only") +
  theme_minimal()+ theme(legend.position="none") +
  scale_y_continuous(limits = c(0, 0.025))

p6b

#same graph as g1 but for moral conviction?
s3g3<-ggplot(pilot_long, aes(x=time, y=uhc_mconv, color = conv_cond)) +
  #geom_boxplot() +
  geom_jitter(size=1, width = .05, alpha = .9) +labs() +
  geom_violin(alpha=0.2,position="identity", trim = FALSE)+
  geom_smooth(method = "lm", se = FALSE, aes(group = comb_cond))

s3g3 + facet_wrap(~consen_cond, labeller = labeller(consen_cond = 
                                                     c("SocialConsensus-High" = "High Social Consensus",
                                                       "SocialConsensus-Low" = "Low Social Consensus")
))+
  labs(
    x = "Time", 
    y = "Percieved Moral Conviction", 
    title = "Changes in Moral Conviction for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Set1", labels = c("Moral Condition",
                                                                      "Pragmatic Condition"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(-65, 65))

#lets run the analysis to see if category predicts outcome well at all
#e.g., is there an interaction by category and intervention?

########
##uhc###
########

#base graph
sup_uhc<-lm(fn_uhc_supp ~ conv_cond+consen_cond+in_uhc_supp+in_uhc_change+in_uhc_familiar+utilitarian+deontological, data = pilot)
summary(sup_uhc)
#updated graph
sup_uhc2<-lm(fn_uhc_supp ~ conv_cond*consen_cond*uhc_cat+in_uhc_supp+in_uhc_change+in_uhc_familiar+utilitarian+deontological, data = pilot)
summary(sup_uhc2)
#graph
p13 + facet_wrap(~uhc_cat)+
  labs(
    x = "Time", 
    y = "Support for UHC", 
    title = "Changes in Support for Universal Health Care by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Set2", labels = c("High Social Consensus",
                                                                   "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())

table(pilot$uhc_cat)


########
##cap###
########
sup_cap<-lm(fn_cap_supp ~ conv_cond*consen_cond+in_cap_supp+in_cap_change+in_cap_familiar+utilitarian+deontological, data = pilot)
summary(sup_cap)
#update
sup_cap2<-lm(fn_cap_supp ~ conv_cond*consen_cond*cap_cat+in_cap_supp+in_cap_change+in_cap_familiar+utilitarian+deontological, data = pilot)
summary(sup_cap2)
#graph
p14 + facet_wrap(~cap_cat)+
  labs(
    x = "Time", 
    y = "Support for Capital Punishment", 
    title = "Changes in Support for Capital Punishment by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Dark2", labels = c("High Social Consensus",
                                                                    "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())

table(pilot$cap_cat)

########
## ai ##
########
sup_ai<-lm(fn_ai_supp ~ conv_cond*consen_cond+in_ai_supp+in_ai_change+in_ai_familiar+utilitarian+deontological, data = pilot)
summary(sup_ai)
#update
sup_ai2<-lm(fn_ai_supp ~ conv_cond*consen_cond*ai_cat+in_ai_supp+in_ai_change+in_ai_familiar+utilitarian+deontological, data = pilot)
summary(sup_ai2)
#graph
p15 + facet_wrap(~ai_cat)+
  labs(
    x = "Time", 
    y = "Support for AI", 
    title = "Changes in Support for AI by Time") +
  theme_minimal()+ scale_color_brewer(palette = "Accent", labels = c("High Social Consensus",
                                                                     "Low Social Consensus"))+
  scale_x_discrete(labels = c("Pre","Post"))+ theme(legend.title = element_blank())

table(pilot$ai_cat)

####################################################
## trying to do sensitivity analysis for our crap ##
####################################################

install.packages("sensemakr")

library(sensemakr)


test.sens <- sensemakr(model = sup_uhc, 
                                treatment = "conv_condPRAGMATIC/PRACTICALBLOCK",
                                benchmark_covariates = "in_uhc_supp",
                                kd = 1:3)
plot(test.sens, type = "extreme")
ovb_minimal_reporting(test.sens, format = "html")

summary(test.sens)


##################################
# Cronbach's alpha for our stuff #
##################################
install.packages("psych")
library(psych)

#hilarious (going to have to look at other data lmao)

#############################
# citing R and the packages #
#############################
install.packages("grateful")
library(grateful)

install.packages("mediation")

cite_packages(out.dir = ".")   

########################
# peel out group means #
########################

zp<-subset(pilot, conv_cond == "MORALRESPONSIBILITYBLOCK" | consen_cond == "SocialConsensus-High",
                  select=c(in_ai_supp))
mean(zp[,])
sd(zp[,])
hist(zp[,])

#############################################################
# group means for moral conviction manipulation check table #
#############################################################

zp<-subset(pilot, conv_cond == "PRAGMATIC/PRACTICALBLOCK",
           select=c(fn_uhc_conviction))
mean(zp[,])
sd(zp[,])
hist(zp[,])

### Moral conviction x time checking
mc_uhc<-lm(uhc_mconv ~ conv_cond*time, data = pilot_long)
summary(mc_uhc)

mc_cap<-lm(cap_mconv ~ conv_cond*time, data = pilot_long)
summary(mc_cap)

mc_ai<-lm(ai_mconv ~ conv_cond*time, data = pilot_long)
summary(mc_ai)
