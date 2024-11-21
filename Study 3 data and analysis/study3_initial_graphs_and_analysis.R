### As per usual, lets start by getting tables setup ###
pilot_long<-read.csv("Study 3 data and analysis/study_3_long_clean.csv")
pilot<-read.csv("Study 3 data and analysis/study_3_clean.csv")

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

pilot_xtra_long<-melt(setDT(pilot_long),measure=patterns("_familiar", "_change", "_support", "_mconv"), 
                      value.name=c("topic_familiarity", "topic_changeable", "topic_support", "topic_moral_c"),
                      variable.name="topic")
#subset out the 'pre' results so we can see baseline differences

#rename the topics lol

pilot_xtra_long$topic<-str_replace_all(pilot_xtra_long$topic, c("1"= "uhc", "2" = "cap", "3" = "ai"))

pilot_xtra_long<-subset(pilot_xtra_long,time == 'Pre')

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

conv_uhc<-lm(fn_uhc_conviction ~ conv_cond*consen_cond+in_uhc_conviction+in_uhc_change+in_uhc_familiar+utilitarian+deontological, data = pilot)
summary(conv_uhc)

mini_conv_uhc<-lm(fn_uhc_conviction ~ conv_cond+in_uhc_supp+in_uhc_conviction, data = pilot)
summary(mini_conv_uhc)

#death
sup_cap<-lm(fn_cap_supp ~ conv_cond*consen_cond+in_cap_supp+in_cap_change+in_cap_familiar+utilitarian+deontological, data = pilot)
summary(sup_cap)

conv_cap<-lm(fn_cap_conviction ~ conv_cond*consen_cond+in_cap_conviction+in_cap_change+in_cap_familiar+utilitarian+deontological, data = pilot)
summary(conv_cap)

#ai
sup_ai<-lm(fn_ai_supp ~ conv_cond*consen_cond+in_ai_supp+in_ai_change+in_ai_familiar+utilitarian+deontological, data = pilot)
summary(sup_ai)

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
hist(pilot$fn_uhc_supp-pilot$in_uhc_supp)
hist(pilot$fn_cap_supp-pilot$in_cap_supp)

#run a quick test to see how AI support is affected by things
sup_ai2<-lm(ai_support ~ conv_cond*consen_cond+in_ai_change+in_ai_familiar+utilitarian+deontological+time, data = pilot_long)
summary(sup_ai2)
#ok, so we do see an affect of pre-post, both interventions DID increase support
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

d1<-pilot[,c(16:20)]
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
