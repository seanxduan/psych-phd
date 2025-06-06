### As per usual, lets start by getting tables setup ###
pilot<-read.csv("pilot_clean_2.csv")

#we're gonna start w/ gtsummary, since that's how we closed it out originally
library("lme4")
library("texreg")
library("ggplot2")
library("tidyverse")
library("gtable")
library("gtsummary")

#peel out only the demographic info?
d1<-pilot[,c(23,40:43)]


d1 %>%
  tbl_summary(
    by = condition,
    type = list(Age ~ "continuous"),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / ({p}%)"),
    digits = all_continuous() ~ 2,
    label = c(School_Year ~ "School Year",
              Race ~ "Race",
              Gender ~"Gender",
              Age ~ "Age"),
    missing_text = "(Missing)",
    sort = list(everything() ~ "frequency")
  ) %>% bold_labels() %>% modify_header(label = "**Social Consensus Condition**")
#great job!

#Alright, we can work on basic modeling work @ this juncture next

#whoops need to set it up s/t we can actually grind our score
#instead of having to do other stuff.

rowMeans(pilot[,4:7])
#no good, because...
#note that each individual thing we're supping means something else!

pilot$climate<-(pilot$P1-pilot$Pre_P1)
pilot$UHC<-(pilot$P2-pilot$Pre_P2)
pilot$death<-(pilot$P3-pilot$Pre_P3)
pilot$slave<-(pilot$P4-pilot$Pre_P4)

#total surprise score?
pilot$tot_surprise<-rowMeans(pilot[,28:31])

#lets look @ total utilitarianism (simple avg of util 1-6)
pilot$utilitarian<-rowMeans(pilot[,8:13])

#lets look @ total deontological (simple avg of util 1-6)
pilot$deontological<-rowMeans(pilot[,14:19])

#lets do an EASY one
#are people surprised in the high or low conditions?

m1<-lm(tot_surprise~ condition+NLINE +SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)
summary(m1)

#for UHC
m2<-lm(UHC ~condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)
summary(m2)
#interxn
m2<-lm(UHC ~condition*utilitarian + NLINE + SNS_SCORE + deontological+ SILS_1_1, data = pilot)
summary(m2)
#no fx of interaction!

#lets directly see if the feelings on healthcare are due to utilitarian or deontological reasons
#how would we measure this?
#part for our next proposal, lets make it more clear!

#lets check... did people with high deon/util orientation JUST have higher initial estimates regardless?

m2_a<-lm(Pre_P2 ~ NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)
summary(m2_a)

#how about the final postscore?
m2_b<-lm(P2 ~ condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)
summary(m2_b)

#ok, lets try again w/ the difference score?
m2_c<-lm(UHC ~ condition+ NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)
summary(m2_c)

#


#for climate
m3<-lm(climate ~condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)
summary(m3)

#for death?
m4<-lm(death ~condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)
summary(m4)

#for slavery?
m5<-lm(slave ~condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)
summary(m5)

#hmm we see a bit of neat stuff here!

#lets do a brief check to see how SNS and the Nline task relate
m6<-lm(NLINE ~ SNS_SCORE, data = pilot)
summary(m6)
#not particularly well either way - perhaps some issues with implementation?

#lets throw together some gfx to see if we can find some interesting plots/interactions?
#note - lets reshape so we can do more/better analysis?
pilot_model_long <- reshape(
  data = pilot,
  varying = list(c("Pre_P1","P1"),
                 c("Pre_P2","P2"),
                 c("Pre_P3","P3"),
                 c("Pre_P4","P4")),
  idvar = 'X',
  v.names = c('CLIM_SUP', 'UHC_SUP', 'DEATH_SUP', 'SLAVE_SUP'),
  timevar = 'Time',
  times = c('pre', 'post'),
  direction = 'long'
)
pilot_model_long$Time<-as.factor(pilot_model_long$Time)
m7<-lm(UHC_SUP~ Time + condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(m7)

m7<-lm(UHC_SUP~ Time*condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(m7)


#huh ok well maybe our result was better than expected?
m7<-lm(P2~ Time*condition + Pre_P2+ NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(m7)




#ok, this should mostly be for looking @ the structure of everything for the purpose
#of making graphs.

library(ggplot2)

plot1<-ggplot(pilot_model_long, aes(x=Time, y=UHC_SUP, color=condition)) +
  geom_boxplot() 
plot1 + facet_wrap(~ condition)+ scale_color_brewer(palette = "Set1")

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
plot1_E + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")

plot1_F<-ggplot(pilot, aes(x=utilitarian, y=UHC)) +
  geom_point() + geom_smooth(method = "lm")
plot1_F + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")
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
plot1_H + facet_wrap(~ condition) + scale_color_brewer(palette = "Set1")

##### Is it plausible that we're hitting a ceiling effect?
#e.g. for those that support UHC, our intervention doesn't do very much?

plot2<-ggplot(pilot_model_long, aes(x=Time, y=CLIM_SUP, color=condition)) +
  geom_boxplot() 
plot2 + facet_wrap(~ condition)+ scale_color_brewer(palette = "Set1")

plot3<-ggplot(pilot_model_long, aes(x=Time, y=DEATH_SUP, color=condition)) +
  geom_boxplot() 
plot3 + facet_wrap(~ condition)+ scale_color_brewer(palette = "Set1")

plot4<-ggplot(pilot_model_long, aes(x=Time, y=SLAVE_SUP, color=condition)) +
  geom_boxplot() 
plot4 + facet_wrap(~ condition)+ scale_color_brewer(palette = "Set1")



#####################################################
# simple 2 sample t-tests to test H1a, H1b, and H1c #
#####################################################

#
pilot$climate<-(pilot$P1-pilot$Pre_P1)
pilot$UHC<-(pilot$P2-pilot$Pre_P2)
pilot$death<-(pilot$P3-pilot$Pre_P3)
pilot$slave<-(pilot$P4-pilot$Pre_P4)

#should use subset, lets try to peel out UHC time 1 (i.e. Pre_P2)
#for high condition
subset(pilot, condition=="High",
       select=Pre_P2)

#and for low condition
subset(pilot, condition=="Low",
       select=Pre_P2)

t.test(x = (subset(pilot, condition=="High",
                   select=Pre_P2)), y = (subset(pilot, condition=="Low",
                                                select=Pre_P2)))

# do the same for capital punishment
subset(pilot, condition=="High",
       select=Pre_P3)

t.test(x = (subset(pilot, condition=="High",
                   select=Pre_P3)), y = (subset(pilot, condition=="Low",
                                                select=Pre_P3)))

# do the same for climate change

t.test(x = (subset(pilot, condition=="High",
                   select=Pre_P1)), y = (subset(pilot, condition=="Low",
                                                select=Pre_P1)))

#alright now for H1b social consensus increases support
subset(pilot, condition=="High",
       select=Pre_P2)
subset(pilot, condition=="High",
       select=P2)

t.test(x = (subset(pilot, condition=="High",
                   select=P2)), y = (subset(pilot, condition=="High",
                                               select=Pre_P2)))

t.test(x = (subset(pilot, condition=="High",
                   select=P3)), y = (subset(pilot, condition=="High",
                                            select=Pre_P3)))

t.test(x = (subset(pilot, condition=="High",
                   select=P1)), y = (subset(pilot, condition=="High",
                                            select=Pre_P1)))

#H1c

t.test(x = (subset(pilot, condition=="Low",
                   select=P2)), y = (subset(pilot, condition=="Low",
                                            select=Pre_P2)))

t.test(x = (subset(pilot, condition=="Low",
                   select=P3)), y = (subset(pilot, condition=="Low",
                                            select=Pre_P3)))

t.test(x = (subset(pilot, condition=="Low",
                   select=P1)), y = (subset(pilot, condition=="Low",
                                            select=Pre_P1)))

#test
t.test(x = (subset(pilot, condition=="High",
                   select=P1)), y = (subset(pilot, condition=="Low",
                                            select=P1)))
t.test(x = (subset(pilot, condition=="High",
                   select=P2)), y = (subset(pilot, condition=="Low",
                                            select=P2)))
t.test(x = (subset(pilot, condition=="High",
                   select=P3)), y = (subset(pilot, condition=="Low",
                                            select=P3)))
#there's a difference between the two groups as a whole at time 2
#however, there are not significant differences between the starting value at t1 and t2, they just go in opposite directions

?aov


#check to see if our interaction model is working or busted

#huh ok well maybe our result was better than expected?
m7<-lm(UHC_SUP ~ Time*condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(m7)

m7<-lm(CLIM_SUP ~ Time*condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(m7)

m7<-lm(DEATH_SUP ~ Time*condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(m7)

# either i'm misunderstanding this or .... something? we can walk through it w/ victoria