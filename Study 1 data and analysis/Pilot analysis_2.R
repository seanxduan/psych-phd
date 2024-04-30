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

#lets get condition set-up properly as well lol
#re-naming because we only got number values - so we can print our table
d1 <- d1 %>%
  mutate(Gender = recode(Gender, "1" = 'Male', "2" = 'Female', "3" =  'Gender Variant' ))
d1 <- d1 %>%
  mutate(Race = recode(Race, "1" = 'White', "2" = 'Black', "3" =  'Asian',"4" = "Native American", "5" = "Hispanic", "6" = "Other",
                       "1,2" =  'White/Black', "1,4" = "White/Native American" ,
                       "1,3" =  'White/Asian', "1,5" = "White/Hispanic",
                       "1,6" =  'White/Other', "2,5" = "Black/Hispanic",
                       "1,2,5" =  'White/Black/Hispanic', "5,6" = "Hispanic/Other"))
# just clumping as other
d1 <- d1 %>%
  mutate(Race = recode(Race, "1" = 'White', "2" = 'Black', "3" =  'Asian',"4" = "Native American", "5" = "Hispanic", "6" = "Other",
                       "1,2" =  'White/Black', "1,4" = "Other" ,
                       "1,3" =  'White/Asian', "1,5" = "White/Hispanic",
                       "1,6" =  'Other', "2,5" = "Other",
                       "1,2,5" =  'Other', "5,6" = "Other"))

d1 <- d1 %>%
  mutate(School_Year = recode(School_Year, "1" = 'Freshman', "2" = 'Sophmore', '3' =  'Junior',
                              "4" = 'Senior', "5" = 'Other'))

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
m2<-lm(UHC ~condition*utilitarian + NLINE + SNS_SCORE + condition*deontological+ SILS_1_1, data = pilot)
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
#ok, this should mostly be for looking @ the structure of everything for the purpose
#of making graphs.

library(ggplot2)
### material for making tables for our results sections
library(texreg)
texreg(m7, custom.model.names = "Multi Level Model - Intercept Varies by Subject",
       custom.coef.names = c(),
       caption = "Frequentist Table of Intervention on UHC Support", label = "tab:freq-table1",
       ci.force = TRUE)
# this is fine for... rmarkdown, since we're bodging it in word tho... lets try something else
install.packages("modelsummary")
library(modelsummary)
library(webshot2)
modelsummary(m7)
#oh nice we can use lists of our outputs and then use that to codge together a nice set of tables!

#uhc pros
m2<-lm(UHC ~condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)
#for climate
m3<-lm(climate ~condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)
#for death?
m4<-lm(death ~condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)
#for slavery?
m5<-lm(slave ~condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)

#some code so we can calculate mean and SD of subsets lmao
pilot_predata <- pilot_model_long[which(pilot_model_long$condition=='High'& pilot_model_long$Time=='pre'),]
pilot_postdata <- pilot_model_long[which(pilot_model_long$condition=='High'& pilot_model_long$Time=='post'),]

mean(pilot_predata$UHC_SUP)
sd(pilot_predata$UHC_SUP)
mean(pilot_postdata$UHC_SUP)
sd(pilot_postdata$UHC_SUP)

mean(pilot_predata$DEATH_SUP)
sd(pilot_predata$DEATH_SUP)
mean(pilot_postdata$DEATH_SUP)
sd(pilot_postdata$DEATH_SUP)

mean(pilot_predata$CLIM_SUP)
sd(pilot_predata$CLIM_SUP)
mean(pilot_postdata$CLIM_SUP)
sd(pilot_postdata$CLIM_SUP)

models <- list(
  "UHC"     = lm(UHC ~condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot),
  "Climate" = lm(climate ~condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot),
  "Death"   = lm(death ~condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot),
  "Slavery" = lm(slave ~condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot)
)
modelsummary(models, stars = TRUE, title = 'Effect of Social Consensus and Individual Differences on Highly Polarized Beliefs',
             coef_rename = c("conditionLow" = "Low Social Consensus",
                                                  "NLINE" = "Objective Numeracy",
                                                  "SNS_SCORE" = "Subjective Numeracy",
                                                  "utilitarian" = "Utilitarian Orientation",
                                                  "deontological" = "Deontological Orentation",
                                                  "SILS_1_1" = "Health Literacy"),
             output = "consensus.png")

###

plot1<-ggplot(pilot_model_long, aes(x=condition, y=UHC_SUP, color=condition)) +
  geom_boxplot() + 
  labs(
    x = "Social Consensus Condition", 
    y = "Support for Universal Health Care", 
    colour = "Social Consensus Condition",
    title = "Effect of Social Consensus on Support for Universal Health Care",
    subtitle = "Social consensus has significant effect on support for UHC"
  )
plot1 + facet_wrap(~ Time)+ scale_color_brewer(palette = "Set1")


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
