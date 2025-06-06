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

tbl_summary(d1)


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

m7<-lm(UHC_SUP~ Time * condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(m7)

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


#################
# Study 1 redux #
#################

#we see that the first m7 is significant! this is good!

m7<-lm(UHC_SUP~ Time * condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(m7)

m8<-lm(DEATH_SUP~ Time * condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(m8)

m9<-lm(CLIM_SUP~ Time * condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(m9)


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


## some loose ideas for our ideal gfx
#lets try scatterplot w/ a regression line, colors by condition, left is pre right is post
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

list(pilot_model_long$Time)
levels(pilot_model_long$Time)
#fiddle with levels, post on right, pre on left

pilot_model_long$Time<-relevel(pilot_model_long$Time, "pre")
#orders our level correctly

fplot_xlabs<-c("Pre-Intervention", "Post-Intervention")
#creates our new x-tick labels

fplot<-ggplot(pilot_model_long, aes(x=Time, y=DEATH_SUP, color=condition))+ geom_point(alpha =.3, position = position_jitter(width=.07))

fplot+geom_smooth(method = lm, alpha=.2, aes(group = condition))+ scale_color_manual(values=c("purple", "black"))+ 
  labs(
    x = "Time", 
    y = "Support for Capital Punishment", 
    colour = "Social Consensus Condition",
    title = "Effect of Social Consensus Intervention on Support for Capital Punishment",
    subtitle = "Perception of increased social consensus correlates with increased support"
  ) + scale_x_discrete(labels = fplot_xlabs) + theme_bw()

#really great graph showing the effect of our intervention!

#additional graphs in same style for the three other conditions we're looking at

#alrighty
#we want to mod this so it looks like the 'better poster' graph.

#we want a 'grouped' bar chart

lplot<-ggplot(pilot_model_long, aes(fill=condition, y=UHC_SUP, x=Time)) + 
  geom_bar(position="dodge", stat="identity")

lplot




fplot2<-ggplot(pilot_model_long, aes(x=Time, y=CLIM_SUP, color=condition))+ geom_point(alpha =.3, position = position_jitter(width=.07))

fplot2+geom_smooth(method = lm, alpha=.2, aes(group = condition))+ scale_color_manual(values=c("purple", "black"))+ 
  labs(
    x = "Time", 
    y = "Support for Climate Change", 
    colour = "Social Consensus Condition",
    title = "Effect of Social Consensus Intervention on Support for Climate Change",
    subtitle = "Perception of increased social consensus does not correlate with increased support"
  ) + scale_x_discrete(labels = fplot_xlabs) + theme_bw()



fplot3<-ggplot(pilot_model_long, aes(x=Time, y=UHC_SUP, color=condition))+ geom_point(alpha =.3, position = position_jitter(width=.07))

fplot3+geom_smooth(method = lm, alpha=.2, aes(group = condition))+ scale_color_manual(values=c("purple", "black"))+ 
  labs(
    x = "Time", 
    y = "Support for Universal Health Care", 
    colour = "Social Consensus Condition",
    title = "Effect of Social Consensus Intervention on Support for Universal Health Care",
    subtitle = "Perception of increased social consensus causes increased support"
  ) + scale_x_discrete(labels = fplot_xlabs) + theme_bw()

#try to get the geom_bar set up right?

# Load ggplot2
library(ggplot2)

# create dummy data
data <- data.frame(
  name=letters[1:5],
  value=sample(seq(4,15),5),
  sd=c(1,0.2,3,2,4)
)

# Most basic error bar
ggplot(data) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

#wait a minute... maybe we just cheese out the values into their own df
#then try to reference that for our final graph?

#lets give that a shot?

z<-subset(pilot_model_long, condition == 'High' & Time == 'pre', select = c(UHC_SUP))
mean(z$UHC_SUP)
sd(z$UHC_SUP)
sd(z$UHC_SUP) / sqrt(length(z$UHC_SUP))

z<-subset(pilot_model_long, condition == 'High' & Time == 'post', select = c(UHC_SUP))
mean(z$UHC_SUP)
sd(z$UHC_SUP)
sd(z$UHC_SUP) / sqrt(length(z$UHC_SUP))
##

z<-subset(pilot_model_long, condition == 'Low' & Time == 'pre', select = c(UHC_SUP))
mean(z$UHC_SUP)
sd(z$UHC_SUP)
sd(z$UHC_SUP) / sqrt(length(z$UHC_SUP))
z<-subset(pilot_model_long, condition == 'Low' & Time == 'post', select = c(UHC_SUP))
mean(z$UHC_SUP)
sd(z$UHC_SUP)
sd(z$UHC_SUP) / sqrt(length(z$UHC_SUP))


time <- c(rep("Pre-Intervention" , 2) , rep("Post-Intervention" , 2))
condition <- rep(c("High Social Consensus" , "Low Social Consensus") , 2)
mean <- c(67.896, 67.437, 72.956, 64.901 )
sd <- c(25.237, 26.749, 24.301, 27.177)
se<- c(1.592, 1.678, 1.533, 1.705 )
ldata <- data.frame(time, condition, mean, sd, se)

ggplot(ldata, aes(fill=condition, y=mean, x=time)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar( aes(x=condition, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

#pretty close... adjust colors and ordering, and then we're good?
#maybe add legends and other stuff?

#lets try the example from "https://towardsdatascience.com/grouped-barplot-with-error-bars-in-r-ee87b112204d/"

#make sure to order the levels
ldata$time<-as.factor(ldata$time)
levels(ldata$time)
#fiddle with levels, post on right, pre on left

ldata$time<-relevel(ldata$time, "Pre-Intervention")
#orders our level correctly

barplot <- ldata %>% 
  ggplot(
    aes(x = time, y = mean, fill = condition))+
  geom_col( position = "dodge", width = 0.5, alpha = 1, color = "black", size = 0.1)

barplot + geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                       position =  position_dodge(width = 0.5), width = 0.2)+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,100))+
  scale_fill_manual(values = c("goldenrod", "lightgrey"),
                    name = NULL)+ # NULL removes the legend title "Category".
  theme_bw()+
  theme(legend.position = c(0.2, 0.80))+labs(
    x = "Time", 
    y = "Support for Universal Health Care")



#add in the error bars, and then adjust colors and other bits
#adjust the data itself for the y-axis!

ldata2<-ldata
ldata2$mean<-ldata2$mean-50

barplot2 <- ldata2 %>% 
  ggplot(
    aes(x = time, y = mean, fill = condition))+
  geom_col( position = "dodge", width = 0.5, alpha = 1, color = "black", size = 0.1)

barplot2 + geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                        position =  position_dodge(width = 0.5), width = 0.2)+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,30))+
  scale_fill_manual(values = c("goldenrod", "lightgrey"),
                    name = NULL)+ # NULL removes the legend title "Category".
  theme_bw()+
  theme(legend.position = c(0.2, 0.80))+labs(
    x = "Time", 
    y = "Support for Universal Health Care")


#getting things set up so we can directly test the time x interaction condition ... which we didn't do originally
#whoops!

ix1_m<-lm(UHC_SUP~ Time * condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(ix1_m)

ix2_m<-lm(CLIM_SUP~ Time * condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(ix2_m)

ix3_m<-lm(DEATH_SUP~ Time * condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(ix3_m)

ix4_m<-lm(SLAVE_SUP~ Time * condition + NLINE + SNS_SCORE + utilitarian + deontological + SILS_1_1, data = pilot_model_long)
summary(ix4_m)


#subset data for our tables
pre_low<-subset(pilot_model_long, condition == 'Low' & Time == 'pre')
mean(pre_low$UHC_SUP)
sd(pre_low$UHC_SUP)

mean(pre_low$DEATH_SUP)
sd(pre_low$DEATH_SUP)

mean(pre_low$CLIM_SUP)
sd(pre_low$CLIM_SUP)

post_low<-subset(pilot_model_long, condition == 'Low' & Time == 'post')
mean(post_low$UHC_SUP)
sd(post_low$UHC_SUP)

mean(post_low$DEATH_SUP)
sd(post_low$DEATH_SUP)

mean(post_low$CLIM_SUP)
sd(post_low$CLIM_SUP)
