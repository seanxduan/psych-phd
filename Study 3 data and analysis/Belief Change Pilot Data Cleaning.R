#basic data, analysis, and prep, for our 'pilot/study 1' data!

pilot<-read.csv("Study 3 data and analysis/Belief+Change+Manipulation+Pilot+-+Spring+2023_May+20,+2024_16.28.csv")

#remove two 'blank' rows of data
pilot_data<-pilot[-c(1,2),]

#remove those who didn't finish?
pilot_data<-pilot_data[!(pilot_data$Finished=="False"),]

#delete unnecessary front/back columns?
pilot_data<-pilot_data[,-c(1:5,7:17)]
#using a tool to clear out empty columns
library(janitor)
pilot_data<-remove_empty(pilot_data, which = c("cols"))

library(data.table)

#no pre or post issues
#no individual differences either

library(tidyverse)

#figure out how to structure the names themselves to be more functional?
#maybe just do it in xl :eyes: lol

which( colnames(pilot_data)=="Q121_4")

#no we're just gonna slug through in R b/c we can


###Moral Responsibility for each variable
#UHC
which(colnames(pilot_data)=="Q206_1")
names(pilot_data)[2:11] <- c("mr_uhc_belief", "mr_uhc_persuade", "mr_uhc_supp",
                           #names for moral conviction            
                           "mr_uhc_conv_1","mr_uhc_conv_2","mr_uhc_conv_3","mr_uhc_conv_4",
                           "mr_uhc_conv_5","mr_uhc_conv_6","mr_uhc_conv_7")

#death penalty

which( colnames(pilot_data)=="Q202_1")
which( colnames(pilot_data)=="Q204_10")
names(pilot_data)[12:21] <- c("mr_death_belief", "mr_death_persuade", "mr_death_supp",
                             #names for moral conviction            
                             "mr_death_conv_1","mr_death_conv_2","mr_death_conv_3","mr_death_conv_4",
                             "mr_death_conv_5","mr_death_conv_6","mr_death_conv_7")

#exercise
which( colnames(pilot_data)=="Q196_1")
which( colnames(pilot_data)=="Q195_10")
names(pilot_data)[22:31] <- c("mr_exercise_belief", "mr_exercise_persuade", "mr_exercise_supp",
                              #names for moral conviction            
                              "mr_exercise_conv_1","mr_exercise_conv_2","mr_exercise_conv_3","mr_exercise_conv_4",
                              "mr_exercise_conv_5","mr_exercise_conv_6","mr_exercise_conv_7")

#climate
which( colnames(pilot_data)=="Q197_1")
which( colnames(pilot_data)=="Q199_10")
names(pilot_data)[32:41] <- c("mr_climate_belief", "mr_climate_persuade", "mr_climate_supp",
                              #names for moral conviction            
                              "mr_climate_conv_1","mr_climate_conv_2","mr_climate_conv_3","mr_climate_conv_4",
                              "mr_climate_conv_5","mr_climate_conv_6","mr_climate_conv_7")

### Moral Piggybacking

which(colnames(pilot_data)=="Q319_1")
names(pilot_data)[42:51] <- c("mp_uhc_belief", "mp_uhc_persuade", "mp_uhc_supp",
                             #names for moral conviction            
                             "mp_uhc_conv_1","mp_uhc_conv_2","mp_uhc_conv_3","mp_uhc_conv_4",
                             "mp_uhc_conv_5","mp_uhc_conv_6","mp_uhc_conv_7")

#death penalty

which( colnames(pilot_data)=="Q202_1")
which( colnames(pilot_data)=="Q204_10")
names(pilot_data)[52:61] <- c("mp_death_belief", "mp_death_persuade", "mp_death_supp",
                              #names for moral conviction            
                              "mp_death_conv_1","mp_death_conv_2","mp_death_conv_3","mp_death_conv_4",
                              "mp_death_conv_5","mp_death_conv_6","mp_death_conv_7")

#exercise
which( colnames(pilot_data)=="Q196_1")
which( colnames(pilot_data)=="Q195_10")
names(pilot_data)[62:71] <- c("mp_exercise_belief", "mp_exercise_persuade", "mp_exercise_supp",
                              #names for moral conviction            
                              "mp_exercise_conv_1","mp_exercise_conv_2","mp_exercise_conv_3","mp_exercise_conv_4",
                              "mp_exercise_conv_5","mp_exercise_conv_6","mp_exercise_conv_7")

#climate
which( colnames(pilot_data)=="Q337_1")
which( colnames(pilot_data)=="Q339_10")
names(pilot_data)[72:81] <- c("mp_climate_belief", "mp_climate_persuade", "mp_climate_supp",
                              #names for moral conviction            
                              "mp_climate_conv_1","mp_climate_conv_2","mp_climate_conv_3","mp_climate_conv_4",
                              "mp_climate_conv_5","mp_climate_conv_6","mp_climate_conv_7")

### Pragmatic/Practical 

which(colnames(pilot_data)=="Q343_1")
names(pilot_data)[82:91] <- c("pr_uhc_belief", "pr_uhc_persuade", "pr_uhc_supp",
                              #names for moral conviction            
                              "pr_uhc_conv_1","pr_uhc_conv_2","pr_uhc_conv_3","pr_uhc_conv_4",
                              "pr_uhc_conv_5","pr_uhc_conv_6","pr_uhc_conv_7")

#death penalty

which( colnames(pilot_data)=="Q202_1")
which( colnames(pilot_data)=="Q204_10")
names(pilot_data)[92:101] <- c("pr_death_belief", "pr_death_persuade", "pr_death_supp",
                              #names for moral conviction            
                              "pr_death_conv_1","pr_death_conv_2","pr_death_conv_3","pr_death_conv_4",
                              "pr_death_conv_5","pr_death_conv_6","pr_death_conv_7")

#exercise
which( colnames(pilot_data)=="Q196_1")
which( colnames(pilot_data)=="Q195_10")
names(pilot_data)[102:111] <- c("pr_exercise_belief", "pr_exercise_persuade", "pr_exercise_supp",
                              #names for moral conviction            
                              "pr_exercise_conv_1","pr_exercise_conv_2","pr_exercise_conv_3","pr_exercise_conv_4",
                              "pr_exercise_conv_5","pr_exercise_conv_6","pr_exercise_conv_7")

#climate
which( colnames(pilot_data)=="Q337_1")
which( colnames(pilot_data)=="Q363_10")
names(pilot_data)[112:121] <- c("pr_climate_belief", "pr_climate_persuade", "pr_climate_supp",
                              #names for moral conviction            
                              "pr_climate_conv_1","pr_climate_conv_2","pr_climate_conv_3","pr_climate_conv_4",
                              "pr_climate_conv_5","pr_climate_conv_6","pr_climate_conv_7")

### Hedonic

which(colnames(pilot_data)=="Q367_1")
names(pilot_data)[122:131] <- c("pr_uhc_belief", "pr_uhc_persuade", "pr_uhc_supp",
                              #names for moral conviction            
                              "pr_uhc_conv_1","pr_uhc_conv_2","pr_uhc_conv_3","pr_uhc_conv_4",
                              "pr_uhc_conv_5","pr_uhc_conv_6","pr_uhc_conv_7")

#death penalty

which( colnames(pilot_data)=="Q202_1")
which( colnames(pilot_data)=="Q204_10")
names(pilot_data)[132:141] <- c("pr_death_belief", "pr_death_persuade", "pr_death_supp",
                              #names for moral conviction            
                              "pr_death_conv_1","pr_death_conv_2","pr_death_conv_3","pr_death_conv_4",
                              "pr_death_conv_5","pr_death_conv_6","pr_death_conv_7")

#exercise
which( colnames(pilot_data)=="Q196_1")
which( colnames(pilot_data)=="Q195_10")
names(pilot_data)[142:151] <- c("pr_exercise_belief", "pr_exercise_persuade", "pr_exercise_supp",
                              #names for moral conviction            
                              "pr_exercise_conv_1","pr_exercise_conv_2","pr_exercise_conv_3","pr_exercise_conv_4",
                              "pr_exercise_conv_5","pr_exercise_conv_6","pr_exercise_conv_7")

#climate
which( colnames(pilot_data)=="Q337_1")
which( colnames(pilot_data)=="Q339_10")
names(pilot_data)[152:161] <- c("pr_climate_belief", "pr_climate_persuade", "pr_climate_supp",
                              #names for moral conviction            
                              "pr_climate_conv_1","pr_climate_conv_2","pr_climate_conv_3","pr_climate_conv_4",
                              "pr_climate_conv_5","pr_climate_conv_6","pr_climate_conv_7")

### Control

which(colnames(pilot_data)=="Q391_1")
which( colnames(pilot_data)=="Q393_10")
names(pilot_data)[162:170] <- c("ctrl_uhc_belief", "ctrl_uhc_supp",
                                #names for moral conviction            
                                "ctrl_uhc_conv_1","ctrl_uhc_conv_2","ctrl_uhc_conv_3","ctrl_uhc_conv_4",
                                "ctrl_uhc_conv_5","ctrl_uhc_conv_6","ctrl_uhc_conv_7")

#death penalty

which( colnames(pilot_data)=="Q397_1")
which( colnames(pilot_data)=="Q399_10")
names(pilot_data)[171:179] <- c("ctrl_death_belief", "ctrl_death_supp",
                                #names for moral conviction            
                                "ctrl_death_conv_1","ctrl_death_conv_2","ctrl_death_conv_3","ctrl_death_conv_4",
                                "ctrl_death_conv_5","ctrl_death_conv_6","ctrl_death_conv_7")

#exercise
which( colnames(pilot_data)=="Q405_10")
which( colnames(pilot_data)=="Q195_10")
names(pilot_data)[180:188] <- c("ctrl_exercise_belief", "ctrl_exercise_supp",
                                #names for moral conviction            
                                "ctrl_exercise_conv_1","ctrl_exercise_conv_2","ctrl_exercise_conv_3","ctrl_exercise_conv_4",
                                "ctrl_exercise_conv_5","ctrl_exercise_conv_6","ctrl_exercise_conv_7")

#climate
which( colnames(pilot_data)=="Q411_10")
names(pilot_data)[189:197] <- c("ctrl_climate_belief", "ctrl_climate_supp",
                                #names for moral conviction            
                                "ctrl_climate_conv_1","ctrl_climate_conv_2","ctrl_climate_conv_3","ctrl_climate_conv_4",
                                "ctrl_climate_conv_5","ctrl_climate_conv_6","ctrl_climate_conv_7")

######
######
######

#names are set, lets try to get the 'melt' done properly?
#lmao need to make sure to set each condition differently ofc

#chop out the extra columns, then look and see if our melt is done rigth

which( colnames(pilot_data)=="Like_Free_R")
pilot_data<-pilot_data[,-c(198:237)]

pilot_data_long<-melt(setDT(pilot_data),measure=patterns("uhc_belief", "uhc_persuade", "uhc_supp", 
          "uhc_conv_1", "uhc_conv_2", "uhc_conv_3", "uhc_conv_4","uhc_conv_5", "uhc_conv_6", "uhc_conv_7",
          "death_belief", "death_persuade", "death_supp", 
          "death_conv_1", "death_conv_2", "death_conv_3", "death_conv_4","death_conv_5", "death_conv_6", "death_conv_7",
          "exercise_belief", "exercise_persuade", "exercise_supp", 
          "exercise_conv_1", "exercise_conv_2", "exercise_conv_3", "exercise_conv_4","exercise_conv_5", "exercise_conv_6", "exercise_conv_7",
          "climate_belief", "climate_persuade", "climate_supp", 
          "climate_conv_1", "climate_conv_2", "climate_conv_3", "climate_conv_4","climate_conv_5", "climate_conv_6", "climate_conv_7"), 
               value.name=c("uhc_belief_c", "uhc_persuadable", "uhc_support",
"uhc_conviction_1","uhc_conviction_2","uhc_conviction_3", "uhc_conviction_4", "uhc_conviction_5", "uhc_conviction_6", "uhc_conviction_7",
"death_belief_c", "death_persuadable", "death_support",
"death_conviction_1","death_conviction_2","death_conviction_3", "death_conviction_4", "death_conviction_5", "death_conviction_6", "death_conviction_7",
"exercise_belief_c", "exercise_persuadable", "exercise_support",
"exercise_conviction_1","exercise_conviction_2","exercise_conviction_3", "exercise_conviction_4", "exercise_conviction_5", "exercise_conviction_6", "exercise_conviction_7",
"climate_belief_c", "climate_persuadable", "climate_support",
"climate_conviction_1","climate_conviction_2","climate_conviction_3", "climate_conviction_4", "climate_conviction_5", "climate_conviction_6", "climate_conviction_7"),
               variable.name="condition")
#need to ensure that everything worked out correctly... look at how the code in the other portion worked
#and use that as reference to get everything working out right.

#i think we just have to clean out our empty rows and it's gucci.

#try to clean our our empty rows then check if it looks g
ind<-which(is.na(pilot_data_long$uhc_belief_c))
pilot_long_clean<-pilot_data_long[!ind,]

#we can directly reference the empty row using ==""
ind<-pilot_data_long$uhc_belief_c==""
sum(ind)

pilot_long_clean<-pilot_data_long[!ind,]


#great job


pilot_long_clean <- pilot_long_clean %>% mutate_if(is.character, as.numeric)

pilot_long_clean$condition<-str_replace_all(pilot_long_clean$condition, c("1"= "Moral Responsibility", "2" = "Moral Piggybacking"
                                                                          , "3" = "Pragmatic", "4" = "Hedonic"
                                                                          , "5" = "Control"))

#whoops have to reverse score item 2 WHOOPS
pilot_long_clean[,7]<-(0-pilot_long_clean[,7])
pilot_long_clean[,17]<-(0-pilot_long_clean[,7])
pilot_long_clean[,27]<-(0-pilot_long_clean[,7])
pilot_long_clean[,37]<-(0-pilot_long_clean[,7])

#summarize the aggregate measures
pilot_long_clean$uhc_moral_c<-rowMeans(pilot_long_clean[,6:12])
pilot_long_clean$death_moral_c<-rowMeans(pilot_long_clean[,16:22])
pilot_long_clean$exercise_moral_c<-rowMeans(pilot_long_clean[,26:32])
pilot_long_clean$climate_moral_c<-rowMeans(pilot_long_clean[,36:42])


write.csv(pilot_long_clean,'study2_pilot_clean.csv')

