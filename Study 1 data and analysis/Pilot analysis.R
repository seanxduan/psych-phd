### As per usual, lets start by getting tables setup ###
pilot<-read.csv("pilot_clean.csv")

#we're gonna start w/ gtsummary, since that's how we closed it out originally
library("lme4")
library("texreg")
library("ggplot2")
library("tidyverse")
library("gtable")
library("gtsummary")

#peel out only the demographic info?
d1<-pilot[,c(24,41:44)]
#lets get condition set-up properly as well lol


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