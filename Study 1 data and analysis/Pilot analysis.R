#basic data, analysis, and prep, for our 'pilot/study 1' data!

pilot<-read.csv("Study 1 data and analysis/pilot_data.csv")

#remove two 'blank' rows of data
pilot_data<-pilot[-c(1,2),]

#remove those who didn't finish?
pilot_data<-pilot_data[!(pilot_data$Finished=="False"),]

#delete unnecessary front/back columns?
pilot_data<-pilot_data[,-c(1:5,7:17,109:117)]

library(data.table)

#we need to rename our pre cols so they don't get aggregated it
names(pilot_data)[2:5] <- c("Pre_P1", "Pre_P2", "Pre_P3","Pre_P4")

#left turn! lets try to summate our number line task first!
#lets try pulling it into secondary matrice and work w/ that?

number_line<-pilot_data[,73:92]

number_line<-sapply(number_line, as.numeric)

#create a matrix of reference values and then subtract them to find relative
#score? note that the second half are out of a range of 5, so divide by 5 for proportion

v<-c(1/19, 1/7, 1/4, 3/8, 1/2,
     4/7, 2/3, 7/9, 5/6, 12/13,
     1/19, 4/7, 7/5, 13/9, 8/3,
     11/4, 10/3, 7/2, 17/4, 9/2)
length(v)
s_value<-matrix(v, nrow=42, ncol=length(v), byrow=TRUE)

nl_ans<-number_line - s_value
nl_ans[,11:20]<-(nl_ans[,11:20]/5)
#total amount off (by proportion)
pilot_data$NLINE<-(rowSums(abs(nl_ans)))

#now we have a total number-line score, and can sort by proportion?

#lets do the same for SNS

library(tidyverse)

SNS_mat<-select(pilot_data, contains("SNS"))
SNS_mat<-as.data.frame(SNS_mat)
SNS_mat$SNS_1_1<-as.numeric(SNS_mat$SNS_1_1)
SNS_mat$SNS_2_1<-as.numeric(SNS_mat$SNS_2_1)
SNS_mat$SNS_3_1<-as.numeric(SNS_mat$SNS_3_1)
SNS_mat$SNS_4_1<-as.numeric(SNS_mat$SNS_4_1)
SNS_mat$SNS_5_1<-as.numeric(SNS_mat$SNS_5_1)
SNS_mat$SNS_6_1<-as.numeric(SNS_mat$SNS_6_1)
SNS_mat$SNS_7_1<-as.numeric(SNS_mat$SNS_7_1)
SNS_mat$SNS_8_1<-as.numeric(SNS_mat$SNS_8_1)
SNS_score<-rowMeans(SNS_mat, na.rm = TRUE)

pilot_data$SNS_SCORE<-SNS_score

#lets clean out our col of pre-items!
pilot_data_w_score<-pilot_data[,-c(65:92)]
#great job chiefo
?write.csv
write.csv(pilot_data_w_score, file = "pilot_data_score.csv")

#figure out how to structure the names themselves to be more functional?
#maybe just do it in xl :eyes: lol
names(pilot_data_w_score)[]