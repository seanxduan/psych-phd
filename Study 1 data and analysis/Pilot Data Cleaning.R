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
s_value<-matrix(v, nrow=nrow(pilot_data), ncol=length(v), byrow=TRUE)

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

pilot_data_w_score<-read.csv("pilot_data_score.csv")

#figure out how to structure the names themselves to be more functional?
#maybe just do it in xl :eyes: lol

which( colnames(pilot_data_w_score)=="Q121_4")

#no we're just gonna slug through in R b/c we can

#for the final responses

which( colnames(pilot_data_w_score)=="Q121_4")
names(pilot_data_w_score)[31:34] <- c("Post_High_1_10", "Post_High_2_10", "Post_High_3_10","Post_High_4_10")

which( colnames(pilot_data_w_score)=="Q176_1")
names(pilot_data_w_score)[54:57] <- c("Post_Low_1_10", "Post_Low_2_10", "Post_Low_3_10","Post_Low_4_10")


#for the guesses
which( colnames(pilot_data_w_score)=="Post_Int_1_10")
names(pilot_data_w_score)[19:22] <- c("Pre_High_Guess_1", "Pre_High_Guess_2", "Pre_High_Guess_3","Pre_High_Guess_4")

which(colnames(pilot_data_w_score)=="Q163_10")
names(pilot_data_w_score)[42:45] <- c("Pre_Low_Guess_1", "Pre_Low_Guess_2", "Pre_Low_Guess_3","Pre_Low_Guess_4")

#and for the surprise score?
which( colnames(pilot_data_w_score)=="Q114_4")
names(pilot_data_w_score)[23:26] <- c("High_Sup_1", "High_Sup_2", "High_Sup_3","High_Sup_4")

which( colnames(pilot_data_w_score)=="Q169_4")
names(pilot_data_w_score)[46:49] <- c("Low_Sup_1", "Low_Sup_2", "Low_Sup_3","Low_Sup_4")

which( colnames(pilot_data_w_score)=="Q116_10")
names(pilot_data_w_score)[27:30] <- c("High_Current_1", "High_Current_2", "High_Current_3","High_Current_4")

which( colnames(pilot_data_w_score)=="Q171_10")
names(pilot_data_w_score)[50:53] <- c("Low_Current_1", "Low_Current_2", "Low_Current_3","Low_Current_4")



#names are set, lets try to get the 'melt' done properly?

pilot_data_long<-melt(setDT(pilot_data_w_score),measure=patterns("1_10", "2_10", "3_10", "4_10",
                                                                 "Sup_1", "Sup_2", "Sup_3", "Sup_4",
                                                                 "Guess_1", "Guess_2", "Guess_3", "Guess_4",
                                                                 "Current_1", "Current_2", "Current_3", "Current_4",
                                                                 "Age","Gender","Race","School", "Like_Free_R","Hard_Free_R"), 
               value.name=c("P1", "P2", "P3", "P4",
                            "Suprise_1","Suprise_2","Suprise_3", "Suprise_4",
                            "GSS_1", "GSS_2", "GSS_3", "GSS_4",
                            "CRR_1", "CRR_2", "CRR_3", "CRR_4",
                            "Age","Gender","Race","School_Year","Like_Free_Response", "Hard_Free_Response"),
               variable.name="condition")

#try to clean our our empty rows then check if it looks g
ind<-which(is.na(pilot_data_long$P1))
pilot_long_clean<-pilot_data_long[!ind,]

ind<-which(is.na(pilot_long_clean$P2))
pilot_long_clean<-pilot_long_clean[!ind,]

#great job
#need to cut out the politics answers b/c no-one freaking answered them
#make it a forced response next time

pilot_long_clean<-pilot_long_clean[,-c(19:20)]

pilot_long_clean<-read.csv("pilot_clean.csv")
pilot_long_clean$condition<-str_replace_all(pilot_long_clean$condition, c("1"= "High", "2" = "Low"))

write.csv(pilot_long_clean,'pilot_clean.csv')

