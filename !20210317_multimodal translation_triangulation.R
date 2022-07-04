#导入function文件
Sys.setlocale("LC_ALL","Chinese")
setwd("~/1 PhD/Y1-RUC/AVT experiment/6 R and python/R script")
source("Script_CRITT/CRITTmergeAnnotations.R")
source("Script_CRITT/readTables.R")
source("Script_CRITT/progGraSpeech.R")
source("Script_CRITT/EO.R")
source("Functions/Overdispersion.R")

library(readr)
library(readxl)
library(dplyr)
library(lme4)
library(MuMIn)
library(lmerTest)
library(glmmTMB)
library(performance)
library(MASS)
library(boxcoxmix)
library(moments)

library(broom)
library(ggplot2)
library("effects")
library(gridExtra)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(tidyr)
library(qwraps2)
library(ggfortify)
library(ggeffects)

require(car)
require(MASS)  

set_theme(theme.font="sans")
## Part I. 数据导入+合并
# 导入AVT_ter_final(包含sg, feature, TER数据)
# 导入segment average(word_frequency + Htra数据)
AVT_ter <- read_delim("TER_texts/ter_final.csv",delim=",",col_names=T)
AVT_ter[c("...1","MIns","MDel","AIns","ADel","SIns","SDel","Yawat")] <- NULL
AVT_ter[c(14,15,16,17,18,20,21,23,24,26,27,29,30,34,36)] <- AVT_ter[c(14,15,16,17,18,20,21,23,24,26,27,29,30,34,36)]/1000

word_frequency <- read_delim("data_BNC/word_frequency.csv",delim=",",col_names=T)
word_frequency$X1 <- NULL
AVT_all <- merge(AVT_ter,word_frequency,by="Text_Seg")
AVT_all <- as.data.frame(AVT_all)
#AVT_HT <- AVT_all[AVT_all$Task == "T",]

# 取出基础的的15列，区分文本类型

data <- AVT_all[,c("Text_Seg","Task","Text","Part","Text_type","TokS","WordNum","Num_K1","ComplexPercent","avg_freq","total_freq","log_avg_freq","Ins","Del","Keystroke","TrtS","TrtT","FixS","FixT","HTra")]
data <- within(data,{
  Text_type[Text_type == "doc"] <- "documentary"
})

# 增加时间数据： TreadDur, TproDur, TtypeDur, TpauseDur 几列
data$TreadDur <- AVT_all$TrtS + AVT_all$TrtT # TreadDur = TrtS +TrtT
data$TproDur <- AVT_all$Dur # TproDur = Dur = PreGap + total typing duration
data$TtypeDur <- AVT_all$TG300+AVT_all$TD300 # TtypeDur = TG + TD (sum of all thresholds is the same)
data$TpauseDur_300 <- AVT_all$TG300 #TpauseDur= TG
data$TpauseDur_500 <- AVT_all$TG500
data$TpauseDur_1000 <- AVT_all$TG1000
data$TpauseDur_2000 <- AVT_all$TG2000
data$TpauseDur_5000 <- AVT_all$TG5000

# 增加 count数据：TfixNum, TkeyNum, TpauseNum
data$TfixNum <- AVT_all$FixS + AVT_all$FixT # TfixNum = FixS + FixT
data$TkeyNum <- AVT_all$Del + AVT_all$Ins # TkeyNum = Del + Ins
data$TpauseNum_300 <- AVT_all$TB300 # TpauseNum = TB
data$TpauseNum_500 <- AVT_all$TB500
data$TpauseNum_1000 <- AVT_all$TB1000
data$TpauseNum_2000 <- AVT_all$TB2000
data$TpauseNum_5000 <- AVT_all$TB5000

## 查看两种文本的平均句长、词频
inspect <- unique(data[,c(1,3,5,6,7,10)])
inspect_doc <- inspect[which(inspect$Text_type == "documentary"),]
inspect_doc_1 <- inspect_doc[c(1:3),]
inspect_doc_2 <- inspect_doc[c(4:7),]
inspect_drama <- inspect[which(inspect$Text_type == "drama"),]
inspect_drama_1 <- inspect_drama[c(1:13),]
inspect_drama_2 <- inspect_drama[c(14:21),]

summary(inspect_doc_1)
sum(inspect_doc_1$WordNum)
mean(inspect_doc_1$avg_freq)
sd(inspect_doc_1$WordNum)
sd(inspect_doc_1$avg_freq)

summary(inspect_doc_2)
sum(inspect_doc_2$WordNum)
sd(inspect_doc_2$WordNum)
sd(inspect_doc_2$avg_freq)
mean(inspect_doc_2$avg_freq)

summary(inspect_drama_1)
sum(inspect_drama_1$WordNum)
sd(inspect_drama_1$WordNum)
sd(inspect_drama_1$avg_freq)
mean(inspect_drama_1$avg_freq)

summary(inspect_drama_2)
sum(inspect_drama_2$WordNum)
sd(inspect_drama_2$WordNum)
sd(inspect_drama_2$avg_freq)
mean(inspect_drama_2$avg_freq)

sum(inspect_drama_1$WordNum)
sum(inspect_drama_2$WordNum)

sd(c(95,98))

mean(c(95,98))


summary(inspect_doc)
sd(inspect_doc$WordNum)
sd(inspect_doc$avg_freq)

summary(inspect_drama)
sd(inspect_drama$WordNum)
sd(inspect_drama$avg_freq)
### Part II. 数据分析

## Question 1. Compare PE and HT effort in drama texts
# data: select "drama" labels as subset of "Text_type" column
Q1_data <- data #[which(data$Text_type == "drama"),]

# Q1-Technical: Ins, Del, Keystroke
## Ins
hist(Q1_data$Ins)
hist(log(Q1_data$Ins))

hist(Q1_data$Ins[which(Q1_data$Task == "P")])
hist(log(Q1_data$Ins[which(Q1_data$Task == "P")]))


lme_Q1_ins <- lmer(log(Ins+1)~Task+TokS+log_avg_freq+(1|Part),
                   data=Q1_data)


key_1 <- plot_model(lme_Q1_ins,type="pred",terms=c("Task"),
                    axis.title=c("","Number"),title="Ins",
                    dodge=1)+ylim(0,65)
plot(key_1)

summary(lme_Q1_ins)
fixef(lme_Q1_ins)

## Del
hist(Q1_data$Del)
lme_Q1_del <- lmer(log(Del+1)~Task+TokS+log_avg_freq+(1|Part),
                   data=Q1_data)
summary(lme_Q1_del)

key_2 <- plot_model(lme_Q1_del,type="pred",terms=c("Task"),
                    axis.title=c("","Number"),title="Del",
                    dodge=1)+ylim(0,65)


plot(key_2)
## Keystroke
hist(Q1_data$Keystroke)
lme_Q1_key <- lmer(log(Keystroke+1)~Task+TokS+log_avg_freq+(1|Part),
                   data=Q1_data)
summary(lme_Q1_key)

key_3 <- plot_model(lme_Q1_key,type="pred",terms=c("Task"),
                    axis.title=c("","Number"),title="Key_all",
                    dodge=1)+ylim(0,65)

## Table: summary of 3 keystroke behavior
table_k1 <- data.frame(get_model_data(lme_Q1_ins,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_k2 <- data.frame(get_model_data(lme_Q1_del,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_k3 <- data.frame(get_model_data(lme_Q1_key,type="est"))[,c(1,2,3,6,8,4,5,9)]

table_k1$indicator <- "insertion"
table_k2$indicator <- "deletion"
table_k3$indicator <- "keystroke"

table_Q1_key <- rbind(table_k1,table_k2,table_k3)
write.csv(table_Q1_key,file="Plot_2Q/table_Q1_key.csv",row.names=F,fileEncoding="UTF-8")

## Plot: summary of 3 keystroke behavior
set_theme(base=theme_bw(),
          theme.font = "sans",
          title.align="center",
          panel.gridcol.x="white")
plot_Q1_key <- grid.arrange(key_1,key_2,key_3, nrow=1, ncol=3)

ggsave("plot_Q1_key.tiff",plot=plot_Q1_key,device="tiff", dpi = 400, width = 20, height = 12, units = "cm",path=choose.dir())



# Q1-Cognitive: FixS, FixT, TfixNum, TrtS, TrtT, TreadDur, TpauseDur_300, TpauseNum_300
lme_Q1_FixS <- lmer(log(FixS+1)~Task+TokS+log_avg_freq+(1|Part),
                    data=Q1_data)
lme_Q1_FixT <- lmer(log(FixT+1)~Task+TokS+log_avg_freq+(1|Part),
                    data=Q1_data)
lme_Q1_TfixNum <- lmer(log(TfixNum+1)~Task+TokS+log_avg_freq+(1|Part),
                       data=Q1_data)
lme_Q1_TrtS <- lmer(log(TrtS+1)~Task+TokS+log_avg_freq+(1|Part),
                    data=Q1_data)
lme_Q1_TrtT <- lmer(log(TrtT+1)~Task+TokS+log_avg_freq+(1|Part),
                    data=Q1_data)
lme_Q1_TreadDur <- lmer(log(TreadDur+1)~Task+TokS+log_avg_freq+(1|Part),
                        data=Q1_data)
lme_Q1_pauseDur <- lmer(log(TpauseDur_300+1)~Task+TokS+log_avg_freq+(1|Part),
                        data=Q1_data)
lme_Q1_pauseNum <- lmer(log(TpauseNum_300+1)~Task+TokS+log_avg_freq+(1|Part),
                        data=Q1_data)

# Table: summary of 8 cognitive indicators
table_q1_c1 <- data.frame(get_model_data(lme_Q1_FixS,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q1_c2 <- data.frame(get_model_data(lme_Q1_FixT,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q1_c3 <- data.frame(get_model_data(lme_Q1_TfixNum,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q1_c4 <- data.frame(get_model_data(lme_Q1_TrtS,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q1_c5 <- data.frame(get_model_data(lme_Q1_TrtT,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q1_c6 <- data.frame(get_model_data(lme_Q1_TreadDur,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q1_c7 <- data.frame(get_model_data(lme_Q1_pauseDur,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q1_c8 <- data.frame(get_model_data(lme_Q1_pauseNum,type="est"))[,c(1,2,3,6,8,4,5,9)]

table_q1_c1$indicator <- "FixNum_S"
table_q1_c2$indicator <- "FixNum_T"
table_q1_c3$indicator <- "FixNum"
table_q1_c4$indicator <- "FixDur_S"
table_q1_c5$indicator <- "FixDur_T"
table_q1_c6$indicator <- "FixDur"
table_q1_c7$indicator <- "PauseDur"
table_q1_c8$indicator <- "PauseNum"

table_Q1_cognitive <- rbind(table_q1_c1,table_q1_c2,table_q1_c3,table_q1_c4,table_q1_c5,
                            table_q1_c6,table_q1_c7,table_q1_c8)                          
write.csv(table_Q1_cognitive,file="Plot_2Q/table_Q1_cognitive.csv",row.names=F,fileEncoding="UTF-8")

# Plot: summary of 8 cognitive indicators
q1_c1 <- plot_model(lme_Q1_FixS,type="pred",terms=c("Task"),
                    axis.title=c("","number"),title="FixN_S",
                    dodge=1)+ylim(0,110)
q1_c2 <- plot_model(lme_Q1_FixT,type="pred",terms=c("Task"),
                    axis.title=c("","number"),title="FixN_T",
                    dodge=1)+ylim(0,110)
q1_c3 <- plot_model(lme_Q1_TfixNum,type="pred",terms=c("Task"),
                    axis.title=c("","number"),title="FixN_all",
                    dodge=1)+ylim(0,110)
q1_c4 <- plot_model(lme_Q1_TrtS,type="pred",terms=c("Task"),
                    axis.title=c("","second"),title="FixD_S",
                    dodge=1)+ylim(0,35)
q1_c5 <- plot_model(lme_Q1_TrtT,type="pred",terms=c("Task"),
                    axis.title=c("","second"),title="FixD_T",
                    dodge=1)+ylim(0,35)
q1_c6 <- plot_model(lme_Q1_TreadDur,type="pred",terms=c("Task"),
                    axis.title=c("","second"),title="FixD_all",
                    dodge=1)+ylim(0,35)


plot_Q1_cognitive <- grid.arrange(q1_c1,q1_c2,q1_c3, q1_c4, q1_c5,q1_c6,nrow=2, ncol=3)
ggsave("plot_Q1_cognitive.tiff",plot=plot_Q1_cognitive,dpi = 400, device="tiff",path=choose.dir())

# Q1-Temporal: TtypeDur
lme_Q1_dur <- lmer(log(TtypeDur+1)~Task+TokS+log_avg_freq+(1|Part),
                   data=Q1_data)

# Table: duration 
table_q1_dur <- data.frame(get_model_data(lme_Q1_dur,type="est"))[,c(1,2,3,6,8,4,5,9)]
write.csv(table_q1_dur,file="Plot_2Q/table_Q1_temporal.csv",row.names=F,fileEncoding="UTF-8")

# Plot: duration
q1_dur <- plot_model(lme_Q1_dur,type="pred",terms=c("Task"),
                     axis.title=c("","second"),title="Dur",
                     dodge=1)+ylim(1,20)
plot(q1_dur)
ggsave("plot_Q1_dur.tiff",plot=q1_dur,device="tiff",dpi = 400,width = 8, height = 8, units = "cm",path=choose.dir())

# Q1-HTra: Htra
hist(Q1_data$HTra)
lme_Q1_htra <- lmer(HTra~Task+TokS+log_avg_freq+(1|Part),
                    data=Q1_data)

summary(lme_Q1_htra)

q1_htra <- plot_model(lme_Q1_htra,type="pred",terms=c("Task"),
                      axis.title=c("","count"),title="HTra",
                      dodge=1)
plot(q1_htra)







## Question 2. Compare PE effort in both texts
# data: select "P" labels as subset of "Task" column
Q2_data <- data[which(data$Task == "P"),]

# Q2-Technical: 
# Ins
lme_Q2_ins <- lmer(log(Ins+1)~Text_type+TokS+log_avg_freq+(1|Part),
                   data=Q2_data)
summary(lme_Q2_ins)


# Del
lme_Q2_del <- lmer(log(Del+1)~Text_type+TokS+log_avg_freq+(1|Part),
                   data=Q2_data)
summary(lme_Q2_del)

# Keystroke
lme_Q2_key <- lmer(log(Keystroke+1)~Text_type+TokS+log_avg_freq+(1|Part),
                   data=Q2_data)
summary(lme_Q2_key)

# Table: summary of 3 keystroke behavior
table_q2_k1 <- data.frame(get_model_data(lme_Q2_ins,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q2_k2 <- data.frame(get_model_data(lme_Q2_del,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q2_k3 <- data.frame(get_model_data(lme_Q2_key,type="est"))[,c(1,2,3,6,8,4,5,9)]

table_q2_k1$indicator <- "insertion"
table_q2_k2$indicator <- "deletion"
table_q2_k3$indicator <- "keystroke"

table_Q2_key <- rbind(table_q2_k1,table_q2_k2,table_q2_k3)
write.csv(table_Q2_key,file="Plot_2Q/table_Q2_key.csv",row.names=F,fileEncoding="UTF-8")


# Plot: summary of 3 keystroke behavior
q2_k1 <- plot_model(lme_Q2_ins,type="pred",terms=c("Text_type"),
                    axis.title=c("","number"),title="Ins",
                    dodge=1)+ylim(0,65)
q2_k2 <- plot_model(lme_Q2_del,type="pred",terms=c("Text_type"),
                    axis.title=c("","number"),title="Del",
                    dodge=1)+ylim(0,65)
q2_k3 <- plot_model(lme_Q2_key,type="pred",terms=c("Text_type"),
                    axis.title=c("","number"),title="Key_all",
                    dodge=1)+ylim(0,65)
plot_Q2_key <- grid.arrange(q2_k1,q2_k2,q2_k3, nrow=1, ncol=3)
ggsave("plot_Q2_key.tiff",plot=plot_Q2_key,device="tiff",dpi = 400,width = 20, height = 12, units = "cm",path=choose.dir())

plot(q2_k3)

# Q2-Cognitive:
lme_Q2_FixS <- lmer(log(FixS+1)~Text_type+TokS+log_avg_freq+(1|Part),
                    data=Q2_data)
lme_Q2_FixT <- lmer(log(FixT+1)~Text_type+TokS+log_avg_freq+(1|Part),
                    data=Q2_data)
lme_Q2_TfixNum <- lmer(log(TfixNum+1)~Text_type+TokS+log_avg_freq+(1|Part),
                       data=Q2_data)
lme_Q2_TrtS <- lmer(log(TrtS+1)~Text_type+TokS+log_avg_freq+(1|Part),
                    data=Q2_data)
lme_Q2_TrtT <- lmer(log(TrtT+1)~Text_type+TokS+log_avg_freq+(1|Part),
                    data=Q2_data)
lme_Q2_TreadDur <- lmer(log(TreadDur+1)~Text_type+TokS+log_avg_freq+(1|Part),
                        data=Q2_data)
lme_Q2_pauseDur <- lmer(log(TpauseDur_300+1)~Text_type+TokS+log_avg_freq+(1|Part),
                        data=Q2_data)
lme_Q2_pauseNum <- lmer(log(TpauseNum_300+1)~Text_type+TokS+log_avg_freq+(1|Part),
                        data=Q2_data)

# Table: summary of 8 cognitive indicators
table_q2_c1 <- data.frame(get_model_data(lme_Q2_FixS,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q2_c2 <- data.frame(get_model_data(lme_Q2_FixT,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q2_c3 <- data.frame(get_model_data(lme_Q2_TfixNum,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q2_c4 <- data.frame(get_model_data(lme_Q2_TrtS,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q2_c5 <- data.frame(get_model_data(lme_Q2_TrtT,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q2_c6 <- data.frame(get_model_data(lme_Q2_TreadDur,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q2_c7 <- data.frame(get_model_data(lme_Q2_pauseDur,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_q2_c8 <- data.frame(get_model_data(lme_Q2_pauseNum,type="est"))[,c(1,2,3,6,8,4,5,9)]

table_q2_c1$indicator <- "FixNum_S"
table_q2_c2$indicator <- "FixNum_T"
table_q2_c3$indicator <- "FixNum"
table_q2_c4$indicator <- "FixDur_S"
table_q2_c5$indicator <- "FixDur_T"
table_q2_c6$indicator <- "FixDur"
table_q2_c7$indicator <- "PauseDur"
table_q2_c8$indicator <- "PauseNum"

table_Q2_cognitive <- rbind(table_q2_c1,table_q2_c2,table_q2_c3,table_q2_c4,table_q2_c5,
                            table_q2_c6,table_q2_c7,table_q2_c8)                          
write.csv(table_Q2_cognitive,file="Plot_2Q/table_Q2_cognitive.csv",row.names=F,fileEncoding="UTF-8")

# Plot: summary of 8 cognitive indicators

q2_c1 <- plot_model(lme_Q2_FixS,type="pred",terms=c("Text_type"),
                    axis.title=c("","number"),title="FixN_S",
                    dodge=1)+ylim(0,110)
q2_c2 <- plot_model(lme_Q2_FixT,type="pred",terms=c("Text_type"),
                    axis.title=c("","number"),title="FixN_T",
                    dodge=1)+ylim(0,110)
q2_c3 <- plot_model(lme_Q2_TfixNum,type="pred",terms=c("Text_type"),
                    axis.title=c("","number"),title="FixN_all",
                    dodge=1)+ylim(0,110)
q2_c4 <- plot_model(lme_Q2_TrtS,type="pred",terms=c("Text_type"),
                    axis.title=c("","s"),title="FixD_S",
                    dodge=1)+ylim(0,35)
q2_c5 <- plot_model(lme_Q2_TrtT,type="pred",terms=c("Text_type"),
                    axis.title=c("","s"),title="FixD_T",
                    dodge=1)+ylim(0,35)
q2_c6 <- plot_model(lme_Q2_TreadDur,type="pred",terms=c("Text_type"),
                    axis.title=c("","s"),title="FixD_all",
                    dodge=1)+ylim(0,35)

plot_Q2_cognitive <- grid.arrange(q2_c1,q2_c2,q2_c3, q2_c4, q2_c5,q2_c6,nrow=2, ncol=3)
ggsave("plot_Q2_cognitive.tiff",plot=plot_Q2_cognitive,device="tiff",dpi = 400,path=choose.dir())


# Q2-Temporal: TtypeDur

lme_Q2_dur <- lmer(log(TtypeDur+1)~Text_type+TokS+log_avg_freq+(1|Part),
                   data=Q2_data)

summary(lme_Q2_dur)

# Table: duration 
table_q2_dur <- data.frame(get_model_data(lme_Q2_dur,type="est"))[,c(1,2,3,6,8,4,5,9)]
write.csv(table_q2_dur,file="Plot_2Q/table_Q2_temporal.csv",row.names=F,fileEncoding="UTF-8")

# Plot: duration
q2_dur <- plot_model(lme_Q2_dur, type="pred",terms=c("Text_type"),
                     axis.title=c("","second"),title="Dur",
                     dodge=1)+ylim(1,20)
plot(q2_dur)
ggsave("plot_Q2_dur.tiff",plot=q2_dur,device="tiff", dpi = 400, width = 8, height = 8, units = "cm",path=choose.dir())





## 3. Retrospective data
## 数据导入NASA, Question
# NASA
NASA <- read_excel("data_NASA_questions/NASA_score.xlsx")

## 1). NASA results
# Separate Participant and Text ID in NASA results, add text type.
for (i in 1:length(NASA$Session)){
  NASA$Part[i] <- unlist(strsplit(NASA$Session[i],"_",fixed=TRUE))[1]
  NASA$T_T[i] <- unlist(strsplit(NASA$Session[i],"_",fixed=TRUE))[2]
}

for (i in 1:length(NASA$Session)){
  NASA$Task[i] <- unlist(strsplit(NASA$T_T[i],"",fixed=TRUE))[1]
  NASA$Text[i] <- unlist(strsplit(NASA$T_T[i],"",fixed=TRUE))[2]
}

NASA$Text_type <- NA
NASA <- within(NASA,{
  Text_type[Text == 1|Text == 2] <- "documentary"
  Text_type[Text == 3|Text == 4] <- "drama"
})

NASA_HT <- NASA[NASA$Task == "T",]


# Separate Text ID in word frequency
word_frequency$Text <- NA
for (i in 1:length(word_frequency$Text_Seg)){
  word_frequency$Text[i] <- unlist(strsplit(word_frequency$Text_Seg[i],"_",fixed=TRUE))[1]
}

# calculate task-based text length, average word frequency for LME.
Task_based <- unique(AVT_HT[,c("Text","STseg","TokS")])
Task_based$total_tok <- NA
for (i in Task_based$Text){
  Task_based$total_tok[which(Task_based$Text == i)] <- sum(Task_based$TokS[which(Task_based$Text == i)])
}

Task_table <- unique(Task_based[,c(1,4)])
Task_table$total_freq <- NA
for (i in Task_table$Text){
  Task_table$total_freq[i] <- sum(word_frequency$total_freq[which(word_frequency$Text == i)]) 
}

Task_table$log_total_tok <- NA
Task_table$avg_freq <- NA
Task_table$log_avg_freq <- NA

Task_table$log_total_tok <- log(Task_table$total_tok)
Task_table$avg_freq <- Task_table$total_freq/Task_table$total_tok
Task_table$log_avg_freq <- log(Task_table$avg_freq)


# merge Task_table and NASA_HT
Task_NASA <- merge(Task_table, NASA_HT)


### NASA descriptive statistics
summary(NASA_HT$Mental_demand)

###!!! NASA effect with fixed factors
lmer_mental <- lmer(Mental_demand~Text_type+log_total_tok+log_avg_freq +(1|Part),data=Task_NASA)
lmer_effort <- lmer(Effort~Text_type+log_total_tok+log_avg_freq +(1|Part),data=Task_NASA)
lmer_frustration <- lmer(Frustration~Text_type+log_total_tok+log_avg_freq +(1|Part),data=Task_NASA)
lmer_performance<- lmer(Performance~Text_type+log_total_tok+log_avg_freq +(1|Part),data=Task_NASA)

summary(lmer_performance)

## plot tables
table_m <- data.frame(get_model_data(lmer_mental,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_e <- data.frame(get_model_data(lmer_effort,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_f <- data.frame(get_model_data(lmer_frustration,type="est"))[,c(1,2,3,6,8,4,5,9)]
table_p <- data.frame(get_model_data(lmer_performance,type="est"))[,c(1,2,3,6,8,4,5,9)]

table_m$indicator <- "mental"
table_e$indicator <- "effort"
table_f$indicator <- "frustration"
table_p$indicator <- "performance"

table_NASA <- rbind(table_m,table_e,table_f,table_p)     

## plot models
plot_m <- plot_model(lmer_mental,type="pred",terms=c("Text_type"),
             title="LMER on NASA mental demand score", dodge=1)
plot_e <- plot_model(lmer_effort,type="pred",terms=c("Text_type"),
                     title="LMER on Nasa effort score", dodge=1)
plot_f <- plot_model(lmer_frustration,type="pred",terms=c("Text_type"),
                     title="LMER on Nasa frustration score", dodge=1)
plot_p <- plot_model(lmer_performance,type="pred",terms=c("Text_type"),
                     title="LMER on Nasa performance score", dodge=1)

grid.arrange(plot_m,plot_e,plot_f, plot_p,nrow=2, ncol=2)



### NASA effects without fixed factors

lmer_mental_1 <- lmer(Mental_demand~Text_type+(1|Part),data=Task_NASA)
lmer_effort_1 <- lmer(Effort~Text_type+(1|Part),data=Task_NASA)
lmer_frustration_1 <- lmer(Frustration~Text_type +(1|Part),data=Task_NASA)
lmer_performance_1 <- lmer(Performance~Text_type +(1|Part),data=Task_NASA)


plot_1 <- plot_model(lmer_mental_1,type="pred",terms=c("Text_type"),
                     title="LMER on NASA mental demand score", dodge=1)
plot_2 <- plot_model(lmer_effort_1,type="pred",terms=c("Text_type"),
                     title="LMER on Nasa effort score", dodge=1)
plot_3 <- plot_model(lmer_frustration_1,type="pred",terms=c("Text_type"),
                     title="LMER on Nasa frustration score", dodge=1)
plot_4 <- plot_model(lmer_performance_1,type="pred",terms=c("Text_type"),
                     title="LMER on Nasa performance score", dodge=1)

grid.arrange(plot_1,plot_2,plot_3, plot_4,nrow=2, ncol=2)

