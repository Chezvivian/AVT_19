#导入function文件
Sys.setlocale("LC_ALL","Chinese")
setwd("~/1 PhD/1st year/AVT experiment/6 R and python/R script")
source("Script_CRITT/CRITTmergeAnnotations.R")
source("Script_CRITT/readTables.R")
source("Script_CRITT/progGraSpeech.R")
source("Script_CRITT/EO.R")

library(readr)
library(readxl)
library(ggplot2)
library("effects")
library(gridExtra)

### 1-5： R中导入数据、准备ref/hyp的步骤，输出hyp_ID.txt, ref_ID.txt
      # 需要数据：
          # "data/*.sg" 
          # "TER_texts/Sentence features.xlsx"
      # 输出：
          # 原始文本："AVT_hyp.txt, AVT_ref.txt"
          # 分词文本："AVT_hyp_token.txt，AVT_ref_token.txt"
          # 标签文本："hyp_ID.txt,ref_ID.txt"

### 6: Java中运行tercom.jar脚本，输出ter_all.pra

### 7-8: R中输出包括TER指标的ter_final.csv

### 9-10: 数据分析测试


# 1. AVT_merge: 初始数据集创建。

  # 导入sg tables：共588条/59个变量。(需要数据：data目录下所有sg table)
AVT_sg <- readCRITTables(c("data/"), "*.sg$", path='.')
  
  # 增加2个ID标签：Part_Text_Seg (ID), Text_Seg。
AVT_sg$ID <- as.factor(paste(AVT_sg$Session,AVT_sg$Id,sep="_"))
AVT_sg$Text_Seg <- as.factor(paste(AVT_sg$Text, AVT_sg$STseg, sep = "_"))
 
  # 增加2个特征标签：键盘数量(Ins+Del)、文本类型(doc/drama)。
AVT_sg$Keystroke <- AVT_sg$Ins + AVT_sg$Del 
AVT_sg <- within(AVT_sg,{
  Text_type <- NA
  Text_type[Text == 1|Text==2] <- "doc"
  Text_type[Text == 3|Text==4] <- "drama"
})
  # 导入原文特征：包括原文、MT译文、句长、低频词数量、比例。（需要数据：features.xlsx)
AVT_feature <- read_excel(c("TER_texts/Sentence features.xlsx"))

  # 合并为初始数据集：AVT_merge。
AVT_merge <- merge(AVT_sg,AVT_feature,by="Text_Seg")

# 2. 提取ref/hyp.txt的原文本--清理译文中原来的分词(下划线符号）。
  library(stringr)
  AVT_merge$String <- str_replace_all(AVT_merge$String,"_","")
  AVT_merge$String <- str_trim(AVT_merge$String)

  # 输出"AVT_hyp.txt, AVT_ref.txt"文件，准备下一步分词。
AVT_hyp <- AVT_merge$MT_result
AVT_ref <- AVT_merge$String
  write.table(AVT_hyp,file="TER_texts/AVT_hyp.txt",sep="\n",fileEncoding = "UTF-8",row.names=FALSE,col.names=FALSE,quote=FALSE)  
  write.table(AVT_ref,file="TER_texts/AVT_ref.txt",sep="\n",fileEncoding = "UTF-8",row.names=FALSE,col.names=FALSE,quote=FALSE)  

# 3. ref/hyp分词：输出"AVT_hyp/ref_token.txt"

    #用jiebaR进行分词,worker()与segment()命令。注意不要重复写入。
library(jiebaR)
token_hyp <- worker(symbol=T,encoding="UTF-8",write=T,output="TER_texts/AVT_hyp_token.txt",bylines=TRUE)
segment("TER_texts/AVT_hyp.txt",token_hyp)
token_ref <- worker(symbol=T,encoding="UTF-8",write=T,output="TER_texts/AVT_ref_token.txt",bylines=TRUE)
segment("TER_texts/AVT_ref.txt",token_ref)

# 4. ref/hyp加标签：读取分好词的2个texts, 每个segment添加unique ID: Part_Text_Seg。！注意不要重复写入。
hyp_token <-read_delim("TER_texts/AVT_hyp_token.txt",delim="\n",col_names=c("hyp_token"))
ref_token <- read_delim("TER_texts/AVT_ref_token.txt",delim="\n",col_names=c("ref_token"))
head(ref_token)
  #添加ID，string merge, 准备计算TER
library(stringr)
hyp_token$ID <- as.factor(AVT_merge$ID)
ref_token$ID <- as.factor(AVT_merge$ID)
texts_token <- merge(hyp_token,ref_token)
texts_token$ID <-str_c("(",texts_token$ID,")")
texts_token$hyp <- str_c(texts_token$hyp_token,texts_token$ID)
texts_token$ref <- str_c(texts_token$ref_token,texts_token$ID)

  #因为P11_3_4的翻译任务中，被试没有译文,TER无法计算。手工添加标签，内容为空。
texts_token[c(which(texts_token$ID=="(P11_T3_4)")),5] <- texts_token[c(which(texts_token$ID=="(P11_T3_4)")),1]

# 5. ref/hyp.txt准备完成，写出，进入Java运行。
write.table(texts_token$hyp,file="TER_texts/hyp_ID.txt",sep="\n",fileEncoding = "UTF-8",row.names=FALSE,col.names=FALSE,quote=FALSE)
write.table(texts_token$ref,file="TER_texts/ref_ID.txt",sep="\n",fileEncoding = "UTF-8",row.names=FALSE,col.names=FALSE,quote=FALSE)


# 6. 运行Java脚本，输出6个TER文件。
  # WSL中(Linux环境，默认UTF-8)运行tercom.7.25.jar
  # java -jar tercom.7.25.jar -r ../ref_ID.txt -h ../hyp_ID.txt -n output/ter_all
  # 完成，得到6个TER输出文件
  # 除去清理的1行无效值，共输出587条结果。

# 7. 读取TER指标：从输出文件.pra中读取TER的3个指标：
TER_pra <- read_delim("TER_texts/tercom/output/ter_all.pra",delim="\n",col_names=c("pra"))
TER_pra <- as.matrix(TER_pra)
  str(TER_pra)
  is.atomic(TER_pra)
# 用正则表达式，查找4个特征
  # Sentence ID
  library(stringr)
ID <- str_extract(TER_pra,"^Sentence ID:\\s*(.*)\\s*$") #Sentence ID: P01_2_1:1
  ID <- ID[!is.na(ID)]
  # Alignment
alignment <- str_extract(TER_pra,"^Alignment:\\s*(.*)\\s*$") #Alignment: ( D D     I  )
  alignment <- alignment[!is.na(alignment)]
 # NumShifts
numshifts <- str_extract(TER_pra,"^NumShifts:\\s*(.*)\\s*$") #NumShifts: 0
  numshifts <- numshifts[!is.na(numshifts)]
  # Score
score <- str_extract(TER_pra,"^Score:\\s\\d*\\.\\d*\\s") #Score: 0.0
  score <- score[!is.na(score)]
  # Calculation
calc <- str_extract(TER_pra,"\\((.*)/(.*)\\)") #Calculation: (0.0/7.0)
  calc <- calc[!is.na(calc)]
  # 合并为TER_all，去除多余字符
TER_all <- data.frame(ID,alignment,numshifts,score,calc)
  TER_all$ID <- str_remove(TER_all$ID,"(Sentence ID:\\s)")
  TER_all$ID <- str_remove(TER_all$ID,":1$")
  TER_all$alignment <- str_remove(TER_all$alignment,"Alignment:\\s")
  TER_all$numshifts <- str_remove(TER_all$numshifts,"NumShifts:\\s")
  TER_all$score <- as.numeric(str_remove(TER_all$score,"Score:\\s"))
  TER_all$calc <- str_remove_all(TER_all$calc,"[\\(\\)]")

# 8. 写出TER指标。添加TER指标到AVT_out中，通过ID合并，写出csv文件。
AVT_ter_final <- merge(AVT_merge,TER_all)
write.csv(AVT_ter_final,"TER_texts/ter_final.csv",fileEncoding="UTF-8")

# 9. 数据分析测试：从csv中读取数据。
#测试读取ter_final.csv
TER_final <- read_delim("TER_texts/ter_final.csv",delim=",",col_names=T)
TER_final$X1 <- NULL

# 10. 因子分析

# 查看文本变量之间的差异
library(ggplot2)
ggplot(TER_final,aes(x=Text_type,y=score,fill=Task))+
  geom_boxplot()+
  xlab("TER in texts and tasks")

ggplot(TER_final,aes(x=Text_type,y=score,fill=Text_type))+
  geom_boxplot()+
  xlab("TER in texts and tasks")
# lm
summary(lm(score~Task*Text_type,data=TER_final))

# ANOVA
summary(aov(score~Task*Text_type,data=TER_final))

# lmer
library(lme4)
mod1 <- lme4::lmer(score~Task*Text_type+(1|Part),data=TER_final)
summary(mod1)

mod2 <- lme4::lmer(score~Task*Text_type+(1|Text_Seg),data=TER_final)
summary(mod2)

coef(summary(mod2))
VarCorr(mod2)
str(resid(mod2))

ggplot(fortify(mod2), aes(Text_type, score, color=Task)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=.fitted), fun.y=mean, geom="line")

ggplot(mod2, aes(Text_type, score, color=Task)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=.fitted), fun.y=mean, geom="line")

# glmer

mod3 <- lme4::glmer(score~Task*Text_type+(1|Part),data=TER_final,family="poisson")
summary(mod3)

