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
library(textstem)
library(stringr)

# 1.AVT_st: 导入st文件
AVT_st <- readCRITTables(c("data/"), "*.st$", path='.')
token <- AVT_st[,c(1,2,8,10)]
token <- unique(token)
token$SToken <- stringr::str_replace_all(token$SToken,"_","'")
token$word <- tolower(lemmatize_words(token$SToken))
token$freq <- NA
token$multiple <- NA

# 2. BNC_word: 导入BNC frequency list
BNC_freq <- read.delim("data_BNC/all.num",sep=" ",header=T,col.names=c("freq","word","pos","numfiles"))
BNC_freq$word <- as.character(BNC_freq$word)
class(BNC_freq$freq)

# 3. token匹配BNC_word:查找frequency
for (i in 1:length(token$word)) {
    if (length(BNC_freq[which(BNC_freq$word==token$word[i]),]$freq)==1){
      token$freq[i] <- as.numeric(BNC_freq[which(BNC_freq$word==token$word[i]),]$freq)
    }else if (length(BNC_freq[which(BNC_freq$word==token$word[i]),]$freq)> 1){
      token$freq[i] <- sum(BNC_freq[which(BNC_freq$word==token$word[i]),]$freq)
      token$multiple[i] <- "Yes"
    }else if (length(BNC_freq[which(BNC_freq$word==token$word[i]),]$freq)== 0){
      token$freq[i] <- 0
      }
}
  # 将标点符号标为"---",不列入词频计算
for (i in c("-","&",",",".","?")){
  token$freq[which(token$word==i)] <- 0
}

sum(token$freq=="---")
sum(token$freq==0)
length(token$freq)

# 4. 合并st和frequency文件
  # st+ID
#AVT_st$Text_seg_ID <- as.factor(paste(AVT_st$Text,AVT_st$STseg,AVT_st$Id,sep="_"))
#levels(AVT_st$Text_seg_ID)
  # frequency+ID
#token$Text_seg_ID <- as.factor(paste(token$Text,token$STseg,token$Id,sep="_"))
#levels(token$Text_seg_ID)
#token_ID <- subset(token, select=-c(1:4))
  # merge
#AVT_freq <- merge(AVT_st,token_ID,by="Text_seg_ID")

# 4. 计算segment平均word frequency
token$Text_Seg <- as.factor(paste(token$Text,token$STseg,sep="_"))
segment_freq <- unique(subset(token,select=c(8)))
segment_freq$total_freq <- NA
for (i in segment_freq$Text_Seg){
  segment_freq[which(segment_freq$Text_Seg==i),]$total_freq <- sum(as.numeric(token[which(token$Text_Seg==i),]$freq))
  }

# 5. 导入feature
feature <- read_excel(c("TER_texts/Sentence features.xlsx"))
seg_freq_feature <- merge(segment_freq,feature)
seg_freq_feature$avg_freq <- (seg_freq_feature$total_freq)/(seg_freq_feature$WordNum)
seg_freq_feature$log_avg_freq <- log(seg_freq_feature$avg_freq)

# 6. 导出word_frequency.csv
word_frequency <- seg_freq_feature[,c(1,2,8,9)]
write.csv(word_frequency,"data_BNC/word_frequency.csv",fileEncoding="UTF-8")
