#Password Analysis
#Length of the password
#Special Char
#Number
#Capital
#Repeat Characters
  
library(data.table)
library(dplyr)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(dygraphs)
library(ggplot2)

setwd("/Users/ashutosh_lim/Documents/LearnR")
inpf <- fread(input = "./Data/ttd")
df <- as.data.table(inpf)
setnames(df,"!!!!!","vals")
setnames(df,"V1","vals")
#sapply(hdf$vals,function(xx) { if (grepl("[[:digit:]]",xx) >0) 1 else 0})
hdf$isNum<-sapply(hdf$vals,function(xx) { if (grepl("[[:digit:]]",xx) >0) 1 else 0})
hdf$isUpp<-sapply(hdf$vals,function(xx) { if (grepl("[[:upper:]]",xx) >0) 1 else 0})
hdf$isSpc<-sapply(hdf$vals,function(xx) { if (grepl("[^[:alnum:]]",xx) >0) 1 else 0})
hdf$len<-nchar(hdf$vals)
dfg<-group_by(df,vals)
dfg<-summarise(dfg,dval=n_distinct(vals),dupval=n())


#ggplot(tot_acc,aes(x=tot_acc$States.UTs,y=tot_acc$X2011,group=1))+
#  + geom_line() +
#  + theme(axis.text.x = element_text(angle = 90, hjust = 1))