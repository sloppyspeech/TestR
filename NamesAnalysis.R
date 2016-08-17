library(wordcloud)
library(SnowballC)
library(tm)
library(stringr)
library(RColorBrewer)
library(stringi)
library(data.table)
library(dplyr)
library(ggplot2)
remove(list=ls())

names <- read.delim("./Data/Languages/names_more_than_one.txt")
names<-as.data.table(names)
setnames(names,c("Year","NumPeople","Males","Females","Name"))
groupedby<-names[,list(cnt=sum(NumPeople)),by=Name]
groupedby <-as.data.table(groupedby)
groupedby<-groupedby[order(cnt,decreasing = TRUE)]
groupedby$log<-log(groupedby$cnt)
wordcloud(words=groupedby$Name,freq=groupedby$cnt,scale = c(2.5,.25),min.freq=2, max.words=150,random.order = FALSE,rot.per = .20,colors =brewer.pal(8,"Dark2") )
name.analysis<-names[names$Name=="veena" & names$Year >1960,]
name.analysis.byMostInAYear<-name.analysis[order(NumPeople,decreasing = TRUE)]
name.analysis.byMostInAYear<-name.analysis.byMostInAYear[name.analysis.byMostInAYear$NumPeople>5]
head(name.analysis.byMostInAYear,40)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data = name.analysis.byMostInAYear,aes(x=name.analysis.byMostInAYear$Year,y=name.analysis.byMostInAYear$NumPeople))+
  geom_bar(stat="identity",fill="red")+
  scale_x_continuous(breaks=seq(1930,2010,2))+
  scale_fill_manual(values = cbPalette)

ggplot(data = name.analysis.byMostInAYear,aes(x=factor(name.analysis.byMostInAYear$Year),y=name.analysis.byMostInAYear$NumPeople))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = cbPalette)

