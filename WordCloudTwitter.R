#twitter-log ArvindKejriwal > outputfile.txt

library(wordcloud)
library(SnowballC)
library(tm)
library(stringr)
library(RColorBrewer)
library(stringi)

inp <- readLines("./ladygar.txt")
#inp
length(inp)

cleanText <- function (x){
	x=tolower(x)
	x=grep("^    ",x,value=TRUE)
	x=gsub("    ","",x)
	x=gsub("rt @","",x)
	#-- Remove user references @<username>
	x=gsub(" @\\w+","",x)

	x=gsub("[[:punct:]]","",x)
	#x=gsub("[^[:graph:]]","",x)
	x=gsub("[[:cntrl:]]","",x)
	x=gsub("http\\w+","",x)

	return(x)
}

inp.cleaned <- cleanText(inp)
inp.cleaned<-iconv(inp.cleaned,"utf-8","ASCII",sub="")


actInp <- Corpus(VectorSource(inp.cleaned))
actInp <- tm_map(actInp, content_transformer(tolower),lazy=TRUE)
actInp <- tm_map(actInp, removePunctuation,lazy=TRUE)
actInp <- tm_map(actInp,removeWords,stopwords("english"),lazy=TRUE)
actInp <- tm_map(actInp,removeWords, c('the', 'this','will','just','https','http','htt','httpst','lot','one','set','shri','city','you','with','can','may','devfadnavis','cmomaharashtra'))
actInp <- tm_map(actInp,removeWords, c('amp','also','way','govt','why','order'))
actInp <- tm_map(actInp,removeWords, c('like','says','now','dont','new','day'))


twTdm <- TermDocumentMatrix(actInp)
#
vmat <- as.matrix(twTdm)
vfreq <- data.frame(Words=rownames(vmat), Freq = rowSums(vmat), stringsAsFactors = FALSE)
vfreq_ord <- vfreq[order(vfreq$Freq, decreasing = TRUE), ]
View(vfreq_ord)
vFinWords <- vfreq_ord[1:75,]
View(vFinWords)
wordcloud(words=vFinWords$Words,freq=vFinWords$Freq,colors =brewer.pal(8,"Dark2") )
