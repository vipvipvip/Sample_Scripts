args <- commandArgs(trailingOnly = TRUE)
library(twitteR)
howmany <- 200 #how many past tweets to collect

user <- '@beckyquickcnbc'
outputfile <- paste('c:\\',user,'.txt',sep="")
print(user)
print(outputfile)

tmptimeline <- userTimeline(user,n=as.character(howmany))
tmptimeline.df <- twListToDF(tmptimeline)
tmptimeline.df$text <- gsub("\\n|\\r|\\t", " ", tmptimeline.df$text)
write.table(tmptimeline.df,file=outputfile,append=TRUE,sep="\t",col.names=FALSE)

quit()