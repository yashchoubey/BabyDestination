###for reading file

library(readtext)
conn = file("C:/Users/baby destination/Documents/R/win-library/3.4/WhatsApp Chat with BD Moms-To-Be(Pregnant).txt", 'r')
linn <-readLines(conn)
linesData <- ""
for (i in 1:length(linn)){
  data <- linn[i]
  dataProcessed <- gsub('(.* - .*: )(.*)', '\\1', data)
  if(dataProcessed == data) {
    dataProcessed <- ""
  } else {
    dataProcessed <- gsub('(.* - .*: )(.*)', '\\2', data)
  }
  linesData <- paste(linesData, dataProcessed)
  #process data to remove sender info and time info
}
close(conn)


###For stopwords
#install.packages("tm")
library(tm)
library(NLP)

###Cleaning the data
CleanData <- linesData

CleanData <- tolower(CleanData) #Turn the data into lower case 
CleanData <- removeNumbers(CleanData) 

mystopwords <- c("pm","am", "<", ">", stopwords("en")) 
CleanData <- removeWords(CleanData, mystopwords) #removing stop words



##Stemmer
#install.packages("SnowballC")
library(SnowballC)
CleanData <- wordStem(CleanData, language = "en")

###get frequently used words
#install.packages(c("qdap", "qdapTools", "rJava"))
library(qdapTools)

library(rJava)
library(qdap)

TextFrequency <- freq_terms(CleanData, at.least = 1) 

library(wordcloud) 
wordcloud(TextFrequency$WORD, TextFrequency$FREQ, colors = TextFrequency$FREQ, max.words = 200)

###
#install.packages("syuzhet")
library(syuzhet)
Sentiments <- get_nrc_sentiment(TextFrequency$WORD)
Sentiments <- cbind("Words" = TextFrequency$WORD, Sentiments)
SentimentsScore <- data.frame("Score" = colSums(Sentiments[2:11])) 
TotalSentiments <- cbind("Sentiments" = rownames(SentimentsScore), SentimentsScore) 
rownames(TotalSentiments) <- NULL 

library(ggplot2)
ggplot(data = TotalSentiments, aes(x = Sentiments, y = Score)) + geom_bar(stat = "identity", aes(fill = Sentiments))

