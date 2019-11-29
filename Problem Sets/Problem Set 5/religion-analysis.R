library(readtext)
library(pacman)

# This loads and installs the packages you need at once
pacman::p_load(tm,SnowballC,foreign,plyr,
               twitteR,slam,foreign,
               caret,ranger,rpart,rpart.plot, xgboost)

religion_dirs = "/Users/jason/Downloads/Kaggle Project Metron1"

files = list.files(religion_dirs)

setwd(religion_dirs)

# Import the texts
religious_texts = c()

for(text in files){
  religious_texts = c(
    religious_texts,readtext(text)
    )
}


text_cleaner<-function(corpus){
  tempcorpus = lapply(corpus,toString)
  for(i in 1:length(tempcorpus)){
    tempcorpus[[i]]<-iconv(tempcorpus[[i]], "ASCII", "UTF-8", sub="")
  }
  tempcorpus = lapply(tempcorpus, tolower)
  tempcorpus<-Corpus(VectorSource(tempcorpus))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
  
  #Removing all the special charecters and words
  tempcorpus <- tm_map(tempcorpus, toSpace, "@")
  tempcorpus <- tm_map(tempcorpus, toSpace, "\\|")
  tempcorpus <- tm_map(tempcorpus, toSpace, "#")
  tempcorpus <- tm_map(tempcorpus, toSpace, "http")
  tempcorpus <- tm_map(tempcorpus, toSpace, "https")
  tempcorpus <- tm_map(tempcorpus, toSpace, ".com")
  tempcorpus <- tm_map(tempcorpus, toSpace, "$")
  tempcorpus <- tm_map(tempcorpus, toSpace, "+")
  tempcorpus <- tm_map(tempcorpus, removeNumbers)
  # Remove english common stopwords
  tempcorpus <- tm_map(tempcorpus, removeWords, stopwords("english"))
  # Remove punctuation
  tempcorpus <- tm_map(tempcorpus, removePunctuation)
  
  # Eliminate extra white spaces
  tempcorpus <- tm_map(tempcorpus, stripWhitespace)
  # Stem the document
  #tempcorpus <- tm_map(tempcorpus, PlainTextDocument)
  tempcorpus <- tm_map(tempcorpus,  stemDocument, "english")
  # Remove uninformative high-frequency words
  tempcorpus <- tm_map(tempcorpus, toSpace, "amp")
  return(tempcorpus)
}

cleantweets<-text_cleaner(religious_texts)


# Create TF-IDF
dtm<-DocumentTermMatrix(cleantweets)
dtm<-removeSparseTerms(dtm, 0.90)


dtmtopic <- dtm[rowSums(as.matrix(dtm))> 0, ]

dtm_topic<-as.matrix(dtmtopic)


set.seed(100)
ap_lda11 <- LDA(dtmtopic, k = 5, method="Gibbs")

# Let's see what our topics are comprised of

terms(ap_lda11, k=10)



