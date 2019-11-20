## Topic model demonstration code for POLS 8500
## Author: L. Jason Anastasopoulos (ljanastas@uga.edu)

library(pacman)

# This loads and installs the packages you need at once
pacman::p_load(tm,SnowballC,foreign,plyr,twitteR,slam,foreign,wordcloud,LiblineaR,e1071, topicmodels)

# Use the Associated Press corpus, the "AssociatedPress" object is a document-term matrix. 
data("AssociatedPress")
AssociatedPress

# set a seed so that the output of the model is predictable.
# Estimate a k = 5 topic model

ap_lda <- LDA(AssociatedPress, k = 5, control = list(seed = 1234))
ap_lda


terms(ap_lda, k=10) # top 10 words for each topic

## Let's find the topic distributions for each document
posterior_inference <- posterior(ap_lda)
posterior_topic_dist<-posterior_inference$topics # This is the distribution of topics for each document
posterior_topic_dist[1:10,]

## Topic distribution for documents 1-3

par(mfrow=c(1,3))
hist(posterior_topic_dist[1,], col="blue",main="Document 1", xlab = "")
hist(posterior_topic_dist[2,], col="blue",main="Document 2", xlab = "")
hist(posterior_topic_dist[3,], col="blue",main="Document 3", xlab = "")


## Let's measure topic similarity between document 1 and documents 2 and 3
doc_similarity<-function(doc1,doc2){
  sim<-sum(
    (sqrt(doc1) + sqrt(doc2))^2
  )
  return(sim)
}

doc_similarity(posterior_topic_dist[1,],posterior_topic_dist[2,]) # Documents 1 and 2

doc_similarity(posterior_topic_dist[1,],posterior_topic_dist[3,]) # Documents 1 and 3