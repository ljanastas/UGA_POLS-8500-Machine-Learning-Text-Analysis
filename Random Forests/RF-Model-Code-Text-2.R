library(pacman)

# This loads and installs the packages you need at once
pacman::p_load(tm,SnowballC,foreign,plyr,
               twitteR,slam,foreign,wordcloud,LiblineaR,e1071,
               caret,rpart, quanteda, rpart.plot)


trumptweets <- read.csv("https://www.ocf.berkeley.edu/~janastas/trump-tweet-data.csv")


# Transform the tweets only into a corpus object

trumptweets.text = lapply(trumptweets$Text, function(t) toString(t)) # Converts each tweet to a string within a list. 
trumptweets.text = unlist(trumptweets.text) # Unlist the object, tranform it to a vector 
# Now we needs 

trumptweets.corpus = corpus(trumptweets.text) # Constructing a corpus

trumptweets.tokens  = tokens(trumptweets.corpus, # This code cleans the text for us and parses the tweets into 1,2 and 3 grams
                             remove_numbers = TRUE,  
                             remove_punct = TRUE,
                             remove_symbols = TRUE,
                             ngrams = 1:2,
                             remove_twitter= TRUE,
                             remove_url = TRUE)

trumptweets.dfm <- dfm(trumptweets.tokens,  # Constructs a DFM, removes stopwords, removes punctuation, stems words
                       remove = stopwords("english"), 
                       stem = TRUE)

# Remove the tokens object
rm(trumptweets.tokens)

trumptweets.dfm.trimmed.sparsity = dfm_trim(trumptweets.dfm,  # Trim using sparsity reduction
                                            sparsity = 0.99)

# Check the summary start for the number of retweets for each tweet in the database
summary(trumptweets$Retweets)

dfm_mat_old<-as.matrix(trumptweets.dfm.trimmed.sparsity) # The matrix (DFM) trimmed using sparsity reduction and term frequency

dfm_mat = ifelse(dfm_mat_old>0,1,0)

#dfm_mat_termfreq <- as.matrix(trumptweets.dfm.trimmed.termfreq) # The matrix (DFM) trimmed using term frequencies. 

viraltweets<-ifelse(trumptweets$Retweets > 1256, 1,0)

#viral_indices <- which(viraltweets == 1)


############ Training and testing ########################################

# Divide into train and test with a 70/30 split
set.seed(33)

train=sample(1:dim(dfm_mat)[1],   # Creating an index with each of the observation numbers and randomly shuffling them using a 70/30 split
             dim(dfm_mat)[1]*0.7)
trainX = dfm_mat[train,] # Defines training data
testX = dfm_mat[-train,] # Defines test data
trainY = viraltweets[train]
testY = viraltweets[-train]

traindata<-data.frame(trainY,trainX)
testdata<-data.frame(testY,testX)

library(ranger)

############ Random Forest with Ranger ########################################

rf_fit<-ranger(factor(trainY)~., data=traindata, 
                                 importance='impurity',
                                 write.forest=TRUE,
                                 probability=TRUE)


################################################################################################
################################################################################################
####### Draw the trees #########################################################################
################################################################################################
################################################################################################
################################################################################################

trees=rpart(factor(trainY)~., traindata)
rpart.plot(trees)


################################################################################################
################################################################################################
####### Performance######## ####################################################################
################################################################################################
################################################################################################
################################################################################################


# With ranger we have to generate the predicted probabilities and classify the tweets ourselves
rf_probs<-predict(rf_fit,data.frame(testdata))

rf_class<-ifelse(rf_probs$predictions[,2] > 0.5, 1,0)

predicted_class = factor(rf_class)
true_class = factor(testdata$testY)

cmat = confusionMatrix(predicted_class,true_class, positive = "1")
cmat


################################################################################################
################################################################################################
####### Variable Importance ####################################################################
################################################################################################
################################################################################################
################################################################################################

# Let's extract the variable importance

varimp = rf_fit$variable.importance

# We can create a variable importance plot
# but it's a bit tricky

# Extract the words and their importance scores
words<-names(varimp)
importance<-as.vector(varimp)

# Create a data frame with both
importance.data = data.frame(words,importance)

# Now we need to reorder the data frame in descending order
# and only choose the top few words, let's say 20

importance.data = importance.data[order(-importance.data$importance),]
importance.data = importance.data[1:20,]

# Now we can use ggplot2 to create the plot
# Plot variable importance 
ggplot(importance.data, 
       aes(x=reorder(words,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Word Importance Plot")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
















