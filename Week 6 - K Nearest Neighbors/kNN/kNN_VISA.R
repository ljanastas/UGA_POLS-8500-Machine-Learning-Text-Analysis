# Load the packages

library(pacman)
pacman::p_load(ElemStatLearn,foreign,class,caret)

#visafile = "~/Dropbox/Princeton-Classes-Spring-2018/Applied Machine Learning/WWS586A-Machine-Learning-Policy-Analysis/Data/us_perm_visas.csv"

visadata<-read.csv("https://www.ocf.berkeley.edu/~janastas/data/us_perm_visas.csv") 

visadata<-read.csv(visafile)

#visadata<-read.csv("https://www.ocf.berkeley.edu/~janastas/data/us_perm_visas.csv")


# What kind of visa applications do we have available here?
table(visadata$class_of_admission)

# Let's narrow down the data to only H1-B Visas


h1bvisadata<-visadata[visadata$class_of_admission=="H-1B"&(visadata$case_status=="Certified"|visadata$case_status=="Denied")
                        ,]


attach(h1bvisadata)


# Let's try to predict visa certification on the basis of application charachteristics
# First let's define our dataset
# Change the DVs to dummies

head(country_of_citzenship)

country_of_citizenship.dummies<-model.matrix(~country_of_citzenship)
employer_yr_estab<-as.numeric(employer_yr_estab)
employer_num_employees<-as.numeric(employer_num_employees)


h1bvisatrunc<-data.frame(factor(case_status),
                         country_of_citizenship.dummies,
                         employer_yr_estab,
                         employer_num_employees)

h1bvisatrunc<-h1bvisatrunc[complete.cases(h1bvisatrunc),]


h1bvisaY.factor<-
  ifelse(h1bvisatrunc[,1]=="Certified","Certified","Not Certified")


h1bvisatrunc<-data.frame(h1bvisaY.factor,
                         h1bvisatrunc$employer_num_employees,
                         h1bvisatrunc$employer_yr_estab)

#h1bvisaX<-h1bvisatrunc[,2:6]

# Divide into training and testing
# This is training with an 80/20 split

train=sample(1:dim(h1bvisatrunc)[1]) #

h1bvisatrunc.train<-h1bvisatrunc[train[1:1000],]

h1bvisatrunc.test<-h1bvisatrunc[train[1001:1500],]

#
trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

set.seed(3333)

knn_fit <- train(h1bvisaY.factor ~., data = h1bvisatrunc.train, 
                 method = "knn",
                 trControl=trctrl)

# Now predict on the test set 
test_pred <- predict(knn_fit, newdata = h1bvisatrunc.test)

confusionMatrix(test_pred, h1bvisatrunc.test$h1bvisaY.factor )

#############################################
# With feature engnineering###########################
######################################################
######################################################

h1bvisatrunc<-data.frame(h1bvisaY.factor,
                         h1bvisatrunc$employer_num_employees,
                         h1bvisatrunc$employer_yr_estab)

train=sample(1:dim(h1bvisatrunc)[1]) #

h1bvisatrunc.train<-h1bvisatrunc[train[1:1000],]

h1bvisatrunc.test<-h1bvisatrunc[train[1001:1500],]

#
trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

set.seed(3333)

knn_fit <- train(h1bvisaY.factor ~., data = h1bvisatrunc.train, 
                 method = "knn",
                 trControl=trctrl)

# Now predict on the test set 
test_pred <- predict(knn_fit, newdata = h1bvisatrunc.test)

confusionMatrix(test_pred, h1bvisatrunc.test$h1bvisaY.factor )




