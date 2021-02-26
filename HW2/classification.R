rm(list = ls()) # empty the memory

# import all the necessary data and packages
# install.packages("mlbench") # this line should be run once
library(mlbench)
data(Ionosphere)
dataset<-Ionosphere

# now let us prepare the training set and the test set for the machine learning model
# print(dataset)
# first, we should take a sample for the training set and the test set
ind<-sample(1:nrow(dataset),size= nrow(dataset),replace=FALSE)
#assign the first 200 for training and remaining for testing
train<- dataset[ind[1:200],]
test<- dataset[ind[201:nrow(dataset)],]

# split between the classifier and the other column
y<-as.factor(train[,35])
x<-train[,1:34]

y_test<-as.factor(test[,35])
x_test<-test[,1:34]

# ----------------------------------------------------------------

# now we apply the algorithms for these samples

# Decision Tree Algorithm
# ----------------------------------------------------------------
cat("\n\nDecision Tree algorithm\n----------------------------------------------------------------\n")
# install.packages("rpart") # this line should be run once
library(rpart)

fit_dt<-rpart(y~.,x) # the reason we putted delta sign (~) and period sign (.) is to build a function
dt_acc<-sum(y_test==predict(fit_dt,x_test,type="class"))/length(y_test)
cat(dt_acc)

# Random Forest algorithm
# ----------------------------------------------------------------
cat("\n\nRandom Forest algorithm\n----------------------------------------------------------------\n")
# install.packages("randomForest") # this line should be run once
library(randomForest)

fit_rf<-randomForest(x,y)
rf_acc<-sum(y_test==predict(fit_rf,x_test,type="class"))/length(y_test)
cat(rf_acc)

# AdaBoost algorithm
# ----------------------------------------------------------------
cat("\n\nAdaBoost algorithm\n----------------------------------------------------------------\n")
# install.packages("JOUSBoost") # this line should be run once
library(JOUSBoost)

# in the original dataset there are two classes "good" and "bad" and this cause a problem in AdaBoost algorithm and we need to convert them to 1 and -1
# therefore, we should re-generate the train set and the test set

# first let's start with the train set
# we will convert the class column to +1 and -1 column
yy<-as.character(train[,35])
yy[yy=="good"]=+1
yy[yy=="bad"]= -1

y_ada<-as.numeric(yy) # this victor will contain the list of classes for the train set
x_ada<-as.matrix(train[,1:34]) # this matrix will contain the matrix of the train set record
class(x_ada)<-"numeric"

# now we should re-generate the test set
# we will convert the class column to +1 and -1 column
yyt<-as.character(test[,35])
yyt[yyt=="good"]=+1
yyt[yyt=="bad"]= -1

y_ada_test<-as.numeric(yyt) # this victor will contain the list of classes for the test set
x_ada_test<-as.matrix(test[,1:34]) # this matrix will contain the matrix of the test set record
class(x_ada_test)<-"numeric"

# now let us apply the algorithm for these values
fit_ada<-adaboost(x_ada,y_ada, n_rounds = 50)
ada_acc<-sum(y_ada_test==predict(fit_ada,x_ada_test))/length(y_ada_test)
cat(ada_acc)

# Bagging algorithm
# ----------------------------------------------------------------
cat("\n\nBagging algorithm\n----------------------------------------------------------------\n")
# install.packages("ipred") # this line should be run once
library(ipred)

fit_bag<-bagging(y~.,x)
bag_acc<-sum(y_test==predict(fit_bag,x_test,type="class"))/length(y_test)
cat(bag_acc)

# K Nearest-Neighbor algorithm
# ----------------------------------------------------------------
cat("\n\nK Nearest-Neighbor algorithm\n----------------------------------------------------------------\n")
# install.packages("class") # this line should be run once
library(class)

fit_knn<-knn(x,x_test,y) # this function will generate the expected classification for each record of x_test
knn_acc<-sum(y_test==fit_knn)/length(y_test)
cat(knn_acc)


cat("\n\n----------------------------------------------------------------\n")
print("Mohammad Y Alghafli")
print("ID:1741679")
print(Sys.time())
print(Sys.Date())



