rm(list = ls()) # empty the memory

# install.packages("mlbench") # this line should be run once
library(mlbench)

# now let us fetch the dataset from mlbench
data(PimaIndiansDiabetes)
dataset<- PimaIndiansDiabetes # this dataset has dimensions of 768 record and 9 column

# install.packages("randomForest") # this line should be run once
# install.packages("JOUSBoost")    # this line should be run once
# install.packages("caret")        # this line should be run once

# import all the necessary libraries
library(randomForest)
library(JOUSBoost)
library(caret)

# install.packages("e1071") # this line should be run once
library(e1071)

k<- 10
folds<-createFolds(1:nrow(dataset),k,list=F)

# accuracy arrays
acc_rf<-array()
bac_rf<-array()
f1_rf<-array()

acc_ada<-array()
bac_ada<-array()
f1_ada<-array()

acc_svm<-array()
bac_svm<-array()
f1_svm<-array()

# RandomForest
for(i in 1:k) {
  ind<-which(folds==i)
  train<-dataset[-ind,]
  test<-dataset[ind,]
  #training
  yy<-as.character(train[,ncol(train)])
  yy[yy=="pos"]<-+1
  yy[yy=="neg"]<--1
  y<-as.factor(yy)
  x<-train[,-ncol(train)]
  fit<- randomForest(x,y)
  
  #testing
  yy_test<-as.character(test[,ncol(test)])
  yy_test[yy_test =="pos"]<-+1
  yy_test[yy_test =="neg"]<--1
  y_test<-as.factor(yy_test)
  x_test<-test[,-ncol(test)]
  
  acc_rf[i]<-sum(y_test==predict(fit,x_test))/length(y_test)
  pred<-predict(fit,x_test)
  rf_table<-table(y_test,pred)
  tn<-rf_table[1,1]
  tp<-rf_table[2,2]
  fp<-rf_table[1,2]
  fn<-rf_table[2,1]
  
  bac_rf[i]<-0.5*((tp/(tp+fn))+(tn/(tn+fp)))
  f1_rf[i]<-(2*tp)/(2*tp+fp+fn)
}

# AdaBoost
for(i in 1:k){
  ind<-which(folds==i)
  train<-dataset[-ind,]
  test<-dataset[ind,]
  #training
  yy<-as.character(train[,ncol(train)])
  yy[yy=="pos"]<-+1
  yy[yy=="neg"]<--1
  y<-as.numeric(yy)
  x<-as.matrix(train[,-ncol(train)])
  class(x)
  
  fit<-adaboost(x,y)
  
  #testing
  yy_ada_test<-as.character(test[,ncol(test)])
  yy_ada_test[yy_ada_test =="pos"]<-+1
  yy_ada_test[yy_ada_test =="neg"]<--1
  y_ada_test<-as.numeric(yy_ada_test)
  x_ada_test<-as.matrix(test[,-ncol(test)])
  class(x_ada_test)
  
  acc_ada[i]<-sum(y_ada_test==predict(fit,x_ada_test))/length(y_ada_test)
  pred<-predict(fit,x_ada_test)
  ada_table<-table(y_ada_test,pred)
  
  tn<-ada_table[1,1]
  tp<-ada_table[2,2]
  fp<-ada_table[1,2]
  fn<-ada_table[2,1]
  
  bac_ada[i]<-0.5*((tp/(tp+fn))+(tn/(tn+fp)))
  f1_ada[i]<-(2*tp)/(2*tp+fp+fn)
}

# SVM
for(i in 1:k) {
  ind<-which(folds==i)
  train<-dataset[-ind,]
  test<-dataset[ind,]
  
  #training
  yy<-as.character(train[,ncol(train)]) # this is the classifier value for the training set
  yy[yy=="pos"]<-+1
  yy[yy=="neg"]<--1
  y<-as.factor(yy)
  x<-train[,-ncol(train)]
  
  fit<- svm(x,y)
  
  #testing
  yy_test<-as.character(test[,ncol(test)])
  yy_test[yy_test =="pos"]<-+1
  yy_test[yy_test =="neg"]<--1
  y_test<-as.factor(yy_test)
  x_test<-test[,-ncol(test)]
  
  acc_svm[i]<-sum(y_test==predict(fit,x_test))/length(y_test)
  
  pred<-predict(fit,x_test)
  svm_table<-table(y_test, pred)
  
  tn<-svm_table[1,1]
  tp<-svm_table[2,2]
  fp<-svm_table[1,2]
  fn<-svm_table[2,1]
  
  bac_svm[i]<-0.5*((tp/(tp+fn))+(tn/(tn+fp)))
  f1_svm[i]<-(2*tp)/(2*tp+fp+fn)
}


# Reporting performance result

cat("RandomForest test performance Report\n")
cat("----------------------------------\n")

cat("MACC_test\n")
cat(mean(acc_rf))

cat("\nSD_test\n")
cat(sd(acc_rf))

cat("\n\nMBAC_test\n")
cat(mean(bac_rf))

cat("\nSD_test\n")
cat(sd(bac_rf))

cat("\n\nMF1_test\n")
cat(mean(f1_rf))

cat("\nSD_test\n")
cat(sd(f1_rf))

cat("\n\nAdaBoost test performance Report\n")
cat("----------------------------------\n")

cat("MACC_test\n")
cat(mean(acc_ada))

cat("\nSD_test\n")
cat(sd(acc_ada))

cat("\n\nMBAC_test\n")
cat(mean(bac_ada))

cat("\nSD_test\n")
cat(sd(bac_ada))

cat("\n\nMF1_test\n")
cat(mean(f1_ada))

cat("\nSD_test\n")
cat(sd(f1_ada))

cat("\n\nSVM test performance Report\n")
cat("----------------------------------\n")

cat("MACC_test\n")
cat(mean(acc_svm))

cat("\nSD_test\n")
cat(sd(acc_svm))

cat("\n\nMBAC_test\n")
cat(mean(bac_svm))

cat("\nSD_test\n")
cat(sd(bac_svm))

cat("\n\nMF1_test\n")
cat(mean(f1_svm))

cat("\nSD_test\n")
cat(sd(f1_svm))

cat("\n\n----------------------------------------------------------------\n")
print("Mohammad Y Alghafli")
print("ID:1741679")
print(Sys.time())
print(Sys.Date())
