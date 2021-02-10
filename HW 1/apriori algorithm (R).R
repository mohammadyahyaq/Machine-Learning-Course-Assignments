# first, we fetch the basic configurations required for the development environment
rm(list = ls())
# install.packages("arules") # this line of code install the package in case that you don't have it
library(arules)

# fetch the dataset
setwd("C:/Users/m7md2/Anaconda3 Projects/Association Analysis")
# print(file.exists("sample_dataset.csv")) # this will check if the file exist or not
data = read.csv("sample_dataset.csv", header = TRUE)

# reform the dataset
data[data==1]<-"TRUE"
data[data==0]<-"FALSE"

freq<- apriori(data,parameter = list(support=0.5, target="frequent itemsets"))
# freq<- apriori(data,parameter = list(support=0.5, confidence=0.75))

s1<-inspect(freq[size(freq) == 4])

print("Mohammad Y Alghafli")
print("ID:1741679")
print(Sys.time())
print(Sys.Date())