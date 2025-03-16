install.packages('caTools') #for train and test data split
install.packages('dplyr') #for Data Manipulation
install.packages('ggplot2') #for Data Visualization
install.packages('class') #KNN 
install.packages('caret') #Confusion Matrix
install.packages('corrplot') #Correlation Plot

library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)

#import the dataset
data<-read.csv("online_shoppers_intention.csv")
head(data)
str(data)
summary(data)
dim(data)

#Exchanging character to Integer
data$Month<-as.factor(data$Month)
data$Month<-as.integer(data$Month)
table(data$Month)

data$VisitorType<-as.factor(data$VisitorType)
data$VisitorType<-as.integer(data$VisitorType)
table(data$VisitorType)

head(data)

#checking for any not available in the dataset
anyNA(data)

#Extracting the labels
table(data$Revenue)
round(prop.table(table(data$Revenue))*100,digits = 1)

#Removing outliers
standard.features <- scale(data[,1:17])
standard.features

#Renaming our dataset after removing outliers
data1 <- cbind(standard.features,data[18])
head(data1)
anyNA(data1)

#creating the corplot 
corrplot(cor(data1[,-18]))

#splitting data
set.seed(101)

#Training the data
sample <- sample.split(data1$Revenue,SplitRatio = 0.70)
train <- subset(data1,sample==TRUE)
dim(train)

#Testing the data
test <- subset(data1,sample==FALSE)
dim(test)

#using the KNN model 
predicted.type <- knn(train[,1:17], test[,1:17], train$Revenue,k=1)
predicted.type

#Error in prediction
error <- mean(predicted.type!=test$Revenue)
error

#Confusion Matrix
confusionMatrix(predicted.type,as.factor(test$Revenue),mode="everything")

#Testing alternative k values
predicted.type <- NULL
error.rate <- NULL
for (i in 1:10) {
  predicted.type <- knn(train[1:17],test[1:17],train$Revenue,k=i)
  error.rate[i] <- mean(predicted.type!=test$Revenue)
}
knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))
knn.error

#creating a ggplot for k values
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')

#improving the model performance
data_test_prediction<-knn(train[,-1],test[,-1],train$Revenue,k=7)
data_test_prediction

#checking the error value for k=7
error<-mean(data_test_prediction!=test$Revenue)
error

#Rechecking the accuracy
library(caret)
confusionMatrix(data_test_prediction,as.factor(test$Revenue),mode="everything")
