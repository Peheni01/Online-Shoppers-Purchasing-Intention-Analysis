data<-read.csv("online_shoppers_intention.csv")

head(data)
str(data)
summary(data)

dim(data)

install.packages("cluster")
library(cluster)

data$Month<-as.factor(data$Month)
data$Month<-as.integer(data$Month)
table(data$Month)

data$VisitorType<-as.factor(data$VisitorType)
data$VisitorType<-as.integer(data$VisitorType)
table(data$VisitorType)

head(data)
#checking for any not available in the dataset
anyNA(data)

pairs(data)
plot(BounceRates~ExitRates,data=data)
with(data,text(BounceRates~ExitRates,labels=Month))

normalise<-function(df)
{
  return(((df-min(df))/(max(df))*(1-0))+0)
}  
head(data)

#Normalise the dataset using above normalise function
data_n<-data[,1:17]
data_n
data_n<-as.data.frame(lapply(data,normalise))
data_n
