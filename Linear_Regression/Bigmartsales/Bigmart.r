#set working directory
setwd("~/machine learning practice/multilinearRegression/Bigmartsales")
#Required library
library(caTools)
library(DMwR)
library(MASS)
#load datasets
train <- read.csv("Train.csv")
test <- read.csv("Test.csv")



str(train)
str(test)
summary(train)

#mistake in train data
train$Item_Fat_Content[train$Item_Fat_Content=="LF" | train$Item_Fat_Content=="low fat"]<- "Low Fat"
train$Item_Fat_Content[train$Item_Fat_Content=="reg"]<- "Regular"
test$Item_Fat_Content[test$Item_Fat_Content=="LF" | test$Item_Fat_Content=="low fat"]<- "Low Fat"
test$Item_Fat_Content[test$Item_Fat_Content=="reg"]<- "Regular"
#Item_Visibility 0 is not possible
train$Item_Visibility[train$Item_Visibility==0]<-mean(train$Item_Visibility)
test$Item_Visibility[test$Item_Visibility==0]<-mean(test$Item_Visibility)

#How old Outlet_Establishment is
train$Outlet_Establishment_Year<- 2013-train$Outlet_Establishment_Year
test$Outlet_Establishment_Year <- 2013-test$Outlet_Establishment_Year

#checking for null value
colSums(is.na(train))
colSums(is.na(test))

#remove null value by using kNN
train <- knnImputation(train, k =6, meth = "weighAvg")
test <- knnImputation(test, k =6, meth = "weighAvg")

summary(train)
#split dataset for first model try
split <- sample.split(train$Item_Outlet_Sales, SplitRatio= 0.7)
train_data <- subset(train,split==TRUE)
val_data <- subset(train,split==FALSE)


#train the model
lm0 <- lm(Item_Outlet_Sales~.,data=train)

summary(lm0)

pred0 <- predict(lm0,newdata = val_data)

error<-regr.eval(val_data$Item_Outlet_Sales,pred0)
error

#using stepAic
stepOut<-stepAIC(lm0, direction="both")

summary(stepOut)

#predict the test data
predtest <- predict(stepOut,newdata = test)

outputMod <- predtest
outputMod[predtest<0]<- 0
Item_Identifier<-test$Item_Identifier
Outlet_Identifier <- test$Outlet_Identifier
Item_Outlet_Sales<- outputMod
output <- data.frame(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)

write.csv(output,'SampleSubmission.csv', row.names = F)
