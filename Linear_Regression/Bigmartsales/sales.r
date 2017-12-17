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

#checking for null value
colSums(is.na(train))
colSums(is.na(test))

#remove null value by using kNN
train <- knnImputation(train, k =6, meth = "weighAvg")
test <- knnImputation(test, k =6, meth = "weighAvg")

str(train)
str(test)
#split the dataset 
#split dataset for first model try
split <- sample.split(train$Item_Outlet_Sales, SplitRatio= 0.7)
train_data <- subset(train,split==TRUE)
val_data <- subset(train,split==FALSE)
str(val_data)
str(train_data)
#first model
lm0 <- lm(Item_Outlet_Sales~.,data=train)
summary(lm0)
pred0 <- predict(lm0, newdata = val_data)
error0 <- regr.eval(val_data$Item_Outlet_Sales,pred0)
error0
head(val_data$Item_Outlet_Sales)
head(pred0)
ttt<- data.frame(val_data$Item_Outlet_Sales,pred0)

#using stepAic
stepOut<-stepAIC(lm0, direction="both")

summary(stepOut)

#Lets check the residuals 
par(mfrow=c(2,2))
plot(stepOut)

#We don't want negative values as forecast for bike count!


pred1 <-predict(Output2Mod,newdata = val_data)
error1 <- regr.eval(val_data$Item_Outlet_Sales, pred1)
error1

pp<- data.frame(val_data$Item_Outlet_Sales,pred0,pred1)


#predict the test data
predtest <- predict(stepOut,newdata = test)

outputMod <- predtest
outputMod[predtest<0]<- 0
Item_Identifier<-test$Item_Identifier
Outlet_Identifier <- test$Outlet_Identifier
Item_Outlet_Sales<- outputMod
output <- data.frame(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)

write.csv(output,'SampleSubmission.csv', row.names = F)
