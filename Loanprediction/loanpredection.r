setwd("~/machine learning practice/logisticRegression/Loanprediction")
#load library
library(data.table)
library(dplyr)
library(DMwR)
library(caTools)
library(caret)
library(base)
library(MASS)

#load datasets
train <- fread("train.csv",na.strings = c(""," ",NA,"NA","N/A","null"))
test <-fread("test.csv",na.strings = c(""," ",NA,"NA","N/A","null"))

Loan_ID <- test$Loan_ID

alldata <- bind_rows(train,test)
str(alldata)
alldata$Loan_ID <- NULL
colSums(is.na(train))
colSums(is.na(alldata))

#replace na 
alldata<-centralImputation(alldata)

str(alldata)

alldata$Gender <- as.factor(alldata$Gender)
alldata$Married <- as.factor(alldata$Married)
alldata$Dependents <-as.factor(alldata$Dependents)
alldata$Education <-as.factor(alldata$Education)
alldata$Self_Employed<- as.factor(alldata$Self_Employed)
alldata$Property_Area <- as.factor(alldata$Property_Area)
alldata$Loan_Status <- as.factor(alldata$Loan_Status)
str(alldata)


#split the alldata into train and test data
train <- alldata[1:614,]
test<- alldata[615:981,]


#split the train data into training and validation
split <- sample.split(train$Loan_Status,SplitRatio = 0.8)
train_data <- subset(train,split==T)
val_data <- subset(train, split==F)
str(train_data)
#try with first model
logl<- glm(Loan_Status~.,data = train_data, family = binomial)
summary(logl)


pred<- predict(logl,type = "response")
pred_class <- ifelse(pred >0.5,'Y','N')
table(pred_class,train_data$Loan_Status)

#validate the model
predv<- predict(logl,newdata = val_data,type = "response")

predv_class<- ifelse(predv>0.5,'Y','N')
table(predv_class,val_data$Loan_Status)
confusionMatrix(predv_class,val_data$Loan_Status)
#apply stepAIC
logl_update <- stepAIC(logl,train_data,direction="both")

#validate after stepAIC
predvu<- predict(logl_update,newdata = val_data,type = "response")

predv_classu<- ifelse(predvu>0.5,'Y','N')
table(predv_classu,val_data$Loan_Status)
confusionMatrix(predv_classu,val_data$Loan_Status)


#predict on the test datas
predt <- predict(logl_update,newdata = test,type = "response")
Loan_Status <- ifelse(predt>0.5,'Y','N')
submission <- data.frame(Loan_ID,Loan_Status)

write.csv(submission,"Submission.csv",row.names = FALSE)
