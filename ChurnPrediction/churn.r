#set working directory
setwd("~/Documents/dataScience/myDSproject/ChurnPrediction")

#load library
library(caTools)
library(VIM)
library("plyr")
library(corrplot)
library(partykit)
library(rpart)
#load datasets
churn <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",na.strings = c("","NA",NA,"N/A",NULL,"null"))
str(churn)

#Customer ID doesn't have that much importance so we remove it
customerID <- churn$customerID
churn <- churn[,-1]

summary(churn)

#check for Total charges it seems to be like little skewed right Think for standrazition
hist(churn$TotalCharges, n=10000)

#check for null value
sapply(churn, function(x) sum(is.na(x)))

#Remove null value by using VIM package 
churn_data <- kNN(churn, variable = c("TotalCharges"), k=6) 
churn <- subset(churn_data,select = gender:Churn)

#remove churn_data
rm(churn_data)
str(churn)


#"No internet service" and "No" value is similar
cols_recode1 <- c(7:13)
#mapvalues plyr library 
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                    (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}


#"No phone service" to "No" for multipleLines
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines,from = c("No phone service"), to=c("No")))


#some exploration with tenure variable.
min(churn$tenure); max(churn$tenure)
# 0 to 72 months lets divide it into 5 parts or period of 12
group_tenure <- function(tenure){
  if(tenure >=0 & tenure <= 12){
    return('0-12 Months')
  }else if(tenure >12 & tenure <= 24){
    return("12-24 Month")
  }else if(tenure >24 & tenure <= 48){
    return("24-48 Month")
  }else if(tenure >48 & tenure <= 60){
    return("48-60 Month")
  }else if(tenure > 60){
    return("> 60 Month")
  }
}

churn$tenure_group <- sapply(churn$tenure, group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)
churn$tenure <- NULL
str(churn)

churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,from=c("0","1"),to=c("No", "Yes")))

#Exploratory data analysis and feature selection

#Correlation between numeric variables
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation plot for Numerical variables", method="number")

#There is 65% correlation between TotalCharges and MonthlyCharges
churn$TotalCharges <- NULL

#split the dataset into train and test set
set.seed(6)
split <- sample.split(churn$Churn,SplitRatio = 0.7) 
training <- subset(churn,split==T)
testing <- subset(churn,split==F)

#confirm the splitting is correct
dim(training); dim(testing)
str(training)

#Fitting the Logistic Regression Model
logMod <- glm(Churn ~., family = binomial(link = "logit"),data = training)
summary(logMod)

#The top three most-relevant features include Contract, tenure_group and PaperlessBilling.
anova(logMod, test = "Chisq")

#Assessing the predictive ability of the Logistic Regression model
#testing$Churn <- as.character(testing$Churn)
#testing$Churn<- ifelse(testing$Churn == "No","0","1")

predict.log <- predict(logMod, newdata = testing, type = "response")

predict.log <- ifelse(predict.log >0.5, "Yes", "No")
missclassificError <- mean(predict.log != testing$Churn)
print(paste('Logistic Regression Accuracy',1-missclassificError))

#"Logistic Regression Accuracy 0.805016564126834"

#Logistic Regression Confusion Matrix
print("Confusion Matrix for Logistic Regression")
table(testing$Churn, predict.log)

#library caret
library(caret)
confusionMatrix(testing$Churn, predict.log)

#Odd Ratio
library(MASS)
exp(cbind(OR=coef(logMod), confint(logMod)))

#Decision Tree visualization
tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling,training)
plot(tree)


#Decision Tree Accuracy
pred_tree<- predict(tree, testing)
print("Confusion matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)

#decision tree Accuracy
p1 <- predict(tree, training)
tab1 <- table(Predicted=p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual= testing$Churn)
print(paste('Decision tree Acuracy', sum(diag(tab2))/sum(tab2)))

#pred_tree<- ifelse(pred_tree=="No","0","1")
confusionMatrix(testing$Churn, pred_tree)

#Decision tree by using rpart

mtree <- rpart(Churn~ .,data=training ,method = "class")
#plot(mtree, main="Classification Tree Class", margin=0.15, uniform=TRUE)
text(mtree,use.n = T)
summary(mtree)

#Prune Decision Tree
mtree <- rpart(Churn ~., data=training, method = "class",cp =0.001)
printcp(mtree)
plot(mtree,main="Classification Tree for Survived Class",margin=0.15,uniform=TRUE)

#locate the record with the minimum cross-validation errors:
rw <-which.min(mtree$cptable[,"xerror"])
rw
#get the cost complexity parameter of the record with the minimum cross-validation error
tree.cp <- mtree$cptable[rw,"CP"]
tree.cp

d_prune <- prune(mtree, cp=tree.cp)
d_predict <- predict(d_prune, testing, type="class")

plot(d_prune,main="Classification Tree for Survived Class",margin=0.15,uniform=TRUE)

#test the confusion matrix
confusionMatrix(testing$Churn,d_predict)
#rpart Decision Tree accuracy = 0.7984

#Finallly we observed that Logistic regression is giving us the maximum accuracy on test dataset
#Accuracy "Logistic Regression Accuracy 0.805016564126834"




