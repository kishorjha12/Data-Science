setwd("~/Documents/dataScience/myDSproject/intution")
#load data
df <- read.csv("train_final.csv") 

df <-df[!(is.na(df$Target)),]

Target <- df$Target
df<-df[,-28]
#summary(Target)
#str(df)
#summary(df)

source("processing.r")
rd<-processT(df)
str(rd)
summary(rd)

#combine the independant and dependant variable for training
train <- cbind(rd,Target)

#Apply the first logistic model

lmod1 <- glm(Target ~ ., data = train, family = binomial)

pred <- predict(lmod1, type = "response")
pred_class <- ifelse(pred >0.5, 1, 0)
library(caret)
confusionMatrix(pred_class,train$Target)
summary(lmod1)


#save the model in Rdata form
saveRDS(lmod1,"lmod1.Rdata")
