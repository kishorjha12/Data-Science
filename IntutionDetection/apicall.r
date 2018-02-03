#prediction for newdatasets
setwd("~/Documents/dataScience/myDSproject/intution")
#load the datasets
ndata <- read.csv("test_final.csv")

#load the model
lmod1 <- readRDS("lmod1.Rdata")

Targ<- ndata$Target
ndata<- ndata[,-28]

source("processing.r")
  
new_data <- processT(ndata)

  
#predict the value
predt <- predict(lmod1, newdata= new_data, type= "response")
predt_class <- ifelse(predt>0.5, 1, 0)

library(caret)
confusionMatrix(predt_class,Targ)  

  

