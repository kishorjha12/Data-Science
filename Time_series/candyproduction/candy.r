#set working directory
setwd("~/machine learning practice/timeseries/candyproduction")

#load library
library(sqldf)
library(ggplot2)
library(forecast)
library(TTR)
#load data

candy_data<- read.csv("candy_production.csv") 

#take a look of candy dataset
head(candy_data)
str(candy_data)

#Assign more meaningful variable names
colnames(candy_data)<- c("Period","candy_production")

#convert into datetime
candy_data$Period = as.Date(candy_data$Period,format="%Y-%m-%d")
#If by mistake more than one production value of a month is taken
candy_data<- sqldf("select Period,min(candy_production) as candy_production from candy_data group by Period ")
str(candy_data)

#deal with missing value
#To find the minimum value
minDate <- min(as.Date(candy_data$Period,format="%Y-%m-%d"))
maxDate <- max(as.Date(candy_data$Period,format="%Y-%m-%d"))
seq <- data.frame("Period"=seq(minDate,maxDate,by="months"))

#both seq and candy_data have same number of rows so it doesn't have any missing value

#convert data into timeseries dataset
attach(candy_data)
str(candy_data)
candyts <- ts(candy_production,c(1972,1),c(2017,8),12)
str(candyts)

##Exploratory data analysis---------------
# Take a peek at the dataset
candyts

#check for missing value
sum(is.na(candyts))

#check the frequency of the time series data
frequency(candyts)

#check the cycle of the time series
cycle(candyts)

#Review the summary statistics
summary(candyts)

#plot the raw data using the base plot function
plot(candyts,xlab="Date", ylab= "Candy Production as (%) of 2012 Production",main= "Monthly US candy Production from 1972 to 2017")

#let's try boxplot
boxplot(candyts~cycle(candyts),xlab="Date", ylab = "Candy Production as (%) of 2012 Production" ,main ="Monthly US Candy Production from 1972 to 2017")


##----------Time series Decomposition----------------
# The multiplicative model is: Y[t]=T[t] x S[t] x e[t]
decompose_candyts <- decompose(candyts,"multiplicative")
plot(decompose_candyts)

##Test stationarity of the time series----------
# null hypothesis: not Stationary , alternative hypothesis : stationary
adf.test(candyts)
# p value is <0.05 so we reject null Hypothesis , our timeseries data is stationary


#try with acf and pacf
par(mfrow=c(1,2))
acf(candyts,lag=30)
pacf(candyts,lag=30)

##Model building - Moving Average Method(SMA,WMA and EMA)---------

fitsma<-SMA(candyts,n=2)
str(fitsma)
length(fitsma)
length(candyts)
#Let us see how this model performs. You could choose any of the error metrics. Here we used MAPE to compute the error.
smaMape <-MAPE(fitsma[2:length(candyts)],candy_data$candy_production)
smaMape

#Now fit weighted moving average and compute the error metrics.
fitwma <- WMA(candyts,n=2,wts=1:2)
wmaMape <-MAPE(fitwma[2:length(candyts)],candy_data$candy_production)
wmaMape
#WmaMape<-mean(abs((candyts[2:length(candyts)]-fitwma[2:length(candyts)])/candyts[2:length(candyts)]))
#WmaMape

#Build exponential moving average model and evaluate it. Compare this value with value you
#obtained for WMA model.
fitEma <- EMA(candyts,n=2)
emaMape <-MAPE(fitEma[2:length(candyts)],candy_data$candy_production)
emaMape

# From these three model we are getting minimum error in WMA ........

##Let's check with ARIMA model-----------------
arima_candyts <- auto.arima(candyts)
forecast_candyts <- forecast(arima_candyts, level = c(95))
plot(arima_candyts)
forecast_candyts <- data.frame(forecast_candyts)

AmaMape <-MAPE(arima_candyts$fitted,candy_data$candy_production)
AmaMape

ggtsdiag(arima_candyts)

