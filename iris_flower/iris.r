setwd("~/machine learning practice/clustring")

#load libraby and dataset-------------
#library
library(vegan)
library(cluster)
#load data 
iris_data<-read.csv("Iris.csv") 
data<- iris_data
iris_data <- iris_data[,-6]


#Data Exploration and prepration-----------------
head(iris_data)
str(iris_data)
#Drop the attributes which is not required
iris_data <- iris_data[,-1]
#check for missing values
colSums(is.na(iris_data))
#No missing value so we are not going to impute anything

#Standardize the data using 'z-score'-----------------------------
#so that the computation of distance will be fast
iris_std <-decostand(iris_data,"st")

#Implementing k-means clusterig-----------------------------------
#Execute with random 'k'
#k-means clustering
fit <- kmeans(iris_std,centers = 8)

#Extracting output of the model and interpretation
fit
#with-in-sum of squares in each cluster
fit$withinss
sum(fit$withinss)
#cluster centers
fit$centers
#To check cluster number of each row in data
fit$cluster

#Determine number of clusters
#K-means: Determine number of clusters
wss <- 0
for(i in 1:15){
  set.seed(123)
  wss[i]<- sum(kmeans(iris_std,centers = i)$withinss)
}

#plot the cluster number and withinness error----------------------------
plot(1:15,wss,
     type="b",
     xlab="NUmber of clusters",
     ylab = "Total within-cluster sum of squares")

#fit the model with selected cluster number
set.seed(123)
fit<-kmeans(iris_std,centers = 3)
#with-in-sum of squares in each cluster
fit$withinss
sum(fit$withinss)

#visualising the clusters------------------------------------------------
clusplot(iris_std,
         fit$cluster,
         lines = TRUE,
         shade = TRUE,
         color=TRUE,
         labels = 2,
         plotchar = FALSE,
         span=TRUE,
         main=paste('cluster of clients'),
         xlab = "X-axis",
         ylab = "Y-axis")

#compaire the cluster with original value--------------------
table(data$Species,fit$cluster)
