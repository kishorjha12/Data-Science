setwd("~/machine learning practice/clustring/protiens")

#European Protein consumption

#load library

food <- read.csv("protein.csv")
head(food)

#first clustering on just Red and White meat(p=2) and k=3 clusters.
set.seed(123456) #to fix the random starting clusters
grpMeat <- kmeans(food[,c("WhiteMeat","RedMeat")], centers = 3, nstart = 10)
grpMeat

#List of cluster assignments
o<- order(grpMeat$cluster)
data.frame(food$Country[o],grpMeat$cluster[o])

#graphical Representation of the clustering solution
plot(food$RedMeat,food$WhiteMeat, type = "n",xlim = c(3,19), xlab = "Red Meat", ylab = "White Meat")
text(x=food$RedMeat, y=food$WhiteMeat, labels = food$Country, col=grpMeat$cluster+1)

#protein groups change the number of clusters to 7
set.seed(123456)
grpProtein <- kmeans(food[,-1], centers = 7, nstart = 10)
o=order(grpProtein$cluster)
data.frame(food$Country[o],grpProtein$cluster[o])

library(cluster)
clusplot(food[,-1], grpProtein$cluster,main = '2D representation of the Cluster solution',color = TRUE, shade = TRUE,labels = 2, lines = 0)



#Hierarchical clustering
dendrogram = hclust(d = dist(food, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'protiens',
     ylab = 'average distances')

hc = hclust(d = dist(food, method = 'euclidean'), method = 'ward.D')
y_hc <- cutree(hc,3)
# Visualising the clusters
library(cluster)
clusplot(food,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of protiens'),
         xlab = 'protiens',
         ylab = 'euclidean distances')

