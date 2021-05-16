#Importing the required Libraries

library(dplyr)
library(stats)
library(ggplot2)
library(readr)


#Reading the .csv file

kmeans_data <-read.csv("C:/abcd/DataSet/pjct_demo/heart_failure_clinical_records_dataset.csv")
#kmeans_data


mydata <-select(kmeans_data,c(1,2,3,4,5))
mydata


x<-mydata[,1]
x

y<-mydata[,5]
y

min(x)
max(x)

min(y)
max(y)


#Slicing the Dataset

z<-mydata[,c(1,5)]
z

#Using Elbow method find optimal no of cluster
set.seed(6)
wcss<-vector()
for (i in 1:10) wcss[i]<-sum(kmeans(z,i)$withinss)
plot(1:10,wcss,type="b",main=paste("cluster of heart_failure_dataset"),
     xlab="No. of cluster",ylab="WCSS",panel.first = grid())


#Applying a K-Means Clustering to Heart_failure_clinical_reprot_dataset
set.seed(29)
kmeans <- kmeans(z,3,iter.max = 300,nstart = 10)


#Visualization of the Cluster
library(cluster)
clusplot(z,
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("cluster of heart failure data set"),
         xlab = "Age",
         ylab = "Ejection fraction"
         )




