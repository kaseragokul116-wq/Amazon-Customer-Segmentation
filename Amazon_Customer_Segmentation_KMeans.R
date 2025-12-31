library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)

Data <- read.csv("C:/Users/USER/Downloads/amazon_sales_data 2025.csv") 
Data
ex_data<- Data %>% select(Price,Quantity,Total.Sales) 
ex_data<- na.omit(ex_data)

wcss <- c()

for (k in 1:10) {
  km <- kmeans(ex_data, centers = k, nstart = 25)
  wcss[k] <- km$tot.withinss
}

plot(1:10, wcss, type = "b",
     xlab = "Number of Clusters (K)",
     ylab = "WCSS",
     main = "Elbow Method")
wcss[1:3]

set.seed(123)

kmeans_final <- kmeans(
  ex_data,
  centers = 3,
  nstart = 25
)
kmeans_final$centers #Cluster 3[premium customers],Cluster 2[Churn risk customers],Cluster 1[Regular customers]
result <- cbind(ex_data, Cluster = kmeans_final$cluster)
head(result)
