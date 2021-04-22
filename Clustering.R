############# crime data #############

crime_data <- read.csv(file.choose())

# Normalize data #

normalized_crimedata <- scale(crime_data[,2:5])



##### Hiererchical Clustering #####

# Calculate distances
d <- dist(normalized_crimedata, method = "euclidean")

# Cluster model
fit1 <- hclust(d, method = "complete")
fit2 <- hclust(d, method = "single")
fit3 <- hclust(d, method = "average")
fit4 <- hclust(d, method = "centroid")
plot(fit1, hang = -1)
plot(fit2, hang = -1)
plot(fit3, hang = -1)
plot(fit4, hang = -1)

#clustering
rect.hclust(fit1, k=5, border = "red")
groups <- cutree(fit1, k=5)

#grouping in data
membership <- as.matrix(groups)
final <- data.frame(crime_data,membership)
write.csv(final, file = "Clustered_Crime_data.csv")

#Labelling groups
aggregate(crime_data[,-1],by = list(final$membership),mean)
agg <- aggregate(crime_data[,-1],by = list(final$membership),mean)
write.csv(agg, file = "Aggregate-Crime_data.csv")
getwd()



##### Kmeans clustering #####

wss = (nrow(normalized_crimedata)-1)*sum(apply(normalized_crimedata,2,var))
for (i in 2:8) wss[i] = sum(kmeans(normalized_crimedata, centers = i)$withinss)
plot(1:8, wss, type = "b", xlab = "Number of Clusters", ylab = "Within group sum of squares") 

#from elbow plot, we can select number of clusters = 5

fit_k <- kmeans(normalized_crimedata,5)
str(fit_k)

final_k <- data.frame(crime_data,fit_k$cluster)

agg_k <- aggregate(crime_data[,2:5],by = list(fit_k$cluster), FUN = mean)
write.csv(agg_k, file = "Aggregate_k-Crime_data.csv")



##### DBSCAN #####

install.packages("fpc")
library(fpc)

Dbscan <- dbscan(normalized_crimedata, eps = 1, MinPts = 3)
Dbscan

Dbscan$cluster

table(Dbscan$cluster, crime_data$X)

plot(Dbscan, normalized_crimedata)







############# Airlines #############

install.packages("xlsx")
library("xlsx")

airlines <- read.xlsx(file.choose(),2)
sum(is.na(airlines))

# Normalizing data #

norm_minmax <- function(x){
                            (x - min(x))/(max(x) - min(x))
}
normalized_airlines <- as.data.frame(lapply(airlines[,2:11],norm_minmax))
View(normalized_airlines)
normalized_airlines_1 <- cbind.data.frame(normalized_airlines,airlines$`Award?`)
View(normalized_airlines_1)



##### Hiererchical Clustering #####

# Calculating distance #
d_a <- dist(normalized_airlines_1,method = "euclidean")

# Cluster model
fit_a1 <- hclust(d_a, method = "complete")
fit_a2 <- hclust(d_a, method = "single")
fit_a3 <- hclust(d_a, method = "average")
fit_a4 <- hclust(d_a, method = "centroid")
plot(fit_a1, hang = -1)
plot(fit_a2, hang = -1)
plot(fit_a3, hang = -1)
plot(fit_a4, hang = -1)

#clustering
rect.hclust(fit_a1, k=3, border = "red")
groups_a <- cutree(fit_a1,k=3)

#grouping in data
membership_a <- as.matrix(groups_a)
final_a <- data.frame(airlines,membership_a)

# Labelling groups
agg_a <- aggregate(airlines[,-1],by=list(final_a$membership_a),mean)
View(agg_a)
write.csv(agg_a, file = "Aggregate-Airlines.csv")


##### Kmeans clustering #####

wss = (nrow(normalized_airlines_1)-1)*sum(apply(normalized_airlines_1,2,var))
for (i in 2:15) wss[i] = sum(kmeans(normalized_airlines_1, centers = i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within group sum of squares") 

# from elbow curve Optimum number of clusters = 6

fit_a_k <- kmeans(normalized_airlines_1,6)
str(fit_a_k)

final_a_k <- data.frame(airlines,fit_a_k$cluster)

agg_a_k <- aggregate(airlines[,2:12],by = list(fit_a_k$cluster), FUN = mean)
write.csv(agg_a_k, file = "Aggregate_k-Airlines.csv")

##### DBSCAN #####

library(fpc)

Dbscan_a <- dbscan(normalized_airlines_1, eps = 1, MinPts = 3)
Dbscan_a

Dbscan_a$cluster

table(Dbscan_a$cluster, airlines$`ID#`)

plot(Dbscan, normalized_airlines_1)