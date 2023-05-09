library("readxl")
library("ggplot2")
library("dplyr")
# Reading in the vehicle.xlsx file
data <- read_excel("~/Documents/rcw/data/vehicles.xlsx")
#removing the class and sample column as we do not need it for the k means clustering.
data <- subset(data, select = -c(Samples, Class))
str(data)
#making sure there are no null values in the data set before checking for outliers.
sum(is.na(data))

#using the boxplot to visually show the outliers
boxplot(data)
#use the sapply function to find the z-score for all the samples to then determine the outliers.
#I am creating a new column for the z-score for each of the samples, this makes it easier to remove the outliers.
data$zscore <- sapply(data, function(data) (data-mean(data))/sd(data))
#just double checking the dimensions of the original data set before removing outliers.
dim(data)
head(data$zscore)

#creating a new table that does not contain the outliers, anything that has a z-score lower than 3 and more than -3.
no_outliers <- data[!rowSums(abs(data$zscore) > 3), ]

#checking the maximum and minimum z-score to make sure the previous step worked as planned.
max(no_outliers$zscore)
min(no_outliers$zscore)
#new dimension of the new table, it is now 824 x 19 instead of 846 x 19
dim(no_outliers)

#this is just visually compare the original data set with the newly made one
boxplot(data)
boxplot(no_outliers)

#now removing the z-score column as it will change the result of the clustering
no_outliers <- subset(no_outliers, select = -c(zscore))
#scaling the data using the built-in scale function in R, I am also making a new dataframe to make comparisons easier.
no_outliers_normalised <- as.data.frame(scale(no_outliers))
#making sure the data was normalised properly
no_outliers$Rad.Ra
no_outliers_normalised$Rad.Ra

#using automated tools to determine the best number of cluster centers
library("NbClust")
library("factoextra")
library("cluster")

#Nbclust
NbClust(no_outliers_normalised, distance = "euclidean", method = "kmeans", index="all")
#Elbow method
fviz_nbclust(no_outliers_normalised, kmeans, method='wss')
#Silhouette method
fviz_nbclust(no_outliers_normalised, kmeans, method='silhouette')
#Gap Stastics
fviz_nbclust(no_outliers_normalised, kmeans, method='gap_stat')

#performing kmeans clustering using the most favoured "k" from the automated methods used above.
kmeans_data <- kmeans(no_outliers_normalised, centers = 3, nstart = 10)
fviz_cluster(kmeans_data, data = no_outliers_normalised)

data_cluster <- data.frame(no_outliers_normalised, cluster = as.factor(kmeans_data$cluster))
head(data_cluster)

#showing the kmeans output
kmeans_data
#showing the within cluser summs of squares(WSS) and the between cluster sums of squares (BSS)
kmeans_data$centers
kmeans_data$tot.withinss
kmeans_data$betweenss
#showing the silhouette plot for the clustering
kmeans_data$cluster
silhouette <- silhouette(kmeans_data$cluster, dist(no_outliers_normalised))
fviz_silhouette(silhouette)

#applying PCA to the dataset to reduce dimensionality.
#this is the scaled dataset without the outliers
head(no_outliers_normalised)
pca_data <- prcomp(no_outliers_normalised, center = TRUE, scale = TRUE)
eigenvalues <- pca_data$sdev^2
eigenvectors <- pca_data$rotation
eigenvalues
eigenvectors
summary(pca_data)

#making a new dataset with only the first 6 PCA as the cumulative proportion for them is > 92%
transformed_data <- as.data.frame(-pca_data$x[,1:6])
head(transformed_data)
#new dimension is 824 x 6, thus reducing the attributes by 3 times.
dim(pca_data$x)
dim(transformed_data)

#applying the same 4 automated tools to find the new favoured k for the clustring
#Nbclust
NbClust(transformed_data, distance = "euclidean", method = "kmeans", index="all")
#Elbow method
fviz_nbclust(transformed_data, kmeans, method='wss')
#Silhouette method
fviz_nbclust(transformed_data, kmeans, method='silhouette')
#Gap Stastics
fviz_nbclust(transformed_data, kmeans, method='gap_stat')

#performing kmeans clustering using the new dataset
#performing kmeans clustering using the most favoured "k" from the automated methods used above.
kmeans_pca_data <- kmeans(transformed_data, centers = 3, nstart = 10)
fviz_cluster(kmeans_pca_data, data = transformed_data)

data_cluster <- data.frame(transformed_data, cluster = as.factor(kmeans_pca_data$cluster))
head(data_cluster)

#showing the kmeans output
kmeans_pca_data
#showing the within cluser summs of squares(WSS) and the between cluster sums of squares (BSS)
kmeans_pca_data$centers
kmeans_pca_data$tot.withinss
kmeans_pca_data$betweenss
#showing the silhouette plot for the clustering
kmeans_pca_data$cluster
silhouette <- silhouette(kmeans_pca_data $cluster, dist(transformed_data))
fviz_silhouette(silhouette)
