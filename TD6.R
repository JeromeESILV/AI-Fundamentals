setwd("C:/Users/pc/OneDrive/Uni/R_AI Fundamentals/Ligue1/")

#1 Import the dataset
ligue1 <- read.csv( "ligue1_17_18.csv", header=TRUE, sep=";", row.names=1)

##2 Print the first two rows and the total number of columns in the dataset
head(ligue1, n=2)
ncol(ligue1)

###3 Create a new dataset with only the Points and Yellow.cards columns
pointsCards <- select(ligue1, Points, yellow.cards)

####4 Apply k-means on pointsCards with 2 clusters and 20 iterations
set.seed(1)
km <- kmeans(pointsCards, centers=2, iter.max=20)

#####5 Print and describe the contents of km
print(km)

######6 Print the coordinates of the cluster centers
km$centers

#######7 Plot the data with points colored according to their cluster
plot(pointsCards$yellow.cards, pointsCards$Points, col=km$cluster)

########8 Add the cluster centers to the plot
points(km$centers, col=1:2, pch=8, cex=2)

# Add the labels for each observation
text(pointsCards$yellow.cards, pointsCards$Points, labels=row.names(pointsCards))

#########9 Re-run k-means on pointsCards using 3 and 4 clusters
set.seed(1)
km3 <- kmeans(pointsCards, centers=3, iter.max=20)
km4 <- kmeans(pointsCards, centers=4, iter.max=20)

# Plot the data with 3 clusters
plot(pointsCards$yellow.cards, pointsCards$Points, col=km3$cluster)

# Add the cluster centers to the plot
points(km3$centers, col=1:3, pch=8, cex=2)

# Add the labels for each observation
text(pointsCards$yellow.cards, pointsCards$Points, labels=row.names(pointsCards))

# Plot the data with 4 clusters
plot(pointsCards$yellow.cards, pointsCards$Points, col=km4$cluster)

# Add the cluster centers to the plot
points(km4$centers, col=1:4, pch=8, cex=2)

# Add the labels for each observation
text(pointsCards$yellow.cards, pointsCards$Points, labels=row.names(pointsCards))

##########10 Convert non-numeric columns in ligue1 to numeric
ligue1 <- ligue1 %>% mutate_if(.predicate=is.numeric, as.numeric)

# scale the dataset
ligue1_scaled <- as.data.frame(scale(ligue1))
# set the seed for reproducibility
set.seed(1)

# perform k-means clustering on the ligue1 dataset with 3 clusters and 20 iterations
km.ligue1 <- kmeans(ligue1, centers=3, iter.max=20)

# perform k-means clustering on the ligue1_scaled dataset with 3 clusters and 20 iterations
km.ligue1.scaled <- kmeans(ligue1_scaled, centers=3, iter.max=20)

#11
# set the seed for reproducibility
set.seed(1)

# perform k-means clustering on the ligue1 dataset with 3 clusters and 20 iterations
km.ligue1 <- kmeans(ligue1, centers=3, iter.max=20)

# perform k-means clustering on the ligue1_scaled dataset with 3 clusters and 20 iterations
km.ligue1.scaled <- kmeans(ligue1_scaled, centers=3, iter.max=20)
