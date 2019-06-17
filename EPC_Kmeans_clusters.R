### EPC natural distribution k means clustering

# ===========================================================================
# Packages and data
# ===========================================================================
library(data.table)

EPC <- fread("./05_The_EUC/01_New_data/natural_distriburion.csv")

EPC <- na.omit(EPC)
EPC_PCS <- EPC[,1]
EPC1 <- EPC
EPC1$PCS <- NULL

# ===========================================================================
# Elbow Method for finding the optimal number of clusters
# ===========================================================================


# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(EPC1, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#===========================================================================
# Average silhouette width
#===========================================================================
library(cluster)

sil <- rep(0, k.max)

# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  km.res <- kmeans(EPC1, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  sil[i] <- mean(ss[, 3])
}

# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)

#===========================================================================
# Gap statistic
#===========================================================================

gap_stat <- clusGap(EPC1, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 500)

fviz_gap_stat(gap_stat)

#===========================================================================
# Optimum number of clusters 
#===========================================================================

# Based on elbow plot number of clusters should be 5
# Based on silhouette plot number of clusters should be
# Based on gap statistics number of clusters should be 
clusters <- kmeans(EPC1,6,nstart = 50,iter.max = 15)


# Save the cluster number in the dataset as column 'Borough'
EPC$cluster <- as.factor(clusters$cluster)

clusters

# Inspect 'clusters'
str(clusters)

fviz_cluster(clusters, EPC1, ellipse.type = "norm") + theme_minimal()

### Mapping clusters 

# ===========================================================================
# Packages and data
# ===========================================================================

library(ggplot2)
library(rgdal)   # This package runs readOGR
library(broom)   # This package runs tidy()
library(dplyr)   # Needed to run grepl, even thought that is in base r, also needed for left_join
library(openxlsx)  # this one is needed to open the excel file
library(tmap)

PCS_shp <- readOGR("./99_New_Data/05_The_EUC_data/PostalSector.shp")

PCS_shp@data <- merge(PCS_shp@data, EPC, by.x="", by.y="PCS", all.x=TRUE)

# ===========================================================================
# Create the maps
# ===========================================================================

EPC_PCS_map <- tm_shape(PCS-PCS_shp) +
  tm_polygons(col="cluster", style='fixed', breaks = c(1,2,3,4,5,6,7), n =5, border.col = "grey50",  border.alpha = 0.1, title="EPC clusters", showNA=FALSE)

EPC_PCS_map
