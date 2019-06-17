
### EPC natural distribution k means clustering

# ===========================================================================
# Packages and data
# ===========================================================================
# Packages

library(data.table)

# Data

EPC_nat <- fread("./05_The_EUC/01_New_data/natural_distribution.csv")

EPC_nat <- na.omit(EPC_nat) # take na values out
EPCnat_PCS <- EPC_nat[,1] # replicate 'character' column
EPC1 <- EPC_nat # replicate dataset
EPC1$PCS <- NULL # remove character column

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
# Clustergrams
#===========================================================================


clustergram.kmeans <- function(Data, k, ...)
{
  # this is the type of function that the clustergram
  # 	function takes for the clustering.
  # 	using similar structure will allow implementation of different clustering algorithms
  
  #	It returns a list with two elements:
  #	cluster = a vector of length of n (the number of subjects/items)
  #				indicating to which cluster each item belongs.
  #	centers = a k dimensional vector.  Each element is 1 number that represent that cluster
  #				In our case, we are using the weighted mean of the cluster dimensions by 
  #				Using the first component (loading) of the PCA of the Data.
  
  cl <- kmeans(Data, k,...)
  
  cluster <- cl$cluster
  centers <- cl$centers %*% princomp(Data)$loadings[,1]	# 1 number per center
  # here we are using the weighted mean for each
  
  return(list(
    cluster = cluster,
    centers = centers
  ))
}		

clustergram.plot.matlines <- function(X,Y, k.range, 
                                      x.range, y.range , COL, 
                                      add.center.points , centers.points)
{
  plot(0,0, col = "white", xlim = x.range, ylim = y.range,
       axes = F,
       xlab = "Number of clusters (k)", ylab = "PCA weighted Mean of the clusters", main = c("Clustergram of the PCA-weighted Mean of" ,"the clusters k-mean clusters vs number of clusters (k)"))
  axis(side =1, at = k.range)
  axis(side =2)
  abline(v = k.range, col = "grey")
  
  matlines(t(X), t(Y), pch = 19, col = COL, lty = 1, lwd = 1.5)
  
  if(add.center.points)
  {
    require(plyr)
    
    xx <- ldply(centers.points, rbind)
    points(xx$y~xx$x, pch = 19, col = "red", cex = 1.3)
    
    # add points	
    # temp <- l_ply(centers.points, function(xx) {
    # with(xx,points(y~x, pch = 19, col = "red", cex = 1.3))
    # points(xx$y~xx$x, pch = 19, col = "red", cex = 1.3)
    # return(1)
    # })
    # We assign the lapply to a variable (temp) only to suppress the lapply "NULL" output
  }	
}



clustergram <- function(Data, k.range = 2:10 , 
                        clustering.function = clustergram.kmeans,
                        clustergram.plot = clustergram.plot.matlines, 
                        line.width = .004, add.center.points = T)
{
  # Data - should be a scales matrix.  Where each column belongs to a different dimension of the observations
  # k.range - is a vector with the number of clusters to plot the clustergram for
  # clustering.function - this is not really used, but offers a bases to later extend the function to other algorithms 
  #			Although that would  more work on the code
  # line.width - is the amount to lift each line in the plot so they won't superimpose eachother
  # add.center.points - just assures that we want to plot points of the cluster means
  
  n <- dim(Data)[1]
  
  PCA.1 <- Data %*% princomp(Data)$loadings[,1]	# first principal component of our data
  
  if(require(colorspace)) {
    COL <- heat_hcl(n)[order(PCA.1)]	# line colors
  } else {
    COL <- rainbow(n)[order(PCA.1)]	# line colors
    warning('Please consider installing the package "colorspace" for prittier colors')
  }
  
  line.width <- rep(line.width, n)
  
  Y <- NULL	# Y matrix
  X <- NULL	# X matrix
  
  centers.points <- list()
  
  for(k in k.range)
  {
    k.clusters <- clustering.function(Data, k)
    
    clusters.vec <- k.clusters$cluster
    # the.centers <- apply(cl$centers,1, mean)
    the.centers <- k.clusters$centers 
    
    noise <- unlist(tapply(line.width, clusters.vec, cumsum))[order(seq_along(clusters.vec)[order(clusters.vec)])]	
    # noise <- noise - mean(range(noise))
    y <- the.centers[clusters.vec] + noise
    Y <- cbind(Y, y)
    x <- rep(k, length(y))
    X <- cbind(X, x)
    
    centers.points[[k]] <- data.frame(y = the.centers , x = rep(k , k))	
    #	points(the.centers ~ rep(k , k), pch = 19, col = "red", cex = 1.5)
  }
  
  
  x.range <- range(k.range)
  y.range <- range(PCA.1)
  
  clustergram.plot(X,Y, k.range, 
                   x.range, y.range , COL, 
                   add.center.points , centers.points)
  
  
}

Data <- na.omit(EPC1) # listwise deletion of missing
Data$PCS <- NULL

Data.m <- as.matrix(Data)
clustergram(Data.m, k.range = 2:10, line.width = 0.004)

# test for robustness 

par(cex.lab = 1.2, cex.main = .7)
par(mfrow = c(3,2))
for(i in 1:6) clustergram(Data.m, k.range = 2:10 , line.width = .004, add.center.points = T)


#===========================================================================
# Optimum number of clusters 
#===========================================================================

library(factoextra)

# Based on elbow plot number of clusters should be 5
# Based on silhouette plot number of clusters should be
# Based on gap statistics number of clusters should be 

clusters_nat <- kmeans(EPC1, 7, nstart = 50,iter.max = 15)


# Save the cluster number in the dataset as column 'Borough'
EPC_nat$cluster <- as.factor(clusters_nat$cluster)

fwrite(EPC_nat, "./05_The_EUC/01_New_data/EPCnatural_with_clusters.csv")


clusters_nat

# Inspect 'clusters'
str(clusters)

fviz_cluster(clusters_nat, EPC1, ellipse.type = "norm") + theme_minimal()

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

PCS_shp@data <- merge(PCS_shp@data, EPC, by.x="StrSect", by.y="PCS", all.x=TRUE)

# ===========================================================================
# Create the maps
# ===========================================================================

EPC_PCS_map <- tm_shape(PCS_shp, projection = 27700) +
  tm_polygons(col="cluster", style='fixed', breaks = c(1,2,3,4,5,6,7,8), n = 7, border.col = "grey50",   palette = "Set1", border.alpha = 0.1, title="EPC clusters", showNA=FALSE)

EPC_PCS_map
 
library(tmaptools)
palette_explorer()
