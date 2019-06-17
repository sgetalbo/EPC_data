
### Cluster sub-groups

# ===========================================================================
# Packages and data
# ===========================================================================

library(data.table)
library(factoextra)
library(ggplot2)
library(rgdal)   # This package runs readOGR
library(broom)   # This package runs tidy()
library(dplyr)   # Needed to run grepl, even thought that is in base r, also needed for left_join
library(openxlsx)  # this one is needed to open the excel file
library(tmap)

# Data

Data <- fread("./05_The_EUC/01_New_data/epc_zscores_with_full_clusters.csv")

# ===========================================================================
# Individual cluster datasets
# ===========================================================================

Cluster_1 <- subset(Data,CLUSTER == 1, select = 1:152)
Cluster_2 <- subset(Data,CLUSTER == 2, select = 1:152)
Cluster_3 <- subset(Data,CLUSTER == 3, select = 1:152)
Cluster_4 <- subset(Data,CLUSTER == 4, select = 1:152)
  
# ===========================================================================
# Clustergram functions
# ===========================================================================

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


# ===========================================================================
# Cluster subset clustergrams 
# ===========================================================================

cluster_eval <- function(data) { 

Data.m <- as.matrix(Cluster_1)

# test for robustness 

par(cex.lab = 1.2, cex.main = .7)
par(mfrow = c(3,2))
for(i in 1:6) clustergram(Data.m, k.range = 2:10 , line.width = .004, add.center.points = T)

}

# Save output plots manually

cluster_eval(Cluster_1)
cluster_eval(Cluster_2)
cluster_eval(Cluster_3)
cluster_eval(Cluster_4)

# ===========================================================================
# Cluster visualisation 
# ===========================================================================

# run multiple times changing value of k for visualisation

cluster_vis <- function(dat, i, clust) # dataframe, k, cluster for plot naming 
  {

clusters_epc <- kmeans(dat, i, iter.max = 100000)

table(clusters_epc$cluster)

title <- bquote(paste("Subgroups of Cluster", .(clust), ": k = ", .(i), "cluster assignment"))

viz <- fviz_cluster(clusters_epc, dat, ellipse.type = "convex", geom = "point", main = title, xlab = FALSE, ylab = FALSE) + theme_minimal()

print(viz)
return(clusters_epc)

}


# i value - number of clusters as ascertained from clustergrams

subgroups_1 <- cluster_vis(Cluster_1, 2, "1")
subgroups_2 <- cluster_vis(Cluster_2, 2, "2")
subgroups_3 <- cluster_vis(Cluster_3, 3, "3")
subgroups_4 <- cluster_vis(Cluster_4, 4, "4")

# ===========================================================================
# Cluster reassignment
# ===========================================================================

# Variable means for each cluster - dataframe

cluster_subgroups <- function(dat, sub, clust) {
  
  subgroups_summary <- aggregate(dat, by=list(sub), FUN=mean)

  title <-paste("./05_The_EUC/01_New_data/Subgroup_summary_cluster_",clust,".csv")
  title <- gsub(" ", "", title)
  
  fwrite(subgroups_summary, file = title)
  
}

cluster_subgroups(Cluster_1, subgroups_1$cluster, "1")
cluster_subgroups(Cluster_2, subgroups_2$cluster, "2")
cluster_subgroups(Cluster_3, subgroups_3$cluster, "3")
cluster_subgroups(Cluster_4, subgroups_4$cluster, "4")

# Assign clusters back to original datasets
Cluster_1 <- subset(Data,CLUSTER == 1) # need to reload as cluster assignment was dropped for subgroup clustering
Cluster_2 <- subset(Data,CLUSTER == 2)
Cluster_3 <- subset(Data,CLUSTER == 3)
Cluster_4 <- subset(Data,CLUSTER == 4)

Cluster_1$SUB_GROUP <- subgroups_1$cluster
Cluster_2$SUB_GROUP <- subgroups_2$cluster
Cluster_3$SUB_GROUP <- subgroups_3$cluster
Cluster_4$SUB_GROUP <- subgroups_4$cluster

SUBGROUPS <- rbind(Cluster_1,Cluster_2,Cluster_3,Cluster_4)

fwrite(SUBGROUPS, "./05_The_EUC/01_New_data/Full_clusters_with_subgroups.csv")
