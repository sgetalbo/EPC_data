
### Inital clustering

# ===========================================================================
# Packages and data
# ===========================================================================

# Packages

library(data.table)
library(factoextra)
library(ggplot2)
library(rgdal)   # This package runs readOGR
library(broom)   # This package runs tidy()
library(dplyr)   # Needed to run grepl, even thought that is in base r, also needed for left_join
library(openxlsx)  # this one is needed to open the excel file
library(tmap)

# Data

epc_zscores <- fread("./05_The_EUC/01_New_data/epc_zscores.csv")
box_zscores <- fread("./05_The_EUC/01_New_data/boxcox_zscores.csv")

# ===========================================================================
# Non transformed data
# ===========================================================================

epc_zscores <- na.omit(epc_zscores) # take na values out
epc_zscores_PCS <- epc_zscores[,1] # replicate 'character' column
epc1 <- epc_zscores # replicate dataset
epc1$PCS <- NULL # remove character column

# ===========================================================================
# Abritrary clusters 
# ===========================================================================

clusters_epc <- kmeans(epc1, 5, nstart = 50, iter.max = 15)

# ===========================================================================
# Box-cox transformed data
# ===========================================================================

box_zscores <- na.omit(box_zscores) # take na values out
box_zscores_PCS <- box_zscores[,1] # replicate 'character' column
box1 <- box_zscores # replicate dataset
box1$PCS <- NULL # remove character column

# ===========================================================================
# Abritrary clusters 
# ===========================================================================

clusters_box <- kmeans(box1, 5, nstart = 50, iter.max = 15)

# =============================================
# Examine clusters 
# =============================================

table(clusters_epc$cluster) 
table(clusters_box$cluster)

clusters_epc$centers
clusters_box$centers 

# Variable means for each cluster - dataframe
epc_non_transformed <- fread("./05_The_EUC/01_New_data/nontransformed_data.csv")
epc_non_transformed <- na.omit(epc_non_transformed)

epc_cluster_means <- aggregate(epc_zscores,by=list(clusters_epc$cluster),FUN=mean)
box_cluster_means <- aggregate(box_zscores, by=list(clusters_box$cluster), FUN=mean)
epc_non_transformed_clustermeans <- aggregate(epc_non_transformed, by=list(clusters_epc$cluster), FUN=mean)

fwrite(epc_cluster_means, "./05_The_EUC/01_New_data/epc_zscore_cluster_means.csv")
fwrite(box_cluster_means, "./05_The_EUC/01_New_data/boxcox_zscore_cluster_means.csv")
fwrite(epc_non_transformed_clustermeans, "./05_The_EUC/01_New_data/epc_non_transformed_cluster_means.csv")

fviz_cluster(clusters_epc, epc1, ellipse.type = "convex", geom = "point", main = "Natural distribution cluster assignment", xlab = FALSE, ylab=FALSE) + theme_minimal()
fviz_cluster(clusters_box, box1, ellipse.type= "convex", geom = "point", main = "Box-Cox transformation cluster assignment", xlab = FALSE, ylab=FALSE) + theme_minimal()

# Assign clusters back to original datasets

epc_zscores$CLUSTER <- clusters_epc$cluster
fwrite(epc_zscores, "./05_The_EUC/01_New_data/epc_zscores_with_clusters.csv")

epc_non_transformed <- fread("./05_The_EUC/01_New_data/nontransformed_data.csv")
epc_non_transformed <- na.omit(epc_non_transformed)
epc_non_transformed$CLUSTER <- clusters_epc$cluster

fwrite(epc_non_transformed,"./05_The_EUC/01_New_data/epc_non_transformed_with_clusters.csv")

box_zscores$CLUSTER <- clusters_box$cluster
fwrite(box_zscores, "./05_The_EUC/01_New_data/box_zscore_with_clusters.csv")

# =============================================
# Further work 
# =============================================

# Testing the impact of normalisation

# =============================================
# Natural distribution
# =============================================

# Run k means for multiple k 

rng<-2:10 #K from 2 to 20
tries <-50 #Run the K Means algorithm 100 times
avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp <-kmeans(epc1,centers=v) #Run kmeans
    v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")

# Output the WCSS scores 

# =============================================
# Box-cox distribution
# =============================================


# Run k means for multiple k 

rng<-2:10 #K from 2 to 20
tries <-50 #Run the K Means algorithm 100 times
bavg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp <-kmeans(box1,centers=v) #Run kmeans
    v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  bavg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
plot(rng,bavg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")

# Output the WCSS scores

# =============================================
# Comparison
# =============================================


g <- ggplot(rng, aes(v))
g <- geom_line(aes(y=avg.totw.ss), colour="red")
g <- g + geom_line(aes(y=bavg.totw.ss), colour="green")
g

plot(rng, avg.totw.ss, type="l", col="red" )
par(new=TRUE)
plot(rng, bavg.totw.ss, type="l", col="green" )
