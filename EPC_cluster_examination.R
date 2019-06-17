
### EPC cluster examinations

# ============================================
# Packages and data
# ============================================

# Load in kmeans .Rdata files
load("./05_The_EUC/01_New_data/EPC_saved_clusters.RData")

# Load in both datasets - natural and cube root

EPC_nat <- fread("./05_The_EUC/01_New_data/EPCnatural_with_clusters.csv")
EPC_cr <- fread("./05_The_EUC/01_New_data/EPCcuberoot_with_clusters.csv")

# load in original dataset

EPC <- fread("./05_The_EUC/01_New_data/epc_non_transformed.csv")


# =============================================
# Examine clusters 
# =============================================

table(clusters_nat$cluster)  
table(clusters_cr$cluster)

# Variable means for each cluster - dataframe

Natural_cluster_means <- aggregate(EPC_nat,by=list(clusters_nat$cluster),FUN=mean)
Cuberoot_cluster_means <- aggregate(EPC_cr, by=list(clusters_cr$cluster), FUN=mean)

# Assign clusters back to original dataset

EPC <- na.omit(EPC)

EPC$nat_clust <- clusters_nat$cluster
EPC$cr_clust <- clusters_cr$cluster

aggregate(d[, 3:4], list(d$Name), mean)

Natural_clusters <- aggregate(EPC[, c(2:150)], list(EPC$nat_clust), mean)
Cuberoot_clusters <- aggregate(EPC[, c(2:150)], list(EPC$cr_clust), mean)

fwrite(Natural_clusters, "./05_The_EUC/01_New_data/Natural_distribution_cluster_means.csv")
fwrite(Cuberoot_clusters, "./05_The_EUC/01_New_data/Cuberoot_distribution_cluster_means.csv")

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

EPC_nat_PCS_map <- tm_shape(PCS_shp, projection = 27700) +
  tm_polygons(col="cluster", style='fixed', breaks = c(1,2,3,4,5,6,7,8), n = 7, border.col = "grey50",   palette = "Set1", border.alpha = 0.1, title="EPC clusters", showNA=FALSE)

EPC_nat_PCS_map


# Cluster graphs - counts/lines/bars.

