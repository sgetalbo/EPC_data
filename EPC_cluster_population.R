### Clusters by population density.

library(data.table)
library(magrittr)
library(dplyr)

Data <- fread("./05_The_EUC/01_New_data/epc_zscores_with_full_clusters.csv")
Pop <- fread("./05_The_EUC/01_New_data/Union_by_population.csv")

Pop %<>%
  group_by(PCS) %>% 
  summarise(tot_pop = sum(Total_headcount))
  

Cluster_pop <- merge(Data[, c(153,154)], Pop, by.x = "PCS", by.y="PCS")

Cluster_pop %<>%
  group_by(CLUSTER) %>% 
  summarise(cluster_tot = sum(tot_pop)) %>% 
  mutate(freq = cluster_tot / sum(cluster_tot) *100)

write.csv(Cluster_pop, "./05_The_EUC/01_New_data/Cluster_pop.csv")
