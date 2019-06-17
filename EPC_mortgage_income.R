
#===========================================================================
#  Packages and data
#===========================================================================

library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(rgdal)
library(maptools)

Data <- fread("./05_The_EUC/01_New_data/epc_zscores_with_full_clusters.csv")

income <- read_xlsx("./05_The_EUC/01_New_data/MSOA_total_income.xlsx")

#===========================================================================
#  Income data
#===========================================================================
# 2012 data
SAI_2012_before <- read_xls("./05_The_EUC/01_New_data/small_area_income_2012.xls", sheet = 6, skip = 4, col_names = TRUE)
SAI_2012_after <- read_xls("./05_The_EUC/01_New_data/small_area_income_2012.xls", sheet = 7, skip = 4, col_names = TRUE)
# 2014 data
SAI_2014_before <- read_xls("./05_The_EUC/01_New_data/small_area_income_2014.xls", sheet = 6, skip = 4, col_names = TRUE)
SAI_2014_after <- read_xls("./05_The_EUC/01_New_data/small_area_income_2014.xls", sheet = 7, skip = 4, col_names = TRUE)
# 2016 data
SAI_2016_before <- read_xls("./05_The_EUC/01_New_data/small_area_income_2016.xls", sheet = 6, skip = 4, col_names = TRUE)
SAI_2016_after <- read_xls("./05_The_EUC/01_New_data/small_area_income_2016.xls", sheet = 7, skip = 4, col_names = TRUE)
# Combine them all
weekly_SAI <- cbind(SAI_2012_before[,c(1,7)], SAI_2012_after[,7], SAI_2014_before[,7], SAI_2014_after[,7]) 
# Rename columns
names(weekly_SAI)[2:5] <- c("b_2012", "a_2012", "b_2014", "a_2014")
# Multiply by 52 for annual figures 
annual_SAI <- apply(weekly_SAI[,c(2:5)], 2, 52, FUN = "*")
# 2016 is already an annual figure 
annual_SAI <- cbind(annual_SAI, SAI_2016_before[,7], SAI_2016_after[,c(1,7)])
names(annual_SAI)[5:7] <- c("b_2016",  "MSOA", "a_2016")
# Reorder for sense check 
annual_SAI <- annual_SAI[c(6,1,2,3,4,5,7)]

# Calculate the percentage change for housing costs 
# how to calculate a percentage DECREASE : original - decrease then decrease / original * 100 

# these three calculate the percentage change between before and after housing costs each year
annual_SAI$pch_2012 <- (annual_SAI$b_2012 - annual_SAI$a_2012) / annual_SAI$b_2012 * 100
annual_SAI$pch_2014 <- (annual_SAI$b_2014 - annual_SAI$a_2014) / annual_SAI$b_2014 * 100
annual_SAI$pch_2016 <- (annual_SAI$b_2016 - annual_SAI$a_2016) / annual_SAI$b_2016 * 100

#===========================================================================
#  Quintiles (2012 before)
#===========================================================================

quintiles <- within(annual_SAI, quintile <- as.integer(cut(b_2012, quantile(b_2012, probs=0:5/5, na.rm = TRUE), include.lowest=TRUE)))

#===========================================================================
#  Quintiles - housing cost only 
#===========================================================================

quintiles %>%
  group_by(quintile) %>% 
  summarise_at(vars(2:7), mean) -> quin_B_A

quintile_wide <- melt(data = quin_B_A, "quintile")

quintile_wide <- na.omit(quintile_wide)

quintile_wide$quin_b_a <- paste(quintile_wide$quintile, substr(quintile_wide$variable, 1,1))
quintile_wide$type <- substr(quintile_wide$variable,1,1)

quintile_wide$type <- replace(x = quintile_wide$type, quintile_wide$type=="a", "after")
quintile_wide$type <- replace(x = quintile_wide$type, quintile_wide$type=="b", "before")

quintile_wide$type = factor(quintile_wide$type, levels = c("before", "after"))
quintile_wide$Quintile <- as.factor(quintile_wide$quintile)

quintile_wide$year <- substr(quintile_wide$variable, 3,6)

ggplot(quintile_wide, aes(x=year, y=value,  group=quin_b_a, linetype=type, color=Quintile)) +
  geom_line(aes()) +
  geom_point() + 
  theme_minimal() + 
  labs(x = "Year", y = "Income Change after housing cost")

# percentage change within quintiles
quintiles %>% 
  group_by(quintile) %>% 
  summarise_at(vars(8:10), mean) -> quin_pc

quin_pc_wide <- melt(data = quin_pc, "quintile")

quin_pc_wide <- na.omit(quin_pc_wide)

quin_pc_wide$Quintile <- as.factor(quin_pc_wide$quintile)

quin_pc_wide$year <- substr(quin_pc_wide$variable, 5,8)

ggplot(quin_pc_wide, aes(x=year, y=value,  group=Quintile, color=Quintile)) +
  geom_line(aes()) +
  geom_point() +
  theme_minimal() + 
  labs(x = "Year", y = "Percentage Change after housing cost")

#===========================================================================
#  MSOA PCS reweighting
#===========================================================================

# write the code for reweighting MSOA to PCS.
OA_PCS <- fread("./05_The_EUC/01_New_data/Union_by_population.csv", drop = c(1,5,6))
MSOA_lookup <- fread("./05_The_EUC/01_New_data/PC_OA_LSOA_lookup.csv", drop = c(1,2,4,5,7:11))

MSOA_PCS <- merge(OA_PCS, MSOA_lookup, by.x="OA_CODE", by.y = "OA11CD", all.x = TRUE)

#Generate propotions of OA contained within PCS. 
MSOA_PCS %<>%
  group_by(MSOA11CD, PCS) %>%
  summarise(Total_headcount2 = sum(Total_headcount)) %>%
  mutate(prop_MSOA = round(Total_headcount2/sum(Total_headcount2),4)) %>%
  mutate(perc_MSOA= prop_MSOA*100)

write.csv(MSOA_PCS, "./05_The_EUC/01_New_data/MSOA_PCS_HEADCOUNT.csv")

MSOA_PCS <- fread("./05_The_EUC/01_New_data/MSOA_PCS_HEADCOUNT.csv")

MSOA_PCS$V1 <- NULL
#Merge weights onto income data
PCS_INCOME <- as.data.frame(merge(MSOA_PCS, quintiles, by.x="MSOA11CD", by.y="MSOA"))

#create a PCS headcount column for the calculation
PCS_INCOME %<>% 
  group_by(PCS) %>% 
  mutate(PCS_headcount = sum(Total_headcount2)) %>% 
  ungroup(PCS_INCOME)


#Create new dataset containing only proportions
Weighted_variables <- PCS_INCOME

Weighted_variables %<>% 
  mutate_at(vars(c(6:11)), funs(round((.*Total_headcount2),2))) %>% 
  group_by(PCS) %>% 
  mutate_at(vars(c(6:11)), funs(sum)) %>% 
  mutate_at(vars(c(6:11)), funs(./PCS_headcount))

Weighted_variables <- Weighted_variables[,-c(1,3,4,5)]
Weighted_PCS_level <- distinct(Weighted_variables)

  
# these three calculate the percentage change between before and after housing costs each year
Weighted_PCS_level$pch_2012 <- (Weighted_PCS_level$b_2012 - Weighted_PCS_level$a_2012) / Weighted_PCS_level$b_2012 * 100
Weighted_PCS_level$pch_2014 <- (Weighted_PCS_level$b_2014 - Weighted_PCS_level$a_2014) / Weighted_PCS_level$b_2014 * 100
Weighted_PCS_level$pch_2016 <- (Weighted_PCS_level$b_2016 - Weighted_PCS_level$a_2016) / Weighted_PCS_level$b_2016 * 100

#===========================================================================
# Write new data
#===========================================================================

write.csv(Weighted_PCS_level, "./05_The_EUC/01_New_data/PCS_weighted_income.csv")

#===========================================================================
#  Postcode sector weighted income quintiles
#===========================================================================

PCS_quintiles <- within(Weighted_PCS_level, quintile <- as.integer(cut(b_2012, quantile(b_2012, probs=0:5/5, na.rm = TRUE), include.lowest=TRUE)))

PCS_quintiles %>%
  group_by(quintile) %>% 
  summarise_at(vars(2:7), mean) -> PCS_quin_B_A

PCS_quintile_wide <- melt(data = PCS_quin_B_A, "quintile")

PCS_quintile_wide <- na.omit(PCS_quintile_wide)

PCS_quintile_wide$quin_b_a <- paste(PCS_quintile_wide$quintile, substr(PCS_quintile_wide$variable, 1,1))
PCS_quintile_wide$type <- substr(PCS_quintile_wide$variable,1,1)

PCS_quintile_wide$type <- replace(x = PCS_quintile_wide$type, PCS_quintile_wide$type=="a", "after")
PCS_quintile_wide$type <- replace(x = PCS_quintile_wide$type, PCS_quintile_wide$type=="b", "before")

PCS_quintile_wide$type = factor(PCS_quintile_wide$type, levels = c("before", "after"))
PCS_quintile_wide$Quintile <- as.factor(PCS_quintile_wide$quintile)

PCS_quintile_wide$year <- substr(PCS_quintile_wide$variable, 3,6)

ggplot(PCS_quintile_wide, aes(x=year, y=value,  group=quin_b_a, linetype=type, color=Quintile)) +
  geom_line(aes()) +
  geom_point() +
  theme_minimal() + 
  labs(x = "Year", y = "Income Change", title = "Postcode Sector Weighted Income quintiles")

ggsave("./05_The_EUC/99_Images/PCS_income_quintiles.jpeg")

PCS_quintiles %>% 
  group_by(quintile) %>% 
  summarise_at(vars(8:10), mean) -> PCS_quin_pc

PCS_quin_pc_wide <- melt(data = PCS_quin_pc, "quintile")

PCS_quin_pc_wide <- na.omit(PCS_quin_pc_wide)

PCS_quin_pc_wide$Quintile <- as.factor(PCS_quin_pc_wide$quintile)

PCS_quin_pc_wide$year <- substr(PCS_quin_pc_wide$variable, 5,8)


ggplot(PCS_quin_pc_wide, aes(x=year, y=value,  group=Quintile, color=Quintile)) +
  geom_line(aes()) +
  geom_point() +
  theme_minimal() + 
  labs(x = "Year", y = "Percentage Change", title = "Postcode Sector Weighted Income Percentage Change")

ggsave("./05_The_EUC/99_Images/PCS_income_change.jpeg")

#===========================================================================
#  Cluster income quintiles - Number one
#===========================================================================

PCS_quintiles1 <- unique(PCS_quintiles)


# Then average income for each cluster

Data_inc <- merge(Data[,c(153,154)], PCS_quintiles1[,-c(12)], by.x = "PCS", by.y="PCS", all.x = TRUE, na.rm=FALSE)

# average cluster income 

Data_inc %>% 
  group_by(CLUSTER) %>% 
  summarise_at(vars(3:11), mean, na.rm=TRUE) -> cluster_income

cluster_income_wide <- melt(data = cluster_income, "CLUSTER")

cluster_income_wide <- na.omit(cluster_income_wide)

cluster_income_wide$clust_b_a <- paste(cluster_income_wide$CLUSTER, substr(cluster_income_wide$variable, 1,1))
cluster_income_wide$type <- substr(cluster_income_wide$variable,1,1)

cluster_income_wide$type <- replace(x = cluster_income_wide$type, cluster_income_wide$type=="a", "after")
cluster_income_wide$type <- replace(x = cluster_income_wide$type, cluster_income_wide$type=="b", "before")

cluster_income_wide$type = factor(cluster_income_wide$type, levels = c("before", "after"))
cluster_income_wide$CLUST <- as.factor(cluster_income_wide$CLUSTER)

cluster_income_wide$year <- substr(cluster_income_wide$variable, 3,6)

clust_colours <- c("#E72E07", "#F8F345", "#4087EB", "#2DDD27")

ggplot(cluster_income_wide, aes(x=year, y=value,  group=clust_b_a, linetype=type, color=CLUST)) +
  geom_line(aes()) +
  geom_point() +
  theme_minimal() + 
  scale_color_manual(values = clust_colours) +
  labs(x = "Year", y = "Income Change", title = "Cluster Incomes", subtitle = "Showing change in income before and after housing costs between 2012 and 2016")

cluster_income %>% 
  group_by(CLUSTER) %>% 
  summarise_at(vars(11:13), mean) -> cluster_change


cluster_change_wide <- melt(data = cluster_change, "CLUSTER")

cluster_change_wide <- na.omit(cluster_change_wide)

cluster_change_wide$CLUST <- as.factor(cluster_change_wide$CLUSTER)

cluster_change_wide$year <- substr(cluster_change_wide$variable, 5,8)

ggplot(cluster_change_wide, aes(x=year, y=value,  group=CLUST, color=CLUST)) +
  geom_line(aes()) +
  geom_point() +
  theme_minimal() + 
  scale_color_manual(values = clust_colours) +
  labs(x = "Year", y = "Percentage Change", title = "Cluster Incomes", subtitle = "Showing percentage change in income before and after housing costs between 2012 and 2016")

#===========================================================================
#  Cluster income quintiles - Number two
#===========================================================================


# What can be done in QGIS?

# I need to split the datasets into each cluster and make choropleth maps

fwrite(Data_inc, "./05_The_EUC/01_New_data/cluster_income_ranges.csv")

# Load in a PCS shapefile and join

cluster_shp <- readOGR("./05_The_EUC/01_New_data/EPC_clusters.shp")

PCS_shp@data <- merge(PCS_shp@data, Data_inc, by.x="StrSect", by.y="PCS", all.x=TRUE)

writeSpatialShape(PCS_shp, "./05_The_EUC/01_New_data/PCS_income.shp")

#===========================================================================
#  Cluster income quintiles - summary table 
#===========================================================================

Data_inc %>% 
  group_by(CLUSTER, quintile) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n))-> Cluster_quintile_summary

#===========================================================================
#  
#===========================================================================
