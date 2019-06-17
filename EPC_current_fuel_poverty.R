

#===========================================================================
#  Current fuel poverty 
#===========================================================================


#===========================================================================
#  Packages and data
#===========================================================================
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(rgdal)
library(maptools)


fuel_pov <- read_xlsx(path = "./05_The_EUC/01_New_data/Fuel_poverty_sub-regional_tables_2018.xlsx", sheet = 6, skip = 2, col_names = TRUE)
fuel_pov <- fuel_pov[,c(1,6,7,8)]

OA_PCS <- fread("./05_The_EUC/01_New_data/Union_by_population.csv", drop = c(1,5,6))

LSOA_lookup <- fread("./05_The_EUC/01_New_data/PC_OA_LSOA_lookup.csv")

#===========================================================================
#  Reweighting - LSOA to PCS headcount
#===========================================================================
# write the code for reweighting LSOA to PCS.

LSOA_PCS <- merge(OA_PCS, LSOA_lookup[,c(3,4)], by.x="OA_CODE", by.y = "OA11CD", all.x=TRUE)
LSOA_PCS <- unique(LSOA_PCS)

#Generate propotions of OA contained within PCS. 
LSOA_PCS %<>%
  group_by(LSOA11CD, PCS) %>%
  summarise(Total_headcount2 = sum(Total_headcount)) %>%
  mutate(prop_LSOA = round(Total_headcount2/sum(Total_headcount2),4)) %>%
  mutate(perc_LSOA= prop_LSOA*100)

write.csv(LSOA_PCS, "./05_The_EUC/01_New_data/LSOA_PCS_HEADCOUNT.csv")

LSOA_PCS <- fread("./05_The_EUC/01_New_data/LSOA_PCS_HEADCOUNT.csv", drop = 1)

#===========================================================================
#  Reweighting - Fuel Poverty proportions
#===========================================================================

#Merge weights onto fuel poverty data
fuel_props <- as.data.frame(merge(LSOA_PCS, fuel_pov, by.x="LSOA11CD", by.y="LSOA Code", all.x=TRUE))

#create a PCS headcount column for the calculation
fuel_props %<>% 
  group_by(PCS) %>% 
  mutate(PCS_headcount = sum(Total_headcount2)) %>% 
  ungroup(fuel_props)

#Create new dataset containing only proportions
Weighted_variables <- fuel_props[,-8]

names(Weighted_variables)[6] <- ("total_hh")
names(Weighted_variables)[7] <- ("fp_hh")

Weighted_variables %<>% 
  mutate_at(vars(c(6,7)), funs(round((.*prop_LSOA),2))) %>% 
  group_by(PCS) %>% 
  mutate_at(vars(c(6,7)), funs(sum))

Weighted_PCS_level <- distinct(Weighted_variables[,-c(1,3,4,5,8)])

Weighted_PCS_level$Prop_fuel_pov <-  (Weighted_PCS_level$fp_hh/Weighted_PCS_level$total_hh) * 100

fwrite(Weighted_PCS_level, "./05_The_EUC/01_New_data/PCS_current_fuel_poverty.csv")
