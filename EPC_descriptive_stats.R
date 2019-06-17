library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)

epc <- fread("./99_New_Data/05_The_EUC_data/epc_col_cleaned1.csv", na.strings = c(" ", ""), select = c(4,8))

### EPC energy efficiency by OAC group ###

# read in postcode to OA lookup
OA_PC <- fread("./99_New_Data/05_The_EUC_data/PC_OA_LSOA_lookup.csv")
OA_PC$PCD7 <- gsub(' ', '', OA_PC$PCD7)

epc_oa <- merge(epc, OA_PC[,c(1,3)], by.x="POSTCODE", by.y="PCD7", all.x = TRUE)

# read in OAC groups

OAC <- fread("./99_New_Data/05_The_EUC_data/Output Area Classification/Tables/lookup.csv")

epc_oac <- merge(epc_oa, OAC, by.x="OA11CD", by.y="OA_SA")

epc_oac %<>%
  group_by(GRP) %>% 
  summarise(mean_cee = round(mean(CURRENT_ENERGY_EFFICIENCY),digits = 2))

epc_oac$SPRGRP <- substring(epc_oac$GRP, 1, 1)
  

p<-ggplot(epc_oac, aes(x=GRP, y=mean_cee, fill=SPRGRP)) +
  geom_bar(stat="identity") + 
  theme_minimal()
p


### EPC energy efficiency by IMD decile ###
epc_lsoa <- merge(epc, OA_PC[,c(1,4)], by.x="POSTCODE", by.y="PCD7", all.x = TRUE)

IMD <- fread("./99_New_Data/05_The_EUC_data/IMD_deciles.csv", select = c(1,3,5))

IMD %>%
  filter(Measurement == "Decile") -> IMD_D

IMD_D <- as.data.table(IMD_D)

epc_imd <- merge(epc_lsoa[,c(2,3)], IMD_D[,c(1,3)], by.y ="GeographyCode", by.x="LSOA11CD", allow.cartesian = TRUE)

epc_imd %<>%
  group_by(Value) %>% 
  summarise(mean_cee = round(mean(CURRENT_ENERGY_EFFICIENCY), digits = 2))

p2 <- ggplot(epc_imd, aes(x=Value, y=mean_cee, fill=Value)) +
  geom_bar(stat = "identity") +
  theme_minimal()
p2

### Correlation between EPC energy efficiency score and IMD rank ###
IMD %>%
  filter(Measurement == "Score") -> IMD_S

epc_imdS <- merge(epc_lsoa[,c(2,3)], IMD_S[,c(1,3)], by.y ="GeographyCode", by.x="LSOA11CD", allow.cartesian = TRUE)


p3 <- ggplot(epc_imdS, aes(x=Value, y=CURRENT_ENERGY_EFFICIENCY)) +
  geom_point(shape=0) +    # 0 = hollow squares
  geom_smooth(method=lm)   # Add linear regression line 
p3
