library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)

# just select PCS and current energy efficiency columns
epc <- fread("./99_New_Data/05_The_EUC_data/epc_full_clean.csv", na.strings = c(" ", ""), select = c(4,8))
# just select postcode and OA code
NSPL <- fread("./99_New_Data/NSPL_MAY_2018_UK/Data/NSPL_MAY_2018_UK.csv" , select = c(1, 33))

########## EPC energy efficiency by OAC group ########

NSPL$pcd <- gsub(' ', '', NSPL$pcd)
NSPL$oac11 <- substr(NSPL$oac11, 1 , 2)


epc_oac <- merge(epc, NSPL, by.x="POSTCODE", by.y = "pcd", all.x = TRUE)

epc_oac %<>%
  group_by(oac11) %>% 
  summarise(mean_cee = round(mean(CURRENT_ENERGY_EFFICIENCY),digits = 2))

epc_oac$SPRGRP <- substring(epc_oac$oac11, 1, 1)

epc_oac <- as.data.frame(epc_oac)
epc_oac$oac11 <- tolower(epc_oac$oac11)

epc_oac %<>% 
  filter(oac11 != "")
  
### Fix labels

group_labels <- fread("99_New_Data/05_The_EUC_data/Output Area Classification/Tables/Cluster_Names.csv", select = 2,  na.strings = c(" ", ""))

group_labels %<>%
  filter(Group != "NA")

group_labels$grp_ltr <- substring(group_labels$Group, 1, 2) # numbers represent start and end position.

epc_oac <- merge(epc_oac, group_labels, by.x = "oac11", by.y = "grp_ltr")
names(epc_oac)[3] <- "Supergroup"

### Plot EPC / OAC group

p <-ggplot(epc_oac, aes(x=Group, y=mean_cee, fill=Supergroup)) +
  geom_bar(stat="identity") + 
  theme_minimal() +
  xlab("OAC group") +
  ylab("Mean Current Energy Efficiency Score") +
  ggtitle("Average current energy efficiency rating by OAC group") +
  theme(axis.text.x = element_text(angle = 90)) 
  #theme(legend.position = "none") + 
p


### EPC energy efficiency by IMD decile ###

epc_lsoa <- merge(epc, OA_PC[,c(1,4)], by.x="POSTCODE", by.y="PCD7", all.x = TRUE)

IMD <- fread("./99_New_Data/05_The_EUC_data/IMD_deciles.csv", select = c(1:6))

IMD %>%
  filter(Measurement == "Decile") %>% 
  filter(`Indices of Deprivation` == "a. Index of Multiple Deprivation (IMD)") -> IMD_D

IMD_D <- as.data.table(IMD_D)

epc_imdD <- merge(epc_lsoa[,c(2,3)], IMD_D[,c(1,3)], by.y ="GeographyCode", by.x="LSOA11CD", allow.cartesian = TRUE)

epc_imdD %<>%
  group_by(Value) %>% 
  summarise(mean_cee = round(mean(CURRENT_ENERGY_EFFICIENCY), digits = 2))

p2 <- ggplot(epc_imdD, aes(x=Value, y=mean_cee, fill=Value)) +
  geom_bar(stat = "identity") +
  theme_minimal()
p2

### Correlation between EPC energy efficiency score and IMD score ###

IMD %>%
  filter(Measurement == "Score") %>% 
  filter(`Indices of Deprivation` == "a. Index of Multiple Deprivation (IMD)") -> IMD_S

IMD_S <- as.data.table(IMD_S)

epc_imdS <- merge(epc_lsoa, IMD_S[,c(1,5)], by.y ="GeographyCode", by.x="LSOA11CD")

epc_imdS %<>%
  group_by(LSOA11CD) %>% 
  mutate(mean_cee = round(mean(CURRENT_ENERGY_EFFICIENCY), digits = 2))
  

p3 <- ggplot(epc_imdS, aes(x=Value, y=mean_cee)) +
  geom_point(shape=0) +    # 0 = hollow squares
  geom_smooth(method=lm)   # Add linear regression line 
p3

mod_p3 <- lm(Value ~ mean_cee, epc_imdS)

### Geographically weighted regression

# Next steps stepwise regression and backward elimination using the OLS