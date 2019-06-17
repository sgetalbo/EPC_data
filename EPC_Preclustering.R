#Prerequisite to running this script - EPC_data_wide

#===========================================================================
# Packages and data
#===========================================================================


library(data.table)
library(e1071)
library(dplyr)
library(Hmisc)
library(magrittr)
library(tibble)
library(bestNormalize)
library(kader)
library(corrplot)

### The data structure prior to clustering is typically a table whereby rows represent small geographic areas and columns represent attribute information on which they will be clustered. 
    ### This is now true 

# EPC join to census

census <- fread("./05_The_EUC/01_New_data/weighted_census_props.csv", drop = 1)
epc <- fread("./05_The_EUC/01_New_data/epc_proportions_wide.csv")

epc_census <- merge(epc, census, by="PCS", all.x=TRUE, na.rm=TRUE)


#===========================================================================
# Missing data
#===========================================================================


# Handling missing data

epc_census %>% summarize_all(funs(sum(is.na(.)) / length(.))) * 100 -> missing_data_w

MDw <- t(missing_data_w) # output this table and examine to inform which variables are removed due to missingness.

# Removed following variables due to being over 80% missing 
#ENERGY_CONSUMPTION_CURRENT
#LIGHTING_COST_CURRENT
#HEATING_COST_CURRENT
#HOT_WATER_COST_CURRENT
#FLAT_STOREY_COUNT
#EXTENSION_COUNT
#NUMBER_HABITABLE_ROOMS
#Total_headcount
#1_ROOM
#2_ROOMS
#3_ROOMS
#4_ROOMS
#5_ROOMS
#6_ROOMS
#7_ROOMS
#8_ROOMS
#9_ORMORE_ROOMS

# Also remove total headcount - not relevent
epc_census <- epc_census[, -c(4, 5, 6, 7, 9, 10, 11, 107, 149:157)]


#===========================================================================
# Skewness and correlation
#===========================================================================

### Correlation / distribution / skew and relationships between variables

# Skewness

skew <- apply(epc_census[,c(2:159)],2, function(x) skewness(x, na.rm = TRUE))

skew <- as.data.frame(skew) # output this table and examine to inform transformation methods

skew %<>% 
  rownames_to_column('vars')

skew %>%
  top_n(10) -> skew_top10

fwrite(skew, "./05_The_EUC/01_New_data/non_transformed_skewness_table.csv")

# summary table - number of varibles highly/moderately/symetric

skew_summary <- as.data.frame(table(cut(skew[,2], c(-5, -1, -0.5, 0.5, 1, 95), include.lowest=TRUE)))

skew_summary$prop <- skew_summary$Freq/nrow(skew)*100

fwrite(skew_summary, "./05_The_EUC/01_New_data/skew_summary.csv")

### Changes based on evaluation of skewness of variables 

# Correlation matrix 
source("http://www.sthda.com/upload/rquery_cormat.r")

corr_res_table <- rquery.cormat(epc_census[,2:159], type="flatten", graph=FALSE)
corr_res_df <- as.data.frame(corr_res_table$r)

fwrite(corr_res_df, "./05_The_EUC/01_New_data/non_transformed_correlation_table.csv")

### Changes based on evaluation of correlation of variables 

# Flat top storey and # Solar water heating flag - very highly correlated with photo_flag because 
#  solar panels have to be available to provide solar heating. Remove SWHF as appears in so few results.
epc_census[,c(41:43,86:88)]<- NULL


corr_res_table2 <- rquery.cormat(epc_census[,2:153], type="flatten", graph=FALSE)
corr_res_df2 <- as.data.frame(corr_res_table2$r)

fwrite(corr_res_df2, "./05_The_EUC/01_New_data/non_transformed_correlation_table2.csv")

#===========================================================================
# Transformation and normalisation
#===========================================================================

# apply these transformations to skewed data - the only variable not skewed is ENVIROMENT_IMPACT_CURRENT so this has been left out. 

# log10 normalisation

log10_transforamtion <- apply(epc_census[,c(2, 4:153)],2, function(x) log10(x +10))
log10_transforamtion <- as.data.frame(log10_transforamtion)


# Cube root transformations 
# it is impossible to hard code cube root within apply because of operator precidence.

cube_root_transformation <- apply(epc_census[,c(2, 4:153)], 2, function(x) kader:::cuberoot(x))
cube_root_transformation <- as.data.frame(cube_root_transformation)

# Box cox normalisation

boxcox_test <- apply(epc_census[,c(2, 4:153)], 2, function(x) forecast::BoxCox(x, lambda = "auto"))
boxcox_test <- as.data.frame(boxcox_test)

# Comparison

log10_skew <- apply(log10_transforamtion,2, function(x) skewness(x, na.rm = TRUE))
log10_skew <- as.data.frame(log10_skew) # output this table and examine to inform transformation methods

cuberoot_skew <- apply(cube_root_transformation, 2, function(x) skewness(x, na.rm = TRUE))
cuberoot_skew <- as.data.frame(cuberoot_skew)

boxcox_skew <- apply(boxcox_test,2, function(x) skewness(x, na.rm = TRUE))
boxcox_skew <- as.data.frame(boxcox_skew)

skew_tests <- cbind(log10_skew, cuberoot_skew, boxcox_skew)

skew_tests %<>% 
  rownames_to_column('vars') 

skew_comparison <- merge(skew_top10, skew_tests, by="vars", all.x=TRUE)
fwrite(skew_comparison, "./05_The_EUC/01_New_data/skew_comparison.csv")

# Take natural distributions and boxcox distribution datasets out for use in the clustering 
# algorithm to see how the normalisation affects cluster generation. 

# non-transformed distribution

fwrite(epc_census, "./05_The_EUC/01_New_data/nontransformed_data.csv")

# BoxCox distirbution 

#add back in postcode sector and environmental efficiency
boxcox <- cbind(epc_census[,c(1,3)], boxcox_test)

fwrite(boxcox, "./05_The_EUC/01_New_data/boxcox_distribution.csv")

# Q-Q plot or historgram for illustrative comparison - perhaps not.

hist(epc_census$WTC_TRUE, breaks = 100)
hist(boxcox$WTC_TRUE, breaks = 100)

#===========================================================================
# Z-Score standardisation
#===========================================================================

# Transformation 

epc_zscores <- apply(epc_census[,c(2:153)], 2, function(x) scale(x))
epc_zscores <- cbind(epc_census[,1], epc_zscores)
fwrite(epc_zscores, "./05_The_EUC/01_New_data/epc_zscores.csv")

boxcox_zscores <- apply(boxcox[,c(2:153)], 2, function(x) scale(x))
boxcox_zscores <- cbind(epc_census[,1], boxcox_zscores)
fwrite(boxcox_zscores, "./05_The_EUC/01_New_data/boxcox_zscores.csv")





