##################################################################
# Goal : This script aims to clean up the rainfall and temperature data，
# 
# purpose: generate variables like total rainfalls, rainfall in areas that are prone to flood, first of rain

# Input : 
# 1. daily rainfall extracted from CHIRPS (using script rainfall_daily_cluster.R and cluster buffer shapefiles)
# 2. daily temperture 
# 3. coordinates of clusters. 
# 4. shapefile of livelihood zones. 

# Output: 
# 0. growing degree days (both lhz and cluster level) 
# 1. total rainfalls in growing seasons  （Growing season total precipitation）
# 2. Number of no rain days    
# 3.   GDD - number of days where temp was between 8 to 32 C (Tmax + Tmin)/2
# 4. Schlenker & Roberts (2009), number and percentage of days in a set number of quantile bins (per-bin deviations from long-run in z-scores)
# 5. Deschênes and Greenstone (2007) yiled on weather 


################################
# data to get in the future 
#########################################
# 6. soil quality：Soil characteristics: K factor, slope lenghth, fraction irrigated , moisture capacity, salinity 
# 7. NDVI 
# 
# Yujun Zhou -  04/11/18
###################################################################

