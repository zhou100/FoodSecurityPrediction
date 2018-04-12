##################################################################
# Goal : This script aims to clean up the rainfall and temperature data，
# 
# purpose: generate variables like total rainfalls, rainfall in areas that are prone to flood, first of rain

# Input : 
# 1. daily rainfall extracted from CHIRPS (using script rainfall_daily_cluster.R and cluster buffer shapefiles)
# 2. daily temperture data from African Drought Monitor
# 3. coordinates of clusters. 
# 4. shapefile of livelihood zones. 

# Output: 
# 0. growing degree days (both lhz and cluster level) 
# 1. total rainfalls in growing seasons  （Growing season total precipitation）
# 2. Number of no rain days    
# 3.   GDD - number of days where temp was between 8 to 32 C (Tmax + Tmin)/2 . Deschênes and Greenstone (2007) yield  on weather 

################################
# data to get in the future 
#########################################
# 4. Schlenker & Roberts (2009), number and percentage of days in a set number of quantile bins (per-bin deviations from long-run in z-scores)

# 5.5 temperatue bins  Deschenes and Greenstone (2011) (<10, 20-25, etc. )
# 6. soil quality：Soil characteristics: K factor, slope lenghth, fraction irrigated , moisture capacity, salinity 
# 7. NDVI 
# 
# Yujun Zhou -  04/11/18
###################################################################

######################################
### first day of rain for the rainy season (since October month >=10) for each livelihood zones 
### should there be a threshold other than 0 ?
######################################

precip_lhz_tz <- read.csv("data/raw/rain/CHIRPS_tz_lhz.csv")
precip_lhz_ug <- read.csv("data/raw/rain/CHIRPS_ug_lhz.csv")


precip_clust_tz <- read.csv("D:/CHIRPS_tan_buffer.csv")
precip_clust_ug <- read.csv("D:/CHIRPS_ug_buffer.csv")


# generate the year and month variable from date 
date= as.Date(precip_lhz_ug$date_list,"%m/%d/%Y")
month = strftime(date,"%m")
month = as.numeric(month)
year = strftime(date,"%Y")

precip_lhz_tz["year"] = year
precip_lhz_tz["date"] = date
precip_lhz_tz["month"] = month

precip_lhz_ug["year"] = year
precip_lhz_ug["date"] = date
precip_lhz_ug["month"] = month

precip_clust_ug["year"] = year
precip_clust_ug["date"] = date
precip_clust_ug["month"] = month

precip_clust_tz["year"] = year
precip_clust_tz["date"] = date
precip_clust_tz["month"] = month

########################################################################  
# generate the first date of rain after October month >=10 each year 
######################################################################## 

day1rain_lhz_tz = precip_lhz_tz %>% 
  dplyr::filter(month>9) %>%  # planting/growing season starts from oct
  dplyr::group_by(year) %>%   # calculate for each year individually 
  dplyr::summarise_all(funs(first(date[.>0])))  # first date that has rainfall >0 

day1rain_clust_tz = precip_clust_tz %>% 
  dplyr::filter(month>9) %>%  # planting/growing season starts from oct
  dplyr::group_by(year) %>%   # calculate for each year individually 
  dplyr::summarise_all(funs(first(date[.>0])))  # first date that has rainfall >0 


 
day1rain_lhz_ug = precip_lhz_ug %>% 
  dplyr::filter(month>3) %>%  # planting/growing season starts from oct
  dplyr::group_by(year) %>%   # calculate for each year individually
  dplyr::summarise_all(funs(first(date[.>0])))  # first date that has rainfall >0 



day1rain_clust_ug = precip_clust_ug %>% 
  dplyr::filter(month>3) %>%  # planting/growing season starts from oct
  dplyr::group_by(year) %>%   # calculate for each year individually
  dplyr::summarise_all(funs(first(date[.>0])))  # first date that has rainfall >0 


