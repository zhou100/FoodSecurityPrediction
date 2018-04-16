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
# 0. day1rain, when the first day of rain comes (both lhz and cluster level) 
# 1. total rainfalls in growing seasons  （Growing season total precipitation）
# 2. Number of max no rain days in a crop year
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

package = c("dplyr","zoo","rgeos", "rgdal", "raster")
lapply(package, require, character.only = TRUE)

source("R/functions/CropYear.R") 
 


precip_lhz_tz <- read.csv("data/raw/rain/CHIRPS_tz_lhz.csv")
precip_lhz_ug <- read.csv("data/raw/rain/CHIRPS_ug_lhz.csv")


precip_clust_tz <- read.csv("D:/CHIRPS_tan_buffer.csv")
precip_clust_ug <- read.csv("D:/CHIRPS_ug_buffer.csv")

rainlist = list(precip_lhz_ug,precip_lhz_tz,precip_clust_tz,precip_clust_ug)

# generate the year and month variable from date 
date= as.Date(precip_lhz_ug$date_list,"%m/%d/%Y")


# 1. generate crop year, so that it's summing up by crop year 
source("R/functions/CropYear.R") 

# generate cropyear 
rainlist_cropyear = lapply(rainlist, function(x){CropYear(x,date)})
########################################################################  
# generate the first date of rain after October for Tanzania and After April for Uganda 
# day1rain "the number of days after Oct 1 where five-day rainfall > 10 and it rained at least 3/5 days"
######################################################################## 


# moving mean for that day and previous days (e.g. 5 represents the mean of that day and the for previous days)
library(lubridate)

####livelihood level day1rain for tanzania ###########

precip_lhz_tz_rain = rainlist_cropyear[[2]]%>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "right", fill = NA))) %>%
  na.omit()

lhz_tz_raindate=
  precip_lhz_tz_rain %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(date[.>2.2])))

lhz_tz_day1rain = 
lhz_tz_raindate %>%
  dplyr::select(-cropyear)%>%
  dplyr::mutate_all(funs(interval(.,date))) %>%
  dplyr::mutate_all(funs(as.duration(.))) %>%
  dplyr::mutate_all(funs(as.numeric(.,"days"))) %>%
  dplyr::mutate_all(funs(.*-1)) 
lhz_tz_day1rain$cropyear = lhz_tz_raindate$cropyear

write.csv(lhz_tz_day1rain,"data/clean/lhz_tz_day1rain.csv")

####cluster level day1rain for tanzania ###########

precip_clust_tz_rain = rainlist_cropyear[[3]]%>%
  dplyr::select(-Date) %>% 
  dplyr::group_by(cropyear) %>%  # by cropyear
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "right", fill = NA))) %>%      # generate a 5 days moving median of rain
  na.omit()

clust_tz_raindate=
  precip_clust_tz_rain %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(date[.>2.2])))   # first day that has a moving median >2.2


# generate the days from the beginning of the rainy season to the first day of rain
clust_tz_day1rain = 
  clust_tz_raindate %>%
  dplyr::select(-cropyear)%>%
  dplyr::mutate_all(funs(interval(.,date))) %>%                 # difference betweeen the starting date of the rainy season and first day of rain
  dplyr::mutate_all(funs(as.duration(.))) %>%
  dplyr::mutate_all(funs(as.numeric(.,"days"))) %>%
  dplyr::mutate_all(funs(.*-1)) 

clust_tz_day1rain$cropyear = clust_tz_raindate$cropyear

write.csv(clust_tz_day1rain,"data/clean/clust_tz_day1rain.csv")

#################################################################################
#### generate maxdaysno rain 
########### longest dry spell during the rainy season (Oct-Mar) per crop year (May-Apr)"
#################################################################################

maxdaysnorain_lhz_tz = 
  rainlist_cropyear[[2]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # 1 indicates that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(date[.==1])))   # count the days with 0 rain


write.csv(maxdaysnorain_lhz_tz,"data/clean/maxdaysnorain_lhz_tz.csv")

maxdaysnorain_clust_tz = 
  rainlist_cropyear[[3]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # indicate that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(date[.==1])))   # count the days with 0 rain

maxdaysnorain_clust_tz = maxdaysnorain_clust_tz %>% dplyr::select(-Date)

write.csv(maxdaysnorain_clust_tz,"data/clean/maxdaysnorain_clust_tz.csv")


#################################################################################
#### generate rain_cytot
###########  "total rainfall from Oct to Apr by ipczone and cropyear"
#################################################################################
rain_cytot_lhz_tz = 
rainlist_cropyear[[2]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::summarise_all(funs(sum))   # count the total rain 

write.csv(rain_cytot_lhz_tz,"data/clean/rain_cytot_lhz_tz.csv")

rain_cytot_clust_tz = 
  rainlist_cropyear[[3]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-Date,cropyear,-year,-month) %>%
  dplyr::summarise_all(funs(sum))   # count the total rain 

write.csv(rain_cytot_clust_tz,"data/clean/rain_cytot_clust_tz.csv")


#################################################################################
#### generate growing degree days 
########### 
#################################################################################

mw_tmin <- read.csv("data/clean/temperature/mw_daily_tmin.csv")
mw_tmax <- read.csv("data/clean/temperature/mw_daily_tmax.csv")

date =  mw_tmax["date1"] 
mw_tmean = (mw_tmax[-1] + mw_tmin[-1])/2 -273.15



yearmon = as.yearmon(as.Date(date[,1],"%m/%d/%Y"))


rainlist_cropyear[1]  %>%
  group_by(cropyear) %>%
  dplyr::mutate_all(funs(lag(.,n=12,order_by = date)))





#################################################################################
## merge the rainfall variable with cluster/ipc zone id 
###############################################################


#################################################################################
##generate dummy for flood susceptible areas
#################################################################################
#gen area_flood=1 if ipczone==4| ipczone==35|ipczone==40
 # gen area_fish=1 if ipczone==40


  