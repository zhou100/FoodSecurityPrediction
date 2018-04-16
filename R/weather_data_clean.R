##################################################################
# Goal : This script aims to clean up the rainfall and temperature data，
# 
# purpose: generate variables day1rain,  total rainfall in growing season , rainfall in areas that are prone to flood for Tanzania and Malawi 
# haven't generated the ones for Uganda, because it has two rainy seasons 
# link the weather variable to livelihood zone id and cluster id .

# Input : 
# 1. daily rainfall extracted from CHIRPS (using script rainfall_daily_cluster.R and cluster buffer shapefiles)
# 2. daily temperture data from African Drought Monitor
# 3. coordinates of clusters. 
# 4. shapefile of livelihood zones. 

# Output: 
# 0. day1rain, when the first day of rain comes (both lhz and cluster level) 
# 1. total rainfalls in growing seasons  （Growing season total precipitation）
# 2. Number of max no rain days in a crop year
# 3.
#  GDD - number of days where temp was between 8 to 32 C (Tmax + Tmin)/2 . Deschênes and Greenstone (2007) yield  on weather 

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
precip_lhz_mw <- read.csv("data/raw/rain/chirps_daily_0716.csv")
precip_lhz_mw$Date = as.Date(precip_lhz_mw$Date)

precip_lhz_mw = precip_lhz_mw %>% filter(Date>="2008-01-01")
date_mw = precip_lhz_mw$Date



precip_lhz_mw = precip_lhz_mw %>% dplyr::select(-X,-Date,-month)

mw_tmin <- read.csv("data/raw/temperature/mw_daily_tmin.csv")
colnames(precip_lhz_mw_new) = colnames(mw_tmin)[2:(ncol(mw_tmin)-1)]

precip_lhz_mw$date = date_mw

precip_clust_tz <- read.csv("D:/CHIRPS_tan_buffer.csv")
precip_clust_ug <- read.csv("D:/CHIRPS_ug_buffer.csv")
precip_clust_mw <- read.csv("data/raw/rain/CHIRPS_malawi_cluster.csv")


rainlist = list(precip_lhz_mw,precip_lhz_tz,precip_clust_tz,precip_clust_mw)

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



precip_lhz_mw_rain = rainlist_cropyear[[1]]%>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "right", fill = NA))) %>%
  na.omit()

lhz_mw_raindate=
  precip_lhz_mw_rain %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(date[.>2.2])))

lhz_mw_day1rain = 
  lhz_mw_raindate %>%
  dplyr::select(-cropyear)%>%
  dplyr::mutate_all(funs(interval(.,date))) %>%
  dplyr::mutate_all(funs(as.duration(.))) %>%
  dplyr::mutate_all(funs(as.numeric(.,"days"))) %>%
  dplyr::mutate_all(funs(.*-1)) 
lhz_mw_day1rain$cropyear = lhz_mw_raindate$cropyear

write.csv(lhz_mw_day1rain,"data/clean/lhz_mw_day1rain.csv")

####cluster level day1rain for malawi ###########

precip_clust_mw_rain = rainlist_cropyear[[4]]%>%
  dplyr::select(-Date) %>% 
  dplyr::group_by(cropyear) %>%  # by cropyear
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "right", fill = NA))) %>%      # generate a 5 days moving median of rain
  na.omit()

clust_mw_raindate=
  precip_clust_mw_rain %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(date[.>2.2])))   # first day that has a moving median >2.2


# generate the days from the beginning of the rainy season to the first day of rain
clust_mw_day1rain = 
  clust_mw_raindate %>%
  dplyr::select(-cropyear)%>%
  dplyr::mutate_all(funs(interval(.,date))) %>%                 # difference betweeen the starting date of the rainy season and first day of rain
  dplyr::mutate_all(funs(as.duration(.))) %>%
  dplyr::mutate_all(funs(as.numeric(.,"days"))) %>%
  dplyr::mutate_all(funs(.*-1)) 

clust_mw_day1rain$cropyear = clust_mw_raindate$cropyear

write.csv(clust_mw_day1rain,"data/clean/clust_mw_day1rain.csv")
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


maxdaysnorain_lhz_mw = 
  rainlist_cropyear[[1]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # 1 indicates that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(date[.==1])))   # count the days with 0 rain


write.csv(maxdaysnorain_lhz_mw,"data/clean/maxdaysnorain_lhz_mw.csv")

maxdaysnorain_clust_mw = 
  rainlist_cropyear[[4]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # indicate that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(date[.==1])))   # count the days with 0 rain

maxdaysnorain_clust_mw = maxdaysnorain_clust_mw %>% dplyr::select(-Date)

write.csv(maxdaysnorain_clust_mw,"data/clean/maxdaysnorain_clust_mw.csv")


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

rain_cytot_lhz_mw = 
  rainlist_cropyear[[1]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::summarise_all(funs(sum))   # count the total rain 

write.csv(rain_cytot_lhz_mw,"data/clean/rain_cytot_lhz_mw.csv")

rain_cytot_clust_mw = 
  rainlist_cropyear[[4]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-Date,cropyear,-year,-month) %>%
  dplyr::summarise_all(funs(sum))   # count the total rain 

write.csv(rain_cytot_clust_mw,"data/clean/rain_cytot_clust_mw.csv")


#################################################################################
#### generate mean temperature in the growing season   
#################################################################################

mw_tmin <- read.csv("data/raw/temperature/mw_daily_tmin.csv")
mw_tmax <- read.csv("data/raw/temperature/mw_daily_tmax.csv")

date =as.Date(mw_tmax$date1,"%m/%d/%Y")

mw_tmin_cy= CropYear(mw_tmin,date)
mw_tmax_cy= CropYear(mw_tmax,date)

cropyear = mw_tmin_cy$cropyear 
cropyear_date = mw_tmin_cy$date

mw_tmin_cy = mw_tmin_cy %>% dplyr::select(-date1,-X,-month,-year,-date)
mw_tmax_cy = mw_tmax_cy %>% dplyr::select(-date1,-X,-month,-year,-date)


mw_tmean_full = (mw_tmax_cy + mw_tmin_cy)/2 -273.15


mw_tmean_full$cropyear = cropyear
mw_tmean_full$date = cropyear_date

mw_tmean = 
  
  mw_tmean_full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-date) %>%
  dplyr::summarise_all(funs(mean))   # generate the mean temperature by year by ipczone


write.csv(mw_tmean,"data/clean/mw_tmean.csv")



tz_tmin <- read.csv("data/raw/temperature/tz_daily_tmin.csv")
tz_tmax <- read.csv("data/raw/temperature/tz_daily_tmax.csv")

date =as.Date(tz_tmax$date1,"%m/%d/%Y")

tz_tmin_cy= CropYear(tz_tmin,date)
tz_tmax_cy= CropYear(tz_tmax,date)

cropyear = tz_tmin_cy$cropyear 
cropyear_date = tz_tmin_cy$date

tz_tmin_cy = tz_tmin_cy %>% dplyr::select(-date1,-X,-month,-year,-date)
tz_tmax_cy = tz_tmax_cy %>% dplyr::select(-date1,-month,-year,-date)


tz_tmean_full = (tz_tmax_cy + tz_tmin_cy)/2 -273.15


tz_tmean_full$cropyear = cropyear
tz_tmean_full$date = cropyear_date

tz_tmean = 
  
  tz_tmean_full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-date) %>%
  dplyr::summarise_all(funs(mean))   # generate the mean temperature by year by ipczone


write.csv(tz_tmean,"data/clean/tz_tmean.csv")

#################################################################################
#### generate growing degree days  
###########  number of days where temp was between 8 to 32 C (Tmax + Tmin)/2 . Deschênes and Greenstone (2007) yield  on weather 
#################################################################################


mw_gdd = mw_tmean_full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<32,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(date[.==1])))   # count the days with 0 rain

write.csv(mw_gdd,"data/clean/mw_gdd.csv")


tz_gdd = tz_tmean_full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<32,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(date[.==1])))   # count the days with 0 rain

write.csv(tz_gdd,"data/clean/tz_gdd.csv")


#################################################################################
## merge the rainfall variable with cluster/ipc zone id 
###############################################################

rainlist_cropyear[1]  %>%
  group_by(cropyear) %>%
  dplyr::mutate_all(funs(lag(.,n=12,order_by = date)))


##generate dummy for flood susceptible areas
#################################################################################
#gen area_flood=1 if ipczone==4| ipczone==35|ipczone==40
 # gen area_fish=1 if ipczone==40



##### tanzania Livelihood)Zone)11)(Sisal‐sugar cane‐cattle)zone)
# Livelihood)Zone)6)(tree)crops‐fishing)coastal)zone)
#54: Mtwara-Lindi-Pwani Riverine Zone Livelihood Zone 
# 57: Mpanda Maize, Paddy, Sunflower, and Livestock Livelihood Zone 
#$ 59: Kyela Paddy, Cacao, and Palm Oil Lowlands Livelihood Zone 
# 64: Kamsamba Paddy, Sorghum, and Livestock Lowlands Livelihood Zone 
# 72: Mtera Dam Fishing, Sorghum, and Sesame Livelihood Zone 

colnames(tz_gdd)
#################################################################################

  