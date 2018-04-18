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
source("R/functions/RemoveX.R") 
source("R/functions/WeatherTranspose.R") 



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
source("R/functions/WeatherTranspose.R") 

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

write.csv(lhz_tz_day1rain,"data/clean/weather/lhz_tz_day1rain.csv")

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

write.csv(clust_tz_day1rain,"data/clean/weather/clust_tz_day1rain.csv")



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

write.csv(lhz_mw_day1rain,"data/clean/weather/lhz_mw_day1rain.csv")

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

write.csv(clust_mw_day1rain,"data/clean/weather/clust_mw_day1rain.csv")
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


write.csv(maxdaysnorain_lhz_tz,"data/clean/weather/maxdaysnorain_lhz_tz.csv")

maxdaysnorain_clust_tz = 
  rainlist_cropyear[[3]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # indicate that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(date[.==1])))   # count the days with 0 rain

maxdaysnorain_clust_tz = maxdaysnorain_clust_tz %>% dplyr::select(-Date)

write.csv(maxdaysnorain_clust_tz,"data/clean/weather/maxdaysnorain_clust_tz.csv")


maxdaysnorain_lhz_mw = 
  rainlist_cropyear[[1]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # 1 indicates that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(date[.==1])))   # count the days with 0 rain


write.csv(maxdaysnorain_lhz_mw,"data/clean/weather/maxdaysnorain_lhz_mw.csv")

maxdaysnorain_clust_mw = 
  rainlist_cropyear[[4]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # indicate that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(date[.==1])))   # count the days with 0 rain

maxdaysnorain_clust_mw = maxdaysnorain_clust_mw %>% dplyr::select(-Date)

write.csv(maxdaysnorain_clust_mw,"data/clean/weather/maxdaysnorain_clust_mw.csv")


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

write.csv(rain_cytot_lhz_tz,"data/clean/weather/rain_cytot_lhz_tz.csv")

rain_cytot_clust_tz = 
  rainlist_cropyear[[3]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-Date,cropyear,-year,-month) %>%
  dplyr::summarise_all(funs(sum))   # count the total rain 

write.csv(rain_cytot_clust_tz,"data/clean/weather/rain_cytot_clust_tz.csv")

rain_cytot_lhz_mw = 
  rainlist_cropyear[[1]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::summarise_all(funs(sum))   # count the total rain 

write.csv(rain_cytot_lhz_mw,"data/clean/weather/rain_cytot_lhz_mw.csv")

rain_cytot_clust_mw = 
  rainlist_cropyear[[4]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-Date,cropyear,-year,-month) %>%
  dplyr::summarise_all(funs(sum))   # count the total rain 

write.csv(rain_cytot_clust_mw,"data/clean/weather/rain_cytot_clust_mw.csv")


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


write.csv(mw_tmean,"data/clean/weather/mw_tmean.csv")



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


write.csv(tz_tmean,"data/clean/weather/tz_tmean.csv")

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

write.csv(mw_gdd,"data/clean/weather/mw_gdd.csv")


tz_gdd = tz_tmean_full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<32,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(date[.==1])))   # count the days with 0 rain

write.csv(tz_gdd,"data/clean/weather/tz_gdd.csv")




#################################################################################
## merge the rainfall variable with cluster/ipc zone id and year 
#############################################################################


library(dplyr)
rain_cytot_clust_mw  = read.csv("data/clean/weather/rain_cytot_clust_mw.csv")
rain_cytot_clust_mw = rain_cytot_clust_mw %>% dplyr::select(-X,-X.1,-date)  # remove unneeded variables 

rain_cytot_clust_tz = read.csv("data/clean/weather/rain_cytot_clust_tz.csv")
rain_cytot_clust_tz = rain_cytot_clust_tz %>% dplyr::select(-X,-date)  # remove unneeded variables 

rain_cytot_lhz_mw = read.csv("data/clean/weather/rain_cytot_lhz_mw.csv")
rain_cytot_lhz_mw = rain_cytot_lhz_mw %>% dplyr::select(-X,-date)  # remove unneeded variables 


rain_cytot_lhz_tz = read.csv("data/clean/weather/rain_cytot_lhz_tz.csv")
rain_cytot_lhz_tz = rain_cytot_lhz_tz %>% dplyr::select(-X,-date)  # remove unneeded variables 


maxdaysnorain_clust_mw  = read.csv("data/clean/weather/maxdaysnorain_clust_mw.csv")
maxdaysnorain_clust_mw = maxdaysnorain_clust_mw %>% dplyr::select(-X,-X.1,-date)  # remove unneeded variables 

maxdaysnorain_clust_tz = read.csv("data/clean/weather/maxdaysnorain_clust_tz.csv")
maxdaysnorain_clust_tz = maxdaysnorain_clust_tz %>% dplyr::select(-X,-date)  # remove unneeded variables 

maxdaysnorain_lhz_mw = read.csv("data/clean/weather/maxdaysnorain_lhz_mw.csv")
maxdaysnorain_lhz_mw = maxdaysnorain_lhz_mw %>% dplyr::select(-X,-date)  # remove unneeded variables 


maxdaysnorain_lhz_tz = read.csv("data/clean/weather/maxdaysnorain_lhz_tz.csv")
maxdaysnorain_lhz_tz = maxdaysnorain_lhz_tz %>% dplyr::select(-X,-date)  # remove unneeded variables 


day1rain_clust_mw  = read.csv("data/clean/weather/clust_mw_day1rain.csv")
day1rain_clust_mw = day1rain_clust_mw %>% dplyr::select(-X,-X.1,-date)  # remove unneeded variables 


day1rain_clust_tz = read.csv("data/clean/weather/clust_tz_day1rain.csv")
day1rain_clust_tz = day1rain_clust_tz %>% dplyr::select(-X,-date)  # remove unneeded variables 


day1rain_lhz_mw = read.csv("data/clean/weather/lhz_mw_day1rain.csv")
day1rain_lhz_mw = day1rain_lhz_mw %>% dplyr::select(-X,-date)  # remove unneeded variables 

day1rain_lhz_tz = read.csv("data/clean/weather/lhz_tz_day1rain.csv")
day1rain_lhz_tz = day1rain_lhz_tz %>% dplyr::select(-X,-date)  # remove unneeded variables 

tz_gdd  = read.csv("data/clean/weather/tz_gdd.csv")
tz_gdd = tz_gdd %>% dplyr::select(-X,-date)  # remove unneeded variables 


tz_tmean = read.csv("data/clean/weather/tz_tmean.csv")
tz_tmean = tz_tmean %>% dplyr::select(-X)  # remove unneeded variables 

mw_gdd = read.csv("data/clean/weather/mw_gdd.csv")
mw_gdd = mw_gdd %>% dplyr::select(-X,-date)  # remove unneeded variables 

mw_tmean = read.csv("data/clean/weather/mw_tmean.csv")
mw_tmean = mw_tmean %>% dplyr::select(-X)  # remove unneeded variables 


source("R/functions/WeatherTranspose.R") 
mw_clust= list(rain_cytot_clust_mw,day1rain_clust_mw,maxdaysnorain_clust_mw)
tz_clust = list(rain_cytot_clust_tz,day1rain_clust_tz,maxdaysnorain_clust_tz)

mw_lhz = list(day1rain_lhz_mw, mw_gdd,mw_tmean,rain_cytot_lhz_mw,maxdaysnorain_lhz_mw)
tz_lhz  = list(day1rain_lhz_tz, tz_gdd,tz_tmean,rain_cytot_lhz_tz,maxdaysnorain_lhz_tz)

mw_clust_weather= lapply(mw_clust, WeatherTranspose)
tz_clust_weather= lapply(tz_clust, WeatherTranspose)
mw_lhz_weather= lapply(mw_lhz, WeatherTranspose)
tz_lhz_weather= lapply(tz_lhz, WeatherTranspose)

source("R/functions/RemoveX.R") 
mw_clust_weather= lapply(mw_clust_weather, RemoveX)
tz_clust_weather= lapply(tz_clust_weather, RemoveX) 

##generate dummy for flood susceptible areas
#################################################################################

# need a concordance table of  cluster id and ipczone from coord and 
mw_2010 = read.csv("data/clean/concordance/mw_concord_2010.csv")
mw_2010 = mw_2010 %>% distinct()

mw_2013 = read.csv("data/clean/concordance/mw_concord_2013.csv")
mw_2013 = mw_2013 %>% distinct()
mw_2013 = mw_2013[c("fnid","clust")]

mw_hh  = dplyr::bind_rows(mw_2010,mw_2013) %>% distinct() %>% mutate_all(funs(as.character))


tz_concordance <-  read.csv("data/clean/concordance/Tanzania_coord_lhz.csv")
tz_concordance =  tz_concordance %>% dplyr::select(ea_id,FNID)%>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))

colnames(mw_hh) = c("FNID","id")
colnames(tz_concordance)=c("id","FNID") 
colnames(ug_concordance)=c("id","FNID") 

source("R/functions/FnidV.R") 

fnid_to_v = read.csv("data/clean/concordance/FNID to V .csv")
colnames(fnid_to_v) = c("FNID","id")

mw_lhz_weather[[1]] = FnidV(mw_lhz_weather[[1]],fnid_to_v)
mw_lhz_weather[[4]] = FnidV(mw_lhz_weather[[4]],fnid_to_v)
mw_lhz_weather[[5]] = FnidV(mw_lhz_weather[[5]],fnid_to_v)


# MW2012C3020515 MW2012C3031005  MW2012C3031206

flood_mw_fnid = c("MW2012C3020515","MW2012C3031005","MW2012C3031206")

floodmax_lhz_mw = mw_lhz_weather[[4]] %>% dplyr::filter(!FNID %in% flood_mw_fnid)%>% mutate(value =0 )
floodmax_lhz_mw2 = mw_lhz_weather[[4]] %>% dplyr::filter(FNID %in% flood_mw_fnid)
floodmax_lhz_mw = dplyr::bind_rows(floodmax_lhz_mw,floodmax_lhz_mw2)
floodmax_lhz_mw["FS_year"] =floodmax_lhz_mw["cropyear"] 

mw_flood_ea = 
  mw_hh %>% dplyr::filter(FNID %in% flood_mw_fnid)  %>% dplyr::select(ea_id)
mw_flood_ea 

floodmax_clust_mw = mw_clust_weather[[1]] %>% dplyr::filter(!id %in% mw_flood_ea)%>% mutate(value =0 )
floodmax_clust_mw2 = mw_clust_weather[[1]] %>% dplyr::filter(id %in% mw_flood_ea)
floodmax_clust_mw = dplyr::bind_rows(floodmax_clust_mw2,floodmax_clust_mw)
floodmax_clust_mw["FS_year"] =floodmax_clust_mw["cropyear"] 

floodmax_lhz_mw2 = mw_lhz_weather[[4]] %>% dplyr::filter(FNID %in% flood_mw_fnid)
floodmax_lhz_mw = dplyr::bind_rows(floodmax_lhz_mw,floodmax_lhz_mw2)
floodmax_lhz_mw["FS_year"] =floodmax_lhz_mw["cropyear"] 


##### tanzania Livelihood)Zone)11)(Sisal‐sugar cane‐cattle)zone)
# Livelihood)Zone)6)(tree)crops‐fishing)coastal)zone)
#54: Mtwara-Lindi-Pwani Riverine Zone Livelihood Zone 
# 57: Mpanda Maize, Paddy, Sunflower, and Livestock Livelihood Zone 
#$ 59: Kyela Paddy, Cacao, and Palm Oil Lowlands Livelihood Zone 
# 64: Kamsamba Paddy, Sorghum, and Livestock Lowlands Livelihood Zone 
# 72: Mtera Dam Fishing, Sorghum, and Sesame Livelihood Zone 

#  TZ2009L106  TZ2009L111  TZ2009L154  TZ2009L157 TZ2009L159 TZ2009L164 TZ2009L172
flood_tz_fnid = c("TZ2009L106","TZ2009L111","TZ2009L154","TZ2009L157","TZ2009L159","TZ2009L172")

floodmax_lhz_tz = tz_lhz_weather[[4]] %>% dplyr::filter(!id %in% flood_tz_fnid)%>% mutate(value =0 )
floodmax_lhz_tz2 = tz_lhz_weather[[4]] %>% dplyr::filter(id %in% flood_tz_fnid)
floodmax_lhz_tz = dplyr::bind_rows(floodmax_lhz_tz,floodmax_lhz_tz2)
floodmax_lhz_tz["FS_year"] =floodmax_lhz_tz["cropyear"] 


tz_flood_ea = 
  tz_concordance %>% dplyr::filter(FNID %in% flood_tz_fnid)  %>% dplyr::select(ea_id)
tz_flood_ea_str = as.character(tz_flood_ea[,1])

floodmax_clust_tz = tz_clust_weather[[1]] %>% dplyr::filter(!id %in% tz_flood_ea_str)%>% mutate(value =0 )
floodmax_clust_tz2 = tz_clust_weather[[1]] %>% dplyr::filter(id %in% tz_flood_ea_str)
floodmax_clust_tz = dplyr::bind_rows(floodmax_clust_tz2,floodmax_clust_tz)
floodmax_clust_tz["FS_year"] =floodmax_clust_tz["cropyear"] 


write.csv(floodmax_clust_tz,"data/clean/weather/floodmax_clust_tz.csv")
write.csv(floodmax_lhz_tz,"data/clean/weather/floodmax_lhz_tz.csv")
write.csv(floodmax_lhz_mw,"data/clean/weather/floodmax_lhz_mw.csv")


 

#################################################################################
## merge the rainfall variable with cluster/ipc zone id and year 
#############################################################################

#mw_clust= list(rain_cytot_clust_mw,day1rain_clust_mw,maxdaysnorain_clust_mw)
#tz_clust = list(rain_cytot_clust_tz,day1rain_clust_tz,maxdaysnorain_clust_tz)
#mw_lhz = list(day1rain_lhz_mw, mw_gdd,mw_tmean,rain_cytot_lhz_mw,maxdaysnorain_lhz_mw)
# tz_lhz  = list(day1rain_lhz_tz, tz_gdd,tz_tmean,rain_cytot_lhz_tz,maxdaysnorain_lhz_tz)

# raincytot  day1rain  maxdays floodmax

colnames(mw_clust_weather[[1]])[3] = "raincytot"
colnames(mw_clust_weather[[2]])[3] = "day1rain"
colnames(mw_clust_weather[[3]])[3] = "maxdaysnorain"
colnames(tz_clust_weather[[1]])[3] = "raincytot"
colnames(tz_clust_weather[[2]])[3] = "day1rain"
colnames(tz_clust_weather[[3]])[3] = "maxdaysnorain"

colnames(mw_lhz_weather[[1]])[2] = "lhz_day1rain"
colnames(mw_lhz_weather[[1]])[3] = "FS_year"

colnames(mw_lhz_weather[[2]])[1] = "FNID"
colnames(mw_lhz_weather[[2]])[3] = "lhz_growingdays"

colnames(mw_lhz_weather[[3]])[3] = "tmean"
colnames(mw_lhz_weather[[3]])[1] = "FNID"


colnames(mw_lhz_weather[[4]])[2] = "lhz_raincytot"
colnames(mw_lhz_weather[[4]])[3] = "FS_year"

colnames(mw_lhz_weather[[5]])[2] = "lhz_maxdaysnorain"
colnames(mw_lhz_weather[[5]])[3] = "FS_year"

colnames(tz_lhz_weather[[1]])[3] = "lhz_day1rain"
colnames(tz_lhz_weather[[1]])[1] = "FNID"



colnames(tz_lhz_weather[[2]])[3] = "lhz_growingdays"
colnames(tz_lhz_weather[[2]])[1] = "FNID"


colnames(tz_lhz_weather[[3]])[3] = "tmean"
colnames(tz_lhz_weather[[3]])[1] = "FNID"


colnames(tz_lhz_weather[[4]])[3] = "lhz_raincytot"
colnames(tz_lhz_weather[[4]])[1] = "FNID"


colnames(tz_lhz_weather[[5]])[3] = "lhz_maxdaysnorain"
colnames(tz_lhz_weather[[5]])[1] = "FNID"

colnames(floodmax_clust_mw)[2] = "floodmax"
colnames(floodmax_clust_mw)[3] = "FS_year"


colnames(floodmax_clust_tz)[3] = "floodmax"
colnames(floodmax_lhz_tz)[3] = "lhz_floodmax"

colnames(floodmax_lhz_tz)[1] = "FNID"

colnames(floodmax_lhz_mw)[2] = "lhz_floodmax"
colnames(floodmax_lhz_mw)[3] = "FS_year"


floodmax_clust_mw = floodmax_clust_mw %>% mutate(FS_year = as.numeric(FS_year) )
floodmax_clust_mw = floodmax_clust_mw %>% select(-cropyear)

floodmax_lhz_mw = floodmax_lhz_mw %>% select(-cropyear)
floodmax_lhz_mw = floodmax_lhz_mw %>% mutate(FS_year = as.numeric(FS_year),FNID  = as.character(FNID) )

floodmax_lhz_tz = floodmax_lhz_tz %>% select(-cropyear)
floodmax_lhz_tz = floodmax_lhz_tz %>% mutate(FS_year = as.numeric(FS_year),FNID  = as.character(FNID) )


floodmax_clust_tz = floodmax_clust_tz %>% mutate(FS_year = as.numeric(FS_year) )
floodmax_clust_tz = floodmax_clust_tz %>% select(-cropyear)


mw =   left_join(mw_clust_weather[[1]],mw_clust_weather[[2]] )
mw =   left_join(mw, mw_clust_weather[[3]])
mw =   left_join(mw, mw_hh,by = "id")
mw =   left_join(mw, floodmax_clust_mw,by = c("id","FS_year"))

tz_concordance %>% distinct()
  
mw_lhz =   left_join(mw_lhz_weather[[2]], mw_lhz_weather[[1]])
mw_lhz =   left_join(mw_lhz, mw_lhz_weather[[3]])
mw_lhz =   left_join(mw_lhz, mw_lhz_weather[[4]])
mw_lhz =   left_join(mw_lhz, mw_lhz_weather[[5]])
mw_lhz =   left_join(mw_lhz, floodmax_lhz_mw,by = c("FNID","FS_year"))


mw_weather_final = left_join(mw, mw_lhz)

write.csv(mw_weather_final,"data/clean/weather/mw_weather_final.csv")


class(tz_concordance[,2])

tz =   left_join(tz_clust_weather[[1]],tz_clust_weather[[2]] )
tz =   left_join(tz, tz_clust_weather[[3]])
tz =   left_join(tz, floodmax_clust_tz,by = c("id","FS_year"))

tz =   left_join(tz, tz_concordance)

tz_lhz =   left_join(tz_lhz_weather[[2]], tz_lhz_weather[[1]])
tz_lhz =   left_join(tz_lhz, tz_lhz_weather[[3]])
tz_lhz =   left_join(tz_lhz, tz_lhz_weather[[4]])
tz_lhz =   left_join(tz_lhz, tz_lhz_weather[[5]])
tz_lhz =   left_join(tz_lhz, floodmax_lhz_tz,by = c("FNID","FS_year"))





  
mw =   left_join(mw, mw_clust_weather[[3]])



#################################################################################

  