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


rm(list=ls())

package = c("dplyr","zoo","rgeos", "rgdal", "raster","lubridate")
lapply(package, require, character.only = TRUE)

source("R/functions/CropYearTZMW.R") 
source("R/functions/CropYearUG.R") 
source("R/functions/RemoveX.R") 
source("R/functions/WeatherTranspose.R") 


##################################################################################
# load the lhz weather variables 
##################################################################################

precip_lhz_tz <- read.csv("data/raw/rain/CHIRPS_tz_lhz.csv")
precip_lhz_ug <- read.csv("data/raw/rain/CHIRPS_ug_lhz.csv")
precip_lhz_mw <- read.csv("data/raw/rain/chirps_daily_0716.csv")

# colnames(precip_lhz_tz)
# colnames(precip_lhz_ug)
 #colnames(precip_lhz_mw)


# nrow(precip_lhz_tz)
# nrow(precip_lhz_ug)

date.ug = as.Date(precip_lhz_ug$date_list,"%m/%d/%Y")

precip_lhz_ug = precip_lhz_ug %>% dplyr::select (-date_list)
precip_lhz_tz["Date"] = date.ug
precip_lhz_ug["Date"] = date.ug

precip_lhz_mw = precip_lhz_mw %>% mutate(Date =as.Date(Date) ) %>% dplyr::select(-X,-month)

# the mw lhz colnames  are faulty, load the ones from the temperature data
mw_tmin <- read.csv("data/raw/temperature/mw_daily_tmin.csv")
colnames(precip_lhz_mw)[2:61] = colnames(mw_tmin)[2:(ncol(mw_tmin)-1)]


##################################################################################
# load the cluster weather variables 
##################################################################################
load("data/raw/rain/precip_clust_tz.rda")
load("data/raw/rain/CHIRPS_ug_buffer.rda")
load("data/raw/rain/CHIRPS_malawi_cluster.rda")

precip_clust_mw[["Date"]] = as.Date(precip_clust_mw$Date,"%m/%d/%Y")
#colnames(precip_clust_mw)
precip_clust_mw = precip_clust_mw %>% dplyr::select(-X)

precip_clust_ug[["Date"]] = as.Date(precip_clust_ug$Date,"%m/%d/%Y")
precip_clust_ug = precip_clust_ug %>% dplyr::select(-X)

#colnames(precip_clust_tz)[2]
precip_clust_tz[["Date"]] = as.Date(precip_clust_tz$Date,"%m/%d/%Y")
class(precip_clust_tz$Date)

rain.TZMW.list = list(precip_lhz_tz,precip_lhz_mw,precip_clust_tz,precip_clust_mw)
rain.UG.list = list(precip_lhz_ug,precip_clust_ug)

# generate the year and month variable from date 


# 1. generate crop year, so that it's summing up by crop year 
source("R/functions/CropYearTZMW.R") 
source("R/functions/CropYearUG.R") 

#lapply(rainlist,function(x){colnames(x)})

# generate cropyear 
rain.TZMW.cropyear = lapply(rain.TZMW.list, function(x){CropYearTZMW(x)})
rain.UG.cropyear = lapply(rain.UG.list, function(x){CropYearUG(x)})


save(rain.TZMW.cropyear, file = "data/raw/rain/TZMW_rain_cropyear.rda")
save(rain.UG.cropyear, file = "data/raw/rain/UG_rain_cropyear.rda")


############################################################################
### first day of rain for the rainy season (since October or month >=10) for each livelihood zones 
### should there be a threshold other than 0 ?
############################################################################

########################################################################  
# generate the first date of rain after October for Tanzania and After April for Uganda 
# day1rain "the number of days after Oct 1 where five-day rainfall > 10 and it rained at least 3/5 days"
######################################################################## 

rm(list=ls())
load("data/raw/rain/TZMW_rain_cropyear.rda")
load("data/raw/rain/UG_rain_cropyear.rda")
# moving mean for that day and previous days (e.g. 5 represents the mean of that day and the for previous days)

####livelihood level day1rain for tanzania ###########

# rain.TZMW.list = list(precip_lhz_tz,precip_lhz_mw,precip_clust_tz,precip_clust_mw)
# rain.UG.list = list(precip_lhz_ug,precip_clust_ug)

precip.lhz.tz.rain = rain.TZMW.cropyear[[1]]%>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "right", fill = NA))) %>%
  na.omit()

lhz.tz.day1rain=
  precip.lhz.tz.rain %>%
  dplyr::select(-Date,everything()) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(which(.>2.2))+2)) %>% 
  dplyr::select(-Date)

lhz.tz.day1rain[is.na(lhz.tz.day1rain)]=156




####livelihood level day1rain for Uganda ###########
precip.lhz.ug.rain = rain.UG.cropyear[[1]]%>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "left", fill = NA))) %>%
  na.omit()

#colnames(precip.lhz.mw.rain)[3]

lhz.ug.day1rain=
  precip.lhz.mw.rain %>%
  dplyr::select(-Date,everything()) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(which(.>2.2))+2)) %>% 
  dplyr::select(-Date)


####livelihood level day1rain for malawi ###########
precip.lhz.mw.rain = rain.TZMW.cropyear[[2]]%>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "left", fill = NA))) %>%
  na.omit()

lhz.mw.day1rain=
  precip.lhz.mw.rain %>%
  dplyr::select(-Date,everything()) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(which(.>2.2))+2)) %>% 
  dplyr::select(-Date)

save(lhz.tz.day1rain,lhz.ug.day1rain,lhz.mw.day1rain,file="data/clean/weather/lhz_day1rain.RData" )

 
####cluster level day1rain for tanzania ###########
 

# rain.TZMW.list = list(precip_lhz_tz,precip_lhz_mw,precip_clust_tz,precip_clust_mw)
# rain.UG.list = list(precip_lhz_ug,precip_clust_ug)

precip.clust.tz.rain = rain.TZMW.cropyear[[3]]%>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "right", fill = NA))) %>%
  na.omit()

clust.tz.day1rain=
  precip.clust.tz.rain %>%
  dplyr::select(-Date,everything()) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(which(.>2.2))+2)) %>% 
  dplyr::select(-Date)

#lhz.tz.day1rain[is.na(lhz.tz.day1rain)]=156


####cluster level day1rain for Uganda ###########
precip.clust.ug.rain = rain.UG.cropyear[[2]]%>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "left", fill = NA))) %>%
  na.omit()

#colnames(precip.lhz.mw.rain)[3]

clust.ug.day1rain=
  precip.clust.ug.rain %>%
  dplyr::select(-Date,everything()) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(which(.>2.2))+2)) %>% 
  dplyr::select(-Date)


####cluster level day1rain for malawi ###########
precip.clust.mw.rain = rain.TZMW.cropyear[[4]]%>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "left", fill = NA))) %>%
  na.omit()

clust.mw.day1rain=
  precip.clust.mw.rain %>%
  dplyr::select(-Date,everything()) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(which(.>2.2))+2)) %>% 
  dplyr::select(-Date)

save(clust.tz.day1rain,clust.ug.day1rain,clust.mw.day1rain,file="data/clean/weather/clust_day1rain.RData" )



#################################################################################
#### generate maxdaysno rain 
########### longest dry spell during the rainy season (Oct-Mar) per crop year (May-Apr)"
#################################################################################


# rain.TZMW.list = list(precip_lhz_tz,precip_lhz_mw,precip_clust_tz,precip_clust_mw)
# rain.UG.list = list(precip_lhz_ug,precip_clust_ug)

maxdaysnorain.lhz.tz = 
  rain.TZMW.cropyear[[1]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # 1 indicates that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1]))) %>%   # count the days with 0 rain
  dplyr::select(-Date)
  
maxdaysnorain.clust.tz = 
  rain.TZMW.cropyear[[3]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # indicate that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1])))  %>%  # count the days with 0 rain
  dplyr::select(-Date)

maxdaysnorain.lhz.mw = 
  rain.TZMW.cropyear[[2]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # 1 indicates that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1]))) %>%   # count the days with 0 rain
  dplyr::select(-Date)

maxdaysnorain.clust.mw = 
  rain.TZMW.cropyear[[4]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # indicate that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1])))  %>%  # count the days with 0 rain
  dplyr::select(-Date)


maxdaysnorain.lhz.ug = 
  rain.UG.cropyear[[1]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # 1 indicates that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1]))) %>%   # count the days with 0 rain
  dplyr::select(-Date)

maxdaysnorain.clust.ug = 
  rain.UG.cropyear[[2]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # indicate that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1])))  %>%  # count the days with 0 rain
  dplyr::select(-Date)



save(maxdaysnorain.lhz.tz,maxdaysnorain.lhz.ug,maxdaysnorain.lhz.mw,file="data/clean/weather/lhz_maxdaysnorain.RData" )
save(maxdaysnorain.clust.tz,maxdaysnorain.clust.ug,maxdaysnorain.clust.mw,file="data/clean/weather/clust_maxdaysnorain.RData" )


#################################################################################
#### generate rain_cytot
###########  "total rainfall from Oct to Apr （or march to july） by ipczone and cropyear" 
#################################################################################


# rain.TZMW.list = list(precip_lhz_tz,precip_lhz_mw,precip_clust_tz,precip_clust_mw)
# rain.UG.list = list(precip_lhz_ug,precip_clust_ug)

rain.cytot.lhz.tz = 
  rain.TZMW.cropyear[[1]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::summarise_all(funs(sum)) %>%  # count the total rain 
  dplyr::select(-Date)


 rain.cytot.clust.tz = 
  rain.TZMW.cropyear[[3]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-Date,cropyear,-year,-month) %>%
  dplyr::summarise_all(funs(sum))   # count the total rain 

 
 rain.cytot.lhz.mw = 
   rain.TZMW.cropyear[[2]] %>%
   dplyr::group_by(cropyear) %>%
   dplyr::arrange(Date) %>%
   dplyr::select(-cropyear,-year,-month) %>%
   dplyr::summarise_all(funs(sum)) %>%  # count the total rain 
   dplyr::select(-Date)
 
 
 rain.cytot.clust.mw = 
   rain.TZMW.cropyear[[4]] %>%
   dplyr::group_by(cropyear) %>%
   dplyr::arrange(Date) %>%
   dplyr::select(-Date,cropyear,-year,-month) %>%
   dplyr::summarise_all(funs(sum))  # count the total rain 

 rain.cytot.lhz.ug = 
   rain.UG.cropyear[[1]] %>%
   dplyr::group_by(cropyear) %>%
   dplyr::arrange(Date) %>%
   dplyr::select(-cropyear,-year,-month) %>%
   dplyr::summarise_all(funs(sum)) %>%  # count the total rain 
   dplyr::select(-Date)
 
 
 rain.cytot.clust.ug = 
   rain.UG.cropyear [[2]] %>%
   dplyr::group_by(cropyear) %>%
   dplyr::arrange(Date) %>%
   dplyr::select(-cropyear,-year,-month) %>%
   dplyr::summarise_all(funs(sum)) %>%  # count the total rain 
   dplyr::select(-Date)
 
 
 save(rain.cytot.lhz.tz,rain.cytot.lhz.ug,rain.cytot.lhz.mw,file="data/clean/weather/lhz_rain_cytot.RData" )
 save(rain.cytot.clust.tz,rain.cytot.clust.ug,rain.cytot.clust.mw,file="data/clean/weather/clust_rain_cytot.RData" )
 
 
 
#################################################################################
#### generate mean temperature in the growing season   
#################################################################################

mw_tmin <- read.csv("data/raw/temperature/mw_daily_tmin.csv")
mw_tmax <- read.csv("data/raw/temperature/mw_daily_tmax.csv")
tz_tmin <- read.csv("data/raw/temperature/tz_daily_tmin.csv")
tz_tmax <- read.csv("data/raw/temperature/tz_daily_tmax.csv")
ug_tmin <- read.csv("data/raw/temperature/ug_daily_tmin.csv")
ug_tmax <- read.csv("data/raw/temperature/ug_daily_tmax.csv")

source("R/functions/CropYearTZMW.R") 
source("R/functions/CropYearUG.R") 

temp.TZMW.list= list(tz_tmax,tz_tmin,mw_tmax,mw_tmin)
temp.UG.list= list(ug_tmax,ug_tmin)


formatTempDF= function(df){
  formatted.DF = df %>% dplyr::mutate(Date= as.Date(date1,"%m/%d/%Y")) %>% dplyr::select(-date1)   
    
  tryCatch( {formatted.DF = formatted.DF %>% dplyr::select(-X)},error=function(e){} )
  return(formatted.DF)
}

temp.TZMW.list.format = lapply(temp.TZMW.list, function(x){formatTempDF(x)})
temp.UG.list.format = lapply(temp.UG.list, function(x){formatTempDF(x)})


temp.TZMW.cropyear = lapply(temp.TZMW.list.format, function(x){CropYearTZMW(x)})
temp.UG.cropyear = lapply(temp.UG.list.format, function(x){CropYearUG(x)})

##########################################################
## create mean temp variable for Uganda
##########################################################

ug.date = temp.UG.cropyear[[1]]$Date
ug.cropyear = temp.UG.cropyear[[1]]$cropyear

temp.UG.cropyear[[1]] = temp.UG.cropyear[[1]] %>% dplyr::select(-Date,-month,-year,-cropyear)
temp.UG.cropyear[[2]] = temp.UG.cropyear[[2]] %>% dplyr::select(-Date,-month,-year,-cropyear)

ug.temp.mean.full = (temp.UG.cropyear[[1]] + temp.UG.cropyear[[2]])/2 -273.15
ug.temp.mean.full["Date"] = ug.date
ug.temp.mean.full["cropyear"] = ug.cropyear


ug.tmean = 
  ug.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-Date) %>%
  dplyr::summarise_all(funs(mean))   # generate the mean temperature by year by ipczone

 
##########################################################
## create mean temp variable for Tanzania 
##########################################################

tz.date = temp.TZMW.cropyear[[1]]$Date
tz.cropyear = temp.TZMW.cropyear[[1]]$cropyear


temp.TZMW.cropyear[[1]] = temp.TZMW.cropyear[[1]] %>% dplyr::select(-Date,-month,-year,-cropyear)
temp.TZMW.cropyear[[2]] = temp.TZMW.cropyear[[2]] %>% dplyr::select(-Date,-month,-year,-cropyear)

tz.temp.mean.full = (temp.TZMW.cropyear[[1]] + temp.TZMW.cropyear[[2]])/2 -273.15
tz.temp.mean.full["Date"] = tz.date
tz.temp.mean.full["cropyear"] = tz.cropyear


tz.tmean = 
  tz.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-Date) %>%
  dplyr::summarise_all(funs(mean))   # generate the mean temperature by year by ipczone


##########################################################
## create mean temp variable for Tanzania 
##########################################################

mw.date = temp.TZMW.cropyear[[3]]$Date
mw.cropyear = temp.TZMW.cropyear[[3]]$cropyear


temp.TZMW.cropyear[[3]] = temp.TZMW.cropyear[[3]] %>% dplyr::select(-Date,-month,-year,-cropyear)
temp.TZMW.cropyear[[4]] = temp.TZMW.cropyear[[4]] %>% dplyr::select(-Date,-month,-year,-cropyear)

mw.temp.mean.full = (temp.TZMW.cropyear[[3]] + temp.TZMW.cropyear[[4]])/2 -273.15
mw.temp.mean.full["Date"] = mw.date
mw.temp.mean.full["cropyear"] = mw.cropyear


mw.tmean = 
  mw.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-Date) %>%
  dplyr::summarise_all(funs(mean))   # generate the mean temperature by year by ipczone




save(tz.tmean,ug.tmean,mw.tmean,file="data/clean/weather/tmean.RData" )



#################################################################################
#### generate growing degree days  
###########  number of days where temp was between 8 to 32 C (Tmax + Tmin)/2 . Deschênes and Greenstone (2007) yield  on weather 
#################################################################################


mw.gdd = mw.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<32,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1])))   %>% # count the days with 0 rain
  dplyr::select(-Date)

tz.gdd = tz.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<32,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1]))) %>%  # count the days with 0 rain
  dplyr::select(-Date)
  
ug.gdd = 
  ug.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<32,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1]))) %>%  # count the days with 0 rain
  dplyr::select(-Date)

save(tz.gdd,ug.gdd,mw.gdd, file="data/clean/weather/gdd.RData")
#################################################################################
## merge the rainfall variable with cluster/ipc zone id and year 
#############################################################################


library(dplyr)
rm(list=ls())

load("data/clean/weather/clust_day1rain.RData")
load("data/clean/weather/clust_maxdaysnorain.RData")
load("data/clean/weather/clust_rain_cytot.RData")

load("data/clean/weather/lhz_day1rain.RData")
load("data/clean/weather/lhz_maxdaysnorain.RData")
load("data/clean/weather/lhz_rain_cytot.RData")

load("data/clean/weather/gdd.RData")
load("data/clean/weather/tmean.RData")

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

tz_concordance <-  read.csv("data/clean/concordance/Tanzania_coord_lhz.csv")
tz_concordance =  tz_concordance %>% dplyr::select(ea_id,FNID)%>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))




colnames(mw_hh) = c("FNID","id")
colnames(tz_concordance)=c("id","FNID") 
#colnames(ug_concordance)=c("id","FNID") 

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

# select the clusters that are in the flood prone region 
mw_flood_clust = 
  mw_hh %>% dplyr::filter(FNID %in% flood_mw_fnid)  %>% dplyr::select(id)
mw_flood_clust 

# create flood max at mw clust
floodmax_clust_mw_flood = mw_clust_weather[[1]] %>% dplyr::filter(id %in% mw_flood_clust)%>% mutate(raincytot =0 )
floodmax_clust_mw_noflood = mw_clust_weather[[1]] %>% dplyr::filter(!id %in% mw_flood_clust)
floodmax_clust_mw = dplyr::bind_rows(floodmax_clust_mw_noflood,floodmax_clust_mw_flood)
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
  tz_concordance %>% dplyr::filter(FNID %in% flood_tz_fnid)  %>% dplyr::select(id)
tz_flood_ea_str = as.character(tz_flood_ea[,1])


tz_clust_weather[[1]]$id = as.vector(tz_clust_weather[[1]]$id)   
  
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

colnames(floodmax_clust_mw)[3] = "floodmax"
#colnames(floodmax_clust_mw)[3] = "FS_year"


colnames(floodmax_clust_tz)[3] = "floodmax"
colnames(floodmax_lhz_tz)[3] = "lhz_floodmax"

colnames(floodmax_lhz_tz)[1] = "FNID"

colnames(floodmax_lhz_mw)[2] = "lhz_floodmax"
colnames(floodmax_lhz_mw)[3] = "FS_year"


floodmax_clust_mw = floodmax_clust_mw %>% dplyr::select(-cropyear)

floodmax_clust_mw = floodmax_clust_mw %>% dplyr::mutate(FS_year = as.numeric(FS_year) )
# floodmax_clust_mw = floodmax_clust_mw %>% select(-cropyear)

floodmax_lhz_mw = floodmax_lhz_mw %>% dplyr::select(-cropyear)
floodmax_lhz_mw = floodmax_lhz_mw %>% dplyr::mutate(FS_year = as.numeric(FS_year),FNID  = as.character(FNID) )

floodmax_lhz_tz = floodmax_lhz_tz %>% dplyr::select(-cropyear)
floodmax_lhz_tz = floodmax_lhz_tz %>% dplyr::mutate(FS_year = as.numeric(FS_year),FNID  = as.character(FNID) )


floodmax_clust_tz = floodmax_clust_tz %>% dplyr::mutate(FS_year = as.numeric(FS_year) )
floodmax_clust_tz = floodmax_clust_tz %>% dplyr::select(-cropyear)


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

save(mw_weather_final,file="data/clean/weather/mw_weather_final.rda")


class(tz_concordance[,2])

tz_c =   left_join(tz_clust_weather[[1]],tz_clust_weather[[2]] )
tz_c =   left_join(tz_c, tz_clust_weather[[3]])
tz_c =   left_join(tz_c, floodmax_clust_tz,by = c("id","FS_year"))

tz_lhz =   left_join(tz_lhz_weather[[2]], tz_lhz_weather[[1]])
tz_lhz =   left_join(tz_lhz, tz_lhz_weather[[3]])
tz_lhz =   left_join(tz_lhz, tz_lhz_weather[[4]])
tz_lhz =   left_join(tz_lhz, tz_lhz_weather[[5]])
tz_lhz =   left_join(tz_lhz, floodmax_lhz_tz,by = c("FNID","FS_year"))


tz = left_join(tz_lhz, floodmax_clust_tz)
tz_final = left_join(tz, tz_c,by = c("id","FS_year"))
  

save(tz_final,file="data/clean/weather/tz_weather_final.rda")


#################################################################################

  