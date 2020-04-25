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
rm(list=ls())
load("data/raw/rain/tz_rain_0818.rda")
load("data/raw/rain/ug_rain_0818.rda")
load("data/raw/rain/mw_rain_0818.rda")

 
# colnames(precip_clust_ug)

precip_clust_mw[["Date"]] = as.Date(precip_clust_mw$Date,"%m/%d/%Y")
colnames(precip_clust_mw)


colnames(precip_clust_tz)[colnames(precip_clust_tz)=="date"]= "Date"
colnames(precip_clust_ug)[colnames(precip_clust_ug)=="date"] = "Date"

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
  precip.lhz.ug.rain %>%
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
  dplyr::select(-cropyear,-year,-month,-Date) %>%
  dplyr::summarise_all(funs(sum))   # count the total rain 
  
#colnames(rain.cytot.lhz.tz)

 rain.cytot.clust.tz = 
  rain.TZMW.cropyear[[3]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-Date,cropyear,-year,-month) %>%
  dplyr::summarise_all(funs(sum))   # count the total rain 
 #colnames(rain.cytot.clust.tz)
 
 rain.cytot.lhz.mw = 
   rain.TZMW.cropyear[[2]] %>%
   dplyr::group_by(cropyear) %>%
   dplyr::arrange(Date) %>%
   dplyr::select(-cropyear,-year,-month,-Date) %>%
   dplyr::summarise_all(funs(sum)) 
 

 #colnames(rain.cytot.lhz.mw)
 
 rain.cytot.clust.mw = 
   rain.TZMW.cropyear[[4]] %>%
   dplyr::group_by(cropyear) %>%
   dplyr::arrange(Date) %>%
   dplyr::select(-Date,cropyear,-year,-month) %>%
   dplyr::summarise_all(funs(sum))  # count the total rain 

# colnames(rain.cytot.clust.mw)
 
 
 rain.cytot.lhz.ug = 
   rain.UG.cropyear[[1]] %>%
   dplyr::group_by(cropyear) %>%
   dplyr::arrange(Date) %>%
   dplyr::select(-cropyear,-year,-month, -Date) %>%
   dplyr::summarise_all(funs(sum)) 
 
 #colnames(rain.cytot.lhz.ug)
  
 rain.cytot.clust.ug = 
   rain.UG.cropyear [[2]] %>%
   dplyr::group_by(cropyear) %>%
   dplyr::arrange(Date) %>%
   dplyr::select(-cropyear,-year,-month,-Date) %>%
   dplyr::summarise_all(funs(sum))   # count the total rain 
  
 #colnames(rain.cytot.clust.ug)
 
 
 
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
  dplyr::mutate_all(funs(ifelse(.>8 & .<30,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1])))   %>% # count the days with 0 rain
  dplyr::select(-Date)

tz.gdd = tz.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<30,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1]))) %>%  # count the days with 0 rain
  dplyr::select(-Date)
  
ug.gdd = 
  ug.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<30,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1]))) %>%  # count the days with 0 rain
  dplyr::select(-Date)

save(tz.gdd,ug.gdd,mw.gdd, file="data/clean/weather/gdd.RData")





#################################################################################
#### generate heat days  (>30 c )
###########  number of days where temp was between 8 to 32 C (Tmax + Tmin)/2 . Deschênes and Greenstone (2007) yield  on weather 
#################################################################################


mw.heatday = mw.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>=30,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1])))   %>% # count the days with 0 rain
  dplyr::select(-Date)

tz.heatday = tz.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<32,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1]))) %>%  # count the days with 0 rain
  dplyr::select(-Date)

ug.heatday = 
  ug.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<32,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1]))) %>%  # count the days with 0 rain
  dplyr::select(-Date)

save(tz.heatday,ug.heatday,mw.heatday, file="data/clean/weather/heatday.RData")
#################################################################################
## Transpose the rainfall variable with cluster/ipc zone id and year 
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
load("data/clean/weather/heatday.RData")

source("R/functions/WeatherTranspose.R") 

mw.clust.list = list(rain.cytot.clust.mw,clust.mw.day1rain,maxdaysnorain.clust.mw)
tz.clust.list = list(rain.cytot.clust.tz,clust.tz.day1rain,maxdaysnorain.clust.tz)
ug.clust.list = list(rain.cytot.clust.ug,clust.ug.day1rain,maxdaysnorain.clust.ug)

 
mw.lhz.list = list(lhz.mw.day1rain, mw.gdd,mw.tmean,rain.cytot.lhz.mw,maxdaysnorain.lhz.mw,mw.heatday)
tz.lhz.list  = list(lhz.tz.day1rain, tz.gdd,tz.tmean,rain.cytot.lhz.tz,maxdaysnorain.lhz.tz,tz.heatday)
ug.lhz.list  = list(lhz.ug.day1rain, ug.gdd,ug.tmean,rain.cytot.lhz.ug,maxdaysnorain.lhz.ug,ug.heatday)

 
mw.clust.list.transpose= lapply(mw.clust.list, WeatherTranspose)
tz.clust.list.transpose= lapply(tz.clust.list, WeatherTranspose)
ug.clust.list.transpose= lapply(ug.clust.list, WeatherTranspose)

mw.lhz.list.transpose= lapply(mw.lhz.list, WeatherTranspose)
tz.lhz.list.transpose= lapply(tz.lhz.list, WeatherTranspose)
ug.lhz.list.transpose= lapply(ug.lhz.list, WeatherTranspose)


source("R/functions/RemoveX.R") 
mw.clust.list.transpose= lapply(mw.clust.list.transpose, RemoveX)
tz.clust.list.transpose= lapply(tz.clust.list.transpose, RemoveX) 
ug.clust.list.transpose= lapply(ug.clust.list.transpose, RemoveX) 


mw.lhz.list.transpose= lapply(mw.lhz.list.transpose, RemoveX)
tz.lhz.list.transpose= lapply(tz.lhz.list.transpose, RemoveX) 
ug.lhz.list.transpose= lapply(ug.lhz.list.transpose, RemoveX) 

 


#################################################################################
##generate dummy for flood susceptible areas
#################################################################################

# need a concordance table of  cluster id and ipczone from coord and 
library(dplyr)
mw_concordance = read.csv("data/clean/concordance/mw_ea_lhz .csv")
# mw_concordance = mw_concordance %>% dplyr::select(-X) %>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))
colnames(mw_concordance)


tz_concordance <-  read.csv("data/clean/concordance/tz_ea_lhz .csv")
# tz_concordance =  tz_concordance %>%  na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))

ug_concordance <-  read.csv("data/clean/concordance/ug_ea_lhz .csv")
# ug_concordance =  ug_concordance %>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))

# colnames(tz_concordance) = c("id","FNID")
# colnames(ug_concordance) = c("id","FNID")
# 
# 
# fnid_to_v = read.csv("data/clean/concordance/FNID to V .csv",stringsAsFactors=FALSE)
# colnames(fnid_to_v) = c("id","VID")
# 
# source("R/functions/FnidV.R") 
# mw.lhz.list.transpose = lapply(mw.lhz.list.transpose,function(x){FnidV(x,fnid_to_v)})


# mw.lhz.list = list(lhz.mw.day1rain, mw.gdd,mw.tmean,rain.cytot.lhz.mw,maxdaysnorain.lhz.mw)

# MW2012C3020515 MW2012C3031005  MW2012C3031206

flood_mw_fnid = c("MW2012C3020515","MW2012C3031005","MW2012C3031206")

floodmax.lhz.mw.noflood = mw.lhz.list.transpose[[4]] %>% dplyr::filter(!id %in% flood_mw_fnid)%>% mutate(value =0 )
floodmax.lhz.mw.flood = mw.lhz.list.transpose[[4]] %>% dplyr::filter(id %in% flood_mw_fnid)

floodmax_lhz_mw = dplyr::bind_rows(floodmax.lhz.mw.noflood,floodmax.lhz.mw.flood)
# flood max should be in the same year 
floodmax_lhz_mw["FS_year"] =floodmax_lhz_mw["cropyear"] 

# select the clusters that are in the flood prone region 
mw_flood_clust = 
  mw_concordance %>% dplyr::filter(FNID %in% flood_mw_fnid) %>% mutate(ea_year = FS_year )
# mw_flood_clust 

# create flood max at mw clust
# mw.clust.list = list(rain.cytot.clust.mw,clust.mw.day1rain,maxdaysnorain.clust.mw)

clust_mw_rain  = mw.clust.list.transpose[[1]] %>% separate(id, c("ea_year", "ea_id"))%>% unique()

# 1366, 1494, 1545, 1554, 1555, 1556, 2922, 3050, 3101, 3110, 3111

floodmax_clust_mw_flood = clust_mw_rain %>% dplyr::filter(ea_id %in% mw_flood_clust$ea_id )
floodmax_clust_mw_noflood = clust_mw_rain %>% dplyr::filter(!(ea_id %in% mw_flood_clust$ea_id)) %>% mutate(value =0 )
floodmax_clust_mw = dplyr::bind_rows(floodmax_clust_mw_noflood,floodmax_clust_mw_flood)

# flood max should be in the same year , so change the FS_year to the current year, instead of one year before
floodmax_clust_mw["FS_year"] =floodmax_clust_mw["cropyear"] 

##############################################################################################################  
##### Tanzania Floodprone regions 
##### Livelihood)Zone)11)(Sisal‐sugar cane‐cattlzone)
##############################################################################################################  
# Livelihood)Zone)6)(tree)crops‐fishing)coastal)zone)
#54: Mtwara-Lindi-Pwani Riverine Zone Livelihood Zone 
# 57: Mpanda Maize, Paddy, Sunflower, and Livestock Livelihood Zone 
#$ 59: Kyela Paddy, Cacao, and Palm Oil Lowlands Livelihood Zone 
# 64: Kamsamba Paddy, Sorghum, and Livestock Lowlands Livelihood Zone 
# 72: Mtera Dam Fishing, Sorghum, and Sesame Livelihood Zone 

# needed to reference the shapefile and online map
#  TZ2009L106  TZ2009L111  TZ2009L154  TZ2009L157 TZ2009L159 TZ2009L164 TZ2009L172
flood_tz_fnid = c("TZ2009L106","TZ2009L111","TZ2009L154","TZ2009L157","TZ2009L159","TZ2009L172")

tz.lhz.list.transpose[[4]]$id = as.character(tz.lhz.list.transpose[[4]]$id)

floodmax_lhz_tz_noflood = tz.lhz.list.transpose[[4]] %>% dplyr::filter(!id %in% flood_tz_fnid)%>% mutate(value =0 )
floodmax_lhz_tz_flood = tz.lhz.list.transpose[[4]] %>% dplyr::filter(id %in% flood_tz_fnid)
floodmax_lhz_tz = dplyr::bind_rows(floodmax_lhz_tz_noflood,floodmax_lhz_tz_flood)

# flood max should be in the same year 
floodmax_lhz_tz["FS_year"] =floodmax_lhz_tz["cropyear"] 


tz_flood_ea = 
  tz_concordance %>% dplyr::filter(FNID %in% flood_tz_fnid)  %>% dplyr::select(ea_id)
tz_flood_ea_str = as.character(tz_flood_ea[,1])


# tz.clust.list = list(rain.cytot.clust.tz,clust.tz.day1rain,maxdaysnorain.clust.tz)

tz.clust.list.transpose[[1]]$id = as.character(tz.clust.list.transpose[[1]]$id)   

floodmax_clust_tz_noflood  = tz.clust.list.transpose[[1]] %>% dplyr::filter(!id %in% tz_flood_ea_str)%>% mutate(value =0 )
floodmax_clust_tz_flood  = tz.clust.list.transpose[[1]] %>% dplyr::filter(id %in% tz_flood_ea_str)
floodmax_clust_tz = dplyr::bind_rows(floodmax_clust_tz_noflood,floodmax_clust_tz_flood)
floodmax_clust_tz["FS_year"] =floodmax_clust_tz["cropyear"] 


##############################################################################################################  
##### Uganda Floodprone regions  (From FEWS NET)
 ##############################################################################################################  

# LHZ 4: Kazinga Channel Cassava, Maize, Fruit, Vegetable, and Cotton Zone
# LHZ 7: Western Rift Valley Cocoa, Coffee and Cassava Zone
# LHZ 10: Albertine‐West Nile Lowland Cattle Zone
# LHZ 14: Karuma‐Masinga‐Oyam Tobacco, Maize, and Cassava Zone
# LHZ 17: Amuru‐Gulu Rice, Groundnut, Sorghum, and Livestock Zone
# LHZ 21: South Kitgum‐Pader‐Abim‐Kotido Simsim, Groundnuts, Sorghum and Cattle Zone
# LHZ 28: Mt. Elgon Highland Irish Potato and Cereal Zone
# LHZ 30: Eastern Lowland Rice and Root Crop Zone

 
# needed to reference the shapefile and online map, but basically the 
# last two digits is the lhz id  
# ug.lhz.list.transpose[[1]]$id

flood_ug_fnid = c("UG2011L104","UG2011L107","UG2011L110","UG2011L114","UG2011L117","UG2011L121","UG2011L128","UG2011L130")

ug.lhz.list.transpose[[4]]$id = as.character(ug.lhz.list.transpose[[4]]$id)

floodmax_lhz_ug_noflood = ug.lhz.list.transpose[[4]] %>% dplyr::filter(!id %in% flood_ug_fnid)%>% mutate(value =0 )
floodmax_lhz_ug_flood = ug.lhz.list.transpose[[4]] %>% dplyr::filter(id %in% flood_ug_fnid)
floodmax_lhz_ug = dplyr::bind_rows(floodmax_lhz_ug_noflood,floodmax_lhz_ug_flood)

# flood max should be in the same year 
floodmax_lhz_ug["FS_year"] =floodmax_lhz_ug["cropyear"] 


ug_flood_ea = 
  ug_concordance %>% dplyr::filter(FNID %in% flood_ug_fnid)  %>% dplyr::select(ea_id)
ug_flood_ea_str = as.character(ug_flood_ea[,1])


# ug.clust.list = list(rain.cytot.clust.ug,clust.ug.day1rain,maxdaysnorain.clust.ug)

ug.clust.list.transpose[[1]]$id = as.character(ug.clust.list.transpose[[1]]$id)   

floodmax_clust_ug_noflood  = ug.clust.list.transpose[[1]] %>% dplyr::filter(!id %in% ug_flood_ea_str)%>% mutate(value =0 )
floodmax_clust_ug_flood  = ug.clust.list.transpose[[1]] %>% dplyr::filter(id %in% ug_flood_ea_str)
floodmax_clust_ug = dplyr::bind_rows(floodmax_clust_ug_noflood,floodmax_clust_ug_flood)
floodmax_clust_ug["FS_year"] =floodmax_clust_ug["cropyear"] 


 

save(floodmax_lhz_tz,floodmax_lhz_ug,floodmax_lhz_mw,floodmax_clust_tz,floodmax_clust_ug,floodmax_clust_mw,file = "data/clean/weather/floodmax.RData")



save(tz.lhz.list.transpose, ug.lhz.list.transpose, mw.lhz.list.transpose,tz.clust.list.transpose, ug.clust.list.transpose,  
     mw.clust.list.transpose,file="data/clean/weather/weather_transpose.RData")

 



#################################################################################
## merge the rainfall variable with cluster/ipc zone id and year 
#############################################################################


# raincytot  day1rain  maxdays floodmax
rm(list=ls())
load("data/clean/weather/weather_transpose.RData")
load("data/clean/weather/floodmax.RData")



# mw.clust.list = list(rain.cytot.clust.mw,clust.mw.day1rain,maxdaysnorain.clust.mw)
# tz.clust.list = list(rain.cytot.clust.tz,clust.tz.day1rain,maxdaysnorain.clust.tz)
# ug.clust.list = list(rain.cytot.clust.ug,clust.ug.day1rain,maxdaysnorain.clust.ug)
# 
# 
# mw.lhz.list = list(lhz.mw.day1rain, mw.gdd,mw.tmean,rain.cytot.lhz.mw,maxdaysnorain.lhz.mw)
# tz.lhz.list  = list(lhz.tz.day1rain, tz.gdd,tz.tmean,rain.cytot.lhz.tz,maxdaysnorain.lhz.tz)
# ug.lhz.list  = list(lhz.ug.day1rain, ug.gdd,ug.tmean,rain.cytot.lhz.ug,maxdaysnorain.lhz.ug)

clust.names = c("raincytot","day1rain","maxdaysnorain")

for (i in 1:length(mw.clust.list.transpose)){
  colnames(mw.clust.list.transpose[[i]])[3] = clust.names[i]
  colnames(tz.clust.list.transpose[[i]])[3] = clust.names[i]
  colnames(ug.clust.list.transpose[[i]])[3] = clust.names[i]

  colnames(mw.clust.list.transpose[[i]])[1] = "ea_id"
  colnames(tz.clust.list.transpose[[i]])[1] = "ea_id"
  colnames(ug.clust.list.transpose[[i]])[1] = "ea_id"
}


lhz.names = c("lhz_day1rain","gdd","tmean","lhz_raincytot","lhz_maxdaysnorain","heatdays")

for (i in 1:length(mw.lhz.list.transpose)){
  colnames(mw.lhz.list.transpose[[i]])[3] = lhz.names[i]
  colnames(tz.lhz.list.transpose[[i]])[3] = lhz.names[i]
  colnames(ug.lhz.list.transpose[[i]])[3] = lhz.names[i]
  
  colnames(mw.lhz.list.transpose[[i]])[1] = "FNID"
  colnames(tz.lhz.list.transpose[[i]])[1] = "FNID"
  colnames(ug.lhz.list.transpose[[i]])[1] = "FNID"
}

# mw.lhz.list.transpose[[1]]
 
colnames(floodmax_clust_mw)[colnames(floodmax_clust_mw)=="id"] = "ea_id"
colnames(floodmax_clust_tz)[colnames(floodmax_clust_tz)=="id"] = "ea_id"
colnames(floodmax_clust_ug)[colnames(floodmax_clust_ug)=="id"] = "ea_id"
colnames(floodmax_clust_mw)[colnames(floodmax_clust_mw)=="value"]= "floodmax"
colnames(floodmax_clust_tz)[colnames(floodmax_clust_tz)=="value"] = "floodmax"
colnames(floodmax_clust_ug)[colnames(floodmax_clust_ug)=="value"] = "floodmax"

colnames(floodmax_lhz_mw)[colnames(floodmax_lhz_mw)=="id"] = "FNID"
colnames(floodmax_lhz_tz)[colnames(floodmax_lhz_tz)=="id"] = "FNID"
colnames(floodmax_lhz_ug)[colnames(floodmax_lhz_ug)=="id"] = "FNID"
colnames(floodmax_lhz_mw)[colnames(floodmax_lhz_mw)=="id"] = "lhz_floodmax"
colnames(floodmax_lhz_tz)[colnames(floodmax_lhz_tz)=="id"] = "lhz_floodmax"
colnames(floodmax_lhz_ug)[colnames(floodmax_lhz_ug)=="id"] = "lhz_floodmax"


floodmax_clust_mw = floodmax_clust_mw %>% dplyr::select(-cropyear) %>% dplyr::mutate(FS_year = as.numeric(FS_year) )
floodmax_clust_tz = floodmax_clust_tz %>% dplyr::select(-cropyear) %>% dplyr::mutate(FS_year = as.numeric(FS_year) ) 
floodmax_clust_ug = floodmax_clust_ug %>% dplyr::select(-cropyear) %>% dplyr::mutate(FS_year = as.numeric(FS_year) ) 

floodmax_lhz_mw = floodmax_lhz_mw %>% dplyr::select(-cropyear) %>% dplyr::mutate(FS_year = as.numeric(FS_year),FNID  = as.character(FNID) )
floodmax_lhz_tz = floodmax_lhz_tz %>% dplyr::select(-cropyear) %>% dplyr::mutate(FS_year = as.numeric(FS_year),FNID  = as.character(FNID) )
floodmax_lhz_ug = floodmax_lhz_ug %>% dplyr::select(-cropyear) %>% dplyr::mutate(FS_year = as.numeric(FS_year),FNID  = as.character(FNID) )


# need a concordance table of  cluster id and ipczone from coord and 
library(dplyr)
# mw_concordance = mw_concordance %>% dplyr::select(-X) %>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))


tz_concordance <-  read.csv("data/clean/concordance/tz_ea_lhz .csv")
# tz_concordance =  tz_concordance %>%  na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))

ug_concordance <-  read.csv("data/clean/concordance/ug_ea_lhz .csv")
# ug_concordance =  ug_concordance %>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))



#################################################################################
## merge Malawi data
#############################################################################

mw_concordance = read.csv("data/clean/concordance/mw_ea_lhz .csv")
mw_concordance = mw_concordance %>% mutate(ea_year = as.character(FS_year)) %>% dplyr::select(-FS_year) %>% mutate(ea_id = as.character(ea_id))

mw.weather.clust = full_join(mw.clust.list.transpose[[1]],mw.clust.list.transpose[[2]])
mw.weather.clust = full_join(mw.weather.clust,mw.clust.list.transpose[[3]])
mw.weather.clust  = mw.weather.clust%>% separate(ea_id, c("ea_year", "ea_id"))%>% unique() 
mw.weather.clust = full_join(mw.weather.clust,mw_concordance,by=c("ea_year","ea_id"))


mw.weather.clust  = mw.weather.clust %>% filter(ea_year == FS_year) %>% dplyr::select(-ea_year)

mw.weather.lhz= full_join(mw.lhz.list.transpose[[1]],mw.lhz.list.transpose[[2]])
mw.weather.lhz= full_join(mw.weather.lhz,mw.lhz.list.transpose[[3]])
mw.weather.lhz= full_join(mw.weather.lhz,mw.lhz.list.transpose[[4]])
mw.weather.lhz= full_join(mw.weather.lhz,mw.lhz.list.transpose[[5]])
mw.weather.lhz= full_join(mw.weather.lhz,mw.lhz.list.transpose[[6]])

colnames(floodmax_lhz_mw)[colnames(floodmax_lhz_mw)=="value"] = "lhz_floodmax"

mw.weather.final = full_join(mw.weather.clust,mw.weather.lhz)

# mw.weather.final = mw.weather.clust
  
  
# mw.weather.final = full_join(mw.weather.final,floodmax_clust_mw,by=c("FS_year","ea_id"))
# mw.weather.final = full_join(mw.weather.final,floodmax_lhz_mw,by=c("FS_year","FNID"))

 

mw.weather.final = mw.weather.final %>% dplyr::arrange(FS_year) %>% dplyr::filter(!is.na(ea_id))  

# colnames(mw.weather.final)[colnames(mw.weather.final)=="VID.x"]="VID"
# 
# mw.weather.final$floodmax[is.na(mw.weather.final$floodmax)] =0
# mw.weather.final$floodmax[is.na(mw.weather.final$lhz_floodmax)] =0
save(mw.weather.final,file="data/clean/weather/mw_weather_final.RData")


#################################################################################
## merge Tanzania data
#############################################################################
tz_concordance$ea_id = as.character(tz_concordance$ea_id)


tz.weather.clust = full_join(tz.clust.list.transpose[[1]],tz.clust.list.transpose[[2]])
tz.weather.clust = full_join(tz.weather.clust,tz.clust.list.transpose[[3]])
tz.weather.clust = full_join(tz.weather.clust,tz_concordance, by= c("ea_id","FS_year"))

tz.weather.clust

for (i in 1:length(tz.lhz.list.transpose)){
  tz.lhz.list.transpose[[i]]$FNID = as.character(tz.lhz.list.transpose[[i]]$FNID)  
}


#lapply(tz.lhz.list.transpose[[1]],class)

tz.weather.lhz= full_join(tz.lhz.list.transpose[[1]],tz.lhz.list.transpose[[2]])
tz.weather.lhz= full_join(tz.weather.lhz,tz.lhz.list.transpose[[3]])
tz.weather.lhz= full_join(tz.weather.lhz,tz.lhz.list.transpose[[4]])
tz.weather.lhz= full_join(tz.weather.lhz,tz.lhz.list.transpose[[5]])
tz.weather.lhz= full_join(tz.weather.lhz,tz.lhz.list.transpose[[6]])

tz.weather.final = full_join(tz.weather.clust,tz.weather.lhz)
tz.weather.final = full_join(tz.weather.final,floodmax_clust_tz,by=c("FS_year","ea_id"))
tz.weather.final = full_join(tz.weather.final,floodmax_lhz_tz,by=c("FS_year","FNID"))


tz.weather.final = tz.weather.final %>% arrange(FS_year) %>% filter(!is.na(ea_id)) %>% filter(!is.na(cropyear)) 

save(tz.weather.final,file="data/clean/weather/tz_weather_final.RData")

#################################################################################
## merge Uganda data
#############################################################################
ug.weather.clust = full_join(ug.clust.list.transpose[[1]],ug.clust.list.transpose[[2]])
ug.weather.clust = full_join(ug.weather.clust,ug.clust.list.transpose[[3]])
ug.weather.clust = full_join(ug.weather.clust,ug_concordance,by=c("ea_id"))

for (i in 1:length(ug.lhz.list.transpose)){
  ug.lhz.list.transpose[[i]]$FNID = as.character(ug.lhz.list.transpose[[i]]$FNID)  
}

ug.weather.lhz= full_join(ug.lhz.list.transpose[[1]],ug.lhz.list.transpose[[2]])
ug.weather.lhz= full_join(ug.weather.lhz,ug.lhz.list.transpose[[3]])
ug.weather.lhz= full_join(ug.weather.lhz,ug.lhz.list.transpose[[4]])
ug.weather.lhz= full_join(ug.weather.lhz,ug.lhz.list.transpose[[5]])



ug.weather.lhz= full_join(ug.weather.lhz,ug_concordance,by="FNID") 
ug.weather.lhz = ug.weather.lhz %>% arrange(FS_year) %>% filter(!is.na(FNID)) %>% filter(!is.na(cropyear)) 
save(ug.weather.lhz,file="data/clean/weather/ug_weather_lhz.RData")


ug.weather.final = full_join(ug.weather.clust,ug.weather.lhz)
ug.weather.final = full_join(ug.weather.final,floodmax_clust_ug,by=c("FS_year","ea_id"))
ug.weather.final = full_join(ug.weather.final,floodmax_lhz_ug,by=c("FS_year","FNID"))


ug.weather.final = ug.weather.final %>% arrange(FS_year) %>% filter(!is.na(ea_id)) %>% filter(!is.na(cropyear)) 



save(ug.weather.final,file="data/clean/weather/ug_weather_final.RData")

#################################################################################

  