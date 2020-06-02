####################################################################################################################################
# Goal : This script aims to put together the houhsehold, price and weather data. 

# purpose: 
# 1. generate variables at the household level to be used in the regression 
# 2. generate cluster geovariables used to extract weather and match with prices 

# Input : 
# 1. csv file of cleaned lsms household survey   dataset

# Output: 
# 0. cluster geovariables 

# Yujun Zhou -  04/18/18
###################################################################

rm(list = ls())

require(tidyverse)
library(zoo)
library(imputeTS)
source("R/functions/Yearmon.R")

####################################################
### Merge Malawi Data 
####################################################

# read household data 
mw.lsms = read.csv("data/clean/household/mw_hh_aggregate.csv",stringsAsFactors = FALSE)
 
colSums(is.na(mw.lsms))


mw.lsms$ea_id = as.character(mw.lsms$ea_id)

source("R/functions/Yearmon.R")
mw.lsms = yearmon (mw.lsms, year_var = "FS_year",month_var = "FS_month" )

# mw.lsms = mw.lsms %>% dplyr::select( -date)


# read price data 
load("data/clean/market/mw_price_final.RData")

colSums(is.na(mw_price_merge_final))


# mw_price_merge_final


# class(mw_price_merge_final$ea_id)


# weekly price data to monthly 
mw.current.price = mw_price_merge_final %>% 
  dplyr::select(-mkt,-dist_km) %>% 
  distinct() %>% 
  group_by(ea_id,FS_year,yearmon) %>% 
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))

library(imputeTS)

## Missing Nov 2016 price data, impute by the average of Dec and Oct 2016 prices 
 

mw.nov2016.price = mw.current.price %>% 
  dplyr::filter(yearmon== "Dec 2016"|yearmon== "Oct 2016") %>%
  dplyr::group_by(ea_id,FS_year) %>% 
  dplyr::select(-yearmon) %>%
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE))) %>%
  dplyr::mutate(yearmon="Nov 2016")

mw.nov2016.price$yearmon = as.yearmon(mw.nov2016.price$yearmon)
mw.nov2016.price$date = as.Date(mw.nov2016.price$yearmon)

mw.current.price = dplyr::union(mw.current.price,mw.nov2016.price)

mw.current.price.impute = mw.current.price %>% 
                          mutate(year=format(date,"%Y")) %>% 
                          group_by(ea_id,year) 

# interpolate missing using recent months prices  

for ( index in  5:ncol(mw.current.price.impute)-1) {
  col.name.temp = colnames(mw.current.price.impute)[index]
  mw.current.price.impute[col.name.temp] = na.interpolation(unlist(mw.current.price[col.name.temp]),option = "stine")
}

colSums(is.na(mw.current.price.impute))

library(imputeTS)



# create a one month lag for each price 
mw.lag1.price = mw.current.price.impute 
mw.lag1.price["yearmon_lag1"]= mw.lag1.price$yearmon + 0.1
mw.lag1.price["yearmon"] = mw.lag1.price["yearmon_lag1"]
mw.lag1.price = mw.lag1.price %>% ungroup() %>% dplyr::select(-yearmon_lag1,-date,-year)
names(mw.lag1.price)[4:ncol(mw.lag1.price)] = paste("lag1",names(mw.lag1.price)[4:ncol(mw.lag1.price)],sep="_")

# 3 months lag 
mw.lag3.price = mw.current.price.impute 
mw.lag3.price["yearmon_lag3"]= mw.lag3.price$yearmon + 0.25
mw.lag3.price["yearmon"] = mw.lag3.price["yearmon_lag3"]
mw.lag3.price = mw.lag3.price %>% ungroup() %>% dplyr::select(-yearmon_lag3,-date,-year)
names(mw.lag3.price)[4:ncol(mw.lag3.price)] = paste("lag3",names(mw.lag3.price)[4:ncol(mw.lag3.price)],sep="_")


# 6 months lag 
mw.lag6.price = mw.current.price.impute 
mw.lag6.price["yearmon_lag6"]= mw.lag6.price$yearmon + 0.5
mw.lag6.price["yearmon"] = mw.lag6.price["yearmon_lag6"]
mw.lag6.price = mw.lag6.price %>% ungroup() %>% dplyr::select(-yearmon_lag6,-date,-year)
names(mw.lag6.price)[4:ncol(mw.lag6.price)] = paste("lag6",names(mw.lag6.price)[4:ncol(mw.lag6.price)],sep="_")

# 12 months lag
mw.lag12.price = mw.current.price.impute 
mw.lag12.price["yearmon_lag12"]= mw.lag12.price$yearmon + 1
mw.lag12.price["yearmon"] = mw.lag12.price["yearmon_lag12"]
mw.lag12.price = mw.lag12.price %>% ungroup() %>% dplyr::select(-yearmon_lag12,-date,-year)
names(mw.lag12.price)[4:ncol(mw.lag12.price)] = paste("lag12",names(mw.lag12.price)[4:ncol(mw.lag12.price)],sep="_")


# Join hh and prices
mw.lag1.price$ea_id = as.character(mw.lag1.price$ea_id)
mw.lag3.price$ea_id = as.character(mw.lag3.price$ea_id)
mw.lag6.price$ea_id = as.character(mw.lag6.price$ea_id)
mw.lag12.price$ea_id = as.character(mw.lag12.price$ea_id)


mw.master.hh = left_join(mw.lsms,mw.lag1.price)
mw.master.hh = left_join(mw.master.hh,mw.lag3.price)
mw.master.hh = left_join(mw.master.hh, mw.lag6.price)
mw.master.hh = left_join(mw.master.hh,mw.lag12.price)

colSums(is.na(mw.master.hh))
 

# mw.master.hh_notna = mw.master.hh %>% dplyr::filter(!is.na(mw.master.hh$lag1_clust_beans_mktthin) )
# 
# mw.master.hh_na = mw.master.hh %>% dplyr::filter(is.na(mw.master.hh$lag1_clust_beans_mktthin))
# 
# mw.master.hh_na_11 = mw.master.hh_na %>% dplyr::filter( FS_year == 2011)
# mw.master.hh_na_17 = mw.master.hh_na %>% dplyr::filter( FS_year == 2017)
# 
# mw.master.hh_notna_11 = mw.master.hh_notna %>% dplyr::filter( FS_year == 2011)
# mw.master.hh_notna_17 = mw.master.hh_notna %>% dplyr::filter( FS_year == 2017)
# 
# 
# unique(mw.master.hh_na_11$yearmon)
# unique(mw.master.hh_na_17$yearmon)
# 
# mw.master.hh_na_11$lag1_clust_maize_price = median(mw.master.hh_notna_11$lag1_clust_maize_price)
# mw.master.hh_na_11$lag1_clust_rice_price = median(mw.master.hh_notna_11$lag1_clust_rice_price)
# mw.master.hh_na_11$lag1_clust_nuts_price = median(mw.master.hh_notna_11$lag1_clust_nuts_price)
# mw.master.hh_na_11$lag1_clust_beans_price = median(mw.master.hh_notna_11$lag1_clust_beans_price)
# mw.master.hh_na_11$lag1_clust_maize_mktthin =1
# mw.master.hh_na_11$lag1_clust_rice_mktthin = 1
# mw.master.hh_na_11$lag1_clust_nuts_mktthin = 1
# mw.master.hh_na_11$lag1_clust_beans_mktthin = 1
# mw.master.hh_na_17$lag1_clust_maize_price = median(mw.master.hh_notna_17$lag1_clust_maize_price)
# mw.master.hh_na_17$lag1_clust_rice_price = median(mw.master.hh_notna_17$lag1_clust_rice_price)
# mw.master.hh_na_17$lag1_clust_nuts_price = median(mw.master.hh_notna_17$lag1_clust_nuts_price)
# mw.master.hh_na_17$lag1_clust_beans_price = median(mw.master.hh_notna_17$lag1_clust_beans_price)
# mw.master.hh_na_17$lag1_clust_maize_mktthin =1
# mw.master.hh_na_17$lag1_clust_rice_mktthin = 1
# mw.master.hh_na_17$lag1_clust_nuts_mktthin = 1
# mw.master.hh_na_17$lag1_clust_beans_mktthin = 1

# 
# mw.master.hh = bind_rows(mw.master.hh_notna,mw.master.hh_na_11 )
# mw.master.hh = bind_rows(mw.master.hh,mw.master.hh_na_17 )
#  

# colSums(is.na(mw.master.hh))

mw.master.hh = mw.master.hh %>% dplyr::select(-hhsize)

save(mw.master.hh,file="data/clean/dataset/mw_hh+price.RData")


# mw.master.hh= mw.lsms
load("data/clean/dataset/mw_hh+price.RData")

load("data/clean/weather/mw_weather_final.RData")
# 
# mw.weather.final = mw.weather.final %>% dplyr::filter(!is.na(VID) & !is.na(tmean))
# 
# mw.weather.final["lhz_floodmax"][is.na(mw.weather.final["lhz_floodmax"])] = 0


unique(mw.weather.final$FS_year)

mw.weather.final = mw.weather.final %>%   dplyr::select(-lat_modified,-lon_modified)
 
mw.master.hh$FS_year = as.numeric(mw.master.hh$FS_year)
mw.master.hh = left_join(mw.master.hh,mw.weather.final,by=c("ea_id","FS_year"))



mw.master.hh_notna_11 = mw.master.hh %>% dplyr::filter( FS_year == 2011 )
 
 

# mw.master.hh = mw.master.hh %>% dplyr::filter(!is.na(VID) & !is.na(date))

colSums(is.na(mw.master.hh))


mw_na = mw.master.hh %>% dplyr::filter(is.na(tmean ) )

mw_notna = mw.master.hh %>% dplyr::filter(!is.na(tmean))
colSums(is.na(mw_notna))

table(ss$FS_year)


mw_na$lhz_day1rain = median(mw_notna$lhz_day1rain)
mw_na$gdd = median(mw_notna$gdd)
mw_na$tmean = median(mw_notna$tmean)
mw_na$lhz_raincytot = median(mw_notna$lhz_raincytot)
mw_na$lhz_maxdaysnorain = median(mw_notna$lhz_maxdaysnorain)
mw_na$heatdays  = median(mw_notna$heatdays )


mw.hh.data = bind_rows(mw_notna,mw_na)

mw.hh.data = mw.hh.data %>% dplyr::select(-X,-FNID)

mw.hh.data$rCSI[is.na(mw.hh.data$rCSI)] = 0 
mw.hh.data$cell_phone[is.na(mw.hh.data$cell_phone)] = 0 
mw.hh.data$number_celphones[is.na(mw.hh.data$number_celphones)] = 0 

  
colSums(is.na(mw.hh.data))

mw.hh.data = mw.hh.data %>% 
  dplyr::filter(FCS!=-Inf )

# lapply(mw.master.hh, class)

# 
# mw.hh.data = mw.master.hh %>% dplyr::select (-HHID,-ea_id,-cropyear,-year,-yearmon,-date)
# 
# mw.hh.data = mw.master.hh %>% dplyr::select (-VID,-cropyear,-yearmon)
# 
# mw.hh.data = mw.hh.data %>% filter(FCS!=-Inf )


mw.master.clust = mw.hh.data %>% 
  dplyr::filter(FCS!=-Inf ) %>% 
  dplyr::select(-HHID,-cropyear) %>%
  group_by(ea_id,FS_year) %>%   
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))  

colSums(is.na(mw.master.clust))


#mw.hh.data = mw.hh.data %>% 
#  dplyr::select(-lat_modified,-lon_modified,-FNID)
# lapply(mw.master.clust, class)

# mw.master.clust$FCS[1415:1419]


 
# 
# write.csv(mw.master.clust, file= "data/clean/dataset/mw_cluster_noprice.csv",row.names = FALSE)


write.csv(mw.hh.data, file= "data/clean/dataset/mw_dataset_hh.csv",row.names = FALSE)
write.csv(mw.master.clust, file= "data/clean/dataset/mw_dataset_cluster.csv",row.names = FALSE)


####################################################
### Merge Tanzania Data 
####################################################

rm(list = ls())
require(tidyverse)
library(zoo)
library(imputeTS)
source("R/functions/Yearmon.R")
# read household data 
 
tz.lsms = read.csv("data/clean/household/tz_hh_aggregate.csv",stringsAsFactors = FALSE)

tz.lsms$ea_id = as.character(tz.lsms$clusterid)
tz.lsms = yearmon (tz.lsms, year_var = "FS_year",month_var = "FS_month" )

# remove columns that can not be used in the prediction anlaysis 
tz.lsms = tz.lsms %>% 
  dplyr::select(-clusterid,-date ) 

# check for missing values 
colSums(is.na(tz.lsms))

  
# read price data 
load("data/clean/market/tz_price_final.RData")

colnames(tz_price_merge_final) 

# tz_price_merge_final

library(zoo)
tz_price_merge_final$yearmon = as.yearmon(tz_price_merge_final$yearmon)
# class(tz_price_merge_final$ea_id)

tz.current.price = tz_price_merge_final %>% 
  dplyr::select(-mkt,-dist_km,-weights,-FNID) %>% 
  distinct() %>% 
  group_by(ea_id,yearmon) %>% 
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))

library(imputeTS)


colSums(is.na(tz.current.price))


tz.current.price.impute = tz.current.price %>% 
  dplyr::mutate(year=format(date,"%Y")) %>% 
  dplyr::select(-clust_bean_price,-lhz_bean_price) %>% 
  dplyr::group_by(ea_id,year) 



for ( index in  4:ncol(tz.current.price.impute)-1) {
  col.name.temp = colnames(tz.current.price.impute)[index]
  tz.current.price.impute[col.name.temp] = na.interpolation(unlist(tz.current.price[col.name.temp]),option = "stine")
}

colSums(is.na(tz.current.price.impute))


# create a one month lag for each price 
tz.lag1.price = tz.current.price.impute 
tz.lag1.price["yearmon_lag1"]= tz.lag1.price$yearmon + 0.1
tz.lag1.price["yearmon"] = tz.lag1.price["yearmon_lag1"]
tz.lag1.price = tz.lag1.price %>% ungroup() %>% dplyr::select(-yearmon_lag1,-date,-year)
names(tz.lag1.price)[3:ncol(tz.lag1.price)] = paste("lag1",names(tz.lag1.price)[3:ncol(tz.lag1.price)],sep="_")

# 3 months lag 
tz.lag3.price = tz.current.price.impute 
tz.lag3.price["yearmon_lag3"]= tz.lag3.price$yearmon + 0.25
tz.lag3.price["yearmon"] = tz.lag3.price["yearmon_lag3"]
tz.lag3.price = tz.lag3.price %>% ungroup() %>% dplyr::select(-yearmon_lag3,-date,-year)
names(tz.lag3.price)[3:ncol(tz.lag3.price)] = paste("lag3",names(tz.lag3.price)[3:ncol(tz.lag3.price)],sep="_")


# 6 months lag 
tz.lag6.price = tz.current.price.impute 
tz.lag6.price["yearmon_lag6"]= tz.lag6.price$yearmon + 0.5
tz.lag6.price["yearmon"] = tz.lag6.price["yearmon_lag6"]
tz.lag6.price = tz.lag6.price %>% ungroup() %>% dplyr::select(-yearmon_lag6,-date,-year)
names(tz.lag6.price)[3:ncol(tz.lag6.price)] = paste("lag6",names(tz.lag6.price)[3:ncol(tz.lag6.price)],sep="_")

# 12 months lag
tz.lag12.price = tz.current.price.impute 
tz.lag12.price["yearmon_lag12"]= tz.lag12.price$yearmon + 1
tz.lag12.price["yearmon"] = tz.lag12.price["yearmon_lag12"]
tz.lag12.price = tz.lag12.price %>% ungroup() %>% dplyr::select(-yearmon_lag12,-date,-year)
names(tz.lag12.price)[3:ncol(tz.lag12.price)] = paste("lag12",names(tz.lag12.price)[3:ncol(tz.lag12.price)],sep="_")



# Join hh and prices
tz.master.hh = left_join(tz.lsms,tz.lag1.price, by = c("ea_id","yearmon"))
tz.master.hh = left_join(tz.master.hh,tz.lag3.price, by = c("ea_id","yearmon"))
tz.master.hh = left_join(tz.master.hh, tz.lag6.price, by = c("ea_id","yearmon"))
tz.master.hh = left_join(tz.master.hh,tz.lag12.price, by = c("ea_id","yearmon"))


# read weather data 
load("data/clean/weather/tz_weather_final.RData")


tz.weather.final = tz.weather.final %>% dplyr::filter(!is.na(tmean))

tz.weather.final["lhz_floodmax"][is.na(tz.weather.final["lhz_floodmax"])] = 0
tz.weather.final["floodmax"][is.na(tz.weather.final["floodmax"])] = 0

 
 colSums(is.na(tz.weather.final))


# tz_price_merge_final

library(zoo)
   


colSums(is.na(tz.master.hh))


tz.master.hh = left_join(tz.master.hh,tz.weather.final, by = c("ea_id","FS_year"))


# fill in missing values by the ones in the same year/month and same cluster 
tz.master.fill = tz.master.hh %>% 
  group_by(FNID) %>% 
  mutate(dist_road= ifelse(is.na(dist_road), mean(dist_road, na.rm=TRUE), dist_road)) %>% 
  mutate(dist_popcenter= ifelse(is.na(dist_popcenter), mean(dist_popcenter, na.rm=TRUE), dist_popcenter)) %>% 
  mutate(percent_ag= ifelse(is.na(percent_ag), mean(percent_ag, na.rm=TRUE), percent_ag)) %>% 
  mutate(nutri_reten_severe_constraint= ifelse(is.na(nutri_reten_severe_constraint), mean(nutri_reten_severe_constraint, na.rm=TRUE), nutri_reten_severe_constraint)) %>% 
  mutate(nutri_moderate_constraint= ifelse(is.na(nutri_moderate_constraint), mean(nutri_moderate_constraint, na.rm=TRUE), nutri_moderate_constraint)) %>% 
  mutate(dummy_terrain_rough= ifelse(is.na(dummy_terrain_rough), mean(dummy_terrain_rough, na.rm=TRUE), dummy_terrain_rough)) %>% 
  mutate(nutri_severe_constraint= ifelse(is.na(nutri_severe_constraint), mean(nutri_severe_constraint, na.rm=TRUE), nutri_severe_constraint))  

colSums(is.na(tz.master.fill))

#sapply(tz.master.hh,class)

tz.master.hh = tz.master.fill %>% 
  dplyr::select(-"cropyear",-"HHID") %>%
  na.omit()

colSums(is.na(tz.master.hh))


tz.master.clust = tz.master.hh %>% 
  group_by(ea_id,FS_year) %>%   
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))  

 # colnames(tz.master.hh)

tz.master.clust = tz.master.clust %>% 
  ungroup() %>%
  select(-yearmon,-FNID)

write.csv(tz.master.hh, file= "data/clean/dataset/tz_dataset_hh.csv",row.names = FALSE)
write.csv(tz.master.clust, file= "data/clean/dataset/tz_dataset_cluster.csv",row.names = FALSE)


####################################################
### Merge Uganda Data 
####################################################
rm(list = ls())
require(tidyverse)
library(zoo)
library(imputeTS)
source("R/functions/Yearmon.R")
# read household data 

ug.lsms = read.csv("data/clean/household/ug_hh_aggregate.csv",stringsAsFactors = FALSE)

ug.lsms$ea_id = as.character(ug.lsms$ea_id)
ug.lsms = yearmon (ug.lsms, year_var = "FS_year",month_var = "FS_month" )

# remove columns that can not be used in the prediction anlaysis 
ug.lsms = ug.lsms %>% 
  dplyr::select(-date,-region1 ) 

# check for missing values 
colSums(is.na(ug.lsms))

# read price data 
load("data/clean/market/ug_price_final.RData")

ug.current.price = ug_price_merge_final %>% 
   dplyr::select(-mkt,-dist_km,-weights,-FNID) %>% 
   distinct() %>% 
   group_by(ea_id,yearmon) %>% 
   dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))
 
 library(imputeTS)
 
 
 colSums(is.na(ug.current.price))
 
 
 ug.current.price.impute = ug.current.price %>% 
   dplyr::mutate(year=format(date,"%Y")) %>% 
   dplyr::select(-clust_bean_price,-lhz_bean_price,-clust_maize_price,-lhz_maize_price) %>% 
   dplyr::group_by(ea_id,year) 
 
 
 
 # create a one month lag for each price # create a one month lag for each price 
 ug.lag1.price = ug.current.price.impute 
 ug.lag1.price["yearmon_lag1"]= ug.lag1.price$yearmon + 0.1
 ug.lag1.price["yearmon"] = ug.lag1.price["yearmon_lag1"]
 ug.lag1.price = ug.lag1.price %>% ungroup() %>% dplyr::select(-yearmon_lag1,-date,-year)
 names(ug.lag1.price)[3:ncol(ug.lag1.price)] = paste("lag1",names(ug.lag1.price)[3:ncol(ug.lag1.price)],sep="_")
 
 # 3 months lag 
 ug.lag3.price = ug.current.price.impute 
 ug.lag3.price["yearmon_lag3"]= ug.lag3.price$yearmon + 0.25
 ug.lag3.price["yearmon"] = ug.lag3.price["yearmon_lag3"]
 ug.lag3.price = ug.lag3.price %>% ungroup() %>% dplyr::select(-yearmon_lag3,-date,-year)
 names(ug.lag3.price)[3:ncol(ug.lag3.price)] = paste("lag3",names(ug.lag3.price)[3:ncol(ug.lag3.price)],sep="_")
 
 
 # 6 months lag 
 ug.lag6.price = ug.current.price.impute 
 ug.lag6.price["yearmon_lag6"]= ug.lag6.price$yearmon + 0.5
 ug.lag6.price["yearmon"] = ug.lag6.price["yearmon_lag6"]
 ug.lag6.price = ug.lag6.price %>% ungroup() %>% dplyr::select(-yearmon_lag6,-date,-year)
 names(ug.lag6.price)[3:ncol(ug.lag6.price)] = paste("lag6",names(ug.lag6.price)[3:ncol(ug.lag6.price)],sep="_")
 
 # 12 months lag
 ug.lag12.price = ug.current.price.impute 
 ug.lag12.price["yearmon_lag12"]= ug.lag12.price$yearmon + 1
 ug.lag12.price["yearmon"] = ug.lag12.price["yearmon_lag12"]
 ug.lag12.price = ug.lag12.price %>% ungroup() %>% dplyr::select(-yearmon_lag12,-date,-year)
 names(ug.lag12.price)[3:ncol(ug.lag12.price)] = paste("lag12",names(ug.lag12.price)[3:ncol(ug.lag12.price)],sep="_")
 
 
 
 # Join hh and prices
 ug.master.hh = left_join(ug.lsms,ug.lag1.price, by = c("ea_id","yearmon"))
 ug.master.hh = left_join(ug.master.hh,ug.lag3.price, by = c("ea_id","yearmon"))
 ug.master.hh = left_join(ug.master.hh, ug.lag6.price, by = c("ea_id","yearmon"))
 ug.master.hh = left_join(ug.master.hh,ug.lag12.price, by = c("ea_id","yearmon"))
 
 
 colSums(is.na( ug.master.hh))
 

# read weather data 
load("data/clean/weather/ug_weather_final.RData")

colSums(is.na(ug.weather.final))
 

ug.weather.final = ug.weather.final %>% dplyr::filter(!is.na(tmean))

ug.weather.final["lhz_floodmax"][is.na(ug.weather.final["lhz_floodmax"])] = 0
ug.weather.final["floodmax"][is.na(ug.weather.final["floodmax"])] = 0

colSums(is.na(ug.weather.final))


ug.master.hh = left_join(ug.lsms,ug.current.price.impute, by = c("ea_id","yearmon"))
colSums(is.na(ug.master.hh))

ug.master.hh = ug.master.hh %>% dplyr::filter(!is.na(date))



ug.master.hh = left_join(ug.master.hh,ug.weather.final, by = c("ea_id","FS_year"))

  
library(tidyverse)
ug.master.hh = ug.master.hh %>%   dplyr::select( -yearmon,-year,-cropyear,-date)


ug.master.clust = ug.master.hh %>% 
  group_by(ea_id,FS_year) %>%   
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE))) 

ug.master.clust = ug.master.clust %>%
  ungroup() %>%
  select(-HHID,-FNID)
#colSums(is.na(ug.master.clust))

# colnames(ug.master.hh)

write.csv(ug.master.hh, file= "data/clean/dataset/ug_dataset_hh.csv",row.names = FALSE)
write.csv(ug.master.clust, file= "data/clean/dataset/ug_dataset_cluster.csv",row.names = FALSE)

# 
# # Merge into one dataset 
# 
# 
# 
# rm(list = ls())
# 
# require(tidyverse)
# library(zoo)
# 
# 
# mw.clust = read.csv("data/clean/dataset/mw_dataset_cluster.csv",stringsAsFactors = FALSE)
# 
# tz.clust = read.csv("data/clean/dataset/tz_dataset_cluster.csv",stringsAsFactors = FALSE)
# 
# ug.clust = read.csv("data/clean/dataset/ug_dataset_cluster.csv",stringsAsFactors = FALSE)
# 
# 
# 
# mw.clust = mw.clust %>% 
#   mutate(Cellphone = cell_phone) %>% 
#   mutate(num_cell = number_celphones) %>% 
#   select(- starts_with("region"),
#          -FNID,-hhsize,-dist_admarc,-hhsize,- cell_phone,- number_celphones,
#          -clust_maize_price,-clust_rice_price,-clust_nuts_price,
#          -clust_beans_price,-clust_rice_mktthin,-clust_nuts_mktthin,-clust_beans_mktthin,
#          -lhz_maize_price,-lhz_rice_price,-lhz_nuts_price,-lhz_beans_price,
#          -lhz_rice_mktthin,-lhz_nuts_mktthin,-lhz_beans_mktthin,-heatdays  
#          
#          )
# 
# tz.clust = tz.clust %>% 
#   select(- starts_with("region"),
#          -clust_maize_price,-clust_rice_price,
#          -clust_rice_mktthin,
#          -lhz_maize_price,-lhz_rice_price,
#          -lhz_rice_mktthin,-heatdays,
#          -clust_bean_mktthin, -lhz_bean_mktthin 
#   )
# 
# 
# ug.clust = ug.clust %>% 
#   select(- starts_with("region"),
#          -clust_cassava_price,-clust_bean_mktthin,-clust_cassava_mktthin,
#          -lhz_cassava_price,-lhz_bean_mktthin,-lhz_cassava_mktthin,
#          -FNID
#   )
# 
# 
# colnames(mw.clust)[!(colnames(mw.clust) %in% colnames(ug.clust))]
# colnames(tz.clust)[!(colnames(tz.clust) %in% colnames(ug.clust))]
# 
# colnames(ug.clust)[!(colnames(ug.clust) %in% colnames(mw.clust))]
# colnames(tz.clust)[!(colnames(tz.clust) %in% colnames(mw.clust))]
# 
# 
# ug.clust = ug.clust %>% filter(FCS!=-Inf)
# 
# # removing the testing set
# 
# # mw15/16
# 
# mw.test = mw.clust %>%
#           filter(FS_year == 2015|FS_year == 2016)
# 
# # tz14/15
# tz.test = tz.clust %>%
#   filter(FS_year == 2015|FS_year == 2014)
# 
# 
# # ug12
# ug.test = ug.clust %>%
#   filter(FS_year == 2012)
# 
# mw.train = mw.clust %>%
#   filter(FS_year <2016 )
# 
# tz.train = tz.clust %>%
#   filter(FS_year <2014 )
# 
# ug.train = ug.clust %>%
#   filter(FS_year <2012 )
# 
# one_dataset= bind_rows(mw.train,tz.train)
# 
# one_dataset= bind_rows(one_dataset,ug.train)
# 
# rural_dataset = one_dataset %>% filter(rural==1)
# 
# write.csv(rural_dataset,"data/clean/segmentation/rural_dataset.csv" , row.names = FALSE )
# write.csv(one_dataset,"data/clean/segmentation/one_dataset.csv" , row.names = FALSE )
# 
# 
# write.csv(ug.test,"data/clean/segmentation/ug_test.csv" , row.names = FALSE )
# write.csv(tz.test,"data/clean/segmentation/tz_test.csv" , row.names = FALSE )
# write.csv(mw.test,"data/clean/segmentation/mw_test.csv" , row.names = FALSE )
# 
# # 
# # # fill in missing values by the ones in the same year/month and same cluster 
# # ug.lsms.fill = ug.lsms %>% 
# #   group_by(ea_id,FS_year) %>% 
# #   mutate(dist_road= ifelse(is.na(dist_road), mean(dist_road, na.rm=TRUE), dist_road)) %>% 
# #   mutate(dist_popcenter= ifelse(is.na(dist_popcenter), mean(dist_popcenter, na.rm=TRUE), dist_popcenter)) %>% 
# #   mutate(dist_market= ifelse(is.na(dist_market), mean(dist_market, na.rm=TRUE), dist_market)) %>% 
# #   mutate(dist_admctr= ifelse(is.na(dist_admctr), mean(dist_admctr, na.rm=TRUE), dist_admctr)) %>% 
# #   mutate(ag_percent= ifelse(is.na(ag_percent), mean(ag_percent, na.rm=TRUE), ag_percent)) %>% 
# #   mutate(floor_cement= ifelse(is.na(floor_cement), mean(floor_cement, na.rm=TRUE), floor_cement)) %>% 
# #   mutate(floor_dirt_sand_dung= ifelse(is.na(floor_dirt_sand_dung), mean(floor_dirt_sand_dung, na.rm=TRUE), floor_dirt_sand_dung)) %>% 
# #   mutate(roof_iron= ifelse(is.na(roof_iron), mean(roof_iron, na.rm=TRUE), roof_iron)) %>% 
# #   mutate(roof_natural= ifelse(is.na(roof_natural), mean(roof_natural, na.rm=TRUE), roof_natural)) %>% 
# #   mutate(roof_other= ifelse(is.na(roof_other), mean(roof_other, na.rm=TRUE), roof_other)) %>% 
# #   mutate(Radio= ifelse(is.na(Radio), mean(Radio, na.rm=TRUE), Radio)) %>% 
# #   mutate(Television= ifelse(is.na(Television), mean(Television, na.rm=TRUE), Television)) %>% 
# #   mutate(Bicycle= ifelse(is.na(Bicycle), mean(Bicycle, na.rm=TRUE), Bicycle)) %>% 
# #   mutate(Motorcycle= ifelse(is.na(Motorcycle), mean(Motorcycle, na.rm=TRUE), Motorcycle)) %>% 
# #   mutate(Car= ifelse(is.na(Car), mean(Car, na.rm=TRUE), Car)) %>% 
# #   mutate(cellphone= ifelse(is.na(cellphone), mean(cellphone, na.rm=TRUE), cellphone)) %>% 
# #   mutate(number_celphones= ifelse(is.na(number_celphones), mean(number_celphones, na.rm=TRUE), number_celphones)) %>% 
# #   mutate(dummy_terrain_rough= ifelse(is.na(dummy_terrain_rough), mean(dummy_terrain_rough, na.rm=TRUE), dummy_terrain_rough)) %>% 
# #   mutate(slope= ifelse(is.na(slope), mean(slope, na.rm=TRUE), slope)) %>% 
# #   mutate(elevation= ifelse(is.na(elevation), mean(elevation, na.rm=TRUE), elevation)) 
# 
