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


library(dplyr)
####################################################
### Merge Malawi Data 
####################################################
rm(list = ls())

# read household data 
mw.lsms = read.csv("data/clean/mw_lsms.csv",stringsAsFactors = FALSE)

# read price data 
load("data/clean/market/mw_price_final.RData")

# read weather data 
load("data/clean/weather/mw_weather_final.RData")

colnames(mw_price_merge_final) 

# mw_price_merge_final

library(zoo)
mw_price_merge_final$yearmon = as.yearmon(mw_price_merge_final$yearmon)
# class(mw_price_merge_final$ea_id)
mw.lsms$ea_id = as.character(mw.lsms$ea_id)
mw.lsms$yearmon = as.yearmon(mw.lsms$yearmon)
mw.lsms$rural =  ifelse(mw.lsms$reside=="rural",1,0)


mw.current.price = mw_price_merge_final %>% 
  dplyr::select(-mkt,-dist_km,-weights,-FNID) %>% 
  distinct() %>% 
  group_by(ea_id,yearmon) %>% 
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))


mw.master.hh = left_join(mw.lsms,mw.current.price, by = c("ea_id","yearmon"))
mw.master.hh = left_join(mw.master.hh,mw.weather.final, by = c("ea_id","FS_year","FNID"))

mw.master.clust = mw.master.hh %>% 
              group_by(ea_id) %>%   
              dplyr::summarise_all(funs(mean(.,na.rm=TRUE))) %>%
              dplyr::select(-FNID,-case_id,-Reason1,-Reason2,-Reason3,-reside,-TA_names,-hh_a01)
          
# colnames(mw.master.hh)
write.csv(mw.master.hh, file= "data/mw_dataset_hh.csv",row.names = FALSE)
write.csv(mw.master.clust, file= "data/mw_dataset_cluster.csv",row.names = FALSE)


####################################################
### Merge Tanzania Data 
####################################################

rm(list = ls())

# read household data 
tz.lsms = read.csv("data/clean/tz_lsms.csv",stringsAsFactors = FALSE)

# read price data 
load("data/clean/market/tz_price_final.RData")

# read weather data 
load("data/clean/weather/tz_weather_final.RData")

colnames(tz_price_merge_final) 

# tz_price_merge_final

library(zoo)
tz_price_merge_final$yearmon = as.yearmon(tz_price_merge_final$yearmon)
# class(tz_price_merge_final$ea_id)
tz.lsms$ea_id = as.character(tz.lsms$ea_id)
tz.lsms$yearmon = as.yearmon(tz.lsms$yearmon)
tz.lsms$rural =  ifelse(tz.lsms$reside=="rural",1,0)

tz.current.price = tz_price_merge_final %>% 
  dplyr::select(-mkt,-dist_km,-weights,-FNID) %>% 
  distinct() %>% 
  group_by(ea_id,yearmon) %>% 
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))


tz.master.hh = left_join(tz.lsms,tz.current.price, by = c("ea_id","yearmon"))
tz.master.hh = left_join(tz.master.hh,tz.weather.final, by = c("ea_id","FS_year","FNID"))

tz.master.clust = tz.master.hh %>% 
  group_by(ea_id) %>%   
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE))) %>%
  dplyr::select(-FNID,-case_id,-reside,-hh_a01)

 # colnames(tz.master.hh)

write.csv(tz.master.hh, file= "data/tz_dataset_hh.csv",row.names = FALSE)
write.csv(tz.master.clust, file= "data/tz_dataset_cluster.csv",row.names = FALSE)


####################################################
### Merge Uganda Data 
####################################################
rm(list = ls())

# read household data 
ug.lsms = read.csv("data/clean/ug_lsms.csv",stringsAsFactors = FALSE)

# read price data 
load("data/clean/market/ug_price_final.RData")

# read weather data 
load("data/clean/weather/ug_weather_final.RData")

colnames(ug_price_merge_final) 

# ug_price_merge_final

library(zoo)
ug_price_merge_final$yearmon = as.yearmon(ug_price_merge_final$yearmon)
# class(ug_price_merge_final$ea_id)
ug.lsms$ea_id = as.character(ug.lsms$ea_id)
ug.lsms$yearmon = as.yearmon(ug.lsms$yearmon)
ug.lsms$rural =  ifelse(ug.lsms$reside=="rural",1,0)

ug.current.price = ug_price_merge_final %>% 
  dplyr::select(-mkt,-dist_km,-weights,-FNID) %>% 
  distinct() %>% 
  group_by(ea_id,yearmon) %>% 
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))


ug.master.hh = left_join(ug.lsms,ug.current.price, by = c("ea_id","yearmon"))
ug.master.hh = left_join(ug.master.hh,ug.weather.final, by = c("ea_id","FS_year","FNID"))

ug.master.clust = ug.master.hh %>% 
  group_by(ea_id) %>%   
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE))) %>%
  dplyr::select(-FNID,-case_id,-reside,-hh_a01)

# colnames(ug.master.hh)

write.csv(ug.master.hh, file= "data/ug_dataset_hh.csv",row.names = FALSE)
write.csv(ug.master.clust, file= "data/ug_dataset_cluster.csv",row.names = FALSE)
