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


require(tidyverse)
####################################################
### Merge Malawi Data 
####################################################
rm(list = ls())

# read household data 
mw.lsms = read.csv("data/clean/mw_lsms.csv",stringsAsFactors = FALSE)

mw.lsms$ea_id = as.character(mw.lsms$ea_id)
mw.lsms$yearmon = as.yearmon(mw.lsms$yearmon)
mw.lsms$rural =  ifelse(mw.lsms$reside=="rural",1,0)

# remove columns that can not be used in the prediction anlaysis 
mw.lsms = mw.lsms %>% 
          select(-cellphone_cost,-Reason1,-Reason2,-Reason3,-MAHFP,-hh_a01,-hh_wgt,-head_age,-slope,-reside) %>%
          filter(!is.na(FS_year) & !is.na(FCS) & !is.na(rCSI)) 

# check for missing values 
colSums(is.na(mw.lsms))



# fill in missing values by the ones in the same year/month and same cluster 
mw.lsms.fill = mw.lsms %>% 
         group_by(ea_id,FS_year,FS_month) %>% 
          mutate(number_celphones= ifelse(is.na(number_celphones), mean(number_celphones, na.rm=TRUE), number_celphones)) %>% 
  mutate(floor_cement= ifelse(is.na(floor_cement), mean(floor_cement, na.rm=TRUE), floor_cement)) %>% 
  mutate(floor_tile= ifelse(is.na(floor_tile), mean(floor_tile, na.rm=TRUE), floor_tile)) %>% 
  mutate(dist_road= ifelse(is.na(dist_road), mean(dist_road, na.rm=TRUE), dist_road)) %>% 
  mutate(dist_popcenter= ifelse(is.na(dist_popcenter), mean(dist_popcenter, na.rm=TRUE), dist_popcenter)) %>% 
  mutate(dist_admarc= ifelse(is.na(dist_admarc), mean(dist_admarc, na.rm=TRUE), dist_admarc)) %>% 
  mutate(ag_percent= ifelse(is.na(ag_percent), mean(ag_percent, na.rm=TRUE), ag_percent)) %>% 
  mutate(elevation= ifelse(is.na(elevation), mean(elevation, na.rm=TRUE), elevation)) 
  
# check if the missing still exist 

colSums(is.na(mw.lsms.fill))

# read price data 
load("data/clean/market/mw_price_final.RData")

colnames(mw_price_merge_final) 

# mw_price_merge_final

library(zoo)
mw_price_merge_final$yearmon = as.yearmon(mw_price_merge_final$yearmon)
# class(mw_price_merge_final$ea_id)

mw.current.price = mw_price_merge_final %>% 
  dplyr::select(-mkt,-dist_km,-weights,-FNID) %>% 
  distinct() %>% 
  group_by(ea_id,yearmon) %>% 
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))

library(imputeTS)


mw.current.price.impute = mw.current.price %>% 
                          mutate(year=format(date,"%Y")) %>% 
                          group_by(ea_id,year) 


for ( index in  4:ncol(mw.current.price.impute)-1) {
  col.name.temp = colnames(mw.current.price.impute)[index]
  mw.current.price.impute[col.name.temp] = na.interpolation(unlist(mw.current.price[col.name.temp]),option = "stine")
}

# create a one month lag for each price 
mw.current.price.impute["yearmon_lag1"]= mw.current.price.impute$yearmon + 0.1

mw.current.price.impute["yearmon"] = mw.current.price.impute["yearmon_lag1"]

mw.current.price.impute = mw.current.price.impute %>% dplyr::select(-yearmon_lag1)



mw.master.hh = left_join(mw.lsms.fill,mw.current.price.impute, by = c("ea_id","yearmon"))


# read weather data 
load("data/clean/weather/mw_weather_final.RData")

mw.weather.final = mw.weather.final %>% dplyr::filter(!is.na(VID) & !is.na(tmean))

mw.weather.final["lhz_floodmax"][is.na(mw.weather.final["lhz_floodmax"])] = 0

colSums(is.na(mw.weather.final))


mw.master.hh = left_join(mw.master.hh,mw.weather.final, by = c("ea_id","FS_year","FNID"))


mw.master.hh = mw.master.hh %>% dplyr::filter(!is.na(VID) & !is.na(date))

colSums(is.na(mw.master.hh))

mw.master.clust = mw.master.hh %>% 
              group_by(ea_id,FS_year,FNID) %>%   
              dplyr::select(-FNID,-case_id,-TA_names,-head_gender,-head_edlevel,-year,-Month,-cropyear,-VID) %>%
              dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))  
          

colSums(is.na(mw.master.clust))


# colnames(mw.master.hh)
write.csv(mw.master.hh, file= "data/mw_dataset_hh.csv",row.names = FALSE)
write.csv(mw.master.clust, file= "data/mw_dataset_cluster.csv",row.names = FALSE)


####################################################
### Merge Tanzania Data 
####################################################

rm(list = ls())

# read household data 
tz.lsms = read.csv("data/clean/tz_lsms.csv",stringsAsFactors = FALSE)


tz.lsms$ea_id = as.character(tz.lsms$ea_id)
tz.lsms$yearmon = as.yearmon(tz.lsms$yearmon)
tz.lsms$rural =  ifelse(tz.lsms$reside=="rural",1,0)

# remove columns that can not be used in the prediction anlaysis 
tz.lsms = tz.lsms %>% 
  select(-slope,-reside,-Motorcyclet,-Motorcycle) %>%
  filter(!is.na(FS_year) & !is.na(FCS) & !is.na(rCSI)) 

# check for missing values 
colSums(is.na(tz.lsms))



# fill in missing values by the ones in the same year/month and same cluster 
tz.lsms.fill = tz.lsms %>% 
  group_by(FNID) %>% 
  mutate(dist_road= ifelse(is.na(dist_road), mean(dist_road, na.rm=TRUE), dist_road)) %>% 
  mutate(dist_popcenter= ifelse(is.na(dist_popcenter), mean(dist_popcenter, na.rm=TRUE), dist_popcenter)) %>% 
  mutate(ag_percent= ifelse(is.na(ag_percent), mean(ag_percent, na.rm=TRUE), ag_percent)) %>% 
  mutate(elevation= ifelse(is.na(elevation), mean(elevation, na.rm=TRUE), elevation)) 

colSums(is.na(tz.lsms.fill))

 tz.lsms.fill = tz.lsms.fill %>% select(-dist_agmkt,-dist_headquater) %>% filter(!is.na(elevation))

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
tz.current.price.impute["yearmon_lag1"]= tz.current.price.impute$yearmon + 0.1

tz.current.price.impute["yearmon"] = tz.current.price.impute["yearmon_lag1"]

tz.current.price.impute = tz.current.price.impute %>% dplyr::select(-yearmon_lag1)

# read weather data 
load("data/clean/weather/tz_weather_final.RData")


tz.weather.final = tz.weather.final %>% dplyr::filter(!is.na(tmean))

tz.weather.final["lhz_floodmax"][is.na(tz.weather.final["lhz_floodmax"])] = 0
tz.weather.final["floodmax"][is.na(tz.weather.final["floodmax"])] = 0

 
 colSums(is.na(tz.weather.final))


# tz_price_merge_final

library(zoo)
   

tz.master.hh = left_join(tz.lsms.fill,tz.current.price.impute, by = c("ea_id","yearmon"))

colSums(is.na(tz.master.hh))


tz.master.hh = left_join(tz.master.hh,tz.weather.final, by = c("ea_id","FS_year","FNID"))

tz.master.hh = tz.master.hh %>% dplyr::filter(!is.na(date))


tz.master.clust = tz.master.hh %>% 
  group_by(ea_id,FS_year,FNID) %>%   
  dplyr::select(-FNID,-case_id,-hh_a01,-Month,-year,-cropyear) %>% 
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))  

 # colnames(tz.master.hh)

write.csv(tz.master.hh, file= "data/tz_dataset_hh.csv",row.names = FALSE)
write.csv(tz.master.clust, file= "data/tz_dataset_cluster.csv",row.names = FALSE)


####################################################
### Merge Uganda Data 
####################################################
rm(list = ls())

# read household data 
ug.lsms = read.csv("data/clean/ug_lsms.csv",stringsAsFactors = FALSE)


ug.lsms$ea_id = as.character(ug.lsms$ea_id)
ug.lsms$yearmon = as.yearmon(ug.lsms$yearmon)
ug.lsms$rural =  ifelse(ug.lsms$reside=="rural",1,0)

# remove columns that can not be used in the prediction anlaysis 
ug.lsms = ug.lsms %>% 
  filter(!is.na(FS_year) & !is.na(FCS) & !is.na(HDDS) & !is.na(ea_id)) 

# check for missing values 
colSums(is.na(ug.lsms))



# fill in missing values by the ones in the same year/month and same cluster 
ug.lsms.fill = ug.lsms %>% 
  group_by(ea_id,FS_year) %>% 
  mutate(dist_road= ifelse(is.na(dist_road), mean(dist_road, na.rm=TRUE), dist_road)) %>% 
  mutate(dist_popcenter= ifelse(is.na(dist_popcenter), mean(dist_popcenter, na.rm=TRUE), dist_popcenter)) %>% 
  mutate(dist_market= ifelse(is.na(dist_market), mean(dist_market, na.rm=TRUE), dist_market)) %>% 
  mutate(dist_admctr= ifelse(is.na(dist_admctr), mean(dist_admctr, na.rm=TRUE), dist_admctr)) %>% 
  mutate(ag_percent= ifelse(is.na(ag_percent), mean(ag_percent, na.rm=TRUE), ag_percent)) %>% 
  mutate(floor_cement= ifelse(is.na(floor_cement), mean(floor_cement, na.rm=TRUE), floor_cement)) %>% 
  mutate(floor_dirt_sand_dung= ifelse(is.na(floor_dirt_sand_dung), mean(floor_dirt_sand_dung, na.rm=TRUE), floor_dirt_sand_dung)) %>% 
  mutate(roof_iron= ifelse(is.na(roof_iron), mean(roof_iron, na.rm=TRUE), roof_iron)) %>% 
  mutate(roof_natural= ifelse(is.na(roof_natural), mean(roof_natural, na.rm=TRUE), roof_natural)) %>% 
  mutate(roof_other= ifelse(is.na(roof_other), mean(roof_other, na.rm=TRUE), roof_other)) %>% 
  mutate(Radio= ifelse(is.na(Radio), mean(Radio, na.rm=TRUE), Radio)) %>% 
  mutate(Television= ifelse(is.na(Television), mean(Television, na.rm=TRUE), Television)) %>% 
  mutate(Bicycle= ifelse(is.na(Bicycle), mean(Bicycle, na.rm=TRUE), Bicycle)) %>% 
  mutate(Motorcycle= ifelse(is.na(Motorcycle), mean(Motorcycle, na.rm=TRUE), Motorcycle)) %>% 
  mutate(Car= ifelse(is.na(Car), mean(Car, na.rm=TRUE), Car)) %>% 
  mutate(cellphone= ifelse(is.na(cellphone), mean(cellphone, na.rm=TRUE), cellphone)) %>% 
  mutate(number_celphones= ifelse(is.na(number_celphones), mean(number_celphones, na.rm=TRUE), number_celphones)) %>% 
  mutate(dummy_terrain_rough= ifelse(is.na(dummy_terrain_rough), mean(dummy_terrain_rough, na.rm=TRUE), dummy_terrain_rough)) %>% 
  mutate(slope= ifelse(is.na(slope), mean(slope, na.rm=TRUE), slope)) %>% 
  mutate(elevation= ifelse(is.na(elevation), mean(elevation, na.rm=TRUE), elevation)) 




ug.lsms.fill = ug.lsms.fill  %>% filter(!is.na(Radio) & !is.na(roof_iron))

colSums(is.na(ug.lsms.fill))


ug.lsms.fill["roof_natural"][which(is.na(ug.lsms.fill["roof_natural"])),]=0

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
 
 
 
 # create a one month lag for each price 
 ug.current.price.impute["yearmon_lag1"]= ug.current.price.impute$yearmon + 0.1
 
 ug.current.price.impute["yearmon"] = ug.current.price.impute["yearmon_lag1"]
 
 ug.current.price.impute = ug.current.price.impute %>% dplyr::select(-yearmon_lag1)
 
 
 colSums(is.na(ug.current.price.impute))
 

# read weather data 
load("data/clean/weather/ug_weather_final.RData")

colSums(is.na(ug.weather.final))
 

ug.weather.final = ug.weather.final %>% dplyr::filter(!is.na(tmean))

ug.weather.final["lhz_floodmax"][is.na(ug.weather.final["lhz_floodmax"])] = 0
ug.weather.final["floodmax"][is.na(ug.weather.final["floodmax"])] = 0

colSums(is.na(ug.weather.final))


ug.master.hh = left_join(ug.lsms.fill,ug.current.price.impute, by = c("ea_id","yearmon"))
colSums(is.na(ug.master.hh))

ug.master.hh = ug.master.hh %>% dplyr::filter(!is.na(date))



ug.master.hh = left_join(ug.master.hh,ug.weather.final, by = c("ea_id","FS_year","FNID"))

ug.master.hh = ug.master.hh %>% dplyr::filter(!is.na(cropyear))


ug.master.clust = ug.master.hh %>% 
  group_by(ea_id,FS_year,FNID) %>%   
  dplyr::select(-case_id,-reside,-hh_a01,-hh_a02,-Month,-year,-cropyear) %>%

  dplyr::summarise_all(funs(mean(.,na.rm=TRUE))) 


#colSums(is.na(ug.master.clust))

# colnames(ug.master.hh)

write.csv(ug.master.hh, file= "data/ug_dataset_hh.csv",row.names = FALSE)
write.csv(ug.master.clust, file= "data/ug_dataset_cluster.csv",row.names = FALSE)
