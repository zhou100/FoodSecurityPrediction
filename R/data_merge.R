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

###
rm(list = ls())

tz_hh<-read.csv("data/clean/tan_hh.csv")
tz_hh = tz_hh %>%  select(-X)

tz_weather = read.csv("D:/tz_weather_final.csv")
colnames(tz_weather)[11] = "ea_id"
tz_weather = tz_weather %>%  select(-X,-cropyear.x,-cropyear.y)
tz_weather = tz_weather %>% mutate(ea_id = as.character(ea_id))

tz_price = read.csv("data/clean/market/tz_price_merge.csv")
tz_price = tz_price %>%  select(-X,-mkt,dist_km,date)


library(dplyr)
tz_price = tz_price  %>% dplyr::mutate(ea_id = as.character(ea_id))
tz_hh = tz_hh  %>% dplyr::mutate(ea_id = as.character(ea_id))


tz_master = dplyr::left_join(tz_hh,tz_price,by = c("ea_id","yearmon","FNID"))
tz_master = dplyr::left_join(tz_master,tz_weather,by = c("ea_id","FS_year","FNID"))




write.csv(tz_master,"data/clean/tz_hh_master.csv")

tz_master = read.csv("data/clean/tz_hh_master.csv")
tz_master = tz_master %>% select(-case_id,-reside,-hh_a01,-date,-X)

tz_master_cluster  = tz_master  %>% dplyr::group_by(FNID,ea_id,yearmon) %>% summarise_all(funs(mean))
write.csv(tz_master_cluster,"data/clean/tz_clust_master.csv")


tz_master_cluster["logFCS"] = log(tz_master_cluster["FCS"])



unique(tz_master_cluster$FS_year)
unique(tz_master_cluster$FS_month)

test = tz_master_cluster[tz_master_cluster$FS_year==2013 & tz_master_cluster$FS_month ==10|tz_master_cluster$FS_year==2013 & tz_master_cluster$FS_month ==11 |tz_master_cluster$FS_year==2013 & tz_master_cluster$FS_month ==9,]
train = tz_master_cluster[tz_master_cluster$FS_year==2012 | tz_master_cluster$FS_year==2010 | tz_master_cluster$FS_year==2011 | tz_master_cluster$FS_year==2013 & tz_master_cluster$FS_month !=10 & tz_master_cluster$FS_month !=11 & tz_master_cluster$FS_month !=9,]

test = test %>% ungroup %>%  select(-FS_month,-FS_year,-ea_id,-FNID,-FCS,-yearmon,-asset_index)
train = train %>%  ungroup %>% select(-FS_month,-FS_year,-ea_id,-FNID,-FCS,-yearmon,-asset_index)



test_clust = test %>% select(-lhz_growingdays,-lhz_day1rain,-lhz_raincytot,-lhz_floodmax,-lhz_maize_price,-lhz_maize_mktthin,-lhz_rice_price,-lhz_rice_mktthin )
train_clust = train %>% select(-lhz_growingdays,-lhz_day1rain,-lhz_raincytot,-lhz_floodmax,-lhz_maize_price,-lhz_maize_mktthin,-lhz_rice_price,-lhz_rice_mktthin )



  
write.csv(test_clust,"data/clean/tz_test_clust.csv")
write.csv(train_clust,"data/clean/tz_train_clust.csv")

