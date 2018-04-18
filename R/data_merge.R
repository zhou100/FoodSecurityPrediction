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

tz_weather = read.csv("data/clean/weather/tz_weather_final.csv")
colnames(tz_weather)[11] = "ea_id"
tz_weather = tz_weather %>%  select(-X,-cropyear.x,-cropyear.y)
tz_weather = tz_weather %>% mutate(ea_id = as.character(ea_id))

tz_price = read.csv("data/clean/market/tz_price_merge.csv")
tz_price = tz_price %>%  select(-X)


library(dplyr)

tz_master = dplyr::left_join(tz_hh,tz_price,by = c("ea_id","yearmon","FNID"))
tz_master = tz_master %>% dplyr::mutate(ea_id = as.character(ea_id))


tz_master = dplyr::left_join(tz_hh,tz_weather,by = c("ea_id","FS_year","FNID"))
tz_master=tz_master %>% dplyr::mutate(ea_id = as.character(ea_id))
tz_price = tz_price  %>% dplyr::mutate(ea_id = as.character(ea_id))
tz_master =  dplyr::left_join(tz_master,tz_price,by = c("ea_id","yearmon","FNID"))


write.csv(tz_master,"data/clean/tz_hh_master.csv")


tz_master_cluster  = tz_master  %>% dplyr::group_by(FNID,ea_id,yearmon) %>% summarise_all(funs(mean))
tz_master_cluster = tz_master_cluster %>% select(-case_id,-reside,-Month,-hh_a01,-mkt)

write.csv(tz_master_cluster,"data/clean/tz_clust_master.csv")

unique(tz_master_cluster$FS_year)
test = tz_master_cluster[tz_master_cluster$FS_year==2013,]
train = tz_master_cluster[tz_master_cluster$FS_year==2012 | tz_master_cluster$FS_year==2010 | tz_master_cluster$FS_year==2011,]

write.csv(test,"data/clean/tz_test.csv")
write.csv(train,"data/clean/tz_train.csv")

