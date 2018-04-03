##################################################################
# Goal : This script aims to clean up wfp price data, impute missing prices, generate mkt_thiness measures and then match them to different levels 
# purpose: use the clean market price and thinness measure to generate the most relevant price at the cluster level, TA level and IPC zone 

# Input : 
# 1. raw csv files downloaded from wfp price 
# 2. market coordinates generated from market_coordinates.R 
# 3. coordinates of clusters. 
# 4. shapefile of livelihood zones. 
# 


# Output: 
# 1. a df of price by product by mkt by yearmon, with missing imputed by nearest market or interpolated by recent months
# 2. a df of market thinness measures by product by mkt by yearmon   
# 3. matching the price and mkt_thinness to the cluster and ipczone level 
# 
# Yujun Zhou -  03/20/18
###################################################################

library(dplyr)

source("R/functions/Yearmon.R") 
source("R/functions/market_transpose.R") 
source("R/functions/NearMkt.R") 
source("R/functions/spatial_price_impute.R") 



######################################################
# general cleaning
######################################################

# read in the price data
price_raw<-read.csv("data/raw/price/price_ethiopia_tanzania_uganda.csv")
price_raw[price_raw==0]<-NA 

# head(price_raw)

# subset the countries 

tanzania_price = price_raw %>% 
  dplyr::filter(adm0_name == "United Republic of Tanzania")

uganda_price =  price_raw %>% 
  dplyr::filter(adm0_name == "Uganda")


# subset columns
country_list<-list(tanzania_price,uganda_price)
country_list <- lapply(country_list, function(x){
  x%>% dplyr::select(mkt_name, cm_name,mp_month,mp_year,mp_price )
  })

# long to wide, i.e. put the price of different commodities into their own columns 
country_list <- lapply(country_list, function(x){
  x%>% tidyr::spread(key = cm_name, value = mp_price) 
})


# generate Date and yearmon variable to help with join 
country_list <- lapply(country_list, function(x){
  yearmon(x,"mp_year","mp_month")
})


# separe each commodity to different data frames

tanzania_bean = country_list[[1]] %>% 
  dplyr::select(mkt_name,Beans,date)

tanzania_maize= country_list[[1]] %>% 
  dplyr::select(mkt_name,Maize,date)

tanzania_rice= country_list[[1]] %>% 
  dplyr::select(mkt_name,Rice,date)

colnames( country_list[[2]])<-c("mkt_name","mp_month", "mp_year", "Beans","Cassava","Maize","Maizeflour","Millet"  ,  "Sorghum" ,  "yearmon", "date" )  

uganda_bean = country_list[[2]] %>% 
  dplyr::select(mkt_name,Beans,date)

uganda_maize= country_list[[2]] %>% 
  dplyr::select(mkt_name,Maize,date)

uganda_cassava= country_list[[2]] %>% 
  dplyr::select(mkt_name,Cassava,date)


uganda_maizeflour= country_list[[2]] %>% 
  dplyr::select(mkt_name,Maizeflour,date)

uganda_millet= country_list[[2]] %>% 
  dplyr::select(mkt_name,Millet,date)

uganda_sorghum= country_list[[2]] %>% 
  dplyr::select(mkt_name,Sorghum ,date)




tanzania_prices<-list(tanzania_bean,tanzania_maize,tanzania_rice)
uganda_prices<-list(uganda_bean,uganda_maize,uganda_cassava,uganda_maizeflour,uganda_millet,uganda_sorghum)



source("R/functions/market_transpose.R") 

tanzania_prices_trans <- lapply(tanzania_prices, function(x){
  market_transpose(x)
})
 
uganda_prices_trans <- lapply(uganda_prices, function(x){
  market_transpose(x)
})


####################################
# impute price by the nearest market 
####################################

# read in the mkt coordinates 

mkt_coord_TZN<-read.csv("data/clean/market/mkt_coord_TZN.csv")
mkt_coord_ug<-read.csv("data/clean/market/mkt_coord_ug.csv")

# find the nearest mkt for each market using NearMKt function
source("R/functions/NearMkt.R") 
near_ug =  NearMkt(mkt_coord_ug)
near_tzn = NearMkt(mkt_coord_TZN)
 
# impute missing price by the price of the nearest mkt using SpatialPriceImpu function
source("R/functions/spatial_price_impute.R") 
uganda_prices_imputed <- lapply(uganda_prices_trans, function(x){
  SpatialPriceImpu(x,near_ug)
})
tanzania_prices_imputed <- lapply(tanzania_prices_trans, function(x){
  SpatialPriceImpu(x,near_tzn)
})


write.csv(tanzania_prices_imputed[[1]],"data/clean/market/tanzania_bean_price.csv" )
write.csv(tanzania_prices_imputed[[2]],"data/clean/market/tanzania_maize_price.csv" )
write.csv(tanzania_prices_imputed[[3]],"data/clean/market/tanzania_rice_price.csv" )

write.csv(uganda_prices_imputed[[1]],"data/clean/market/uganda_bean_price.csv" )
write.csv(uganda_prices_imputed[[2]],"data/clean/market/uganda_maize_price.csv" )
write.csv(uganda_prices_imputed[[3]],"data/clean/market/uganda_cassava_price.csv" )
write.csv(uganda_prices_imputed[[4]],"data/clean/market/uganda_maizeflour_price.csv" )
write.csv(uganda_prices_imputed[[5]],"data/clean/market/uganda_millet_price.csv" )
write.csv(uganda_prices_imputed[[6]],"data/clean/market/uganda_sorghum_price.csv" )

   
########################################
# mkt_thinness measure for each market 
#######################################
# mkt thinness for each market 
# number of missings for each yearmon for each mkt 
 
tanzania_mktthin<- lapply(tanzania_prices_trans,function(x){ifelse(is.na(x), 1, 0)})
for (i in 1:length(tanzania_prices_trans)) {
  tanzania_mktthin[[i]][,1]<-tanzania_prices_trans[[i]][,1]
}

uganda_mktthin<- lapply(uganda_prices_trans,function(x){ifelse(is.na(x), 1, 0)})
for (i in 1:length(uganda_prices_trans)) {
  uganda_mktthin[[i]][,1]<-uganda_prices_trans[[i]][,1]
}
 

write.csv(tanzania_mktthin[[1]],"data/clean/market/tanzania_bean_mktthin.csv" )
write.csv(tanzania_mktthin[[2]],"data/clean/market/tanzania_maize_mktthin.csv" )
write.csv(tanzania_mktthin[[3]],"data/clean/market/tanzania_rice_mktthin.csv" )

write.csv(uganda_mktthin[[1]],"data/clean/market/uganda_bean_mktthin.csv" )
write.csv(uganda_mktthin[[2]],"data/clean/market/uganda_maize_mktthin.csv" )
write.csv(uganda_mktthin[[3]],"data/clean/market/uganda_cassava_mktthin.csv" )
write.csv(uganda_mktthin[[4]],"data/clean/market/uganda_maizeflour_mktthin.csv" )
write.csv(uganda_mktthin[[5]],"data/clean/market/uganda_millet_mktthin.csv" )
write.csv(uganda_mktthin[[6]],"data/clean/market/uganda_sorghum_mktthin.csv" )



################################################################################
# link price and mkt_thinness measure for livelihood zones with population weights 
###############################################################################


landscan_pop <- raster("shapefiles/LandScanData/Population/lspop2011") # land scan data 

markets_theissen_TZ <- readOGR("shapefiles/livelihood_zone/TZ_LHZ_2009/thiessen_TZ_clip.shp")    # Tanzania thiessen polygons 
lhz_TZ <- readOGR("shapefiles/livelihood_zone/TZ_LHZ_2009/TZ_LHZ_2009_proj.shp")                  # Tanzania livelihood zones       
lhz_TZ_intersect <- readOGR("shapefiles/livelihood_zone/TZ_LHZ_2009/intersect/tz_intersect.shp")  # intersection       



markets_theissen_UG <- readOGR("shapefiles/livelihood_zone/UG_LHZ_2011/thiessen_ug_clip.shp")    #Uganda thiessen polygons
lhz_UG <- readOGR("shapefiles/livelihood_zone/UG_LHZ_2011/UG_LHZ_2011_proj.shp")                # Uganda  livelihood zones  
lhz_UG_intersect <- readOGR("shapefiles/livelihood_zone/UG_LHZ_2011/intersect/ug_intersect.shp")  # intersection       




 