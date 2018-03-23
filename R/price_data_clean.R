##################################################################
# Goal : This script aims to clean up wfp price data, impute missing prices, generate mkt_thiness measures 
# purpose: use the clean market price and thinness measure to generate the most relevant price at the cluster level, TA level and IPC zone 

# Input : 
# 1. raw csv files downloaded from wfp price 
# 2. market coordinates generated from market_coordinates.R 
# 3. 

# Output: 
# 1. a df of price by product by mkt by yearmon, with missing imputed by nearest market or interpolated by recent months
# 2. a df of market thinness measures by product by mkt by yearmon   
# 
# Yujun Zhou -  03/20/18
###################################################################

library(dplyr)

##################
# general cleaning
##################

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
source("R/functions/Yearmon.R") 
country_list <- lapply(country_list, function(x){
  yearmon(x,"mp_year","mp_month")
})

 

####################################
# impute price by the nearest market 
####################################

# find the nearest mkt for each market 
# read in the mkt coordinates 

mkt_coord_TZN<-read.csv("data/clean/market/mkt_coord_TZN.csv")
mkt_coord_ug<-read.csv("data/clean/market/mkt_coord_ug.csv")

source("R/functions/NearMkt.R") 

NearMkt(mkt_coord_ug)

mkt_coord_TZN