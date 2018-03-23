##################################################################
# Goal : retrieve market geoordinates information 
# input : raw csv files downloaded from wfp price 
# output:  a list of market geoordinates for the given countries. 
# 
# Yujun Zhou -  03/22/18
###################################################################
# read in the price data, 


library(dplyr)
#import url, geoCode, coordFind functions 
source("R/functions/GoogleMapApi.r") 

# subset 
price_raw<-read.csv("data/raw/price/price_ethiopia_tanzania_uganda.csv")
# head(price_raw)
price_master =  dplyr::select(price_raw,adm0_name, mkt_name, cm_name,mp_month,mp_year,mp_price )

tanzania_price = dplyr::filter(price_master,adm0_name == "United Republic of Tanzania")
uganda_price = dplyr::filter(price_master,adm0_name == "Uganda")

# find the geo-coordinates of the markets in TZN using google map api
market_names_tan<-unique(tanzania_price$mkt_name)
market_names_tan<- as.character(market_names_tan)

market_list_tanzania<- lapply(market_names_tan, function(x){paste("market",x,sep=" ")})
market_list_tanzania = unlist (market_list_tanzania)
market_list_tanzania

address_tanzania<- lapply(market_list_tanzania, function(x){paste(x,"Tanzania",sep=",")})
address_tanzania = unlist (address_tanzania)
address_tanzania

coord_tanzania = coordFind(address_tanzania)
coord_tanzania$mkt  = market_names_tan
# -4.816988, 34.750763
coord_tanzania[coord_tanzania$mkt =="Singida",]$lat = -4.816988
coord_tanzania[coord_tanzania$mkt =="Singida",]$lon = 34.750763
coord_tanzania

mkt_coord_TZN = dplyr::select(coord_tanzania,lat,lon,mkt)
write.csv(x= mkt_coord_TZN,file = "data/clean/market/mkt_coord_TZN.csv")

# find the geo-coordinates of the markets using google map api

market_names_ug<-unique(uganda_price$mkt_name)
market_names_ug<- as.character(market_names_ug)

market_list_ug<- lapply(market_names_ug, function(x){paste("market",x,sep=" ")})
market_list_ug = unlist (market_list_ug)
market_list_ug

address_ug<- lapply(market_list_ug, function(x){paste(x,"Uganda",sep=",")})
address_ug = unlist (address_ug)
address_ug

coord_ug = coordFind(address_ug)
coord_ug$mkt  = market_names_ug

coord_ug
# 2.772771, 32.301031

coord_ug[coord_ug$mkt =="Gulu",]$lat = 2.772771
coord_ug[coord_ug$mkt =="Gulu",]$lon = 32.301031
coord_ug

mkt_coord_ug = dplyr::select(coord_ug,lat,lon,mkt)
write.csv(x= mkt_coord_ug,file = "data/clean/market/mkt_coord_ug.csv")


