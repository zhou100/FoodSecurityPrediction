##################################################################
# Goal : This script aims to clean up wfp price data, impute missing prices, generate mkt_thiness measures and then match them to different geospatial levels 
# purpose: use the clean market price and thinness measure to generate the most relevant price at the cluster level, TA level and IPC zone 

# Input : 
# 1. raw csv files downloaded from wfp price 
# 2. population density raster data 
# 3. coordinates of clusters. 
# 4. shapefile of livelihood zones. 

# Output: 
# 0. market coordinates generated from market_coordinates.R 
# 1. a df of price by product by mkt by yearmon, with missing imputed by nearest market or interpolated by recent months
# 2. a df of market thinness measures by product by mkt by yearmon   
# 3. matching the price and mkt_thinness to the cluster and ipczone level 
# 
# Yujun Zhou -  03/20/18
###################################################################

rm(list=ls())

package = c("plyr","dplyr","maptools","rgeos", "rgdal", "raster","FastKNN","geosphere")

lapply(package, require, character.only = TRUE)


source("R/functions/Yearmon.R") 
source("R/functions/market_transpose.R") 
source("R/functions/NearMkt.R") 
source("R/functions/spatial_price_impute.R") 
source("R/functions/PopuWeight.R") 
source("R/functions/NameToPrice.R") 
source("R/functions/WeightedPrice.R") 
source("R/functions/MktReshape.R") 

##################################################################
#  1. get market coordinates 
##################################################################

##################################################################
# Goal : retrieve market geoordinates information 
# input : raw csv files downloaded from wfp price 
# output:  a list of market geoordinates for the given countries. 
###################################################################
source("R/functions/GoogleMapApi.r") 
map.key = ""


# subset 
price_raw<-read.csv("data/raw/price/price_ethiopia_tanzania_uganda.csv")
# head(price_raw)
price_master =  dplyr::select(price_raw,adm0_name, mkt_name, cm_name,mp_month,mp_year,mp_price )

tanzania_price = dplyr::filter(price_master,adm0_name == "United Republic of Tanzania")
uganda_price = dplyr::filter(price_master,adm0_name == "Uganda")

malawi.maize.price = read.csv("data/raw/price/malawi/maize_joined_0817.csv")


# find the geo-coordinates of the markets in TZN using google map api
market_names_tan<-unique(tanzania_price$mkt_name)
market_names_tan<- as.character(market_names_tan)

market_list_tanzania<- lapply(market_names_tan, function(x){paste("market",x,sep=" ")})
market_list_tanzania = unlist (market_list_tanzania)
 
address_tanzania<- lapply(market_list_tanzania, function(x){paste(x,"Tanzania",sep=",")})
address_tanzania = unlist (address_tanzania)
 
coord_tanzania = coordFind(address_tanzania)
coord_tanzania$mkt  = market_names_tan

coord_tanzania

# # -4.816988, 34.750763
# coord_tanzania[coord_tanzania$mkt =="Singida",]$lat = -4.816988
# coord_tanzania[coord_tanzania$mkt =="Singida",]$lon = 34.750763

coord_tanzania[coord_tanzania$mkt =="Lindi",]$lat = -9.996098
coord_tanzania[coord_tanzania$mkt =="Lindi",]$lon = 39.713580

mkt_coord_TZN = dplyr::select(coord_tanzania,lat,lon,mkt)
mkt_coord_TZN
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

# coord_ug[coord_ug$mkt =="Gulu",]$lat = 2.772771
# coord_ug[coord_ug$mkt =="Gulu",]$lon = 32.301031


#0.916289, 31.766471
coord_ug[coord_ug$mkt =="Kiboga",]$lat = 0.916289
coord_ug[coord_ug$mkt =="Kiboga",]$lon = 31.766471

coord_ug

mkt_coord_ug = dplyr::select(coord_ug,lat,lon,mkt)
write.csv(x= mkt_coord_ug,file = "data/clean/market/mkt_coord_ug.csv")



# find the geo-coordinates of the markets in Malawi using google map api
mkt.names.mw = colnames(malawi.maize.price)
mkt.names.mw = mkt.names.mw[6:length(mkt.names.mw)]
mkt.names.mw[42] = "BEMBEKE"
mkt.names.mw[45]= "TSANGANO"
mkt.names.mw[51]= "MONKEY BAY"

mkt.names.mw.lower = unlist(lapply(mkt.names.mw,tolower))

mkt.list.mw <- lapply(mkt.names.mw.lower, function(x){paste("grocery",x,sep=" ")})
mkt.list.mw = unlist (mkt.list.mw)


address.mw<- lapply(mkt.list.mw, function(x){paste(x,"Malawi",sep=",")})
address.mw = unlist (address.mw)
address.mw

coord.mw = coordFind(address.mw)
coord.mw$mkt = mkt.names.mw
coord.mw

# HEWE	-11.19237,	33.46928
# CHATOLOMA	-12.81557795,	33.43434
# KASIYA	-13.76667,	33.38333
# CHIMBIYA	-14.59522,	35.7987
# BEMBEKE_TURNOFF	-14.403275,	34.36941
# SHARPEVALEY	-14.60627,	34.73305
# MAYAKA	-15.58018384,	35.3545
# EMBANGWENI	-12.166667,	33.46667


mkt.coord.mw = coord.mw %>% dplyr::select(lat,lon,mkt)

coord.mw[coord.mw$mkt =="HEWE",]$lat = -11.19237
coord.mw[coord.mw$mkt =="HEWE",]$lon = 33.46928

coord.mw[coord.mw$mkt =="CHATOLOMA",]$lat = -12.81557795
coord.mw[coord.mw$mkt =="CHATOLOMA",]$lon = 33.43434

coord.mw[coord.mw$mkt =="KASIYA",]$lat = -13.76667
coord.mw[coord.mw$mkt =="KASIYA",]$lon = 33.38333

coord.mw[coord.mw$mkt =="CHIMBIYA",]$lat = -14.59522
coord.mw[coord.mw$mkt =="CHIMBIYA",]$lon = 35.7987

coord.mw[coord.mw$mkt =="BEMBEKE",]$lat = -14.403275
coord.mw[coord.mw$mkt =="BEMBEKE",]$lon =34.36941


coord.mw[coord.mw$mkt =="SHARPEVALEY",]$lat = 	-14.60627
coord.mw[coord.mw$mkt =="SHARPEVALEY",]$lon =34.73305


coord.mw[coord.mw$mkt =="MAYAKA",]$lat = -15.58018384
coord.mw[coord.mw$mkt =="MAYAKA",]$lon =35.3545


coord.mw[coord.mw$mkt =="EMBANGWENI",]$lat = -12.166667
coord.mw[coord.mw$mkt =="EMBANGWENI",]$lon =33.46667

write.csv(x= coord.mw,file = "data/clean/market/mkt_coord_mw.csv")


#############################################################################################################
# 2. general cleaning (separete the raw data into by country and by commodity)
#############################################################################################################

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

source("R/functions/Yearmon.R") 
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


ug_newprice= read.csv("data/raw/price/ugdanda_price_0811.csv" )

ug_newprice["ï..Date"] = as.Date(ug_newprice[,1],format = "%m/%d/%Y")

cassava_price_ug_new = ug_newprice %>% dplyr::filter(Commodity == "Cassava, Flour")
cassava_price_ug_new = cassava_price_ug_new %>% dplyr::select(-Commodity)
colnames(cassava_price_ug_new) = c("date","mkt_name","Cassava")
cassava_price_ug_new  = cassava_price_ug_new[c( "mkt_name","Cassava","date")]
cassava_price_ug_new['mkt_name']<- as.character(cassava_price_ug_new$mkt_name)

maizeflour_price_ug_new = ug_newprice %>% dplyr::filter(Commodity == "Maize Flour")
maizeflour_price_ug_new = maizeflour_price_ug_new %>% dplyr::select(-Commodity)
colnames(maizeflour_price_ug_new) = c("date","mkt_name","Maizeflour")
maizeflour_price_ug_new  = maizeflour_price_ug_new[ c( "mkt_name","Maizeflour","date")]
maizeflour_price_ug_new['mkt_name']<- as.character(maizeflour_price_ug_new$mkt_name)


uganda_maizeflour = dplyr::bind_rows(uganda_maizeflour, maizeflour_price_ug_new)
uganda_maizeflour  = distinct(uganda_maizeflour)
uganda_cassava = dplyr::bind_rows(uganda_cassava, cassava_price_ug_new)
uganda_cassava  = distinct(uganda_cassava)



# save the prices in a list to make the loop easy 
tanzania_prices<-list(tanzania_bean,tanzania_maize,tanzania_rice)
uganda_prices<-list(uganda_bean,uganda_maize,uganda_cassava,uganda_maizeflour,uganda_millet,uganda_sorghum)
 

source("R/functions/market_transpose.R") 

tanzania_prices_trans <- lapply(tanzania_prices, function(x){
  market_transpose(x)
})
 
uganda_prices_trans <- lapply(uganda_prices, function(x){
  market_transpose(x)
})


malawi.maize.price = read.csv("data/raw/price/malawi/maize_joined_0817.csv")
malawi.rice.price = read.csv("data/raw/price/malawi/rice_joined_0817.csv")
malawi.nuts.price = read.csv("data/raw/price/malawi/nuts_joined_0817.csv")
malawi.beans.price = read.csv("data/raw/price/malawi/beans_joined_0817.csv")

mw.prices.list<-list(malawi.maize.price,malawi.rice.price,malawi.nuts.price,malawi.beans.price)

mw_mkt_transpose = function(df){
    df = df %>% dplyr::select(-X)
    df.trans = as.data.frame(t(df))
    colnames(df.trans)  =   as.character(unlist(df.trans[1,]))
    df.trans = df.trans[-(1:4),]
    df.trans = df.trans %>% tibble::rownames_to_column()
    colnames(df.trans)[1] = "mkt"    
    df.trans[["mkt"]][which(df.trans[["mkt"]]=="BEMBEKE.TURN.OFF")]="BEMBEKE"
    df.trans[["mkt"]][which(df.trans[["mkt"]]=="TSANGANO.TURN.OFF")]="TSANGANO"
    df.trans[["mkt"]][which(df.trans[["mkt"]]=="MONKEY.BAY")]="MONKEY BAY"
    return(df.trans)
}


mw_prices_trans <- lapply(mw.prices.list, function(x){
  mw_mkt_transpose(x)
})

########################################################################
# 3.  impute price by the nearest market 
########################################################################

# read in the mkt coordinates 



mkt_coord_TZN<-read.csv("data/clean/market/mkt_coord_TZN.csv")
mkt_coord_ug<-read.csv("data/clean/market/mkt_coord_ug.csv")
mkt_coord_mw<-read.csv("data/clean/market/mkt_coord_mw.csv")

# find the nearest mkt for each market using NearMKt function
source("R/functions/NearMkt.R") 
near_ug =  NearMkt(mkt_coord_ug)
near_tzn = NearMkt(mkt_coord_TZN)
near_mw = NearMkt(mkt_coord_mw)

 
# impute missing price by the price of the nearest mkt using SpatialPriceImpu function
source("R/functions/spatial_price_impute.R") 
uganda_prices_imputed <- lapply(uganda_prices_trans, function(x){
  SpatialPriceImpu(x,near_ug)
})

tanzania_prices_imputed <- lapply(tanzania_prices_trans, function(x){
  SpatialPriceImpu(x,near_tzn)
})

mw_prices_imputed <- lapply(mw_prices_trans, function(x){
  SpatialPriceImpu(x,near_mw)
})
################################################################################
# 4. generate mkt_thinness measure for each market 
###############################################################################
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
 

mw_mktthin<- lapply(mw_prices_trans,function(x){ifelse(is.na(x), 1, 0)})
for (i in 1:length(mw_prices_trans)) {
  mw_mktthin[[i]][,1]<-mw_prices_trans[[i]][,1]
}



tan_names = c("bean","maize","rice")
mw_names = c("maize","rice","nuts","beans")
ug_names<-c("bean","maize","cassava","maizeflour","millet","sorghum")

path = "data/clean/market/impute_thin/"

###########################################################################################
####  write imputed prices
###########################################################################################
for (i in 1:length(tan_names)){
  write.csv(tanzania_prices_imputed[[i]], paste(path,paste(tan_names[i],"_price_tz.csv",sep = ""),sep = "" ),row.names=FALSE)
}


for (i in 1:length(ug_names)){
  write.csv(uganda_prices_imputed[[i]], paste(path,paste(ug_names[i],"_price_ug.csv",sep = ""),sep = "" ),row.names=FALSE)
}

for (i in 1:length(mw_names)){
  write.csv(mw_prices_imputed[[i]], paste(path,paste(mw_names[i],"_price_mw.csv",sep = ""),sep = "" ),row.names=FALSE)
}


###########################################################################################
####  write market thinness variable 
###########################################################################################
for (i in 1:length(mw_names)){
  write.csv(mw_mktthin[[i]], paste(path,paste(mw_names[i],"_mktthin_mw.csv",sep = ""),sep = "" ),row.names=FALSE)
}


for (i in 1:length(tan_names)){
  write.csv(tanzania_mktthin[[i]], paste(path,paste(tan_names[i],"_mktthin_tz.csv",sep = ""),sep = "" ),row.names=FALSE)
}

for (i in 1:length(ug_names)){
  write.csv(uganda_mktthin[[i]], paste(path,paste(ug_names[i],"_mktthin_ug.csv",sep = ""),sep = "" ),row.names=FALSE)
}


################################################################################
# 5.  livelihood zones with population weights for each market shed
###############################################################################


#landscan_pop <- raster("shapefiles/LandScanData/Population/lspop2011") # land scan data 

# library(curl);
# id <- "0B-wuZ2XMFIBUd09Ob0pKVkRzQTA";
# sURL <- sprintf("https://docs.google.com/uc?id=%s&export=download", id);
# con <- curl(sURL);
# read.csv(con)
 
 
landscan_pop <- raster("D:/LandScanData/Population/lspop2011") # land scan data (not uploaded)
 
lhz_TZ <- readOGR("shapefiles/livelihood_zone/TZ_LHZ_2009/TZ_LHZ_2009.shp")                  # Tanzania livelihood zones
lhz_TZ_intersect <- readOGR("shapefiles/livelihood_zone/TZ_LHZ_2009/intersect/tz_intersect.shp")  # intersection of lhz and market_thinness

lhz_UG <- readOGR("shapefiles/livelihood_zone/UG_LHZ_2011/UG_LHZ_2011.shp")                # Uganda  livelihood zones
lhz_UG_nowater = lhz_UG[lhz_UG$LZNAMEEN!="Lake and Open Water",]    # drop the lake area since no population there
lhz_UG_intersect <- readOGR("shapefiles/livelihood_zone/UG_LHZ_2011/intersect/ug_intersect.shp")  # intersection of lhz and market_thinness


lhz_mw <- readOGR("shapefiles/livelihood_zone/malawi/livelihood zone 2012/MW_Admin1_LHZ_2012.3/MW_Admin1_LHZ_2012.3.shp")                  # Tanzania livelihood zones
lhz_mw_intersect <- readOGR("shapefiles/livelihood_zone/malawi/mw_intersect.shp")  # intersection of lhz and market_thinness


 

source("R/functions/PopuWeight.R")
tz_popweight = PopuWeight(landscan_pop,lhz_TZ,lhz_TZ_intersect)
ug_popweight = PopuWeight(landscan_pop,lhz_UG_nowater,lhz_UG_intersect)
mw_popweight = PopuWeight(landscan_pop,lhz_mw,lhz_mw_intersect)

write.csv(mw_popweight,"data/clean/market/mw_popweight.csv" )
write.csv(tz_popweight,"data/clean/market/tz_popweight.csv" )
write.csv(ug_popweight,"data/clean/market/ug_popweight.csv" )

###############################################################################
# 6. link price and mkt_thinness measure for livelihood zones based on the pop weight computed above
###############################################################################
 
tz_popweight <- read.csv("data/clean/market/tz_popweight.csv")
ug_popweight <- read.csv("data/clean/market/ug_popweight.csv")
mw_popweight <- read.csv("data/clean/market/mw_popweight.csv")

path = "data/clean/market/impute_thin/"

file_list <- list.files(path=path,
                        pattern = "csv$",
                        full.names=FALSE)
dfnames<-file_list
dfnames <- gsub(".csv","", dfnames)


list2env(
  lapply(setNames(file_list, make.names(dfnames)),
         function(i){read.csv(paste(path,i,sep=""),stringsAsFactors = FALSE)}), envir = .GlobalEnv)


# save the prices in a list to make the loop easy

tz_prices_impu<-list(bean_price_tz,maize_price_tz,rice_price_tz)
ug_prices_impu<-list(bean_price_ug,maize_price_ug,cassava_price_ug,maizeflour_price_ug,millet_price_ug,sorghum_price_ug)
mw_prices_impu<-list(maize_price_mw,rice_price_mw,nuts_price_mw,beans_price_mw)


tz_mktthin<-list(bean_mktthin_tz,maize_mktthin_tz,rice_mktthin_tz)
ug_mktthin<-list(bean_mktthin_ug,maize_mktthin_ug,cassava_mktthin_ug,maizeflour_mktthin_ug,millet_mktthin_ug,sorghum_mktthin_ug)
mw_mktthin<-list(maize_mktthin_mw,rice_mktthin_mw,nuts_mktthin_mw,beans_mktthin_mw)



source("R/functions/NameToPrice.R")

tz_lhz_price_unweight <- lapply(tz_prices_impu, function(x){
  NameToPrice(tz_popweight,x)
})

for (index in 1:length(tz_lhz_price_unweight)){
  tz_lhz_price_unweight[[index]] = tz_lhz_price_unweight[[index]] %>% dplyr::select(-X.x,-X.y)  
}


ug_lhz_price_unweight <- lapply(ug_prices_impu, function(x){
  NameToPrice(ug_popweight,x)
})

for (index in 1:length(ug_lhz_price_unweight)){
  ug_lhz_price_unweight[[index]] = ug_lhz_price_unweight[[index]] %>% dplyr::select(-X.x,-X.y)  
}


mw_lhz_price_unweight <- lapply(mw_prices_impu, function(x){
  NameToPrice(mw_popweight,x)
})

for (index in 1:length(mw_lhz_price_unweight)){
  mw_lhz_price_unweight[[index]] = mw_lhz_price_unweight[[index]] %>% dplyr::select(-X.x,-X.y)  
}



tz_lhz_mktthin_unweight <- lapply(tz_mktthin, function(x){
  NameToPrice(tz_popweight,x)
})


ug_lhz_mktthin_unweight <- lapply(ug_mktthin, function(x){
  NameToPrice(ug_popweight,x)
})

mw_lhz_mktthin_unweight <- lapply(mw_mktthin, function(x){
  NameToPrice(mw_popweight,x)
})

source("R/functions/WeightedPrice.R")


tz_lhz_price <- lapply(tz_lhz_price_unweight, function(x){
  WeightedPrice(x)
})

ug_lhz_price <- lapply(ug_lhz_price_unweight, function(x){
  WeightedPrice(x)
})

mw_lhz_price <- lapply(mw_lhz_price_unweight, function(x){
  WeightedPrice(x)
})



tz_lhz_mktthin <- lapply(tz_lhz_mktthin_unweight, function(x){
  WeightedPrice(x)
})

ug_lhz_mktthin <- lapply(ug_lhz_mktthin_unweight, function(x){
  WeightedPrice(x)
})

mw_lhz_mktthin <- lapply(ug_lhz_mktthin_unweight, function(x){
  WeightedPrice(x)
})

tan_names = c("bean","maize","rice")
ug_names<-c("bean","maize","cassava","maizeflour","millet","sorghum")
mw_names = c("maize","rice","nuts","beans")

path = "data/clean/market/lhz_prices/"

 for (i in 1:length(tan_names)){
   write.csv(tz_lhz_price[[i]], paste(path,paste(tan_names[i],"_lhz_price_tz.csv",sep = ""),sep = "" ),row.names=FALSE)
 }


for (i in 1:length(ug_names)){
  write.csv(ug_lhz_price[[i]], paste(path,paste(ug_names[i],"_lhz_price_ug.csv",sep = ""),sep = "" ),row.names=FALSE)
}

for (i in 1:length(mw_names)){
  write.csv(mw_lhz_price[[i]], paste(path,paste(mw_names[i],"_lhz_price_mw.csv",sep = ""),sep = "" ),row.names=FALSE)
}


for (i in 1:length(tan_names)){
  write.csv(tz_lhz_mktthin[[i]], paste(path,paste(tan_names[i],"_lhz_mktthin_tz.csv",sep = ""),sep = "" ),row.names=FALSE )
}

for (i in 1:length(ug_names)){
  write.csv(ug_lhz_mktthin[[i]], paste(path,paste(ug_names[i],"_lhz_mktthin_ug.csv",sep = ""),sep = "" ),row.names=FALSE )
}

for (i in 1:length(mw_names)){
  write.csv(mw_lhz_mktthin[[i]], paste(path,paste(mw_names[i],"_lhz_mktthin_mw.csv",sep = ""),sep = "" ),row.names=FALSE )
}

#


###############################################################################
# 7. link price and mkt_thinness measure at the Cluster level 
###############################################################################
# need a concordance table with cluster and its nearest mkt using MktNearCluster function 
rm(list=ls())
gc()

# Market geo-coordinates 
mkt_coord_tz<-read.csv("data/clean/market/mkt_coord_TZN.csv") %>% dplyr::select(-X)
mkt_coord_ug<-read.csv("data/clean/market/mkt_coord_ug.csv") %>% dplyr::select(-X)
mkt_coord_mw<-read.csv("data/clean/market/mkt_coord_mw.csv") %>% dplyr::select(-X)

# cluster geo-coordinates 
clust_coord_tz<-read.csv("data/clean/concordance/tz_coordiantes.csv")
clust_coord_ug<-read.csv("data/clean/concordance/ug_coordiantes.csv")
clust_coord_mw<-read.csv("data/clean/concordance/mw_coordiantes.csv")

clust_coord_ug = na.omit(clust_coord_ug)
clust_coord_tz = na.omit(clust_coord_tz)
clust_coord_mw = na.omit(clust_coord_mw)


source("R/functions/MktNearCluster.R") 

# cluster geo-coordinates 
cluster_mkt_concord_tz = MktNearCluster(clust_coord_tz,mkt_coord_tz)
cluster_mkt_concord_ug = MktNearCluster(clust_coord_ug,mkt_coord_ug)
cluster_mkt_concord_mw = MktNearCluster(clust_coord_mw,mkt_coord_mw)


colnames(cluster_mkt_concord_tz)[colnames(cluster_mkt_concord_tz)=="near_mkt"] = "mkt"

colnames(cluster_mkt_concord_ug)[colnames(cluster_mkt_concord_ug)=="near_mkt"] = "mkt"
colnames(cluster_mkt_concord_mw)[colnames(cluster_mkt_concord_mw)=="near_mkt"] = "mkt"

# read in the imputed price data 
path = "data/clean/market/impute_thin/"

file_list <- list.files(path=path, 
                        pattern = "csv$",
                        full.names=FALSE)
dfnames<-file_list
dfnames <- gsub(".csv","", dfnames)


list2env(
  lapply(setNames(file_list, make.names(dfnames)), 
         function(i){read.csv(paste(path,i,sep=""))}), envir = .GlobalEnv)


# save the prices in a list to make the loop easy 

tz_prices_impu<-list(bean_price_tz,maize_price_tz,rice_price_tz)
ug_prices_impu<-list(bean_price_ug,maize_price_ug,cassava_price_ug,maizeflour_price_ug,millet_price_ug,sorghum_price_ug)
mw_prices_impu<-list(maize_price_mw,rice_price_mw,nuts_price_mw,beans_price_mw)


tz_mktthin<-list(bean_mktthin_tz,maize_mktthin_tz,rice_mktthin_tz)
ug_mktthin<-list(bean_mktthin_ug,maize_mktthin_ug,cassava_mktthin_ug,maizeflour_mktthin_ug,millet_mktthin_ug,sorghum_mktthin_ug)
mw_mktthin<-list(maize_mktthin_mw,rice_mktthin_mw,nuts_mktthin_mw,beans_mktthin_mw)


source("R/functions/NameToPrice.R") 

tz_cluster_price<- lapply(tz_prices_impu, function(x){
  NameToPrice(cluster_mkt_concord_tz,x)
})


tan_names = c("bean","maize","rice")
ug_names<-c("bean","maize","cassava","maizeflour","millet","sorghum")
mw_names = c("maize","rice","nuts","beans")

dir.create("data/clean/market/cluster_prices")
path = "data/clean/market/cluster_prices/"


for (i in 1:length(tan_names)){
  write.csv(tz_cluster_price[[i]], paste(path,paste(tan_names[i],"_clust_price_tz.csv",sep = ""),sep = "" ),row.names=FALSE)
}

gc()



ug_cluster_price <- lapply(ug_prices_impu, function(x){
  NameToPrice(cluster_mkt_concord_ug,x)
})

for (i in 1:length(ug_names)){
  write.csv(ug_cluster_price[[i]], paste(path,paste(ug_names[i],"_clust_price_ug.csv",sep = ""),sep = "" ),row.names=FALSE)
}



gc()


mw_cluster_price <- lapply(mw_prices_impu, function(x){
  NameToPrice(cluster_mkt_concord_mw,x)
})

for (i in 1:length(mw_names)){
  write.csv(mw_cluster_price[[i]], paste(path,paste(mw_names[i],"_clust_price_mw.csv",sep = ""),sep = "" ),row.names=FALSE)
}

gc()


tz_cluster_mktthin <- lapply(tz_mktthin, function(x){
  NameToPrice(cluster_mkt_concord_tz,x)
})

for (i in 1:length(tan_names)){
  write.csv(tz_cluster_mktthin[[i]], paste(path,paste(tan_names[i],"_clust_mktthin_tz.csv",sep = ""),sep = "" ),row.names=FALSE)
}

gc()


ug_cluster_mktthin <- lapply(ug_mktthin, function(x){
  NameToPrice(cluster_mkt_concord_ug,x)
})


for (i in 1:length(ug_names)){
  write.csv(ug_cluster_mktthin[[i]], paste(path,paste(ug_names[i],"_clust_mktthin_ug.csv",sep = ""),sep = "" ),row.names=FALSE)
}

gc()


mw_cluster_mktthin <- lapply(mw_mktthin, function(x){
  NameToPrice(cluster_mkt_concord_mw,x)
})



for (i in 1:length(mw_names)){
  write.csv(mw_cluster_mktthin[[i]], paste(path,paste(mw_names[i],"_clust_mktthin_mw.csv",sep = ""),sep = "" ),row.names=FALSE)
}

gc()









# maize_clust_price_tz= read.csv("data/clean/market/cluster_prices/maize_clust_price_tz.csv")
# maize_clust_mktthin_tz= read.csv("data/clean/market/cluster_prices/maize_clust_mktthin_tz.csv")
# 
# rice_clust_price_tz= read.csv("data/clean/market/cluster_prices/rice_clust_price_tz.csv")
# rice_clust_mktthin_tz= read.csv("data/clean/market/cluster_prices/rice_clust_mktthin_tz.csv")
# 
# maize_lhz_price_tz= read.csv("data/clean/market/lhz_prices/maize_lhz_price_tz.csv")
# position = str_which(colnames(maize_lhz_price_tz), "weights") 
# colnames(maize_lhz_price_tz)[position] = "X.1"
# 
# maize_lhz_mktthin_tz= read.csv("data/clean/market/lhz_prices/maize_lhz_mktthin_tz.csv")
# position = str_which(colnames(maize_lhz_mktthin_tz), "weights") 
# colnames(maize_lhz_mktthin_tz)[position] = "X.1"
# 
# rice_lhz_price_tz= read.csv("data/clean/market/lhz_prices/rice_lhz_price_tz.csv")
# position = str_which(colnames(rice_lhz_price_tz), "weights") 
# colnames(rice_lhz_price_tz)[position] = "X.1"
# 
# 
# rice_lhz_mktthin_tz= read.csv("data/clean/market/lhz_prices/rice_lhz_mktthin_tz.csv")
# position = str_which(colnames(rice_lhz_mktthin_tz), "weights") 
# colnames(rice_lhz_mktthin_tz)[position] = "X.1"


###############################################################################
# 8. Transpose all the prices 
###############################################################################

rm(list=ls())
gc()

path = "data/clean/market/cluster_prices/"

file_list <- list.files(path=path, 
                        pattern = "csv$",
                        full.names=FALSE)
dfnames<-file_list
dfnames <- gsub(".csv","", dfnames)


list2env(
  lapply(setNames(file_list, make.names(dfnames)), 
         function(i){read.csv(paste(path,i,sep=""))}), envir = .GlobalEnv)




path = "data/clean/market/lhz_prices/"

file_list <- list.files(path=path, 
                        pattern = "csv$",
                        full.names=FALSE)
dfnames<-file_list
dfnames <- gsub(".csv","", dfnames)


list2env(
  lapply(setNames(file_list, make.names(dfnames)), 
         function(i){read.csv(paste(path,i,sep=""))}), envir = .GlobalEnv)



tan_names = c("bean","maize","rice")
ug_names<-c("bean","maize","cassava","maizeflour","millet","sorghum")
mw_names = c("maize","rice","nuts","beans")

tz_clust_price<-list(bean_clust_price_tz,maize_clust_price_tz,rice_clust_price_tz)
ug_clust_price<-list(bean_clust_price_ug,maize_clust_price_ug,cassava_clust_price_ug,maizeflour_clust_price_ug,millet_clust_price_ug,sorghum_clust_price_ug)
mw_clust_price<-list(maize_clust_price_mw,rice_clust_price_mw,nuts_clust_price_mw,beans_clust_price_mw)

tz_clust_mktthin<-list(bean_clust_mktthin_tz,maize_clust_mktthin_tz,rice_clust_mktthin_tz)
ug_clust_mktthin<-list(bean_clust_mktthin_ug,maize_clust_mktthin_ug,cassava_clust_mktthin_ug,maizeflour_clust_mktthin_ug,millet_clust_mktthin_ug,sorghum_clust_mktthin_ug)
mw_clust_mktthin<-list(maize_clust_mktthin_mw,rice_clust_mktthin_mw,nuts_clust_mktthin_mw,beans_clust_mktthin_mw)

tz_lhz_price<-list(bean_lhz_price_tz,maize_lhz_price_tz,rice_lhz_price_tz)
ug_lhz_price<-list(bean_lhz_price_ug,maize_lhz_price_ug,cassava_lhz_price_ug,maizeflour_lhz_price_ug,millet_lhz_price_ug,sorghum_lhz_price_ug)
mw_lhz_price<-list(maize_lhz_price_mw,rice_lhz_price_mw,nuts_lhz_price_mw,beans_lhz_price_mw)

tz_lhz_mktthin<-list(bean_lhz_mktthin_tz,maize_lhz_mktthin_tz,rice_lhz_mktthin_tz)
ug_lhz_mktthin<-list(bean_lhz_mktthin_ug,maize_lhz_mktthin_ug,cassava_lhz_mktthin_ug,maizeflour_lhz_mktthin_ug,millet_lhz_mktthin_ug,sorghum_lhz_mktthin_ug)
mw_lhz_mktthin<-list(maize_lhz_mktthin_mw,rice_lhz_mktthin_mw,nuts_lhz_mktthin_mw,beans_lhz_mktthin_mw)


source("R/functions/MktReshape.R") 


clust_price_tz_long = lapply(tz_clust_price,MktReshape)
clust_mktthin_tz_long = lapply(tz_clust_mktthin,MktReshape)

clust_price_ug_long = lapply(ug_clust_price,MktReshape)
clust_mktthin_ug_long = lapply(ug_clust_mktthin,MktReshape)

 
clust_price_mw_long = lapply(mw_clust_price,MktReshape)
clust_mktthin_mw_long = lapply(mw_clust_mktthin,MktReshape)

lhz_price_tz_long = lapply(tz_lhz_price,MktReshape)
lhz_mktthin_tz_long = lapply(tz_lhz_mktthin,MktReshape)

lhz_price_ug_long = lapply(ug_lhz_price,MktReshape)
lhz_mktthin_ug_long = lapply(ug_lhz_mktthin,MktReshape)

lhz_price_mw_long = lapply(mw_lhz_price,MktReshape)
lhz_mktthin_mw_long = lapply(mw_lhz_mktthin,MktReshape)

 
 
# change column names 

tan_clust_price_names= paste("clust",tan_names,"price",sep = "_")
tan_lhz_price_names= paste("lhz",tan_names,"price",sep = "_")
tan_clust_thin_names= paste("clust",tan_names,"mktthin",sep = "_")
tan_lhz_thin_names= paste("lhz",tan_names,"mktthin",sep = "_")

ug_clust_price_names= paste("clust",ug_names,"price",sep = "_")
ug_lhz_price_names= paste("lhz",ug_names,"price",sep = "_")
ug_clust_thin_names= paste("clust",ug_names,"mktthin",sep = "_")
ug_lhz_thin_names= paste("lhz",ug_names,"mktthin",sep = "_")

mw_clust_price_names= paste("clust",mw_names,"price",sep = "_")
mw_lhz_price_names= paste("lhz",mw_names,"price",sep = "_")
mw_clust_thin_names= paste("clust",mw_names,"mktthin",sep = "_")
mw_lhz_thin_names= paste("lhz",mw_names,"mktthin",sep = "_")

for (i in 1:length(tan_clust_price_names)){
  colnames(clust_price_tz_long[[i]])[colnames(clust_price_tz_long[[i]])=="value"] =  tan_clust_price_names[i]
  colnames(clust_mktthin_tz_long[[i]])[colnames(clust_mktthin_tz_long[[i]])=="value"] =  tan_clust_thin_names[i]
  colnames(lhz_price_tz_long[[i]])[colnames(lhz_price_tz_long[[i]])=="value"] =  tan_lhz_price_names[i]
  colnames(lhz_mktthin_tz_long[[i]])[colnames(lhz_mktthin_tz_long[[i]])=="value"] =  tan_lhz_thin_names[i]
}

for (i in 1:length(ug_clust_price_names)){
  colnames(clust_price_ug_long[[i]])[colnames(clust_price_ug_long[[i]])=="value"] =  ug_clust_price_names[i]
  colnames(clust_mktthin_ug_long[[i]])[colnames(clust_mktthin_ug_long[[i]])=="value"] =  ug_clust_thin_names[i]
  colnames(lhz_price_ug_long[[i]])[colnames(lhz_price_ug_long[[i]])=="value"] =  ug_lhz_price_names[i]
  colnames(lhz_mktthin_ug_long[[i]])[colnames(lhz_mktthin_ug_long[[i]])=="value"] =  ug_lhz_thin_names[i]
}

for (i in 1:length(mw_clust_price_names)){
  colnames(clust_price_mw_long[[i]])[colnames(clust_price_mw_long[[i]])=="value"] =  mw_clust_price_names[i]
  colnames(clust_mktthin_mw_long[[i]])[colnames(clust_mktthin_mw_long[[i]])=="value"] =  mw_clust_thin_names[i]
  colnames(lhz_price_mw_long[[i]])[colnames(lhz_price_mw_long[[i]])=="value"] =  mw_lhz_price_names[i]
  colnames(lhz_mktthin_mw_long[[i]])[colnames(lhz_mktthin_mw_long[[i]])=="value"] =  mw_lhz_thin_names[i]
}



dir.create("data/clean/market/cluster_prices_long")
path = "data/clean/market/cluster_prices_long/"


saveRDS(clust_price_tz_long, paste(path,"clust_price_tz_long.rds",sep = '' ))
saveRDS(lhz_mktthin_tz_long, paste(path,"lhz_mktthin_tz_long.rds",sep = '' ))
saveRDS(clust_mktthin_tz_long, paste(path,"clust_mktthin_tz_long.rds",sep = '' ))
saveRDS(lhz_mktthin_tz_long, paste(path,"lhz_mktthin_tz_long.rds",sep = '' ))



saveRDS(clust_price_mw_long, paste(path,"clust_price_mw_long.rds",sep = '' ))
saveRDS(clust_mktthin_mw_long, paste(path,"clust_mktthin_mw_long.rds",sep = '' ))
saveRDS(lhz_price_mw_long, paste(path,"lhz_price_mw_long.rds",sep = '' ))
saveRDS(lhz_mktthin_mw_long, paste(path,"lhz_mktthin_mw_long.rds",sep = '' ))




saveRDS(clust_price_ug_long, paste(path,"clust_price_ug_long.rds",sep = '' ))
saveRDS(clust_mktthin_ug_long, paste(path,"clust_mktthin_ug_long.rds",sep = '' ))
saveRDS(lhz_price_ug_long, paste(path,"lhz_price_ug_long.rds",sep = '' ))
saveRDS(lhz_mktthin_ug_long, paste(path,"lhz_mktthin_ug_long.rds",sep = '' ))





 
###############################################################################
# 9. Merge all the prices  and save the data 
###############################################################################

# merge different prices in Tanzania 

rm(list=ls())
gc()

path = "data/clean/market/cluster_prices_long/"


clust_price_tz_long  = readRDS(paste(path,"clust_price_tz_long.rds",sep = '' ))
clust_mktthin_tz_long =  readRDS(paste(path,"clust_mktthin_tz_long.rds",sep = '' ))
lhz_price_tz_long = readRDS(paste(path,"lhz_price_tz_long.rds",sep = '' ))
lhz_mktthin_tz_long = readRDS(paste(path,"lhz_mktthin_tz_long.rds",sep = '' ))

tz_cluster_prices = dplyr::left_join(clust_price_tz_long[[1]],clust_price_tz_long[[2]])
tz_cluster_prices = dplyr::left_join(tz_cluster_prices,clust_price_tz_long[[3]])

tz_cluster_prices = dplyr::left_join(tz_cluster_prices,clust_mktthin_tz_long[[1]])
tz_cluster_prices = dplyr::left_join(tz_cluster_prices,clust_mktthin_tz_long[[2]])
tz_cluster_prices = dplyr::left_join(tz_cluster_prices,clust_mktthin_tz_long[[3]])


tz_lhz_prices = left_join(lhz_price_tz_long[[1]],lhz_price_tz_long[[2]])
tz_lhz_prices = left_join(tz_lhz_prices,lhz_price_tz_long[[3]])
tz_lhz_prices = left_join(tz_lhz_prices,lhz_mktthin_tz_long[[1]])
tz_lhz_prices = left_join(tz_lhz_prices,lhz_mktthin_tz_long[[2]])
tz_lhz_prices = left_join(tz_lhz_prices,lhz_mktthin_tz_long[[3]])


# tz_concordance <-  read.csv("data/clean/concordance/tz_ea_lhz .csv")
# # tz_concordance =  tz_concordance %>% dplyr::select(ea_id,FS_year,lat_modified lon_modified,FNID)%>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))
# tz_cluster_prices = tz_cluster_prices  %>% dplyr::distinct()%>% mutate( ea_id = as.character(ea_id) ) 
# 
# tz_concordance = tz_concordance %>% mutate( ea_id = as.character(ea_id) ) 
# tz_cluster_prices = dplyr::left_join(tz_cluster_prices,tz_concordance)
# 
# tz_price_merge_final = dplyr::left_join(tz_cluster_prices,tz_lhz_prices)  %>% arrange(ea_id,yearmon)

tz_price_merge_final = tz_cluster_prices

save(tz_price_merge_final,file = "data/clean/market/tz_price_final.RData")


# merge different prices in Uganda 

rm(list=ls())
gc()

path = "data/clean/market/cluster_prices_long/"


clust_price_ug_long = readRDS(paste(path,"clust_price_ug_long.rds",sep = '' ))
clust_mktthin_ug_long =  readRDS(paste(path,"clust_mktthin_ug_long.rds",sep = '' ))
lhz_price_ug_long = readRDS(paste(path,"lhz_price_ug_long.rds",sep = '' ))
lhz_mktthin_ug_long = readRDS(paste(path,"lhz_mktthin_ug_long.rds",sep = '' ))



ug_cluster_prices = dplyr::left_join(clust_price_ug_long[[1]],clust_price_ug_long[[2]])
ug_cluster_prices = dplyr::left_join(ug_cluster_prices,clust_price_ug_long[[3]])
ug_cluster_prices = dplyr::left_join(ug_cluster_prices,clust_mktthin_ug_long[[1]])
ug_cluster_prices = dplyr::left_join(ug_cluster_prices,clust_mktthin_ug_long[[2]])
ug_cluster_prices = dplyr::left_join(ug_cluster_prices,clust_mktthin_ug_long[[3]])


ug_lhz_prices = left_join(lhz_price_ug_long[[1]],lhz_price_ug_long[[2]])
ug_lhz_prices = left_join(ug_lhz_prices,lhz_price_ug_long[[3]])
ug_lhz_prices = left_join(ug_lhz_prices,lhz_mktthin_ug_long[[1]])
ug_lhz_prices = left_join(ug_lhz_prices,lhz_mktthin_ug_long[[2]])
ug_lhz_prices = left_join(ug_lhz_prices,lhz_mktthin_ug_long[[3]])

# 
# ug_concordance <-  read.csv("data/clean/concordance/ug_ea_lhz .csv")
# # ug_concordance =  ug_concordance %>% dplyr::select(ea_id,FNID)%>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))
# ug_cluster_prices = ug_cluster_prices  %>% dplyr::distinct()%>% mutate( ea_id = as.character(ea_id) ) 
# ug_cluster_prices = dplyr::left_join(ug_cluster_prices,ug_concordance)
# ug_price_merge_final = dplyr::left_join(ug_cluster_prices,ug_lhz_prices)  %>% arrange(ea_id,yearmon)


ug_price_merge_final = ug_cluster_prices

save(ug_price_merge_final,file = "data/clean/market/ug_price_final.RData")

# merge different prices in Malawi 

rm(list=ls())
gc()

path = "data/clean/market/cluster_prices_long/"


clust_price_mw_long = readRDS(paste(path,"clust_price_mw_long.rds",sep = '' ))
clust_mktthin_mw_long =  readRDS(paste(path,"clust_mktthin_mw_long.rds",sep = '' ))
lhz_price_mw_long = readRDS(paste(path,"lhz_price_mw_long.rds",sep = '' ))
lhz_mktthin_mw_long = readRDS(paste(path,"lhz_mktthin_mw_long.rds",sep = '' ))



mw_cluster_prices = dplyr::left_join(clust_price_mw_long[[1]],clust_price_mw_long[[2]])
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,clust_price_mw_long[[3]])
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,clust_price_mw_long[[4]])
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,clust_mktthin_mw_long[[1]])
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,clust_mktthin_mw_long[[2]])
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,clust_mktthin_mw_long[[3]])
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,clust_mktthin_mw_long[[4]])

mw_cluster_prices$clust_maize_mktthin= ifelse(is.na(mw_cluster_prices$clust_maize_price), 1, 0)
mw_cluster_prices$clust_rice_mktthin= ifelse(is.na(mw_cluster_prices$clust_rice_price), 1, 0)
mw_cluster_prices$clust_nuts_mktthin= ifelse(is.na(mw_cluster_prices$clust_nuts_price), 1, 0)
mw_cluster_prices$clust_beans_mktthin= ifelse(is.na(mw_cluster_prices$clust_beans_price), 1, 0)


mw_lhz_prices = left_join(lhz_price_mw_long[[1]],lhz_price_mw_long[[2]])
mw_lhz_prices = left_join(mw_lhz_prices,lhz_price_mw_long[[3]])
mw_lhz_prices = left_join(mw_lhz_prices,lhz_price_mw_long[[4]])
mw_lhz_prices = left_join(mw_lhz_prices,lhz_mktthin_mw_long[[1]])
mw_lhz_prices = left_join(mw_lhz_prices,lhz_mktthin_mw_long[[2]])
mw_lhz_prices = left_join(mw_lhz_prices,lhz_mktthin_mw_long[[3]])
mw_lhz_prices = left_join(mw_lhz_prices,lhz_mktthin_mw_long[[4]])

mw_lhz_prices$lhz_maize_mktthin= ifelse(is.na(mw_lhz_prices$lhz_maize_price), 1, 0)
mw_lhz_prices$lhz_rice_mktthin= ifelse(is.na(mw_lhz_prices$lhz_rice_price), 1, 0)
mw_lhz_prices$lhz_nuts_mktthin= ifelse(is.na(mw_lhz_prices$lhz_nuts_price), 1, 0)
mw_lhz_prices$lhz_beans_mktthin= ifelse(is.na(mw_lhz_prices$lhz_beans_price), 1, 0)

# 
# mw_concordance <-  read.csv("data/clean/concordance/mw_ea_lhz .csv")
# # mw_concordance =  mw_concordance %>% dplyr::select(ea_id,FNID)%>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))
# mw_cluster_prices = mw_cluster_prices  %>% dplyr::distinct()%>% mutate( ea_id = as.character(ea_id) ) 
# mw_cluster_prices = dplyr::left_join(mw_cluster_prices,mw_concordance)
# mw_price_merge_final = dplyr::left_join(mw_cluster_prices,mw_lhz_prices)  %>% arrange(ea_id,yearmon)

mw_price_merge_final = mw_cluster_prices

save(mw_price_merge_final,file = "data/clean/market/mw_price_final.RData")

