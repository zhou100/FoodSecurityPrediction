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


package = c("dplyr","maptools","rgeos", "rgdal", "raster")
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
 
address_tanzania<- lapply(market_list_tanzania, function(x){paste(x,"Tanzania",sep=",")})
address_tanzania = unlist (address_tanzania)
 
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




########################################################################
# 3.  impute price by the nearest market 
########################################################################

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
 

tan_names = c("bean","maize","rice")
ug_names<-c("bean","maize","cassava","maizeflour","millet","sorghum")
path = "data/clean/market/impute_thin/"

for (i in 1:length(tan_names)){
  write.csv(tanzania_prices_imputed[[i]], paste(path,paste(tan_names[i],"_price_tz.csv",sep = ""),sep = "" ))
}


for (i in 1:length(ug_names)){
  write.csv(uganda_prices_imputed[[i]], paste(path,paste(ug_names[i],"_price_ug.csv",sep = ""),sep = "" ))
}

for (i in 1:length(tan_names)){
  write.csv(tanzania_mktthin[[i]], paste(path,paste(tan_names[i],"_mktthin_tz.csv",sep = ""),sep = "" ))
}

for (i in 1:length(ug_names)){
  write.csv(uganda_mktthin[[i]], paste(path,paste(ug_names[i],"_mktthin_ug.csv",sep = ""),sep = "" ))
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


source("R/functions/PopuWeight.R") 
tz_popweight = PopuWeight(landscan_pop,lhz_TZ,lhz_TZ_intersect)
ug_popweight = PopuWeight(landscan_pop,lhz_UG_nowater,lhz_UG_intersect)

write.csv(tz_popweight,"data/clean/market/tz_popweight.csv" )
write.csv(ug_popweight,"data/clean/market/ug_popweight.csv" )

###############################################################################
# 6. link price and mkt_thinness measure for livelihood zones based on the pop weight computed above
###############################################################################

tz_popweight <- read.csv("data/clean/market/tz_popweight.csv")
ug_popweight <- read.csv("data/clean/market/ug_popweight.csv")

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

tz_mktthin<-list(bean_mktthin_tz,maize_mktthin_tz,rice_mktthin_tz)
ug_mktthin<-list(bean_mktthin_ug,maize_mktthin_ug,cassava_mktthin_ug,maizeflour_mktthin_ug,millet_mktthin_ug,sorghum_mktthin_ug)



source("R/functions/NameToPrice.R") 

tz_lhz_price_unweight <- lapply(tz_prices_impu, function(x){
  NameToPrice(tz_popweight,x)
})


ug_lhz_price_unweight <- lapply(ug_prices_impu, function(x){
  NameToPrice(ug_popweight,x)
})

tz_lhz_mktthin_unweight <- lapply(tz_mktthin, function(x){
  NameToPrice(tz_popweight,x)
})


ug_lhz_mktthin_unweight <- lapply(ug_mktthin, function(x){
  NameToPrice(ug_popweight,x)
})



source("R/functions/WeightedPrice.R") 

tz_lhz_price <- lapply(tz_lhz_price_unweight, function(x){
  WeightedPrice(x)
})
ug_lhz_price <- lapply(ug_lhz_price_unweight, function(x){
  WeightedPrice(x)
})

tz_lhz_mktthin <- lapply(tz_lhz_mktthin_unweight, function(x){
  WeightedPrice(x)
})

ug_lhz_mktthin <- lapply(ug_lhz_mktthin_unweight, function(x){
  WeightedPrice(x)
})
 

tan_names = c("bean","maize","rice")
ug_names<-c("bean","maize","cassava","maizeflour","millet","sorghum")
path = "data/clean/market/lhz_prices/"

 for (i in 1:length(tan_names)){
   write.csv(tz_lhz_price[[i]], paste(path,paste(tan_names[i],"_lhz_price_tz.csv",sep = ""),sep = "" ))
 }
    

for (i in 1:length(ug_names)){
  write.csv(ug_lhz_price[[i]], paste(path,paste(ug_names[i],"_lhz_price_ug.csv",sep = ""),sep = "" ))
}

for (i in 1:length(tan_names)){
  write.csv(tz_lhz_mktthin[[i]], paste(path,paste(tan_names[i],"_lhz_mktthin_tz.csv",sep = ""),sep = "" ))
}

for (i in 1:length(ug_names)){
  write.csv(ug_lhz_mktthin[[i]], paste(path,paste(ug_names[i],"_lhz_mktthin_ug.csv",sep = ""),sep = "" ))
}
# 

###############################################################################
# 7. link price and mkt_thinness measure at the Cluster level 
###############################################################################
# need a concordance table with cluster and its nearest mkt using MktNearCluster function 

# Market geo-coordinates 
mkt_coord_TZN<-read.csv("data/clean/market/mkt_coord_TZN.csv")
mkt_coord_ug<-read.csv("data/clean/market/mkt_coord_ug.csv")


# cluster geo-coordinates 
clust_coord_tz<-read.csv("data/clean/Tanzania_coord.csv")
clust_coord_ug<-read.csv("data/clean/Uganda_coord.csv")

clust_coord_ug = na.omit(clust_coord_ug)
clust_coord_tz = na.omit(clust_coord_tz)

source("R/functions/MktNearCluster.R") 

# cluster geo-coordinates 
cluster_mkt_concord_tz = MktNearCluster(clust_coord_tz,mkt_coord_TZN)
cluster_mkt_concord_ug = MktNearCluster(clust_coord_ug,mkt_coord_ug)
colnames(cluster_mkt_concord_tz)[2] = "mkt"
colnames(cluster_mkt_concord_ug)[2] = "mkt"


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

tz_mktthin<-list(bean_mktthin_tz,maize_mktthin_tz,rice_mktthin_tz)
ug_mktthin<-list(bean_mktthin_ug,maize_mktthin_ug,cassava_mktthin_ug,maizeflour_mktthin_ug,millet_mktthin_ug,sorghum_mktthin_ug)



source("R/functions/NameToPrice.R") 

tz_cluster_price<- lapply(tz_prices_impu, function(x){
  NameToPrice(cluster_mkt_concord_tz,x)
})


ug_cluster_price <- lapply(ug_prices_impu, function(x){
  NameToPrice(cluster_mkt_concord_ug,x)
})

tz_cluster_mktthin <- lapply(tz_mktthin, function(x){
  NameToPrice(cluster_mkt_concord_tz,x)
})


ug_cluster_mktthin <- lapply(ug_mktthin, function(x){
  NameToPrice(cluster_mkt_concord_ug,x)
})



tan_names = c("bean","maize","rice")
ug_names<-c("bean","maize","cassava","maizeflour","millet","sorghum")
dir.create("data/clean/market/cluster_prices")
path = "data/clean/market/cluster_prices/"

for (i in 1:length(tan_names)){
  write.csv(tz_cluster_price[[i]], paste(path,paste(tan_names[i],"_clust_price_tz.csv",sep = ""),sep = "" ))
}


for (i in 1:length(ug_names)){
  write.csv(ug_cluster_price[[i]], paste(path,paste(ug_names[i],"_clust_price_ug.csv",sep = ""),sep = "" ))
}

for (i in 1:length(tan_names)){
  write.csv(tz_cluster_mktthin[[i]], paste(path,paste(tan_names[i],"_clust_mktthin_tz.csv",sep = ""),sep = "" ))
}

for (i in 1:length(ug_names)){
  write.csv(ug_cluster_mktthin[[i]], paste(path,paste(ug_names[i],"_cluster_mktthin_ug.csv",sep = ""),sep = "" ))
}
 


maize_clust_price_tz= read.csv("data/clean/market/cluster_prices/maize_clust_price_tz.csv")
maize_clust_mktthin_tz= read.csv("data/clean/market/cluster_prices/maize_clust_mktthin_tz.csv")

rice_clust_price_tz= read.csv("data/clean/market/cluster_prices/rice_clust_price_tz.csv")
rice_clust_mktthin_tz= read.csv("data/clean/market/cluster_prices/rice_clust_mktthin_tz.csv")

maize_lhz_price_tz= read.csv("data/clean/market/lhz_prices/maize_lhz_price_tz.csv")
position = str_which(colnames(maize_lhz_price_tz), "weights") 
colnames(maize_lhz_price_tz)[position] = "X.1"

maize_lhz_mktthin_tz= read.csv("data/clean/market/lhz_prices/maize_lhz_mktthin_tz.csv")
position = str_which(colnames(maize_lhz_mktthin_tz), "weights") 
colnames(maize_lhz_mktthin_tz)[position] = "X.1"

rice_lhz_price_tz= read.csv("data/clean/market/lhz_prices/rice_lhz_price_tz.csv")
position = str_which(colnames(rice_lhz_price_tz), "weights") 
colnames(rice_lhz_price_tz)[position] = "X.1"


rice_lhz_mktthin_tz= read.csv("data/clean/market/lhz_prices/rice_lhz_mktthin_tz.csv")
position = str_which(colnames(rice_lhz_mktthin_tz), "weights") 
colnames(rice_lhz_mktthin_tz)[position] = "X.1"

library(stringr)

clust_price_tz = list(maize_clust_price_tz,maize_clust_mktthin_tz,rice_clust_price_tz,rice_clust_mktthin_tz)
lhz_price_tz = list(maize_lhz_price_tz,maize_lhz_mktthin_tz,rice_lhz_price_tz,rice_lhz_mktthin_tz)


source("R/functions/MktReshape.R") 
clust_price_tz_long = lapply(clust_price_tz,MktReshape)
lhz_price_tz_long = lapply(lhz_price_tz,MktReshape)


# change column names 
colnames(clust_price_tz_long[[1]])[5] = "maize_price" 
colnames(clust_price_tz_long[[2]])[5] = "maize_mktthin" 
colnames(clust_price_tz_long[[3]])[5] = "rice_price" 
colnames(clust_price_tz_long[[4]])[5] = "rice_mktthin" 

colnames(lhz_price_tz_long[[1]])[3] = "lhz_maize_price" 
colnames(lhz_price_tz_long[[2]])[3] = "lhz_maize_mktthin" 
colnames(lhz_price_tz_long[[3]])[3] = "lhz_rice_price" 
colnames(lhz_price_tz_long[[4]])[3] = "lhz_rice_mktthin" 


# merge different prices 
tz_concordance <-  read.csv("data/clean/concordance/Tanzania_coord_lhz.csv")
tz_concordance =  tz_concordance %>% dplyr::select(ea_id,FNID)%>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))

clust_price_merge = dplyr::left_join(clust_price_tz_long[[1]],clust_price_tz_long[[2]])
clust_price_merge = dplyr::left_join(clust_price_merge,clust_price_tz_long[[3]])
clust_price_merge = dplyr::left_join(clust_price_merge,clust_price_tz_long[[4]])

clust_price_merge = clust_price_merge  %>% dplyr::distinct()%>% mutate( ea_id = as.character(ea_id) ) 
clust_price_merge = dplyr::left_join(clust_price_merge,tz_concordance)


lhz_price_merge = left_join(lhz_price_tz_long[[1]],lhz_price_tz_long[[2]])
lhz_price_merge = left_join(lhz_price_merge,lhz_price_tz_long[[3]])
lhz_price_merge = left_join(lhz_price_merge,lhz_price_tz_long[[4]])

tz_price_merge_final = dplyr::left_join(clust_price_merge,lhz_price_merge)  %>% arrange(ea_id,yearmon)

write.csv(tz_price_merge_final,"data/clean/market/tz_price_merge.csv")