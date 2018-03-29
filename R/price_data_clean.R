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
# create yearmonn variables
price$yearmon <-strftime(price$date,format = "%Y%m") #save the yearmon
week_count<-as.data.frame(table(unique(price)$yearmo))

# count missings for each yearmon
missings<-aggregate(.~ yearmon, data=price[,5:ncol(price)], function(x) {sum(is.na(x))}, na.action = NULL)
# count how many weeks in each yearmon
#weekscount<-count(price, yearmon)$n

weekscount<-week_count$Freq
# mkt_thinness measure for each mkt: percent of missings for each market
mkt_thinness<-as.data.frame(matrix(NA,113,72))
for (i in 1:nrow(missings)){
  mkt_thinness[i,]<-  missings[i,2:ncol(missings)]/weekscount[i]
}
mkt_thinness<-cbind(missings$yearmon,mkt_thinness)
names(mkt_thinness)<-colnames(missings)

write.csv(mkt_thinness,"mkt_thinness.csv")

# transpose the dataset for merge 
mkt_thinness_transpose = as.data.frame(t(mkt_thinness[,2:ncol(mkt_thinness)]))
colnames(mkt_thinness_transpose)<-t(mkt_thinness$yearmon)
mkt_thinness_transpose$mkt_name<-rownames(mkt_thinness_transpose)

# transform to long format with yearmon
mkt_long <- melt(mkt_thinness_transpose[,1:ncol(mkt_thinness_transpose)], id.vars = "mkt_name")
names(mkt_long)<-c("mkt_name","yearmo","mkt_thinn")
mkt_long$yearmo<-as.character(mkt_long$yearmo)

# read in the imputed prices 
price_yearmo<-read.csv("imputed_price_yearmo.csv")
price$yearmon <-strftime(price$date,format = "%Y%m") #save the yearmon
names(price_yearmo)[1]<-"yearmo"

### merge in the mkt_imputed price 
price_yearmo_t<-t(price_yearmo)

#price_yearmo<-aggregate(.~ yearmon, data=price[,5:ncol(price)], FUN = "median", na.action = rm)
long_price <- melt(price_yearmo,id.vars="yearmo")
names(long_price)<-c("yearmo","mkt_name","imputed_price")
long_price$yearmo<-as.character(long_price$yearmo)

# join prices and mkt thinness 
join<-dplyr::left_join(long_price, mkt_long, by = c("mkt_name","yearmo"))

write.csv(join,"price_thinness_long.csv")



#########################################
# mkt_thinness measure for each household 
#########################################
hh_mkt <- read.csv("hh_near_mkt.csv")
hh_mkt$near_mkt<-toupper(hh_mkt$near_mkt)

# change the names to help with the merge 
colnames(mkt_thinness)[colnames(mkt_thinness)=="MONKEY.BAY"]<-"MONKEY BAY"
colnames(mkt_thinness)[colnames(mkt_thinness)=="BEMBEKE.TURN.OFF"]<-"BEMBEKE_TURNOFF"
colnames(mkt_thinness)[colnames(mkt_thinness)=="TSANGANO.TURN.OFF"]<-"TSANGANO_TURNOFF"
name_diff<-dplyr::setdiff(hh_mkt$near_mkt,names(mkt_thinness))
name_diff

#name_diff<-dplyr::setdiff(hh_mkt$near_mkt,names(price_yearmo))
#name_diff


### loop: get hh i’s nearest mkt thinness at yearmo j 
mat_hh<-matrix(NA,nrow(hh_mkt),nrow(mkt_thinness))
dim(mat_hh)

for (i in 1:nrow(hh_mkt)){
  for (j in 1:nrow(mkt_thinness)){
    tryCatch(
      {
        mat_hh[i,j]= mkt_thinness[j,as.character(hh_mkt$near_mkt[i])]
      }, 
      error=function(cond){       # Choose a return value in case of warning
        message("Here's the original warning message:")
        message(cond)
      })
  }
}
df_hh<-as.data.frame(mat_hh)
names(df_hh)<-mkt_thinness$yearmon

hh_mkt_thin<-cbind(hh_mkt,df_hh)
hh_long <- melt(hh_mkt_thin[,3:ncol(hh_mkt_thin)], id.vars = c("case_id","near_mkt","near_dist"))
names(hh_long)[names(hh_long)=="value"]<-"mkt_thinn"
names(hh_long)[names(hh_long)=="variable"]<-"yearmo"



 
 