
library("geosphere")
library("matrixStats")
library("readxl")
library(dplyr)
library(lubridate)
library(reshape2)
library("readxl")
library("tidyr")
library("dplyr")
library("openxlsx")
library("zoo")

############################################################
### This code is used to find prices and mkt_thinness in each Clusters in 2013 
############################################################

#########################################
# mkt_thinness measure for each cluster 
#########################################

# cluster and mkt coodinate table 
clust_mkt_coord <- read.csv("data/cluster_near_mkt.csv")
mkt_thinness <- read.csv("data/mkt_thinness.csv")

# price for each mkt for each yearmon
price_yearmo <- read.csv("data/imputed_price_yearmo.csv")
colnames(price_yearmo)[1]<-"yearmo"


# change the names to help with the merge 
clust_mkt_coord$near_mkt<-toupper(clust_mkt_coord$near_mkt)

colnames(mkt_thinness)[colnames(mkt_thinness)=="MONKEY.BAY"]<-"MONKEY BAY"
colnames(mkt_thinness)[colnames(mkt_thinness)=="BEMBEKE.TURN.OFF"]<-"BEMBEKE_TURNOFF"
colnames(mkt_thinness)[colnames(mkt_thinness)=="TSANGANO.TURN.OFF"]<-"TSANGANO_TURNOFF"
#name_diff<-dplyr::setdiff(clust_mkt_coord$near_mkt,names(mkt_thinness))
#name_diff



### loop: get hh i’s nearest mkt thinness at yearmo j 
mat_clust<-matrix(NA,nrow(clust_mkt_coord),nrow(mkt_thinness))
dim(mat_clust)

for (i in 1:nrow(clust_mkt_coord)){
  for (j in 1:nrow(mkt_thinness)){
    tryCatch(
      {
        mat_clust[i,j]= mkt_thinness[j,as.character(clust_mkt_coord$near_mkt[i])]
      }, 
      error=function(cond){       # Choose a return value in case of warning
        message("Here's the original warning message:")
        message(cond)
      })
  }
}
df_clust<-as.data.frame(mat_clust)
names(df_clust)<-mkt_thinness$yearmon

clust_mkt_coord_thin<-cbind(clust_mkt_coord,df_clust)
clust_long <- melt(clust_mkt_coord_thin[,2:ncol(clust_mkt_coord_thin)], id.vars = c("ea_id","near_mkt","near_dist"))
names(clust_long)[names(clust_long)=="value"]<-"mkt_thinn"
names(clust_long)[names(clust_long)=="variable"]<-"yearmo"


write.csv(clust_long,"clust_thinness_2013.csv")



### loop: get cluster i’s nearest mkt price at yearmo j 


clust_mkt_coord$near_mkt<-toupper(clust_mkt_coord$near_mkt)

colnames(price_yearmo)[colnames(price_yearmo)=="MONKEY.BAY"]<-"MONKEY BAY"
colnames(price_yearmo)[colnames(price_yearmo)=="BEMBEKE.TURN.OFF"]<-"BEMBEKE_TURNOFF"
colnames(price_yearmo)[colnames(price_yearmo)=="TSANGANO.TURN.OFF"]<-"TSANGANO_TURNOFF"
#name_diff<-dplyr::setdiff(clust_mkt_coord$near_mkt,names(price_yearmo))
#name_diff

clust_mkt_coord<-clust_mkt_coord[,2:ncol(clust_mkt_coord)]
mat_clust_price<-matrix(NA,nrow(clust_mkt_coord),nrow(price_yearmo))
dim(mat_clust_price)

for (i in 1:nrow(clust_mkt_coord)){
  for (j in 1:nrow(price_yearmo)){
    tryCatch(
      {
        mat_clust_price[i,j]= price_yearmo[j,as.character(clust_mkt_coord$near_mkt[i])]
      }, 
      error=function(cond){       # Choose a return value in case of warning
        message("Here's the original warning message:")
        message(cond)
      })
  }
}
df_clust_price<-as.data.frame(mat_clust_price)
names(df_clust_price)<-price_yearmo$yearmo

clust_price<-cbind(clust_mkt_coord,df_clust_price)
clust_long_price <- melt(clust_price[,1:ncol(clust_price)], id.vars = c("ea_id","near_mkt","near_dist"))
names(clust_long_price)[names(clust_long_price)=="value"]<-"price"
names(clust_long_price)[names(clust_long_price)=="variable"]<-"yearmo"
clust_long_price$lnprice_impu<-log(clust_long_price$price)


write.csv(clust_long_price,"clust_price_2013.csv")

