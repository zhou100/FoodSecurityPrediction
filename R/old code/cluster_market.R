
library("geosphere")
library("matrixStats")
library("readxl")
library(dplyr)
library(lubridate)
library("readxl")
library("tidyr")
library("dplyr")
library("openxlsx")
library("zoo")

############################################################
### This code is used for linking markets to Clusters in 2013 
############################################################

mkt_coord <- read.csv("data/geocoordinates.csv")
#head(mkt_coord)
mkt_coord<-unique(mkt_coord)

# change the names to get match 
name_list<-as.character(mkt_coord$Name)
name_list<-toupper(name_list)
name_list<-gsub("_",".",name_list)
name_list<-gsub(" ",".",name_list)
name_list[name_list=="TSANGANO.TURNOFF"]<-"TSANGANO.TURN.OFF"
name_list[name_list=="BEMBEKE.TURNOFF"]<-"BEMBEKE.TURN.OFF"

cluster_coord <- read.csv("data/hhloc_2013.csv")
cluster_coord <- unique(cluster_coord)

# calculate distance of clusters to mkts
dist_mat_cluster <- distm(cluster_coord[,c('LON_DD_MOD','LAT_DD_MOD')], mkt_coord[,c('Longitude','Latitude')], fun=distVincentyEllipsoid)
dist_mat_cluster<-as.data.frame(dist_mat_cluster)
#assign the name to the point in list1 based on shortest distance in the matrix
#cluster_coord$near_mkt <- mkt_coord[,1][max.col(-df)]


index <- vector(mode="list",length = nrow(dist_mat_cluster))
for (i in 1:nrow(dist_mat_cluster)) {
  index[i]<-which.min(apply(dist_mat_cluster[i,],MARGIN=2,min))
}

cluster_coord<-as.data.frame(cluster_coord)
cluster_coord$near_mkt<-NA
mkt_coord<-as.data.frame(mkt_coord)
# unlist it to index_nearest
index_nearest<-unlist(index)

length(index_nearest)
index_nearest
# save the name of the nearest market 
for (i in 1:nrow(cluster_coord)){
  cluster_coord$near_mkt[i] <- as.character(mkt_coord$Name[index_nearest[i]])
}
# save the distance in kilometers 
cluster_coord$dist_km<-rowMins(as.matrix(dist_mat_cluster))/1000

cluster_mkt=data.frame(ea_id=cluster_coord$ea_id,near_mkt=cluster_coord$near_mkt,near_dist = cluster_coord$dist_km)
write.csv(cluster_mkt,"data/cluster_near_mkt.csv")



