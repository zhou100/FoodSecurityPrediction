


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


################################################################################################
# The purpose of this code is to find the nearest mkt for each household in 2010 
#################################################################################################

### linking markets to households
setwd("~/Box Sync/Research/Malawi_FewS/")
mkt_coord <- read.csv("geocoordinates.csv")
#head(mkt_coord)
mkt_coord<-unique(mkt_coord)

# change the names to get match 
name_list<-as.character(mkt_coord$Name)
name_list<-toupper(name_list)
name_list<-gsub("_",".",name_list)
name_list<-gsub(" ",".",name_list)
name_list[name_list=="TSANGANO.TURNOFF"]<-"TSANGANO.TURN.OFF"
name_list[name_list=="BEMBEKE.TURNOFF"]<-"BEMBEKE.TURN.OFF"

cluster_coord <- read.csv("hhloc_2013.csv")

# calculate distance of hh to mkts
dist_mat_hh <- distm(hh_coord[,c('lon_modified','lat_modified')], mkt_coord[,c('Longitude','Latitude')], fun=distVincentyEllipsoid)
distance_mat_hh<-as.data.frame(dist_mat_hh)
#assign the name to the point in list1 based on shortest distance in the matrix
#hh_coord$near_mkt <- mkt_coord[,1][max.col(-df)]

# the index of nearest mkt
index <- vector(mode="list",length = nrow(distance_mat_hh))
for (i in 1:nrow(distance_mat_hh)) {
  index[i]<-which.min(apply(distance_mat_hh[i,],MARGIN=2,min))
}

hh_coord<-as.data.frame(hh_coord)
hh_coord$near_mkt<-NA
mkt_coord<-as.data.frame(mkt_coord)
# unlist it to index_nearest
index_nearest<-unlist(index)

length(index_nearest)
index_nearest
# save the name of the nearest market 
for (i in 1:nrow(df)){
  hh_coord$near_mkt[i] <- as.character(mkt_coord$Name[index_nearest[i]])
}
# save the distance in kilometers 
hh_coord$dist_km<-rowMins(mat)/1000

hh_mkt=data.frame(case_id=hh_coord$case_id,near_mkt=hh_coord$near_mkt,near_dist = hh_coord$dist_km)
write.csv(hh_mkt,"hh_near_mkt.csv")







