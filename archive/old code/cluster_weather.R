

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
# The purpose of this code is impute the missings in cluster weather by weather in the nearest cluster 
#################################################################################################

setwd("~/Box Sync/Research/Malawi_FewS/")

# distance betwween each cluster to cover the missing weather 
malawi <- read.csv("malawi_data_new.csv")
case_list<-c("clust","lat_modified","lon_modified")
cluster_coord<-malawi[case_list]
cluster_coord<-unique(cluster_coord)
cluster_matrix <- distm(cluster_coord[,c('lon_modified','lat_modified')],fun=distVincentyEllipsoid)

mat_neighbor<-matrix(NA,nrow(cluster_coord),1)
for (i in 1:nrow(cluster_coord)){
  mat_neighbor[i,]<-k.nearest.neighbors(i, cluster_matrix, k = 1)
}
mat_neighbor
cluster_matrix[1,]
# X10105511" "X20701659" "X31102591"

which(cluster_coord$clust == 10105511)
which(cluster_coord$clust == 20701659)
which(cluster_coord$clust == 31102591)

mat_neighbor[21]
cluster_coord$clust[22]


mat_neighbor[302]
cluster_coord$clust[324]


mat_neighbor[652]
cluster_coord$clust[651]

# 10105511 <- 10105513
# 20701659 <- 20720368
# 31102591<- 31102266



clust_weather <- read.csv("CHIRPS_malawi_buffer.csv")

clust_weather$X10105511 <-clust_weather$X10105513
clust_weather$X20701659 <-clust_weather$X20720368
clust_weather$X31102591 <-clust_weather$X31102266

library("reshape2")
clust_long<- melt(clust_weather,na.rm=TRUE,measure.vars = colnames(sub)[2:ncol(sub)])

colnames(clust_long) <-c("DATE","clust","rain")
clust_long$clust<-as.character(clust_long$clust)
clust_long$clust<-gsub("X", "", clust_long$clust)

#sub_long<-clust_long[1:5,]
clust_long$DATE<- as.Date(clust_long$DATE,"%m/%d/%Y")

write.csv(clust_long,"CHIRPS_malawi_cluster_long.csv")
unique(malawi$clust)


# check for the closeset clust next to 
sub<-clust_weather[1:5,]
lapply[i,is.na(sub[,i])]

apply(sub,2,function(x) {all(is.na(x))})
some<-sub[ , colSums(is.na(sub)) != 0]
colnames(some)

