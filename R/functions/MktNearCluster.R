##################################################################
# Goal : find the nearest market to the cluster based on  market coordinates 
# input: data frame containing market names and coordinates 
# output: a matrix of market and its nearest neighbors
################################## 

####### k nearest neighbor ######
library(FastKNN)
library(geosphere)
library(dplyr)


MktNearCluster = function(clust_coord,mkt_coord){
  # calculate a distance matrix based on lat and lons 
  
  # calculate distance of hh to mkts
  dist_mat_clust <- distm(clust_coord[,c('lon_modified','lat_modified')], mkt_coord[,c('lon','lat')], fun=distVincentyEllipsoid)
  distance_mat_clust<-as.data.frame(dist_mat_clust)
  #assign the name to the point in list1 based on shortest distance in the matrix
  #clust_coord$near_mkt <- mkt_coord[,1][max.col(-df)]
  
  # the index of nearest mkt
  index <- vector(mode="list",length = nrow(distance_mat_clust))
  for (i in 1:nrow(distance_mat_clust)) {
    index[i]<-which.min(apply(distance_mat_clust[i,],MARGIN=2,min))
  }
  
  
  # unlist it to index_nearest
  index_nearest<-unlist(index)
    # save the name of the nearest market 
  for (i in 1:nrow(distance_mat_clust)){
    index = index_nearest[i]
    clust_coord[["near_mkt"]][i] <- as.character(mkt_coord[["mkt"]][index])
  }
  
   
  # save the distance in kilometers 
  
  clust_coord["dist_km"]<-apply(dist_mat_clust,1,min)/1000
  
  clust_coord = clust_coord %>% dplyr::select(-lat_modified,-lon_modified,-X)
  
  return(clust_coord)
}

# mkt distance 
# knn_training_function(mkt_coord, mkt_matrix, mkt_coord[,1], k = 1)

# # generate neighbor matrix to save results 
# mat_neighbor1<-matrix(NA,nrow(mkt_coord),20)
# mat_neighbor2<-matrix(NA,nrow(mkt_coord),10)
# 
# for (i in 1:nrow(mkt_coord)){
#   mat_neighbor1[i,]<-k.nearest.neighbors(i, mkt_matrix, k = 20)
# }