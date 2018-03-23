##################################################################
# Goal : find the nearest market based on 
# input: data frame containing market names and coordinates 
# output: a list of market and its nearest neighbors
################################## 

####### k nearest neighbor ######
library(FastKNN)
library(geosphere)

NearMkt = function(df){
  dist_matrix <- distm(df[,c('lon','lat')],fun=distVincentyEllipsoid)
  k1neighbor = knn_training_function(df, dist_matrix, label = df[["mkt"]], k = 1)
  
  
  return(k1neighbor)
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