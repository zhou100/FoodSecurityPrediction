##################################################################
# Goal : find the nearest market based on  market coordinates 
# input: data frame containing market names and coordinates 
# output: a matrix of market and its nearest neighbors
################################## 

####### k nearest neighbor ######
library(FastKNN)
library(geosphere)

NearMkt = function(coord_df){
  # calculate a distance matrix based on lat and lons 
  dist_matrix <- distm(coord_df[,c('lon','lat')],fun=distVincentyEllipsoid)
  
  # prepare a matrix for storing the near neighbors 
  mat_neighbor<-matrix(NA,nrow(coord_df),(nrow(coord_df)-1))

  # using knn neighbors to find the nearest neighbors 
  for (i in 1:nrow(coord_df)){
    mat_neighbor[i,]<-k.nearest.neighbors(i, dist_matrix, k = (nrow(coord_df)-1))
  }
  
  # from index to market name 
  
  mat_neighbor_name<-matrix(NA,nrow(coord_df),(nrow(coord_df)-1))
  
  for (i in 1:nrow(mat_neighbor)){
    for (j in 1:ncol(mat_neighbor)){

    index = mat_neighbor[i,j]
    mat_neighbor_name[i,j] <-as.character(coord_df[["mkt"]][index])

    }
  }
  
  neighbor_df<- cbind(as.character(coord_df[["mkt"]]),mat_neighbor_name)
  
  # add in the market names as row names 
  colnames(neighbor_df)<- c("mkt",paste("k",seq(1, (ncol(neighbor_df)-1), by = 1),sep =""))
  
  
  
  
  return(neighbor_df)
}
 