##################################################################
# Goal : compute the number of missings and impute the missing price by markets nearby 

# inputs: 
# 1. price_mat: a data frame containing market names, prices and date in wide format 
# 2. neighbor_matrix: a matrix of market and its nearest neighbors

# output:  
# 1. price_mat: data frame with imputed prices
##################################################################


SpatialPriceImpu = function(price_mat, neighbor_matrix){
  
  # run the following loop to compute the number of missings and impute the missing price by markets nearby 
  # for a given k 
  # if price_mat[i,j] has an na, then go grab an average of price_mat[neighbor_matrix[i,1:k],j] 
  # check if price_mat still has an NA, if not, end the loop
  
  
  max_k = ncol(neighbor_matrix) -1  # start with 
  price_mat_copy = price_mat # use a copy to store the 
  missings<-matrix(NA,max_k,2)
  
  
  
  
  for (k in 1:max_k){
    for (i in 1:nrow(price_mat)){
      for (j in 2:ncol(price_mat)){
        if (is.na(price_mat_copy[i,j])==TRUE){
          # document the missing market's name 
            miss_mkt_name = as.character(price_mat_copy[i,1])
          # find its k nearest neighbors 
            
            neighbors = neighbor_matrix[,(neighbor_matrix[,"mkt"] ==miss_mkt_name)]
            
            # find the name of the kth nearest neighbors 
              neighbor_k_name = as.character(neighbors[k+1])
             
              # find the price of that neighbor in the same month (column j) 
              price_mat_copy[i,j] <- as.numeric(price_mat_copy[price_mat_copy$mkt == neighbor_k_name,][j])
             # price_mat[i,j]  < - 
              }
      }
    }
    
  
    # store the number of missings 
    missings[k,1]<-k
    missings[k,2]<-sum(colSums(is.na(price_mat_copy))) # total missings
  }
  
  
  # based on the result above, use the k value that minimizes the number of missings 
  max_k  <- which.min(missings[,2])
  price_mat_imputed = price_mat
  
  for (k in 1:max_k){
    for (i in 1:nrow(price_mat)){
      for (j in 2:ncol(price_mat)){
        if (is.na(price_mat_imputed[i,j])==TRUE){
          # document the missing market's name 
          miss_mkt_name = price_mat_imputed[i,1]
          # find its k nearest neighbors 
          neighbors = neighbor_matrix[,neighbor_matrix[,"mkt"] ==miss_mkt_name]
          
          # find the name of the kth nearest neighbors 
          neighbor_k_name = as.character(neighbors[k+1])
          
          # find the price of that neighbor in the same month (column j) 
          price_mat_imputed[i,j] <- as.numeric(price_mat_imputed[price_mat_imputed$mkt == neighbor_k_name,][j])
         }
      }
    }
  }
  
  
  # return price_mat_imputed: data frame with imputed prices
  
  
   return(price_mat_imputed)
}

 

 