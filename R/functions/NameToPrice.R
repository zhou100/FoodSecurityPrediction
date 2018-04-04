##################################################################
# Goal : find the price of a market and merge it in with the population weights

# Input : 
# 1.df: pop weight list (or any other df contains the mkt names)
# 2.df: price/market thinness 


# Output: 
# 1. df: price joined to the pop weight , ready for computing weighted price  
################################################################### 
library(dplyr)
NameToPrice <- function(pop_weight, price_df){
  
  # ideas: for a mkt name found in pop_weight 
  
  # take the mkt name  as a vector 
  
  mkt_name<- as.vector(sapply(pop_weight["mkt"], as.character))
  
  
  joined_df= c()
  for (i in 1:length(mkt_name)){
    #   # find the row in price_df of the particular name , save the prices in a price vector
    price_vector=  as.vector (price_df[price_df["mkt"]==mkt_name[i],])
    # stack the price vectors in a data frame
    joined_df <- rbind(joined_df,price_vector)
  }
  
  merged_df <- dplyr::left_join(pop_weight,joined_df,by = "mkt")
  
  merged_df <- dplyr::select(merged_df,-c(X.x,X.y))
  # rename the columns as the name of price_df 
  
  return ( dplyr::distinct(merged_df))
}