##################################################################
# Goal : find the price of a market and merge it in with the population weights

# Input : 
# 1.market_df: df,  (or any other df contains the mkt names, for example population weight)
# 2.price_df: df, price/market thinness 
# 3.MKTcolname: character, name of 


# Output: 
# 1. df: price joined to the pop weight , ready for computing weighted price  
################################################################### 
library(dplyr)

NameToPrice <- function(market_df, price_df,MKTcolname="mkt"){
  
  # ideas: for a mkt name found in market_df 
  
  # take the mkt name  as a vector 
  
  mkt_name<- as.vector(sapply(market_df[MKTcolname], as.character))

  joined_df= c()
  for (i in 1:length(mkt_name)){
    #   # find the row in price_df of the particular name , save the prices in a price vector
    price_vector= price_df[price_df[MKTcolname]==mkt_name[i]]
    joined_df <- rbind(joined_df,price_vector)
  }
  joined_df = as.data.frame(joined_df)
  rownames(joined_df)=NULL
  col2 =  as.character(unlist(joined_df$V2))
  
  indx <- sapply(joined_df[,3:ncol(joined_df)], is.factor)
  joined_df[indx] <- lapply(joined_df[indx], function(x) as.numeric(as.character(x)))
  
  joined_df["V2"] = col2 
  
  colnames(joined_df) = colnames(price_df)
  
  merged_df <- dplyr::left_join(market_df,joined_df,by =MKTcolname)

  return ( dplyr::distinct(merged_df))}