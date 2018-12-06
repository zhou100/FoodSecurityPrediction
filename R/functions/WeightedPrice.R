##################################################################
# Goal : generate weighted price based on  weights and prices
# Input : 
# 1.df:  joined unweighed price and popweight after applying NameToPrice

# Output: 
# 1. df: pop weighed price, one price for each livelihood zone
################################################################### 
library(dplyr)
WeightedPrice <- function(lhz_price_unweight){
  # save the FNID, market name and weight
  tryCatch(
    {lhz_price_unweight = lhz_price_unweight %>% dplyr::select(-X)},
    error = function(e){}
    )
  
  lhz_price_weighted<- lhz_price_unweight[,1:3]
  
  for (i in 4:ncol(lhz_price_unweight)){
    # generate a vector that is weight * price 
    
    wprice<-      as.numeric(unlist(lhz_price_unweight["weights"])) *     as.numeric(unlist(lhz_price_unweight[i]))  
    wprice.df = as.data.frame(wprice)
    names(wprice.df) = colnames(lhz_price_unweight)[i]
    
    # combind all the months of the generated price pieces
    lhz_price_weighted<-dplyr::bind_cols(lhz_price_weighted, wprice.df)    
  }
  
  colnames(lhz_price_weighted) = colnames(lhz_price_unweight)
  
  lhz_price_weighted = lhz_price_weighted  %>% dplyr::group_by(FNID)
  # remove the market names, since characters can't apply the sum functoin 
  lhz_price_weighted <- lhz_price_weighted %>% dplyr::select(-mkt)
  
  # by FNID, sum up all the prices 
  # check that summed weight should be 1 for each FNID 
  lhz_price_computed = lhz_price_weighted %>% dplyr::group_by(FNID) %>% dplyr::summarise_all(funs(sum))
  
  return(lhz_price_computed)
  
}