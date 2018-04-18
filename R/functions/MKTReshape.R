##################################################################
# Goal : transpose weather df from wide  to long
# input: market_df in wide 
# output: market_df_transpose, a data frame in the long format with clust/lhz in the first column
#################################
library(dplyr)
library(zoo)
library(stringr)

MktReshape <- function(market_df){
  market_df = market_df %>%  dplyr::select(-X,-X.1)
  
  position = str_which(colnames(market_df), "X2006.01.01") 
  
  
  market_df_long = market_df  %>%
    tidyr:: gather( key = date, value ="value", position:ncol(market_df))  # wide to long 
  
  date = market_df_long["date"]
  date_mkt = gsub(x=as.matrix(date),"X","")
  date_mkt = as.Date(date_mkt,format = "%Y.%m.%d" )
  
  
  market_df_long["date"] = date_mkt
  market_df_long["yearmon"] = as.yearmon(date_mkt)
    
    
    
  return(market_df_long) 
}
