##################################################################
# Goal : transpose price matrix from long to wide  
# input: price_df, a data frame containing market names, price and date in the long format 
# output: price_df_transpose, a data frame in the wide format with market names in the first column
#################################

market_transpose <- function(price_df){
  # make uniform colnames 
  colnames(price_df)<-c("mkt","price","date")
  
  # from long to wide 
  price_df_wide = price_df%>% 
    tidyr::spread(key = mkt, value = price) 
  
  # make date as rownames to help with transpose
  rownames(price_df_wide)<-price_df_wide$date
  price_df_wide = price_df_wide%>%  
    dplyr::select(-date)
  
  # transpose 
  price_df_transpose<-t(price_df_wide)
  
  # make the rownames as the first column
  price_df_transpose <- tibble::rownames_to_column(as.data.frame(price_df_transpose), "mkt")
  
  return(price_df_transpose) 
}
