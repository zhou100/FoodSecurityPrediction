##################################################################
# Goal : transpose weather df from wide  to long
# input: weather_df in wide 
# output: weather_df_transpose, a data frame in the long format with clust/lhz in the first column
#################################
library(dplyr)
WeatherTranspose <- function(weather_df){
  
  weather_df_t = as.data.frame(t(weather_df))  # transpose 
  
  colnames(weather_df_t) = as.character(weather_df_t["cropyear",]) # create columns names 

   trans_weather_t = tibble::rownames_to_column(weather_df_t, var = "id")  # make the row names as a column
  
  
   trans_weather_t  = trans_weather_t %>% dplyr::slice(2:nrow(trans_weather_t)) # remove the crop year 
  
   weather_df_transpose = trans_weather_t  %>%
    tidyr:: gather( key = cropyear, value ="value", 2:ncol(trans_weather_t))  # wide to long 
   
   weather_df_transpose = weather_df_transpose %>% mutate(FS_year = as.integer(cropyear) + 1)


  return(weather_df_transpose) 
}
