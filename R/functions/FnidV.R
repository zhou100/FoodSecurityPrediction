## join fnid and v 

library(dplyr)
FnidV  <- function(weather_df,concord){
  weather_df$id = as.character(weather_df$id)
  
  weather_df =  weather_df %>% dplyr::left_join(concord, by = "id")
  return(weather_df)
} 