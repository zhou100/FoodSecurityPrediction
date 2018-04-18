## join fnid and v 

library(dplyr)
FnidV  <- function(weather_df,concord){
  weather_df =  weather_df %>% dplyr::left_join(concord, by = "id")
  weather_df = weather_df %>% dplyr::select(-id)
  return(weather_df)
} 