## remove the X in the cluster id 

library(dplyr)
RemoveX <- function(weather_df){
  id = weather_df["id"]
  weather_df["id"] = gsub(x=as.matrix(id),"X","")
  return(weather_df)
  } 