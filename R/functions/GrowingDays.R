
# calculate the growing degree days 

library(dplyr)
library(zoo)
GrowingDays = function(df){
  
  date = df["date1"]
  growdays  = ifelse(cropyear1<32 & cropyear1>5,1,0) 
  growdays$yearmon = cropyear1$yearmon
  growdays %>% group_by(as.numeric(yearmon)) 
  
  %>% summarise_all( .==1)  
}