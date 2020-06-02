##################################################################
# Goal : generate Date and yearmon variable to help with join
# input: data frame with year and month variables
# output: data frame with Date and yearmon
################################## 
library(tidyverse)
 
yearmon = function(df,year_var,month_var){
   
  year = df[[year_var]]
  month = df[[month_var]]
  month_character<-month.abb[month] 
  yearmon_character = paste(month_character,"1st",year,sep="/")
  yearmon_format = lubridate::mdy(yearmon_character)
  df[["date"]] = yearmon_format
  return(df)
}
 