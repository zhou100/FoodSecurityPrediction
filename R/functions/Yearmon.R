##################################################################
# Goal : generate Date and yearmon variable to help with join
# input: data frame with year and month variables
# output: data frame with Date and yearmon
################################## 
library(zoo)
 
yearmon = function(df,year_var,month_var){
   
  year = df[[year_var]]
  month = df[[month_var]]
  month_character<-month.abb[month] 
  yearmon_character = paste(month_character,year,sep="/")
  yearmon_format<-as.yearmon(yearmon_character,format = "%b/%Y")
  date_format<-as.Date(yearmon_format)
  df[["yearmon"]] = yearmon_format
  df[["date"]] = date_format
  return(df)
}


 