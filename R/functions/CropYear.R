
##################################################################
# Goal : generate the crop year indicator in the rainfall data frame 

# Input : 
# 1.df: df, rains extracted from CHIRPS or temperature from African Drought monitor
# 2.date: vector, contains the date variable 



# Output: 
# 1. df: cropyear_weather with only rain during the raining season with    
################################################################### 

CropYear = function(df,date){
  library(dplyr)
  library(zoo)
  month = strftime(date,"%m")
  month = as.numeric(month)
  year = strftime(date,"%Y")
  year = as.numeric(year)
  
  df["date"]  = date
  df["month"] = month
  df["year"] = year
  
  
  cropyear1 =   df %>% 
    filter( date>=as.Date("2008-10-01") & date<=as.Date("2009-05-01") )  %>%
    dplyr::mutate(cropyear = 2008)
  
  cropyear2 =   df %>% 
    filter( date>=as.Date("2009-10-01") & date<=as.Date("2010-05-01") ) %>%
    dplyr::mutate(cropyear = 2009)
  
  cropyear3 =   df %>% 
    filter( date>=as.Date("2010-10-01") & date<=as.Date("2011-05-01") )  %>%
    dplyr::mutate(cropyear = 2010)
  
  cropyear4 =   df %>% 
    filter( date>=as.Date("2011-10-01") & date<=as.Date("2012-05-01") )  %>%
    dplyr::mutate(cropyear = 2011)
  
  cropyear5 =   df %>% 
    filter( date>=as.Date("2012-10-01") & date<=as.Date("2013-05-01") )  %>%
    dplyr::mutate(cropyear = 2012)
  
  cropyear6 =   df %>% 
    filter( date>=as.Date("2013-10-01") & date<=as.Date("2014-05-01") )  %>%
    dplyr::mutate(cropyear = 2013)
  
  cropyear7 =   df %>% 
    filter( date>=as.Date("2014-10-01") & date<=as.Date("2015-05-01") )  %>%
    dplyr::mutate(cropyear = 2014)
  
  cropyear8 =   df %>% 
    filter( date>=as.Date("2015-10-01") & date<=as.Date("2016-05-01") )  %>%
    dplyr::mutate(cropyear = 2015)
  
  cropyear_weather = dplyr::bind_rows(cropyear1,cropyear2,cropyear3,cropyear4,cropyear5,cropyear6,cropyear7,cropyear8)
  
  return(cropyear_weather)
}