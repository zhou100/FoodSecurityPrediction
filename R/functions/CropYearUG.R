
##################################################################
# Goal : generate the crop year indicator in the rainfall data frame 

# Input : 
# 1.df: df, rains extracted from CHIRPS or temperature from African Drought monitor
# 2.Date: vector, contains the Date variable 



# Output: 
# 1. df: cropyear_weather with only rain during the raining season with    
################################################################### 
library(dplyr)
library(zoo)

CropYearUG = function(df){
  
  df = df %>% dplyr::mutate(month = strftime(as.Date(Date),"%m") )%>% dplyr::mutate(year = as.numeric (strftime(as.Date(Date),"%Y")) )
  

  cropyear1 =   df %>% 
    filter( Date>=as.Date("2008-03-01") & Date<=as.Date("2008-07-01") )  %>%
    dplyr::mutate(cropyear = 2008)
  
  cropyear2 =   df %>% 
    filter( Date>=as.Date("2009-03-01") & Date<=as.Date("2009-07-01") ) %>%
    dplyr::mutate(cropyear = 2009)
  
  cropyear3 =   df %>% 
    filter( Date>=as.Date("2010-03-01") & Date<=as.Date("2011-07-01") )  %>%
    dplyr::mutate(cropyear = 2010)
  
  cropyear4 =   df %>% 
    filter( Date>=as.Date("2011-03-01") & Date<=as.Date("2012-07-01") )  %>%
    dplyr::mutate(cropyear = 2011)
  
  cropyear5 =   df %>% 
    filter( Date>=as.Date("2012-03-01") & Date<=as.Date("2013-07-01") )  %>%
    dplyr::mutate(cropyear = 2012)
  
  cropyear6 =   df %>% 
    filter( Date>=as.Date("2013-03-01") & Date<=as.Date("2014-07-01") )  %>%
    dplyr::mutate(cropyear = 2013)
  
  cropyear7 =   df %>% 
    filter( Date>=as.Date("2014-03-01") & Date<=as.Date("2015-07-01") )  %>%
    dplyr::mutate(cropyear = 2014)
  
  cropyear8 =   df %>% 
    filter( Date>=as.Date("2015-03-01") & Date<=as.Date("2016-07-01") )  %>%
    dplyr::mutate(cropyear = 2015)
  
  cropyear_weather = dplyr::bind_rows(cropyear1,cropyear2,cropyear3,cropyear4,cropyear5,cropyear6,cropyear7,cropyear8)
  
  return(cropyear_weather)
}