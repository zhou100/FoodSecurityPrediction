##################################################################
# Goal : This script aims to clean up the rainfall and temperature data，
# 
# purpose: generate variables like total rainfalls, rainfall in areas that are prone to flood, first of rain

# Input : 
# 1. daily rainfall extracted from CHIRPS (using script rainfall_daily_cluster.R and cluster buffer shapefiles)
# 2. daily temperture data from African Drought Monitor
# 3. coordinates of clusters. 
# 4. shapefile of livelihood zones. 

# Output: 
# 0. growing degree days (both lhz and cluster level) 
# 1. total rainfalls in growing seasons  （Growing season total precipitation）
# 2. Number of max no rain days in a crop year
# 3.   GDD - number of days where temp was between 8 to 32 C (Tmax + Tmin)/2 . Deschênes and Greenstone (2007) yield  on weather 

################################
# data to get in the future 
#########################################
# 4. Schlenker & Roberts (2009), number and percentage of days in a set number of quantile bins (per-bin deviations from long-run in z-scores)

# 5.5 temperatue bins  Deschenes and Greenstone (2011) (<10, 20-25, etc. )
# 6. soil quality：Soil characteristics: K factor, slope lenghth, fraction irrigated , moisture capacity, salinity 
# 7. NDVI 
# 
# Yujun Zhou -  04/11/18
###################################################################

######################################
### first day of rain for the rainy season (since October month >=10) for each livelihood zones 
### should there be a threshold other than 0 ?
######################################

precip_lhz_tz <- read.csv("data/raw/rain/CHIRPS_tz_lhz.csv")
precip_lhz_ug <- read.csv("data/raw/rain/CHIRPS_ug_lhz.csv")


precip_clust_tz <- read.csv("D:/CHIRPS_tan_buffer.csv")
precip_clust_ug <- read.csv("D:/CHIRPS_ug_buffer.csv")


# generate the year and month variable from date 
date= as.Date(precip_lhz_ug$date_list,"%m/%d/%Y")
month = strftime(date,"%m")
month = as.numeric(month)
year = strftime(date,"%Y")

# generate cropyear 
if ("2008-10-01"<precip_lhz_tz$date<"2009-05-01"){
  precip_lhz_tz["cropyear_tz"] = 1 
}

precip_lhz_tz["cropyear_tz"] = ifelse(precip_lhz_tz$month>= 8 & precip_lhz_tz$date <="2009-05-01",1,0)


#boulder_daily_precip <- boulder_daily_precip %>%
#  mutate(DATE = as.Date(DATE, format = "%m/%d/%y"))



precip_lhz_tz["year"] = year
precip_lhz_tz["date"] = date
precip_lhz_tz["month"] = month

precip_lhz_ug["year"] = year
precip_lhz_ug["date"] = date
precip_lhz_ug["month"] = month

precip_clust_ug["year"] = year
precip_clust_ug["date"] = date
precip_clust_ug["month"] = month

precip_clust_tz["year"] = year
precip_clust_tz["date"] = date
precip_clust_tz["month"] = month



# 1. generate crop year, so that it's summing up by crop year 
# 2. transmute 
# rain to moving sums of daily rain -10 >0
# 2. arrange(desc()) to sort
# mutate_all(dplyr::between(-1,0.5))
# 3. use moving sums of rain >0, 
# 4. first(date[.>0]


########################################################################  
# generate the first date of rain after October for Tanzania and After April for Uganda 
# day1rain "the number of days after Oct 1 where five-day rainfall > 10 and it rained at least 3/5 days"
######################################################################## 
library(dplyr)

# moving mean for that day and previous days (e.g. 5 represents the mean of that day and the for previous days)
# df2 = df %>%
#   group_by(site, year) %>%
#   arrange(site, year, day) %>%
#   mutate(temp.5 = rollmean(x = temp, 5, align = "right", fill = NA))
# head(df2, 75)
# 
# # moving mean for the previous days not including the current day (e.g. 5 represents the mean of the 5 previous days)
# df2 = df2 %>%
#   mutate(temp.lag1 = lag(temp, n = 1)) %>%
#   mutate(temp.5.previous = rollapply(data = temp.lag1, 
#                                      width = 5, 
#                                      FUN = mean, 
#                                      align = "right", 
#                                      fill = NA, 
#                                      na.rm = T))
# head(df2, 75)

# df2 = df2 %>%
#   mutate(precip.30 = rollsum(x = precip, 30, align = "right", fill = NA))
# head(df2, 75)


# create lag weather
precip_lhz_tz %>%
  group_by(year) %>%
  dplyr::mutate_all(funs(lag(.,n=12,order_by = date)))


precip_boulder_AugOct <- boulder_daily_precip %>%
  filter(DATE >= as.Date('2013-08-15') & DATE <= as.Date('2013-10-15'))




day1rain_lhz_tz = precip_lhz_tz %>% 
  dplyr::filter(month>9) %>%  # planting/growing season starts from oct
  dplyr::group_by(year) %>%   # calculate for each year individually 
  dplyr::summarise_all(funs(first(date[.>0])))  # first date that has rainfall >0 

day1rain_clust_tz = precip_clust_tz %>% 
  dplyr::filter(month>9) %>%  # planting/growing season starts from oct
  dplyr::group_by(year) %>%   # calculate for each year individually 
  dplyr::summarise_all(funs(first(date[.>0])))  # first date that has rainfall >0 


 
day1rain_lhz_ug = precip_lhz_ug %>% 
  dplyr::filter(month>3) %>%  # planting/growing season starts from oct
  dplyr::group_by(year) %>%   # calculate for each year individually
  dplyr::summarise_all(funs(first(date[.>0])))  # first date that has rainfall >0 



day1rain_clust_ug = precip_clust_ug %>% 
  dplyr::filter(month>3) %>%  # planting/growing season starts from oct
  dplyr::group_by(year) %>%   # calculate for each year individually
  dplyr::summarise_all(funs(first(date[.>0])))  # first date that has rainfall >0 

write.csv(day1rain_clust_ug,"data/clean/rain/day1rain_clust_ug.csv")
write.csv(day1rain_clust_tz,"data/clean/rain/day1rain_clust_tz.csv")
write.csv(day1rain_lhz_tz,"data/clean/rain/day1rain_lhz_tz.csv")
write.csv(day1rain_lhz_ug,"data/clean/rain/day1rain_lhz_ug.csv")

# where five-day rainfall > 10 and it rained at least 3/5 days"

#################################################################################
#### generate maxdaysno rain 
########### longest dry spell during the rainy season (Oct-Mar) per crop year (May-Apr)"
#################################################################################
day1rain_lhz_ug



#rain_cytot = rain_Oct+rain_Nov+rain_Dec+rain_Jan+rain_Feb+rain_Mar+rain_Apr

# rain_cytot
