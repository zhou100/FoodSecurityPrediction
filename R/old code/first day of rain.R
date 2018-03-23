

setwd("C:\\Users\\guyuye2\\Desktop\\Malawi")
library(zoo)
chirps_2007 <- read.csv("CHIRPS_Malawi_Mean_2007.csv")
chirps_2008 <- read.csv("CHIRPS_Malawi_Mean_2008.csv")
chirps_2009 <- read.csv("CHIRPS_Malawi_Mean_2009.csv")
chirps_2010 <- read.csv("CHIRPS_Malawi_Mean_2010.csv")
chirps_2011 <- read.csv("CHIRPS_Malawi_Mean_2011.csv")
chirps_2012 <- read.csv("CHIRPS_Malawi_Mean_2012.csv")
chirps_2013 <- read.csv("CHIRPS_Malawi_Mean_2013.csv")
chirps_2014 <- read.csv("CHIRPS_Malawi_Mean_2014.csv")
chirps_2015 <- read.csv("CHIRPS_Malawi_Mean_2015.csv")
chirps_2016 <- read.csv("CHIRPS_Malawi_Mean_2016.csv")



chirps_0716 <- rbind(chirps_2007, chirps_2008, chirps_2009, chirps_2010, chirps_2011, chirps_2012, chirps_2013,
                     chirps_2014, chirps_2015, chirps_2016)
chirps_0716$Date <- seq(as.Date("2007-01-01"), as.Date("2016-12-31"), by = "days")
chirps_0716 <- chirps_0716[, c(62, 2:61)]

chirps_0716$month <- substr(as.character(chirps_0716$Date), 6,7)
write.csv(chirps_0716, file = "chirps_daily_0716.csv")


chirps_0716_fall <- subset(chirps_0716, chirps_0716$month >=10)
chirps_0716_fall$year <- substr(as.character(chirps_0716_fall$Date), 1, 4)
chirps_fall_07 <- subset(chirps_0716_fall, chirps_0716_fall$year == "2007")

first_day_07 <- c()
for (i in chirps_fall_07[2:61]){
  district_rain <- cbind(i, chirps_fall_07$Date)
  district_rain <- subset(district_rain, district_rain[,1]!=0)
  day <- as.Date(district_rain[1,2])
  first_day_07 <- cbind(first_day_07, day)
}


chirps_fall_08 <- subset(chirps_0716_fall, chirps_0716_fall$year == "2008")
first_day_08 <- c()
for (i in chirps_fall_08[2:61]){
  district_rain <- cbind(i, chirps_fall_08$Date)
  district_rain <- subset(district_rain, district_rain[,1]!=0)
  day <- as.Date(district_rain[1,2])
  first_day_08 <- cbind(first_day_08, day)
}

chirps_fall_09 <- subset(chirps_0716_fall, chirps_0716_fall$year == "2009")
first_day_09 <- c()
for (i in chirps_fall_09[2:61]){
  district_rain <- cbind(i, chirps_fall_09$Date)
  district_rain <- subset(district_rain, district_rain[,1]!=0)
  day <- as.Date(district_rain[1,2])
  first_day_09 <- cbind(first_day_09, day)
}

chirps_fall_10 <- subset(chirps_0716_fall, chirps_0716_fall$year == "2010")
first_day_10 <- c()
for (i in chirps_fall_10[2:61]){
  district_rain <- cbind(i, chirps_fall_10$Date)
  district_rain <- subset(district_rain, district_rain[,1]!=0)
  day <- as.Date(district_rain[1,2])
  first_day_10 <- cbind(first_day_10, day)
}

chirps_fall_11 <- subset(chirps_0716_fall, chirps_0716_fall$year == "2011")
first_day_11 <- c()
for (i in chirps_fall_11[2:61]){
  district_rain <- cbind(i, chirps_fall_11$Date)
  district_rain <- subset(district_rain, district_rain[,1]!=0)
  day <- as.Date(district_rain[1,2])
  first_day_11 <- cbind(first_day_11, day)
}

chirps_fall_12 <- subset(chirps_0716_fall, chirps_0716_fall$year == "2012")
first_day_12 <- c()
for (i in chirps_fall_12[2:61]){
  district_rain <- cbind(i, chirps_fall_12$Date)
  district_rain <- subset(district_rain, district_rain[,1]!=0)
  day <- as.Date(district_rain[1,2])
  first_day_12 <- cbind(first_day_12, day)
}

chirps_fall_13 <- subset(chirps_0716_fall, chirps_0716_fall$year == "2013")
first_day_13 <- c()
for (i in chirps_fall_13[2:61]){
  district_rain <- cbind(i, chirps_fall_13$Date)
  district_rain <- subset(district_rain, district_rain[,1]!=0)
  day <- as.Date(district_rain[1,2])
  first_day_13 <- cbind(first_day_13, day)
}

chirps_fall_14 <- subset(chirps_0716_fall, chirps_0716_fall$year == "2014")
first_day_14 <- c()
for (i in chirps_fall_14[2:61]){
  district_rain <- cbind(i, chirps_fall_14$Date)
  district_rain <- subset(district_rain, district_rain[,1]!=0)
  day <- as.Date(district_rain[1,2])
  first_day_14 <- cbind(first_day_14, day)
}

chirps_fall_15 <- subset(chirps_0716_fall, chirps_0716_fall$year == "2015")
first_day_15 <- c()
for (i in chirps_fall_15[2:61]){
  district_rain <- cbind(i, chirps_fall_15$Date)
  district_rain <- subset(district_rain, district_rain[,1]!=0)
  day <- as.Date(district_rain[1,2])
  first_day_15 <- cbind(first_day_15, day)
}

chirps_fall_16 <- subset(chirps_0716_fall, chirps_0716_fall$year == "2016")
first_day_16 <- c()
for (i in chirps_fall_16[2:61]){
  district_rain <- cbind(i, chirps_fall_16$Date)
  district_rain <- subset(district_rain, district_rain[,1]!=0)
  day <- as.Date(district_rain[1,2])
  first_day_16 <- cbind(first_day_16, day)
}




first_day <- rbind(first_day_07, first_day_08, first_day_09, first_day_10, first_day_11, first_day_12, first_day_13,
                   first_day_14, first_day_15, first_day_16)
first_day_date <- as.matrix(as.character(as.Date(first_day)))
ID <- rep(1:60, each= 10)
first_day_date <- cbind(first_day_date, paste0("V",ID))
colnames(first_day_date) <- c("first_day", "ID")
first_day_date_df <- as.data.frame(first_day_date)
first_day_date_df$year <- rep(2007:2016)
first_day_date_wide <- reshape(first_day_date_df, idvar = "ID", timevar = "year", direction = "wide")

write.csv(first_day_date_wide, file = "first_day_of_rain.csv")
