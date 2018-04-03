
setwd("C:/Users/guyuye2/Downloads/malawi")
packages <- c("sp", "maptools", "raster", "RCurl", "R.utils", "rgdal", "rgeos", "data.table", "zoo")
lapply(packages, require, character.only = TRUE)

chirps_max <- read.csv("CHIRPS_Malawi_Max.csv")
chirps_min <- read.csv("CHIRPS_Malawi_Min.csv")
chirps_mean <- read.csv("CHIRPS_Malawi_Mean.csv")
link1 <- read.csv("FNID_to_V.csv")
ipc <- read.csv("ipc_new.csv")
price <- read.csv("price.csv")
malawi <- readOGR(".", "MW_Admin1_LHZ_2012.3")
weights1 <- read.csv("pop_weights.csv")
proj.latlon <- CRS("+proj=longlat +datum=WGS84")
proj4string <- CRS("+init=epsg:20936")

chirps_max_small <- subset(chirps_max, as.yearmon(chirps_max$X) > as.yearmon("2007-11"))
chirps_min_small <- subset(chirps_min, as.yearmon(chirps_min$X) > as.yearmon("2007-11"))
chirps_mean_small <- subset(chirps_mean, as.yearmon(chirps_mean$date) > as.yearmon("2007-11"))

library(reshape)
temp1 <- melt(chirps_max_small, id = "X")
colnames(temp1) <- c("date", "V", "chirps_max")
temp2 <- melt(chirps_min_small, id = "X")
colnames(temp2) <- c("date", "V", "chirps_min")
temp3 <- melt(chirps_mean_small, id = "date")
colnames(temp3) <- c("date", "V", "chirps_mean")

temp_chirp <- cbind(temp1, temp2, temp3)
temp_chirp <- temp_chirp[, c("date", "V", "chirps_mean", "chirps_min", "chirps_max")]
temp_chirp_merge <- merge(temp_chirp, link1, by = "V")
temp_chirp_merge <- merge(temp_chirp_merge, malawi@data, by = "FNID")
temp_chirp_merge$date_c <- as.character(temp_chirp_merge$date)

#### IPC ####
link2 <- temp_chirp_merge[, c("V", "FNID")]
link2 <- unique(link2)
ipc <- join(ipc, link2, by = "FNID", type = "left", match = "all")
ipc <- ipc[, c(1, 9:48)]
#ipc <- subset(ipc, !is.na(ipc$V))
ipc_transpose <- t(ipc)
date <- as.character(seq(as.Date("2009-07-01"), as.Date("2016-04-01"), by = "quarter"))
#ipc_transpose <- cbind(ipc_transpose, date)
colnames(ipc_transpose) <- ipc_transpose[41,]
ipc_transpose <- ipc_transpose[-c(1),]
ipc_transpose <- ipc_transpose[-c(41),]
#ipc_transpose <- subset(ipc_transpose, row.names(ipc_transpose)!= "V")
date_raw <- as.data.frame(row.names(ipc_transpose))
date_raw$date1 <- substr(date_raw$`row.names(ipc_transpose)`, 3, 8)
date_raw$date <- paste0(substr(date_raw$date1, 1, 4), "-", substr(date_raw$date1, 5,6), "-01")
ipc_transpose <- cbind(ipc_transpose, date_raw$date)

#ipc_transpose <- cbind(ipc_transpose, date)
row.names(ipc_transpose) <- NULL
row.names(ipc_transpose) <- ipc_transpose[,61]


ipc_long <- melt(ipc_transpose, id ="date")
ipc_long <- subset(ipc_long, ipc_long$X2 != "date")
ipc_long$date<- substr(as.character(ipc_long$X1), 1,7)
ipc_long_small <- ipc_long[, c("X2","value","date")]
colnames(ipc_long_small) <- c("V", "IPC", "date")
ipc_long_small <- ipc_long_small[-c(2401:2440),]

####Combine IPC and CHIRPS###
ipc_chirps <- merge(ipc_long_small, temp_chirp_merge, by.x = c("V", "date"), by.y = c("V", "date_c"), all = TRUE)
ipc_chirps$date <- NULL

### Price data ###
price_m <- melt(price, id = "yearmo")
colnames(price_m) <- c("admin_name", "date", "price")
price_m$date <- gsub("X","", price_m$date)
price_m$date <- paste0(substr(price_m$date, 1,4), "-", substr(price_m$date, 5,6))
#temp_chirp_merge$admin_cap <- toupper(temp_chirp_merge$ADMIN2)
#temp_chirps_merge_weights <- merge(temp_chirp_merge, weights, by.x = c("LZCODE", "admin_cap"), by.y = c("LZCODE", "Name"), all = TRUE)
colnames(price_m)[1] <- "Name"
price_m_markets <- join(markets_thsn.proj@data, price_m, by = "Name", type= "left", match = "all")
#price_m_markets <- merge(markets_thsn.proj, price_m, by.x = "Name", by.y = "admin_name", all = TRUE)


library(plyr)

#2. plot lhz_unsplit
#3. average markets inside polygons

weights$X <- NULL
weights$ADMIN3 <- NULL
weights$EFF_PERD <- NULL
weights$Id <- NULL
weights$OBJECTID <- NULL
#weights <- weights@data

library(doBy)
price_weights <- merge(weights, price_m_markets, by = "Name")
unweighted_price <- summaryBy(price~FNID+FNID_OLD+date, data =price_weights, FUN = mean)
price_weights$weights <- ifelse(!is.na(price_weights$weights), price_weights$weights, 0)
price_weights$agg_price <- price_weights$weights*price_weights$price
weighted_price <- summaryBy(agg_price~FNID+FNID_OLD+date, data = price_weights, FUN = sum)
price_both <- merge(unweighted_price, weighted_price, by = c("FNID", "date"))


### chirps weights ###
pop_district <- weights[, c("FNID", "FNID_OLD","pop.sum")]
pop_district <- unique(pop_district)
pop_district$pop_weight <- pop_district$pop.sum/sum(pop_district$pop.sum, na.rm = T)
#pop_district$pop_total <- pop_district$pop.sum*pop_district$pop_weight


chirps_ipc_pop <- merge(ipc_chirps, pop_district, by = "FNID", all = TRUE)
chirps_ipc_pop$chirps_max_w <- chirps_ipc_pop$chirps_max*chirps_ipc_pop$pop_weight*100
chirps_ipc_pop$chirps_min_w <- chirps_ipc_pop$chirps_min*chirps_ipc_pop$pop_weight*100
chirps_ipc_pop$chirps_mean_w <- chirps_ipc_pop$chirps_mean*chirps_ipc_pop$pop_weight*100
#chirps_pop <- summaryBy(chirps_mean+chirps_mean_w+chirps_min+chirps_min_w+chirps_max+chirps_max_w~FNID+date, data = chirps_ipc_pop, FUN = mean)
chirp_ipc_pop_price <- merge(chirps_ipc_pop, price_both, by.x = c("FNID","FNID_OLD.x", "date"), by.y = c("FNID","FNID_OLD", "date"))
attach(chirp_ipc_pop_price)
chirp_pop_w <- summaryBy(chirps_mean_w+chirps_max_w+chirps_min_w~FNID+date, data = chirp_ipc_pop_price, FUN = sum)
chirp_pop_all <- merge(ipc_chirps, chirp_pop_w, by = c("FNID", "date"), all = T)
chirp_pop_simple <- chirp_pop_all[-c(10,11,14,15,17)]

chirp_pop_price <- merge(chirp_pop_simple, price_both, by = c("FNID", "date"), all = T)
chirp_pop_price$FNID_OLD.x <- NULL

#setcolorder(chirp_ipc_pop_price, c("FNID", "date","V","IPC", "chirps_mean", "chirps_mean_w", "chirps_max",
                 #"chirps_max_w", "chirps_min", "chirps_min_w"))
colnames(chirp_pop_price)[15] <- "price_mean"
colnames(chirp_pop_price)[17] <- "price_w"
#colnames(chirp_ipc_pop_price)[7] <- "ADMIN2"

write.csv(chirp_pop_price, file = "chirp_ipc_pop_price.csv")
