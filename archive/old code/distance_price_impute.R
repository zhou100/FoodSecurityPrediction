library("geosphere")
library("matrixStats")
library("readxl")
library(dplyr)
library(lubridate)
library("readxl")
library("tidyr")
library("dplyr")
library("openxlsx")
library("zoo")

################################################################################################
# The purpose of this code is impute the missings in prices by prices in the nearest mkt 
#################################################################################################


### linking markets to households
setwd("~/Box Sync/Research/Malawi_FewS/")
mkt_coord <- read.csv("geocoordinates.csv")
#head(mkt_coord)
mkt_coord<-unique(mkt_coord)

# change the names to get match 
name_list<-as.character(mkt_coord$Name)
name_list<-toupper(name_list)
name_list<-gsub("_",".",name_list)
name_list<-gsub(" ",".",name_list)
name_list[name_list=="TSANGANO.TURNOFF"]<-"TSANGANO.TURN.OFF"
name_list[name_list=="BEMBEKE.TURNOFF"]<-"BEMBEKE.TURN.OFF"



####### k nearest neighbor ######
library(FastKNN)
# mkt distance 
mkt_matrix <- distm(mkt_coord[,c('Longitude','Latitude')],fun=distVincentyEllipsoid)
# knn_training_function(mkt_coord, mkt_matrix, mkt_coord[,1], k = 1)

# generate neighbor matrix to save results 
mat_neighbor1<-matrix(NA,nrow(mkt_coord),20)
mat_neighbor2<-matrix(NA,nrow(mkt_coord),10)

for (i in 1:nrow(mkt_coord)){
mat_neighbor1[i,]<-k.nearest.neighbors(i, mkt_matrix, k = 20)
}


#neighbor matrix up to k = 20 
#mat_neighbor1


maize <- read.csv("/Users/yujunzhou/Box Sync/Research/Price_data_auto/maize_price_joined.csv")
maize[maize==0]<-NA
maize<-maize[!(maize$date==2013-07-30 & maize$date==2013-07-29 & maize$date==2013-07-31 & maize$date==2013-12-30),]

maize$yearmo <- format(as.Date(maize$date), "%Y%m")

# monthly price replaced by median of weeks
maize_price_yearmo<-maize[,5:ncol(maize)] %>% 
  group_by(yearmo) %>%
  summarise_all(funs(median(., na.rm=TRUE)))

# transpose 
maize_yearmo_transpose<-t(maize_price_yearmo)
maize_yearmo_transpose<-rbind(maize_yearmo_transpose,maize_price_yearmo$yearmo)
colnames(maize_yearmo_transpose)<-maize_yearmo_transpose[74,]
maize_yearmo_transpose<-maize_yearmo_transpose[-c(1,74),]
maize_yearmo_transpose<-as.data.frame(maize_yearmo_transpose)
maize_yearmo_transpose$name<-rownames(maize_yearmo_transpose)
maize_yearmo_transpose

# match the order of mkts in prices to the mkt_coordinate file
maize_price_order<-maize_yearmo_transpose[match(name_list, maize_yearmo_transpose$name),]
# check if the names are in the same order 
#rownames(maize_price_order)
#name_list

maize_price_order<- maize_price_order[,-ncol(maize_price_order)]
# store them in a matrix
m1<-as.matrix(maize_price_order)
class(m1) <- "numeric" 
# replace  the 0 prices as NA
m1[m1==0.000]<-NA
maize_price_order<-as.data.frame(m1)


# for a given k 
# if maize_price_order[i,j] has an na, then go grab an average of maize_price_order[mat_neighbor2[i,1:k],j] 
# check if maize_price_order still has an NA, if not, end the loop
missings<-matrix(NA,10,2)

for (k in 1:10){
  for (i in 1:nrow(maize_price_order)){
    for (j in 1:ncol(maize_price_order)){
      if (is.na(maize_price_order[i,j])==TRUE) 
        maize_price_order[i,j]<-median(maize_price_order[mat_neighbor1[i,1:k],j],na.rm = TRUE)
    }
  }
  
  # check if no missings remain (can't do this because several weeks are always missing)
 # if (any(colSums(is.na(maize_price_order))==0)) break
  # store the result
  missings[k,1]<-k
  missings[k,2]<-sum(colSums(is.na(maize_price_order))) # total missings
}

missings



# for the several weeks  that are completely missing, use temporal interpolation 

sum(colSums(is.na(maize_price_order)))
#maize_price_order[colSums(is.na(maize_price_order))>0]
library(imputeTS)
df<-as.data.frame(t(maize_price_order))
df<-cbind(maize_price_yearmo$yearmo,df)

x <- zoo(df[,2:ncol(df)],df[,1])
x <- na.interpolation(x, option = "linear")
x

sum(colSums(is.na(x)))
write.csv(x,"imputed_price_yearmo.csv")

# save the
