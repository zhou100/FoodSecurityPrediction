# set your working directory 
# username <- "zhou100"
# mydir <- paste("/Users/yujunzhou/Google Drive/dataImprove/", username, sep ="")
# setwd (mydir) 
setwd ("//ad.uillinois.edu/aces/ACE/personal/zhou100/")



library(ggplot2) #Calls: fortify, ggplot
library(rgdal)   
library(sp)      
library(raster)
library(gdalUtils)
library(parallel)
library(RCurl)
library(R.utils)
library(rgeos)
library(dplyr)

package = c("maptools", "rgdal", "PBSmapping", "raster", "snow")
lapply(package, require, character.only = TRUE)


#Read Malawi map (shapefile)
# "../../map" is a relative path from your working directory - it means up two directories, then inside the map directory
#Mal=readOGR(paste(getwd(),"/buffer/buffer.shp",sep = ""),layer="buffer")
#Mal=readOGR("Ward",layer="ward_2010_exported")
Mal=readOGR(paste(getwd(),"malawi_buffer_new.shp",sep = "/"))

start_year = 2008 # pick your start year
end_year = 2016 # pick your end year

## Define URL and file names for bulk download
url<-character(length = length(seq(start_year,end_year)))
for (i in 1:length(url)){
  year<-seq(start_year,end_year)[i]
  url[i]<-paste(paste("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_daily/tifs/p05",year,sep="/"),"/",sep="")
}


for (i in 1:length(seq(start_year,end_year))){
  
filename <- getURL(url[i], ftp.use.epsv = FALSE, dirlistonly = TRUE)
# filename <- strsplit(filename, "\n")
# if you are winodws user, use this line instead
filename <- strsplit(filename, "\r\n") # windows user
filenames <- unlist(filename)


for (filename in filenames) {
  download.file(paste(url[i], filename, sep = ""), 
                paste(getwd(), "/", filename, sep = ""))
}
}


## Unzip all the gz. files in working directory

zip.list <- list.files(getwd(),
                       pattern = "tif.gz$",
                       full.names = TRUE)
#zip.list
for(zipfile in zip.list) {
  gunzip(zipfile)
}

# 
rlist <- list.files(path=getwd(), 
                    pattern = "tif$",
                    full.names=TRUE)
## Stack raster layers in a list

r <- stack(rlist)

#rlist
#r

time2 <- proc.time() #start timer
cat("Download time:","\n")
summary(time2 - starttime)

proj.latlon <- CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")
Mal <- spTransform(Mal, CRS = proj.latlon)


detach("package:R.utils", unload=TRUE) # to prevent errors in the extract below

mat.data <- c()
time1 <- proc.time() #start timer
#
for(i in 1:nlayers(r)) {
  #ex <- extract(r[[i]], Mal)
  raster_proj <- projectRaster(r[[i]], crs = proj.latlon)
  clip1_mal <- crop(raster_proj, extent(Mal)) #crop to extent of polygon
  clip2_mal <- rasterize(Mal, clip1_mal, mask=TRUE)
  ex <- extract(clip2_mal, Mal)
  #mat <- t(mclapply(ex, FUN = mean,mc.cores = 4)) #multiple core version
  mat <- t(lapply(ex, FUN = mean ))
  mat.data <-rbind(mat.data, mat)
}


# transform the eaid feature in factor form to characters 
name_list = list(lenth= length(Mal$ea_id))
i <- sapply(Mal$ea_id, is.factor) 
name_list[i] <- lapply(Mal$ea_id[i], as.character)

# Assign household ID to the data and check for overlap
colnames(mat.data) <- unlist(name_list)
overlap <- mat.data[ , colSums(is.na(mat.data)) != 0] # check for the overlapped ones 
mat.data<-mat.data[ , colSums(is.na(mat.data)) == 0] # save the complete data 

# subset the shapefile to 
Mal_overlap<-subset(Mal, ea_id %in% colnames(overlap))
mat.data.overlap <- c()

for(i in 1:nlayers(r)) {
  raster_proj <- projectRaster(r[[i]], crs = proj.latlon)
  clip1_mal_overlap <- crop(raster_proj, extent(Mal_overlap)) #crop to extent of polygon
  clip2_mal_overlap <- rasterize(Mal_overlap, clip1_mal_overlap, update=TRUE,updateValue=NA)
  ex <- extract(clip2_mal_overlap, Mal_overlap)
  #mat <- t(mclapply(ex, FUN = mean,mc.cores = 4)) #multiple core version
  mat <- t(lapply(ex, FUN = mean ))
  mat.data.overlap <-rbind(mat.data.overlap, mat)
}

# Combining first and second iterations
colnames(mat.data.overlap)<-as.character(Mal_overlap$ea_id)
mat.data<-dplyr::bind_cols(as.data.frame(mat.data),as.data.frame(mat.data.overlap))


colnames(mat.data)<-Mal_overlap$ea_id     # Assign distrcit names 

date_list = c()
for(i in 1:nlayers(r)){
  date1<-unlist(strsplit(rlist[[i]], "\\\\"))[3]
  date1<-gsub(pattern = "ad.uillinois.edu/aces/ACE/personal/zhou100/chirps-v2.0.",replacement="",x= date1)
  date1<-gsub(pattern = ".tif",replacement = "", x = date1)
  date1<-as.Date(date1,"%Y.%m.%d")
  date1<-format(date1, "%m/%d/%Y")
  date_list<-append(date_list,date1)
}

mat.data$Date <- date_list
mat.data<-mat.data[,c(length(mat.data), 1:(length(mat.data)-1))]


write.csv(as.matrix(mat.data),"CHIRPS_malawi_buffer.csv",row.names = FALSE)

time3 <- proc.time() #end timer
cat("Processing time:","\n")
summary(time3 - time1)


endtime <- proc.time() #end timer
cat("Entire time:","\n")
summary(time2 - starttime)
 


