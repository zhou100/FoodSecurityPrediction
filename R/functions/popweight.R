##################################################################
# Goal : create population weights for each livelihood zones 

# Input : 
# 1. population raster file from LandScan
# 2. Livelihood zone shapefile 
# 3. Market thessien polygon created by QGIS 
# 3.1. import the market coodinates as point data
# 3.2. voroncani polygon based on the mkt point data with buffers so it's bigger than the lhz
# 3.3. clip the polygon by the lhz shapefile.

# all the shapefile are projected in advance to the projection system: EPSG:102022 - Africa_Albers_Equal_Area_Conic

# Output: 
# 1. population weights
#################################################################### 



package = c("sf","tidyverse","maptools","rgeos", "rgdal", "PBSmapping", "raster", "snow")
lapply(package, require, character.only = TRUE)



popweight <- function(pop_raster, lhz, thiessen_poly_clip,poly_intersect){
  
  landscan_clip <- crop(pop_raster, extent(lhz))
  landscan_clip <- mask(landscan_clip, lhz)
  
  
  #### project into the same projection system ####
  
  proj.latlon <- CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  
  ########################################################################################################
  # if your lhz and thiessen polygons are not in the format already, run these two lines 
  #######################################################################################################
  #lhz <- spTransform(lhz, CRS = proj.latlon)
  #thiessen_poly_clip <- spTransform(thiessen_poly_clip, CRS = proj.latlon)
  
  
  pop.proj <- projectRaster(landscan_clip, crs = proj.latlon)
  
  
  
  
}




## select the first livelihood zone
plot(subset(lhz.proj, LZNUM == "1"))
## select the first theissen polygon
plot(subset(markets_thsn.proj, OBJECTID == "1"), add = TRUE, col = "red")
plot(subset(markets_thsn.proj, OBJECTID == "2"), add = TRUE, col = "red")     
thsn1 <- lhz.proj[subset(markets_thsn.proj, OBJECTID =="1"),]
thsn2 <- intersect(lhz.proj, subset(markets_thsn.proj, OBJECTID =="1"))

markets_theissen_TZ
lhz_TZ

thsn_lhz = st_intersection(lhz_TZ,markets_theissen_TZ)
thsn_lhz = intersect(lhz_UG,markets_theissen_UG)


thsn_lhz <- intersect(lhz.proj, markets_thsn.proj)
plot(thsn_lhz)
thsn_lhz = lhz_TZ_intersect

areas <- data.frame(area = sapply(thsn_lhz@polygons, FUN = function(x){slot(x, "area")}))
row.names(areas) <- sapply(thsn_lhz@polygons, FUN = function(x){slot(x, "ID")})
attArea <- spCbind(thsn_lhz, areas)
attArea$seqID <- paste0("V",seq(nrow(attArea)))
View(attArea)
pop_ex <- extract(pop.proj, attArea)
mat <- t(lapply(pop_ex, FUN = sum))
tmat <- data.frame(matrix(unlist(t(mat)), nrow=250, byrow=T))
tmat$ID <- paste0("V", seq(nrow(tmat)))
colnames(tmat)[1] <- "pop"
pop_thsn <- merge(attArea, tmat, by.x = "seqID", by.y = "ID")


shape_pop <- pop_thsn[, c("OBJECTID", "pop")]
shape_pop@data <- summaryBy(pop~OBJECTID, FUN = sum, data = shape_pop@data, na.rm = T)
View(shape_pop)
pop_thsn <- merge(pop_thsn, shape_pop@data, by = "OBJECTID")
pop_thsn$weights <- pop_thsn@data$pop/pop_thsn@data$pop.sum

