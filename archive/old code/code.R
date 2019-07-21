

setwd("C:\\Users\\YeGuyu\\Downloads\\Malawi")

package = c("maptools", "rgdal", "PBSmapping", "raster", "snow")
lapply(package, require, character.only = TRUE)
memory.limit(7000)

landscan_pop <- raster("LandScanData/Population/lspop2011")
landscan_clip <- crop(landscan_pop, extent(lhz))
landscan_clip <- mask(landscan_clip, lhz)
lhz <- readOGR(".", "livelihood_zones_split")
markets_theissen <- readOGR(".", "Markets_Polygon_Clip")
plot(markets_theissen)

#### Define projection system ####
proj.latlon <- CRS("+proj=longlat +datum=WGS84")
proj4string <- CRS("+init=epsg:20936")

lhz.proj <- spTransform(lhz, CRS = proj4string)
markets_thsn.proj <- spTransform(markets_theissen, CRS = proj4string)
pop.proj <- projectRaster(landscan_clip, crs = proj4string)


## select the first livelihood zone
plot(subset(lhz.proj, LZNUM == "1"))
## select the first theissen polygon
plot(subset(markets_thsn.proj, OBJECTID == "1"), add = TRUE, col = "red")
plot(subset(markets_thsn.proj, OBJECTID == "2"), add = TRUE, col = "red")     
thsn1 <- lhz.proj[subset(markets_thsn.proj, OBJECTID =="1"),]
thsn2 <- intersect(lhz.proj, subset(markets_thsn.proj, OBJECTID =="1"))

lhz.proj@data$split_id <- seq(1, nrow(lhz.proj))
thsn_lhz <- intersect(lhz.proj, markets_thsn.proj)
plot(thsn_lhz)

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

     