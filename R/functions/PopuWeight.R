##################################################################
# Goal : create population weights for each livelihood zones 

# Input : 
# 1. population raster file from LandScan
# 2. Livelihood zone shapefile 
# 3. an intersection layer of livelihood zone and market thiessen polygon 
# 3.0 Market thessien polygon created by QGIS 
# 3.1. import the market coodinates as point data
# 3.2. voroncani polygon based on the mkt point data with buffers so it's bigger than the lhz
# 3.3. clip the polygon by the lhz shapefile.
# 3.4 intersect it with the livelihood zone shapefile ( I did this in QGIS or ArcGIS to avoid some weird boundary problem)

# all the shapefile shoule be projected to projection system: EPSG:102022 - Africa_Albers_Equal_Area_Conic

# Output: 
# 1. population weights dataframe the following variables
# 1.1 FNid indicating the name of the livelihood zone
# 1.2 market name of the market price 
# 1.3. pop_weight: the relative portion of population in a livelihood zone that belong to each market. 

#################################################################### 

package = c("dplyr","maptools","rgeos", "rgdal", "raster")
lapply(package, require, character.only = TRUE)

PopuWeight <- function(pop_raster, lhz, poly_intersect){

  #  cilip the poplation raster to the extent of the livelihood zones 
  landscan_clip <- crop(pop_raster, extent(lhz))

  ########################################################################################################
  # transform the projection of lhz and thiessen polygons to Africa_Albers_Equal_Area_Conic 
  # do this after the clip above so you don't need to reproject the big landscan raster for the globe 
  #######################################################################################################
  #### define projection in proj4string format
  
  proj.latlon <- CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  
  #### project into the same projection system ####
  lhz_proj <- spTransform(lhz, CRS = proj.latlon)
  poly_intersect_proj <- spTransform(poly_intersect, CRS = proj.latlon)
  landscan_clip.proj<-projectRaster(landscan_clip,crs = proj.latlon)
  
  #######################################################################################################
  # first we need to grab the area that are divided by the market and the livelihood zones 
  # from the intersection of the LHZ and the thiessen polygons 
  # these are the polygon areas that we extract the population from 
  
  areas <- data.frame(area = sapply(poly_intersect_proj@polygons, FUN = function(x){slot(x, "area")}))
  row.names(areas) <- sapply(poly_intersect_proj@polygons, FUN = function(x){slot(x, "ID")})    # save the id 
  
  attArea <- spCbind(poly_intersect_proj, areas) # merge the areas into the intersected shapefile
  attArea$seqID <- paste0("V",seq(nrow(attArea))) # set a sequence id for these little areas (seqID)

  # rasterize it using mask to speed up the extraction 
  landscan_clip_mask <- mask(landscan_clip.proj, lhz_proj)
  
  # extract the population data using the polygon 
  pop_ex <- extract(landscan_clip_mask, attArea)
 
  # calculate the sum of population in each little areas (seqID)
  pop_mat <- sapply(pop_ex, FUN = function(x){sum(x,na.rm= TRUE)})
  
  # save the population of each little area into a dataframe 
  pop_df <-  data.frame( seqID= paste0("V", seq(nrow(attArea))), pop = pop_mat )
  
  # create a concordance table for "seqID","mkt","FNID"
  fnid_mkt_coord<- attArea@data[c("seqID","mkt","FNID")]
  
  # merege popultion of each seqID with mkt and FNID 
  pop_df_join <- dplyr::left_join(fnid_mkt_coord, pop_df, by="seqID")
  
  # compute the sum of population by each livelihood zone
  pop_df_join = pop_df_join %>% dplyr::group_by(FNID) %>% dplyr::mutate(pop_sum = sum(pop))
  
  # compute the relative portion of population in a livelihood zone that belong to each market. 
  pop_df_join = pop_df_join %>% dplyr::mutate(weights = pop/pop_sum)
  
  # save the result and return
  pop_weight = pop_df_join %>% dplyr::select(FNID,mkt,weights)
  
  return(pop_weight)
  
}


 
 
