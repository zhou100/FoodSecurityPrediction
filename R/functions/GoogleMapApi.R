###  script from josecarlosgonz 
####  https://gist.github.com/josecarlosgonz/6417633

#### This script uses RCurl and RJSONIO to download data from Google's API:
#### Latitude, longitude, location type (see explanation at the end), formatted address
#### Notice ther is a limit of 2,500 calls per day


library(RCurl)
library(RJSONIO)
library(dplyr)

map.key = ""

url <- function(address, return.call = "json", sensor = "false") {
  root <- "https://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, paste("&key=",map.key,sep=""), sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    lat = as.numeric(as.character(lat))
    lng = as.numeric(as.character(lng))
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA,NA))
  }
}



 
# write this into a function 
coordFind <- function(address,verbose=FALSE) {
# address is a list of strings of places that you want to find
  locations <- sapply(address, function(x){geoCode(x)})
  loc.df = as.data.frame(t(locations))
  colnames(loc.df) <- c("lat", "lon", "location_type", "formatted")
  loc.df = loc.df %>% tibble::rownames_to_column(var = "search")
  loc.df$lat = as.numeric(as.character(loc.df$lat))
  loc.df$lon = as.numeric(as.character(loc.df$lon))
  

  return (loc.df)
}


############################################
## usage: make the address you want into a vector 
###########################################
##Test with a single address
address <- c("The White House, Washington, DC", "The Capitol, Washington, DC")

coordFind(address)









########################
#Location type, for more info check here: https://developers.google.com/maps/documentation/directions/
#"ROOFTOP" indicates that the returned result is a precise geocode for which we have location information accurate down to street address precision.
#RANGE_INTERPOLATED" indicates that the returned result reflects an approximation (usually on a road) interpolated between two precise points (such as intersections). Interpolated results are generally returned when rooftop geocodes are unavailable for a street address.
#GEOMETRIC_CENTER" indicates that the returned result is the geometric center of a result such as a polyline (for example, a street) or polygon (region).
#APPROXIMATE" indicates that the returned result is approximate.
###############################


##############