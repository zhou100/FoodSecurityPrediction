###  script from josecarlosgonz 
####  https://gist.github.com/josecarlosgonz/6417633

#### This script uses RCurl and RJSONIO to download data from Google's API:
#### Latitude, longitude, location type (see explanation at the end), formatted address
#### Notice ther is a limit of 2,500 calls per day

library(RCurl)
library(RJSONIO)
library(plyr)

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
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
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA,NA))
  }
}
##Test with a single address
#address #address
#[1] "38.8976831"
#[2] "-77.0364972"
#[3] "APPROXIMATE"
#[4] "The White House, 1600 Pennsylvania Avenue Northwest, Washington, D.C., DC 20500, USA"

# Use plyr to ggeocoding for a vector
# address <- c("The White House, Washington, DC", "The Capitol, Washington, DC")
# locations <- ldply(address, function(x) geoCode(x))
# names(locations) <- c("lat", "lon", "location_type", "formatted")
# head(locations)

# write this into a function 
coordFind <- function(address,verbose=FALSE) {
# address is a list of strings of places that you want to find
  locations <- ldply(address, function(x) geoCode(x))
  names(locations) <- c("lat", "lon", "location_type", "formatted")
  return (locations)
}


# lat lon location_type
# 1 38.8976831 -77.0364972 APPROXIMATE
# 2 38.8899389 -77.0090505 APPROXIMATE
#formatted
# 1 The White House, 1600 Pennsylvania Avenue Northwest, Washington, D.C., DC 20500, USA
# 2 United States Capitol, East Capitol St NE & First St, Washington, D.C., DC 20004, USA




##########
#Location type, for more info check here: https://developers.google.com/maps/documentation/directions/
#"ROOFTOP" indicates that the returned result is a precise geocode for which we have location information accurate down to street address precision.
#RANGE_INTERPOLATED" indicates that the returned result reflects an approximation (usually on a road) interpolated between two precise points (such as intersections). Interpolated results are generally returned when rooftop geocodes are unavailable for a street address.
#GEOMETRIC_CENTER" indicates that the returned result is the geometric center of a result such as a polyline (for example, a street) or polygon (region).
#APPROXIMATE" indicates that the returned result is approximate.