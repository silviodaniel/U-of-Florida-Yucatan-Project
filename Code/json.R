#usr/bin/R/

#### This script uses RCurl and RJSONIO to download data from Google's API:
#### Latitude, longitude, location type (see explanation at the end), formatted address
#### Notice ther is a limit of 2,500 calls per day
# install.packages("RCurl")
# install.packages("RJSONIO")

library(RCurl)
library(RJSONIO)
library(plyr)

url <- function(address, return.call = "json", sensor = "false") {
  root <- "https://maps.googleapis.com/maps/api/geocode/"
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
address <- c("The White House, Washington, DC", "The Capitol, Washington, DC")
locations <- ldply(address, function(x) geoCode(x))
names(locations) <- c("lat", "lng", "location_type", "formatted")
head(locations)

#Task: to have this output from ldply but have all addresses stored in "address"





