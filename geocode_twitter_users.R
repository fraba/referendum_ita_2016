#!/usr/bin/Rscript

db <- '/home/ubuntu/referendum_ita_2016/twitter/data/referendum_16_twt_store.sqlite'
path_to_file <- '/home/ubuntu/referendum_ita_2016/twitter/data/geolocated_users.RData'
path_to_log <- '/home/ubuntu/referendum_ita_2016/twitter/geocode_twitter_user.log'
api_limit <- 2500

write("", file=path_to_log,append=TRUE)
write("", file=path_to_log,append=TRUE)
write(paste0(Sys.time(), ": ", "Starting script..."), file=path_to_log,append=TRUE)

require(data.table)
source('https://raw.githubusercontent.com/fraba/R_cheatsheet/master/database.R')
source_users <- data.table(sqliteGetTable(db, 'user'))

if (!file.exists(path_to_file)) {
  geolocated_users <- data.table(id = character(),
                                 location = character(), # tolower()
                                 lat = numeric(),
                                 lon = numeric(),
                                 region = character())
} else {
  load(path_to_file)
  write(paste0(Sys.time(), ": ", "Loaded geolocated_users.RData with  ", nrow(geolocated_users), " users."), file=path_to_log,append=TRUE)
}

tmp_location_lonlat <- unique(geolocated_users[,.(location, lat, lon)])

# Define users who require geolocation
users_to_geolocate <- source_users[,.(id,location)]
users_to_geolocate <- users_to_geolocate[location != '']
users_to_geolocate <- subset(users_to_geolocate, !(id %in% geolocated_users$id))
users_to_geolocate$string_is_located <- 
  sapply(users_to_geolocate$location, 
         FUN = function(x) any(tolower(x) %in% tolower(geolocated_users$location)))
users_to_geolocate$location <- tolower(users_to_geolocate$location)

write(paste0(Sys.time(), ": ", nrow(users_to_geolocate), " users to locate."), file=path_to_log,append=TRUE)

# If already located
if (any(users_to_geolocate$string_is_located == TRUE)) {
  
  tmp_geolocated_users <- data.table(id = character(),
                                     location = character())
  
  tmp_geolocated_users <- 
    rbind(tmp_geolocated_users, 
          data.table(
            id = subset(users_to_geolocate, string_is_located==TRUE)$id,
            location = tolower(subset(users_to_geolocate, string_is_located==TRUE)$location)))
  
  setkey(tmp_geolocated_users, 'location')
  setkey(tmp_location_lonlat, 'location')
  
  tmp_geolocated_users <- merge(tmp_geolocated_users, tmp_location_lonlat)
  write(paste0(Sys.time(), ": ", nrow(tmp_geolocated_users), " located using location of other users."), file=path_to_log,append=TRUE)
  users_to_geolocate <- subset(users_to_geolocate, !(id %in% tmp_geolocated_users$id))
} else {
  tmp_geolocated_users <- data.table(id = character(),
                                     location = character(),
                                     lon = numeric(),
                                     lat = numeric())
}

if (nrow(users_to_geolocate) == 0) quit()

library(ggmap)

geocodeString <- function (location) {
  require(ggmap)
  coordinates <- tryCatch(
    geocode(location, output = "latlon"),
    warning = function(w) {
      w
    },
    error=function(e) {
      message("Error connecting with the API\n")
      message("Here's the original error message:\n")
      message(e)
      # Choose a return value in case of error
      return("API error")
    }
  )
  return(cbind(location, coordinates))
}

tmp_description_to_geolocate <- 
  data.table(location = tolower(unique(users_to_geolocate$location)),
             lon = NA,
             lat = NA)
tmp_description_to_geolocate$lon <- as.numeric(tmp_description_to_geolocate$lon)
tmp_description_to_geolocate$lat <- as.numeric(tmp_description_to_geolocate$lat)

bottom_row <- ifelse(nrow(tmp_description_to_geolocate) > api_limit, api_limit, nrow(tmp_description_to_geolocate))

tmp_description_to_geolocate <- 
  tmp_description_to_geolocate[1:bottom_row,]

write(paste0(Sys.time(), ": ", nrow(tmp_description_to_geolocate), " descriptions to geolocate."), file=path_to_log,append=TRUE)

for (i in 1:nrow(tmp_description_to_geolocate)) {
  try ({ 
    result <- geocodeString(tmp_description_to_geolocate$location[i])
    tmp_description_to_geolocate[location == result$location, names(tmp_description_to_geolocate) := as.list(result)][]
    
  })
}
setkey(tmp_description_to_geolocate, 'location')
setkey(users_to_geolocate, 'location')

users_to_geolocate$string_is_located <- NULL

write(paste0(Sys.time(), ": ", nrow(users_to_geolocate), " users geolocated."), file=path_to_log,append=TRUE)

tmp_geolocated_users <-
  rbind(tmp_geolocated_users, 
        merge(users_to_geolocate, tmp_description_to_geolocate))

# Geo
library(sp)
library(rgeos)
library(rgdal)
load('/home/ubuntu/referendum_ita_2016/twitter/data/Reg2011_WGS84_simp.RData')

italy_box_y <- c(36, 47.5)
italy_box_x <- c(6.1, 19.5)

cleaned_tmp_geolocated_users <- tmp_geolocated_users[!is.na(lon),]
cleaned_tmp_geolocated_users <-
  cleaned_tmp_geolocated_users[lon != 12.567380 & lat != 41.87194,] # Italy
cleaned_tmp_geolocated_users <-
  cleaned_tmp_geolocated_users[lon >= italy_box_x[1] & lon <= italy_box_x[2] &
                                 lat >= italy_box_y[1] & lat <= italy_box_y[2],] # Within Italy

user_points <- 
  SpatialPoints(as.matrix(cleaned_tmp_geolocated_users[,.(lon,lat)]),
                proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
crs <- CRS(sprintf(utmStr, 32))
pUTM <- spTransform(italy_adm1, crs)
ptsUTM <- spTransform(user_points, crs)

## Set up container for results
n <- length(ptsUTM)
nearestRegions <- character(n)

## For each point, find name of nearest polygon
for (i in seq_along(nearestRegions)) {
  nearestRegions[i] <- as.character(pUTM$NOME)[which.min(gDistance(ptsUTM[i,], pUTM, byid=TRUE))]
}

cleaned_tmp_geolocated_users$region <- nearestRegions
setkey(cleaned_tmp_geolocated_users, 'id')
setkey(tmp_geolocated_users, 'id')

tmp_geolocated_users <- 
  merge(tmp_geolocated_users, cleaned_tmp_geolocated_users[,.(id,region)], all.x = TRUE)

geolocated_users <- rbind(geolocated_users, tmp_geolocated_users)

save(geolocated_users, file = path_to_file)

write(paste0(Sys.time(), ": ", "end of script."), file=path_to_log,append=TRUE)


