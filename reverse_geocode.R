library(XML)
library(data.table)

load(file = 'jp_people_groups.rda')

findNearbyPlaceName <- function(dat = peopleGroupsDT[i], user = 'jhtravis') {
  geonamesUrl <- paste0("http://api.geonames.org/findNearbyPlaceName?lat=", dat$Latitude, "&lng=", dat$Longitude, "&username=", user)
  urlCon <- curl::curl(geonamesUrl)
  gnData <- readLines(urlCon)
  close(urlCon)
  
  xmlToDataFrame(gnData)
}

reverseGeocodes <- vector("list", length = nrow(peopleGroupsDT))

system.time({
  for(i in seq_along(peopleGroupsDT$PeopleID3)) {
    
    err <- 0
    while(err == 0) {
    
      tryCatch({
        responseDF <- findNearbyPlaceName(dat = peopleGroupsDT[i])
        #responseDF$peopleGroupId3 <- paste0(peopleGroupsDT$PeopleID3[i])
        reverseGeocodes[[i]] <- responseDF
        Sys.sleep(0.1)
        cat('Finished iteration ', i, ' of ', nrow(peopleGroupsDT), '\n')
        
        err <- 1
      }, error = function(e) {
          sleepTime <- 10
          cat(paste('\t', e, '\t - will try again in', sleepTime, 'seconds...\n'))
          Sys.sleep(sleepTime)
      })
    }
  }
})

reverseGeocodesDT <- rbindlist(reverseGeocodes)
write.csv(x = churches2, file = 'churches_with_reverse_geocodes.csv', row.names = FALSE)




# findByGeonameId <- function(dat = reverseGeocodesDT[i], user = 'jhtravis') {
#   geonamesUrl <- paste0("http://api.geonames.org/get?geonameId=", dat$geonameId, "&username=", user)
#   urlCon <- curl::curl(geonamesUrl)
#   gnData <- readLines(urlCon)
#   close(urlCon)
#   
#   cat('Finished iteration ', i, ' of ', nrow(dat), '\n')
#   
#   xmlToDataFrame(gnData)
# }