library(httr)
library(jsonlite)
library(data.table)

joshuaProjectGET <- function(apiKey = '7B08wuY152TI', limit = 1000, page = 1) {
  url = paste0('https://joshuaproject.net/api/v2/people_groups?api_key=', apiKey, '&limit=', limit, '&page=', page)
  return(fromJSON(url))
}

# modify variables (NOTE: seems like 1000 is per query limit)
lim <- 1000
key <- '7B08wuY152TI' # ideally use environment variable for API key

responseList <- joshuaProjectGET(apiKey = key, limit = lim)
paginationTotalPages <- responseList$meta$pagination$total_pages

# need to add more robust exception handling (after hackathon)
if(paginationTotalPages > 0 & responseList$status$status_code == 200) {
  index <- seq(paginationTotalPages)
  
  # convert to lapply??
  peopleGroups <- vector("list", length = length(index))
  for (i in index) {
    responseList <- joshuaProjectGET(apiKey = key, limit = lim, page = i)
    peopleGroups[[i]] <- responseList$data
    cat(paste0(' - Finished page ', i, ' of ', (length(index)), ' \n'))
  }
  
  peopleGroupsDT <- rbindlist(peopleGroups)
  
}

# use lookup table to assign progress level description based on https://joshuaproject.net/global_list/progress
jpScaleDefs <- c('1' = 'Unreached', '2' = 'Formerly or falsely reached', '3' = 'Minimally reached', '4' = 'Partially reached', '5' = 'Significantly reached')
peopleGroupsDT$JPScaleDefs <- unname(jpScaleDefs[as.character(peopleGroupsDT$JPScale)])

# idealy will write to database, just save as CSV and RDS for now
# setwd('<SET PROPER WORKING DIRECTORY>')
write.csv(x = peopleGroupsDT, file = 'jp_people_groups.csv', row.names = FALSE)
save(object = peopleGroupsDT, file = 'jp_people_groups.rda')