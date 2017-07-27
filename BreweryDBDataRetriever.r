library(jsonlite) # for working with JSON data
library(lubridate) # for working with dates


# my BreweryDB API key
breweryDBKey <- "11b192faea1a549172fe2423db077bc5" 

source("BreweryDBRWrapper.R")

beers <- BreweryDB_beers(breweryDBKey)
print(beers)
