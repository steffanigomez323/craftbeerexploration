library(httr)

# the BreweryDB API key for authentication
breweryDBKey <- ""

# the base BreweryDB URL that queries will be appended to
breweryDBBaseURL <- "http://api.brewerydb.com/v2/"

# this function returns the beers endpointn of the API, using parameter
# requests if necessary
BreweryDB_beers <- function(api_key) {
  print(api_key)
  GET(paste(breweryDBBaseURL, "beers", sep=""), query = list(key = api_key, ids = c("z3VfEn")))
}
