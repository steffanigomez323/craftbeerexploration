library(httr)

# the BreweryDB API key for authentication
breweryDBKey <- ""

# the base BreweryDB URL that queries will be appended to
breweryDBBaseURL <- "http://api.brewerydb.com/v2/"

# this function returns the beers endpoint of the API, using parameter
# it needs name, abv, ibu, glasswareId, srmId, availableId, styleId or ids
# without premium to get beers
BreweryDB_endpoint <- function(api_key, endpoint, options = NULL) {
  if (is.null(options)) {
    GET(paste(breweryDBBaseURL, endpoint, sep = ""), query = list(key = api_key))
  } else {
    paramsRequest <- c(options, key = api_key)
    GET(paste(breweryDBBaseURL, endpoint, sep = ""), query = paramsRequest)
  }
}