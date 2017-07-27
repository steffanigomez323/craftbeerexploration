library(httr) # used for GET requests from the BreweryDB API
library(jsonlite) # for working with JSON data
library(lubridate) # for working with dates


# my BreweryDB API key
breweryDBKey <- "11b192faea1a549172fe2423db077bc5" 

# the base BreweryDB URL that queries will be appended to
breweryDBBaseURL <- "http://api.brewerydb.com/v2/?"

beers <- GET(breweryDBBaseURL, query = list(api_key = breweryDBKey))
print(beers)

