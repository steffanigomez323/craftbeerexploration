#library(jsonlite) # for working with JSON data
#library(lubridate) # for working with dates

source("BreweryDBRWrapper.R")

# my BreweryDB API key
breweryDBKey <- "11b192faea1a549172fe2423db077bc5" 

beers <- BreweryDB_endpoint(breweryDBKey, "beers", options = list(ids = c("z3VfEn")))
print(beers)

beerStylesRequest <- BreweryDB_endpoint(breweryDBKey, "styles")
print(beerStylesRequest)
beerStylesData <- content(beerStylesRequest)
names(beerStylesData)
names(beerStylesData$data[[1]])
length(beerStylesData$data)
beerStylesData$data[[1]]
beerStylesData$data[[171]]
beerStylesData$data[[1]]$description

# going to use the different styles to request the beers, and use
# the styles to map the beers to categories
# when requesting the beers, request with brewery information

# going to need a category to style, style to beer for sure
# want to make dynamic so we don't save all the data, 
# however for the purpose of this class we are going to make sure
# we save all the ids to beers and breweries, styles, categories, 
# and locations

# this is where data frames come into play, we could have one data frame 
# with category, style, beer, and brewery all into one
# with separate frames for category to style and style to beer
# and beer to brewery (draw it out and see what it looks like)

# then begin porting in data from RateBeer about the different kinds
# of beers