suppressPackageStartupMessages(library(jsonlite)) # for working with JSON data
suppressPackageStartupMessages(library(tidyverse)) # to transform and clean data
suppressPackageStartupMessages(library(tidyr)) # for help with turning JSONs into tidy frames

source("BreweryDBRWrapper.R")

# in order to make more dynamic, could keep track of how many observations we've seen and 
# do the math to see if we should update (not a priority)

# this function retrieves all the beer data from the API and stores it in the
# path stored in the beersFile variable
retrieveBeerData <- function(APIKey, saveFile) {
  # takes about an hour to fill
  beers <- NULL
  
  beersRequestData <- BreweryDB_endpoint(APIKey, "beers", 
                                         options = list(p = as.character(1), 
                                                        withBreweries = "Y")) %>%
    content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
  beerNumPages <- beersRequestData$numberOfPages
  
  for (i in 1:beerNumPages) {
    print(i)
    beersRequestData <- BreweryDB_endpoint(APIKey, "beers", 
                                           options = list(p = as.character(i), 
                                                          withBreweries = "Y")) %>%
      content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
    unfilteredBeerData <- beersRequestData$data
    
    unfilteredBeerData$categoryId <- unfilteredBeerData$style$categoryId
    
    unfilteredBeerData$breweryId <- lapply(unfilteredBeerData$breweries, 
                                           FUN = function(x) { paste(x$id, 
                                                                     collapse = " ") })
    
    headerstoAdd <- setdiff(c("id", "name", "description", "abv", "ibu", 
                              "styleId", "srmId"), 
                            names(unfilteredBeerData))
    if (!is_empty(headerstoAdd)) {
      for (colName in headerstoAdd) {
        unfilteredBeerData[colName] <- NA
      }
    }
    if (is.null(beers)) {
      beers <- unfilteredBeerData %>%
        select(id, name, description, abv, ibu, styleId, categoryId, breweryId, 
               srmId) %>%
        as_tibble()
    } else {
      beers <- rbind(beers, unfilteredBeerData %>% 
                       select(id, name, description, abv, ibu, styleId, 
                              categoryId, breweryId, srmId) %>%
                       as_tibble())
    }
  }
  rm(beersRequestData, unfilteredBeerData, headerstoAdd)
  write_rds(beers, saveFile)
}

# this function retrives all of the brewery information and stores it in the path
# in the breweriesFile variable
retriveBreweryData <- function(APIKey, saveFile) {
  # takes a few minutes
  breweries <- NULL
  
  breweriesRequestData <- BreweryDB_endpoint(APIKey, "breweries", 
                                             options = list(p = as.character(1))) %>%
    content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
  breweryNumPages <- breweriesRequestData$numberOfPages
  
  for (i in 1:breweryNumPages) {
    breweriesRequestData <- BreweryDB_endpoint(APIKey, "breweries", 
                                               options = list(p = as.character(i), 
                                                              withLocations = "Y")) %>%
      content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
    unfilteredBreweriesData <- breweriesRequestData$data
    
    unfilteredBreweriesData$locationId <- lapply(unfilteredBreweriesData$locations, 
                                                 FUN = function(x) { paste(x$id, 
                                                                           collapse = " ") })
    
    # the headers left to add if there is a column that is missing before we add them
    headerstoAdd <- setdiff(c("id", "name", "description", "website", "established", 
                              "isOrganic"), 
                            names(unfilteredBreweriesData))
    
    if (!is_empty(headerstoAdd)) {
      for (colName in headerstoAdd) {
        # add missing column if it exists
        unfilteredBreweriesData[colName] <- NA
      }
    }
    if (is.null(breweries)) {
      breweries <- unfilteredBreweriesData %>%
        select(id, name, description, website, established, isOrganic, locationId) %>%
        as_tibble()
    } else {
      breweries <- rbind(breweries, unfilteredBreweriesData %>% 
                           select(id, name, description, website, established, 
                                  isOrganic, locationId) %>%
                           as_tibble())
    }
    
  }
  rm(breweriesRequestData, unfilteredBreweriesData, headerstoAdd)
  write_rds(breweries, saveFile) 
}

# this function retrives all of the information about all of the possible locations
# and stores it in the path stored in the locationsFile data
retrieveLocationsData <- function(APIKey, saveFile) {
  locations <- NULL
  
  locationsRequestData <- BreweryDB_endpoint(APIKey, "locations", 
                                             options = list(p = as.character(1))) %>%
    content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
  locationNumPages <- locationsRequestData$numberOfPages
  
  # takes about 4 minutes
  for (i in 1:locationNumPages) {
    locationsRequestData <- BreweryDB_endpoint(APIKey, "locations", 
                                               options = list(p = as.character(i))) %>%
      content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
    unfilteredLocationsData <- locationsRequestData$data
    
    # the headers left to add if there is a column that is missing before we add them
    headerstoAdd <- setdiff(c("id", "name", "streetAddress", "locality", "region", 
                              "postalCode", "latitude", "longitude", 
                              "locationTypeDisplay", "isPrimary", "countryIsoCode", 
                              "inPlanning", "isClosed", "openToPublic", "yearOpened", 
                              "website", "breweryId"), 
                            names(unfilteredLocationsData))
    
    if (!is_empty(headerstoAdd)) {
      for (colName in headerstoAdd) {
        # add missing column if it exists
        unfilteredLocationsData[colName] <- NA
      }
    }
    if (is.null(locations)) {
      locations <- unfilteredLocationsData %>%
        select(id, name, streetAddress, locality, region, postalCode, latitude, 
               longitude, locationTypeDisplay, isPrimary, countryIsoCode, inPlanning,
               isClosed, openToPublic, yearOpened, website, breweryId) %>% 
        as_tibble() 
    } else {
      locations <- rbind(locations, unfilteredLocationsData %>% 
                           select(id, name, streetAddress, locality, region, 
                                  postalCode, latitude, longitude, locationTypeDisplay, 
                                  isPrimary, countryIsoCode, inPlanning, isClosed,
                                  openToPublic, yearOpened, website, breweryId) %>%
                           as_tibble())
    }
  }
  rm(locationsRequestData, unfilteredLocationsData, headerstoAdd)
  write_rds(locations, saveFile) 
}

