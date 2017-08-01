library(jsonlite) # for working with JSON data
library(tidyjson) # also for working with JSON data
library(tidyverse) # to transform and clean data


source("BreweryDBRWrapper.R")

# my BreweryDB API key
breweryDBKey <- "11b192faea1a549172fe2423db077bc5" 

#the file in which the beers R data frame is stored
beersFile <- "data/beers.rds"

#the file in which the breweries R data frame is stored
breweriesFile <- "data/breweries.rds"

#the file in which the locations R data frame is stored
locationsFile <- "data/locations.rds"


beerStylesRequestData <- BreweryDB_endpoint(breweryDBKey, "styles") %>%
  content(as = "text", encoding = "UTF-8")
beerStyles <- fromJSON(beerStylesRequestData, simplifyDataFrame = TRUE)$data %>%
  select(id, categoryId, name, shortName, description, ibuMin, ibuMax, abvMin, 
         abvMax, srmMin, srmMax, ogMin, fgMin, fgMax) %>%
  as_tibble()


beers <- read_rds(beersFile)

beers <- NULL
# takes about 40 minutes to fill
for (i in 1:1317) {
  beersRequestData <- BreweryDB_endpoint(breweryDBKey, "beers", options = list(p = as.character(i))) %>%
    content(as = "text", encoding = "UTF-8")
  if (is.null(beers)) {
    beers <- fromJSON(beersRequestData, simplifyDataFrame = TRUE)$data %>% 
      select(id, name, description, abv, ibu, styleId) %>%
      as_tibble()
  } else {
    # some beers don't have an ibu content and therefore don't have an ibu column
    # the same goes for description
    

    # the headers left to add if there is a column that is missing before we add them
    headerstoAdd <- setdiff(c("id", "name", "description", "abv", "ibu","styleId"), 
                            names(fromJSON(beersRequestData, simplifyDataFrame = TRUE)$data))
    if (is_empty(headerstoAdd)) {
      beers <- rbind(beers, fromJSON(beersRequestData)$data %>%
                       select(id, name, description, abv, ibu, styleId) %>%
                       as_tibble())
    } else {
        beersData <- fromJSON(beersRequestData, simplifyDataFrame = TRUE)$data
        for (colName in headerstoAdd) {
          beersData[colName] <- NA
        }
        beers <- rbind(beers, beersData %>% 
                         select(id, name, description, abv, ibu, styleId) %>%
                         as_tibble())
      }
  }
}
rm(beersData)
write_rds(beers, beersFile)


########## GETTING RATINGS ##########






############### END #################



# beers <- NULL
# for (i in seq_along(beerStyles$id)) {
#   beersRequestData <- BreweryDB_endpoint(breweryDBKey, "beers", options = list(styleId = as.character(i))) %>%
#     content(as = "text", encoding = "UTF-8")
#   if (is.null(beers)) {
#     beers <- fromJSON(beersRequestData)$data %>% 
#       select(id, name, description, abv, ibu, styleId) %>%
#       as_tibble()
#   } else {
#     # some beers don't have an ibu content and therefore don't have an ibu column
#     # the same goes for description
#     
#     # the headers left to add if there is a column that is missing before we add them
#     headerstoAdd <- setdiff(c("id", "name", "description", "abv", "ibu","styleId"), names(fromJSON(beersRequestData)$data))
#     if (is_empty(headerstoAdd)) {
#       beers <- rbind(beers, fromJSON(beersRequestData)$data %>%
#                        select(id, name, description, abv, ibu, styleId) %>%
#                        as_tibble())
#     } else {
#       beersData <- fromJSON(beersRequestData)$data
#       for (colName in headerstoAdd) {
#         beersData[colName] <- NA
#       }
#       beers <- rbind(beers, beersData %>% 
#                        select(id, name, description, abv, ibu, styleId) %>%
#                        as_tibble())
#     }
#   }
# }
# # takes about 3 minutes to get all the beers
# rm(beersData)


beerCategoriesRequestData <- BreweryDB_endpoint(breweryDBKey, "categories") %>%
  content(as = "text", encoding = "UTF-8")
beerCategories <- fromJSON(beerCategoriesRequestData, simplifyDataFrame = TRUE)$data %>%
  select(id, name) %>%
  as_tibble()
beerCategories <- head(beerCategories, -1) # we have a null row at the end


breweries <- read_rds(breweriesFile)

breweries <- NULL
# takes about 2 minutes
for (i in 1:184) {
  breweriesRequestData <- BreweryDB_endpoint(breweryDBKey, "breweries", 
                                             options = list(p = as.character(i))) %>%
    content(as = "text", encoding = "UTF-8")
  if (is.null(breweries)) {
    breweries <- fromJSON(breweriesRequestData, simplifyDataFrame = TRUE)$data %>%
      select(id, name, description) %>%
      as_tibble()
  } else {
    # some breweries don't have a description and therefore don't have a description column

    
    # the headers left to add if there is a column that is missing before we add them
    headerstoAdd <- setdiff(c("id", "name", "description"), 
                            names(fromJSON(breweriesRequestData)$data))
    if (is_empty(headerstoAdd)) {
      breweries <- rbind(breweries, fromJSON(breweriesRequestData, 
                                             simplifyDataFrame = TRUE)$data %>%
                       select(id, name, description) %>%
                       as_tibble())
    } else {
      breweriesData <- fromJSON(beersRequestData, simplifyDataFrame = TRUE)$data
      for (colName in headerstoAdd) {
        # add missing column if it exists
        breweriesData[colName] <- NA
      }
      breweries <- rbind(breweries, breweriesData %>% 
                       select(id, name, description) %>%
                       as_tibble())
    }
  }
}
rm(breweriesData)
write_rds(breweries, breweriesFile)


locations <- read_rds(locationsFile)


locations <- NULL
# takes about 4 minutes
for (i in 1:202) {
  locationsRequestData <- BreweryDB_endpoint(breweryDBKey, "locations", 
                                             options = list(p = as.character(i))) %>%
    content(as = "text", encoding = "UTF-8")
  if (is.null(locations)) {
    locations <- fromJSON(locationsRequestData, simplifyDataFrame = TRUE)$data %>%
      select(id, name, streetAddress, locality, region, postalCode, latitude, longitude, 
             locationTypeDisplay, isPrimary, countryIsoCode, breweryId) %>% 
        as_tibble()
  } else {
    # some locations don't have certain attributes and therefore don't have that particular column
    
    
    # the headers left to add if there is a column that is missing before we add them
    headerstoAdd <- setdiff(c("id", "name", "streetAddress", "locality", "region", 
                              "postalCode", "latitude", "longitude", "locationTypeDisplay", 
                              "isPrimary", "countryIsoCode", "breweryId"), 
                            names(fromJSON(locationsRequestData, simplifyDataFrame = TRUE)$data))
    if (is_empty(headerstoAdd)) {
      locations <- rbind(locations, fromJSON(locationsRequestData, simplifyDataFrame = TRUE)$data %>%
                           select(id, name, streetAddress, locality, region, 
                                  postalCode, latitude, longitude, locationTypeDisplay, 
                                  isPrimary, countryIsoCode, breweryId) %>%
                           as_tibble())
    } else {
      locationsData <- fromJSON(locationsRequestData, simplifyDataFrame = TRUE)$data
      for (colName in headerstoAdd) {
        # add missing column if it exists
        locationsData[colName] <- NA
      }
      locations <- rbind(locations, locationsData %>% 
                           select(id, name, streetAddress, locality, region, 
                                  postalCode, latitude, longitude, locationTypeDisplay, 
                                  isPrimary, countryIsoCode, breweryId) %>%
                           as_tibble())
    }
  }
}
rm(locationsData)
write_rds(locations, locationsFile)
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


beerStyles <- arrange(beerStyles, categoryId)
# adds category information to the diferent styles so we can search styles by category
beerCategoriestoStyles <- merge(beerStyles, beerCategories %>% 
                                  rename(categoryId = id, categoryName = name), 
                                by = "categoryId") %>% rename(styleId = id, styleName = name)

# does not include NA's I believe
beerStylestoBeers <- merge(beers %>% arrange(styleId) %>% rename(beerId = id, beerName = name, beerDescription = description), 
                           beerCategoriestoStyles %>% arrange(styleId), by = "styleId") %>% arrange(styleId, beerName)

