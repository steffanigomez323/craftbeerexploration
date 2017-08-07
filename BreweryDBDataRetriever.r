library(jsonlite) # for working with JSON data
library(tidyjson) # also for working with JSON data
# ran into bugs in the package so had to ditch it and do it by hand
library(tidyverse) # to transform and clean data
library(tidyr) # for help with turning JSONs into tidy frames

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
         abvMax, srmMin, srmMax, ogMin, ogMax, fgMin, fgMax) %>%
  as_tibble()
rm(beerStylesRequestData)
# takes about 40 minutes to fill
#  id"                        "name"                      "nameDisplay"               "description"              
# "abv"                       "glasswareId"               "srmId"                     "styleId"                  
# "isOrganic"                 "status"                    "statusDisplay"             "createDate"               
# "updateDate"                "glass"                     "srm"                       "style"                    
# "breweries"                 "ibu"                       "availableId"               "available"                
# "labels"                    "originalGravity"           "servingTemperature"        "servingTemperatureDisplay"

# beers:
#  id"                        "name"                      "nameDisplay"               "description"              
# "abv"                       "glasswareId"               "srmId"                     "styleId"                  
# "isOrganic"                 "status"                    "statusDisplay"             "createDate"               
# "updateDate"                "glass"                     "srm"                                          
# "breweries"                 "ibu"                       "availableId"               "available"                
# "labels"                    "originalGravity"           "servingTemperature"        "servingTemperatureDisplay"
#   "style" :
#       id, categoryId, category (id, name), name, description, ibuMin, ibuMax, abvMax, srmMin, srmMax, 
#       ogMin, fgMin, fgMax
#   breweries:
#     "id"                  "name"                "nameShortDisplay"    "description"         "website"            
#     "established"         "isOrganic"           "images"              "status"              "statusDisplay"      
#     "createDate"          "updateDate"          "isMassOwned"         "brandClassification" 
#     "locations":
#         "id"                  "name"                "locality"            "region"              "postalCode"         
#         "website"             "latitude"            "longitude"           "isPrimary"           "inPlanning"         
#         "isClosed"            "openToPublic"        "locationType"        "locationTypeDisplay" "countryIsoCode"     
#         "yearOpened"          "status"              "statusDisplay"       "createDate"          "updateDate"         
#         "country": 
#             "isoCode"     "name"        "displayName" "isoThree"    "numberCode"  "createDate" 


#lfs <- lapply(df$locations, data.frame, stringsasFactors = FALSE)
#lf <- bind_rows(lfs)

#we will join location later

  
styleAttributes <- c("categoryId")
breweryAttributes <- c("id", "name", "description")
locationAttributes <- c("id", "name", "locality", "region", "postalCode", "latitude", "longitude",
                        "locationType", "locationTypeDisplay", "countryIsoCode")
beers <- read_rds(beersFile)
beers <- NULL
# takes about an hour to fill

# have to see if we run into any NAs in style
# then we have to separate rows by breweries and add location ids to them

# have to change possible NULLs to NAs

# in order to make more dynamic, could keep ttrack of how many observations we've seen and 
# do the math to see if we should update (not a priority)

beersRequestData <- BreweryDB_endpoint(breweryDBKey, "beers", options = list(p = as.character(1))) %>%
  content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
beerNumPages <- beersRequestData$numberOfPages

for (i in 1:beerNumPages) {
  print(i)
  beersRequestData <- BreweryDB_endpoint(breweryDBKey, "beers", options = list(p = as.character(i), withBreweries = "Y")) %>%
    content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
  unfilteredBeerData <- beersRequestData$data

  unfilteredBeerData$categoryId <- unfilteredBeerData$style$categoryId
  
  unfilteredBeerData$breweryId <- lapply(unfilteredBeerData$breweries, FUN = function(x) { paste(x$id, collapse = " ") })
  

  headerstoAdd <- setdiff(c("id", "name", "description", "abv", "ibu","styleId"), 
                          names(unfilteredBeerData))
  if (!is_empty(headerstoAdd)) {
    for (colName in headerstoAdd) {
      unfilteredBeerData[colName] <- NA
    }
  }
  if (is.null(beers)) {
    beers <- unfilteredBeerData %>%
      select(id, name, description, abv, ibu, styleId, categoryId, breweryId) %>%
      as_tibble()
  } else {
    beers <- rbind(beers, unfilteredBeerData %>% 
                     select(id, name, description, abv, ibu, styleId, categoryId, breweryId) %>%
                     as_tibble())
  }
}
rm(beersRequestData, unfilteredBeerData, headerstoAdd)
beers <- write_rds(beers, beersFile)
beers <- read_rds(beersFile)
write.csv(beers, beersFile)
write_rds(beers, beersFile)


########## GETTING RATINGS ##########






############### END #################

beerCategoriesRequestData <- BreweryDB_endpoint(breweryDBKey, "categories") %>%
  content(as = "text", encoding = "UTF-8")
beerCategories <- fromJSON(beerCategoriesRequestData, simplifyDataFrame = TRUE)$data %>%
  select(id, name) %>%
  as_tibble()
beerCategories <- head(beerCategories, -1) # we have a null row at the end
rm(beerCategoriesRequestData)

breweries <- read_rds(breweriesFile)

breweries <- NULL

breweriesRequestData <- BreweryDB_endpoint(breweryDBKey, "breweries", options = list(p = as.character(1))) %>%
  content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
breweryNumPages <- breweriesRequestData$numberOfPages
#3:15
# takes about 3 minutes

breweriesRequestData <- BreweryDB_endpoint(breweryDBKey, "breweries", 
                                           options = list(p = as.character(1), withLocations = "Y")) %>%
  content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
unfilteredBreweriesData <- breweriesRequestData$data

unfilteredBreweriesData$locationId <- lapply(unfilteredBreweriesData$locations, FUN = function(x) { paste(x$id, collapse = " ") })


for (i in 1:breweryNumPages) {
  breweriesRequestData <- BreweryDB_endpoint(breweryDBKey, "breweries", 
                                             options = list(p = as.character(i), withLocations = "Y")) %>%
    content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
  unfilteredBreweriesData <- breweriesRequestData$data
  
  unfilteredBreweriesData$locationId <- lapply(unfilteredBreweriesData$locations, FUN = function(x) { paste(x$id, collapse = " ") })
  
  # the headers left to add if there is a column that is missing before we add them
  headerstoAdd <- setdiff(c("id", "name", "description"), 
                          names(unfilteredBreweriesData))
  
  if (!is_empty(headerstoAdd)) {
    for (colName in headerstoAdd) {
      # add missing column if it exists
      unfilteredBreweriesData[colName] <- NA
    }
  }
  if (is.null(breweries)) {
    breweries <- unfilteredBreweriesData %>%
      select(id, name, description, locationId) %>%
      as_tibble()
  } else {
    breweries <- rbind(breweries, unfilteredBreweriesData %>% 
                         select(id, name, description, locationId) %>%
                         as_tibble())
  }
  
}
rm(breweriesRequestData, unfilteredBreweriesData, headerstoAdd)
write_rds(breweries, breweriesFile)


locations <- read_rds(locationsFile)


locations <- NULL

locationsRequestData <- BreweryDB_endpoint(breweryDBKey, "locations", options = list(p = as.character(1))) %>%
  content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
locationNumPages <- locationsRequestData$numberOfPages

# takes about 4 minutes
for (i in 1:locationNumPages) {
  locationsRequestData <- BreweryDB_endpoint(breweryDBKey, "locations", 
                                             options = list(p = as.character(i))) %>%
    content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
  unfilteredLocationsData <- locationsRequestData$data
  
  # the headers left to add if there is a column that is missing before we add them
  headerstoAdd <- setdiff(c("id", "name", "streetAddress", "locality", "region", 
                            "postalCode", "latitude", "longitude", "locationTypeDisplay", 
                            "isPrimary", "countryIsoCode", "breweryId"), 
                          names(unfilteredLocationsData))
  
  if (!is_empty(headerstoAdd)) {
    for (colName in headerstoAdd) {
      # add missing column if it exists
      unfilteredLocationsData[colName] <- NA
    }
  }
  if (is.null(locations)) {
   locations <- unfilteredLocationsData %>%
     select(id, name, streetAddress, locality, region, postalCode, latitude, longitude, 
            locationTypeDisplay, isPrimary, countryIsoCode, breweryId) %>% 
     as_tibble() 
  } else {
    locations <- rbind(locations, unfilteredLocationsData %>% 
                         select(id, name, streetAddress, locality, region, 
                                postalCode, latitude, longitude, locationTypeDisplay, 
                                isPrimary, countryIsoCode, breweryId) %>%
                         as_tibble())
  }
}
rm(locationsRequestData, unfilteredLocationsData, headerstoAdd)
write_rds(locations, locationsFile)

# then begin porting in data from RateBeer about the different kinds
# of beers,

# turns out getting ratings is much more difficult, and would require actual
# HTML scraping which I don't want to get into now, so going to focus on 
# the data that I do have from the breweryDB APIs


# DONE: merge the categories and styles again (check for proper code)
# DONE: expand the beers & breweries data frames on breweryId and locationId
# DONE: merge the breweries and location information
# TODO: turn all NULLs and NAs strings to actual NA type
# after that, I believe we're done with the data dictionairy and we can do 
# project 3 deliverable

beerStyles <- arrange(beerStyles, categoryId)

# adds category information to the diferent styles so we can search styles by category
beerCategoriesStyles <- beerStyles %>% inner_join(beerCategories %>% rename(categoryId = id, categoryName = name), 
                                                  by = "categoryId")
#is.na(beers$name) <- beers$name == "NULL"

for (attr in names(beers)) {
  is.na(beers[attr]) <- beers[attr] == "NULL"
}

for (attr in names(breweries)) {
  is.na(breweries[attr]) <- breweries[attr] == "NULL"
}


for (attr in names(locations)) {
  is.na(locations[attr]) <- locations[attr] == "NULL"
}

breweries <- breweries %>% 
  mutate(locationId = strsplit(as.character(locationId), " ")) %>% 
  unnest(locationId)
write_rds(breweries, breweriesFile)


beers$abv <- as.numeric(beers$abv)
beers
beers %>% ggplot(aes(x = abv)) + geom_histogram()
beers %>% filter(abv > 50)

# some serious cleaning is needed, have to dump outliers and values that don't make sense
# we can use initial graphs to show the outliers

beers <- beers %>% 
  mutate(breweryId = strsplit(as.character(breweryId), " ")) %>%
  unnest(breweryId)
write_rds(beers, beersFile)

beersandStyles <- beers %>% inner_join(beerStyles %>% rename(styleId = id, 
                    styleName = name, styleshortName = shortName, stylesDescription = description), 
                     by = c("styleId", "categoryId"))
breweriesandLocations <- breweries %>% rename(breweryId = id, breweryName = name, 
                                        breweryDescription = description) %>% 
  inner_join(locations %>% rename(locationId = id, locationName = name), 
             by = c("locationId", "breweryId"))

beersBreweriesLocations <- beers %>% rename(beerId = id, beerName = name, 
                                beerDescription = description) %>% 
  inner_join(breweriesandLocations, by = "breweryId")
