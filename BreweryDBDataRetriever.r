library(jsonlite) # for working with JSON data
library(tidyjson) # also for working with JSON data
# ran into bugs in the package so had to ditch it and do it by hand
library(tidyverse) # to transform and clean data
library(tidyr) # for help with turning JSONs into tidy frames
#library(moments) # for calculating skewness

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

beersRequestData <- BreweryDB_endpoint(breweryDBKey, "beers", options = list(p = as.character(1), withBreweries = "Y")) %>%
  content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
beerNumPages <- beersRequestData$numberOfPages

for (i in 1:beerNumPages) {
  print(i)
  beersRequestData <- BreweryDB_endpoint(breweryDBKey, "beers", options = list(p = as.character(i), withBreweries = "Y")) %>%
    content(as = "text", encoding = "UTF-8") %>% fromJSON(simplifyDataFrame = TRUE)
  unfilteredBeerData <- beersRequestData$data

  unfilteredBeerData$categoryId <- unfilteredBeerData$style$categoryId
  
  unfilteredBeerData$breweryId <- lapply(unfilteredBeerData$breweries, FUN = function(x) { paste(x$id, collapse = " ") })

  headerstoAdd <- setdiff(c("id", "name", "description", "abv", "ibu","styleId", "srmId"), 
                          names(unfilteredBeerData))
  if (!is_empty(headerstoAdd)) {
    for (colName in headerstoAdd) {
      unfilteredBeerData[colName] <- NA
    }
  }
  if (is.null(beers)) {
    beers <- unfilteredBeerData %>%
      select(id, name, description, abv, ibu, styleId, categoryId, breweryId, srmId) %>%
      as_tibble()
  } else {
    beers <- rbind(beers, unfilteredBeerData %>% 
                     select(id, name, description, abv, ibu, styleId, categoryId, breweryId, srmId) %>%
                     as_tibble())
  }
}
rm(beersRequestData, unfilteredBeerData, headerstoAdd)
write_rds(beers, beersFile)
beers <- read_rds(beersFile)


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
                            "isPrimary", "countryIsoCode", "inPlanning", "isClosed", "breweryId"), 
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

beers$abv <- as.numeric(beers$abv)
beers$ibu <- as.numeric(beers$ibu)

breweries <- breweries %>% 
  mutate(locationId = strsplit(as.character(locationId), " ")) %>% 
  unnest(locationId)
write_rds(breweries, breweriesFile)


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

################# DATA CLEANING #########################

beers <- read_rds(beersFile)

# SERIOUS CLEANING AND FILTERING IS NEEDED

# The first thing that we have to do is clean our data, now that we have all of our data. This is done below.

# now that we have all of our data, we might want to take a look at the distribution of the most distinguishable
# beer characteristics, abv (alcohol per beer volume, expressed as a percentagage out of 100) and ibu (international
# bitterness unit value, which is a measure of how bitter the beer is)

summary(beers$abv)

# summary statistics of the abv distribution out of all the beers reveal that the maximum abv is 308. Since a percent
# is out of 100, everything above 100 doesn't make sense and we can remove all the beers whose abv is above 100
# since the credibility of that beer is now questionable

beers %>% filter(abv > 100) %>% nrow()
beers %>% filter(abv > 100)

# there is only 1 beer whose abv is above 100, and we dispose of that observation
beers <- beers %>% filter(id != "EHPIi4")

# the distribution could be considered normal, with a skewness of 3.428445

#skewness(beers$abv, na.rm = TRUE)

# let's take a look at ibu

summary(beers$ibu)

beers %>% filter(ibu > 120) %>% select(id, ibu)

# there are a few outliers but after googling to make sure of their authenticity, no observations are removed
# from the beers for wrong ibu range

summary(beers$ibu) # have to look up ibu to see what is the possible range
# parts per million usually no more than 120

beers %>% ggplot(aes(x = ibu)) + geom_histogram(colour = "black", binwidth = 10)

beers %>% filter(ibu > 120) %>% nrow

# only 153 observations above 110, ibu doesn't normally go above 120 according to research, the tongue can distinguish
# up to 110 ibus


# since there isn't anything to clean in breweries; we can't tell what is a good description and what is a 
# bad description, and when the data dictionaries are merged with each other on the foreign keys, beers that 
# don't have a brewery are not included, breweries with no locations are not included, and locations without an 
# associated brewery isn't included, since when we merge, we are using an inner_join. 

# so we move on to locations. Since most of the variables in location are strings, we must check the address and 
# latitudes and longitudes to make sure they make sense in the context of the observation. 


numStates <- locations %>% filter(countryIsoCode == "US") %>% count(region) %>% nrow

# there are 99 states in the United States according to the dataset, which is erroneous. We expect to see 51 
# "different" states, the 50 states and then NA for observations without a state 

locations %>% filter(countryIsoCode == "US" & is.na(region)) %>% nrow

# there are 16 observations without a state

locations %>% filter(countryIsoCode == "US" & is.na(region))

changeLocationState <- function(locationdataFrame, locationId, regionValue) {
  for (i in 1:nrow(locationdataFrame[,1])) {
    if (locationdataFrame$id[i] == locationId) {
      locationdataFrame$region[i] <- regionValue
      return(locationdataFrame)
    }
  }
}


locations <- changeLocationState(locations, "2sfE3h", "Texas")
locations <- changeLocationState(locations, "nLdHCU", "California")
locations <- changeLocationState(locations, "GM3x67", "North Carolina")

# remember that washington, dc is not a state
# difficult to see whether addresses are valid or what postal codes are valid, location data cleaning will be a lot
# more obvious once it is visualized

# have to turn state abbreviations into full state names

# the ranges of the latitude and longitude seem alright, import dates so we can look at things by year? 



topstateswithBreweries <- beersBreweriesLocations %>% filter(countryIsoCode == "US") %>% count(region) %>% 
  arrange(desc(n))


# we can verify country isocodes, locationTypeDisplay, and check the ranges of latitude and longitude

summary(locations$latitude)
summary(locations$longitude)

# looking at the different types of locationTypeDisplay 
locations %>% count(locationTypeDisplay) %>% arrange(desc(n))

# all seem good, going to take a look at country isocodes

countryCodes <- locations %>% count(countryIsoCode) %>% arrange(desc(n))

# all the countryIsoCodes seem alright, should compare to official country codes


# this shows us all the state issues we have to accout for

# we can go through these 16 observations by hand and add them since it's only 16



# check for duplicates in all the data dictionaries??????




# find the top 5 styles and their categories

# abv and ibu

beersBreweriesLocations %>% ggplot(aes(abv, y = ibu)) + geom_point()
beersBreweriesLocations %>% filter(countryIsoCode == "GB") %>%
  ggplot(aes(abv, ibu)) + geom_jitter()

# now we visualize the distribution to get a better picture
summary(beers$abv)
beers %>% ggplot(aes(abv)) + geom_histogram(binwidth=1)

# if we wanted to see the distribution of location types among the dataset
beersBreweriesLocations %>% ggplot(aes(x = locationTypeDisplay)) + geom_bar()
#beersBreweriesLocations %>% ggplot(aes(abv)) + geom_histogram(binwidth = 1) + facet_wrap(~ countryIsoCode)

# it seems that the majority of beer abv is between 0 and 20, so let's visualize that
beers %>% ggplot(aes(abv)) + geom_histogram(binwidth = 1) + xlim(0, 20)

beers %>% filter(ibu <= 120) %>% ggplot(aes(ibu)) + geom_histogram(colour = "black", binwidth = 5)

# definitely right skewed

# displaying the range of abv and ibu for each beer style would be great

# getting statistics per country & state would also be great

# displaying states with the most breweries would be nice

# could also do count of types of breweries to see which are more popular

# we are interested in the spread of beers and breweries across the world, so how are beers and breweries related
# to their locations? what are the top styles in each country? what about state? what are the top 5 postal codes, 
# states, and countries in regards to number of different breweries and then number of different beers? are they 
# different? what does that say about beers and breweries? which breweries have the most beers? which breweries have
# the most locations? are they different lists? why might this be? 

# visualizations of style and category information would be very useful 

# visualizing srmId
summary(beers$srmId)
beers %>% ggplot(aes(srmId)) + geom_histogram(color = "BLACK")



####################### END #############################
