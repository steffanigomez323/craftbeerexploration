suppressPackageStartupMessages(library(jsonlite)) # for working with JSON data
suppressPackageStartupMessages(library(tidyverse)) # to transform and clean data
suppressPackageStartupMessages(library(plotly)) # used for interactive graphs
suppressPackageStartupMessages(library(tidyr)) # for help with turning JSONs into tidy frames
suppressPackageStartupMessages(library(outliers)) # for computing outliers

source("BreweryDBDataRetriever.R")

# my BreweryDB API key
breweryDBKey <- "11b192faea1a549172fe2423db077bc5"

#the file in which the beers R data frame is stored
beersFile <- "data/beers.rds"

#the file in which the breweries R data frame is stored
breweriesFile <- "data/breweries.rds"

#the file in which the locations R data frame is stored
locationsFile <- "data/locations.rds"

updateData <- FALSE

# getting beer style data since it is instantaneous
beerStylesRequestData <- BreweryDB_endpoint(breweryDBKey, "styles") %>%
  content(as = "text", encoding = "UTF-8")
beerStyles <- fromJSON(beerStylesRequestData, simplifyDataFrame = TRUE)$data %>%
  select(id, categoryId, name, shortName, description, ibuMin, ibuMax, abvMin, 
         abvMax, srmMin, srmMax, ogMin, ogMax, fgMin, fgMax) %>%
  as_tibble()
rm(beerStylesRequestData)

# getting beer category data since it is instantaneous
beerCategoriesRequestData <- BreweryDB_endpoint(breweryDBKey, "categories") %>%
  content(as = "text", encoding = "UTF-8")
beerCategories <- fromJSON(beerCategoriesRequestData, simplifyDataFrame = TRUE)$data %>%
  select(id, name) %>%
  as_tibble()
beerCategories <- head(beerCategories, -1) # we have a null row at the end
rm(beerCategoriesRequestData)

if (updateData) {
  retrieveBeerData(breweryDBKey, beersFile)
  retriveBreweryData(breweryDBKey, breweriesFile)
  retrieveLocationsData(breweryDBKey, locationsFile)
}

############################### DATA CLEANING ##################################
beers <- read_rds(beersFile)
breweries <- read_rds(breweriesFile)
locations <- read_rds(locationsFile)

for (attr in names(beers)) {
  beers[ attr == "NULL" ] <- NA
}

for (attr in names(breweries)) {
  breweries[ attr == "NULL" ] <- NA
}

for (attr in names(locations)) {
  locations[ attr == "NULL"] <- NA
}

beers$abv <- as.numeric(beers$abv)
beers$ibu <- as.numeric(beers$ibu)

beerStyles$ibuMin <- as.numeric(beerStyles$ibuMin)
beerStyles$ibuMax <- as.numeric(beerStyles$ibuMax)
beerStyles$abvMin <- as.numeric(beerStyles$abvMin)
beerStyles$abvMax <- as.numeric(beerStyles$abvMax)
beerStyles$srmMin <- as.numeric(beerStyles$srmMin)
beerStyles$srmMax <- as.numeric(beerStyles$srmMax)
beerStyles$ogMin <- as.numeric(beerStyles$ogMin)
beerStyles$ogMax <- as.numeric(beerStyles$ogMax)
beerStyles$fgMin <- as.numeric(beerStyles$fgMin)
beerStyles$fgMax <- as.numeric(beerStyles$fgMax)


# The first thing that we have to do is clean our data, now that we have all of our data. 
# This is done below.

# now that we have all of our data, we might want to take a look at the distribution of 
# the most distinguishable beer characteristics, abv (alcohol per beer volume, expressed 
# as a percentagage out of 100) and ibu (international bitterness unit value, which is a 
# measure of how bitter the beer is)

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

summary(beers$ibu)
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

# there are 103 states in the United States according to the dataset, which is 
# erroneous. We expect to see 52 "different" states, the 50 states, the District 
# Capital and thenNA for observations without a state 

locations %>% filter(countryIsoCode == "US" & is.na(region)) %>% nrow

# there are 16 observations without a state

locations %>% filter(countryIsoCode == "US" & is.na(region))

changeLocationState <- function(locationdataFrame, locationId, regionValue) {
  print(locationdataFrame)
  for (i in 1:nrow(locationdataFrame[,1])) {
    if (locationdataFrame$id[i] == locationId) {
      locationdataFrame$region[i] <- regionValue
      print(nrow(locationdataFrame))
      return(locationdataFrame)
    }
  }
}

#nrow(locations)

#locationswithNAStates <- locations %>% filter(countryIsoCode == "US" & is.na(region)) %>% 
#                            select(id) %>% as.list()

# using google to find the states since it's only 16

#NAlocationsIds <- c("2sfE3h", "6fAJtO", "0QiT0E", "GTwDJI", "5yDXi6", "4nSc9t", 
#                    "LMfC4N", "hbF6tu", "GM3x67", "PHV3BL", "YKKcj2", "1KlIcf")

#NAstates <- c("Texas", "California", "California", "California", "California", 
#              "New York", "New York", "New Jersey", "North Carolina", 
#              "DC", "DC")

locations <- changeLocationState(locations, "2sfE3h", "Texas")
locations <- changeLocationState(locations, "6fAJtO", "California")
locations <- changeLocationState(locations, "0QiT0E", "California")
locations <- changeLocationState(locations, "GTwDJI", "California")
locations <- changeLocationState(locations, "5yDXi6", "California")
locations <- changeLocationState(locations, "4nSc9t", "New York")
locations <- changeLocationState(locations, "LMfC4N", "New York")
locations <- changeLocationState(locations, "hbF6tu", "New Jersey")
locations <- changeLocationState(locations, "GM3x67", "North Carolina")
locations <- changeLocationState(locations, "PHV3BL", "DC")
locations <- changeLocationState(locations, "YKKcj2", "DC")
locations <- changeLocationState(locations, "1KlIcf", "California")

#for (i in 1:16) {
#print(locations)
#  changeLocationState(locations, locationswithNAStates[i], NAstates[i])
#}

# remember that washington, dc is not a state
# difficult to see whether addresses are valid or what postal codes are valid, 
# location data cleaning will be a lot more obvious once it is visualized

# have to turn state abbreviations into full state names

# the ranges of the latitude and longitude seem alright, import dates so we can look at things by year? 


# we can verify country isocodes, locationTypeDisplay, and check the ranges of latitude and longitude

summary(locations$latitude)
summary(locations$longitude)


# all seem good, going to take a look at country isocodes

countryCodes <- locations %>% count(countryIsoCode) %>% arrange(desc(n))

# all the countryIsoCodes seem alright, should compare to official country codes


# this shows us all the state issues we have to account for

# check for duplicates in all the data dictionaries??????

# FUNCTIONAL PROGRAMMING

################################# END ##########################################

########################### DATA DICTIONARY ###################################

beerStyles <- arrange(beerStyles, categoryId)

# adds category information to the diferent styles so we can search styles by category
beerCategoriesStyles <- beerStyles %>% inner_join(beerCategories %>% rename(categoryId = id, categoryName = name), 
                                                  by = "categoryId")

breweries <- breweries %>% 
  mutate(locationId = strsplit(as.character(locationId), " ")) %>% 
  unnest(locationId)

beers <- beers %>% 
  mutate(breweryId = strsplit(as.character(breweryId), " ")) %>%
  unnest(breweryId)

# associates beers with their style information 
beersandStyles <- beers %>% inner_join(beerStyles %>% rename(styleId = id, 
                                                             styleName = name, 
                                                             styleshortName = shortName, 
                                                             stylesDescription = description), 
                                       by = c("styleId", "categoryId"))

# associates breweries with their location(s)
breweriesandLocations <- breweries %>% rename(breweryId = id, breweryName = name, 
                                              breweryDescription = description) %>% 
  inner_join(locations %>% rename(locationId = id, locationName = name), 
             by = c("locationId", "breweryId"))

# associates beers with the breweries that brew them and the locations of those
# brewreries
beersBreweriesLocations <- beers %>% rename(beerId = id, beerName = name, 
                                            beerDescription = description) %>% 
  inner_join(breweriesandLocations, by = "breweryId")

#write_rds(beers, "data/beersClean.rds")
#write_rds(breweries, "data/breweriesClean.rds")
#write_rds(locations, "data/locationsClean.rds")
#write_rds(breweriesandLocations, "data/breweriesLocations.rds")
#write_rds(beersBreweriesLocations, "data/maindictionary.rds")
#write_rds(beerCategoriesStyles, "data/categoriesStyles.rds")
################################ END ###########################################

#beers <- read_rds("data/beersClean.rds")
#breweries <- read_rds("data/breweriesClean.rds")
#locations <- read_rds("data/locationsClean.rds")
breweriesLocations <- read_rds("data/breweriesLocations.rds")
beersBreweriesLocations <- read_rds("data/maindictionary.rds")
beerCategoriesStyles <- read_rds("data/categoriesStyles.rds")

################################ EDA ###########################################

# if we wanted to see the distribution of location types among the dataset
beersBreweriesLocations %>% ggplot(aes(x = locationTypeDisplay)) + geom_bar() + 
  ggtitle("The Frequency of the Different Kinds of Breweries") +
  labs(x = "Location Type", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# it seems that the majority of beer abv is between 0 and 20, so let's visualize that
beers %>% ggplot(aes(abv)) + geom_histogram(binwidth = 1) + xlim(0, 20) + 
  ggtitle("The Distribution of Beers's Alcohol per Beer Volume Under 20%") +
  labs(x = "Alcohol per Beer Volume (%)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# definitely right skewed

#   TODO: show style information in a great informational visualization to show style and                category

abvranges <- beerCategoriesStyles %>%
  ggplot(aes(x = id, y = (abvMin + abvMax) / 2)) + 
  geom_linerange(aes(ymin=abvMin, ymax=abvMax, group = name, color = categoryName), na.rm = TRUE) + 
  labs(color = "Category Name", x = "Beer Style Ids", y = "ABV Ranges") + 
  ylim(0, 27) + 
  ggtitle("ABV Ranges by Beer Style and Category") +
  theme(plot.title = element_text(hjust = 0.5))

abvranges + coord_flip()

ggplotly(abvranges + coord_flip(), tooltip = c("group"))

beerCategoriesStyles %>% 
  ggplot(aes(x = id, y = ibuMin)) + 
  geom_linerange(aes(ymin=ibuMin, ymax=ibuMax, color = categoryName), na.rm = TRUE) + 
  labs(color = "Category Name", x = "Beer Style Ids", y = "IBU Ranges") +
  ggtitle("IBU Ranges by Beer Style and Category") +
  theme(plot.title = element_text(hjust = 0.5))

# srm colors reasonably won't be more than 50, so excluding styles with a max 
# of more than 50

beerCategoriesStyles %>% filter(srmMax <= 50) %>%
  ggplot(aes(x = id, y = srmMin)) + 
  geom_linerange(aes(ymin=srmMin, ymax=srmMax, color = categoryName), na.rm = TRUE) + 
  labs(color = "Category Name", x = "Beer Style Ids", y = "SRM Color Ranges") +
  ggtitle("SRM Color Ranges by Beer Style and Category") +
  theme(plot.title = element_text(hjust = 0.5))


#   TODO: show abv and ibu of beers per country
#    going to have to do some javascript or something in a shiny app to do this

abvUS <- beersBreweriesLocations %>% filter(abv <= 20 & countryIsoCode == "US") %>%
  ggplot(aes(x = abv)) + geom_histogram(na.rm = TRUE, binwidth = 1) + 
  ggtitle("Alcohol Per Beer Volume (%) Content Below 20") + 
  labs(x = "Alcohol Per Beer Volume (%)", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(abvUS)

ibuUS <- beersBreweriesLocations %>% filter(ibu <= 120 & countryIsoCode == "US") %>%
  ggplot(aes(x = ibu)) + geom_histogram(na.rm = TRUE, binwidth = 5) + 
  ggtitle("International Bitterness Unit (ppm) Up to 120 IBUs") + 
  labs(x = "International Bitterness Unit (ppm)", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(ibuUS)

#   TODO: do summary statistics of abv and ibu per country
#       - show summary statistics, standard deviations, range, shape, and outliers

beersperCountry <- beersBreweriesLocations %>% filter(countryIsoCode == "US")

z_scores <- beersperCountry %>% filter(!is.na(abv)) %>% 
  select(abv) %>%
  scores(type = "z")

as.data.frame(unclass(summary(beersperCountry$abv))) %>% 
  rename(abv = "unclass(summary(beersperCountry$abv))") %>% t %>% as.data.frame %>%
  mutate(StdDev = sd(beersperCountry$abv, na.rm = TRUE)) %>% 
  mutate(Skewness = skewness(beersperCountry$abv, na.rm = TRUE)) %>% 
  mutate(Kurtosis = kurtosis(beersperCountry$abv, na.rm = TRUE)) %>% 
  mutate(OutlierNum = which(abs(z_scores) > 2) %>% length) %>%
  mutate(ObservationNum = beersperCountry %>% nrow)

# with outliers being those that are 2 standard deviations away from the mean
# what does this mean in the sense of the 

#   TODO: add another layers for region and locality if possible
#   TODO: since it is difficult to show most x for continuous variables, show 
#         the top 5 styles per country and state

# finding the top 5 styles in the US
beersBreweriesLocations %>% filter(countryIsoCode == "US") %>% count(styleId) %>% 
  arrange(desc(n)) %>% rename(frequency = n, id = styleId) %>% head(5) %>% 
  inner_join(beerCategoriesStyles, by = c("id")) %>%
  select(name, frequency)

# finding the top 5 styles in each state
beersBreweriesLocations %>% filter(countryIsoCode == "US") %>% 
  count(styleId, region) %>% 
  arrange(region, desc(n)) %>% 
  plyr::ddply("region", function(x) head(x[order(x$n, decreasing = TRUE) , ], 5)) %>%
  rename(frequency = n, id = styleId) %>%
  inner_join(beerCategoriesStyles, by = c("id")) %>% select(region, name, frequency)

#   TODO: show ranges of abv per country and per state
beersBreweriesLocations %>% filter(!is.na(abv)) %>% group_by(countryIsoCode) %>% 
  summarise(minABV = min(abv), maxABV = max(abv))

beersBreweriesLocations %>% filter(!is.na(abv) & countryIsoCode == "US") %>% 
  group_by(region) %>% summarise(minABV = min(abv), maxABV = max(abv)) %>% 
  arrange(desc(maxABV))

#   TODO: show the top srmIDs per country, state, and locality/postalCode
#             (since srm ids are correlated with style)

beersBreweriesLocations %>% filter(!is.na(srmId)) %>% ggplot(aes(srmId)) + 
  geom_histogram(color = "BLACK", binwidth = 1) +
  labs(x = "SRM ID", y = "Frequency") + 
  ggtitle("The Distribution of the SRM Color Scale") + 
  theme(plot.title = element_text(hjust = 0.5))

beersBreweriesLocations %>% filter(!is.na(srmId)) %>% count(srmId) %>% 
                                     arrange(desc(n)) %>% rename(frequency = n)

# by country
beersBreweriesLocations %>% filter(!is.na(srmId)) %>% count(srmId, countryIsoCode) %>%
  arrange(countryIsoCode, desc(n)) %>% plyr::ddply("countryIsoCode", 
                        function(x) head(x[order(x$n, decreasing = TRUE) , ], 3))

# by state
beersBreweriesLocations %>% filter(!is.na(srmId) & countryIsoCode == "US") %>% 
  count(srmId, region) %>% arrange(region, desc(n)) %>% 
  plyr::ddply("region", function(x) head(x[order(x$n, decreasing = TRUE) , ], 3))

#   TODO: can show srmIDs with style and see what that says about beer styles &                          colors

  # average srmId, abv, and ibu by beer style

beersBreweriesLocations %>% group_by(styleId) %>% 
  summarise(avgSRM = mean(srmId, na.rm = TRUE),
            minSRM = min(srmId, na.rm = TRUE),
            maxSRM = max(srmId, na.rm = TRUE),
            avgABV = mean(abv, na.rm = TRUE),
            minABV = min(abv, na.rm = TRUE),
            maxABV = max(abv, na.rm = TRUE),
            avgIBU = mean(ibu, na.rm = TRUE),
            minIBU = min(ibu, na.rm = TRUE),
            maxIBU = max(ibu, na.rm = TRUE)) %>% 
  rename(id = styleId) %>%
  inner_join(beerCategoriesStyles, by = c("id")) %>% 
  select(name, avgSRM, minSRM, maxSRM, avgABV, minABV, maxABV, avgIBU, minIBU, maxIBU)

#   TODO: show the most countries, states, and localities with the most breweries

# top 5 states with the most breweries in the US
beersBreweriesLocations %>% filter(countryIsoCode == "US") %>% count(region) %>% 
  arrange(desc(n)) %>% rename(frequency = n) %>% head(5)

# top 5 countries with the most breweries
beersBreweriesLocations %>% count(countryIsoCode) %>% arrange(desc(n)) %>% 
  rename(frequency = n) %>% head(5)

# finding the top 10 cities with the most breweries (all happen to be in the US)
beersBreweriesLocations %>% filter(!is.na(locality)) %>% count(locality) %>% 
  arrange(desc(n)) %>% rename(frequency = n) %>% head(10)

#   TODO: do all of this grouping by breweryType

# the top kinds of locations in the BreweryDB API dataset, which includes all 
# of the kinds of locations
locations %>% count(locationTypeDisplay) %>% arrange(desc(n))

# the top kinds of locations by country
locations %>% filter(countryIsoCode == "US") %>% count(locationTypeDisplay) %>% 
  arrange(desc(n))

# the 3 top kinds of locations by states in the US
locations %>% filter(countryIsoCode == "US") %>% count(locationTypeDisplay, region) %>% arrange(region, desc(n)) %>% plyr::ddply("region", function(x) head(x[order(x$n, decreasing = TRUE) , ], 3))

#   TODO: top countries with the most beers
beersBreweriesLocations %>% count(countryIsoCode) %>% arrange(desc(n))

#   TODO: top states in the US with the most beers
beersBreweriesLocations %>% filter(countryIsoCode == "US") %>% count(region) %>%
  arrange(desc(n))

#   TODO: top cities with the most beers in each state in the US
beersBreweriesLocations %>% filter(countryIsoCode == "US") %>% count(locality, region) %>%
  arrange(region, desc(n)) %>% plyr::ddply("region", function(x) head(x[order(x$n, decreasing = TRUE) , ], 3))

#   TODO: compare all of this information


# getting statistics per country & state would also be great

# TURNING THESE TABLES INTO VISUALIZATIONS WOULD BE GREAT, SOME COOL VISUALIZATIONS
# COULD COME OUT OF ALL OF THESE TABLES

##################################### END ######################################