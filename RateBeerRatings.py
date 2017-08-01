import ratebeer
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
import pprint

# this script will get the ratings for all the beers by searching RateBeer
# for each beer by name, and associate each beer with its average ratings and
# number of reviews


pandas2ri.activate()

readRDS = robjects.r['readRDS']
beerRDS = readRDS('data/beers.rds')
beers = pandas2ri.ri2py(df)

pprint(beers)

rb = RateBeer()
