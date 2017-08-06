from ratebeer import RateBeer # this can be insalled with "pip install ratebeer"
# is necessary to run this
import pandas as pd
import numpy as np
from pprint import pprint
import unicodedata

# we need all of these python packages as well

# this script will get the ratings for all the beers by searching RateBeer
# for each beer by name, and associate each beer with its average ratings and
# number of reviews

def _dirtystrip(line):
    """
        Returns a RateBeer appropriate search string. Normalizes to get accents as
        individual characters and then strips them. ord < 256 allows non-ASCI characters to remain.

        Also strip / and !. RateBeer can't handle them. Man, their search seriously sucks.
    """
    line = unicodedata.normalize('NFKD',line)
    line = line.replace('/',' ')
    line = line.replace('!',' ')
    return ''.join([x for x in line if ord(x) < 256])

beersFile = "data/beers.csv"

beersdf = pd.read_csv(beersFile)

pprint(beersdf.columns.tolist())

rb = RateBeer()

testBeer = rb.get_beer("/beer/albrecht-18o-imperial-ipa/402585/")

pprint(testBeer.overall_rating)
pprint(testBeer.mean_rating)

pprint(testBeer.name)
exit(1)

# pprint("summit: " + _dirtystrip(u"Summit"))
#
# pprint(rb.search(_dirtystrip('Summit')))

try:
    for name in beersdf["name"]:
        name = _dirtystrip(name)
        pprint(name)
        rateBeerPossBeers = rb.search(name)
        pprint(rateBeerPossBeers)
        if rateBeerPossBeers["beers"]:
            pprint(rateBeerPossBeers)
            break
except:
    pass