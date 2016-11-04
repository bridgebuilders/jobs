library(readr)


allLanguageListing <- read_csv(file = 'data-raw/joshua_project/allLanguageListing.csv',trim_ws = TRUE)
allPeoplesByCountry <- read_csv(file = 'data-raw/joshua_project/allPeoplesByCountry.csv',trim_ws = TRUE)
peopleCtryLangListing <- read_csv(file = 'data-raw/joshua_project/peopleCtryLangListing.csv',trim_ws = TRUE)
unreachedPeoplesByCountry <- read_csv(file = 'data-raw/joshua_project/unreachedPeoplesByCountry.csv',trim_ws = TRUE)

morocco <- unreachedPeoplesByCountry 

# 
# jpabsum
# jpcontinentsum
# jpcountries
# jplangpeopctry
# jplanguages
# jpministries
# jppeopleclusters
# jppeoples
# jpregionsum
# jpreligionsum
# jpresources
# jpscalesum
# jpsouthasia
# jpsouthasiasum
# jptotals
# jpupgotd
