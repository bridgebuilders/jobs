
#global.r

peopleGroupsDT <- read_feather(path = 'peopleGroupsDT.feather',columns = c('Continent','RegionName','Ctry','Longitude','Latitude','LocationInCountry','PeopleID3','JPScaleDefs','Population','PeopNameAcrossCountries','PeopleCluster','PrimaryLanguageName','CtryLiteracyRate','PrimaryReligion','PercentAdherents','PercentEvangelical'))
allChoices <- peopleGroupsDT %>% select(one_of('Continent', 'RegionName', 'Ctry')) %>% unique()
zoom_range <- read.csv('data-raw/zoom_levels.csv')