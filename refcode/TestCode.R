source("R/AddCensusDemographics.R")
load("data/ArlingtonMA_VoterPrecinct.RData")
#set up directories we'll need
dir.create("data/test")

#set api_key
#api_key <-"YOUR API KEY"

#set parameters
state <- 25
county <- 17
demographicvars <- c(Pop="P0010001",
                     MajorityPop="P0050003") #Total Population and not Hispanic or Latino, White alone
api_key <- NULL
year <- 2010

#add census demographics
#MapWithDemographics <- AddCensusDemographics(map,api_key,state,county)
MapWithDemographics <- AddCensusDemographics(map = ArlingtonMA_VoterPrecinct,state = state,county = county,year = year,demographicvars = demographicvars)

#output map
st_write(MapWithDemographics,dsn="data/test/",layer="MapWithDemographics",driver = "ESRI Shapefile")

#clean up directory
# unlink("data/cache",recursive = TRUE)
unlink("data/test",recursive = TRUE)
