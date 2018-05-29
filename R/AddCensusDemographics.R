# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#library(lwgeom)

#This takes a shapefile, state (abbrevation, or FIPS code), and if desired a vector of county FIPS codes and
#returns the same shapefile with Census Demographics attached
AddCensusDemographics <- function(map,year=2010,state,county=NULL,
                                  demographicvars=NULL,api_key=NULL){

  library(sf)
  library(tidycensus)

  #make sure the state is valid
  if(!((state%in%StateAndCountyFIPScodes2010$StateAbr)|(state%in%StateAndCountyFIPScodes2010$StateFIPS))){
    stop("Invalid State")
  }

  #convert abbrevation to FIPS
  if(state%in%StateAndCountyFIPScodes2010$StateAbr){
    states <- unique(StateAndCountyFIPScodes2010[,1:2])
    state <- states$StateFIPS[states$StateAbr==state]
    rm(states)
  }

  #if county is null, get all counties for the state
  if(is.null(county)){
    county <- unique(StateAndCountyFIPScodes2010$CountyFIPS[StateAndCountyFIPScodes2010$StateFIPS==state])
  }else{
    if(!(min(county%in%StateAndCountyFIPScodes2010$CountyFIPS[StateAndCountyFIPScodes2010$StateFIPS==state]))){
      stop("Invalid County")
    }
  }


  #Set demographic variables if null
  if(is.null(demographicvars)){
  demographicvars <- c(Pop18 = "P0100001",
                       Pop18WH = "P0100003",
                       Pop18BL = "P0100004",
                       Pop18AI = "P0100005",
                       Pop18AS = "P0100006",
                       Pop18HPI = "P0100007",
                       Pop18HSP = "P0110002"
                       # ,
                       # InstitPopWhite = "PCT020A003",
                       # InstitPopBlack = "PCT020B003",
                       # InstitPopAmInd = "PCT020C003",
                       # InstitPopAsian = "PCT020D003",
                       # InstitPopNativeHawPI = "PCT020E003",
                       # InstitPopHisp = "PCT020H003"
                       )
  }

  ##Need to loop through all counties
  blkmap <- get_decennial(geography = "block", variables = demographicvars,
                          year=year, state = state, county = county[1], geometry = TRUE,
                          summary_var = NULL, output = "wide",key=api_key)

  if(length(county)>1){
    for(i1 in 2:length(county)){
      countyblkmap <- get_decennial(geography = "block", variables = demographicvars,
                                    year=year, state = state, county = county[i1],
                                    geometry = TRUE, summary_var = NULL,
                                    output = "wide",key=api_key)
      blkmap <- rbind(blkmap,countyblkmap)
    }
    rm(countyblkmap,i1)
  }


  # blkmap <- get_decennial(geography = "block", variables = demographicvars,
  #                         state = state, geometry = TRUE, output = "wide")



  #get a copy of only the spatial data
  blkmapFull <- blkmap
  blkmap <- st_geometry(blkmap)
  mapFull <- map
  map <- st_geometry(map)

  #put shapefiles in same projection
  map <- st_transform(map,st_crs(blkmap))

  #which blocks intersect each precinct
  #gIntersection and raster::intersection take way to long on everything
  #it is much faster to just figure out what intersects at all, and then only intersect those
  mapi <- suppressMessages(st_intersects(map,blkmap,sparse = TRUE, prepared = TRUE))

  intersected.blocks <- sort(unique(unlist(mapi)))

  blkareas <- double(length(blkmap))
  blkareas[] <- NA

  #what is the area of each intersected block
  blkareas[intersected.blocks] <- sapply(intersected.blocks,function(x){
    st_area(blkmap[x])
  })

  #how much of each intersected block is within each precinct
  percent.of.block.intersecting <- sapply(1:length(mapi),function(x){

    percent.of.area.interescting <- sapply(mapi[[x]],function(y){
      intersection <- suppressMessages(st_intersection(map[x],blkmap[y]))
      #what is the area of the intersection
      if(is.null(intersection)){
        return(0)
      }else{
        return(st_area(intersection)/blkareas[y])
      }
    })
  })

  #get maps with the data again and remove backups
  map <- mapFull
  blkmap <- blkmapFull
  rm(mapFull)
  rm(blkmapFull)

  blkmapdata <- dplyr::select(as.data.frame(blkmap), -geometry, -GEOID, -NAME)
  blkmapdata <- as.matrix(blkmapdata)

  #for each polygon add the portion (%of block area included in the precinct) of the population
  #from each block to the precinct
  temp <- apply(as.matrix(1:length(mapi)), MARGIN=1,  FUN=function(x){
    percent.of.block.intersecting[[x]] %*% blkmapdata[mapi[[x]],]
  })

  #transpose to match
  temp <- t(temp)
  colnames(temp) <- colnames(blkmapdata)

  return(cbind(map,temp))
}
