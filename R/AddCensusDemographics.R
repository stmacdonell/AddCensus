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

#'Add Census demographics data to your simple features object
#'
#'@param map A simple features map covering a part of one (and only one) state.
#'@param year The Census year from which you want data.
#'@param state The state that contains map. Accepts FIPS code and two letter abreviation.
#'@param county The FIPS code of the county/counties containing map. This defaults to NULL, but the function runs more quickly the fewer counties downloaded.
#'@param demographicvars The demographic variables you want to add. If NULL (the default) it downloads the total population which is "P0010001" for 2010 and "P001001" for 1990 and 2000. Codes can be looked up at https://api.census.gov/data.html by clicking on "variables" next to the Census year you want.
#'@param api_key Your census API key. Works with NULL (default), but you are subject to limits without a key.
#'@return A new simple features object with U.S. Census demographic data added.
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
    if(year==2010){
      demographicvars <- c(TotalPop="P0010001",
                           WhitePop="P0030002")
    }else{
      if(year==2000){
        demographicvars <- c(TotalPop="P001001",
                             WhitePop="P003003")
      }else{
        if(year==1990){
          demographicvars <- c(TotalPop="P001001",
                               WhitePop="P0060001")
        }else{
          stop("Invalid year")
        }
      }
    }
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
