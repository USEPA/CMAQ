## ###########################################################
## Purpose: Pull information about the projection used from a Models3
##          file.  Build a string describing the projection which can
##          be used by the R package rgdal.
##
## Input:
##   file: File name of Models3 file whose projection we want to use.
##
## Returns: String describing model projection, which can utilized
##          by the rgdal package in R (for projections to and from
##          longitude/latitude, for example).
##
## Assumes:
##   1. Your Models3 file uses a Lambert conic conformal or polar
##      stereographic projection.
##   2. Availability of R package ncdf.
##
## 
## Notes:
##   1. The R package rgdal depends on the PROJ.4 cartographic
##       projections library (http://trac.osgeo.org/proj/).  The format
##       of the string must therefore be in a style acceptable to
##       PROJ.4.  (See http://www.remotesensing.org/geotiff/proj_list.)
##   2. See information about the meaning of IOAPI projection
##       arguments at
##       http://www.baronams.com/products/ioapi/GRIDS.html.
##   3. Projection info is stored in global attributes of the Models3 file.
##
## 
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
get.proj.info.Models3 <- function(file){

  ## Require the netCDF packaget to read info from netCDF files (since
  ## IOAPI is a netCDF-based format).
  require("ncdf")
  
  ## Open netCDF file which has the projection we want to use..
  nc <- open.ncdf(con=file)


  ## Based on the source code for the package geospatial (in
  ## geospatial/R/projection.R and geospatial/data/refellpsd.txt) the
  ## default shape of earth is spherical with a radius of 6370997
  ## meters.  (This seems to be true, based on my testing with small
  ## examples, even though the IOAPI header file CONST3.EXT says that
  ## REARTH = 6367333.)  So, I set the sphere's radius here to be
  ## consistent with earlier applications that were doing using the
  ## RMET packages.
  sphere.radius <- 6370997.0

  
  ## ##############################################
  ## FIND OUT THE PROJECTION AND PARAMETERS GOVERNING IT.
  
  ## Find out what projection the grid is on.
  grid.type <- att.get.ncdf(nc, varid=0, attname="GDTYP")$value

  ## Depending on the type of grid, we extract the information we need
  ## to govern that type of projection.  The projection info is found
  ## in the global attributes.  (To get global attributes, rather than
  ## variable attributes, give 0 as the variable ID.)

  ## Lambert conic conformal (if GDTYP==2)
  if (grid.type==2){
    ## Standard parallel 1 is given by P_ALP.
    p.alp <- att.get.ncdf(nc, varid=0, attname="P_ALP")$value
    ## Standard parallel 2 is given by P_BET.
    p.bet <- att.get.ncdf(nc, varid=0, attname="P_BET")$value
    ## Central meridian is given by P_GAM.
    p.gam <- att.get.ncdf(nc, varid=0, attname="P_GAM")$value
    ## Latitude of the center of the Cartesian coordinate system given
    ## by YCENT.
    ycent <- att.get.ncdf(nc, varid=0, attname="YCENT")$value

    ## Form string based on the projection information.
    proj.string <- paste("+proj=lcc +lat_1=", p.alp, " +lat_2=", p.bet,
                         " +lat_0=", ycent,
                         " +lon_0=", p.gam,
                         " +a=", sphere.radius, " +b=", sphere.radius, sep="")
  }


  ## Polar stereographic (if GDTYP==6)
  else if (grid.type==6){

    ## P_ALP identifies pole:  north (1) or south (-1)
    p.alp <- att.get.ncdf(nc, varid=0, attname="P_ALP")$value
    if (p.alp==1.0)
      proj4.lat0 <- 90.0  ##Latitude for North Pole for PROJ.4
    else if (p.alp==-1.0)
      proj4.lat0 <- -90.0  ##Latitude for South Pole for PROJ.4
    else{
      nc <- close.ncdf(nc)
      stop(paste("For polar stereographic projections (GDTYP=6), P_ALP is ",
                 p.alp, "; it should be either 1 or -1.", sep=""))
    }

    ## P_BET identifies the "secant latitude" (latitude of the true scale).
    p.bet <- att.get.ncdf(nc, varid=0, attname="P_BET")$value
    ## Central meridian is given by P_GAM.
    p.gam <- att.get.ncdf(nc, varid=0, attname="P_GAM")$value

    ## Form string based on the projection information.
    proj.string <- paste("+proj=stere +lat_ts=", p.bet,
                         " +lat_0=", proj4.lat0,
                         " +lon_0=", p.gam,
                         " +a=", sphere.radius, " +b=", sphere.radius, sep="")
  }


  ## Function will be exited with a warning if the grid type is not
  ## one of those listed above.
  else{
    ## Close the Models3 file and exit the function.
    nc <- close.ncdf(nc)
    stop(paste("Grid type ", grid.type, " cannot be handled by this function.", sep=""))
  }
  ## ##############################################

  
  ## Close the Models3 file.
  nc <- close.ncdf(nc)
  rm(nc)


  ## Return the string which can be passed to project() and other
  ## functions in R package rgdal.
  return(proj.string)
}




## ###########################################################
## Purpose: Pull information about the grid used from the Models3
## file.  This includes information such as the origin of the grid
## (lower left corner coordinates in grid units).
##
## Input:
##   file: File name of Models3 file whose projection we want to use.
##
## Returns: List containing information about the grid, including the
##   origin point of the grid (lower left coordinates in grid units),
##   projection, grid cell spacing, etc.
##
## Assumes:
##   Availability of function get.proj.info.Models3().
## 
## Notes:
##   Information about grid cell size, extent of grid, etc. is stored
##   in global attributes of the Models3 file.
##
## 
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
get.grid.info.Models3 <- function(file){

  ## Require the netCDF packaget to read info from netCDF files (since
  ## IOAPI is a netCDF-based format).
  require("ncdf")
  
  ## Open netCDF file which has the projection we want to use..
  nc <- open.ncdf(con=file)


  ## Get information about the origin (lower left coordinates in grid
  ## units).
  x.orig <- att.get.ncdf(nc, varid=0, attname="XORIG")$value
  y.orig <- att.get.ncdf(nc, varid=0, attname="YORIG")$value

  ## Get information about the horizontal grid cell size (meters).
  x.cell.width <- att.get.ncdf(nc, varid=0, attname="XCELL")$value
  y.cell.width <- att.get.ncdf(nc, varid=0, attname="YCELL")$value
  if (x.cell.width != y.cell.width)
    warning(paste("Grid cells are not square.  Width in x direction is ", x.cell.width, ", and width in y direction is ", y.cell.width, sep=""))

  ## Number of rows and columns tells us the extent of the grid.
  ncols <- att.get.ncdf(nc, varid=0, attname="NCOLS")$value
  nrows <- att.get.ncdf(nc, varid=0, attname="NROWS")$value
  ## Get number of vertical layers.
  nlays <- att.get.ncdf(nc, varid=0, attname="NLAYS")$value

  ## Now form a list to hold this information about the grid.
  grid.info.list <- list(x.orig=x.orig, y.orig=y.orig, x.cell.width=x.cell.width, y.cell.width=y.cell.width, ncols=ncols, nrows=nrows, nlays=nlays)


  ## Close the Models3 file.
  nc <- close.ncdf(nc)
  rm(nc)

  ## Return the string which can be passed to project() and other
  ## functions in R package rgdal.
  return(grid.info.list)
}




## ###########################################################
## Purpose: Find the locations of the grid cell centers in grid
## units.
##
## Input:
##   file: File name of Models3 file of interest.
##   units.km: TRUE (default value) to get coordinates in model units of km.
##             FALSE to get coordinates in model units of m.
##
## Returns: Matrix with number of rows equal to the number of grid
##   cells and two columns.  The first column contains the
##   x-coordinate of the grid cell centers; the second column contains
##   the y-coordinate of the grid cell centers.  The points are
##   listed in order such that the x-coordinates are changing faster
##   than the y-coordinates.
## 
## 
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
get.matrix.all.grid.cell.ctrs <- function(file, units.km=T){

  ## First, get the cell centers of the columns (providing the
  ## x-coordinates).
  x.coord <- get.coord.for.dimension(file, dimension="column", position="ctr", units.km=units.km)
  ## Then, get the cell centers of the rows (providing the
  ## y-coordinates).
  y.coord <- get.coord.for.dimension(file, dimension="row", position="ctr", units.km=units.km)
  
  ## Return a matrix with all combinations of these x- and y-coordinates.
  all.ctrs <- as.matrix(expand.grid(x.coord, y.coord))
  return(all.ctrs)
}




## ###########################################################
## Purpose: For either the rows or the columns, return the centers
## or the edges of either the rows or columns.
##
## Input:
##   file: File name of Models3 file of interest.
##   dimension: User chooses to get information for either rows
##     ("row") or columns ("column" or "col").
##   position: User chooses whether to get center (default), lower
##     edge, or upper edge coordinates of each row or each column.
##   units.km: TRUE (default value) to get coordinates in model units of km.
##             FALSE to get coordinates in model units of m.
##
## Returns: The user chooses either rows or columns.  If rows, return
##   a vector containing the y-coordinate of the center, left (lower)
##   edge, or right (upper) edge of each row, depending on user choice.
##   If columns, return a vector containing the x-coordinate of the
##   center, lower edge or upper edge of each column, depending on user
##   choice.
##
## Assumes:
##   Availability of function get.grid.info.Models3().
## 
## 
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
get.coord.for.dimension <- function(file, dimension, position="ctr", units.km=T){

  ## Get info about the grid.
  grid.info <- get.grid.info.Models3(file)


  ## Depending on whether we want the center, lower edge, or upper
  ## edge for each cell, we set the offset appropriately.
  if (position=="ctr")
    offset <- 0.5
  else if (position=="lower")
    offset <- 0.0
  else if (position=="upper")
    offset <- 1.0
  else
    stop('Position parameter must be either "lower", "ctr", or "upper".')


  ## Take different actions depending on whether user chooses "row" or
  ## "column" for dimenstions.
  if (dimension=="row")
    coords <- seq(from=grid.info$y.orig+(offset*grid.info$y.cell.width), by=grid.info$y.cell.width, length=grid.info$nrows)
  else if ( (dimension=="column") || (dimension=="col") )
    coords <- seq(from=grid.info$x.orig+(offset*grid.info$x.cell.width), by=grid.info$x.cell.width, length=grid.info$ncols)
  else
    stop('Parameter dimension must be either "row" or "column".')


  ## Return the string which can be passed to project() and other
  ## functions in R package rgdal.
  if (units.km)
    return(coords/1000)
  else
    return(coords)
}




## ###########################################################
## Purpose: Project coordinates from longitude/latitude to model units.
##
## Inputs:
##   longitude: vector of longitudes for the points to be projected
##   latitude: vector of latitudes for the points to be projected
##   file: File name of Models3 file giving the desired model projection.
##   units.km: TRUE (default value) to get coordinates in model units of km.
##             FALSE to get coordinates in model units of m.
##
## Returns: Coordinates in model units of either m or km (depending on
##          value of units.km).
##
## Assumes: 
##   1. Availability of R packages ncdf and rgdal.
##   2. Projection is lambert conic conformal. 
##
##
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
proj.lonlat.to.Models3 <- function(longitude, latitude, file, units.km=T){

  ## The project() function used below is in R package rgdal.
  require("rgdal")

  ## Form projection string describing the projection in the given
  ## Models3 file.
  proj.string <- get.proj.info.Models3(file)

  ## Project locations from longitude and latitude onto CMAQ units (by
  ## default, this is done in meters).
  coords.proj <- project(cbind(longitude, latitude), proj=proj.string)
  colnames(coords.proj) <- c("x", "y")
  
  ## Projection coordinates are in meters by default, but put them in
  ## kilometers if units.km=T.
  if (units.km)
    return(coords.proj/1000)
  else
    return(coords.proj)
}




## ###########################################################
## Purpose: Project coordinates from model units to longitude/latitude.
##
## Inputs:
##   x: x-coordinates of points in model units
##   y: y-coordinates of points in model units
##   file: File name of Models3 file which provided the model projection.
##   units.km: TRUE (default value) if coordinates are in km.
##             FALSE if coordinates are in m
##
## Returns: Coordinates in terms of longitude and latitude.
##
## Assumes:
##   1. Availability of R packages ncdf and rgdal.
##   2. Projection is lambert conic conformal. 
##
## Note: The model projection units are of the sort derived in the
##       function proj.lonlat.to.Models3 in above code.
##
##
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
proj.Models3.to.lonlat <- function(x, y, file, units.km=T){
  
  ## The project() function used below is in R package rgdal.
  require("rgdal")

  ## Form projection string describing the projection in the given
  ## Models3 file.
  proj.string <- get.proj.info.Models3(file)

  ## Project locations from CMAQ units to longitude and latitude.
  if (units.km)
    coords.lonlat <- project(1000*cbind(x, y), proj=proj.string, inv=T)
  else
    coords.lonlat <- project(cbind(x, y), proj=proj.string, inv=T)


  ## Name columns appropriately.
  colnames(coords.lonlat) <- c("longitude", "latitude")

  return(coords.lonlat)
}




## ###########################################################
## Purpose: Project coordinates based on projection in first file to
## the projection given in file2.
##
## Inputs:
##   x: x-coordinates in model units from projection in first file
##   y: y-coordinates in model units from projection in second file
##   file.from.proj: Name of Models3 file with the same model projection
##                   as x-coords and y-coords.
##   file.to.proj: Name of Models3 file with the model projection to which
##                 you want x-coords and y-coords to be transformed.
##   units.km: TRUE (default value) if coordinates are in km.
##             FALSE if coordinates are in m
##
## Returns: Coordinates in model units using projection in file.to.proj.
##
## Assumes:
##   1. Availability of R packagess ncdf and rgdal.
##   2. Projection is lambert conic conformal or polar stereographic
##      projection.
##
##
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
proj.Models3.file1.to.Models3.file2 <- function(x, y, file.from.proj,
                                                file.to.proj,
                                                units.km=T){

  ## The spTransform() function used below is in R package rgdal.
  require("rgdal")

  ## Form projection string describing the projection in the given
  ## Models3 file.
  from.proj.string <- get.proj.info.Models3(file.from.proj)
  to.proj.string <- get.proj.info.Models3(file.to.proj)

  ## Put the given coordinates into "Spatial Points" form for use with
  ## spTransform function.
  if (units.km)
    from.coords <- data.frame(x=1000*x, y=1000*y)
  else
    from.coords <- data.frame(x=x, y=y)
  ## Make into a SpatialPoints object.
  coordinates(from.coords) <- c("x", "y")
  proj4string(from.coords) <- CRS(from.proj.string)
  
  ## Project locations from CMAQ units to longitude and latitude.
  to.coords <- spTransform(from.coords, CRS(to.proj.string))

  ## Return the coordinates, adjusting them if units.km=T.
  if (units.km)
    return(coordinates(to.coords)/1000)
  else
    return(coordinates(to.coords))
}




## ###########################################################
## Purpose: Get map lines in the model projection units.
## 
## Input:
##   file: File name of Models3 file providing the model projection.
##   region: Geogrpaphical database to use.  Choices are "world",
##     "USA", "state", and "county".  Default is "state".
##   units.km: TRUE (default value) to have coordinates in km
##             FALSE to have coordinates in m.
##
## Returns: Map lines in either m or km (depending on value of units.km).
##
## Assumes:
##   Availability of R packages maps, ncdf, and rgdal.
##
##
## Note: The model projection units are of the sort derived in the
##       function proj.lonlat.to.Models3 in above code.
##
##
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
get.map.lines.Models3.proj <- function(file, region="state", units.km=T){

  ## The project() function used below is in R package rgdal.
  require("rgdal")
  ## We need the maps package to get the map lines.
  require("maps")
  
  ## Form projection string describing the projection in the given
  ## Models3 file.
  proj.string <- get.proj.info.Models3(file)

  ## Get the coords of the boundary lines in lat/lon.
  raw.map.lonlat <- map(region, plot=F, resolution=0)
  map.lonlat <- cbind(raw.map.lonlat$x, raw.map.lonlat$y)
  rm(raw.map.lonlat)

  ## We want to re-project these map boundaries onto the projection that
  ## provided by the specified Models3 file.
  map.CMAQ <- project(map.lonlat, proj=proj.string)
  colnames(map.CMAQ) <- c("x", "y")
  rm(map.lonlat)

  ## Projection coordinates are in meters by default, but put them in
  ## kilometers if units.km=T.
  if (units.km)
    return(map.CMAQ/1000)
  else
    return(map.CMAQ)
}



## ###########################################################
## Purpose: Read the date-time steps in the Models3 file.  Put these
## into datetime format.
##
## Input:
##   file: File name of Models3 file of interest.
##
## Returns: List of datetimes included in the Models3 file.
##
## Assumes: This code assumes that the time step is not negative.
## (For instance, the Models3 I/OAPI does allow for negative time
## steps, but these negative time steps will NOT be handled properly
## by this function.)
## 
## 
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
get.datetime.seq <- function(file){

  ## Require the netCDF packaget to read info from netCDF files (since
  ## IOAPI is a netCDF-based format).
  require("ncdf")
  
  ## Open netCDF file which has the projection we want to use..
  nc <- open.ncdf(con=file)


  ## Get information about the time step increment.
  tstep.incr <- att.get.ncdf(nc, varid=0, attname="TSTEP")$value

  ## Test to see whether the file is time-independent (in which case,
  ## the time increment will be 0).
  if (tstep.incr==0){
    ## Close the Models3 file, issue warning, and exit the function.
    nc <- close.ncdf(nc)
    warning("Time step increment is zero.  This appears to be a time-independent file.")
    return(NULL)
  }

  
  ## If the time increment is not zero, then we assume there are time
  ## steps for the variables in the file.

  ## Find the starting date and time.
  Models3.start.date <- att.get.ncdf(nc, varid=0, attname="SDATE")$value
  start.date <- decipher.Models3.date(Models3.start.date)
  Models3.start.time <- att.get.ncdf(nc, varid=0, attname="STIME")$value
  start.time <- decipher.Models3.time(Models3.start.time)

  ## Combine the starting date and time to form datetime object (POSIX
  ## class).
  start.datetime <- combine.date.and.time(date=start.date, time=start.time)

  
  ## Find the increment separating the time steps.
  tstep.incr.list <- decipher.Models3.time(tstep.incr)
  ## To find the increment of the time step in seconds.
  tstep.in.secs <- (tstep.incr.list$hrs*60*60) + (tstep.incr.list$mins*60) + tstep.incr.list$secs

  
  ## How many datetimes are there?  (What is the length of the time
  ## dimension?)
  num.time.steps <- nc$dim$TSTEP$len

  
  ## Now get a sequence.
  datetime.seq <- seq.POSIXt(from=start.datetime, by=tstep.in.secs,
                             length=num.time.steps)

  
  ## Close the Models3 file.
  nc <- close.ncdf(nc)
  rm(nc)


  ## Return the date-time sequence we've developed.
  return(datetime.seq)
}






## ###########################################################
## Purpose: Decipher Models3 time format (HHMMSS) into hours, minutes,
## and seconds.
##
## Input:  Models3 time (numeric) in the format HHMMSS
##
## Returns: List with hrs (hours), mins (minutes), and secs (seconds)
## components.
##
## Assumes: This code assumes that the time is not negative.  (For
## instance, the Models3 I/OAPI does allow for negative time steps,
## but these negative time steps will NOT be handled properly by this
## function.)
##
## Note: The Models3 time is an integer, so we can't just extract the
## first 2 characters, next 2 characters, etc.  If the time step is
## one hour, then the time we extract will be 100, not 000100.
## 
## 
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
decipher.Models3.time <- function(Models3.time){

  ## Find number of hours.
  hrs <- trunc(Models3.time/10000)

  ## Find number of minutes.
  mins <- trunc( (Models3.time - (hrs * 10000)) / 100 )

  ## Find number of seconds
  secs <- Models3.time - (hrs*10000) - (mins * 100)

  return(list(hrs=hrs, mins=mins, secs=secs))
}




## ###########################################################
## Purpose: Decipher Models3 datee format (YYYYDDD) into R's Date
## class.
##
## Input: Models3 date (numeric) in the format YYYYDDD, where DDD is a
## Julian day (since the beginning of YYYY).
##
## Returns: Starting date YYYYDDD in R's Date class.
##
##
## Note: The Models3 date is an integer, so we can't just extract the
## first 4 characters, next 3 characters, etc.
## 
## 
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
decipher.Models3.date <- function(Models3.date){

  ## Find the year.
  yr <- trunc(Models3.date/1000)

  ## Find number of minutes.
  julian.day <- Models3.date %% 1000

  ## The first day of this year is our base date.
  jan1.yr <- as.Date(paste(yr, "-01-01", sep=""), format="%Y-%m-%d")

  ## Pass to as.Date function the number of days since the "origin"
  ## date.  Our origin date is Jan. 1, YYYY, which is stored as a Date
  ## object in jan1.yr.  Note that Jan. 1, YYYY is Julian day 001.
  my.date <- as.Date(julian.day-1, origin=jan1.yr) 

  return(my.date)
}




## ###########################################################
## Purpose: Combine date and time to obtain date-time in POSIX format.
##
## Input:
##   1. Date in Date format or as character string in format "YYYY-MM-DD".
##   2. Time as list with hrs, mins, and secs components or as
##      character string in "HH:MM:SS" (with hours 00-23).
##
## Returns: A date-time in POSIX format.
##
## Assumes: This code assumes that the time is not negative.  (For
## instance, the Models3 I/OAPI does allow for negative time steps,
## but these negative time steps will NOT be handled properly by this
## function.)
##
## 
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
combine.date.and.time <- function(date, time){

  ## Check whether time is a list like that returned by the
  ## decipher.Models3.time() function.
  if (is.list(time))  
    datetime <- strptime(paste(as.character(date), " ", time$hrs, ":",
                               time$mins, ":", time$secs, sep=""),
                         format="%Y-%m-%d %H:%M:%S", tz="GMT")

  ## Otherwise, assume time is a character string of form HH:MM:SS.
  else
    datetime <- strptime(paste(as.character(date), " ", time, sep=""),
                         format="%Y-%m-%d %H:%M:%S", tz="GMT")

  return(datetime)
}




## ###########################################################
## Purpose: Read in variable values from Models3 files.
##
## Input:
##   file: name of Models3 file to be read
##   var: name (character string) or number (positive integer) of
##     variable whose values are to be read
##   lcol, ucol: Lower and upper column bounds (positive integers) to
##     be read.  The default is to read all columns.
##   lrow, urow: Lower and upper row bounds (positive integers) to be
##     read.  The default is to read all rows.
##   llay, ulay: Lower and upper layer bounds (positive integers) to
##     be read.  The default is to read the first layer only.
##   ldatetime, udatetime: Starting and ending date-times (either Date
##     or POSIX class) in GMT.  The default is to read all date-times.
##     If the file is time-independent, the one available time step
##     will be read and user input for ldatetime and udatetime will be
##     disregarded.
##   units.km: TRUE (default value) if x,y-coordinates in model units of km.
##             FALSE if x,y-coordinates are in model units of m.
##
##
## Returns: List with several elements.
##   Element "data" holds the actual variable values in a 4D (or 3D,
##     in the case of time-independent files) array.  Dimensions are
##     columns, rows, layers, date-time steps.
##   Elements "x.cell.ctr" and "y.cell.ctr" give the coordinates in
##     model projection units of the centers of the grid cells.
##   Element "layer" gives the layer numbers.
##   Element "datetime" gives the date-time steps associated with the
##     variable, if the file is not time-independent.  For
##     time-independent files, element datetime is set to NULL.
##   Element "units.km" is T if model projection units are in km, F if in m.
##
## Assumes: The Models3 file is either time-independent or
## time-stepped.  It cannot be of type circular-buffer.
##
## 
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
get.Models3.variable <- function(file, var, lcol, ucol, lrow, urow,
                                 llay, ulay, ldatetime, udatetime,
                                 units.km=T){


  ## Require the netCDF packaget to read info from netCDF files (since
  ## IOAPI is a netCDF-based format).
  require("ncdf")
  
  ## Open netCDF file which has the projection we want to use..
  nc <- open.ncdf(file)


  ## ##########################
  ## MAKE SURE THE VARIABLE SPECIFIED IN PARAMETER var IS VALID.

  ## Get list of variable names.  Check that the variable name
  ## provided is on the list.  If a variable number is provided, make
  ## sure that there is a variable with that number.
  all.varnames <-  names(nc$var)
  

  ## Check to make sure the user specified a variable to read.  If
  ## not, close file, print error message, and exit.
  if (missing(var)){
    nc <- close.ncdf(nc)
    stop( paste("Specify the name or number of the variable to be read.  Variable names are: ", paste(all.varnames, collapse=", "), sep="") )
  }


  ## If var is a character string, its name must be on the list of
  ## variable names.
  if ( is.character(var) && !(var %in% all.varnames) ){
    nc <- close.ncdf(nc)
    stop( paste("File ", file, " does not contain variable named ", var, sep="") )
  }


  ## If var is numeric, it must be an integer between 1 and the number
  ## of variables.
  if (is.numeric(var)){

    ## Specified variable number must be an integer.
    if ( !(trunc(var) == var) ){
      nc <- close.ncdf(nc)
      stop( paste("Parameter var must be a whole number between 1-", length(all.varnames), sep="") )
    }

    ## Specified variable number cannot be less than 1 or more than
    ## the number of variables.    
    if ( (var < 1) || (var > length(all.varnames)) ){
      nc <- close.ncdf(nc)
      stop( paste("File ", file, " contains variables numbered 1-", length(all.varnames), sep="") )
    }
  }


  ## Parameter var must be either numeric or a character string.
  if ( !is.numeric(var) && !is.character(var) ){
    nc <- close.ncdf(nc)
    stop( "Parameter var must give either the name or number of the variable to be read." )
  }
  ## ##########################


  ## ##########################
  ## MAKE SURE THE INPUT FOR WHICH ROWS, COLUMNS, AND LAYERS TO READ
  ## MAKES SENSE.

  ## Find out the dimensions of the chosen variable.  I assume that
  ## these are listed in terms of number of columns, number of rows,
  ## number of layers, number of date-time steps.
  dimens <- nc[["var"]][[var]][["size"]]
  if (length(dimens) < 4){
    nc <- close.ncdf(nc)
    stop( "There are less than 4 dimensions in this file.")
  }


  ## If lower/upper column limits are missing, then make them the
  ## minimum/maximum available in the file.
  if (missing(lcol))
    lcol <- 1
  if (missing(ucol))
    ucol <- dimens[1]
  ## Check to make sure that the upper column limit is greater than or
  ## equal to the lower column limit.
  if (ucol < lcol){
    nc <- close.ncdf(nc)
    stop(paste("Upper column limit, ", ucol, ", is less than lower column limit, ", lcol, sep=""))
  }

  
  ## If lower/upper row limits are missing, then make them the
  ## minimum/maximum available in the file.
  if (missing(lrow))
    lrow <- 1
  if (missing(urow))
    urow <- dimens[2]
  ## Check to make sure that the upper row limit is greater than or
  ## equal to the lower row limit.
  if (urow < lrow){
    nc <- close.ncdf(nc)
    stop(paste("Upper row limit, ", urow, ", is less than lower column limit, ", lrow, sep=""))
  }

  
  ## If lower/upper row limits are missing, then make them the
  ## minimum/maximum available in the file.
  if (missing(llay))
    llay <- 1
  if (missing(ulay))
    ulay <- dimens[3]
  ## Check to make sure that the upper layer limit is greater than or
  ## equal to the lower layer limit.
  if (ulay < llay){
    nc <- close.ncdf(nc)
    stop( paste("Upper layer limit, ", ulay, ", is less than lower layer limit, ", llay, sep="") )
  }


  ## Check to make sure row, column, and layer limits are positive numbers.
  if ( (lcol <= 0) && (ucol <= 0) && (lrow <= 0) && (urow <= 0) && (llay <= 0) && (ulay <= 0)){
    nc <- close.ncdf(nc)
    stop("Upper and lower row, column, and layer boundaries must be positive.")
  }


  ## Find the proper subset of columns.
  column.seq <- 1:dimens[1]
  which.column <- which( (lcol <= column.seq) & (column.seq <= ucol) )
  start.column <- min(which.column)
  count.column <- max(which.column) - min(which.column) + 1
  rm(column.seq, which.column)
  
  ## Find the proper subset of rows.
  row.seq <- 1:dimens[2]
  which.row <- which( (lrow <= row.seq) & (row.seq <= urow) )
  start.row <- min(which.row)
  count.row <- max(which.row) - min(which.row) + 1
  rm(row.seq, which.row)
  
  ## Find the proper subset of layers.
  layer.seq <- 1:dimens[3]
  which.layer <- which( (llay <= layer.seq) & (layer.seq <= ulay) )
  start.layer <- min(which.layer)
  count.layer <- max(which.layer) - min(which.layer) + 1
  rm(which.layer)
  ## ##########################


  ## ##########################
  ## SUBSET THE DATE-TIMES (parameters ldatetime, udatetime).

  ## Check whether the file is time independent.  It is time
  ## independent if the time step increment is zero.
  tstep.incr <- att.get.ncdf(nc, varid=0, attname="TSTEP")$value


  ## If the time step is not 0, ensure we get the correct range of
  ## time steps.
  if (tstep.incr != 0){

    ## Form a sequence of all the datetimes included in the Models3 file.
    datetime.seq <- get.datetime.seq(file)


    ## If ldatetime is missing, assign it the earliest date-time; if
    ## udatetime is missing assign it the latest date-time.
    if (missing(ldatetime))
      ldatetime <- min(datetime.seq)
    if (missing(udatetime))
      udatetime <- max(datetime.seq)
  

    ## Check to see if the date-time limits are in Date format.  If so,
    ## make them into a POSIX format date.  For the lower limit, this
    ## would mean a time stamp at midnight (beginning of the given
    ## date).  For the upper limit, this would mean a time stamp at 23:59:59
    ## (last part of the given date).
    if ("Date" %in% class(ldatetime))
      ldatetime <- combine.date.and.time(date=ldatetime, time="00:00:00")
    if ("Date" %in% class(udatetime))
      udatetime <- combine.date.and.time(date=udatetime, time="23:59:59")


    ## Check to make sure lower bound on datetime is same as or earlier
    ## than the upper bound.
    if (udatetime < ldatetime){
      nc <- close.ncdf(nc)
      stop(paste("Upper date-time bound, ", udatetime, ", is before lower date-time bound, ", ldatetime, sep=""))
    }


    ## Find the dates in the sequence which fall in the specified range.
    which.datetime <- which( (ldatetime <= datetime.seq) & (datetime.seq <= udatetime) )
    start.datetime <- min(which.datetime)
    count.datetime <- max(which.datetime) - min(which.datetime) + 1
  }


  ## For a time indep. file, can only read the one time step available.
  else{
    message("Time independent file - reading only time step available.")
    start.datetime <- 1
    count.datetime <- 1
  }
  ## ##########################


  ## ##########################
  ## ACTUALLY EXTRACT THE DATA FOR THIS VARIABLE (parameter var).

  extracted.data <- get.var.ncdf( nc, varid=var, start=c(start.column, start.row, start.layer, start.datetime), count=c(count.column, count.row, count.layer, count.datetime) )

  ## If the time step is not 0, then there are presumably meaningful
  ## time steps in this file.  If the time step is 0, then the file is
  ## time-independent, and we allow the array to be a 3D array.
  
  ## Force this matrix/array into a 4D array, so that all dimensions
  ## are represented, even if some dimensions are of length 1.  Then,
  ## we pass the units for each dimension.
  if (tstep.incr != 0)
    dim(extracted.data) <- c(count.column, count.row, count.layer,
                           count.datetime)
  else
    dim(extracted.data) <- c(count.column, count.row, count.layer)
  ## ##########################

  
  ## ##########################
  ## FIND THE UNITS ASSOCIATED WITH X- AND Y- COORDINATES (IN MODEL
  ## UNITS) OF THE CENTER OF THE GRID CELLS.  WE ALREADY HAVE THE
  ## DATETIME SEQUENCE FROM THE CALCULATIONS ABOVE.

  x.coord <- get.coord.for.dimension(file, dimension="column",
                                     position="ctr", units.km)
  y.coord <- get.coord.for.dimension(file, dimension="row",
                                     position="ctr", units.km)
  ## ##########################


  ## ##########################
  ## PUT DATA AND UNITS TOGETHER IN A LIST TO RETURN TO USER.

  ## If not a time-independent file, then include date-time steps.
  if (tstep.incr != 0)
    extracted.list <- list(data=extracted.data, x.cell.ctr=x.coord,
                           y.cell.ctr=y.coord, layer=layer.seq,
                           datetime=datetime.seq[which.datetime],
                           units.km=units.km)
  else
    extracted.list <- list(data=extracted.data, x.cell.ctr=x.coord,
                           y.cell.ctr=y.coord, layer=layer.seq,
                           datetime=NULL, units.km=units.km)
  ## ##########################

  
  ## Close netCDF file.
  nc <- close.ncdf(nc)

  ## Return list of extracted information about variable var.
  return(extracted.list)
}




## ###########################################################
## Purpose: Subset the array resulting from a get.Models3.variable()
##          function call.
##
## Input:
##   var.info: list given by function get.Models3.variable().
##   llx, urx: Lower and upper x-coordinate bounds for the subsetted
##     grid in model projection units.  Defaults are the current
##     boundaries of the x range.
##   lly, ury: Lower and upper y-coordinate bounds for the subsetted
##     grid in model projection units.  Defaults are the current
##     boundaries of the y range.
##   llay, ulay; Lower and upper layers (positive integers) to include
##     in the subset.
##   ldatetime, udatetime: Starting and ending date-times (either Date
##     or POSIX class).  Defaults are the current boundaries of the
##     date-time range.
##   units.km: TRUE (default value) if x,y-coordinates in model units of km.
##             FALSE if x,y-coordinates are in model units of m.
##
## Returns: Subsetted array of variable values.
##
##
## Note: Subsetting the desired layers is easy, because we assume that
##   they are just numbered 1-num.layers.  The tricky parts are the
##   x,y-coordinates and the date-times, because we sometimes think of
##   those in terms of the relevant units (and not in terms of column
##   number, row number, or time step number.
## 
##
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 30Mar2010
## ###########################################################
subset.result.get.Models3.variable <- function(var.info, llx, urx, lly, ury, llay, ulay, ldatetime, udatetime, units.km=T){


  ## #############
  ## IF BOUNDARIES ARE GIVEN IN m, PUT THEM IN km.

  ## If llx, lly, urx, ury are not in km, divide by 1000 to put in km.
  if (!units.km){ 
    if (!missing(llx))
      llx <- llx/1000
    if (!missing(lly))
      lly <- lly/1000
    if (!missing(urx))
      urx <- urx/1000
    if (!missing(ury))
      ury <- ury/1000
  }
  ## #############


  ## #############
  ## DEAL WITH COLUMNS FIRST.

  ## How many columns are there?
  num.columns <- length(var.info$x.cell.ctr)

  if (num.columns > 1){

    ## Want to find the left and right bounds for each column.  To do
    ## that we need to find the cell width.  We do this in km.
    if (var.info$units.km){  ## Info about variable is already in km.
      cell.width <- var.info$x.cell.ctr[2] - var.info$x.cell.ctr[1]
      lbd <- var.info$x.cell.ctr - (cell.width/2.0)
      ubd <- var.info$x.cell.ctr + (cell.width/2.0)
    }
    else{  ## Info about variable is in m.
      cell.width <- (var.info$x.cell.ctr[2] - var.info$x.cell.ctr[1])/1000
      lbd <- (var.info$x.cell.ctr/1000) - (cell.width/2.0)
      ubd <- (var.info$x.cell.ctr/1000) + (cell.width/2.0)
    }
    rm(cell.width)

    ## If llx/urx is missing, set to preserve current boundaries.
    if (missing(llx))
      llx <- min(lbd)
    if (missing(urx))
      urx <- max(ubd)
    ## Check to make sure left limit is not greater than right limit.
    if (llx > urx)
      stop("Lower limit in the x direction is greater than upper limit.")
    
    ## Find the columns for which both the left and right sides fit
    ## inside the specified x range.
    which.columns <- which( (llx <= lbd) & (ubd <= urx) )
  }


  else{
    which.columns <- 1
    if ( (!missing(llx)) || (!missing(urx)) )
      message("Only one column for this variable, llx and urx input ignored.")
  }
  ## #############


  ## #############
  ## DEAL WITH ROWS.

  ## How many rows are there?
  num.rows <- length(var.info$y.cell.ctr)

  if (num.rows > 1){

    ## Want to find the left and right bounds for each column.  To do
    ## that we need to find the cell width.  We do this in km.
    if (var.info$units.km){  ## Info about variable is already in km.
      cell.width <- var.info$y.cell.ctr[2] - var.info$y.cell.ctr[1]
      lbd <- var.info$y.cell.ctr - (cell.width/2.0)
      ubd <- var.info$y.cell.ctr + (cell.width/2.0)
    }
    else{  ## Info about variable is in m.
      cell.width <- (var.info$y.cell.ctr[2] - var.info$y.cell.ctr[1])/1000
      lbd <- (var.info$y.cell.ctr/1000) - (cell.width/2.0)
      ubd <- (var.info$y.cell.ctr/1000) + (cell.width/2.0)      
    }
    rm(cell.width)
    
    ## If lly/ury is missing, set to preserve current boundaries.
    if (missing(lly))
      lly <- min(lbd)
    if (missing(ury))
      ury <- max(ubd)
    ## Check to make sure left limit is not greater than right limit.
    if (lly > ury)
      stop("Lower limit in the y direction is greater than upper limit.")
    
    ## Find the rows for which both the lower and upper sides fit
    ## inside the specified y range.
    which.rows <- which( (lly <= lbd) & (ubd <= ury) )
  }


  else{
    which.rows <- 1
    if ( (!missing(lly)) || (!missing(ury)) )
      message("Only one row for this variable, lly and ury input ignored.")
  }
  ## #############


  ## #############
  ## DEAL WITH LAYERS.

  ## How many layers are there?
  num.layers <- length(var.info$layer)

  if (num.layers > 1){

    ## If llay/ulay is missing, set to preserve current boundaries.
    if (missing(llay))
      lly <- min(var.info$layer)
    if (missing(ulay))
      ury <- max(var.info$layer)
    ## Check to make sure lower limit is not greater than upper limit.
    if (llay > ulay)
      stop("Lower layer limit is greater than upper limit.")
    
    ## Find the rows for which both the lower and upper sides fit
    ## inside the specified y range.
    which.layers <- which( (llay <= var.info$layer)
                          & (var.info$layer <= ulay) )
  }


  else{
    which.layers <- 1
    if ( (!missing(llay)) || (!missing(ulay)) )
      message("Only one layer for this variable, llay and ulay input ignored.")
  }
  ## #############


  ## #############
  ## DEAL WITH DATE-TIMES.

  ## How many date-time steps are there?
  num.datetimes <- length(var.info$datetime)
  ## If this length is 0, then var.info$datetime is NULL.  This means
  ## that the file is time-independent, and we cannot subset the
  ## date-time steps.
  if (num.datetimes == 0)
    time.indep <- TRUE
  else
    time.indep <- FALSE


  if (num.datetimes > 1){

    ## If date-time limits are missing, set to preserve current boundaries.
    if (missing(ldatetime))
      ldatetime <- min(var.info$datetime)
    if (missing(udatetime))
      udatetime <- max(var.info$datetime)


    ## Check to see if the date-time limits are in Date format.  If so,
    ## make them into a POSIX format date.  For the lower limit, this
    ## would mean a time stamp at midnight (beginning of the given
    ## date).  For the upper limit, this would mean a time stamp at 23:59:59
    ## (last part of the given date).
    if ("Date" %in% class(ldatetime))
      ldatetime <- combine.date.and.time(date=ldatetime, time="00:00:00")
    if ("Date" %in% class(udatetime))
      udatetime <- combine.date.and.time(date=udatetime, time="23:59:59")


    ## Check to make sure upper limit is not less than lower limit.
    if (ldatetime > udatetime)
      stop("Lower date-time limit is greater than upper date-time limit.")


    ## Find the columns for which both the left and right sides fit
    ## inside the specified x range.
    which.datetimes <- which( (ldatetime <= var.info$datetime)
                             & (var.info$datetime <= udatetime) )
  }

  
  else if (!time.indep){
    which.datetimes <- 1
    if ( (!missing(ldatetime)) || (!missing(udatetime)) )
      message("Only one date-time step for this variable, ldatetime and udatetime input ignored.")
  }


  else{
    if ( (!missing(ldatetime)) || (!missing(udatetime)) )
      message("Variable is time-independent, ldatetime and udatetime input ignored.")
  }
  ## #############


  ## #############
  ## SUBSET THE VARIOUS ELEMENTS OF THE INPUT VARIABLE INFO LIST, AND
  ## RETURN THE SUBSETTED INFO.
  
  ## Subset the array first, recognizing that time-independent files
  ## cannot be subsetted on the basis of date-time.
  if (!time.indep){
    subset.data <- var.info$data[which.columns, which.rows, which.layers, which.datetimes]
    dim(subset.data) <- c(length(which.columns), length(which.rows), length(which.layers), length(which.datetimes))
  }
  else{
    subset.data <- var.info$data[which.columns, which.rows, which.layers]
    dim(subset.data) <- c(length(which.columns), length(which.rows), length(which.layers))
  }


  ## Subset the x.cell.ctr and y.cell.ctr list elements.
  if (var.info$units.km){
    subset.x.cell.ctr <- var.info$x.cell.ctr[which.columns]
    subset.y.cell.ctr <- var.info$y.cell.ctr[which.rows]
  }
  else{
   subset.x.cell.ctr <- var.info$x.cell.ctr[which.columns]/1000
   subset.y.cell.ctr <- var.info$y.cell.ctr[which.rows]/1000
  } 
  ## Subset the layer sequence.
  subset.layer <- var.info$layer[which.layers]
  ## If not time-independent, subset the datetime element.
  if (!time.indep)
    subset.datetime <- var.info$datetime[which.datetimes]

  
  ## Our x- and y-coordinates are currently in km.  If user passed in
  ## units.km=F, we need to move these back to m (multiply by 1000).
  if (!units.km){
    subset.x.cell.ctr <- subset.x.cell.ctr * 1000
    subset.y.cell.ctr <- subset.y.cell.ctr * 1000
  }
  

  ## Form the list for the subsetted variable info.
  if (!time.indep)
    subset.list <- list(data=subset.data, x.cell.ctr=subset.x.cell.ctr, y.cell.ctr=subset.y.cell.ctr, layer=subset.layer, datetime=subset.datetime, units.km=units.km)
  else
    subset.list <- list(data=subset.data, x.cell.ctr=subset.x.cell.ctr, y.cell.ctr=subset.y.cell.ctr, layer=subset.layer, units.km=units.km)
  ## #############


  ## Return subsetted variable info to user.
  return(subset.list)
}
