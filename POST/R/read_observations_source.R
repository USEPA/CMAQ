## ###########################################################
## Read observational data from different networks and data formats.
## This file contains the following functions to read observations:
## read.Daily.AIRNOW
## read.aircraft 
## read.ICARTT
## read.Aeronet.var 
## read.TOLNet 
## read.ICARTT_vector
#############################################################

## ###########################################################
## Purpose:  Read in daily AIRNOW data to combine with MODEL 
## data by location and time (paired)
## Inputs:
##  AQS.file: AQF file location and file name 
##  variable name: (O3, PM25, etc.)
##  sdate and edate: The beginning and End date to match the model 
## Returns:
## data frame 
##  
## REVISION HISTORY:
##   Prototype: Daiwen Kang, 17Sep2013
## ###########################################################
read.Daily.AIRNOW <- function(fdir, var, sdate, edate) {
  dir.len <- nchar(fdir)
  if(substr(fdir, dir.len, dir.len) != "/") {
      file.dir <- paste(fdir, "/", sep="")
    } else {
       file.dir <- fdir
    }
#  file.dir <- "/work/AQF/CNUS_CMAQ_Runs/Evaluation/airnow/"
 ### get the site information #### 
     site.file <- paste(file.dir, "historical-sites.dat", sep="")
    site.inf <- data.frame(read.csv(site.file,   header=F))
    site.inf <- site.inf[, -(8:9)]
    site.inf$lon <- -(site.inf$V5+site.inf$V6/60.0+site.inf$V7/3600.0)
    site.inf$lat <- site.inf$V2+site.inf$V3/60.0+site.inf$V4/3600.0
    names(site.inf)[1] <- "sid"
    site.inf <- site.inf[, c("sid", "lon", "lat")]
     site.inf$sid <- sprintf("%09d", site.inf$sid)

   if(var == "O3") {
       file.f <- "super"
   } else {
      file.f <- "pmfine"
   }
   num.days <- as.numeric(as.Date(edate) - as.Date(sdate))+1
    In.OBS <- NULL
   for(i in 1:num.days) {
       in.day <- as.Date(sdate) + i-1
       mmddyy <- format(in.day, "%m%d%y")
       print(paste("Now processing: ", format(in.day, "%m/%d/%Y"), sep=""))
       con <- file(paste(file.dir, file.f, "_", mmddyy, "_Peak.obs", sep=""), open="r")
       datalist <- list()
       flaglist <- list()
      record <- readLines(con, warn=FALSE)
      close(con)
       tz.index <- grep("TZONE", record)
       myVector <- unlist(strsplit(gsub(" ", "", record[tz.index]), ","))
      tz.name <- myVector[2]
      tz.offset <- as.numeric(myVector[3])
       begin.index <- grep("BEGIN_DATA", record)
       end.index <- grep("END_DATA", record)
      dateon <- in.day
      data.index <- seq((begin.index[1]+1), (end.index[1]-2), by=2)
      flag.index <- seq((begin.index[1]+2), (end.index[1]-1), by=2)
      data.lines <- record[data.index]
      flag.lines <- record[flag.index]
      data.rds <- lapply(lapply(gsub(" ", "",data.lines), strsplit, ","), unlist)
      data.num <- lapply(lapply(data.rds, unlist), function(x) as.numeric(x[(length(x)-23) : length(x)]))
      sites <- lapply(lapply(data.rds, unlist), function(x) x[length(x)-24])
      flag.rds <- lapply(lapply(gsub(" ", "",flag.lines), strsplit, ","), unlist)
      flag.rds <- lapply(lapply(flag.rds, unlist), function(x) x[(length(x)-23):length(x)])
      flag.index <- lapply(lapply(flag.rds, unlist), function(x) grep("[^G|I]", x))
       dtoff <- as.POSIXct(paste(dateon, " ", sprintf("%02d", c(0:23)), ":00:00", sep=""), tz=tz.name, usetz=TRUE)
       dton <- dtoff - 3540
       data.num <- mapply(function(x, y) replace(x, y, NA), lapply(data.num, unlist), lapply(flag.index, unlist))
       In.s <- data.frame(sid=rep(unlist(sites), each=length(dton)), dateon=dton, dateoff=dtoff,gif= tz.offset, obs = as.vector(data.num) )
       In.OBS <- rbind(In.OBS, In.s)
        }
     In.OBS <- merge(site.inf, In.OBS)
     In.OBS <- In.OBS[with(In.OBS, order(sid, dateon)), ]
     row.names(In.OBS) <- NULL
     return(In.OBS)
}

## ###########################################################
## Purpose:  Read in daily aircraft measurement data in 
## comma delimited format
## Inputs:
##  infile: aircraft file location and file name 
## variable name: (T, RH, Ozone etc.)
## sdate and edate: The beginning and End date to match the model 
## Returns:
## data frame 
##  
## REVISION HISTORY:
##   Prototype: Daiwen Kang, 14OCT2013
## ###########################################################
read.aircraft <- function(infile, vars) {
   record <- read.csv(infile, header = FALSE, as.is=TRUE)
   name.index <- grep("Date", record$V1)
   var.names <- as.vector(t(record[name.index, ]))
   record <- record[-(1:(name.index+2)), ]
   names(record) <- var.names
   air.data <- record[,c("Date", "DOY", "Time (UT)", "Lat", "Lon", "Alt (GPS)", vars)]
   air.data$Date <- format(as.Date(air.data$Date, "%m/%d/%Y"), "%Y-%m-%d")
   air.data$datetime <- combine.date.and.time(air.data$Date, time=air.data[,3])
   air.data <- air.data[, c("datetime", "Lat", "Lon", "Alt (GPS)", vars)]
   row.names(air.data) <- NULL
  return(air.data)
 }
 
## ###########################################################
## Purpose:  Read in measurement data in ICARTT format 
## Inputs:
##  infile: ICARTT file location and file name 
## time.vars: either a triple (Start_UTC, Stop_UTC, and Mid_UTC ) for daily file
## or (UTC, JDAY) for integrate file
## loc.vars: latitude, longitude, and one of the altitudes (pressure ALT, GPS ALT, etc. )
## variable names (T, RH, Ozone etc.)
## Returns:
## data frame or list
##  
## REVISION HISTORY:
##   Prototype: Daiwen Kang, 4NOV2013
##   Modified: Daiwen Kang, 18Apr2014 to accomodate different types
## ###########################################################
read.ICARTT <- function(infile, time.vars=NULL, loc.vars=NULL, par.vars=NULL, rw.names=FALSE, space.independent=FALSE, time.independent=FALSE,comma=TRUE) {
  header <- readLines(infile, n=7L, warn=TRUE)
  headrec <- as.numeric(unlist(strsplit(header[1], '[, ]'))[1])
  head7 <- unlist(strsplit(header[7], ','))
  header <- readLines(infile, n=headrec, warn=TRUE)
  start_date <- format(strptime(paste(head7[1], head7[2], head7[3]), format="%Y %m %d"), "%Y-%m-%d")
  origin <- paste(head7[1], "01", "01", sep="-")
  var.num <- as.numeric(header[10])
  var.names <- NULL
  var.units <- NULL
  for(i in 13:(13+var.num-1)) {
      var.names <- append(var.names, sub("^\\s+", "", unlist(strsplit(header[i], ','))[1]))
      var.units <- append(var.units, sub("^\\s+", "", unlist(strsplit(header[i], ','))[2]))
   }
  for(i in 2:length(var.names)) {
     if(var.names[i] == var.names[i-1]){
       var.names[i-1] <- paste(var.names[i-1], var.units[i-1], sep='_')
       var.names[i] <- paste(var.names[i], var.units[i], sep='_')
      }
   }
  if(space.independent) { # for measurement at a fixed location
    loc.ind <- grep("LOCATION", header)
    Location <- unlist(strsplit(header[loc.ind], "[:;,]"))
    Location <- sub("^\\s+", "", Location)
    if("lat" %in% Location){
      lat <- Location[grep("lat", Location) + 1]
    }
    if("lon" %in% Location){
      lon <- Location[grep("lon", Location) + 1]
    }
    if(length(grep("[0-9]N", Location)) > 0) {
      lat <- Location[grep("[0-9]N", Location)]
    }
        if(length(grep("[0-9]W", Location)) > 0) {
      lon <- Location[grep("[0-9]W", Location)]
    }
    if(!exists("lat")) {
      lat <- Location[2]
    }
    if(!exists("lon")) {
      lon <- Location[3]
    }
    atl <- Location[length(Location)]
  }
  if(time.independent) {# for measurement at a fixed time (such as O3 sonde)
    time.ind <- grep("Launch time", header)
    time <- sub("^\\s+", "", unlist(strsplit(header[time.ind], '='))[2])
    datetime <- paste(start_date, time)
  }
 if(rw.names) { #the variable names are included before data record as one row
   if(comma) {
    record <- read.csv(infile, header = TRUE, as.is=TRUE, skip = headrec-1)
   } else {
     record <- read.table(infile, header = TRUE, as.is=TRUE, skip = headrec-1)
     record <- record[, -1]
   }
     rec.names <- names(record)
 } else {
    if(comma) {
      record <- read.csv(infile, header = TRUE, as.is=TRUE, skip = headrec-1)
    } else {
     record <- read.table(infile, header = FALSE, as.is=TRUE, skip = headrec)
     record <- record[, -1]
    }
     names(record) <- var.names
 }
  if(is.null(par.vars)) {
    par.vars <- var.names
   }
   if(space.independent & time.independent) {
   s.data <- record[, par.vars]
   air.data <- list(latitude=lat, longitude=lon, launchHieight=atl, datetime=datetime, icartt.data=s.data)
  } else {
     if(is.null(time.vars)){
       ind.utc <-  grep("UTC", rec.names, ignore.case=T, value=T)
     } else {
       ind.utc <- time.vars
     }
     if(space.independent) {
       s.data <- record[,c(ind.utc,  par.vars)]
     } else {
       s.data <- record[,c(ind.utc, loc.vars, par.vars)]
     }
    if(length(ind.utc) == 1) { #only one time variable
      hr <- floor(s.data[,ind.utc]/3600)
      mm <- floor((s.data[,ind.utc]%%3600)/60)
      ss <- (s.data[,ind.utc]%%3600)%%60
      s.data$datetime <- combine.date.and.time(start_date, time = paste(hr, mm, ss, sep=":"))
      if(space.independent) {
        s.data <- s.data[, c("datetime", par.vars)]
      } else {
        s.data <- s.data[, c("datetime", loc.vars, par.vars)]
      }
     } else if(length(ind.utc) >= 3) { #| tolower(time.vars[1])=="start_utc") has more than 2 time variables
      hr.st <- floor(s.data[,ind.utc[1]]/3600)
      mm.st <- floor((s.data[,ind.utc[1]]%%3600)/60)
      ss.st <- (s.data[,ind.utc[1]]%%3600)%%60
      s.data$timeon <- combine.date.and.time(start_date, time = paste(hr.st, mm.st, ss.st, sep=":"))
      hr.ed <- floor(s.data[,ind.utc[2]]/3600)
      mm.ed <- floor((s.data[,ind.utc[2]]%%3600)/60)
      ss.ed <- (s.data[,ind.utc[2]]%%3600)%%60
      s.data$timeoff <- combine.date.and.time(start_date, time = paste(hr.ed, mm.ed, ss.ed, sep=":"))
      hr.md <- floor(s.data[,ind.utc[3]]/3600)
      mm.md <- floor((s.data[,ind.utc[3]]%%3600)/60)
      ss.md <- (s.data[,ind.utc[3]]%%3600)%%60
      s.data$datetime <- combine.date.and.time(start_date, time = paste(hr.md, mm.md, ss.md, sep=":"))
       if(space.independent) {
        s.data <- s.data[, c("timeon","datetime", "timeoff", par.vars)]
      } else {
        s.data <- s.data[, c("timeon","datetime", "timeoff", loc.vars, par.vars)]
      }
    } else { #have 2 variables, usually a UTC time and jyear variable
      if("JDAY" %in% toupper(ind.utc[2])) {
        hr <- floor(s.data[,ind.utc[1]]/3600)
        mm <- floor((s.data[,ind.utc[1]]%%3600)/60)
        ss <- (s.data[,ind.utc[1]]%%3600)%%60
      s.data$datetime <- combine.date.and.time(as.Date(s.data[,ind.utc[2]] - 1, origin=origin), time = paste(hr, mm, ss, sep=":"))
#      s.data$datetime <- combine.date.and.time(start_date, time = paste(hr, mm, ss, sep=":"))
      if(space.independent) {
        s.data <- s.data[, c("datetime", par.vars)]
      } else {
        s.data <- s.data[, c("datetime", loc.vars, par.vars)]
      }
    } else {
      hr.st <- floor(s.data[,ind.utc[1]]/3600)
      mm.st <- floor((s.data[,ind.utc[1]]%%3600)/60)
      ss.st <- (s.data[,ind.utc[1]]%%3600)%%60
      s.data$timeon <- combine.date.and.time(start_date, time = paste(hr.st, mm.st, ss.st, sep=":"))
      hr.ed <- floor(s.data[,ind.utc[2]]/3600)
      mm.ed <- floor((s.data[,ind.utc[2]]%%3600)/60)
      ss.ed <- (s.data[,ind.utc[2]]%%3600)%%60
      s.data$timeoff <- combine.date.and.time(start_date, time = paste(hr.ed, mm.ed, ss.ed, sep=":"))
      if(space.independent) {
        s.data <- s.data[, c("timeon", "timeoff", par.vars)]
      } else {
        s.data <- s.data[, c("timeon", "timeoff", loc.vars, par.vars)]
      }
    }
   }
   if(space.independent) {
     air.data <- list(latitude=lat, longitude=lon, launchHieight=atl, icartt.data=s.data)
   } else {
     air.data <- s.data
  }
 }
   return(air.data)
}

 ## ###########################################################
## Purpose:  Read in the processed AERONET data to combine with MODEL data  
## by location and time (paired)
## Inputs:
##  AERONET.file: AQF file location and file name 
## variable name (AOT, etc.)
## sdate and edate: The beginning and End date to match the model 
## Returns:
## data frame 
##  
## REVISION HISTORY:
##   Prototype: Daiwen Kang, 9Sep2015
## ###########################################################

read.Aeronet.var <- function(AQS.file, var, sdate, edate) {
  In.OBS <- data.frame(read.csv(AQS.file,header=TRUE))
  In.OBS <- In.OBS[, names(In.OBS) %in% c("SITE_ID", "DATEON","DATEOFF", var)]
  In.OBS<- In.OBS[substr(In.OBS$DATEON, 1, 10) >= sdate & substr(In.OBS$DATEON, 1, 10) <= edate, ]
  colnames(In.OBS)[[1]] <- "sid"
  colnames(In.OBS)[[4]] <- "obs"
#  In.OBS$sid <- sprintf("%09d", In.OBS$sid) 
  return(In.OBS)
}

## ###########################################################
## Purpose:  Read in measurement data in  TOLNet format 
## Inputs:
##  infile: TOLNet file location and file name 
## time.vars: either a triple (Start_UTC, Stop_UTC, and Mid_UTC ) for daily file
## or (UTC, JDAY) for integrate file
## loc.vars: latitude, longitude, and one of the altitudes (pressure ALT, GPS ALT, etc. )
## variable names (T, RH, Ozone etc.)
## Returns:
## data frame or list
##  
## REVISION HISTORY:
##   Prototype: Daiwen Kang, 11Sep2015
## ###########################################################

read.TOLNet <- function(infile, loc.vars=NULL, par.vars=NULL, rw.names=FALSE) {
   TOLNetobj <- list()
   con <- file(infile, "r")
  header <- readLines(con, n=4L, warn=TRUE)
  LineCounter <- 4
  ngh <- as.numeric(unlist(strsplit(header[1], '[,;]'))[1])
  nprof <- as.numeric(unlist(strsplit(header[3], '[,;]'))[1])
  ncol <- as.numeric(unlist(strsplit(header[4], '[,;]'))[1])
  rlines <- ncol+1
  header <- readLines(con, n=rlines, warn=TRUE)
  LineCounter <- LineCounter + rlines
  header <- readLines(con, n=1L, warn=TRUE)
  ngc <- as.numeric(unlist(strsplit(header[1], '[,;]'))[1])
  LineCounter <- LineCounter + 1
  header <- readLines(con, n=ngc, warn=TRUE)
  LineCounter <- LineCounter + ngc
   for(i in 1:nprof) {
      plist <- list()
      header <- readLines(con, n=1L, warn=TRUE)   #read off the #BEGIN PROFLIE line
       LineCounter <- LineCounter + 1
       header <- readLines(con, n=1L, warn=TRUE)
       nph <- as.numeric(unlist(strsplit(header[1], '[,;]'))[1])   ## lines of this profile header
       header <- readLines(con, n=1L, warn=TRUE)
       npl <- as.numeric(unlist(strsplit(header[1], '[,;]'))[1])     ## Profile data record number
       ### read the rest of this profile header and extract the location and date-time information
       header <- readLines(con, nph-2, warn=TRUE)
       loc.parms <- parse.Location(header)
       DT.parms <- parse.DateTime(header)
       ### Read current profile data including the variable names and put the data into a list
        header <- readLines(con, n=npl+1, warn=TRUE)
        plist[["Location"]] <- loc.parms
        plist[["pnames"]] <- c("lon", "lat", "elevation")
        plist[["DateTime"]] <- DT.parms
        plist[["DTnames"]] <- c("StartDT", "EndDT", "MedDT")
       plist[["profileData"]] <- read.csv(textConnection(header))[, c(loc.vars, par.vars)]
        LineCounter <- LineCounter + nph + npl+1
        TOLNetobj[[i]] <-  plist
    }
     close(con)
      return(TOLNetobj)
 }

## ###########################################################
## Purpose:  Read in measurement data in  ICARTT vector format 
## Inputs:
##  infile: Lidar measurement file location and file name 
## The file contains  a triple (Start_UTC, Stop_UTC, and Mid_UTC ) for each profiel
## and only one vector variable (e.g. O3[]) and the altitude parameters (Alt_start,
## Alt_incr, and total profile data points)
## loc.vars: latitude, longitude, and one of the altitudes (pressure ALT, GPS ALT, etc. )
## Returns:
## data frame or list
##  
## REVISION HISTORY:
##   Prototype: Daiwen Kang, 13October2015
## ###########################################################
read.ICARTT_vector <- function(infile) {
  con <- file(infile, "r")
  header <- readLines(con, n=7L, warn=TRUE)
  headrec <- as.numeric(unlist(strsplit(header[1], '[, ]'))[1])
  head7 <- unlist(strsplit(header[7], ','))
  header <- readLines(con, n=headrec-7, warn=TRUE)
    loc.parms <- parse.Location(header)
  start_date <- format(strptime(paste(head7[1], head7[2], head7[3]), format="%Y %m %d"), "%Y-%m-%d")

 TOLNetobj <- list()
  i <- 0
 while(length(pro_time <- readLines(con, n=1L, warn=TRUE)) > 0 ) {
      i <- i+1
     plist <- list()
      time_vector <- as.numeric(unlist(strsplit(pro_time, ',')))
      StartDT <- parse.UTCtime(time_vector[1], start_date)
      EndDT <- parse.UTCtime(time_vector[2], start_date)
      MedDT <- parse.UTCtime(time_vector[3], start_date)
      DT.parms <- c(StartDT, EndDT, MedDT)
      Num_alt <- time_vector[4]
      Alt_start <- time_vector[5]
      Alt_incr <- time_vector[6]
       prf_alt <- seq(from=Alt_start, by=Alt_incr, length.out=Num_alt)
      O3v <- as.numeric(unlist(strsplit(readLines(con, n=1L, warn=TRUE), ',')))
         plist[["Location"]] <- loc.parms
        plist[["pnames"]] <- c("lon", "lat", "elevation")
        plist[["DateTime"]] <- DT.parms
        plist[["DTnames"]] <- c("StartDT", "EndDT", "MedDT")
        plist[["profileData"]] <- data.frame(Alt_mAGL=prf_alt, O3=O3v)
        TOLNetobj[[i]] <-  plist
  }
    close(con)
      return(TOLNetobj)
 }
   

