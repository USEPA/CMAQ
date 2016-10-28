### AQS Species R Merge Script ###
###
### This script merges the individual yearly species files available from the AQS website
### in .csv format into a single file with all species and records. Missing
### species for a record are indicated by -999. This script merges both daily
### and hourly species, and currently has slightly different species lists for
### each averaging period. This script assumes that the input .csv files are named
### using the same names as listed below in the format of [time_avg]_[species_name]_[year].csv
### where time can be either hourly or daily and species name is as indicated below. This
### is the default name of the files from AQS (after the files are unzipped).
###
### The hourly_obs_fac and daily_obs_fac are conversions factors applied to each species to 
### either account for the number of carbons in each species, since the observation are in   
### units of total carbon mass (e.g. ppbC), or to convert units (e.g ppm to ppb)
### 
### Note that a blank correction value of 0.4 ug/m3 is being applied to OC values. Also, where
### NH4 values are missing but SO4 and NO3 value are present (i.e. IMPROVE sites), NH4
### is estimated using the equation NH4 = 0.2903*NO3 + 0.375*SO4
###
### Created by K. Wyat Appel
### Last edited: March 22, 2016

##################################################################################
### Values read from environment variables set in merge_aqs_species.csh script ###
##################################################################################
year      <- Sys.getenv("year")
time_avg  <- Sys.getenv("time_avg")
base_dir  <- Sys.getenv("base_dir")
directory <- paste(base_dir,year,time_avg,sep="/") 
out_file  <- paste(base_dir,"/AQS_",time_avg,"_data_",year,".csv",sep="")
##################################################################################

##################################
### Daily AQS species to merge ###
##################################
# species output units are ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ug/m3,ppb,ppb,pbb,ppb,ppb

### species names that will be used to identify file names and used as the header names in the merged file
var_names_daily  <- c("PM25","PM10","SO4","NO3","NH4","OC","EC","Na","Cl","Al","Ca","Fe","Si","Ti","Mg","K","Mn","Benzene","Propylene","Toluene","Butadiene","Acrolein","Ethylene","Acetaldehyde","Formaldehyde","Isoprene","Ethane")

#### Factor to apply to observed value to account for units conversion or the number of carbons
daily_obs_fac    <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,6,3,7,4,3,5,2)
##################################

###################################
### Hourly AQS species to merge ###
###################################
# species output units are ug/m3,ug/m3,ppb,ppb,ppb,ppb,ppb,ppb,hPa,%,C,C,m/2,deg,ppb,ppb,ppb,ppb,ppb,ppb,ppb,ug/m3,ug/m3,ug/m3,ug/m3

#### species names that will be used to identify file names and used as the header names in the merged file
var_names_hourly <- c("PM25","PM10","O3","CO","SO2","NO","NO2","NOX","NOY","Pressure","RH","Temperature","Dewpoint","Wind_Speed","Wind_Direction","Benzene","Propylene","Toluene","Butadiene","Isoprene","Ethane","Ethylene","SO4","NO3","OC","EC")

### Factor to apply to observed value to account for units conversion or the number of carbons
hourly_obs_fac   <- c(1,1,.001,.001,1,1,1,1,1,1,1,1,1.9438,1.9438,1,1,6,3,7,4,5,2,2,1,1,1,1)
###################################

if (time_avg == "daily") {
   ob_infile       <- paste(directory,"/",time_avg,"_",var_names_daily[1],"_",year,".csv",sep="")
   print(paste("processing file ",ob_infile,sep=""))
   temp.df         <-read.csv(ob_infile,colClasses="character")
   temp.df$site_id <- paste(temp.df$State.Code,temp.df$County.Code,temp.df$Site.Num,sep="")
   temp.df$timeon  <- "00:00:00"
   temp.df$dateon  <- paste(temp.df$Date.Local,temp.df$timeon,sep=" ")
   temp.df$timeoff <- "23:59:59"
   temp.df$dateoff <- paste(temp.df$Date.Local,temp.df$timeoff,sep=" ")
   unique_record   <- paste(temp.df$site_id,temp.df$POC,temp.df$dateon,temp.df$dateoff)
   temp2.df        <- subset(temp.df, !duplicated(unique_record))
   aqs_merged      <- data.frame(site_id=temp2.df$site_id,POCode=temp2.df$POC,dateon=temp2.df$dateon,dateoff=temp2.df$dateoff,PM25=as.numeric(temp2.df$Arithmetic.Mean)/daily_obs_fac[1])

   for (i in 2:length(var_names_daily)) {
      ob_infile       <- paste(directory,"/",time_avg,"_",var_names_daily[i],"_",year,".csv",sep="")
      print(paste("processing file ",ob_infile,sep=""))
      temp.df         <-read.csv(ob_infile,colClasses="character")
      temp.df$site_id <- paste(temp.df$State.Code,temp.df$County.Code,temp.df$Site.Num,sep="")
      temp.df$timeon  <- "00:00:00"
      temp.df$dateon  <- paste(temp.df$Date.Local,temp.df$timeon,sep=" ")
      temp.df$timeoff <- "23:59:59"
      temp.df$dateoff <- paste(temp.df$Date.Local,temp.df$timeoff,sep=" ")
      unique_record   <- paste(temp.df$site_id,temp.df$POC,temp.df$dateon,temp.df$dateoff)
      temp2.df        <- subset(temp.df, !duplicated(unique_record))
      temp3.df        <- data.frame(site_id=temp2.df$site_id,POCode=temp2.df$POC,dateon=temp2.df$dateon,dateoff=temp2.df$dateoff,obs=as.numeric(temp2.df$Arithmetic.Mean)/daily_obs_fac[i])
      aqs_merged      <- merge(aqs_merged,temp3.df,by=c("site_id","POCode","dateon","dateoff"),all.x=T,all.y=T,incomparables="-999")
      names(aqs_merged)[names(aqs_merged)=="obs"] <- var_names_daily[i] # Replace obs column name with species name
   }
   aqs_merged$OC_Blank <- -0.4
}

if (time_avg == "hourly") {
   ob_infile       <- paste(directory,"/",time_avg,"_",var_names_hourly[1],"_",year,".csv",sep="")
   print(paste("processing file ",ob_infile,sep=""))
   temp.df         <- read.csv(ob_infile,colClasses="character")
   temp.df$site_id <- paste(temp.df$State.Code,temp.df$County.Code,temp.df$Site.Num,sep="")
   temp.df$timeon  <- paste(substr(temp.df$Time.Local,1,2),":00:00",sep="")
   temp.df$dateon  <- paste(temp.df$Date.Local,temp.df$timeon,sep=" ")
   temp.df$timeoff <- paste(substr(temp.df$Time.Local,1,2),":59:00",sep="")
   temp.df$dateoff <- paste(temp.df$Date.Local,temp.df$timeoff,sep=" ")
   unique_record   <- paste(temp.df$site_id,temp.df$POC,temp.df$dateon,temp.df$dateoff)
   temp2.df        <- subset(temp.df, !duplicated(unique_record))
   aqs_merged      <- data.frame(site_id=temp2.df$site_id,POCode=temp2.df$POC,dateon=temp2.df$dateon,dateoff=temp2.df$dateoff,PM25=as.numeric(temp2.df$Sample.Measurement)/hourly_obs_fac[1])

   for (i in 2:length(var_names_hourly)) {
      ob_infile       <- paste(directory,"/",time_avg,"_",var_names_hourly[i],"_",year,".csv",sep="")
      print(paste("processing file ",ob_infile,sep=""))
      temp.df         <-read.csv(ob_infile,colClasses="character")
      temp.df$site_id <- paste(temp.df$State.Code,temp.df$County.Code,temp.df$Site.Num,sep="")
      temp.df$timeon  <- paste(substr(temp.df$Time.Local,1,2),":00:00",sep="")
      temp.df$dateon  <- paste(temp.df$Date.Local,temp.df$timeon,sep=" ")
      temp.df$timeoff <- paste(substr(temp.df$Time.Local,1,2),":59:00",sep="")
      temp.df$dateoff <- paste(temp.df$Date.Local,temp.df$timeoff,sep=" ")
      unique_record   <- paste(temp.df$site_id,temp.df$POC,temp.df$dateon,temp.df$dateoff)
      temp2.df        <- subset(temp.df, !duplicated(unique_record))
      {
         if ((var_names_hourly[i] == "Temperature") || (var_names_hourly[i] == "Dewpoint")) { 
            temp3.df        <- data.frame(site_id=temp2.df$site_id,POCode=temp2.df$POC,dateon=temp2.df$dateon,dateoff=temp2.df$dateoff,obs=((9*as.numeric(temp2.df$Sample.Measurement)/5)+32))
         }
         else {
            temp3.df        <- data.frame(site_id=temp2.df$site_id,POCode=temp2.df$POC,dateon=temp2.df$dateon,dateoff=temp2.df$dateoff,obs=as.numeric(temp2.df$Sample.Measurement)/hourly_obs_fac[i])
         }
      }
      aqs_merged      <- merge(aqs_merged,temp3.df,by=c("site_id","POCode","dateon","dateoff"),all.x=T,all.y=T,incomparables="-999")
      names(aqs_merged)[names(aqs_merged)=="obs"] <- var_names_hourly[i] # Replace obs column header name with species name
   }
}

###############################
### Write final merged file ###
###############################

aqs_merged[is.na(aqs_merged)] <- -999 # Replace NAs with -999
aqs_merged_sorted <- aqs_merged[order(as.character(aqs_merged$site_id)),] # Sort file by site ID

### Replace missing NH4 values with an estimated value based on NO3 and SO4 measurements ###
if (time_avg == 'daily') {
   NH4_missing <- aqs_merged_sorted$NH4 == -999 & aqs_merged_sorted$SO4 >= 0 & aqs_merged_sorted$NO3 >= 0
   aqs_merged_sorted$NH4[NH4_missing] <- 0.2903*aqs_merged_sorted$NO3[NH4_missing] + 0.375*aqs_merged_sorted$SO4[NH4_missing]   
}

write.table(aqs_merged_sorted,file=out_file,row.names=F,sep=",",quote=F) # Write merged file as csv
