#!/bin/csh -f

# ====================== JPROCv5.2 Run Script ======================= 
# Usage: run.jproc >&! jproc_V5.log &                                 
#
# To report problems or request help with this script/program:        
#             http://www.cmascenter.org
# =================================================================== 

#> Source the config.cmaq file to set the run environment
 source ../../../config.cmaq

#> Check that CMAQ_DATA is set: 
 if ( ! -e $CMAQ_DATA ) then
    echo "   $CMAQ_DATA path does not exist"
    exit 1
    endif
 echo " "; echo " Input data path, CMAQ_DATA set to $CMAQ_DATA"; echo " "

 set APPL     = v52 
 set CFG      = CMAQ-BENCHMARK
 set MECH     = cb6r3_ae6_aq 
 set EXEC     = JPROC_${APPL}_$EXEC_ID

#> Set the working directory:
 set BASE     = $CMAQ_HOME/UTIL/jproc/scripts
 set BLD   = ${BASE}/BLD_$APPL

 cd $BASE; date; set timestamp; cat $BASE/cfg.${CFG}; echo " "; set echo

#> JPROC run dates (produces one file per day)

 set STDATE   = 2011182         #> the beginning day for this run
 set ENDATE   = 2011182         #> the ending day

# =====================================================================
#> Input/Output Directories
# =====================================================================

 set CSQYpath   = $CMAQ_DATA/phot  # CSQY input data
 set PROFpath   = $CMAQ_DATA/phot  # PROF input data
 set ETpath     = $CMAQ_DATA/phot  # ET input data
 set TOMSpath   = $CMAQ_DATA/phot  # TOMS input data
 set OUTDIR   = $CMAQ_DATA/jproc   # Output directory

# =====================================================================
#> Input Files
# =====================================================================

 set ETfile    = ETirradiance.dat
 set PROFfile  = PROFILES.dat
 set O2ABSfile = O2_JPL06-2
 set O3ABSfile = O3O1D_JPL06-2
 set TOMSfile  = not_available

#>- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 setenv ET        $ETpath/$ETfile
 setenv PROFILES  $PROFpath/$PROFfile
 setenv TOMS      $TOMSpath/$TOMSfile
 setenv O2ABS     $CSQYpath/$O2ABSfile
 setenv O3ABS     $CSQYpath/$O3ABSfile
 setenv CSQY      $CSQYpath

# check ET input file

 if (! ( -e $ET ) ) then
    echo " $ET not found "
    exit
 endif

# check profile input file

 if (! ( -e $PROFILES ) ) then
    echo " $PROFILES not found "
    exit
 endif

# check TOMS input file

 setenv JPROC_TOMSEXIST  N  # Assume TOMS data file does not exist for this run
 if ( -e $TOMS ) then
    setenv JPROC_TOMSEXIST  Y
 endif

# check O2 absorption input file

 if (! ( -e $O2ABS ) ) then
    echo " $O2ABS not found "
    exit
 endif

# check O3 absorption input file

 if (! ( -e $O3ABS ) ) then
    echo " $O3ABS not found "
    exit
 endif

 if ( ! -d "$OUTDIR" ) mkdir -p $OUTDIR

 ls -l $BLD/$EXEC
 unlimit
 limit

 unalias rm
 
 @ Date = $STDATE
 while ( $Date <= $ENDATE )         # Loop thru all the days to run
    setenv JPROC_STDATE $Date
    echo "   Running for $Date ..."
    set JVfile = JTABLE_${Date}     # Daily output file name
    setenv JVALUES $OUTDIR/$JVfile
    if ( -e $JVALUES ) rm $JVALUES  # Remove existing output file

#   Executable call:
    time $BLD/$EXEC
    @ Date = $Date + 1
 end

 exit() 
