#! /bin/csh -f

# ===================== CALC_TMETRIC_v5.5.X Run Script =============
# Usage: run_calc_tmetric.csh >&! calc_tmetric.log &
#
# To report problems or request help with this script/program:
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org
# ===================================================================

# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel 

 cd ../../..
 source ./config_cmaq.csh

#> Set General Parameters for Configuring the Simulation
 set VRSN      = v55               #> Code Version
 set PROC      = mpi               #> serial or mpi
 set MECH      = cb6r5_ae7_aq      #> Mechanism ID
 set APPL      = Bench_2016_12SE1        #> Application Name (e.g. Gridname)
                                                      
#> Define RUNID as any combination of parameters above or others. By default,
#> this information will be collected into this one string, $RUNID, for easy
#> referencing in output binaries and log files as well as in other scripts.
 set RUNID = ${VRSN}_${compilerString}_${APPL}

#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! $?BINDIR ) then
  set BINDIR = ${CMAQ_HOME}/POST/calc_tmetric/scripts/BLD_calc_tmetric_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 set EXEC = calc_tmetric_${VRSN}.exe


#> Set output directory
 set POSTDIR = ${CMAQ_DATA}/POST                      #> Location where output file will be written

  if ( ! -e $POSTDIR ) then
	  mkdir $POSTDIR
  endif


# =====================================================================
#> CALC_TMETRIC Configuration Options
# =====================================================================

#> operation to perform - SUM (default) or AVG
 setenv OPERATION AVG
# setenv OPERATION SUM

#> list of species to output - set to "ALL" to process all species from INFILE, or list species to process
#> the output variable names will have the ${OPERATION} value (SUM or AVG) appended to the input variable names
# setenv SPECIES_1 ALL
 setenv SPECIES_1 O3
 setenv SPECIES_2 CO
 setenv SPECIES_3 PM25_TOT

#############################################################
#  Input files
#############################################################

#> ioapi input files containing SPECIES_{N} (max of 366)
 setenv M3_FILE_1 ${CMAQ_DATA}/POST/COMBINE_ACONC_${RUNID}_201607.nc
# setenv M3_FILE_2 ${CMAQ_DATA}/POST/COMBINE_ACONC_${RUNID}_201608.nc
        #[Add location of input file, e.g. COMBINE_ACONC file.]

#############################################################
#  Output files
#############################################################

#> ioapi output file
 setenv OUTFILE ${POSTDIR}/average_concentrations_${RUNID}.nc

#> Executable call:
 ${BINDIR}/${EXEC}

 set progstat = ${status}
 if ( ${progstat} ) then
   echo "ERROR ${progstat} in $BINDIR/$EXEC"
   exit( ${progstat} )
 endif

 exit()
