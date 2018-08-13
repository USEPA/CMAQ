#! /bin/csh -f

# ===================== CALC_TMETRIC_v5.3 Run Script =============
# Usage: run.calc_tmetric.csh >&! calc_tmetric_v53.log &
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

#> Set the model version
 set VRSN = v53

#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! $?BINDIR ) then
  setenv BINDIR ${CMAQ_HOME}/POST/calc_tmetric/scripts/BLD_calc_tmetric_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 setenv EXEC calc_tmetric_${VRSN}.exe


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
 setenv M3_FILE_1 ${CMAQ_DATA}/POST/COMBINE_ACONC_201107.nc
# setenv M3_FILE_2 ${CMAQ_DATA}/POST/COMBINE_ACONC_201108.nc
        #[Add location of input file, e.g. COMBINE_ACONC file.]

#############################################################
#  Output files
#############################################################

#> ioapi output file
 setenv OUTFILE ${CMAQ_DATA}/POST/average_concentrations.nc

#> Executable call:
 ${BINDIR}/${EXEC}

 exit()
