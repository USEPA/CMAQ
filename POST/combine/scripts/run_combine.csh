#! /bin/csh -f


# ====================== COMBINE Run Script ======================== 
# Usage: run.combine.uncoupled.csh >&! combine_v52_uncoupled.log &                                
#
# To report problems or request help with this script/program:     
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org  (CMAS Website)
# ===================================================================  


# ~~~~~~~~~~~~~~~~~~~~~~~~ Start EPA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#> Portable Batch System - The following specifications are 
#> recommended for executing the runscript on the cluster at the 
#> National Computing Center used primarily by EPA.
#PBS -N run.combine.Uncoupled.csh
#PBS -l walltime=1:30:00
#PBS -l nodes=login
#PBS -q singlepe 
#PBS -V
#PBS -m n
#PBS -j oe
#PBS -o ./combine_SE52BENCH.log

#> Configure the system environment
#> source /etc/profile.d/modules.csh 
#> Set location of combine executable.
#> setenv BINDIR /home/css/CMAQ-Tools/scripts/combine
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~ End EPA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel 
 source ../../config_cmaq.csh

#> Set the model version
 set VRSN = v52

#> Set General Parameters for Labeling the Simulation
 set MECH = cb6r3_ae6_aq          #> Mechanism ID
 set APPL = v52_intel_SE52BENCH	  #> Application Name (e.g. Code version, compiler, gridname, emissions, etc.)

#> Set the build directory if this was not set above 
#> (this is where the CMAQ executable is located by default).
 if ( ! -e $BINDIR ) then
  setenv BINDIR $CMAQ_HOME/Tools/Combine/BLD_combine_${VRSN}_${compiler}
 endif

#> Set the name of the executable.
 setenv EXEC combine_${VRSN}.exe

#> Set location of CMAQ repo.  This will be used to point to the correct species definition files.
  setenv REPO_HOME  [Add location of CMAQv5.2 repository here]

#> Set working, input and output directories
 setenv WORKDIR    $cwd                         #> Working Directory. Where the runscript/data are.
 setenv METDIR     ${WORKDIR}/INPUT/met/mcip    #> Met Output Directory
 setenv CCTMOUTDIR ${WORKDIR}/OUTPUT            #> CCTM Output Directory
 setenv POSTDIR    ${WORKDIR}/POST              #> Location where combine file will be written


# =====================================================================
#> COMBINE Configuration Options
# =====================================================================

#> Set Start and End Days for looping
 set START_DATE = "2011-07-10"     #> beginning date (July 10, 2011)
 set END_DATE   = "2011-07-14"     #> ending date    (July 14, 2011)
 
#> Set location of species definition files for concentration and deposition species.
 setenv SPEC_CONC $REPO_HOME/POST/combine/scripts/spec_def_files/SpecDef_${MECH}.txt
 setenv SPEC_DEP  $REPO_HOME/POST/combine/scripts/spec_def_files/SpecDef_Dep_${MECH}.txt

#> Use GENSPEC switch to generate a new specdef file (does not generate output file).
 setenv GENSPEC N


# =====================================================================
#> Begin Loop Through Simulation Days to Create ACONC File
# =====================================================================

#> Set the species definition file for concentration species.
 setenv SPECIES_DEF $SPEC_CONC
 
#> Loop through all days between START_DAY and END_DAY
 set TODAYG = ${START_DATE}
 set TODAYJ = `date -ud "${START_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
 set STOP_DAY = `date -ud "${END_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

 while ($TODAYJ <= $STOP_DAY )  #>Compare dates in terms of YYYYJJJ
 
  #> Retrieve Calendar day Information
   set YYYY = `date -ud "${TODAYG}" +%Y`
   set YY = `date -ud "${TODAYG}" +%y`
   set MM = `date -ud "${TODAYG}" +%m`
   set DD = `date -ud "${TODAYG}" +%d`
  #> for files that are indexed with Julian day:
   #  set YYYYJJJ = `date -ud "${TODAYG}" +%Y%j` 

  #> Define name of combine output file to save hourly average concentration.
  #> A new file will be created for each month/year.
   setenv OUTFILE ${POSTDIR}/COMBINE_ACONC_${APPL}_$MM$YYYY

  #> Define name of input files needed for combine program.
  #> File [1]: CMAQ conc/aconc file
  #> File [2]: MCIP METCRO3D file
  #> File [3]: CMAQ APMDIAG file
  #> File [4]: MCIP METCRO2D file
   setenv INFILE1 $CCTMOUTDIR/CCTM_ACONC_${APPL}_$YYYY$MM$DD
   setenv INFILE2 $METDIR/METCRO3D_$YY$MM$DD
   setenv INFILE3 $CCTMOUTDIR/CCTM_APMDIAG_${APPL}_$YYYY$MM$DD
   setenv INFILE4 $METDIR/METCRO2D_$YY$MM$DD

  #> Executable call:
   ${BINDIR}/${EXEC}

  #> Increment both Gregorian and Julian Days
   set TODAYG = `date -ud "${TODAYG}+1days" +%Y-%m-%d` #> Add a day for tomorrow
   set TODAYJ = `date -ud "${TODAYG}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

 end #Loop to the next Simulation Day


# =====================================================================
#> Begin Loop Through Simulation Days to Create DEP File
# =====================================================================

#> Set the species definition file for concentration species.
 setenv SPECIES_DEF $SPEC_DEP
 
#> Loop through all days between START_DAY and END_DAY
 set TODAYG = ${START_DATE}
 set TODAYJ = `date -ud "${START_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
 set STOP_DAY = `date -ud "${END_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

 while ($TODAYJ <= $STOP_DAY )  #>Compare dates in terms of YYYYJJJ
 
  #> Retrieve Calendar day Information
   set YYYY = `date -ud "${TODAYG}" +%Y`
   set YY = `date -ud "${TODAYG}" +%y`
   set MM = `date -ud "${TODAYG}" +%m`
   set DD = `date -ud "${TODAYG}" +%d`
  #> for files that are indexed with Julian day:
   #  set YYYYJJJ = `date -ud "${TODAYG}" +%Y%j` 

  #> Define name of combine output file to save hourly total deposition.
  #> A new file will be created for each month/year.
   setenv OUTFILE ${POSTDIR}/COMBINE_DEP_${APPL}_$MM$YYYY

  #> Define name of input files needed for combine program.
  #> File [1]: CMAQ DRYDEP file
  #> File [2]: CMAQ WETDEP file
  #> File [3]: MCIP METCRO2D
  #> File [4]: {empty}
   setenv INFILE1 $CCTMOUTDIR/CCTM_DRYDEP_${APPL}_$YYYY$MM$DD
   setenv INFILE2 $CCTMOUTDIR/CCTM_WETDEP1_${APPL}_$YYYY$MM$DD
   setenv INFILE3 $METDIR/METCRO2D_$YY$MM$DD
   setenv INFILE4

  #> Executable call:
   ${BINDIR}/${EXEC}

  #> Increment both Gregorian and Julian Days
   set TODAYG = `date -ud "${TODAYG}+1days" +%Y-%m-%d` #> Add a day for tomorrow
   set TODAYJ = `date -ud "${TODAYG}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

 end #Loop to the next Simulation Day

 
 exit()
