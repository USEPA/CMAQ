#!/bin/csh

#
# script to compile and run CMAQ system executables for three compilers in standard and debug mode
#
# the script changes into the various "scripts" directories, loops over compilers, makes minor
# modifications to the bldit and run scripts in each directory, and optionally submits the
# CCTM run scripts via sbatch
#
# If the user would like to build and/or run with non-default settings in the bldit and/or run scripts
# for the selected case, cd to CCTM/scripts and edit these files before invoking this script
#
# usage: ./developer_compile_execute_masterscript.csh > & ! developer_compile_execute_masterscript.log
#
# Revision History: 
# 
# 03/2019 C. Hogrefe : Created Script
# 08/2019 F. Sidi & B. Hutzell    : Updated Domain names, added CLOBBER, Mechanism, Chemistry Solver Options, 
#                                   flexibility to change the number of compilers, added make.it script in each CCTM 
#                                   build directory to ensure consistent environment when recompiling code during development
# 12/2019 F. Sidi                 : Updated VRSN to 5.3.1
# 01/2020 F. Sidi & B. Hutzell    : Updated make.it to workoff Makefile existing instead of *.exe

set VRSN = v532

set CASE =  Bench_2016_12SE1 #currently 2015_HEMI, 2010_4CALIF1, 2011_12US1, 2014_12US1, 2016_12US1, Bench_2011_12SE1

set CLOBBER = FALSE #TRUE 

set Mechanism = cb6r3_ae7_aq #cb6r3m_ae7_kmtbr 

set Chem_Solver = ebi_${Mechanism} #smvgear,ros3 


set compiler         = ("intel" "pgi" "gcc")
set compiler_version = (18.0    17.4   6.1)

if( $#compiler != $#compiler_version )then
   echo 'Error number of compilers and compiler_versions do not equal'
   exit()
endif

set COMPILE_PREP = Y
set COMPILE_CCTM = Y
set COMPILE_POST = Y

set RUN_PREP = N #PLACEHOLDER, NOT IMPLEMENTED
set RUN_CCTM = Y 
set RUN_POST = N #PLACEHOLDER, NOT IMPLEMENTED

set SLURM_GID = mod3eval
#set SLURM_GID = mod3dev

#
# no edits should be needed below this point
#

#
# PREP
#

if ( $COMPILE_PREP == 'Y' ) then

 echo '##### COMPILING ICON AND BCON #####'

 set CMAQ_HOME = $cwd

 set prog    = (BCON ICON)
 set prog_lc = (bcon icon)
 
 foreach n (1 2)

  cd ${CMAQ_HOME}/PREP/${prog_lc[$n]}/scripts
  @ ccount = 1 # compiler counter for versioning
  foreach i ($compiler)

   echo ' '
   
   #default no debug

   ./bldit_${prog_lc[$n]}.csh ${i} ${compiler_version[$ccount]} >&! ./bldit_${prog_lc[$n]}_${i}${compiler_version[$ccount]}.log

   if (-e BLD_${prog[$n]}_${VRSN}_${i}${compiler_version[$ccount]}/${prog[$n]}_${VRSN}.exe) then

    echo 'Successful compile for ' ${prog[$n]} ' ' ${i} ' no debug'
 
   else

    echo '***** Compile error for ' ${prog[$n]} ' ' ${i} ' no debug'
    echo '***** check for errors in' PREP/${prog_lc[$n]}/scripts/bldit_${prog_lc[$n]}_${i}${compiler_version[$ccount]}.log

   endif

   # debug

   sed 's/$Blder $Cfile/$Blder -debug_cctm $Cfile/g' ./bldit_${prog_lc[$n]}.csh >! ./dummy.csh
   sed 's/${VRSN}_${compilerString}/${VRSN}_${compilerString}_debug/g' ./dummy.csh >! ./bldit_${prog_lc[$n]}_debug.csh
   chmod u+x ./bldit_${prog_lc[$n]}_debug.csh
   'rm' ./dummy.csh

   ./bldit_${prog_lc[$n]}_debug.csh ${i} ${compiler_version[$ccount]} >&! ./bldit_${prog_lc[$n]}_${i}${compiler_version[$ccount]}_debug.log

   if (-e BLD_${prog[$n]}_${VRSN}_${i}${compiler_version[$ccount]}_debug/${prog[$n]}_${VRSN}.exe) then

    echo 'Successful compile for ' ${prog[$n]} ' ' ${i} ' debug'
 
   else

    echo '***** Compile error for ' {prog[$n]} ' ' ${i} ' debug'
    echo '***** check for errors in' PREP/${prog_lc[$n]}/scripts/bldit_${prog_lc[$n]}_${i}${compiler_version[$ccount]}_debug.log

   endif
   
   @ ccount++ # add one to compiler count
  end

  cd ${CMAQ_HOME}

 end
 
endif

#
# CCTM
#

set icount = 0
@ ccount = 1 # reset compiler count to 1 
set jobid = (-9 -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 )

foreach i ($compiler)

echo ' '

if ( $COMPILE_CCTM == 'Y' ) then

 echo ' '
 echo '##### COMPILING CCTM #####'

 set CMAQ_HOME = $cwd

 foreach prog (cctm)

  cd ${CMAQ_HOME}/CCTM/scripts

   
   # default no debug
    
    sed 's/set Mechanism = cb6r3_ae7_aq/set Mechanism = '${Mechanism}'/' ./bldit_${prog}.csh >! ./dummy.csh # Editing File to change the mechanism 
    sed 's/gas\/ebi_\${Mechanism}/gas\/'${Chem_Solver}'/' ./dummy.csh >! ./bldit_${prog}_mod.csh            # Editing File to change Chemistry Solver
    chmod u+x ./bldit_${prog}_mod.csh
   'rm' ./dummy.csh

   ./bldit_${prog}_mod.csh ${i} ${compiler_version[$ccount]} >&! ./bldit_${prog}_mod_${i}${compiler_version[$ccount]}.log

   if (-e BLD_CCTM_${VRSN}_${i}${compiler_version[$ccount]}/Makefile ) then
    set make_it = "BLD_CCTM_"${VRSN}"_"${i}${compiler_version[$ccount]}"/make.it"
    echo "#! /bin/csh -f" >! ${make_it}
    echo " "              >> ${make_it}
    echo "source ../../../config_cmaq.csh "${i}" "${compiler_version[$ccount]}  >> ${make_it}
    echo "setenv debug false"                                         >> ${make_it}
    echo 'if ( $#argv == 1 )then'                                     >> ${make_it}
    echo '   if ( $1  == "clean" )make clean'                         >> ${make_it}
    echo "endif"                                                      >> ${make_it}
    echo "make -j"                                                    >> ${make_it}
    echo "unsetenv debug"                                             >> ${make_it}
    echo "unsetenv compiler"                                          >> ${make_it}
    echo "unsetenv compilerVrsn"                                      >> ${make_it}
    echo 'exit()'         >> ${make_it}
    chmod +x  ${make_it}
   else
     echo "**** bldmake failed to create Makefile ****"
   endif 

   if (-e BLD_CCTM_${VRSN}_${i}${compiler_version[$ccount]}/CCTM_${VRSN}.exe) then
     
     echo 'Successful compile for ' ${prog} ' ' ${i} ' no debug'
   
   else 
     
     echo '***** Compile error for ' ${prog} ' ' ${i} ' no debug'
     echo '***** check for errors in' CCTM/scripts/bldit_${prog}_${i}${compiler_version[$ccount]}.log
   
   endif

   # debug
   
   sed 's/#set Debug_CCTM/set Debug_CCTM/g' ./bldit_${prog}.csh >! ./dummy.csh
   sed -i 's/set Mechanism = cb6r3_ae7_aq/set Mechanism = '${Mechanism}'/' dummy.csh # Editing File to change the mechanism 
   sed -i 's/gas\/ebi_\${Mechanism}/gas\/'${Chem_Solver}'/' dummy.csh                # Editing File to change Chemistry Solver
   sed 's/${VRSN}_${compilerString}/${VRSN}_${compilerString}_debug/g' ./dummy.csh >! ./bldit_${prog}_debug.csh
   chmod u+x ./bldit_${prog}_debug.csh
   'rm' ./dummy.csh

   ./bldit_${prog}_debug.csh ${i} ${compiler_version[$ccount]} >&! ./bldit_${prog}_${i}${compiler_version[$ccount]}_debug.log

   if (-e BLD_CCTM_${VRSN}_${i}${compiler_version[$ccount]}_debug/Makefile ) then
    set make_it = "BLD_CCTM_"${VRSN}"_"${i}${compiler_version[$ccount]}"_debug/make.it"
    echo "#! /bin/csh -f" >! ${make_it}
    echo " "              >> ${make_it}
    echo "source ../../../config_cmaq.csh "${i}" "${compiler_version[$ccount]}  >> ${make_it}
    echo "setenv debug true"                                          >> ${make_it}
    echo 'if ( $#argv == 1 )then'                                     >> ${make_it}
    echo '   if ( $1  == "clean" )make clean'                         >> ${make_it}
    echo "endif"                                                      >> ${make_it}
    echo "make -j"                                                    >> ${make_it}
    echo "unsetenv debug"                                             >> ${make_it}
    echo "unsetenv compiler"                                          >> ${make_it}
    echo "unsetenv compilerVrsn"                                      >> ${make_it}
    echo 'exit()'         >> ${make_it}
    chmod +x ${make_it}
   else
    echo "**** bldmake failed to create Makefile ****"
   endif
   
   if (-e BLD_CCTM_${VRSN}_${i}${compiler_version[$ccount]}_debug/CCTM_${VRSN}.exe) then
   
    echo 'Successful compile for ' ${prog} ' ' ${i} ' debug'
   
   else
    
    echo '***** Compile error for ' ${prog} ' ' ${i} ' debug'
    echo '***** check for errors in' CCTM/scripts/bldit_${prog}_${i}${compiler_version[$ccount]}_debug.log
   
   endif

  cd ${CMAQ_HOME}

 end

endif

if ( $RUN_CCTM == 'Y' ) then

 set CMAQ_HOME = $cwd

 foreach prog (cctm)

   cd ${CMAQ_HOME}/CCTM/scripts

   if (-e BLD_CCTM_${VRSN}_${i}${compiler_version[$ccount]}/CCTM_${VRSN}.exe) then

     # submit no debug job

     @ icount = ${icount} + 1
 
     sed 's/#SBATCH --gid=mod3eval/#SBATCH --gid='${SLURM_GID}'/g' ./run_${prog}_${CASE}.csh >! ./dummy1.csh
     sed -i 's/set CLOBBER_DATA = FALSE/set CLOBBER_DATA = '${CLOBBER}'/' dummy1.csh # Editing File to set clobber
     sed -i 's/cb6r3_ae7_aq/'${Mechanism}'/' dummy1.csh # Editing File to set mechanism
     sed -i 's/#SBATCH -J CMAQ_Bench/#SBATCH -J CMAQ_'${i}'/' dummy1.csh # Editing File to set job name
     sed 's/#SBATCH -A mod3eval/#SBATCH -A '${SLURM_GID}'/g' ./dummy1.csh >! ./dummy2.csh
     sed 's/%j.txt/%j_'${CASE}'_'${i}${compiler_version[$ccount]}'.txt/g' ./dummy2.csh >! ./dummy3.csh
     sed 's/setenv compiler intel/setenv compiler '${i}'/g' ./dummy3.csh >! ./dummy4.csh
     sed 's/setenv compilerVrsn Empty/setenv compilerVrsn '${compiler_version[$ccount]}'/g' ./dummy4.csh >! ./run_${prog}_${CASE}_${i}${compiler_version[$ccount]}.csh
     chmod u+x ./run_${prog}_${CASE}_${i}${compiler_version[$ccount]}.csh
     'rm' ./dummy*.csh
     set sbatch_string = `sbatch ./run_${prog}_${CASE}_${i}${compiler_version[$ccount]}.csh`
     echo $sbatch_string
     set jobid[$icount] = `echo ${sbatch_string} | cut -f4 -d' '`

   else

    echo '***** no executable for ' ${prog} ' ' ${i} ' no debug'

   endif

   if (-e BLD_CCTM_${VRSN}_${i}${compiler_version[$ccount]}_debug/CCTM_${VRSN}.exe) then
          
     # submit debug job
     
     @ icount = ${icount} + 1
  
     sed 's/#SBATCH --gid=mod3eval/#SBATCH --gid='${SLURM_GID}'/g' ./run_${prog}_${CASE}.csh >! ./dummy1.csh
     sed -i 's/set CLOBBER_DATA = FALSE/set CLOBBER_DATA = '${CLOBBER}'/' dummy1.csh # Editing File to set clobber
     sed -i 's/cb6r3_ae7_aq/'${Mechanism}'/' dummy1.csh # Editing File to set mechanism
     sed -i 's/#SBATCH -J CMAQ_Bench/#SBATCH -J CMAQ_debug_'${i}'/' dummy1.csh # Editing File to set job name
     sed 's/#SBATCH -A mod3eval/#SBATCH -A '${SLURM_GID}'/g' ./dummy1.csh >! ./dummy2.csh
     sed 's/%j.txt/%j_'${CASE}'_'${i}${compiler_version[$ccount]}'_debug.txt/g' ./dummy2.csh >! ./dummy3.csh
     sed 's/setenv compiler intel/setenv compiler '${i}'/g' ./dummy3.csh >! ./dummy4.csh
     sed 's/setenv compilerVrsn Empty/setenv compilerVrsn '${compiler_version[$ccount]}'/g' ./dummy4.csh >! ./dummy5.csh
     sed 's/${VRSN}_${compilerString}_${APPL}/${VRSN}_${compilerString}_debug_${APPL}/g' ./dummy5.csh >! ./dummy6.csh
     sed 's#${CMAQ_HOME}/CCTM/scripts/BLD_CCTM_${VRSN}_${compilerString}#${CMAQ_HOME}/CCTM/scripts/BLD_CCTM_${VRSN}_${compilerString}_debug#g' ./dummy6.csh >! ./run_${prog}_${CASE}_${i}${compiler_version[$ccount]}_debug.csh
     chmod u+x ./run_${prog}_${CASE}_${i}${compiler_version[$ccount]}_debug.csh
     'rm' ./dummy*.csh
     set sbatch_string = `sbatch ./run_${prog}_${CASE}_${i}${compiler_version[$ccount]}_debug.csh`
     echo $sbatch_string
     set jobid[$icount] = `echo ${sbatch_string} | cut -f4 -d' '`

   else

    echo '***** no executable for ' ${prog} ' ' ${i} ' debug'

   endif

  cd ${CMAQ_HOME}

 end

endif
@ ccount++  # add one to compiler count

end

#
#echo $jobid
#
#the jobid array harvested when submitting the six CCTM jobs could
#be used for submitting subsequent sbatch jobs (e.g. to process 
#timing results, create automated plots, etc.) that would be executed
#once a given CCTM job finishes.
#The sbatch syntax for submitting such a hypothetical timing processing
#script would be something along these lines:
#sbatch --dependency=after:jobid[1] process_timing.csh 
#

#
# POST
#
#
# for debug mode compile, replace "$Blder $Cfile" with "$Blder -debug_cctm $Cfile"
# and "${VRSN}_${compilerString}" with "${VRSN}_${compilerString}_debug"
#

if ( $COMPILE_POST == 'Y' ) then

 echo ' '
 echo '##### COMPILING POSTPROCESSING TOOLS #####'

 set CMAQ_HOME = $cwd

 foreach prog (appendwrf  bldoverlay  block_extract  calc_tmetric  combine  hr2day  sitecmp  sitecmp_dailyo3  writesite)
  
  @ ccount=1 # reset compiler count 

  cd ${CMAQ_HOME}/POST/${prog}/scripts

  foreach i ($compiler)

   echo ' '
   
   # default no debug

   ./bldit_${prog}.csh ${i} ${compiler_version[$ccount]} >&! ./bldit_${prog}_${i}${compiler_version[$ccount]}.log

   if (-e BLD_${prog}_${VRSN}_${i}${compiler_version[$ccount]}/${prog}_${VRSN}.exe) then

    echo 'Successful compile for ' ${prog} ' ' ${i} ' no debug'
 
   else

    echo '***** Compile error for ' ${prog} ' ' ${i} ' no debug'
    echo '***** check for errors in' POST/${prog}/scripts/bldit_${prog}_${i}${compiler_version[$ccount]}.log

   endif

   # debug

   sed 's/$Blder $Cfile/$Blder -debug_cctm $Cfile/g' ./bldit_${prog}.csh >! ./dummy.csh
   sed 's/${VRSN}_${compilerString}/${VRSN}_${compilerString}_debug/g' ./dummy.csh >! ./bldit_${prog}_debug.csh
   chmod u+x ./bldit_${prog}_debug.csh
   'rm' ./dummy.csh

   ./bldit_${prog}_debug.csh ${i} ${compiler_version[$ccount]} >&! ./bldit_${prog}_${i}${compiler_version[$ccount]}_debug.log

   if (-e BLD_${prog}_${VRSN}_${i}${compiler_version[$ccount]}_debug/${prog}_${VRSN}.exe) then

    echo 'Successful compile for ' ${prog} ' ' ${i} ' debug'
 
   else

    echo '***** Compile error for ' ${prog} ' ' ${i} ' debug'
    echo '***** check for errors in' POST/${prog}/scripts/bldit_${prog}_${i}${compiler_version[$ccount]}_debug.log

   endif
   @ ccount++ # add one to compiler count
  end

 cd ${CMAQ_HOME}

 end

endif
