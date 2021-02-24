#! /bin/csh -f

# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel

#> Check that the host system is Linux-based
 set BLD_OS = `uname -s`
 if ($BLD_OS != 'Linux') then
    echo "   $BLD_OS -> wrong bldit script for host!"
    exit 1
 endif

#> Set Compiler Identity by User Input: Options -> intel | pgi | gcc
 if ( $#argv == 1 ) then
   setenv compiler $argv[1]
   setenv compilerVrsn Empty
 else if ( $#argv == 2 ) then
   #> Compiler Name and Version have been provided
   setenv compiler $1
   setenv compilerVrsn $2
 else
   #> use default compiler and version
   setenv compiler intel
   setenv compilerVrsn Empty
   echo "compiler and version not set"
   echo "usage: $0 <compiler>"
   echo "setting compiler to intel"
 endif

#> Source the config.cmaq file to set the build environment
 if( -e ../../../config_cmaq.csh )then
    cd ../../..
    source ./config_cmaq.csh
 else
#work offline from CMAQ repository and build environment
    setenv offline "Y"
    setenv compilerString ${compiler}
    setenv CMAQ_HOME $cwd/..
    setenv CMAQ_REPO ${CMAQ_HOME}
 endif
 set VRSN      = v532               #> Code Version

#> CMAQv5.1 Mechanism Options: cb05tucl_ae6_aq cb05tump_ae6_aq cb05e51_ae6_aq saprc07tb_ae6_aq saprc07tc_ae6_aq  saprc07tic_ae6i_aq   racm2_ae6_aq 
 if ( ! $?MECH ) then
    set MECH      = cb6r3_ae7_aq       #> Mechanism ID
 endif
 setenv CLEAR "TRUE" #> over-write existing output files
                                                      
#> Set working, input and output directories
 echo ${CMAQ_HOME}
 if( ! ( $?offline ) )then
   set CHEMMECH_DIR = ${CMAQ_HOME}/UTIL/chemmech
 else
   set CHEMMECH_DIR = ${CMAQ_HOME}
 endif
 
 set WORKDIR = ${CHEMMECH_DIR}/scripts
 if ( ! $?CHEMMECH_INPUT ) then
   set CHEMMECH_INPUT =  ${CHEMMECH_DIR}/input/${MECH}
 endif
 if ( ! $?TRAC_NML ) then
    set TRAC_NML  = ${CHEMMECH_INPUT}/Species_Table_TR_0.nml #> Tracer namelist ID
 endif
 if ( ! $?OUTDIR ) then
   setenv OUTDIR ${CHEMMECH_DIR}/output/${MECH}
 endif

#> Set the build directory 
#> where the CMAQ executable is located by default
 if ( ! $?BINDIR ) then
   setenv BINDIR $WORKDIR/BLD_chemmech_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 setenv EXEC CHEMMECH_${VRSN}.exe

#> compile the program if it does not already exist
 if ( ! -e ${BINDIR}/${EXEC} ) then
   if ( -d ${BINDIR} ) then
      cd ${BINDIR}; make clean; make; cd -
      if ( ! ( -e ${BINDIR}/${EXEC} ) ) then
          echo "failed to compile ${BINDIR}/${EXEC}"
          exit 1
      endif
   else
      if ( ! ( -e ${BINDIR}/${EXEC} ) ) then
         echo "Directory chemmech executable: ${BINDIR}"
         echo "does not exist. Run its build script."
      else
         echo "Directory chemmech executable: ${BINDIR}"
         echo "exists but is not a directory."
         echo "Run its build script to replace it."
      endif
      exit(1)
   endif 
 endif

# =====================================================================
#> CHEMMECH Configuration Options
# =====================================================================

#> option use CMAQ species namelists to determine CGRID species indices
 setenv USE_SPCS_NAMELISTS T
#> get name of user to tag html output file
 setenv NAME ` getent passwd $USER | cut -d : -f 5 | cut -d ";"  -f 1 `

# =====================================================================
#> CHEMMECH Input Files
# =====================================================================

 setenv MECHDEF         ${CHEMMECH_INPUT}/mech_${MECH}.def
 setenv MAPPING_ROUTINE "${BINDIR}/map_chemistry_spc.F90"
 
 setenv gc_matrix_nml ${CHEMMECH_INPUT}/GC_${MECH}.nml
 setenv ae_matrix_nml ${CHEMMECH_INPUT}/AE_${MECH}.nml
 setenv nr_matrix_nml ${CHEMMECH_INPUT}/NR_${MECH}.nml
 setenv tr_matrix_nml ${TRAC_NML}
 #setenv tr_matrix_nml ${CHEMMECH_INPUT}/trac0/Species_Table_TR_0.nml


#Check if input files exist
 set input_files = ( ${MECHDEF} ${MAPPING_ROUTINE} ${gc_matrix_nml} ${nr_matrix_nml} ${tr_matrix_nml} )
 foreach file ( ${input_files} )
   if( ! ( -e $file ) )then
     setenv missing_file "Y"
     echo "Input file: ${file} does not exist"
   endif
 end
 if( $?missing_file )then
    echo "Execution failed for the above cause(s)"
    exit 1
 endif

# =====================================================================
#> CHEMMECH Output Files
# =====================================================================

 # Create Output Folder if it doesn't exist already
 if ( ! -e $OUTDIR ) then
     mkdir -p $OUTDIR
 endif

 # Copy input files to Output Directory
 cp -f $MECHDEF $OUTDIR/
 cp -f ${CHEMMECH_INPUT}/*nml $OUTDIR/

 # Set Path for Output Files
 setenv SPCSDATX         ${OUTDIR}/SPCS.ext # lists species in mechanism
 setenv RXNS_DATA_MODULE ${OUTDIR}/RXNS_DATA_MODULE.F90
 setenv RXNS_FUNC_MODULE ${OUTDIR}/RXNS_FUNC_MODULE.F90
 setenv EQNS_KPP_FILE    ${OUTDIR}/mech_${MECH}.eqn
 setenv SPCS_KPP_FILE    ${OUTDIR}/mech_${MECH}.spc

 if( -e ${RXNS_DATA_MODULE} && -e ${RXNS_FUNC_MODULE} )then
   if( $CLEAR == "FALSE")then
      echo "Previous output exists; set CLEAR to TRUE to delete"
      exit(1)
   endif
 endif
# =====================================================================
#> Run CHEMMECH 
# =====================================================================

 if( !( -e $BINDIR/$EXEC ) )then
  ls -l $BINDIR/$EXEC
  exit 1
 endif

 $BINDIR/$EXEC
 if ( $? != 0 ) then
    echo "CHEMMECH ($BINDIR/$EXEC) failed for some reason. Halt Build Process!"
    exit 1
 endif

 if( ( -e ${RXNS_DATA_MODULE} ) && ( -e ${RXNS_FUNC_MODULE} ) )then
      echo "output written to ${OUTDIR}"
 else
      echo "failed to create ${RXNS_DATA_MODULE} or ${RXNS_FUNC_MODULE}"
      exit 1
 endif

exit(0)
