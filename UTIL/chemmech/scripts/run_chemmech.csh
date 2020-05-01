#! /bin/csh -f

# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel

 #> Source the config.cmaq file to set the build environment
 cd ../../..
 source ./config_cmaq.csh
       
#> Set General Parameters for Configuring the Simulation
 set VRSN      = v532               #> Code Version

#> CMAQv5.1 Mechanism Options: cb05tucl_ae6_aq cb05tump_ae6_aq cb05e51_ae6_aq saprc07tb_ae6_aq saprc07tc_ae6_aq  saprc07tic_ae6i_aq   racm2_ae6_aq 
 if ( ! $?MECH ) then
    set MECH      = cb6r3_ae7_aq       #> Mechanism ID
 endif
                                                      
#> Set working, input and output directories
 set CHEMMECH_DIR = ${CMAQ_HOME}/UTIL/chemmech
 set WORKDIR =      ${CHEMMECH_DIR}/scripts
 set INPDIR =       ${CHEMMECH_DIR}/input/${MECH}
 setenv OUTDIR      ${CHEMMECH_DIR}/output/${MECH}

#> Set the build directory if this was not set above 
#> (this is where the CMAQ executable is located by default).
 if ( ! $?BINDIR ) then
    setenv BINDIR $WORKDIR/BLD_chemmech_${VRSN}_${compilerString}
 endif

#> Set the name of the executable.
 setenv EXEC CHEMMECH_${VRSN}.exe

#> compile the program if it does not already exist
 if ( ! -e ${BINDIR}/${EXEC} ) then
   cd ${BINDIR}; make clean; make
   if ( ! ( -e ${EXEC} ) ) then
      echo "failed to compile ${BINDIR}/${EXEC}"
      exit 1
   endif
 endif

# =====================================================================
#> CHEMMECH Configuration Options
# =====================================================================

#> option use CMAQ species namelists to determine CGRID species indices
 setenv USE_SPCS_NAMELISTS T

# =====================================================================
#> CHEMMECH Input Files
# =====================================================================

 setenv MECHDEF         $INPDIR/mech_${MECH}.def
 setenv MAPPING_ROUTINE "${BINDIR}/map_chemistry_spc.F90"
 
 setenv gc_matrix_nml $INPDIR/GC_${MECH}.nml
 setenv ae_matrix_nml $INPDIR/AE_${MECH}.nml
 setenv nr_matrix_nml $INPDIR/NR_${MECH}.nml
 setenv tr_matrix_nml $INPDIR/Species_Table_TR_0.nml
 #setenv tr_matrix_nml $INPDIR/trac0/Species_Table_TR_0.nml

 # Check for input folder and if it doesn't exists, try to find it in
 # the Repository
 if ( ! -e $INPDIR ) then
     mkdir -p $INPDIR
     if ( ! -e ${CMAQ_REPO}/CCTM/src/MECHS/${MECH} ) then
        echo "Cannot Find Inputs for CHEMMECH. Please put them in ${INPDIR}"
        exit 1
     endif
     cp -f ${CMAQ_REPO}/CCTM/src/MECHS/${MECH}/* ${INPDIR}/
     cp -f ${CMAQ_REPO}/CCTM/src/MECHS/trac0/* ${INPDIR}/
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
 cp -f $INPDIR/*nml $OUTDIR/

 # Set Path for Output Files
 setenv SPCSDATX         ${OUTDIR}/SPCS.ext # lists species in mechanism
 setenv RXNS_DATA_MODULE ${OUTDIR}/RXNS_DATA_MODULE.F90
 setenv RXNS_FUNC_MODULE ${OUTDIR}/RXNS_FUNC_MODULE.F90
 setenv EQNS_KPP_FILE    ${OUTDIR}/mech_${MECH}.eqn
 setenv SPCS_KPP_FILE    ${OUTDIR}/mech_${MECH}.spc

# =====================================================================
#> Run CHEMMECH 
# =====================================================================

 if( !( -e $BINDIR/$EXEC ) )then
  ls -l $BINDIR/$EXEC
  exit 1
 endif

 $BINDIR/$EXEC

 if( ( -e ${RXNS_DATA_MODULE} ) && ( -e ${RXNS_FUNC_MODULE} ) )then
      echo "output written to ${OUTDIR}"
 else
      echo "failed to create ${RXNS_DATA_MODULE} or ${RXNS_FUNC_MODULE}"
      exit 1
 endif


