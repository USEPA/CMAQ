#! /bin/csh -f

# Script to run the program CHEMMECH for CMAQv5.2beta

 set Xpath = ../src               #> Executable directory
 set EXEC  = CHEMMECH                  #> Executable name

#> CMAQv5.1 Mechanism Options: cb05tucl_ae6_aq cb05tump_ae6_aq cb05e51_ae6_aq saprc07tb_ae6_aq saprc07tc_ae6_aq  saprc07tic_ae6i_aq   racm2_ae6_aq 
 set Mechanism = racm2_ae6_aq          #> CMAQ mechanism ID

#> option use CMAQ species namelists to determine CGRID species indices
 setenv USE_SPCS_NAMELISTS T

 set Opath = ../output/${Mechanism}   #> Output Directory
 set Mpath = ../input/${Mechanism}  #> Directory with mechanism definitions
 
 setenv MECHDEF  $Mpath/mech.def
 setenv MAPPING_ROUTINE "${Xpath}/map_chemistry_spc.F90"
 
 set NML    = ${Mpath}
 set NML_TR = ../input/trac0
 set GC_NML = $NML/GC_${Mechanism}.nml
 set AE_NML = $NML/AE_${Mechanism}.nml
 set NR_NML = $NML/NR_${Mechanism}.nml

 set TR_NML = $NML_TR/Species_Table_TR_0.nml

setenv gc_matrix_nml $GC_NML
setenv ae_matrix_nml $AE_NML
setenv nr_matrix_nml $NR_NML
setenv tr_matrix_nml $TR_NML

set day = ` date "+%b-%d-%Y" `
  set Opath = ../output/${Mechanism}"-"${day}
  if( ! ( -d $Opath ) ) mkdir -p $Opath

# output files ................................................................

 if( $Opath != ${Mpath} )then
    \cp -f $MECHDEF  $Opath/.
    \cp -f $NML/*nml $Opath/.
 endif

 setenv SPCSDATX    $Opath/SPCS.ext # lists species in mechanism

 setenv RXNS_DATA_MODULE $Opath/RXNS_DATA_MODULE.F90
 setenv RXNS_FUNC_MODULE $Opath/RXNS_FUNC_MODULE.F90
 setenv OUTDIR           $Opath

 set KPP_Out_Path = ${Opath}
 if( ! ( -d ${KPP_Out_Path} ) )mkdir -p ${KPP_Out_Path}

 set KPP_EQN =  mech_${Mechanism}.eqn
 set KPP_SPC =  mech_${Mechanism}.spc

 setenv EQNS_KPP_FILE ${KPP_Out_Path}/${KPP_EQN}
 setenv SPCS_KPP_FILE ${KPP_Out_Path}/${KPP_SPC} 

 if( !( -e $Xpath/$EXEC ) )then
  ls -l $Xpath/$EXEC
  exit()
 endif

 $Xpath/$EXEC

 unset echo 

 if( ( -e ${RXNS_DATA_MODULE} ) && ( -e ${RXNS_FUNC_MODULE} ) )then
      echo "output written to ${Opath}"
 else
      echo "failed to create ${RXNS_DATA_MODULE} or ${RXNS_FUNC_MODULE}"
      exit()
 endif

 exit()

