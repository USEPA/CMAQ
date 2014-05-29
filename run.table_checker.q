#! /bin/csh -f
set echo

#scom command defines compiler and libraries
#source ~/scom -d -c ifc

#define path to mechanism include or module data files
 set mech_archive = /home/hwo/CCTM_git_repository/MECHS
 setenv suffix v503
# setenv mechanism cb05tucl_ae6_aq
# setenv mechanism saprc07tc_ae6_aq
# setenv mechanism saprc99_ae6_aq
 setenv mechanism racm2_ae6_aq
#setenv GC_INC /home/hwo/cmaq-v5.0/mechanisms/ozone_tracer
#setenv GC_INC /home/hwo/cmaq-v5.0/mechanisms/saprc07tc_xya_isoprene_v5_v50
#setenv GC_INC /home/hwo/cmaq-v5.0/mechanisms/racm2_ae6_aq
#setenv GC_INC /home/hwo/tools/mech_processor/cb05tucl-chlorine_update
#setenv GC_INC /home/hwo/tools/mech_processor/cb6_setup
 setenv GC_INC ${mech_archive}/${mechanism}

#Whether to include spectral values of refractive indices for aerosol species [T|Y|F|N]
setenv WVL_AE_REFRAC T

#Variables used to name executable, i.e., CSQY_TABLE_PROCESSOR_mechanism
#setenv suffix AE_REFRACT
#setenv APPL cb05tucl-chlorine_update
#setenv APPL  cb6_prototype
#setenv APPL saprc07tc_ae6_aq
#setenv APPL ozone_tracer
 setenv APPL   ${mechanism}_${suffix}


set BASE = $cwd
set XBASE = $BASE

set EXEC = CSQY_TABLE_CHECKER_${APPL}

#create executable
 cd ${BASE}/Bld_check_outputs ; make clean; make -f dumb.makefile; cd ${BASE}

 set INDIR = ${BASE}/CSQY_TABLE_${APPL}
#set OUTDIR = ${GC_INC}
if( ! ( -d $INDIR ) )then
    ls $INDIR
    exit()
endif

setenv  OPTICS_DATA ${INDIR}"/PHOT_OPTICS_DATA"
setenv  CSQY_DATA   ${INDIR}"/CSQY_DATA_"${mechanism}


if( ! ( -e  ${XBASE}/${EXEC} ) )then
     \ls ${XBASE}/${EXEC}
     echo "make failed or value of XBASE incorrect"
     exit()
endif

 ${XBASE}/${EXEC}

#\rm -f  OPTICS_DATA 
#\rm -f  CSQY_DATA 
 
 cd $BASE


exit()
