#! /bin/csh -f
set echo

#scom command defines compiler and libraries
#source ~/scom -d -c ifc

setenv COMPILER PGF90
#define path to mechanism include or module data files
 set mech_archive = /home/hwo/CCTM_git_repository/MECHS
# setenv suffix halogen_update 
 setenv suffix ikx
#setenv APPL cb05tucl_ae6_aq
 setenv APPL cb05e6_ae6_aq
#setenv APPL saprc07tc_ae6_aq
#setenv APPL saprc07tb_ae6_aq
#setenv APPL saprc99_ae6_aq
#setenv APPL racm2_ae6_aq
#setenv GC_INC /home/hwo/cmaq-v5.0/mechanisms/ozone_tracer
#setenv GC_INC /home/hwo/cmaq-v5.0/mechanisms/saprc07tc_xya_isoprene_v5_v50
#setenv GC_INC /home/hwo/cmaq-v5.0/mechanisms/racm2_ae6_aq
#setenv GC_INC ${mech_archive}/${APPL}
#setenv GC_INC /home/hwo/tools/CSQY_table_processor/cb05tucl-halogen_update
#setenv GC_INC /home/hwo/tools/mech_processor/cb05tucl_ae6st_aq
#setenv GC_INC /home/hwo/tools/mech_processor/saprc07tb_ae6_aq
#setenv GC_INC /home/hwo/tools/mech_processor/cb05tucl-chlorine_update
#setenv GC_INC /home/hwo/tools/mech_processor/cb6_setup
 setenv GC_INC /home/hwo/tools/CSQY_table_processor/CB05e6-ikx
 
#Whether to include spectral values of refractive indices for aerosol species [T|Y|F|N]
setenv WVL_AE_REFRAC F

#whether optical and CSQY data written to two separate file
setenv SPLIT_OUTPUT F

#Variables used to name executable, i.e., CSQY_TABLE_PROCESSOR_mechanism
#setenv suffix AE_REFRACT
#setenv APPL cb05tucl-chlorine_update
#setenv APPL  cb6_prototype
#setenv APPL saprc07tc_ae6_aq
#setenv APPL ozone_tracer
 setenv APPL   ${APPL}_${suffix}


set BASE = $cwd
set XBASE = /home/hwo/tools/CSQY_table_processor
#set EXEC = CSQY_TABLE_PROCESSOR_saprc07tc_xya_isoprene_v4
#set EXEC = CSQY_TABLE_PROCESSOR_racm2_ae6_aq
#set EXEC = CSQY_TABLE_PROCESSOR_saprc07tc_xya_isoprene_v5
#set EXEC = CSQY_TABLE_PROCESSOR_cb05tucl_ae6_aq
#set EXEC = CSQY_TABLE_PROCESSOR_saprc07tc_ae6_aq
#set EXEC = CSQY_TABLE_PROCESSOR_cb05tucl_ae6_aq_AE_REFRACT
#set EXEC = CSQY_TABLE_PROCESSOR_saprc07tc_ae6_aq_AE_REFRACT
# set EXEC = CSQY_TABLE_PROCESSOR_cb05tucl_ae6_aq_v501
# set EXEC = CSQY_TABLE_PROCESSOR_cb05tump_ae6_aq_v501
 set EXEC = CSQY_TABLE_PROCESSOR_${APPL}

#create executable
 cd BLD ; make clean; make -f dumb.makefile; cd ..
#cd BLD ; make -f Makefile; cd ..

#set OUTDIR = ${BASE}/CSQY_TABLE_nlcd40_27wvbins
#set OUTDIR = ${BASE}/CSQY_TABLE_nlcd40-v501
 set OUTDIR = ${BASE}/CSQY_TABLE_${APPL}
#set OUTDIR = ${GC_INC}
if( ! ( -d $OUTDIR ) )mkdir $OUTDIR

cp -f ${OUTDIR}"/PHOT_OPTICS_DATA"  ${BASE}/OPTICS_DATA 
cp -f ${OUTDIR}/CSQY_DATA_*_aq ${BASE}/CSQY_DATA 

#set CSQY_DIR = /home/hutzellb/cmaq_toxics/data/photolysis_CSQY_data-new_names
set CSQY_DIR    = /home/hwo/jproc/photolysis_CSQY_data
set REFRACT_DIR = ${BASE}/water_clouds
set WVBIN_DIR = ${BASE}/flux_data

ln -s -f ${CSQY_DIR}  CSQY_DATA_RAW
ln -s -f $WVBIN_DIR/wavel-bins.dat  WVBIN_FILE
ln -s -f $WVBIN_DIR/solar-p05nm-UCI.dat  FLUX_FILE
ln -s -f $OUTDIR OUT_DIR

#define files for aerosol refractive index
ln -s -f $REFRACT_DIR/water_refractive_index.dat WATER
ln -s -f $REFRACT_DIR/inso00  INSOLUBLE
ln -s -f $REFRACT_DIR/inso00  DUST
ln -s -f $REFRACT_DIR/waso00 SOLUTE
#ln -s -f $REFRACT_DIR/soot00 SOOT
ln -s -f $REFRACT_DIR/soot00-two_way-Oct_21_2012 SOOT
ln -s -f $REFRACT_DIR/ssam00 SEASALT

# cd $OUTDIR

if( ! ( -e  ${XBASE}/${EXEC} ) )then
     \ls ${XBASE}/${EXEC}
     echo "make failed or value of XBASE incorrect"
     exit()
endif

 ${XBASE}/${EXEC}

 \rm -f CSQY_DATA_RAW
 \rm -f WVBIN_FILE
 \rm -f FLUX_FILE
 \rm -f OUT_DIR

 \rm -f  WATER
 \rm -f  INSOLUBLE
 \rm -f  SOLUTE
 \rm -f  SOOT
 \rm -f  SEASALT
 \rm -f  DUST
 \rm -f ${OUTDIR}/${EXEC}
#\rm -f  OPTICS_DATA 
#\rm -f  CSQY_DATA 
 
 cd $BASE


exit()
