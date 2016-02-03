# outck.q

# DDM input files
if ( $?SN_INpath ) then
   setenv     SEN_INPUT     $SN_INpath/$SN_INfile
   if (! (-e $SEN_INPUT) ) then
      echo " $SEN_INPUT not found "
      exit 1
   endif
endif
 
# for regions file

if ( $?DDM3D_RGN ) then
   if ( $DDM3D_RGN == 'Y' ) then
      setenv     REGIONS_1    $REGNpath/$REGNfile
      if (! (-e $REGIONS_1) ) then
         echo " $REGIONS_1 not found "
         exit 1
      endif
   endif
endif
 
# for sensitivity IC files (not always there, so no existance check - RISKY!!!)
 
set SAE_ICpath = $SGC_ICpath
set SNR_ICpath = $SGC_ICpath
set SAE_ICfile = $SGC_ICfile
set SNR_ICfile = $SGC_ICfile
setenv     INIT_GASC_S     $SGC_ICpath/$SGC_ICfile
setenv     INIT_AERO_S     $SAE_ICpath/$SAE_ICfile
setenv     INIT_NONR_S     $SNR_ICpath/$SNR_ICfile
 
# for sensitivity BC files (not always there, so no existance check - RISKY!!!)
 
set SAE_BCpath = $SGC_BCpath
set SNR_BCpath = $SGC_BCpath
set SAE_BCfile = $SGC_BCfile
set SNR_BCfile = $SGC_BCfile
setenv     BNDY_GASC_S     $SGC_BCpath/$SGC_BCfile
setenv     BNDY_AERO_S     $SAE_BCpath/$SAE_BCfile
setenv     BNDY_NONR_S     $SNR_BCpath/$SNR_BCfile

# action if the output files already exist ...
 setenv CTM_CONC_1      "$OUTDIR/$CONCfile -v"
 setenv A_CONC_1        "$OUTDIR/$ACONCfile -v"
 setenv CTM_DRY_DEP_1   "$OUTDIR/$DD1file -v"
 setenv CTM_DEPV_DIAG   "$OUTDIR/$DV1file -v"
 setenv CTM_PT3D_DIAG   "$OUTDIR/$PT1file -v"
 setenv B3GTS_S         "$OUTDIR/$BIO1file -v"
 setenv SOILOUT         "$OUTDIR/$SOIL1file -v"
 setenv CTM_WET_DEP_1   "$OUTDIR/$WD1file -v"
 setenv CTM_WET_DEP_2   "$OUTDIR/$WD2file -v"
 setenv CTM_VIS_1       "$OUTDIR/$AV1file -v"
 setenv CTM_DIAM_1      "$OUTDIR/$AD1file -v"
 setenv CTM_RJ_1        "$OUTDIR/$RJ1file -v"
 setenv CTM_RJ_2        "$OUTDIR/$RJ2file -v"
 setenv CTM_SSEMIS_1    "$OUTDIR/$SSEfile -v"
 setenv CTM_DUST_EMIS_1 "$OUTDIR/$DSEfile -v"
 setenv CTM_IPR_1       "$OUTDIR/$PA1file -v"
 setenv CTM_IPR_2       "$OUTDIR/$PA2file -v"
 setenv CTM_IPR_3       "$OUTDIR/$PA3file -v"
 setenv CTM_IRR_1       "$OUTDIR/$IRR1file -v"
 setenv CTM_IRR_2       "$OUTDIR/$IRR2file -v"
 setenv CTM_IRR_3       "$OUTDIR/$IRR3file -v"
 setenv CTM_DEPV_FST    "$OUTDIR/$DEPVFSTfile -v"
 setenv CTM_DEPV_MOS    "$OUTDIR/$DEPVMOSfile -v"
 setenv CTM_DRY_DEP_FST "$OUTDIR/$DDFSTfile -v"
 setenv CTM_DRY_DEP_MOS "$OUTDIR/$DDMOSfile -v"
 setenv CTM_SENS_1      "$OUTDIR/$SENSfile -v" 
 setenv A_SENS_1        "$OUTDIR/$ASENSfile -v" 
 setenv CTM_SWETDEP_1   "$OUTDIR/$SWDfile -v" 
 setenv CTM_SDRYDEP_1   "$OUTDIR/$SDDfile -v" 
 setenv S_CGRID         "$OUTDIR/$CGRIDfile -v"

 set flist = ( $CTM_CONC_1\
               $S_CGRID\
               $A_CONC_1\
               $CTM_DRY_DEP_1\
               $CTM_DEPV_DIAG\
               $CTM_PT3D_DIAG\
               $B3GTS_S\
               $SOILOUT\
               $CTM_WET_DEP_1\
               $CTM_WET_DEP_2\
               $CTM_VIS_1\
               $CTM_DIAM_1\
               $CTM_RJ_1\
               $CTM_RJ_2\
               $CTM_SSEMIS_1\
               $CTM_DUST_EMIS_1\
               $CTM_IPR_1\
               $CTM_IPR_2\
               $CTM_IPR_3\
               $CTM_IRR_1\
               $CTM_IRR_2\
               $CTM_IRR_3 \
               $CTM_DEPV_FST \
               $CTM_DEPV_MOS \
               $CTM_DRY_DEP_FST \
               $CTM_DRY_DEP_MOS \
               $CTM_SENS_1 \
               $A_SENS_1 \
               $CTM_SWETDEP_1 \
               $CTM_SDRYDEP_1 )
 unalias rm 
 foreach f ( $flist )
    if ( $f != '-v' ) then
       set file = `echo $f | cut -d' ' -f1`
       if ( -e $file ) then
          echo " $file already exists "
          if ( $DISP == 'delete' ) then
             echo " $file being deleted "
             rm $file
           else if ( $DISP == 'update' ) then
             echo " $file being updated "
           else
             echo "*** RUN ABORTED ***"
             exit 1
           endif
        endif
     endif
  end

