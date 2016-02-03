# outck.q

#path of output....
 setenv DIROUT $OUTDIR

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
 setenv S_CGRID          $OUTDIR/$CGRIDfile
 setenv SA_CONC_1       "$OUTDIR/$SA_CONCfile -v"
 setenv SA_CGRID_1      "$OUTDIR/$SA_CGRIDfile -v"
 setenv SA_ACONC_1      "$OUTDIR/$SA_ACONCfile -v"
 setenv SA_DD_1         "$OUTDIR/$SA_DDfile -v"
 setenv SA_WD_1         "$OUTDIR/$SA_WDfile -v"

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
               $SA_CONC_1\
               $SA_CGRID_1\
               $SA_ACONC_1\
               $SA_DD_1\
               $SA_WD_1 )

 unalias rm 
 foreach file ( $flist )
    if ( $file != '-v' ) then
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

