# in_out.q

# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header: /project/work/rep/scripts/SCRIPTS/src/cctm/in_out.q,v 1.9 2008/09/17 15:56:58 sjr Exp $ 

# what(1) key, module and SID; SCCS file; date and time of last delta:
# %W% %P% %G% %U%

# for supplementary emissions file ...

 if ( $?EMISpath2 ) then
    setenv EMIS_SUP          $EMISpath2/$EMISfile2
    if ( ! (-e $EMIS_SUP) ) then
       echo " $EMIS_SUP not used "
       endif
    endif

# for Tracer Dep vel and Emissions

 if ( $?TR_DVpath ) then
    setenv DEPV_TRAC_1          $TR_DVpath/$TR_DVfile
    if (! (-e $DEPV_TRAC_1) ) then
       echo " $DEPV_TRAC_1 not found "
       exit 1
       endif
    endif

 if ( $?TR_EMpath ) then
    setenv EMIS_TRAC_1          $TR_EMpath/$TR_EMfile
    if (! (-e $EMIS_TRAC_1) ) then
       echo " $EMIS_TRAC_1 not found "
       exit 1
       endif
    endif

# for OCEAN file ...

 if ( $?OCEANpath ) then
    setenv      OCEAN_1      $OCEANpath/$OCEANfile
    if ( ! (-e $OCEAN_1) ) then
       echo  " $OCEAN_1 not found "
       exit 1
       endif
    endif

# for emissions file ...

 setenv     EMIS_1          $EMISpath/$EMISfile

# for ICBC files ...

 setenv     INIT_GASC_1     $GC_ICpath/$GC_ICfile
 setenv     BNDY_GASC_1     $GC_BCpath/$GC_BCfile
 setenv     INIT_AERO_1     $AE_ICpath/$AE_ICfile
 setenv     BNDY_AERO_1     $AE_BCpath/$AE_BCfile
 setenv     INIT_NONR_1     $NR_ICpath/$NR_ICfile
 setenv     BNDY_NONR_1     $NR_BCpath/$NR_BCfile
 setenv     INIT_TRAC_1     $TR_ICpath/$TR_ICfile
 setenv     BNDY_TRAC_1     $TR_BCpath/$TR_BCfile

# for meteorological files ...

 setenv     GRID_DOT_2D     $METpath/$GD2file
 setenv     GRID_CRO_2D     $METpath/$GC2file
#setenv     GRID_BDY_2D     $METpath/$GB2file
#setenv     GRID_DOT_3D     $METpath/$GD3file
#setenv     GRID_CRO_3D     $METpath/$GC3file
#setenv     GRID_BDY_3D     $METpath/$GB3file
 setenv     MET_CRO_2D      $METpath/$MC2file
#setenv     MET_BDY_2D      $METpath/$MB2file
 setenv     MET_DOT_3D      $METpath/$MD3file
 setenv     MET_CRO_3D      $METpath/$MC3file
 setenv     MET_BDY_3D      $METpath/$MB3file

# for layer definition file (use the MET_CRO_3D file)...

 setenv     LAYER_FILE      $METpath/$MC3file

# for J-values file ...

 setenv     XJ_DATA         $JVALpath/$JVALfile

#              $GRID_BDY_2D
#              $MET_BDY_2D

 set flist = ( $EMIS_1\
               $INIT_GASC_1\
               $BNDY_GASC_1\
               $INIT_AERO_1\
               $BNDY_AERO_1\
               $INIT_NONR_1\
               $BNDY_NONR_1\
               $INIT_TRAC_1\
               $BNDY_TRAC_1\
               $GRID_DOT_2D\
               $GRID_CRO_2D\
               $MET_CRO_2D\
               $MET_DOT_3D\
               $MET_CRO_3D\
               $MET_BDY_3D\
               $LAYER_FILE\
               $XJ_DATA )
 foreach file ( $flist )
    if (! (-e $file) ) then
       echo " $file not found "
       exit 1
       endif
    end

# out.q
# action if the output files already exist ...
 setenv CTM_CONC_1      "$OUTDIR/$CONCfile -v"
 setenv A_CONC_1        "$OUTDIR/$ACONCfile -v"
 setenv S_CGRID         "$OUTDIR/$CGRIDfile -v"
 setenv CTM_DRY_DEP_1   "$OUTDIR/$DD1file -v"
 setenv CTM_WET_DEP_1   "$OUTDIR/$WD1file -v"
 setenv CTM_WET_DEP_2   "$OUTDIR/$WD2file -v"
 setenv CTM_SSEMIS_1    "$OUTDIR/$SS1file -v"
 setenv CTM_VIS_1       "$OUTDIR/$AV1file -v"
 setenv CTM_DIAM_1      "$OUTDIR/$AD1file -v"
 setenv CTM_IPR_1       "$OUTDIR/$PA1file -v"
 setenv CTM_IPR_2       "$OUTDIR/$PA2file -v"
 setenv CTM_IPR_3       "$OUTDIR/$PA3file -v"
 setenv CTM_IRR_1       "$OUTDIR/$IRR1file -v"
 setenv CTM_IRR_2       "$OUTDIR/$IRR2file -v"
 setenv CTM_IRR_3       "$OUTDIR/$IRR3file -v"
 setenv CTM_RJ_1        "$OUTDIR/$RJ1file -v"
 setenv CTM_RJ_2        "$OUTDIR/$RJ2file -v"

 set flist = ( $CTM_CONC_1\
               $A_CONC_1\
               $CTM_DRY_DEP_1\
               $CTM_WET_DEP_1\
               $CTM_WET_DEP_2\
               $CTM_SSEMIS_1\
               $CTM_VIS_1\
               $CTM_DIAM_1\
               $CTM_IPR_1\
               $CTM_IPR_2\
               $CTM_IPR_3\
               $CTM_IRR_1\
               $CTM_IRR_2\
               $CTM_IRR_3\
               $CTM_RJ_1\
               $CTM_RJ_2 )

#              $S_CGRID\#  <- don't put in possible remove list
 echo " Updating $S_CGRID"

 unalias rm 
 foreach file ( $flist )
    if ( $file != '-v' ) then
       if ( -e $file ) then
          echo " $file already exists"
          if ( $DISP == 'delete' ) then
             echo " Deleting $file"
             rm $file
             else if ( $DISP == 'update' ) then
             echo " Updating $file"
             else
             echo "*** RUN ABORTED ***"
             exit 1
             endif
          endif
       endif
    end
