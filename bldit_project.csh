#!/bin/csh -f

# ================== CMAQ5.2 Extraction Script ====================== #
# Requirements: CMAQ git source code repository                       #
#                                                                     #
# To report problems or request help with this script/program:        #
#             http://www.cmascenter.org/help_desk.cfm                 #
# =================================================================== #

#> This script may be executed when first downloading or cloning the CMAQ
#> repository. The routine will copy important script including config.cmaq,
#> bldit.cctm, and run.cctm as well as scripts for other utilities into 
#> a $CMAQ_HOME project directory of the user's choice.
#>
#> Default location for CMAQ model build is one directory above
#> the repository. The user may also set their own preferred 
#> directory.
 set CMAQ_HOME = /home/username/cmaq_project

#> This section allows users to choose explicitly which tools
#> to make available from the repo. For each selected tool,
#> extract_scripts.csh will copy any build and run scritps
#> out of the repo for you. Set each to [Y/N]
 set EXT_CCTM    = Y

 # Pre-processing Tools
 set EXT_AGDUST = Y
 set EXT_BCON = Y
 set EXT_ICON = Y
 set EXT_LTNG = Y
 set EXT_MCIP = Y
 
 # Post-Processing Tools
 set EXT_COMBINE = Y
 set EXT_APPENDWRF = Y
 set EXT_BLDOVERLAY = Y
 set EXT_BLOCK_EXTRACT = Y
 set EXT_HR2DAY = Y
 set EXT_SITECMP = Y
 set EXT_SITECMP_DAILYO3 = Y
 set EXT_WRITESITE = Y


#> model source code repository location 
 set REPO_HOME = $cwd

#> Check that the host system is Linux-based
 set BLD_OS = `uname -s`
 if ($BLD_OS != 'Linux') then
    echo "   $BLD_OS -> wrong bldit script for host!"
    exit 1
 endif

#> Check to make sure $CMAQ_HOME exists
 if ( ! -e "$CMAQ_HOME" ) then
    mkdir -pv $CMAQ_HOME
 else
    if ( ! -d "$CMAQ_HOME" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif

#> Return to repository top-level directory
 cd $REPO_HOME


#===============================================================================
#> Copy config_cmaq.csh to Project directory and insert correct location
#> of the repository these scripts are coming from.
#===============================================================================
 cp config_cmaq.csh $CMAQ_HOME/config_cmaq.csh
 sed -i '/setenv CMAQ_REPO \$CMAQ_HOME/c\ setenv CMAQ_REPO '"$REPO_HOME" $CMAQ_HOME/config_cmaq.csh

#===============================================================================
#> Copy CCTM scripts
#===============================================================================
 if ( $EXT_CCTM == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/CCTM/scripts" ) then
       mkdir -pv $CMAQ_HOME/CCTM/scripts
    endif
    cp CCTM/scripts/bldit_cctm.csh $CMAQ_HOME/CCTM/scripts/bldit_cctm.csh
    cp CCTM/scripts/run_cctm.csh $CMAQ_HOME/CCTM/scripts/run_cctm.csh
 endif

#===============================================================================
#> Copy AGDUST scripts
#===============================================================================
 if ( $EXT_AGDUST == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/PREP/agdust/scripts" ) then
       mkdir -pv $CMAQ_HOME/PREP/agdust/scripts
    endif
    cp PREP/agdust/scripts/run_calmap.csh $CMAQ_HOME/PREP/agdust/scripts/run_calmap.csh
 endif

#===============================================================================
#> Copy BCON scripts
#===============================================================================
 if ( $EXT_BCON == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/PREP/bcon/scripts" ) then
       mkdir -pv $CMAQ_HOME/PREP/bcon/scripts
    endif
    cp PREP/bcon/scripts/bldit_bcon.csh $CMAQ_HOME/PREP/bcon/scripts/bldit_bcon.csh
    cp PREP/bcon/scripts/run_bcon.csh $CMAQ_HOME/PREP/bcon/scripts/run_bcon.csh
 endif

#===============================================================================
#> Copy ICON scripts
#===============================================================================
 if ( $EXT_ICON == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/PREP/icon/scripts" ) then
       mkdir -pv $CMAQ_HOME/PREP/icon/scripts
    endif
    cp PREP/icon/scripts/bldit_icon.csh $CMAQ_HOME/PREP/icon/scripts/bldit_icon.csh
    cp PREP/icon/scripts/run_icon.csh $CMAQ_HOME/PREP/icon/scripts/run_icon.csh
 endif

#===============================================================================
#> Copy LTNG scripts
#===============================================================================
 if ( $EXT_LTNG == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/PREP/ltng/scripts" ) then
       mkdir -pv $CMAQ_HOME/PREP/ltng/scripts
    endif
    cp PREP/ltng/scripts/* $CMAQ_HOME/PREP/ltng/scripts/
 endif

#===============================================================================
#> Copy MCIP scripts
#===============================================================================
 if ( $EXT_MCIP == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/PREP/mcip/scripts" ) then
       mkdir -pv $CMAQ_HOME/PREP/mcip/scripts
    endif
    cp PREP/mcip/scripts/run_mcip.csh $CMAQ_HOME/PREP/mcip/scripts/
 endif
 
#===============================================================================
#> Copy Combine Post-Processor scripts
#===============================================================================
 if ( $EXT_COMBINE == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/POST/combine/scripts" ) then
       mkdir -pv $CMAQ_HOME/POST/combine/scripts
    endif
    cp POST/combine/scripts/bldit_combine.csh  $CMAQ_HOME/POST/combine/scripts/bldit_combine.csh
    cp POST/combine/scripts/run_combine.csh    $CMAQ_HOME/POST/combine/scripts/run_combine.csh
 endif

#===============================================================================
#> Copy Appendwrf Post-Processor scripts
#===============================================================================
 if ( $EXT_APPENDWRF == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/POST/appendwrf/scripts" ) then
       mkdir -pv $CMAQ_HOME/POST/appendwrf/scripts
    endif
    cp POST/appendwrf/scripts/bldit_appendwrf.csh  $CMAQ_HOME/POST/appendwrf/scripts/bldit_appendwrf.csh
    cp POST/appendwrf/scripts/run_appendwrf.csh    $CMAQ_HOME/POST/appendwrf/scripts/run_appendwrf.csh
 endif

#===============================================================================
#> Copy bldoverlay Post-Processor scripts
#===============================================================================
 if ( $EXT_BLDOVERLAY == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/POST/bldoverlay/scripts" ) then
       mkdir -pv $CMAQ_HOME/POST/bldoverlay/scripts
    endif
    cp POST/bldoverlay/scripts/bldit_bldoverlay.csh  $CMAQ_HOME/POST/bldoverlay/scripts/bldit_bldoverlay.csh
    cp POST/bldoverlay/scripts/run_bldoverlay.csh    $CMAQ_HOME/POST/bldoverlay/scripts/run_bldoverlay.csh
 endif

#===============================================================================
#> Copy block_extract Post-Processor scripts
#===============================================================================
 if ( $EXT_BLOCK_EXTRACT == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/POST/block_extract/scripts" ) then
       mkdir -pv $CMAQ_HOME/POST/block_extract/scripts
    endif
    cp POST/block_extract/scripts/bldit_block_extract.csh  $CMAQ_HOME/POST/block_extract/scripts/bldit_block_extract.csh
    cp POST/block_extract/scripts/run_block_extract.csh    $CMAQ_HOME/POST/block_extract/scripts/run_block_extract.csh
 endif

#===============================================================================
#> Copy hr2day Post-Processor scripts
#===============================================================================
 if ( $EXT_HR2DAY == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/POST/hr2day/scripts" ) then
       mkdir -pv $CMAQ_HOME/POST/hr2day/scripts
    endif
    cp POST/hr2day/scripts/bldit_hr2day.csh  $CMAQ_HOME/POST/hr2day/scripts/bldit_hr2day.csh
    cp POST/hr2day/scripts/run_hr2day.csh    $CMAQ_HOME/POST/hr2day/scripts/run_hr2day.csh
 endif

#===============================================================================
#> Copy sitecmp Post-Processor scripts
#===============================================================================
 if ( $EXT_SITECMP == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/POST/sitecmp/scripts" ) then
       mkdir -pv $CMAQ_HOME/POST/sitecmp/scripts
    endif
    cp POST/sitecmp/scripts/bldit_sitecmp.csh  $CMAQ_HOME/POST/sitecmp/scripts/
    cp POST/sitecmp/scripts/run_sitecmp_AQS_Hourly.csh    $CMAQ_HOME/POST/sitecmp/scripts/
 endif

#===============================================================================
#> Copy sitecmp_dailyo3 Post-Processor scripts
#===============================================================================
 if ( $EXT_SITECMP_DAILYO3 == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/POST/sitecmp_dailyo3/scripts" ) then
       mkdir -pv $CMAQ_HOME/POST/sitecmp_dailyo3/scripts
    endif
    cp POST/sitecmp_dailyo3/scripts/bldit_sitecmp_dailyo3.csh  $CMAQ_HOME/POST/sitecmp_dailyo3/scripts/
    cp POST/sitecmp_dailyo3/scripts/run_sitecmp_dailyo3_AQS.csh $CMAQ_HOME/POST/sitecmp_dailyo3/scripts/
    cp POST/sitecmp_dailyo3/scripts/run_sitecmp_dailyo3_CASTNET.csh $CMAQ_HOME/POST/sitecmp_dailyo3/scripts/
 endif

#===============================================================================
#> Copy writesite Post-Processor scripts
#===============================================================================
 if ( $EXT_WRITESITE == 'Y' ) then
    if ( ! -e "$CMAQ_HOME/POST/writesite/scripts" ) then
       mkdir -pv $CMAQ_HOME/POST/writesite/scripts
    endif
    cp POST/writesite/scripts/bldit_writesite.csh  $CMAQ_HOME/POST/writesite/scripts/bldit_writesite.csh
    cp POST/writesite/scripts/run_writesite.csh    $CMAQ_HOME/POST/writesite/scripts/run_writesite.csh
 endif

#===============================================================================
 # Insert Job Scheduler Preface into Run Scripts for those working inside EPA
 #source /work/MOD3DEV/cmaq_common/pbs_run.csh  #>>> Comment Out if not at EPA

#===============================================================================
#> Exit the Script
#===============================================================================
 echo "Configuration and Run Scripts have been Extracted and placed in: $CMAQ_HOME"
 echo "You may now edit these scripts to conform to your system and run options."

 exit
 

