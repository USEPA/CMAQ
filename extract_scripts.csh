#!/bin/csh -f

# ================== CMAQ5.2 Extraction Script ====================== #
# Requirements: CMAQ git source code repository                       #
#                                                                     #
# To report problems or request help with this script/program:        #
#             http://www.cmascenter.org/help-desk.cfm                 #
# =================================================================== #

#> This script may be executed when first downloading or cloning the CMAQ
#> repository. The routine will copy important script including config.cmaq,
#> bldit.cctm, and run.cctm as well as scripts for other utilities into 
#> an CMAQ_WORK working directory of the user's choice.
#>
#> The default location of the working directory is one level up from 
#> the repository top level. This script should be executed within the repo
#> as the locations of various scripts are relative to its assumed location.
#>
#> Default Assumed Tree Structure:
#>   CMAQv5.2 (CMAQ_WORK / Working Directory )  
#>     --> CMAQ_REPO (may be located outside CMAQ_WORK)
#>     --> config_cmaq.csh
#>     --> bldit_cctm.csh
#>     --> run_cctm.csh
#>     --> BLD_CCTM... (CCTM Build Directories)
#>          --> Source code, object files
#>          --> Makefile
#>     --> tools
#>          --> bldmake
#>          --> combine
#>              bldit_combine.csh
#>              run_combine.csh
#>              --> BLD_COMBINE... (Combine Build Directories)
#>          --> sitecmp
#>              bldit_sitecmp.csh
#>              run_sitecmp.csh
#>              --> BLD_SITECMP...
#>          --> [Other Tools]
#>     --> lib (links to libraries created by config.cmaq)
#>

#> This section allows users to choose explicitly which tools
#> to make available from the repo. For each selected tool,
#> extract_scripts.csh will copy any build and run scritps
#> out of the repo for you. Set each to [Y/N]
 set EXT_CCTM    = Y
 set EXT_COMBINE = Y


#> model source code repository location 
 set REPO_HOME = $cwd

#> Default location for CMAQ model build is one directory above
#> the repository. The user may also set their own preferred 
#> directory.
 cd ../
 set CMAQ_WORK = $cwd
 #set CMAQ_WORK  [User-Specified Location]

#> Check that the host system is Linux-based
 set BLD_OS = `uname -s`
 if ($BLD_OS != 'Linux') then
    echo "   $BLD_OS -> wrong bldit script for host!"
    exit 1
 endif

#> Check to make sure $CMAQ_WORK exists
 if ( ! -e "$CMAQ_WORK" ) then
    mkdir -pv $CMAQ_WORK
 else
    if ( ! -d "$CMAQ_WORK" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif

#> Return to repository top-level directory
 cd $REPO_HOME


#===============================================================================
#> Copy config.cmaq
#===============================================================================
 cp config_cmaq.csh $CMAQ_WORK/config_cmaq.csh

#===============================================================================
#> Copy CCTM scripts
#===============================================================================
 if ( $EXT_CCTM == 'Y' ) then
    cp CCTM/scripts/bldit_cctm.csh $CMAQ_WORK/bldit_cctm.csh
    cp CCTM/scripts/run_cctm.csh $CMAQ_WORK/run_cctm.csh
 endif
 
#===============================================================================
#> Copy Combine Post-Processor scripts
#===============================================================================
 if ( $EXT_COMBINE == 'Y' ) then
    if ( ! -e "$CMAQ_WORK/tools/combine" ) then
       mkdir -pv $CMAQ_WORK/tools/combine
    endif

    cp POST/combine/scripts/bldit_combine.csh  $CMAQ_WORK/tools/combine/bldit_combine.csh
    cp POST/combine/scripts/run_combine.csh    $CMAQ_WORK/tools/combine/run_combine.csh
 endif

#===============================================================================
#> Exit the Script
#===============================================================================
 echo "Configuration and Run Scripts have been Extracted and placed in: $CMAQ_WORK"
 echo "You may now edit these scripts to conform to your system and run options."

 exit
 

