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
#> an Origin working directory of the user's choice.
#>
#> The default location of the working directory is one level up from 
#> the repository top level. This script should be executed within the repo
#> as the locations of various scripts are relative to its assumed location.
#>
#> Default Assumed Tree Structure:
#>   CMAQv5.2 (Origin / Working Directory )
#>     --> CMAQ_REPO (Source Code Repository )
#>     --> config.cmaq.csh
#>     --> bldit.cctm.csh
#>     --> run.cctm.csh
#>     --> BLD_CCTM... (CCTM Build Directories)
#>          --> Source code, object files
#>          --> Makefile
#>     --> tools
#>          --> bldmake
#>          --> combine
#>              bldit.combine.csh
#>              run.combine.csh
#>              --> BLD_COMBINE... (Combine Build Directories)
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
 set Origin = $cwd
 #set Origin  [User-Specified Location]

#> Check that the host system is Linux-based
 set BLD_OS = `uname -s`
 if ($BLD_OS != 'Linux') then
    echo "   $BLD_OS -> wrong bldit script for host!"
    exit 1
 endif

#> Check to make sure $Origin exists
 if ( ! -e "$Origin" ) then
    mkdir -pv $Origin
 else
    if ( ! -d "$Origin" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif

#> Return to repository top-level directory
 cd $REPO_HOME


#===============================================================================
#> Copy config.cmaq
#===============================================================================
 cp config_cmaq.csh $Origin/config_cmaq.csh

#===============================================================================
#> Copy CCTM scripts
#===============================================================================
 if ( $EXT_CCTM == 'Y' ) then
    cp CCTM/scripts/bldit_cctm.csh $Origin/bldit_cctm.csh
    cp CCTM/scripts/run_cctm.csh $Origin/run_cctm.csh
 endif
 
#===============================================================================
#> Copy Combine Post-Processor scripts
#===============================================================================
 if ( $EXT_COMBINE == 'Y' ) then
    if ( ! -e "$Origin/tools/combine" ) then
       mkdir -pv $Origin/tools/combine
    endif

    cp POST/combine/scripts/bldit_combine.csh  $Origin/tools/combine/bldit_combine.csh
    cp POST/combine/scripts/run_combine.csh    $Origin/tools/combine/run_combine.csh
 endif

#===============================================================================
#> Exit the Script
#===============================================================================
 echo "Configuration and Run Scripts have been Extracted and placed in: $Origin"
 echo "You may now edit these scripts to conform to your system and run options."

 exit
 

