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
#>     --> config.cmaq
#>     --> bldit.cctm
#>     --> run.cctm
#>     --> BLD_CCTM... (CCTM Build Directories)
#>          --> Source code, object files
#>          --> Makefile
#>     --> Tools
#>          --> bldmake
#>          --> Combine
#>              --> BLD_COMBINE... (Combine Build Directories)
#>     --> lib (links to libraries created by config.cmaq)
#>

#> model source code repository location 
 set REPO_HOME = $cwd

#> Default location for CMAQ model build is one directory above
#> the repository. The user may also set their own preferred 
#> directory.
 cd ../
 set Origin = $cwd
 #set Origin  [User-Specified Location]

#> Return to repository top-level directory
 cd $REPO_HOME

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


#===============================================================================
#> Copy config.cmaq
#===============================================================================
 cp config.cmaq $Origin/config.cmaq

#===============================================================================
#> Copy CCTM scripts
#===============================================================================
 cp CCTM/scripts/bldit.cctm $Origin/bldit.cctm
 cp CCTM/scripts/run.cctm $Origin/run.cctm
 
#===============================================================================
#> Copy Combine Post-Processor scripts
#===============================================================================
 cp POST/combine/scripts/bldit.combine $Origin/bldit.combine
 cp POST/combine/scripts/run.combine $Origin/run.ccombine


 exit
 

