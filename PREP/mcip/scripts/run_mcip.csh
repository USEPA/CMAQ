#!/bin/csh -f 

#------------------------------------------------------------------------------#
#  The Community Multiscale Air Quality (CMAQ) system software is in           #
#  continuous development by various groups and is based on information        #
#  from these groups: Federal Government employees, contractors working        #
#  within a United States Government contract, and non-Federal sources         #
#  including research institutions.  These groups give the Government          #
#  permission to use, prepare derivative works of, and distribute copies       #
#  of their work in the CMAQ system to the public and to permit others         #
#  to do so.  The United States Environmental Protection Agency                #
#  therefore grants similar permission to use the CMAQ system software,        #
#  but users are requested to provide copies of derivative works or            #
#  products designed to operate in the CMAQ system to the United States        #
#  Government without restrictions as to use by others.  Software              #
#  that is used with the CMAQ system but distributed under the GNU             #
#  General Public License or the GNU Lesser General Public License is          #
#  subject to their copyright restrictions.                                    #
#------------------------------------------------------------------------------#

#=======================================================================
#
#  Script:  run.mcip
#  Purpose: Runs Models-3/CMAQ Meteorology-Chemistry Interface
#           Processor.  Part of the US EPA's Models-3/CMAQ system.
#  Method:  In UNIX/Linux:  run.mcip >&! mcip.log
#  Revised: See README.revisions
#=======================================================================

#-----------------------------------------------------------------------
# Set identification for input and output files.
#
#   APPL       = Application Name (tag for MCIP output file names)
#   CoordName  = Coordinate system name for GRIDDESC
#   GridName   = Grid Name descriptor for GRIDDESC
#   InMetDir   = Directory that contains input meteorology files
#   InTerDir   = Directory that contains input MM5 "TERRAIN" file or
#                WRF Preprocessing System "GEOGRID" file.  (Used for
#                providing fractional land-use categories.  For MM5,
#                it will only work if IEXTRA was set to TRUE in
#                MM5's TERRAIN program.  Is TRUE for P-X simulations.
#                Not needed for WRF if "LANDUSEF" is part of history
#                file.)
#   InSatDir   = Directory that contains GOES satellite files.  (Used
#                with satellite processing from UAH; otherwise leave
#                blank.)
#   OutDir     = Directory to write MCIP output files
#   ProgDir    = Directory that contains the MCIP executable
#   WorkDir    = Working Directory for Fortran links and namelist
#-----------------------------------------------------------------------

source $CMAQ_HOME/config.cmaq

set APPL       = 110702   
set CoordName  = LamCon_40N_97W # 16-character maximum
set GridName   = SE52BENCH     # 16-character maximum

set DataPath   = $CMAQ_DATA
set InMetDir   = $DataPath/wrf
set InTerDir   = $DataPath/wrf
set InSatDir   = $DataPath/goes
set OutDir     = $CMAQ_DATA/mcip/$GridName
set ProgDir    = $CMAQ_HOME/PREP/mcip/src
set WorkDir    = $OutDir

#-----------------------------------------------------------------------
# Set name(s) of input meteorology file(s)
#
#   File name(s) must be set inside parentheses since "InMetFiles" is
#   a C-shell script array.  Multiple file names should be space-
#   delimited.  Additional lines can be used when separated by a
#   back-slash (\) continuation marker.  The file names can be as
#   they appear on your system; MCIP will link the files in by a
#   Fortran unit number and the explicit name via a namelist.  The
#   files must be listed in chronological order.  The maximum number
#   of input meteorology files must be less than or equal to the number
#   in MAX_MM in file_mod.F (default is 367).
#
#   Example:
#     set InMetFiles = ( $InMetDir/MMOUT_DOMAIN2.time1 \
#                        $InMetDir/MMOUT_DOMAIN2.time2 )
#
#-----------------------------------------------------------------------

set InMetFiles = ( $InMetDir/subset_wrfout_d01_2011-07-01_00:00:00 \
                   $InMetDir/subset_wrfout_d01_2011-07-02_00:00:00 \
                   $InMetDir/subset_wrfout_d01_2011-07-03_00:00:00 )

set IfTer      = "F"
set InTerFile  = $InTerDir/geo_em.d01.nc

set InSatFiles = ( )

#-----------------------------------------------------------------------
# Set user control options.
#
#   LPV:     0 = Do not compute and output potential vorticity
#            1 = Compute and output potential vorticity
#
#   LWOUT:   0 = Do not output vertical velocity
#            1 = Output vertical velocity
#
#   LUVCOUT: 0 = Do not output u- and v-component winds on C-grid
#            1 = Output u- and v-component winds on C-grid
#
#   LSAT:    0 = No satellite input is available (default)
#            1 = GOES observed cloud info replaces model-derived input
#-----------------------------------------------------------------------

set LPV     = 1
set LWOUT   = 0
set LUVCOUT = 1
set LSAT    = 0

#-----------------------------------------------------------------------
# Set run start and end date.  (YYYY-MO-DD-HH:MI:SS.SSSS)
#   MCIP_START:  First date and time to be output [UTC]
#   MCIP_END:    Last date and time to be output  [UTC]
#   INTVL:       Frequency of output [minutes]
#-----------------------------------------------------------------------

set MCIP_START = 2011-07-02-00:00:00.0000  # [UTC]
set MCIP_END   = 2011-07-03-00:00:00.0000  # [UTC]

set INTVL      = 60 # [min]

#-----------------------------------------------------------------------
# Set CTM layers.  Should be in descending order starting at 1 and 
# ending with 0.  There is currently a maximum of 100 layers allowed.
# To use all of the layers from the input meteorology without
# collapsing (or explicitly specifying), set CTMLAYS = -1.0.
#-----------------------------------------------------------------------

set CTMLAYS = "-1.0"
#set CTMLAYS = "1.000, 0.997, 0.993, 0.988, 0.980, 0.970, 0.960, 0.950, 0.930, \
              #0.910, 0.890, 0.870, 0.840, 0.800, 0.760, 0.720, 0.680, 0.640, \
              #0.600, 0.550, 0.450, 0.350, 0.250, 0.150, 0.060, 0.000"


#-----------------------------------------------------------------------
# Determine whether or not static output (GRID) files will be created.
#-----------------------------------------------------------------------

set MKGRID = T

#-----------------------------------------------------------------------
# Set number of meteorology "boundary" points to remove on each of four
# horizontal sides of MCIP domain.  This affects the output MCIP domain
# dimensions by reducing meteorology domain by 2*BTRIM + 2*NTHIK + 1,
# where NTHIK is the lateral boundary thickness (in BDY files), and the
# extra point reflects conversion from grid points (dot points) to grid
# cells (cross points).  Setting BTRIM = 0 will use maximum of input
# meteorology.  To remove MM5 lateral boundaries, set BTRIM = 5.
#
# *** If windowing a specific subset domain of input meteorology, set
#     BTRIM = -1, and BTRIM will be ignored in favor of specific window
#     information in X0, Y0, NCOLS, and NROWS.
#-----------------------------------------------------------------------

set BTRIM = 0

#-----------------------------------------------------------------------
# Define MCIP subset domain.  (Only used if BTRIM = -1.  Otherwise,
# the following variables will be set automatically from BTRIM and
# size of input meteorology fields.)
#   X0:     X-coordinate of lower-left corner of full MCIP "X" domain
#           (including MCIP lateral boundary) based on input MM5 domain.
#           X0 refers to the east-west dimension.  Minimum value is 1.
#   Y0:     Y-coordinate of lower-left corner of full MCIP "X" domain
#           (including MCIP lateral boundary) based on input MM5 domain.
#           Y0 refers to the north-south dimension.  Minimum value is 1.
#   NCOLS:  Number of columns in output MCIP domain (excluding MCIP
#           lateral boundaries).
#   NROWS:  Number of rows in output MCIP domain (excluding MCIP
#           lateral boundaries).
#-----------------------------------------------------------------------

set X0    =   6 
set Y0    =   6
set NCOLS =  72
set NROWS = 100

#-----------------------------------------------------------------------
# Set coordinates for cell for diagnostic prints on output domain.
# If coordinate is set to 0, domain center cell will be used.
#-----------------------------------------------------------------------

set LPRT_COL = 0
set LPRT_ROW = 0

#-----------------------------------------------------------------------
# Optional:  Set WRF Lambert conformal reference latitude.
#            (Handy for matching WRF grids to existing MM5 grids.)
#            If not set, MCIP will use average of two true latitudes.
# To "unset" this variable, set the script variable to "-999.0".
# Alternatively, if the script variable is removed here, remove it
# from the setting of the namelist (toward the end of the script).
#-----------------------------------------------------------------------

set WRF_LC_REF_LAT = 40.0

#=======================================================================
#=======================================================================
# Set up and run MCIP.
#   Should not need to change anything below here.
#=======================================================================
#=======================================================================

set PROG = mcip

date

#-----------------------------------------------------------------------
# Make sure directories exist.
#-----------------------------------------------------------------------

if ( ! -d $InMetDir ) then
  echo "No such input directory $InMetDir"
  exit 1
endif

if ( ! -d $OutDir ) then
  echo "No such output directory...will try to create one"
  mkdir -p $OutDir
  if ( $status != 0 ) then
    echo "Failed to make output directory, $OutDir"
    exit 1
  endif
endif

if ( ! -d $ProgDir ) then
  echo "No such program directory $ProgDir"
  exit 1
endif

if ( $LSAT == 1 ) then
  if ( ! -d $InSatDir ) then
    echo "No such satellite input directory $InSatDir"
    exit 1
  endif
endif

#-----------------------------------------------------------------------
# Make sure the input files exist.
#-----------------------------------------------------------------------

if ( $IfTer == "T" ) then
  if ( ! -f $InTerFile ) then
    echo "No such input file $InTerFile"
    exit 1
  endif
endif

foreach fil ( $InMetFiles )
  if ( ! -f $fil ) then
    echo "No such input file $fil"
    exit 1
  endif
end

if ( $LSAT == 1 ) then
  foreach fil ( $InSatFiles )
    if ( ! -f $fil ) then
      echo "No such input file $fil"
      exit 1
    endif
  end
endif

#-----------------------------------------------------------------------
# Make sure the executable exists.
#-----------------------------------------------------------------------

if ( ! -f $ProgDir/${PROG}.exe ) then
  echo "Could not find ${PROG}.exe"
  exit 1
endif

#-----------------------------------------------------------------------
# Create a work directory for this job.
#-----------------------------------------------------------------------

if ( ! -d $WorkDir ) then
  mkdir -p $WorkDir
  if ( $status != 0 ) then
    echo "Failed to make work directory, $WorkDir"
    exit 1
  endif
endif

cd $WorkDir

#-----------------------------------------------------------------------
# Set up script variables for input files.
#-----------------------------------------------------------------------

if ( $IfTer == "T" ) then
  if ( -f $InTerFile ) then
    set InTer = $InTerFile
  else
    set InTer = "no_file"
  endif
else
  set InTer = "no_file"
endif

set FILE_GD  = $OutDir/GRIDDESC
set FILE_HDR = $OutDir/mmheader.${APPL}

#-----------------------------------------------------------------------
# Create namelist with user definitions.
#-----------------------------------------------------------------------

set MACHTYPE = `uname`
if ( ( $MACHTYPE == "AIX" ) || ( $MACHTYPE == "Darwin" ) ) then
  set Marker = "/"
else
  set Marker = "&END"
endif

cat > $WorkDir/namelist.${PROG} << !

 &FILENAMES
  file_gd    = "$FILE_GD"
  file_hdr   = "$FILE_HDR"
  file_mm    = "$InMetFiles[1]",
!

if ( $#InMetFiles > 1 ) then
  @ nn = 2
  while ( $nn <= $#InMetFiles )
    cat >> $WorkDir/namelist.${PROG} << !
               "$InMetFiles[$nn]",
!
    @ nn ++
  end
endif

if ( $IfTer == "T" ) then
cat >> $WorkDir/namelist.${PROG} << !
  file_ter   = "$InTer"
!
endif

if ( $LSAT == 1 ) then
  cat >> $WorkDir/namelist.${PROG} << !
  file_sat   = "$InSatFiles[1]",
!
  if ( $#InSatFiles > 1 ) then
    @ nn = 2
    while ( $nn <= $#InSatFiles )
      cat >> $WorkDir/namelist.${PROG} << !
               "$InSatFiles[$nn]",
!
      @ nn ++
    end
  endif
endif

cat >> $WorkDir/namelist.${PROG} << !
  makegrid   = .${MKGRID}.
 $Marker

 &USERDEFS
  lpv        =  $LPV
  lwout      =  $LWOUT
  luvcout    =  $LUVCOUT
  lsat       =  $LSAT
  mcip_start = "$MCIP_START"
  mcip_end   = "$MCIP_END"
  intvl      =  $INTVL
  coordnam   = "$CoordName"
  grdnam     = "$GridName"
  ctmlays    =  $CTMLAYS
  btrim      =  $BTRIM
  lprt_col   =  $LPRT_COL
  lprt_row   =  $LPRT_ROW
  wrf_lc_ref_lat = $WRF_LC_REF_LAT
 $Marker

 &WINDOWDEFS
  x0         =  $X0
  y0         =  $Y0
  ncolsin    =  $NCOLS
  nrowsin    =  $NROWS
 $Marker

!

#-----------------------------------------------------------------------
# Set links to FORTRAN units.
#-----------------------------------------------------------------------

rm fort.*
if ( -f $FILE_GD ) rm -f $FILE_GD

ln -s $FILE_HDR                  fort.2
ln -s $FILE_GD                   fort.4
ln -s $WorkDir/namelist.${PROG}  fort.8
if ( $IfTer == "T" ) then
  ln -s $InTerFile               fort.9
endif

set NUMFIL = 0
foreach fil ( $InMetFiles )
  @ NN = $NUMFIL + 10
  ln -s $fil fort.$NN
  @ NUMFIL ++
end

#-----------------------------------------------------------------------
# Set output file names and other miscellaneous environment variables.
#-----------------------------------------------------------------------

setenv IOAPI_CHECK_HEADERS  T
setenv EXECUTION_ID         $PROG

setenv GRID_BDY_2D          $OutDir/GRIDBDY2D_${APPL}.nc
setenv GRID_CRO_2D          $OutDir/GRIDCRO2D_${APPL}.nc
setenv GRID_CRO_3D          $OutDir/GRIDCRO3D_${APPL}.nc
setenv GRID_DOT_2D          $OutDir/GRIDDOT2D_${APPL}.nc
setenv MET_BDY_3D           $OutDir/METBDY3D_${APPL}.nc
setenv MET_CRO_2D           $OutDir/METCRO2D_${APPL}.nc
setenv MET_CRO_3D           $OutDir/METCRO3D_${APPL}.nc
setenv MET_DOT_3D           $OutDir/METDOT3D_${APPL}.nc

#-----------------------------------------------------------------------
# Execute MCIP.
#-----------------------------------------------------------------------

$ProgDir/${PROG}.exe

if ( $status == 0 ) then
  rm fort.*
  exit 0
else
  echo "Error running $PROG"
  exit 1
endif
