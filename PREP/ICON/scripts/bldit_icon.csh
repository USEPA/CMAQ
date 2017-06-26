#! /bin/csh -f

# ====================== ICONv5.0.1 Build Script ==================== #
# Usage: bldit.icon >&! bldit.icon.log                                #
# Requirements: I/O API & netCDF libs, Git, and a Fortran compiler    #
# Note that this script is configured/tested for Red Hat Linux O/S    #
# The following environment variables must be set for this script to  #
# build an executable.                                                #
#   setenv M3MODEL <source code Git repository>                       #
#   setenv M3LIB   <code librarie>                                    #
# To report problems or request help with this script/program:        #
#             http://www.cmascenter.org/html/help.html                #
# =================================================================== #

 if ( $#argv == 1 ) then
 setenv COMPILER $argv[1]
 else
    echo "usage: $0 <compiler>"
    echo " where <compiler> is intel, pgi or gcc"
    exit(2)
 endif

#> Source the config.cmaq file to set the build environment
#source ../config.cmaq
 source /home/yoj/src/repo/config.cmaq

#> Check for M3MODEL and M3LIB settings:
 if ( ! -e $M3MODEL || ! -e $M3LIB ) then 
    echo "   $M3MODEL or $M3LIB directory not found"
    exit 1
 endif
 echo "   Model archive path: $M3MODEL"
 echo "         library path: $M3LIB"

 set BLD_OS = `uname -s`        ## Script set up for Linux only 
 if ($BLD_OS != 'Linux') then
    echo "   $BLD_OS -> wrong bldit script for host!"
    exit 1
 endif

 git status

 set echo

#:#:#:#:#:#:#:#:#:#:#:# Begin User Input Section #:#:#:#:#:#:#:#:#:#:#:#

#> user choices: Git repository
 setenv Project $M3MODEL/ICON-git
 setenv GlobInc $M3HOME/ICL-git
 setenv Mechs   $M3HOME/MECHS-git

#> user choices: base working directory, application string
#set Base = $cwd
 set Base = ..
 set APPL  = V5f
 set MODEL = ICON_${APPL}_$EXEC_ID
 set CFG   = cfg.$MODEL 

#> user choices: bldmake command
 set Opt = verbose      # show requested commands as they are executed
 set MakeOpt            # builds a Makefile to make the model; comment out
                        # this option for bldmake to also compile the model

#> user choices: various modules

 set Revision = release       # release = latest CVS revision
#set Revision = '"CMAQv5_0_1"'

 set ModCommon = common

 set ModType   = profile
#set ModType   = m3conc
#set ModType   = tracer

#set ModMech   = cb05
#set ModMech   = saprc99
#set ModMech   = saprc07t

#> user choices: mechanism
#set Mechanism = cb05cl_ae5_aq
#set Mechanism = cb05tucl_ae5_aq
 set Mechanism = cb05tucl_ae6_aq
#set Mechanism = cb05tump_ae6_aq
#set Mechanism = saprc99_ae5_aq
#set Mechanism = saprc99_ae6_aq
#set Mechanism = saprc07tb_ae6_aq
#set Mechanism = saprc07tc_ae6_aq
 set Tracer    = trac0               # default: no tracer species

#> user choices: computing system configuration:
#>    name of the "BLD" directory for checking out and compiling source code
#>    compiler name and location/link flags
#>    library paths

 set Bld = $Base/BLD_${APPL}

#> Set full path of Fortran 90 compiler
 set FC = ${myFC}
 set FP = $FC

#> Set location of M3Bld executable
#set Blder = $M3LIB/build/bldmake
 set Blder = $M3HOME/BLDMAKE-git/bldmake

#> Set location of libraries/include files
 set IOAPI   = "${M3LIB}/ioapi_3.1/Linux2_${system}${compiler_ext} -lioapi"
 set IOAPIMOD = ${M3LIB}/ioapi_3.1/Linux2_${system}${compiler_ext}

 set NETCDF = "${M3LIB}/netcdf/lib -lnetcdf"

 set PARMOD = "."
 set STENEX = "."
 set MPI_INC = "."
#
#> Set compiler flags
 set F_FLAGS    = "${myFFLAGS} -I ${IOAPIMOD} -I ${PARMOD} -I ${STENEX} -I."
 set F90_FLAGS  = "${myFRFLAGS} -I ${IOAPIMOD} -I ${PARMOD} -I ${STENEX} -I."
 set CPP_FLAGS  = ""
 set C_FLAGS    = "${myCFLAGS} -DFLDMN -I ${MPI_INC}"
 set LINK_FLAGS = "${myLINK_FLAG}"

#:#:#:#:#:#:#:#:#:#:#:# End of User Input Section :#:#:#:#:#:#:#:#:#:#:#:#:#

 if ( ! -e "$Bld" ) then
    mkdir $Bld
 else
    if ( ! -d "$Bld" ) then
       echo "   *** target exists, but not a directory ***"
       exit 1
    endif
 endif
 
#cd $Bld

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#

 set LIB1 =
 set LIB2 =
 set LIB3 =
 set Str1 =
 set Str2 =
 set STX  = 

 set LIB4 = "-L${IOAPI}"
 set LIB5 = "-L${NETCDF}"
 set LIBS = "$LIB1 $LIB2 $LIB3 $LIB4 $LIB5"

#source $Base/relinc.icon
#if ( $status ) exit 1

 set ICL_MECH  = $Mechs/$Mechanism
 set ICL_TRAC  = $Mechs/$Tracer

#setenv CVSROOT $Project

#> make the config file

 set Cfile = ${CFG}.bld
 set quote = '"'

 echo                                                               > $Cfile
 echo "model       $MODEL;"                                        >> $Cfile
 echo                                                              >> $Cfile
 echo "FPP         $FP;"                                           >> $Cfile
 echo                                                              >> $Cfile
 set text = "$quote$CPP_FLAGS $STX$quote;"
 echo "cpp_flags   $text"                                          >> $Cfile
 echo                                                              >> $Cfile
 echo "f_compiler  $FC;"                                           >> $Cfile
 echo                                                              >> $Cfile
 echo "f_flags     $quote$F_FLAGS$quote;"                          >> $Cfile
 echo                                                              >> $Cfile
 echo "f90_flags   $quote$F90_FLAGS$quote;"                        >> $Cfile
 echo                                                              >> $Cfile
 echo "c_flags     $quote$C_FLAGS$quote;"                          >> $Cfile
 echo                                                              >> $Cfile
 echo "link_flags  $quote$LINK_FLAGS$quote;"                       >> $Cfile
 echo                                                              >> $Cfile
 echo "libraries   $quote$LIBS$quote;"                             >> $Cfile
 echo                                                              >> $Cfile
 echo "global      $Opt;"                                          >> $Cfile
 echo                                                              >> $Cfile

 set text="// mechanism:"
 echo "$text ${Mechanism}"                                         >> $Cfile
 echo "// project archive: ${Project}"                             >> $Cfile
 echo                                                              >> $Cfile

 if ( $compiler == gfort ) then 
    set ICL_MECH = '.'
 endif
 echo "include SUBST_RXCMMN     $ICL_MECH/RXCM.EXT;"               >> $Cfile
 echo "include SUBST_RXDATA     $ICL_MECH/RXDT.EXT;"               >> $Cfile
 echo                                                              >> $Cfile

 echo "$Str1"                                                      >> $Cfile
 echo "$Str2"                                                      >> $Cfile
 echo                                                              >> $Cfile

 set text = "common"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModCommon};"                                       >> $Cfile
 echo                                                              >> $Cfile

# if ( $?ParOpt ) then      # Multiprocessor system configuration
#   set text = "par"
#   echo "// options are" $text                                    >> $Cfile
#   echo "Module ${ModPar};"                                       >> $Cfile
#   echo                                                           >> $Cfile
# endif

 set text = "profile, m3conc, tracer"
 echo "// options are" $text                                       >> $Cfile
 echo "Module ${ModType};"                                         >> $Cfile
 echo                                                              >> $Cfile

#set text = "cb05, saprc99, saprc07t"
#echo "// options are" $text                                       >> $Cfile
#echo "Module ${ModMech};"                                         >> $Cfile
#echo                                                              >> $Cfile

 if ( $?ModMisc ) then
    echo "Module ${ModMisc};"                                      >> $Cfile
    echo                                                           >> $Cfile
 endif

#> make the makefile or the model executable

 unalias mv rm
 if ( $?MakeOpt ) then
    $Blder -make $Cfile   # $Cfile = ${CFG}.bld
    mv Makefile $Bld/Makefile.$COMPILER
    if ( -e $Bld/Makefile.$COMPILER && -e $Bld/Makefile ) rm $Bld/Makefile
    ln -s $Bld/Makefile.$COMPILER $Bld/Makefile
 else
    set NoMake
    $Blder $Cfile
 endif
 if ( $status != 0 ) then
    echo "   *** failure in $Blder ***"
    exit 1
 endif
 if ( -e "$Base/${CFG}" ) then
    echo "   >>> previous ${CFG} exists, re-naming to ${CFG}.old <<<"
    unalias mv
    mv $Base/${CFG} $Base/${CFG}.old
 endif
 mv ${CFG}.bld $Bld/${CFG}
 if ( ( $Opt != no_compile ) && \
      ( $Opt != no_link    ) && \
      ( $Opt != parse_only ) && \
      ( $Opt != show_only  ) && \
        $?NoMake ) then
    mv $MODEL $Base
 endif

 exit
