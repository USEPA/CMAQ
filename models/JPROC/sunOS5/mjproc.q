#! /bin/csh -f
# script to run the jvalue processor (jproc)

# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header: /project/yoj/arc/JPROC/src/sunOS5/mjproc.q,v 1.1 1998/02/18 16:55:19 yoj Exp $ 

# what(1) key, module and SID; SCCS file; date and time of last delta:
# @(#)jproc.q	1.3 /project/mod3/JPROC/doc/bldrun/sunOS5/SCCS/s.jproc.q 17 Jun 1997 10:20:05

# for Sun Sparc 20, UltraSparc 2
# method: jproc.q >&! jproc.log &
 
# method: qsub
# QSUB -lM 4.0Mw
# QSUB -lT 600
##QSUB -q tempest
# QSUB -eo -o /tmp/you/your_directory/jproc.log

 date; set timestamp; cat cfg; echo "    "; set echo

 set APPL     = a1b
#set CFG      = a1b
 set CFG      = $APPL
 set DRIVER   = JPROC_$CFG

#set BASE  = /tmp/you/your_directory
 set BASE  = $cwd

# the parameters for this run - produces one file per day

 set STDATE   = 1995186            # the beginning day for this run
 set ENDATE   = 1995200            # the ending day
#set ENDATE   = 1995186            # the ending day

# input files and directories

 setenv M3HOME /project/cmaq2
 setenv M3DATA /project/cmaq2/beta_tut0

 set CSQYpath   = $M3DATA  # CSQY input data
 set PROFpath   = $M3DATA  # PROF input data
 set ETpath     = $M3DATA  # ET input data
 set TOMSpath   = $M3DATA  # TOMS input data

 set ETfile    = ETirradiance.dat
 set PROFfile  = PROFILES.dat
 set O2ABSfile = O2_NASA94
 set O3ABSfile = O3O1D_NASA94
#set TOMSfile  = ga880802.n7t
#set TOMSfile  = ga880727.n7t
 set TOMSfile  = not_available

# output file and directory
 
#set OUTDIR   = $BASE                    # Jvalue output data
#set OUTDIR   = /project/mod3/data        # Jvalue output data
#set OUTDIR   = /pub/storage/sjr/tuv_devel        # Jvalue output data
#set OUTDIR   = /project/cmaq2/jproc/radm2_cis1_ae 
 set OUTDIR   = /project/cmaq3/yoj/test

#  setenv for input files

 setenv ET        $ETpath/$ETfile
 setenv PROFILES  $PROFpath/$PROFfile
 setenv TOMS      $TOMSpath/$TOMSfile
 setenv O2ABS     $CSQYpath/$O2ABSfile
 setenv O3ABS     $CSQYpath/$O3ABSfile
 setenv CSQY      $CSQYpath

# check ET input file

 if (! ( -e $ET ) ) then
    echo " $ET not found "
    exit
    endif

# check profile input file

 if (! ( -e $PROFILES ) ) then
    echo " $PROFILES not found "
    exit
    endif

# check TOMS input file

 setenv JPROC_TOMSEXIST  N  # Assume TOMS data file does not exist for this run
 if ( -e $TOMS ) then
    setenv JPROC_TOMSEXIST  Y
    endif

# check O2 absorption input file

 if (! ( -e $O2ABS ) ) then
    echo " $O2ABS not found "
    exit
    endif

# check O3 absorption input file

 if (! ( -e $O3ABS ) ) then
    echo " $O3ABS not found "
    exit
    endif

 env

#debugger $BASE/$DRIVER; exit()

 ls -l $BASE/$DRIVER
#timex $BASE/$DRIVER

 unalias rm
 @ Date = $STDATE
 while ( $Date <= $ENDATE )
    setenv JPROC_STDATE $Date
    echo "   Running for $Date ..."
    set JVfile = JTABLE_${Date}
    setenv JVALUES $OUTDIR/$JVfile
#   remove existing output file
    if ( -e $JVALUES ) rm $JVALUES
#   setenv for output file
    $BASE/$DRIVER
    @ Date++
    end

 exit() 
