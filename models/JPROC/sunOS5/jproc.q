#! /bin/csh -f
# script to run the jvalue processor (jproc)

# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header: /project/yoj/arc/JPROC/src/sunOS5/jproc.q,v 1.1 1997/10/13 17:41:44 yoj Exp $ 

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

#set BASE  = /tmp/you/your_directory
 set BASE  = $cwd

# the parameters for this run

 set STDATE   = 1988209            # the beginning date for this run
 set DRIVER   = JPROC              # jprocessor version

# input files and directories

 setenv M3HOME /home/models3
 setenv M3DATA $M3HOME/datasets/nostudies

 set CSQYpath   = $M3DATA  # CSQY input data
 set PROFpath   = $M3DATA  # PROF input data
 set ETpath     = $M3DATA  # ET input data
 set TOMSpath   = $M3DATA  # TOMS input data

 set ETfile    = ETirradiance.dat
 set PROFfile  = PROFILES.dat
 set O2ABSfile = O2_NASA94
 set O3ABSfile = O3O1D_NASA94
#set TOMSfile  = ga880802.n7t
 set TOMSfile  = ga880727.n7t
#set TOMSfile  = not_available

# output file and directory
 
 set OUTDIR   = $BASE                    # Jvalue output data
#set OUTDIR   = /project/mod3/data        # Jvalue output data
#set OUTDIR   = /pub/storage/sjr/tuv_devel        # Jvalue output data
 set JVfile   = JTABLE_$STDATE

# check ET input file

 if (! (-e $ETpath/$ETfile) ) then
    echo " $ETpath/$ETfile not found "
    exit
 endif

# check profile input file

 if (! (-e $PROFpath/$PROFfile) ) then
    echo " $PROFpath/$PROFfile not found "
    exit
 endif

# check TOMS input file

 set TOMSEXIST   = N               # assume no TOMS data file for this run
 if (-e $TOMSpath/$TOMSfile) then
    set TOMSEXIST   = Y            # TOMS data file does for this run
 endif

# check O2 absorption input file

 if (! (-e $CSQYpath/$O2ABSfile) ) then
    echo " $CSQYpath/$O2ABSfile not found "
    exit
 endif

# check O3 absorption input file

 if (! (-e $CSQYpath/$O3ABSfile) ) then
    echo " $CSQYpath/$O3ABSfile not found "
    exit
 endif

# remove existing output file

 unalias rm

 if (-e $OUTDIR/$JVfile)  rm $OUTDIR/$JVfile

#  setenv for input files

 setenv ET        $ETpath/$ETfile
 setenv PROFILES  $PROFpath/$PROFfile
 setenv TOMS      $TOMSpath/$TOMSfile
 setenv O2ABS     $CSQYpath/$O2ABSfile
 setenv O3ABS     $CSQYpath/$O3ABSfile
 setenv CSQY      $CSQYpath

#  setenv for output file

 setenv JVALUES      $OUTDIR/$JVfile

#  setenv for user input

 setenv JPROC_STDATE      $STDATE
 setenv JPROC_TOMSEXIST   $TOMSEXIST

 env

#debugger $BASE/$DRIVER; exit()

 ls -l $BASE/$DRIVER
 timex $BASE/$DRIVER

 exit() 
