
C***************************************************************************
C  Significant portions of Models-3/CMAQ software were developed by        *
C  Government employees and under a United States Government contract.     *
C  Portions of the software were also based on information from non-       *
C  Federal sources, including software developed by research institutions  *
C  through jointly funded cooperative agreements. These research institu-  *
C  tions have given the Government permission to use, prepare derivative   *
C  works, and distribute copies of their work to the public within the     *
C  Models-3/CMAQ software release and to permit others to do so. EPA       *
C  therefore grants similar permissions for use of Models-3/CMAQ software, *
C  but users are requested to provide copies of derivative works to the    *
C  Government without re-strictions as to use by others.  Users are        *
C  responsible for acquiring their own copies of commercial software       *
C  associated with the Models-3/CMAQ release and are also responsible      *
C  to those vendors for complying with any of the vendors' copyright and   *
C  license restrictions. In particular users must obtain a Runtime license *
C  for Orbix from IONA Technologies for each CPU used in Models-3/CMAQ     *
C  applications.                                                           *
C                                                                          *
C  Portions of I/O API, PAVE, and the model builder are Copyrighted        *
C  1993-1997 by MCNC--North Carolina Supercomputing Center and are         *
C  used with their permissions subject to the above restrictions.          *
C***************************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)PARMS.e	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.PARMS.e 02 Jan 1997 15:17:39

C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C PARMS.e

      INTEGER, PARAMETER :: MAXRXNUM    = 2000

      INTEGER, PARAMETER :: MAXSPEC     = 700

      INTEGER, PARAMETER :: MAXPRODS    = 30   ! mechanism products dimension

      INTEGER, PARAMETER :: MAXRCTNTS   = 3    ! mechanism reactants dimension

      INTEGER, PARAMETER :: MAXPHOTRXNS = 100

      INTEGER, PARAMETER :: MAXSPECRXNS = 100

      INTEGER, PARAMETER :: MAXSPECTERMS = 10

      INTEGER, PARAMETER :: MAXFALLOFF  = 50

      INTEGER, PARAMETER :: MAX3BODIES  = 50

      INTEGER, PARAMETER :: MAXWRDLEN   = 16

      INTEGER, PARAMETER :: MAXCONSTS   = 5    ! mechanism "constants"

      INTEGER, PARAMETER :: MAXNLIST     =  50   ! Max no. of species in SS or Eliminate lists
      INTEGER, PARAMETER :: KPPEQN_UNIT  =  95    
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
