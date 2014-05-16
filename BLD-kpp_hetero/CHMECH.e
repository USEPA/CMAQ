
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
C @(#)CHMECH.e	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.CHMECH.e 02 Jan 1997 15:17:38

C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C CHMECH.e

      INTEGER KUNITS, 
     &        KTYPE       ( MAXRXNUM ),
     &        IRXBITS     ( MAXRXNUM ),
     &        IORDER      ( MAXRXNUM ),
     &        KTN1, KRX1  ( MAXRXNUM ), 
     &        KTN2, KRX2  ( MAXRXNUM ), 
     &        KTN3, KRX3  ( MAXRXNUM ),
     &        KTN4, KRX4  ( MAXRXNUM ),
     &        KTN5, KRX5  ( MAXRXNUM ),
     &        KTN6, KRX6  ( MAXRXNUM ),
     &        KTN7, KRX7  ( MAXRXNUM ),
!    &        KCNV, KRXCNV( MAXRXNUM ),
     &        NFALLOFF, 
     &        IRRFALL( MAXFALLOFF )

      INTEGER NWM,   NRXWM  ( MAX3BODIES )
      INTEGER NWW,   NRXWW  ( MAX3BODIES )
      INTEGER NWO2,  NRXWO2 ( MAX3BODIES )
      INTEGER NWN2,  NRXWN2 ( MAX3BODIES )
      INTEGER NWCH4, NRXWCH4( MAX3BODIES )
      INTEGER NWH2,  NRXWH2 ( MAX3BODIES )

      REAL( 8 ) :: RTDAT( 3,MAXRXNUM )
      REAL( 8 ) :: RFDAT( 5,MAXFALLOFF )
      REAL( 8 ) :: CONST( MAXCONSTS )
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
