
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
C $Header: /project/work/rep/MECH/src/driver/mech/WREXTS.f,v 1.6 2001/03/05 19:50:14 yoj Exp $ 

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)WREXTS.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.WREXTS.F 02 Jan 1997 15:26:56

      SUBROUTINE WREXTS (EQNAME_MECH, DESCRP_MECH, NS, SPCLIS, SPC1RX,  SS1RX ) 
      
      USE MECHANISM_DATA
      
      IMPLICIT NONE

 
C Argument variables

      CHARACTER( 120 ), INTENT ( IN ) :: EQNAME_MECH
      CHARACTER(  32 ), INTENT ( IN ) :: DESCRP_MECH
      INTEGER,          INTENT ( IN ) :: NS                ! no. of species found in mechanism table
      CHARACTER(  16 ), INTENT ( IN ) :: SPCLIS( : ) ! species list from mechanism table
      INTEGER,          INTENT ( IN ) :: SPC1RX( : ) ! rx index of 1st occurence of species in mechanism table
      INTEGER,          INTENT ( IN ) :: SS1RX( : )

C Local Variables

      INTEGER ISPC, IRX, IFLD0, IFLD1, IFLD2

 
      CHARACTER( 47 ) :: EXHEAD_SPCS
      CHARACTER(  4 ) :: VARA4, VARB4
      CHARACTER( 20 ) :: BUFF20( MAXRXNUM )

      REAL( 8 )       :: DBUFF( MAXRXNUM )
      REAL            :: SBUFF( MAXRXNUM )
      

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     CTM Species intermediate species File prologue
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
 
c                    12345678901234567890123456789012345678901234567
      EXHEAD_SPCS = 'Mechanism CTM Species intermediate INCLUDE File'
      WRITE( EXUNIT_SPCS, 1031 ) EXHEAD_SPCS
1031  FORMAT( 'C', 1X, 9('-'), 1X, A47, 1X, 10('-') )
      CALL WRHDR1 ( EXUNIT_SPCS, EQNAME_SPCS, 108 )
      WRITE( EXUNIT_SPCS, 1027 )
1027  FORMAT( 'C', 1X, 'Generated from ...' )
      CALL WRHDR1 ( EXUNIT_SPCS, EQNAME_MECH, 108 )
      WRITE( EXUNIT_SPCS, 1033 ) DESCRP_MECH
1033  FORMAT( 'C', 1X, 'for Mechanism Name:', 1X, A64 )
      WRITE( EXUNIT_SPCS, 1035 )
1035  FORMAT( /'C', 1X, 'The following are reserved symbols declared in this',
     &              1X, 'INCLUDE file:'
     &        /'C', 4X, 'NSPCS    = Number of mechanism species'
     &        /'C', 4X, 'SPCNAMES = Table of mechanism species names'
     &        /'C', 4X, 'SPC1RX   = rx index of 1st occurence of species',
     &              1X,            'in mechanism table' )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NS
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_SPCS, 1053 ) NS + N_SS_SPC
1053  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NSPCS =', I4 )


c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     CTMSPC and SPC1RX
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-


      WRITE( EXUNIT_SPCS, 1057 )
1057  FORMAT( /6X, 'CHARACTER( 16 ) :: SPCNAMES( NSPCS )'
     &        /6X, 'INTEGER         :: SPC1RX( NSPCS )' / )

 

      DO ISPC = 1, NS
         WRITE( EXUNIT_SPCS, 1059 ) ISPC, ISPC, SPCLIS( ISPC ), SPC1RX( ISPC )
1059     FORMAT( 6X, 'DATA', 1X, 'SPCNAMES(', I3, '),', 1X, 'SPC1RX(', I3, ')',
     &           2X, '/ ''', A16, ''',', I4, ' /' )
      END DO

      DO ISPC = 1, N_SS_SPC
         WRITE( EXUNIT_SPCS, 1059 ) ISPC + NS, ISPC + NS, SS_SPC( ISPC ), SS1RX( ISPC )
      END DO


c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     Fini
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_SPCS, 2013 ) EXHEAD_SPCS
2013  FORMAT( /'C', 1X, 'End of ', A47, 1X, 12('-') )

      RETURN
      END
