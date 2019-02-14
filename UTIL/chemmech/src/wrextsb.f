
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
C @(#)WREXTSB.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.WREXTSB.F 02 Jan 1997 15:26:57

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE WREXTSB ( EQNAME_MECH, DESCRP_MECH,
     &                     NS, SPCLIS,
     &                     NADSPC, ADNAME, ADNMAP,
     &                     MOLWT )
      
      IMPLICIT NONE

      USE MECHANISM_DATA
      USE GET_ENV_VARS
      
      IMPLICIT NONE
!      INCLUDE 'PARMS.e'

C Argument variables

      CHARACTER( 120 ) :: EQNAME_MECH
      CHARACTER(   8 ) :: DESCRP_MECH

      INTEGER             NS                ! no. of species found in mechanism table
      CHARACTER(  16 ) :: SPCLIS( : ) ! species list from mechanism table

      INTEGER             NADSPC            ! no. of advected species
      CHARACTER(  16 ) :: ADNAME( : ) ! advected species name table
      INTEGER             ADNMAP( : ) ! advected species pointers to full list

      REAL                MOLWT( : )  ! molecular weights from mechanism table

C Local Variables

      INTEGER ISPC

      INTEGER, EXTERNAL :: JUNIT
 
      CHARACTER( 120 ) :: EQNAME_SPCS
      CHARACTER( 120 ) :: EQNAME_ADVS

      CHARACTER(  34 ) :: EXHEAD_SPCS
      CHARACTER(  33 ) :: EXHEAD_ADVS

      CHARACTER(  12 ) :: EXFLNM_SPCS = 'SPCSDATX'
      CHARACTER(  12 ) :: EXFLNM_ADVS = 'ADVSDATX'

      INTEGER EXUNIT_SPCS
      INTEGER EXUNIT_ADVS

C-----------------------------------------------------------------------

      EXUNIT_SPCS = JUNIT()
      EXUNIT_ADVS = JUNIT()

C symbolic link locates "EXFLNM_..."; setenv requires INQUIRE (VALUE_NAME):
      CALL VALUE_NAME ( EXFLNM_SPCS, EQNAME_SPCS )
      CALL VALUE_NAME ( EXFLNM_ADVS, EQNAME_ADVS )

      OPEN ( UNIT = EXUNIT_SPCS, FILE = EQNAME_SPCS, STATUS = 'UNKNOWN' )
      OPEN ( UNIT = EXUNIT_ADVS, FILE = EQNAME_ADVS, STATUS = 'UNKNOWN' )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
C     Mechanism CTM Species INCLUDE File prologue
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

C                    1234567890123456789012345678901234
      EXHEAD_SPCS = 'Mechanism CTM Species INCLUDE File'
      WRITE( EXUNIT_SPCS,1011 ) DESCRP_MECH, EXHEAD_SPCS
1011  FORMAT( 'C', 1X, A8, ':', 1X, A34, 1X, 25('-'))
      CALL WRHDR1 (EXUNIT_SPCS, EQNAME_SPCS, 72 )
      WRITE( EXUNIT_SPCS,1013 )
1013  FORMAT( 'C', 1X, 'Generated from ...' )
      CALL WRHDR1 (EXUNIT_SPCS, EQNAME_MECH, 72 )
      WRITE( EXUNIT_SPCS,1015 )
1015  FORMAT( /'C', 1X, 'The following are reserved symbols declared in this',
     &              1X, 'INCLUDE file:'
     &        /'C', 4X, 'NSPCS    = Number of mechanism CTM species'
!    &        /'C', 4X, 'CTMSPC   = Table of mechanism CTM species names'
     &        /'C', 4X, 'SPCNAMES = Table of mechanism CTM species names'
     &        /'C', 4X, 'MOLWT    = Table of mechanism CTM species molecular',
     &              1X, 'weights')
!    &        /'C', 4X, 'NADSPC   = Number of advected mechanism CTM species'
!    &        /'C', 4X, 'ADNAME   = Index sorted table of advected mechanism',
!    &              1X, 'CTM species names'
!    &        /'C', 4X, 'ADNMAP   = Index of advected',
!    &              1X, 'species indices in CTM species table')
!    &        /'C', 4X, 'NDSPCS   = Number of diagnosed mechanism species'
!    &        /'C', 4X, 'DIAGSPC  = Sorted table of diagnosed mechanism',
!    &              1X, 'CTM species' 
!    &        /'C', 4X, 'DSPMAP   = Sorted table of diagnosed mechanism',
!    &              1X, 'CTM species indices')

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
C     Advected CTM Species INCLUDE File prologue
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

C                    123456789012345678901234567890123
      EXHEAD_ADVS = 'Advected CTM Species INCLUDE File'
      WRITE( EXUNIT_ADVS,1017 ) DESCRP_MECH, EXHEAD_ADVS
1017  FORMAT( 'C', 1X, A8, ':', 1X, A33, 1X, 26('-') )
      CALL WRHDR1 (EXUNIT_ADVS, EQNAME_ADVS, 72 )
      WRITE( EXUNIT_ADVS,1013 )
      CALL WRHDR1 (EXUNIT_ADVS, EQNAME_MECH, 72)
      WRITE( EXUNIT_ADVS,1019 )
1019  FORMAT( /'C', 1X, 'The following are reserved symbols declared in this',
     &              1X, 'INCLUDE file:'
     &        /'C', 4X, 'NADSPC   = Number of advected mechanism species'
     &        /'C', 4X, 'ADNAME   = Sorted table of advected mechanism',
     &              1X, 'species'
     &        /'C', 4X, 'ADNMAP   = Sorted table of advected mechanism',
     &             1X, 'species indices' )
                                                 
c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
C     NS
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

!     WRITE(EXUNIT_SPCS, 1021) NS
!021  FORMAT(/ 6X, 'INTEGER    NSPCSD  !!! <- ELIMINATE !!!'
!    &       / 6X, 'PARAMETER (NSPCSD =', I4, ')')

      WRITE(EXUNIT_SPCS, 1023) NS
1023  FORMAT(/ 6X, 'INTEGER    NSPCS'
     &       / 6X, 'PARAMETER (NSPCS =', I4, ')')

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
C     CTMSPC and MOLWT
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

!     WRITE(EXUNIT_SPCS, 1025)
1025  FORMAT(/ 6X, 'INTEGER    ISPCS')

      WRITE(EXUNIT_SPCS, 1027)
!027  FORMAT(/ 6X, 'CHARACTER*16 CTMSPC(NSPCS)' 
1027  FORMAT(/ 6X, 'CHARACTER*16 SPCNAMES(NSPCS)' 
     &       / 6X, 'REAL          MOLWT(NSPCS)' /)

      DO ISPC = 1, NS
         WRITE(EXUNIT_SPCS, 1029) ISPC, ISPC, SPCLIS(ISPC), MOLWT(ISPC)
!029     FORMAT(6X, 'DATA', 1X, 'CTMSPC(', I3, '),', 1X, 'MOLWT(', I3, ')',
1029     FORMAT(6X, 'DATA', 1X, 'SPCNAMES(', I3, '),', 1X, 'MOLWT(', I3, ')',
     &          2X, '/ ''', A16, ''',', F6.1, ' /')
         END DO

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
C     Advected species in ascending index sorted order
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE(EXUNIT_ADVS, 1031) NADSPC
1031  FORMAT(/ 6X, 'INTEGER    NADSPC'
     &       / 6X, 'PARAMETER (NADSPC =', I4, ')')

      IF (NADSPC .NE. 0) THEN

         WRITE(EXUNIT_ADVS, 1033)
1033     FORMAT(/ 6X, 'CHARACTER*16 ADNAME(NADSPC)'
     &          / 6X, 'INTEGER      ADNMAP(NADSPC)')

         DO ISPC = 1, NADSPC
            WRITE(EXUNIT_ADVS, 1035) ISPC, ISPC, ADNAME(ISPC), ADNMAP(ISPC)
1035        FORMAT(6X, 'DATA', 1X, 'ADNAME(', I3, '),', 1X, 'ADNMAP(', I3, ')',
     &             2X, '/ ''', A16, ''',', I4, ' /')
            END DO

         ELSE

         WRITE(EXUNIT_ADVS, 1037)
1037     FORMAT(/ 'C Advected species information not available ...'
     &          / 6X, 'CHARACTER*16 ADNAME(1)'
     &          / 6X, 'INTEGER      ADNMAP(1)')

         END IF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
C     Fini
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE(EXUNIT_SPCS, 1901) EXHEAD_SPCS
1901  FORMAT(/'C', 1X, 'End of ', A34, 1X, 28('-'))
      WRITE(EXUNIT_ADVS, 1903) EXHEAD_ADVS
1903  FORMAT(/'C', 1X, 'End of ', A33, 1X, 29('-'))

      CLOSE(EXUNIT_SPCS)
      CLOSE(EXUNIT_ADVS)

      RETURN
      END
