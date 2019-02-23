
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
C @(#)LKUPSPEC.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.LKUPSPEC.F 02 Jan 1997 15:26:49

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE LKUPSPEC ( NS, SPECIES, SPCLIS, NXX, SPC1RX, NSPEC )

C=======================================================================
C Finds the index NSPEC of a species SPECIES in list SPCLIS if it exists.
C Otherwise, adds the species to SPCLIS and udates SPCLIS length NS.
 
C Also determines the index of the reaction that finds SPECIES first -
C in SPC1RX
C=======================================================================
      USE MECHANISM_DATA
      
      IMPLICIT NONE
! Arguments:
      INTEGER,         INTENT(INOUT) :: NS
      INTEGER,         INTENT( OUT ) :: NSPEC
      INTEGER,         INTENT(INOUT) :: SPC1RX( : )
      INTEGER,         INTENT(  IN ) :: NXX
      CHARACTER*( * ), INTENT(  IN ) :: SPECIES
      CHARACTER*( * ), INTENT(INOUT) :: SPCLIS( : )
! Local:
      INTEGER :: ISPC

      DO ISPC = 1, NS
         IF ( SPECIES .EQ. SPCLIS( ISPC ) ) THEN   ! found
            NSPEC = ISPC
            RETURN
         END IF
101   END DO
      NS = NS + 1                           ! not found
      IF ( NS .GT. MAXSPEC ) THEN
         WRITE( *,2001 ) MAXSPEC
         STOP
      END IF         
      SPCLIS( NS ) = SPECIES
      NSPEC = NS
      SPC1RX( NS ) = NS
      RETURN
2001  FORMAT( / 5X, '*** ERROR: ',
     &        'Maximum number of species = ', I3, ' exceeded' )
      END
