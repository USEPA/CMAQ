
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/driver/yamo/STD_CONC.F,v 1.7 2011/10/21 16:10:42 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE STD_CONC

      IMPLICIT NONE

C Function: species, layer pointers and definitions for standard CONC
C calculations
C Revision: J.Young 13Sep2011: Increase size of CONC_SPCS dimension -
C           previous revision used dynamic allocation, but GET_ENVLIST
C           was called with the CONC_SPCS argument before it was allocated
C           D.Wong 11 May 2016: Modified code to provide flexibility to handle
C                environment variable CONC_SPCS is set to 'ALL' or not set.

      INTEGER, SAVE :: N_CSPCS    ! Number of species saved to conc file
      CHARACTER( 16 ), SAVE :: CONC_SPCS( 900 ) ! conc file species list
      INTEGER, SAVE :: CONC_BLEV  ! Beginning level saved to conc file
      INTEGER, SAVE :: CONC_ELEV  ! Ending level saved to conc file
      INTEGER, SAVE :: C_NLAYS    ! Number of layers saved to conc file

      INTEGER, SAVE, ALLOCATABLE :: CONC_MAP( : ) ! pointer into CGRID
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: C_GC_SPC( : ) ! pointer into GC_SPC
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: C_AE_SPC( : ) ! pointer into AE_SPC
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: C_NR_SPC( : ) ! pointer into NR_SPC
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: C_TR_SPC( : ) ! pointer into TR_SPC

      REAL, SAVE, ALLOCATABLE :: SGRID( :,:,:,: ) ! conc subset

C species classes configuration for CONC 
      INTEGER, SAVE :: C_GC_STRT
      INTEGER, SAVE :: N_C_GC_SPC
      INTEGER, SAVE :: C_AE_STRT
      INTEGER, SAVE :: N_C_AE_SPC
      INTEGER, SAVE :: C_NR_STRT
      INTEGER, SAVE :: N_C_NR_SPC
      INTEGER, SAVE :: C_TR_STRT
      INTEGER, SAVE :: N_C_TR_SPC

      INTEGER, SAVE :: N_C_AE_SPCD

      CONTAINS

         SUBROUTINE CONC_DEFN ()

         USE HGRD_DEFN             ! horizontal domain specifications
         USE VGRD_DEFN             ! vertical layer specifications
         USE CGRID_SPCS            ! CGRID mechanism species
         USE UTILIO_DEFN

         CHARACTER( 16 ) :: PNAME = 'CONC_DEFN'
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER OFF, VAR, V, NV, LVL
         INTEGER ALLOCSTAT, STATUS
         INTEGER :: JDATE = 0
         INTEGER :: JTIME = 0

         INTEGER, ALLOCATABLE :: GC_MAP( : )
         INTEGER, ALLOCATABLE :: AE_MAP( : )
         INTEGER, ALLOCATABLE :: NR_MAP( : )
         INTEGER, ALLOCATABLE :: TR_MAP( : )

         CHARACTER( 32 ) :: STD_CONC_SPCS  = 'CONC_SPCS'
         CHARACTER( 32 ) :: CONC_BLEV_ELEV = 'CONC_BLEV_ELEV'
         CHARACTER( 16 ) :: V_LIST( 2 )

#ifdef Verbose
         integer logdev
#endif

         INTERFACE
            SUBROUTINE GET_ENVLIST ( ENV_VAR, NVARS, VAL_LIST )
               CHARACTER( * ),  INTENT ( IN )  :: ENV_VAR
               INTEGER,         INTENT ( OUT ) :: NVARS
               CHARACTER( 16 ), INTENT ( OUT ) :: VAL_LIST( : )
            END SUBROUTINE GET_ENVLIST
         END INTERFACE

C-----------------------------------------------------------------------

#ifdef Verbose
         logdev = init3()
#endif

C Retrieve the species saved to the concentration file

         CALL GET_ENVLIST ( STD_CONC_SPCS, N_CSPCS, CONC_SPCS )
         IF ( N_CSPCS .EQ. 0 .OR. CONC_SPCS( 1 ) .EQ. 'ALL' ) THEN
            N_CSPCS = N_GC_CONC + N_AE_CONC + N_NR_CONC + N_TR_SPC
            V = 0
            DO VAR = 1, N_GC_CONC
               V = V + 1
               CONC_SPCS( V ) = GC_CONC( VAR )
            END DO
            DO VAR = 1, N_AE_CONC
               V = V + 1
               CONC_SPCS( V ) = AE_CONC( VAR )
            END DO
            DO VAR = 1, N_NR_CONC
               V = V + 1
               CONC_SPCS( V ) = NR_CONC( VAR )
            END DO
            DO VAR = 1, N_TR_SPC
               V = V + 1
               CONC_SPCS( V ) = TR_SPC( VAR )
            END DO
         END IF

C Retrieve the layer range used in the concentration file

         CALL GET_ENVLIST ( CONC_BLEV_ELEV, NV, V_LIST )
         IF ( NV .LE. 0 ) THEN   ! assume 1:NLAYS
            CONC_BLEV = 1
            CONC_ELEV = NLAYS
         ELSE IF ( NV .NE. 2 ) THEN
            XMSG = 'Environment variable error for ' // CONC_BLEV_ELEV
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         ELSE   ! NV = 2
            READ( V_LIST( 1 ), '( I4 )' ) CONC_BLEV
            READ( V_LIST( 2 ), '( I4 )' ) CONC_ELEV
            IF ( CONC_BLEV .LE. 0 .OR. CONC_ELEV .GT. NLAYS ) THEN
               WRITE( XMSG,'( "Layer range", 2I4, " invalid for this model" )' )
     &         CONC_BLEV, CONC_ELEV
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF
            IF ( CONC_BLEV .NE. 1 ) THEN
               WRITE( XMSG,'( "Layer", I3, " Not 1st layer in CGRID" )' )
     &         CONC_BLEV
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF
         END IF

         C_NLAYS = CONC_ELEV - CONC_BLEV + 1

C Create conc subset array

         ALLOCATE ( SGRID( MY_NCOLS,MY_NROWS,C_NLAYS,N_CSPCS ),
     &              STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'SGRID memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         SGRID = 0.0

C Check species names against include files and create CONC_MAP, and
C get starting index in CGRID and total count for each species class

         ALLOCATE ( CONC_MAP( N_CSPCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'CONC_MAP memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         ALLOCATE ( C_GC_SPC( N_CSPCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'C_GC_SPC memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         ALLOCATE ( C_AE_SPC( N_CSPCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'C_AE_SPC memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         ALLOCATE ( C_NR_SPC( N_CSPCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'C_NR_SPC memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         ALLOCATE ( C_TR_SPC( N_CSPCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'C_TR_SPC memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         ALLOCATE ( GC_MAP( N_CSPCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'GC_MAP memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         ALLOCATE ( AE_MAP( N_CSPCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'AE_MAP memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         ALLOCATE ( NR_MAP( N_CSPCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'NR_MAP memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         ALLOCATE ( TR_MAP( N_CSPCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'TR_MAP memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         N_C_GC_SPC = 0
         N_C_AE_SPC = 0
         N_C_NR_SPC = 0
         N_C_TR_SPC = 0

         DO VAR = 1, N_CSPCS
            V = INDEX1 ( CONC_SPCS( VAR ), N_GC_SPC, GC_SPC )
            IF ( V .GT. 0 ) THEN
               N_C_GC_SPC = N_C_GC_SPC + 1
               C_GC_SPC( N_C_GC_SPC ) = CONC_SPCS( VAR )
               OFF = 0
               GC_MAP( N_C_GC_SPC ) = V + OFF
            ELSE
               V = INDEX1 ( CONC_SPCS( VAR ), N_AE_SPC, AE_SPC )
               IF ( V .GT. 0 ) THEN
                  N_C_AE_SPC = N_C_AE_SPC + 1
                  OFF = N_GC_SPC + 1   ! accounts for advected density
                  C_AE_SPC( N_C_AE_SPC ) = CONC_SPCS( VAR )
                  AE_MAP( N_C_AE_SPC ) = V + OFF
               ELSE
                  V = INDEX1 ( CONC_SPCS( VAR ), N_NR_SPC, NR_SPC )
                  IF ( V .GT. 0 ) THEN
                     N_C_NR_SPC = N_C_NR_SPC + 1
                     OFF = N_GC_SPC + 1 + N_AE_SPC
                     C_NR_SPC( N_C_NR_SPC ) = CONC_SPCS( VAR )
                     NR_MAP( N_C_NR_SPC ) = V + OFF
                  ELSE
                     V = INDEX1 ( CONC_SPCS( VAR ), N_TR_SPC, TR_SPC )
                     IF ( V .GT. 0 ) THEN
                        N_C_TR_SPC = N_C_TR_SPC + 1
                        OFF = N_GC_SPC + 1 + N_AE_SPC + N_NR_SPC
                        C_TR_SPC( N_C_TR_SPC ) = CONC_SPCS( VAR )
                        TR_MAP( N_C_TR_SPC ) = V + OFF
                     ELSE
                        XMSG = 'Variable ' // CONC_SPCS( VAR )
     &                  // ' incorrect for this model'
                        CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
                     END IF
                  END IF
               END IF
            END IF
         END DO

         C_GC_STRT = 1
         C_AE_STRT = C_GC_STRT + N_C_GC_SPC
         C_NR_STRT = C_AE_STRT + N_C_AE_SPC
         C_TR_STRT = C_NR_STRT + N_C_NR_SPC

         IF ( N_C_AE_SPC .GT. 0 ) THEN
            N_C_AE_SPCD = N_C_AE_SPC
         ELSE
            N_C_AE_SPCD = 1
         END IF

         VAR = 0
         DO V = 1, N_C_GC_SPC
            VAR = VAR + 1
            CONC_MAP( VAR ) = GC_MAP( V )
#ifdef Verbose
            write( logdev,* ) '<<>>var, CONC_MAP, _SPCS: ',
     &                         VAR, CONC_MAP( VAR ), CONC_SPCS( VAR )
#endif
         END DO

         DO V = 1, N_C_AE_SPC
            VAR = VAR + 1
            CONC_MAP( VAR ) = AE_MAP( V )
#ifdef Verbose
            write( logdev,* ) '<<>>var, CONC_MAP, _SPCS: ',
     &                         VAR, CONC_MAP( VAR ), CONC_SPCS( VAR )
#endif
         END DO

         DO V = 1, N_C_NR_SPC
            VAR = VAR + 1
            CONC_MAP( VAR ) = NR_MAP( V )
#ifdef Verbose
            write( logdev,* ) '<<>>var, CONC_MAP, _SPCS: ',
     &                         VAR, CONC_MAP( VAR ), CONC_SPCS( VAR )
#endif
         END DO

         DO V = 1, N_C_TR_SPC
            VAR = VAR + 1
            CONC_MAP( VAR ) = TR_MAP( V )
#ifdef Verbose
            write( logdev,* ) '<<>>var, CONC_MAP, _SPCS: ',
     &                         VAR, CONC_MAP( VAR ), CONC_SPCS( VAR )
#endif
         END DO

         IF ( N_C_GC_SPC .EQ. 0 ) THEN
            DEALLOCATE ( C_GC_SPC )
            DEALLOCATE ( GC_MAP )
         END IF
         IF ( N_C_AE_SPC .EQ. 0 ) THEN
            DEALLOCATE ( C_AE_SPC )
            DEALLOCATE ( AE_MAP )
         END IF
         IF ( N_C_NR_SPC .EQ. 0 ) THEN
            DEALLOCATE ( C_NR_SPC )
            DEALLOCATE ( NR_MAP )
         END IF
         IF ( N_C_TR_SPC .EQ. 0 ) THEN
            DEALLOCATE ( C_TR_SPC )
            DEALLOCATE ( TR_MAP )
         END IF

         END SUBROUTINE CONC_DEFN

      END MODULE STD_CONC
