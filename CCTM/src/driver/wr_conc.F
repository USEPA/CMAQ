
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WR_CONC ( JDATE, JTIME, TSTEP )

C Revision History:
C   10/13/99 David Wong at LM
C      -- Called from driver, where CGRID is a pointer (subset) of PCGRID.
C         Necessary, to keep from referencing parts of PCGRID that don't
C         belong to CGRID.
C    1/31/2000 Jeff Young
C      -- f90 memory mgmt
C   Jeff - Dec 00 - move CGRID_MAP into f90 module
C   Jeff - Feb 01 - assumed shape arrays
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; DBUFF for WRITE3
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   30 May 05 J.Young: optional save derived vert. vel. to conc file
C   29 Aug 06 J.Young: in conjunction with state CGRID file
C   30 Jan 10 David Wong: modified the code to write one species at a time
C                         in case ALLVAR3 cannot deal with large CONC size
C   14 Sep 10 J.Young: fix ALLVAR3 bug in writing W_VEL
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   11 May 11 D.Wong: - let non I/O PEs open CTM_CONC_1 for read only
C   12 Aug 15 D.Wong: - Replaced MYPE with IO_PE_INCLUSIVE for parallel
C                       I/O implementation
C                     - Added code to allow non IO processor to open
C                       CTM_CONC_1
C   01 Feb 19 D.Wong: - Implemented centralized I/O approach
C   Sept 19 F. Sidi: Replaced L_CONC_WVEL(depreciated) with W_VEL 
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE STD_CONC              ! standard CONC
      USE WVEL_DEFN             ! derived vertical velocity component
      USE UTILIO_DEFN
      USE ASX_DATA_MOD, ONLY : MET_DATA
      USE CENTRALIZED_IO_MODULE

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! I/O definitions and declarations

      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C Local variables:
      CHARACTER( 16 ) :: PNAME = 'WR_CONC'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      ALLOCSTAT

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      INTEGER      C, R, K, L, V, VAR, SPC   ! loop induction variables

      CHARACTER( 16 ), ALLOCATABLE, SAVE :: VNAME( : )
      INTEGER, SAVE :: NVARS

C-----------------------------------------------------------------------

#ifndef mpas
      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

C open conc file for update

         IF ( .NOT. IO_PE_INCLUSIVE ) THEN
            IF ( .NOT. OPEN3( CTM_CONC_1, FSREAD3, PNAME ) ) THEN
               XMSG = 'Could not open ' // CTM_CONC_1 // ' file for update'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

         IF ( .NOT. DESC3( CTM_CONC_1 ) ) THEN
            XMSG = 'Could not get file description from ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
         ! Retrieve number and values of variable names
         NVARS = NVARS3D
         ALLOCATE ( VNAME( NVARS ), STAT=ALLOCSTAT )
         VNAME = VNAME3D( 1:NVARS )

      END IF   ! firstime

      VAR = 0

      ! Write Gas-Phase Species to CONC File
      DO SPC = 1, N_C_GC_SPC
         VAR = VAR + 1
         IF ( .NOT. WRITE3( CTM_CONC_1, C_GC_SPC( SPC ),
     &      JDATE, JTIME, SGRID( :,:,:,VAR ) ) ) THEN
            XMSG = 'Could not write '
     &           // TRIM( C_GC_SPC( SPC ) )
     &           // ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END DO
 
      ! Write Aerosol Species Concentrations to CONC File
      DO SPC = 1, N_C_AE_SPC
         VAR = VAR + 1
         IF ( .NOT. WRITE3( CTM_CONC_1, C_AE_SPC( SPC ),
     &      JDATE, JTIME, SGRID( :,:,:,VAR ) ) ) THEN
            XMSG = 'Could not write '
     &           // TRIM( C_AE_SPC( SPC ) )
     &           // ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END DO

      ! Write Nonreactive Species Concentrations to CONC File
      DO SPC = 1, N_C_NR_SPC
         VAR = VAR + 1
         IF ( .NOT. WRITE3( CTM_CONC_1, C_NR_SPC( SPC ),
     &      JDATE, JTIME, SGRID( :,:,:,VAR ) ) ) THEN
            XMSG = 'Could not write '
     &           // TRIM( C_NR_SPC( SPC ) )
     &           // ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END DO

      ! Write Tracer Species Concentrations to CONC File
      DO SPC = 1, N_C_TR_SPC
         VAR = VAR + 1
         IF ( .NOT. WRITE3( CTM_CONC_1, C_TR_SPC( SPC ),
     &      JDATE, JTIME, SGRID( :,:,:,VAR ) ) ) THEN
            XMSG = 'Could not write '
     &           // TRIM( C_TR_SPC( SPC ) )
     &           // ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END DO

      ! Write Vertical Velocity to Output CONC file
      IF ( W_VEL ) THEN
         IF ( .NOT. WRITE3( CTM_CONC_1, 'W_VEL',
     &      JDATE, JTIME, WVEL( :,:,CONC_BLEV:CONC_ELEV ) ) ) THEN
            XMSG = 'Could not write W_VEL to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF   ! W_VEL

      ! Write Relative Humidity to Output CONC file
      IF ( L_CONC_RH ) THEN
      IF ( .NOT. WRITE3( CTM_CONC_1, 'RH',
     &     JDATE, JTIME, MET_DATA%RH( :,:,CONC_BLEV:CONC_ELEV ) ) ) THEN
         XMSG = 'Could not write RH to ' // CTM_CONC_1
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
      END IF

      ! Write Temperature to Output CONC file
      IF ( L_CONC_TA ) THEN
      IF ( .NOT. WRITE3( CTM_CONC_1, 'TA',
     &     JDATE, JTIME, MET_DATA%TA( :,:,CONC_BLEV:CONC_ELEV ) ) ) THEN
         XMSG = 'Could not write Temperature to ' // CTM_CONC_1
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
      END IF

      ! Write Pressure to Output CONC file
      IF ( L_CONC_PRES ) THEN
      IF ( .NOT. WRITE3( CTM_CONC_1, 'PRES',
     &     JDATE, JTIME, MET_DATA%PRES( :,:,CONC_BLEV:CONC_ELEV ) ) ) THEN
         XMSG = 'Could not write Pressure to ' // CTM_CONC_1
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
      END IF

! Write Summary Message for Output Procedure
      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', CTM_CONC_1,
     &      'for date and time', JDATE, JTIME
#endif

      RETURN 
      END
