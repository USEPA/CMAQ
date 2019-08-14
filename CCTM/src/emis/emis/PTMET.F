
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
      MODULE PTMET

C-----------------------------------------------------------------------
C Function: 3d point source emissions met data

C Revision History:
C     20 Nov 2007 J.Young: initial implementation
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C     27 Jul 2011 D.Wong: removed calculation which extends SRTCOL and STRTROW
C                         to the west and south direction in subroutines: 
C                         READMC2, READMC3, and READMD3
C     12 Aug 2015 D.Wong: - Used assumed shape array declaration and declared 
C                           associated subroutines in INTERFACE block
C                         - Replaced BMATVEC with BMATVECN which will call
C                           with a 1d or 2d argument subroutine by F90 
C                           poly-morphism feature
C                         - Based on the condition of MY_NSRC ( > 0 or not) to 
C                           determine execution of certain section of code or not
C                         - fixed a bug of incorrect assignment:
C                           PTMET_DATA%LEN1 = EMLAYS, PTMET_DATA%LEN2 = MSRC
C     01 Feb 2019 D.Wong: - Implemented centralized I/O approach and removed 
C                           ZSTATIC implementation
C     05 Aug 2019 D.Wong: - Used met information in the cell where a point source resides
C                           rather than using bi-linear interpolation (call BMATVECN) to
C                           obtain met information for that point source

C-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE UDTYPES, ONLY: PTMET_TYPE
      USE EMIS_VARS, ONLY : EMLAYS

      IMPLICIT NONE

c     TYPE :: PTMET_TYPE
c        INTEGER          :: LEN1, LEN2
C Allocatable per-source meteorology variables
c        REAL,    POINTER :: HFX( : )        ! sensible heat flux [watts/m**2]
c        REAL,    POINTER :: HMIX( : )       ! mixing height [m]
c        REAL,    POINTER :: TSFC( : )       ! surface temperature [degK]
c        REAL,    POINTER :: USTAR( : )      ! friction velocity [m/s]
c        REAL,    POINTER :: PRSFC( : )      ! surface pressure [Pa]
C Allocatable temporary per-layer variables from 1:EMLAYS
c        REAL,    POINTER :: WSPD( : )       ! wind speed [m/s]
c        REAL,    POINTER :: DTHDZ( : )      ! virtual pot temp gradient [degK/m]
C Allocatable temporary per-layer variables from 0:EMLAYS
c        REAL,    POINTER :: PRESF( : )      ! pressure at full-levels
c        REAL,    POINTER :: ZZF( : )        ! per src elevn at full-levels
c Allocatable per-source and per layer meteorology variables.
C Dimensioned by layers, then sources
c!       REAL,    POINTER :: DDZH ( :,: )    ! 1/( ZH(L) - ZH(L-1) )
c        REAL,    POINTER :: DDZF ( :,: )    ! 1/( ZF(L) - ZF(L-1) )
c        REAL,    POINTER :: PRES ( :,: )    ! pressure [Pa]
c        REAL,    POINTER :: DENS ( :,: )    ! air density [kg/m**3]
c        REAL,    POINTER :: QV   ( :,: )    ! moisture mixing ratio [kg/kg]
c        REAL,    POINTER :: TA   ( :,: )    ! temperature [degK]
c        REAL,    POINTER :: UWIND( :,: )    ! x-component wind speed [m/s]
c        REAL,    POINTER :: VWIND( :,: )    ! y-component wind speed [m/s]
c        REAL,    POINTER :: ZF   ( :,: )    ! full layer height [m]
c        REAL,    POINTER :: ZH   ( :,: )    ! mid layer height [m]
c        REAL,    POINTER :: ZSTK ( :,: )    ! ZF( L,S ) - STKHT(S) [m]
c     END TYPE PTMET_TYPE

      TYPE( PTMET_TYPE ), ALLOCATABLE, SAVE :: PTMET_DATA( : )

C cross-point surface met file name
      CHARACTER(  16 ),              SAVE :: MC2NAME
C cross-point layered met file name
      CHARACTER(  16 ),              SAVE :: MC3NAME
C dot-point layered met file name
      CHARACTER(  16 ),              SAVE :: MD3NAME

C-----------------------------------------------------------------------
      CONTAINS

         FUNCTION PTMET_INIT ( ) RESULT ( SUCCESS )

         USE m3utilio
         USE STK_PRMS, ONLY: MY_NSRC         

         IMPLICIT NONE

         LOGICAL               :: SUCCESS                          

         INTEGER N, MSRC, IOS
         CHARACTER( 16 ) :: PNAME = 'PTMET_INIT'   ! procedure name

         SUCCESS = .TRUE.
         ALLOCATE( PTMET_DATA( NPTGRPS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'PTMET_DATA', PNAME )

         DO N = 1, NPTGRPS

            MSRC = MY_NSRC( N )
            PTMET_DATA(N)%LEN1 = EMLAYS; PTMET_DATA(N)%LEN2 = MSRC

            IF ( MY_NSRC( N ) .GT. 0 ) THEN
C Allocate per-source arrays
               ALLOCATE( PTMET_DATA( N )%HFX  ( MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'HFX', PNAME )
               ALLOCATE( PTMET_DATA( N )%HMIX ( MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'HMIX', PNAME )
               ALLOCATE( PTMET_DATA( N )%TSFC ( MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'TSFC', PNAME )
               ALLOCATE( PTMET_DATA( N )%USTAR( MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'USTAR', PNAME )
               ALLOCATE( PTMET_DATA( N )%PRSFC( MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'PRSFC', PNAME )

C Allocate per-source and per-layer arrays
               ALLOCATE( PTMET_DATA( N )%DDZF ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'DDZF', PNAME )
               ALLOCATE( PTMET_DATA( N )%PRES ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'PRES', PNAME )
               ALLOCATE( PTMET_DATA( N )%DENS ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'DENS', PNAME )
               ALLOCATE( PTMET_DATA( N )%QV   ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'QV', PNAME )
               ALLOCATE( PTMET_DATA( N )%TA   ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'TA', PNAME )
               ALLOCATE( PTMET_DATA( N )%UWIND( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'UWIND', PNAME )
               ALLOCATE( PTMET_DATA( N )%VWIND( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'VWIND', PNAME )
               ALLOCATE( PTMET_DATA( N )%ZF   ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ZF', PNAME )
               ALLOCATE( PTMET_DATA( N )%ZH   ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ZH', PNAME )
               ALLOCATE( PTMET_DATA( N )%ZSTK ( EMLAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ZSTK', PNAME )

C Allocate per-layer arrays from 1:EMLAYS
               ALLOCATE( PTMET_DATA( N )%WSPD ( EMLAYS ), STAT=IOS )
               CALL CHECKMEM( IOS, 'WSPD', PNAME )
               ALLOCATE( PTMET_DATA( N )%DTHDZ( EMLAYS ), STAT=IOS )
               CALL CHECKMEM( IOS, 'DTHDZ', PNAME )

C Allocate per-layer arrays from 0:EMLAYS
               ALLOCATE( PTMET_DATA( N )%PRESF( 0:EMLAYS ), STAT=IOS )
               CALL CHECKMEM( IOS, 'PRESF', PNAME )
               ALLOCATE( PTMET_DATA( N )%ZZF  ( 0:EMLAYS ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ZZF', PNAME )
            END IF ! (MY_NSRC( N ) > 0)

         END DO

         END FUNCTION PTMET_INIT 

C-----------------------------------------------------------------------

         SUBROUTINE PTMET_CONVT( JDATE, JTIME )

         USE STK_PRMS, ONLY: MY_NSRC, SOURCE, MY_STKCOL, MY_STKROW
         USE stack_group_data_module, ONLY: STKHT
         USE CENTRALIZED_IO_MODULE, only : interpolate_var
         USE HGRD_DEFN, only : ncols, nrows
         USE VGRD_DEFN, only : nlays

         IMPLICIT NONE

         INTEGER, INTENT( IN ) :: JDATE, JTIME

         INTEGER N, MSRC, S
         CHARACTER( 16 ) :: PNAME = 'PTMET_CONVT'   ! procedure name

         integer :: myc, myr, stat
         real, allocatable, save:: loc_zh(:,:,:),
     &                             loc_zf(:,:,:),
     &                             loc_hfx(:,:),
     &                             loc_pbl(:,:),
     &                             loc_temp2(:,:),
     &                             loc_ustar(:,:),
     &                             loc_prsfc(:,:),
     &                             loc_ta(:,:,:),
     &                             loc_qv(:,:,:),
     &                             loc_pres(:,:,:),
     &                             loc_dens(:,:,:),
     &                             loc_uwind(:,:,:),
     &                             loc_vwind(:,:,:)

         INTERFACE
            SUBROUTINE DELTA_ZS( EMLAYS, MY_NSRC, SRC_MAP, STKHT, ZF, ZSTK, DDZF )
               INTEGER, INTENT( IN )  :: EMLAYS, MY_NSRC
               INTEGER, INTENT( IN )  :: SRC_MAP( : )
               REAL,    INTENT( IN )  :: STKHT( : )
               REAL,    INTENT( IN )  :: ZF  ( :,: )
               REAL,    INTENT( OUT ) :: ZSTK( :,: )
               REAL,    INTENT( OUT ) :: DDZF( :,: )
            END SUBROUTINE DELTA_ZS
         END INTERFACE

C-----------------------------------------------------------------------

         if (.not. allocated(loc_zh)) then
            allocate ( loc_zh(NCOLS,NROWS,NLAYS),
     &                 loc_zf(NCOLS,NROWS,NLAYS),
     &                 loc_hfx(NCOLS,NROWS),
     &                 loc_pbl(NCOLS,NROWS),
     &                 loc_temp2(NCOLS,NROWS),
     &                 loc_ustar(NCOLS,NROWS),
     &                 loc_prsfc(NCOLS,NROWS),
     &                 loc_ta(NCOLS,NROWS,NLAYS),
     &                 loc_qv(NCOLS,NROWS,NLAYS),
     &                 loc_pres(NCOLS,NROWS,NLAYS),
     &                 loc_dens(NCOLS,NROWS,NLAYS),
     &                 loc_uwind(NCOLS+1,NROWS+1,NLAYS),
     &                 loc_vwind(NCOLS+1,NROWS+1,NLAYS),
     &                 stat=stat)
         end if

         call interpolate_var ('ZH', jdate, jtime, loc_zh)
         call interpolate_var ('ZF', jdate, jtime, loc_zf)
         call interpolate_var ('HFX', jdate, jtime, loc_hfx)
         call interpolate_var ('PBL', jdate, jtime, loc_pbl)
         call interpolate_var ('TEMP2', jdate, jtime, loc_temp2)
         call interpolate_var ('USTAR', jdate, jtime, loc_ustar)
         call interpolate_var ('PRSFC', jdate, jtime, loc_prsfc)
         call interpolate_var ('TA', jdate, jtime, loc_ta)
         call interpolate_var ('QV', jdate, jtime, loc_qv)
         call interpolate_var ('PRES', jdate, jtime, loc_pres)
         call interpolate_var ('DENS', jdate, jtime, loc_dens)
         call interpolate_var ('UWINDC', jdate, jtime, loc_uwind)
         call interpolate_var ('VWINDC', jdate, jtime, loc_vwind)

         DO N = 1, NPTGRPS

            MSRC = MY_NSRC( N )

            DO S = 1, MY_NSRC (N )

               myc = MY_STKCOL( N )%ARRY( S )
               myr = MY_STKROW( N )%ARRY( S )

               PTMET_DATA( N )%ZH(:,s) = loc_zh(myc, myr, :)

               PTMET_DATA( N )%ZF(:,s) = loc_zf(myc, myr, :)

               PTMET_DATA( N )%HFX(s) = loc_hfx(myc, myr)

               PTMET_DATA( N )%HMIX(s) = loc_pbl(myc, myr)

               PTMET_DATA( N )%TSFC(s) = loc_temp2(myc, myr)

               PTMET_DATA( N )%USTAR(s) = loc_ustar(myc, myr)

               PTMET_DATA( N )%PRSFC(s) = loc_prsfc(myc, myr)

               PTMET_DATA( N )%TA(:,s) = loc_ta(myc, myr, :)

               PTMET_DATA( N )%QV(:,s) = loc_qv(myc, myr, :)

               PTMET_DATA( N )%PRES(:,s) = loc_pres(myc, myr, :)

               PTMET_DATA( N )%DENS(:,s) = loc_dens(myc, myr, :)

               PTMET_DATA( N )%UWIND(:,s) = 0.5*( loc_uwind(myc, myr, :)
     &                                          + loc_uwind(myc+1, myr, :) )
               
               PTMET_DATA( N )%VWIND(:,s) = 0.5*( loc_vwind(myc, myr, :)
     &                                          + loc_vwind(myc, myr+1, :) )
            END DO

            IF ( MY_NSRC( N ) .GT. 0 ) THEN
C Compute ZSTK, DDZF
               CALL DELTA_ZS( EMLAYS, MSRC,
     &                        SOURCE( N )%ARRY, STKHT( N )%ARRY,
     &                        PTMET_DATA( N )%ZF,
     &                        PTMET_DATA( N )%ZSTK,
     &                        PTMET_DATA( N )%DDZF )
            END IF ! MY_NSRC( N ) > 0

         END DO   ! NPTGRPS

         END SUBROUTINE PTMET_CONVT

C-----------------------------------------------------------------------

      END MODULE PTMET
