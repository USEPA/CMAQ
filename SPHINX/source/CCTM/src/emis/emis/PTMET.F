
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
C                           PTMET_DATA%LEN1 = DESID_LAYS, PTMET_DATA%LEN2 = MSRC
C     01 Feb 2019 D.Wong: - Implemented centralized I/O approach and removed 
C                           ZSTATIC implementation
C     05 Aug 2019 D.Wong: - Used met information in the cell where a point source resides
C                           rather than using bi-linear interpolation (call BMATVECN) to
C                           obtain met information for that point source
C     06 Aug 2019 D.Wong: - For two-way model, use UWIND/VWIND (wind U/V component on the
C                           mass point and in offline CMAQ model, use average value of
C                           UWINDC/VWINDC to approximate UWIND/VWIND. Hence no longer
C                           need UWINDC_AVAIL/VWINDC_AVAIL
C-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE UDTYPES, ONLY: PTMET_TYPE
      USE DESID_VARS, ONLY : DESID_LAYS

      IMPLICIT NONE

c     TYPE :: PTMET_TYPE
c        INTEGER          :: LEN1, LEN2
C Allocatable per-source meteorology variables
c        REAL,    POINTER :: HFX( : )        ! sensible heat flux [watts/m**2]
c        REAL,    POINTER :: HMIX( : )       ! mixing height [m]
c        REAL,    POINTER :: TSFC( : )       ! surface temperature [degK]
c        REAL,    POINTER :: USTAR( : )      ! friction velocity [m/s]
c        REAL,    POINTER :: PRSFC( : )      ! surface pressure [Pa]
C Allocatable temporary per-layer variables from 1:DESID_LAYS
c        REAL,    POINTER :: WSPD( : )       ! wind speed [m/s]
c        REAL,    POINTER :: DTHDZ( : )      ! virtual pot temp gradient [degK/m]
C Allocatable temporary per-layer variables from 0:DESID_LAYS
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

         USE UTILIO_DEFN
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
            PTMET_DATA(N)%LEN1 = DESID_LAYS; PTMET_DATA(N)%LEN2 = MSRC

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
               ALLOCATE( PTMET_DATA( N )%DDZF ( DESID_LAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'DDZF', PNAME )
               ALLOCATE( PTMET_DATA( N )%PRES ( DESID_LAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'PRES', PNAME )
               ALLOCATE( PTMET_DATA( N )%DENS ( DESID_LAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'DENS', PNAME )
               ALLOCATE( PTMET_DATA( N )%QV   ( DESID_LAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'QV', PNAME )
               ALLOCATE( PTMET_DATA( N )%TA   ( DESID_LAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'TA', PNAME )
               ALLOCATE( PTMET_DATA( N )%UWIND( DESID_LAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'UWIND', PNAME )
               ALLOCATE( PTMET_DATA( N )%VWIND( DESID_LAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'VWIND', PNAME )
               ALLOCATE( PTMET_DATA( N )%ZF   ( DESID_LAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ZF', PNAME )
               ALLOCATE( PTMET_DATA( N )%ZH   ( DESID_LAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ZH', PNAME )
               ALLOCATE( PTMET_DATA( N )%ZSTK ( DESID_LAYS,MSRC ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ZSTK', PNAME )

C Allocate per-layer arrays from 1:DESID_LAYS
               ALLOCATE( PTMET_DATA( N )%WSPD ( DESID_LAYS ), STAT=IOS )
               CALL CHECKMEM( IOS, 'WSPD', PNAME )
               ALLOCATE( PTMET_DATA( N )%DTHDZ( DESID_LAYS ), STAT=IOS )
               CALL CHECKMEM( IOS, 'DTHDZ', PNAME )

C Allocate per-layer arrays from 0:DESID_LAYS
               ALLOCATE( PTMET_DATA( N )%PRESF( 0:DESID_LAYS ), STAT=IOS )
               CALL CHECKMEM( IOS, 'PRESF', PNAME )
               ALLOCATE( PTMET_DATA( N )%ZZF  ( 0:DESID_LAYS ), STAT=IOS )
               CALL CHECKMEM( IOS, 'ZZF', PNAME )
            END IF ! (MY_NSRC( N ) > 0)

         END DO

         END FUNCTION PTMET_INIT 

C-----------------------------------------------------------------------
#ifdef mpas
         SUBROUTINE PTMET_CONVT_MPAS ( N )

         USE STK_PRMS, ONLY: MY_NSRC, SOURCE, my_nsrc_mesh_index
         USE stack_group_data_module, ONLY: STKHT
         USE coupler_module

         IMPLICIT NONE

         integer, intent(in) :: n

         INTEGER S

         IF ( MY_NSRC( N ) .GT. 0 ) THEN
            DO S = 1, MY_NSRC( N )
               PTMET_DATA( N )%USTAR( S )  = g2ddata(my_nsrc_mesh_index(s, n),1,ustar_ind)
               PTMET_DATA( N )%HFX( S )    = g2ddata(my_nsrc_mesh_index(s, n),1,hfx_ind)
               PTMET_DATA( N )%HMIX( S )   = g2ddata(my_nsrc_mesh_index(s, n),1,pbl_ind)

               PTMET_DATA( N )%TA( :,S )   = g3ddata(my_nsrc_mesh_index(s, n),1,:,temp_ind)
               PTMET_DATA( N )%ZH( :,S )   = g3ddata(my_nsrc_mesh_index(s, n),1,:,zh_ind)
               PTMET_DATA( N )%ZF( :,S )   = g3ddata(my_nsrc_mesh_index(s, n),1,:,zf_ind)
               PTMET_DATA( N )%DENS( :,S ) = g3ddata(my_nsrc_mesh_index(s, n),1,:,dens_ind)
            END DO
         END IF

         END SUBROUTINE PTMET_CONVT_MPAS

#else
C-----------------------------------------------------------------------

         SUBROUTINE PTMET_CONVT( JDATE, JTIME, N )

         USE STK_PRMS, ONLY: MY_NSRC, SOURCE, MY_STKCOL, MY_STKROW
         USE STACK_GROUP_DATA_MODULE, ONLY: STKHT
         USE CENTRALIZED_IO_MODULE, ONLY : INTERPOLATE_VAR
         USE HGRD_DEFN, ONLY : NCOLS, NROWS
         USE VGRD_DEFN, ONLY : NLAYS

         IMPLICIT NONE

         INTEGER, INTENT( IN ) :: JDATE, JTIME
         INTEGER, INTENT( IN ) :: N              ! Point Source File Number

         INTEGER         :: L, MSRC, S
         CHARACTER( 16 ) :: PNAME = 'PTMET_CONVT'   ! PROCEDURE NAME

         INTEGER :: MYC, MYR, STAT
         
         REAL, ALLOCATABLE, SAVE:: LOC_ZH(:,:,:),
     &                             LOC_ZF(:,:,:),
     &                             LOC_HFX(:,:),
     &                             LOC_PBL(:,:),
     &                             LOC_TEMP2(:,:),
     &                             LOC_USTAR(:,:),
     &                             LOC_PRSFC(:,:),
     &                             LOC_TA(:,:,:),
     &                             LOC_QV(:,:,:),
     &                             LOC_PRES(:,:,:),
     &                             LOC_DENS(:,:,:),
     &                             LOC_UWIND(:,:,:),
     &                             LOC_VWIND(:,:,:)

         LOGICAL, SAVE :: INITIALIZE = .TRUE.
         
         INTERFACE
            SUBROUTINE DELTA_ZS( DESID_LAYS, MY_NSRC, SRC_MAP, STKHT, ZF, ZSTK, DDZF )
               INTEGER, INTENT( IN )  :: DESID_LAYS, MY_NSRC
               INTEGER, INTENT( IN )  :: SRC_MAP( : )
               REAL,    INTENT( IN )  :: STKHT( : )
               REAL,    INTENT( IN )  :: ZF  ( :,: )
               REAL,    INTENT( OUT ) :: ZSTK( :,: )
               REAL,    INTENT( OUT ) :: DDZF( :,: )
            END SUBROUTINE DELTA_ZS
         END INTERFACE

C-----------------------------------------------------------------------

         IF ( INITIALIZE ) THEN
         
            ALLOCATE ( LOC_ZH(NCOLS,NROWS,NLAYS),
     &                 LOC_ZF(NCOLS,NROWS,NLAYS),
     &                 LOC_HFX(NCOLS,NROWS),
     &                 LOC_PBL(NCOLS,NROWS),
     &                 LOC_TEMP2(NCOLS,NROWS),
     &                 LOC_USTAR(NCOLS,NROWS),
     &                 LOC_PRSFC(NCOLS,NROWS),
     &                 LOC_TA(NCOLS,NROWS,NLAYS),
     &                 LOC_QV(NCOLS,NROWS,NLAYS),
     &                 LOC_PRES(NCOLS,NROWS,NLAYS),
     &                 LOC_DENS(NCOLS,NROWS,NLAYS),
     &                 LOC_UWIND(NCOLS+1,NROWS+1,NLAYS),
     &                 LOC_VWIND(NCOLS+1,NROWS+1,NLAYS),
     &                 STAT=STAT)
     
           INITIALIZE = .FALSE.
           
         END IF

         CALL INTERPOLATE_VAR ('ZH', JDATE, JTIME, LOC_ZH)
         CALL INTERPOLATE_VAR ('ZF', JDATE, JTIME, LOC_ZF)
         CALL INTERPOLATE_VAR ('HFX', JDATE, JTIME, LOC_HFX)
         CALL INTERPOLATE_VAR ('PBL', JDATE, JTIME, LOC_PBL)
         CALL INTERPOLATE_VAR ('TEMP2', JDATE, JTIME, LOC_TEMP2)
         CALL INTERPOLATE_VAR ('USTAR', JDATE, JTIME, LOC_USTAR)
         CALL INTERPOLATE_VAR ('PRSFC', JDATE, JTIME, LOC_PRSFC)
         CALL INTERPOLATE_VAR ('TA', JDATE, JTIME, LOC_TA)
         CALL INTERPOLATE_VAR ('QV', JDATE, JTIME, LOC_QV)
         CALL INTERPOLATE_VAR ('PRES', JDATE, JTIME, LOC_PRES)
         CALL INTERPOLATE_VAR ('DENS', JDATE, JTIME, LOC_DENS)
#ifdef twoway
         CALL INTERPOLATE_VAR ('UWIND', JDATE, JTIME, LOC_UWIND)
         CALL INTERPOLATE_VAR ('VWIND', JDATE, JTIME, LOC_VWIND)
#else
         CALL INTERPOLATE_VAR ('UWINDC', JDATE, JTIME, LOC_UWIND)
         CALL INTERPOLATE_VAR ('VWINDC', JDATE, JTIME, LOC_VWIND)
#endif
         !DO N = 1, NPTGRPS

            MSRC = MY_NSRC( N )

            DO S = 1, MY_NSRC (N )

               MYC = MY_STKCOL( N )%ARRY( S )
               MYR = MY_STKROW( N )%ARRY( S )

               PTMET_DATA( N )%HFX(S) = LOC_HFX(MYC, MYR)

               PTMET_DATA( N )%HMIX(S) = LOC_PBL(MYC, MYR)

               PTMET_DATA( N )%TSFC(S) = LOC_TEMP2(MYC, MYR)

               PTMET_DATA( N )%USTAR(S) = LOC_USTAR(MYC, MYR)

               PTMET_DATA( N )%PRSFC(S) = LOC_PRSFC(MYC, MYR)

               DO L = 1, NLAYS
               
                  PTMET_DATA( N )%ZH(L,S)    = LOC_ZH(MYC, MYR, L)
                  
                  PTMET_DATA( N )%ZF(L,S)    = LOC_ZF(MYC, MYR, L)
                  
                  PTMET_DATA( N )%TA(L,S)    = LOC_TA(MYC, MYR, L)
                  
                  PTMET_DATA( N )%QV(L,S)    = LOC_QV(MYC, MYR, L)
                  
                  PTMET_DATA( N )%PRES(L,S)  = LOC_PRES(MYC, MYR, L)
                  
                  PTMET_DATA( N )%DENS(L,S)  = LOC_DENS(MYC, MYR, L)
#ifdef twoway
                  PTMET_DATA( N )%UWIND(:,S) = LOC_UWIND(MYC, MYR, L)

                  PTMET_DATA( N )%VWIND(:,S) = LOC_VWIND(MYC, MYR, L)
#else
                  PTMET_DATA( N )%UWIND(L,S) = 0.5*( LOC_UWIND(MYC, MYR, L)
     &                                       +       LOC_UWIND(MYC+1, MYR, L) )
               
                  PTMET_DATA( N )%VWIND(L,S) = 0.5*( LOC_VWIND(MYC, MYR, L)
     &                                       +       LOC_VWIND(MYC, MYR+1, L) )
#endif                
               END DO

            END DO

            IF ( MY_NSRC( N ) .GT. 0 ) THEN ! Compute ZSTK, DDZF
               CALL DELTA_ZS( DESID_LAYS, MSRC,
     &                        SOURCE( N )%ARRY, STKHT( N )%ARRY,
     &                        PTMET_DATA( N )%ZF,
     &                        PTMET_DATA( N )%ZSTK,
     &                        PTMET_DATA( N )%DDZF )
            END IF ! MY_NSRC( N ) > 0

         !END DO   ! NPTGRPS

         END SUBROUTINE PTMET_CONVT
#endif

C-----------------------------------------------------------------------

      END MODULE PTMET
