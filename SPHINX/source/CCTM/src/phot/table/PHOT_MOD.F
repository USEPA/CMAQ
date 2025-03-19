
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

C $Header$

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE PHOT_MOD


      IMPLICIT NONE
 
 
      REAL, ALLOCATABLE  :: RJ    ( :, :, :, : ) ! average grid cell J-values (min-1)
      REAL, ALLOCATABLE  :: RJ_RES( :, :, :, : ) ! resolved cloud J-values (min-1)
      REAL, ALLOCATABLE  :: RJ_SUB( :, :, :, : ) ! subgrid cloud J-values (min-1)
      REAL, ALLOCATABLE  :: CFRAC_2D_AVE( :,: )  ! layer averaged 3D fractional cloud coverage


!      REAL, ALLOCATABLE  :: ETOT_SFC_WL ( :,:,: )  ! total downward irradiance at sfc [ Watts / m**2  ]
      
!***Indices for specific photolysis rates 

      INTEGER :: LNO2            = 0 
      INTEGER :: LO3O1D          = 0
      INTEGER :: LO3O3P          = 0
      INTEGER :: LACETONE        = 0
      INTEGER :: LKETONE         = 0
      INTEGER :: LMGLY_ADJ       = 0
      INTEGER :: LMGLY_ABS       = 0
      INTEGER :: LHCHOR_06       = 0
      INTEGER :: LH2O2           = 0
      INTEGER :: LHNO3           = 0
      INTEGER :: LACETONE_CO     = 0 
      INTEGER :: LACETONE_CH3CO  = 0
      
      ! Variables passed to the PM diagnostic output module, ELMO
      REAL, ALLOCATABLE, SAVE :: ELMO_AOD_550( :,: )   ! total aerosol optical depth at 550 nm (Angstrom Interpolation)
      REAL, ALLOCATABLE, SAVE :: ELMO_EXT_550( :,:,: ) ! aerosol extinction at 550 nm for layer [1/m]

      CONTAINS
      
      SUBROUTINE INIT_PHOT_SHARED()

          USE RXNS_DATA            ! chemistry varaibles and data
          USE GRID_CONF            ! horizontal & vertical domain specifications
          USE UTILIO_DEFN
! Purpose initialized arrays shared by other science processes      
          IMPLICIT NONE
!Arguments: None

!Local:          
          INTEGER          :: ALLOCSTAT
          INTEGER          :: JP
          CHARACTER( 240 ) :: XMSG = ' '
          LOGICAL          :: ERROR_FLAG = .FALSE.

          LOGICAL, SAVE    :: INITIALIZED = .FALSE.
          
          IF( INITIALIZED )RETURN
          
          INITIALIZED = .TRUE.

          ALLOCATE( RJ      ( NCOLS, NROWS, NLAYS, NPHOTAB ),
     &              RJ_RES  ( NCOLS, NROWS, NLAYS, NPHOTAB ),
     &              RJ_SUB  ( NCOLS, NROWS, NLAYS, NPHOTAB ), STAT = ALLOCSTAT )
          IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating photolysis rate arrays'
              CALL M3EXIT ( 'INIT_PHOT_SHARED', 0, 0, XMSG, XSTAT1 )
          END IF

!          ALLOCATE( ETOT_SFC_WL( NCOLS,NROWS,NWL ), STAT = ALLOCSTAT )
!          IF ( ALLOCSTAT .NE. 0 ) THEN
!              XMSG = 'Failure allocating irradiance rate array'
!              CALL M3EXIT ( 'INIT_PHOT_SHARED', 0, 0, XMSG, XSTAT1 )
!          END IF

          RJ = 0.0; RJ_RES = 0.0; RJ_SUB = 0.0
!          ETOT_SFC_WL = 0.0

          DO JP = 1, NPHOTAB
             SELECT CASE ( PHOTAB( JP ) )
!***check to see if this photolysis reaction is a special case.
                CASE( 'O3O3P', 'O3O3P_SAPRC99', 'O3O3P_06', 'O3_O3P_IUPAC04', 'O3O3P_NASA06', 'O3_O3P_IUPAC10' )
                    LO3O3P = JP
                CASE( 'NO2', 'NO2_SAPRC99', 'NO2_06', 'NO2_RACM2', 'NO2_IUPAC10' )
                    LNO2 = JP
                CASE( 'O3O1D',  'O3O1D_SAPRC99' , 'O3O1D_06', 'O3_O1D_IUPAC04', 'O3O1D_NASA06', 'O3_O1D_IUPAC10' )
                    LO3O1D = JP
                CASE( 'KETONE', 'KET_RACM2' )
                    LKETONE   = JP
                CASE( 'MGLY_ADJ' )
                    LMGLY_ADJ = JP
                CASE(  'MGLY_ABS' )
                    LMGLY_ABS = JP
                CASE( 'ACETONE', 'CH3COCH3_RACM2', 'ACET_IUPAC10' )
                    LACETONE  = JP
                CASE( 'HCHOR_06', 'HCHO_R_SAPRC99', 'HCHO_RAD_RACM2', 'FORM_R_IUPAC10', 'HCHO_R_MCMv32' )
                    LHCHOR_06 = JP
                CASE( 'ACET_CH3CO_CRI' )
                    LACETONE_CH3CO   = JP
                CASE( 'H2O2', 'H2O2_SAPRC99', 'H2O2_RACM2', 'H2O2_IUPAC10' )
                   LH2O2 = JP
                CASE( 'HNO3', 'HNO3_IUPAC04', 'HNO3_IUPAC10', 'HNO3_RACM2' )
                   LHNO3 = JP
              END SELECT
          END DO
           
!           IF ( LNO2   .EQ. 0 ) THEN
!              XMSG = 'NO2 photolysis not found in the mechanism! '
!              CALL M3WARN ( 'INIT_PHOT_SHARED', 0, 0, XMSG )
!           END IF
!           IF ( LO3O1D .EQ. 0 ) THEN
!              XMSG = 'O(1D) production by photolysis not found in the mechanism! '
!              CALL M3WARN ( 'INIT_PHOT_SHARED', 0, 0, XMSG )
!           END IF
!           IF ( LO3O3P .EQ. 0 ) THEN
!              MSG = 'O(3P) production by photolysis not found in the mechanism! '
!              CALL M3WARN ( 'INIT_PHOT_SHARED', 0, 0, XMSG )
!           END IF
           IF ( LH2O2 .EQ. 0 ) THEN
              XMSG = 'H2O2 photolysis not found in the mechanism! '
              ERROR_FLAG = .TRUE.
              CALL M3WARN ( 'INIT_PHOT_SHARED', 0, 0, XMSG )
           END IF
           IF ( LHNO3 .EQ. 0 ) THEN
              XMSG = 'HNO3 photolysis not found in the mechanism! '
              ERROR_FLAG = .TRUE.
              CALL M3WARN ( 'INIT_PHOT_SHARED', 0, 0, XMSG )
           END IF

          IF( ERROR_FLAG )THEN
            XMSG = 'The above fatal error(s) found initializing'
            CALL M3EXIT ( 'INIT_PHOT_SHARED', 0, 0, XMSG, XSTAT1 )
          END IF

           
          
      END SUBROUTINE INIT_PHOT_SHARED
      

      END MODULE PHOT_MOD
