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

      MODULE SA_DEFN

! KWOK: Define tagging emissions, species, dimensions, etc, based on user-supplied sa_io_list
! KWOK: Created Oct 5, 2010
! 29 Oct 18 L.Zhou, S.Napelenok: update for cmaq 5.3 release
! 35 Mar 19 S.Napelenok: recode REGION handling and clean-up for release
! 

      IMPLICIT NONE

      SAVE

      PUBLIC

      INTEGER ITAG

c...Tagging maps
      CHARACTER( 16 ), ALLOCATABLE :: SPC_NAME( :,: )
      INTEGER, ALLOCATABLE, SAVE   :: SPC_INDEX( :,: )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_CGRtoSA( : )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_AEROtoSA( :,: )
      LOGICAL, ALLOCATABLE, SAVE   :: IS_SPC_AEROSOL( :,: )
      LOGICAL, ALLOCATABLE, SAVE   :: TRANSPORT_SPC ( : )
      INTEGER, ALLOCATABLE, SAVE   :: STREAM_TO_TAG ( :,: )
      INTEGER, ALLOCATABLE, SAVE   :: TAGS_PER_STREAM( : )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_DIFFtoSA( : )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_ADVtoSA( : )
c     INTEGER, ALLOCATABLE, SAVE   :: MAP_DEPVtoSA( : )
 
      INTEGER, ALLOCATABLE, SAVE   :: ISAM_PVO3_MAP( : ) ! potential vorticity map

      INTEGER ::     NSPC_SA
      INTEGER ::     NTAG_SA
      INTEGER ::     NSPC_TRANSPORT

c...Define Allocatable Apportionment Arrays
      REAL, POINTER :: ISAM  ( :,:,:,:,: )
      REAL, ALLOCATABLE :: AISAM ( :,:,:,:,: )
      REAL, ALLOCATABLE :: TOT_SADEP ( :,:,:,: )
      REAL, ALLOCATABLE :: CONV_SADEP ( :,:,:,: )
c     REAL, ALLOCATABLE :: MAPFRAC( :,:,: )
c     REAL, ALLOCATABLE :: MFRC_P( :,:,: )
c     REAL, ALLOCATABLE :: VOC5D ( :,:,:,:,: )
      REAL, ALLOCATABLE :: SA_VDEMIS_CONV( :,:,:,:,: ) ! full emissions array mapped to diffused species
      REAL, ALLOCATABLE :: SA_VDEMIS_CONV_OTHER( :,:,:,: ) ! emissions left for 'OTHRTAG'
      REAL, ALLOCATABLE :: SA_VDEMIS_DIFF( :,:,:,:,: ) 

c...Variables regarding the tag list
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: TAGNAME( : )
      CHARACTER( 96 )                    :: TAGCLASSES = ' '
      CHARACTER( 96 ), ALLOCATABLE, SAVE :: TAGSTREAMS_TEMP( : )
      CHARACTER( 96 ), ALLOCATABLE, SAVE :: TAGSTREAMS( :, : )
      INTEGER, ALLOCATABLE, SAVE         :: TAGSTREAMS_NUM( : )

      CHARACTER( 96 ), ALLOCATABLE, SAVE :: ISAMRGN_TEMP( : )
      CHARACTER( 96 ), ALLOCATABLE, SAVE :: ISAMRGN( :, : )
      INTEGER, ALLOCATABLE, SAVE         :: ISAMRGN_NUM( : )
      INTEGER, ALLOCATABLE, SAVE         :: ISAMRGN_MAP( :, : )

c...Logical values for tagging species
      LOGICAL, SAVE :: L_EC      = .FALSE.
      LOGICAL, SAVE :: L_OC      = .FALSE.
      LOGICAL, SAVE :: L_SFATE   = .FALSE.
      LOGICAL, SAVE :: L_NITRATE = .FALSE.
      LOGICAL, SAVE :: L_NH4     = .FALSE.
      LOGICAL, SAVE :: L_PM25    = .FALSE.
      LOGICAL, SAVE :: L_OZONE   = .FALSE.
      LOGICAL, SAVE :: L_VOC     = .FALSE.
      LOGICAL, SAVE :: L_CHLORINE  = .FALSE.
!      LOGICAL, SAVE :: L_CO      = .FALSE.
!      LOGICAL, SAVE :: L_GAS_HAPS  = .FALSE.
!      LOGICAL, SAVE :: L_AERO_HAPS = .FALSE.
      
      LOGICAL, SAVE :: SA_BIDI = .FALSE.

!      INTEGER,         PARAMETER :: N_AERO_HAPS_LIST = 16       
!      CHARACTER( 16 ), PARAMETER :: AERO_HAPS_LIST(  N_AERO_HAPS_LIST ) =
!     & (/ 'ANI     ',
!     &    'ACR_VI  ',
!     &    'ACR_III ',
!     &    'ABE     ',
!     &    'APB     ',
!     &    'ADE_OTHR',
!     &    'ADE_EC  ',
!     &    'ADE_OC  ',
!     &    'ADE_NO3 ',
!     &    'ADE_SO4 ',
!     &    'ADE_CORS',
!     &    'ACD     ',
!     &    'AMN_HAPS',
!     &    'APHG    ',
!     &    'AAS     ',
!     &    'ABENAPY '  /)
     
!      INTEGER                      :: N_AERO_HAPS = 0
!      CHARACTER( 16 ), ALLOCATABLE :: AERO_HAPS(  : ) 
     
      
! Full chemistry commented out because it produced floating
! point error in advection when using debug flags.
!     LOGICAL, SAVE :: L_CHEMISTRY

! Cloud Module variables - sln 4 june 2018
      REAL, SAVE :: DEPSUM_SAVE = 0.0
      REAL, SAVE :: DS4_SAVE    = 0.0
      REAL, SAVE :: REMOV_SAVE  = 0.0

c...Final, combined tags
      INTEGER                      :: N_SPCTAG
      INTEGER,         ALLOCATABLE :: S_SPCTAG( : )
      INTEGER,         ALLOCATABLE :: T_SPCTAG( : )
      CHARACTER( 16 ), ALLOCATABLE :: VNAM_SPCTAG( : )

C ...Tagging indices for bcon, others, icon
      INTEGER, SAVE :: BIDITAG = 0
      INTEGER, SAVE :: BCONTAG = 0
      INTEGER, SAVE :: OTHRTAG = 0
      INTEGER, SAVE :: ICONTAG = 0

      REAL, PRIVATE, ALLOCATABLE :: BUFF2( :,: )

! Diffusion module variables
      REAL, ALLOCATABLE, SAVE :: SA_DDEP( :,:,: )

! Advection module variables
      LOGICAL, ALLOCATABLE, SAVE :: BCON_SPC( : )

        INTEGER, PARAMETER, PRIVATE :: MAX_STRING     = 32  ! max length of tagclass name
        INTEGER, PARAMETER, PRIVATE :: MAX_TAGCLASSES = 200 ! max number of tagclasses
        INTEGER, PARAMETER, PRIVATE :: NUMB_CLASSES   = 9   ! number of defined tagclasses
        
        CHARACTER( MAX_STRING ), PARAMETER, PRIVATE :: DEFINED_CLASSES( NUMB_CLASSES ) = 
     & (/ 'AMMONIUM ',
     &    'OC       ',
     &    'CHLORINE ',
     &    'EC       ',
     &    'SULFATE  ',
     &    'OZONE    ',
     &    'NITRATE  ',
     &    'VOC      ',
     &    'PM25_IONS' /)
        
      
      CONTAINS

C============================================================

        SUBROUTINE CNT_SA_IO_LIST ( NTAGS )

C20140428  Counts the number of emissions tags in the input control file
C         Called by sa_dim.F
C
C

        USE UTILIO_DEFN     ! 20120615
c       USE HGRD_DEFN    
        USE RUNTIME_VARS, ONLY: ABFLUX, MYPE, LOGDEV
                
        IMPLICIT NONE

        CHARACTER( 16 ) :: PNAME = 'CNT_SA_IO_LIST'
        CHARACTER( 256 ) :: EQNAME 
        INTEGER INPUT_UNIT
        INTEGER IOST
        CHARACTER( 80 ) :: XMSG   
C external functions
        INTEGER LEN_TRIM

        INTEGER, INTENT( OUT ) :: NTAGS
        INTEGER ILINE
        CHARACTER( 180 ) :: TXTLINE
        
C-----------------------------------------------------------
        CALL NAMEVAL( 'SA_IOLIST', EQNAME )
        INPUT_UNIT = JUNIT()
         
        OPEN ( UNIT = INPUT_UNIT, FILE = EQNAME, STATUS = 'OLD', IOSTAT = IOST )
        XMSG = 'Error Opening SA_IO_LIST file'
        IF ( IOST .NE. 0 ) THEN
          CALL M3EXIT ( 'SA_IOLIST', 0, 0, XMSG, XSTAT1 )
        ENDIF

        IF ( MYPE .EQ. 0 ) THEN        
          WRITE(LOGDEV, *) 'SA_IO_LIST Sucessfully Opened'
          WRITE(LOGDEV, *) 'Start counting the list...'
        ENDIF
        NTAGS = 0
        COUNTTAG: DO 
          READ ( INPUT_UNIT, '(A)' ) TXTLINE
          IF ( TXTLINE( 1:7 ) .EQ. 'ENDLIST' ) EXIT COUNTTAG
          IF ( TXTLINE( 1:7 ) .EQ. 'TAG NAM' ) THEN
            NTAGS = NTAGS + 1
            IF ( MYPE .EQ. 0 ) PRINT*, TXTLINE
          ENDIF ! if tag_name
c check to see if bidirectional tag is necessary
          IF ( ABFLUX ) THEN
            IF ( TXTLINE( 1:11 ) .EQ. 'TAG CLASSES' ) THEN
              IOST = INDEX( TXTLINE,'AMMONIUM', BACK = .FALSE. )
              IF ( IOST .NE. 0 ) THEN
                SA_BIDI = .TRUE.
                NTAGS   = NTAGS + 1
c               BIDITAG = NTAGS ! set to last tag
              END IF
            END IF
          END IF
        ENDDO COUNTTAG


        IF ( SA_BIDI ) BIDITAG = NTAGS ! set to last tag

        CLOSE( INPUT_UNIT )

        END SUBROUTINE CNT_SA_IO_LIST
C============================================================

        SUBROUTINE RD_SA_IO_LIST ( NTAGS )

C20140428  Read entries in each emissions tag in the input control file
C         Called by sa_dim.F
C
C
C 05 Nov 2018: L.Zhou, S.Napelenok: Reorganized the control file
c 

        USE UTILIO_DEFN 
        USE RUNTIME_VARS, ONLY: ABFLUX, MYPE, LOGDEV

        IMPLICIT NONE
C arguments
        INTEGER, INTENT( IN ) :: NTAGS

        CHARACTER( 16 )    :: PNAME = 'RD_SA_IO_LIST'
        CHARACTER( 256 )   :: EQNAME 
        INTEGER INPUT_UNIT
        INTEGER IOST
        CHARACTER( 120 ) :: XMSG   
C external functions
        INTEGER LEN_TRIM

        INTEGER ILINE
        INTEGER ITAG, TAGLOOP
        CHARACTER( 128 ) :: TXTLINE

C...multi-sectors-for-a-single-tag 20130702
        INTEGER ISGSTK
        CHARACTER( 2 )  :: CSGSTK
        CHARACTER( 16 ) :: FNAME
        LOGICAL LBACK
        INTEGER DELIMINATOR_INDEX 
        INTEGER BGN_SG
        INTEGER I, J ,k
        CHARACTER(1) :: CHR

C-----------------------------------------------------------
        CALL NAMEVAL( 'SA_IOLIST', EQNAME )
        INPUT_UNIT = JUNIT()

        OPEN ( UNIT = INPUT_UNIT, FILE = EQNAME, STATUS = 'OLD', IOSTAT = IOST )
        XMSG = 'Error Opening SA_IO_LIST file'
        IF ( IOST .NE. 0 ) THEN
          CALL M3EXIT ( 'SA_IOLIST', 0, 0, XMSG, XSTAT1 )
        ENDIF
        
        CHR = CHAR(124)
101     READ ( INPUT_UNIT, '(A)' ) TXTLINE
        IF ( TXTLINE(1:3) .EQ. '!!!' .OR. TXTLINE(1:3) .EQ. '' ) GOTO 101
        IF( INDEX( TXTLINE, 'TAG CLASSES' ) .GE. 1 )THEN
           DELIMINATOR_INDEX = INDEX( TXTLINE, CHAR( 124 ) ) + 1
           IF(  DELIMINATOR_INDEX .LE. 1 )THEN
              XMSG = 'ISAM ERROR: TAG CLASSES line missing the "|" deliminator'
              CALL M3EXIT('SA_IOLIST', 0, 0, XMSG, XSTAT2 )
           END IF 
           TAGCLASSES = TXTLINE( DELIMINATOR_INDEX:LEN_TRIM( TXTLINE ) )
           IF( LEN_TRIM( TAGCLASSES ) .LT. 1 )THEN
              XMSG = "ISAM ERROR: TAGCLASSES in control file is blank. "
              CALL M3EXIT('SA_IOLIST', 0, 0, XMSG, XSTAT2 )
           END IF
        ELSE   
           XMSG = "ISAM ERROR: First Nonblank or Noncomment line "
     &         // "in control file must define TAG CLASSES. "
           CALL M3EXIT('SA_IOLIST', 0, 0, XMSG, XSTAT2 )
        END IF

        IF ( MYPE .eq. 0 ) then
          WRITE(LOGDEV, *) 'Reading ISAM control file'
          WRITE(LOGDEV, *) 'TAGCLASSES = ',TRIM(TAGCLASSES)
          WRITE(LOGDEV, *) '  for ', NTAGS, ' tags'
        ENDIF

        IF ( SA_BIDI ) THEN
          TAGLOOP = NTAGS -1
        ELSE
          TAGLOOP = NTAGS
        ENDIF

        DO ITAG = 1, ( TAGLOOP )
        
          TAGNAME( ITAG )         = ' '
          ISAMRGN_TEMP( ITAG )    = ' '
          TAGSTREAMS_TEMP( ITAG ) = ' '
          
102       READ ( INPUT_UNIT, '(A)' ) TXTLINE
          IF ( TXTLINE(1:3) .EQ. '!!!' .OR. TXTLINE(1:3) .EQ. '' ) GOTO 102
          IF( INDEX( TXTLINE, 'TAG NAME' ) .GE. 1 )THEN
              DELIMINATOR_INDEX = INDEX( TXTLINE, CHAR( 124 ) ) + 1
              IF(  DELIMINATOR_INDEX .LE. 1 )THEN
                 XMSG = 'ISAM ERROR: "TAG NAME" line missing the "|" deliminator'
                 CALL M3EXIT('SA_IOLIST', 0, 0, XMSG, XSTAT2 )
              END IF 
              TAGNAME( ITAG ) = TXTLINE( DELIMINATOR_INDEX:LEN_TRIM( TXTLINE ) )
              IF( LEN_TRIM( TAGNAME( ITAG ) ) .LT. 1 )THEN
                 XMSG = 'ISAM ERROR: a "TAG NAME" in control file is blank. '
                 CALL M3EXIT('SA_IOLIST', 0, 0, XMSG, XSTAT2 )
              END IF
          ELSE   
              XMSG = "ISAM ERROR: First line defining a TAG "
     &            // 'in control file must contain the "TAG NAME" ' 
              CALL M3EXIT('SA_IOLIST', 0, 0, XMSG, XSTAT2 )
          END IF

103       READ ( INPUT_UNIT, '(A)' ) TXTLINE
          IF ( TXTLINE(1:3) .EQ. '!!!' .OR. TXTLINE(1:3) .EQ. '' ) GOTO 103
          IF( INDEX( TXTLINE, 'REGION' ) .GE. 1 )THEN
              DELIMINATOR_INDEX = INDEX( TXTLINE, CHAR( 124 )  ) + 1
              IF(  DELIMINATOR_INDEX .LE. 1 )THEN
                 XMSG = 'ISAM ERROR: a "REGION(s)" line missing the "|" deliminator'
                 CALL M3EXIT('SA_IOLIST', 0, 0, XMSG, XSTAT2 )
              END IF 
              ISAMRGN_TEMP( ITAG ) = TXTLINE( DELIMINATOR_INDEX:LEN_TRIM( TXTLINE ) )
              IF( LEN_TRIM( ISAMRGN_TEMP( ITAG ) ) .LT. 1 )THEN
                 XMSG = 'ISAM ERROR: a "REGION(s)" in control file is blank. '
                 CALL M3EXIT('SA_IOLIST', 0, 0, XMSG, XSTAT2 )
              END IF
          ELSE   
              XMSG = "ISAM ERROR: Second line defining a TAG "
     &            // 'in control file must contain the "REGION(s)" ' 
              CALL M3EXIT('SA_IOLIST', 0, 0, XMSG, XSTAT2 )
          END IF

104       READ ( INPUT_UNIT, '(A)' ) TXTLINE
          IF ( TXTLINE(1:3) .EQ. '!!!' .OR. TXTLINE(1:3) .EQ. '' ) GOTO 104
          IF( INDEX( TXTLINE, 'EMIS STREAM' ) .GE. 1 )THEN
              DELIMINATOR_INDEX = INDEX( TXTLINE, CHAR( 124 )  ) + 1
              IF(  DELIMINATOR_INDEX .LE. 1 )THEN
                 XMSG = 'ISAM ERROR: a "EMIS STREAM(s)" line missing the "|" deliminator'
                 CALL M3EXIT('SA_IOLIST', 0, 0, XMSG, XSTAT2 )
              END IF 
              TAGSTREAMS_TEMP( ITAG ) = TXTLINE( DELIMINATOR_INDEX:LEN_TRIM( TXTLINE ) )
              IF( LEN_TRIM( TAGSTREAMS_TEMP( ITAG ) ) .LT. 1 )THEN
                 XMSG = 'ISAM ERROR: a "EMIS STREAM(s)" in control file is blank. '
                 CALL M3EXIT('SA_IOLIST', 0, 0, XMSG, XSTAT2 )
              END IF
          ELSE   
              XMSG = "ISAM ERROR: Third line defining a TAG "
     &            // 'in control file must contain the "EMIS STREAM(s)" ' 
              CALL M3EXIT('SA_IOLIST', 0, 0, XMSG, XSTAT2 )
          END IF

        END DO
        
c add a bidirectional tag if necesssary
        IF ( SA_BIDI ) THEN
          TAGNAME( NTAGS )         = 'BID' 
          ISAMRGN_TEMP( NTAGS )    = 'EVERYWHERE'
          TAGSTREAMS_TEMP( NTAGS ) = 'BIDIRECTIONALNH3'
        ENDIF

        END SUBROUTINE RD_SA_IO_LIST
C============================================================
        
        SUBROUTINE GET_NSPC_SA ()

C20140428  Determine number of ISAM species
C         Called by sa_dim.F
C

        USE RUNTIME_VARS
        USE GRID_CONF    ! just for mype 20140327
        USE CGRID_SPCS, ONLY : N_AE_SPC, AE_SPC
        USE RXNS_DATA,  ONLY : MECHNAME, NUMB_MECH_SPC !Get Chemical Mechanism Name
!       USE DEGRADE_PARAMETERS
        USE UTILIO_DEFN  ! 20130627

        IMPLICIT NONE

        INTEGER :: ITAG
        INTEGER :: J, K

        LOGICAL :: LBACK
        LOGICAL :: DEFINED 
        INTEGER :: BGN_SP
        LOGICAL :: EFLAG = .FALSE. 
        INTEGER :: CLASSES_FOUND  = 0
        
        CHARACTER( 132 ) :: XMSG

        INTEGER :: IOST
        CHARACTER( MAX_STRING ), ALLOCATABLE :: CLASS_NAMES( : )
        INTEGER :: NCLASSES = 0
        
c----------------------------------------------------------


        ALLOCATE( CLASS_NAMES( MAX_TAGCLASSES ) )
        CALL PARSE_STRING( TAGCLASSES, NCLASSES, CLASS_NAMES )
                
        IF( NCLASSES .LT. 1 )THEN
           XMSG = "ISAM ERROR: TAGCLASSES in control file incorrectly defined. "
           CALL M3EXIT('GET_NSPC_SA', 0, 0, XMSG, XSTAT2 )
        END IF
        
        DO J = 1, NCLASSES
           K = INDEX1( CLASS_NAMES( J ), NUMB_CLASSES, DEFINED_CLASSES )
           IF( K .LT. 1 )THEN
              XMSG = "ISAM ERROR: unknown tagclass: " // TRIM( CLASS_NAMES( J ) )
              WRITE(LOGDEV,'(A)')TRIM(XMSG )
              EFLAG = .TRUE. 
           ELSE
              CLASSES_FOUND = CLASSES_FOUND + 1   
           END IF       
        END DO
        
        IF( EFLAG .OR. CLASSES_FOUND .LT. 1 )THEN
           XMSG = 'ISAM ERROR: problem defining tagclasses'
           WRITE(LOGDEV,'(A)')TRIM(XMSG )
           XMSG = 'Permitted TAGCLASSES listed below:'
           WRITE(LOGDEV,'(A)')TRIM(XMSG )
           DO J = 1, NUMB_CLASSES
              WRITE(LOGDEV,'(2X,A)')TRIM( DEFINED_CLASSES( J ) )
           END DO
           XMSG = 'Above fatal error(s) in setting up ISAM'
           CALL M3EXIT('GET_NSPC_SA', 0, 0, XMSG, XSTAT2 )
        END IF 
        
        L_EC     = .FALSE. 
        L_OC     = .FALSE. 
        L_SFATE  = .FALSE. 
        L_NITRATE = .FALSE. 
        L_NH4     = .FALSE. 
        L_PM25    = .FALSE.   
!        L_CO      = .FALSE.   
        L_OZONE   = .FALSE.   
        L_VOC     = .FALSE.   
        L_CHLORINE  = .FALSE.
!       L_GAS_HAPS  = .FALSE.
!       L_AERO_HAPS = .FALSE.
!       L_CHEMISTRY = .FALSE.   

        NSPC_SA = 0

        LBACK = .FALSE. 

!       LBACK = .FALSE.
!       BGN_SP = INDEX( TAGCLASSES,'CHEMISTRY',LBACK )
!       IF ( BGN_SP .NE. 0 ) THEN
!         NSPC_SA = NSPC_SA + NUMB_MECH_SPC  ! CO
!         L_CHEMISTRY = .TRUE.
!         L_NITRATE   = .TRUE.
!         L_VOC       = .TRUE.
!         L_CHLORINE  = .TRUE.
!       ENDIF

        BGN_SP = INDEX( TAGCLASSES,'CHLORINE',LBACK )        
        L_CHLORINE = ( BGN_SP .NE. 0 .OR. L_CHLORINE )
        IF ( L_CHLORINE ) THEN
          NSPC_SA = NSPC_SA + 3  ! ACLI, ACLJ, HCL
        ENDIF

        BGN_SP = INDEX( TAGCLASSES,'EC',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          NSPC_SA = NSPC_SA + 2  ! AECJ, AECI
          L_EC = .TRUE.
        ENDIF

        BGN_SP = INDEX( TAGCLASSES,'OC',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
c          NSPC_SA = NSPC_SA + 13  ! ALVPO1I,ALVPO1J,ASVPO1I,ASVPO1J,ASVPO2I,ASVPO2J,ASVPO3J,AIVPO1J
                                  ! VLVPO1,VSVPO1,VSVPO2,VSVO3,VIVPO1
          NSPC_SA = NSPC_SA + 4   ! APOCJ, APOCI, APNCOMJ, APNCOMI  
          L_OC = .TRUE.
        ENDIF

        BGN_SP = INDEX( TAGCLASSES,'SULFATE',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          L_SFATE  = .TRUE. 
          NSPC_SA = NSPC_SA + 5  ! ASO4J, ASO4I, SO2, SULF, SULRXN
          IF ( MECHNAME .EQ. 'CB6R3M_AE7_KMTBR' ) NSPC_SA = NSPC_SA + 1  ! add DMS
        ENDIF

        BGN_SP = INDEX( TAGCLASSES,'OZONE',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          NSPC_SA = NSPC_SA + 4  ! O3, O1D, O or O3P, HO2
          L_OZONE   = .TRUE.
          L_NITRATE = .TRUE.
          L_VOC     = .TRUE.
        END IF       

        BGN_SP = INDEX( TAGCLASSES,'NITRATE',LBACK )
        L_NITRATE = ( BGN_SP .NE. 0 .OR. L_NITRATE )

        BGN_SP = INDEX( TAGCLASSES,'VOC',LBACK )
        L_VOC = ( BGN_SP .NE. 0 .OR. L_VOC )

!        BGN_SP     = INDEX( TAGCLASSES,'GAS_HAPS',LBACK )        
!        L_GAS_HAPS = ( BGN_SP .NE. 0 .OR. L_GAS_HAPS )
!        IF ( L_GAS_HAPS ) THEN
!          SELECT CASE( MECHNAME )
!             CASE( 'CB6R3_AE6_AQ', 'CB6R3_AE7_AQ', 'CB6R3M_AE7_KMTBR' )
!                NSPC_SA = NSPC_SA + 5  ! FORM_PRIM, ALD2_PRIM, ACRO, ACRO_PRIM, 13BUTADIENE
!             CASE( 'SAPRC07TC_AE6_AQ','SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ' ) 
!                NSPC_SA = NSPC_SA + 5  ! HCHO_PRIM, CCHO_PRIM, ACRO, ACRO_PRIM, 13BUTADIENE 
!             CASE( 'RACM2_AE6_AQ') 
!                NSPC_SA = NSPC_SA + 0   
!          END SELECT
!          CALL DEGRADE_DATA()        
!          NSPC_SA = NSPC_SA + NDEGRADED_FOUND ! degradation species found in species namelists
!        ENDIF

        DEFINED = .TRUE.

        IF( L_NITRATE .OR. L_VOC .OR. L_OZONE )THEN ! determine if tag species are defined by mechanism's name
          SELECT CASE( MECHNAME ) 
             CASE( 'CB6R3_AE6_AQ',     'CB6R3_AE7_AQ',      'CB6R3M_AE7_KMTBR', 
     &             'SAPRC07TC_AE6_AQ', 'SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ',
     &             'RACM2_AE6_AQ'                                               ) 
                DEFINED = .TRUE.
             CASE DEFAULT 
                DEFINED = .FALSE.
          END SELECT
        END IF
       
        IF( .NOT. DEFINED )THEN
            XMSG = "ISAM ERROR: Using L_NITRATE or L_VOC or L_OZONE tag group but their species "
     &          // "for the " // TRIM( MECHNAME ) // " mechanism not defined. Modify GET_NSPC_SA "
     &          // "and GET_SPC_INDEX  subroutines to include MECHNAME cases for it."
            CALL M3EXIT('GET_NSPC_SA', 0, 0, XMSG, XSTAT2 )
        END IF
        
        IF ( L_NITRATE ) THEN
          SELECT CASE( MECHNAME )
             CASE( 'CB6R3_AE6_AQ', 'CB6R3_AE7_AQ' )
                NSPC_SA = NSPC_SA + 13  
!                NSPC_SA = NSPC_SA + 19
             CASE( 'CB6R3M_AE7_KMTBR' )
                NSPC_SA = NSPC_SA + 18  
!                NSPC_SA = NSPC_SA + 24  
             CASE( 'SAPRC07TC_AE6_AQ' ) 
                NSPC_SA = NSPC_SA + 16   
             CASE( 'RACM2_AE6_AQ') 
                NSPC_SA = NSPC_SA + 14   
             CASE( 'SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ' ) 
                NSPC_SA = NSPC_SA + 33  
          END SELECT
! specices common to all mechanisms          
          NSPC_SA = NSPC_SA + 11  ! NO, NO2, NO3, ANO3J, ANO3I, HNO3, HONO, N2O5, PNA or HNO4, PAN, PANX or PAN2 or MPAN
        ENDIF

        IF ( L_VOC ) THEN
          SELECT CASE( MECHNAME )
             CASE( 'CB6R3_AE6_AQ' )
                NSPC_SA = NSPC_SA + 25
             CASE( 'CB6R3_AE7_AQ', 'CB6R3M_AE7_KMTBR' )
                NSPC_SA = NSPC_SA + 26
             CASE( 'RACM2_AE6_AQ') 
                NSPC_SA = NSPC_SA + 37
             CASE( 'SAPRC07TC_AE6_AQ', 'SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ' ) 
                NSPC_SA = NSPC_SA + 40
          END SELECT
        ENDIF

        LBACK = .FALSE.
        BGN_SP = INDEX( TAGCLASSES,'AMMONIUM',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          NSPC_SA = NSPC_SA + 3  ! ANH4J, ANH4I, NH3
          L_NH4 = .TRUE.
        ENDIF

        LBACK = .FALSE.
        BGN_SP = INDEX( TAGCLASSES,'PM25_IONS',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          NSPC_SA = NSPC_SA + 12 ! ANAI/J,AMGJ,AKJ,ACAJ,AFEJ,AALJ,ASI/J,ATIJ,AMNJ,AOTHRI/J
          L_PM25 = .TRUE.
        ENDIF

!        LBACK = .FALSE.
!        BGN_SP = INDEX( TAGCLASSES,'CO',LBACK )
!        IF ( BGN_SP .NE. 0 ) THEN
!          NSPC_SA = NSPC_SA + 1  ! CO
!          L_CO = .TRUE.
!        ENDIF

!        LBACK = .FALSE.
!        BGN_SP = INDEX( TAGCLASSES,'AERO_HAPS',LBACK )
!        IF ( BGN_SP .NE. 0 ) THEN          
!          L_AERO_HAPS = .TRUE.
!          DO J = 1, N_AERO_HAPS_LIST
!             DO K = 1, N_AE_SPC
!                IF( INDEX( AE_SPC( K ), AERO_HAPS_LIST( J ) ) .GT. 0 ) THEN
!                   NSPC_SA     = NSPC_SA + 1
!                   N_AERO_HAPS = N_AERO_HAPS + 1
!                END IF  
!             END DO           
!          END DO
!        ENDIF

        END SUBROUTINE GET_NSPC_SA

! --------------------------------------------------------------------------------
        SUBROUTINE PARSE_STRING ( ENV_VAL, NVARS, VAL_LIST, LOGDEV )

! takes a string of items delimited by white space,
! commas or semi-colons) and parse out the items into variables. Two data
! types: character strings and integers (still represented as strings in
! the env var vaules).
          USE UTILIO_DEFN 
          
          IMPLICIT NONE

          CHARACTER( * ), INTENT ( IN )           :: ENV_VAL
          INTEGER,        INTENT ( OUT )          :: NVARS
          CHARACTER( * ), INTENT ( OUT )          :: VAL_LIST( : )
          INTEGER,        INTENT ( IN ), OPTIONAL :: LOGDEV

          INTEGER             :: MAX_LEN
          INTEGER             :: LEN_EVAL
          CHARACTER( 16 )     :: PNAME = 'PARSE_STRING'
          CHARACTER(  1 )     :: CHR
          CHARACTER( 96 )     :: XMSG

          INTEGER :: JP( MAX_STRING*SIZE( VAL_LIST ) )
          INTEGER :: KP( MAX_STRING*SIZE( VAL_LIST ) )
          INTEGER :: STATUS
          INTEGER :: IP, V

          INTEGER :: LOC_LOGDEV
 
          IF (PRESENT(LOGDEV)) THEN
             LOC_LOGDEV = LOGDEV
          ELSE
             LOC_LOGDEV = 6
          END IF

          MAX_LEN = MAX_STRING * ( SIZE( VAL_LIST ) + 1 ) ! extra character allows deliminator

C Parse:

           NVARS = 0

C don't count until 1st char in string
           
           IP = 0
           KP = 1
           JP = 1
           LEN_EVAL = LEN_TRIM( ENV_VAL ) 
           IF ( LEN_EVAL .GT. MAX_LEN ) THEN
              XMSG = TRIM( PNAME ) // ': The Environment variable, '
     &            // TRIM( ENV_VAL ) // ',  has too long, greater than ' 
              WRITE(LOGDEV,'(A,I8)')TRIM( XMSG ), MAX_LEN
              XMSG = 'Above fatal error encountered '
              CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )                     
           END IF
101        LOOP_101: DO  ! read list
              IP = IP + 1
              IF ( IP .GT. LEN_EVAL ) EXIT LOOP_101
              CHR = ENV_VAL( IP:IP )
              IF ( CHR .EQ. ' ' .OR. ICHAR ( CHR ) .EQ. 09 ) CYCLE LOOP_101
              IF( NVARS .GT. SIZE( VAL_LIST ) )THEN
                 XMSG = TRIM( PNAME ) // ':ERROR: Number of values in List, ' 
     &                //  TRIM( ENV_VAL ) 
     &                // ', greater than the size of its storage array, '
                  WRITE(LOGDEV,'(A,I4)')TRIM( XMSG ), SIZE( VAL_LIST )
                  XMSG = 'Above fatal error encountered '
                  CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )                     
              END IF
              NVARS = NVARS + 1
              JP( NVARS ) = IP   ! 1st char
              IF( IP .EQ. LEN_EVAL )THEN ! word one character long          
                  KP( NVARS ) = IP
                  V = 1
                  EXIT LOOP_101
              END IF     
201           LOOP_201: DO ! read word
                 IP = IP + 1
                 CHR = ENV_VAL( IP:IP )
                 IF ( CHR .NE. ' ' .AND.
     &                CHR .NE. ',' .AND.
     &                CHR .NE. ';' .OR.
     &                ICHAR ( CHR ) .EQ. 09 ) THEN  ! 09 = horizontal tab
                    CYCLE LOOP_201
                 ELSE                               ! last char in word
                    KP( NVARS ) = IP - 1 
                    V = JP( NVARS ) - IP
                    IF( V .GT. MAX_STRING )THEN
                      XMSG =  'The word, ' // ENV_VAL( JP(NVARS):KP(NVARS) ) 
     &                     // ', in list, ' // TRIM( ENV_VAL )
     &                     // ', is too long, '
                      WRITE(LOGDEV,'(A,1X,I2,A,I2)')TRIM( XMSG ), V, ' max allowed ',
     &                MAX_STRING
                      XMSG = 'Above fatal error encountered '
                      CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )                     
                    END IF
                    EXIT LOOP_201
                 END IF 
                 IF ( IP .GE. LEN_EVAL ) EXIT LOOP_101
              END DO LOOP_201
           END DO LOOP_101
           
           IF( NVARS .GT. SIZE( VAL_LIST ) )THEN
              XMSG = TRIM( PNAME ) // ':ERROR: Number of values in List, ' 
     &             //  TRIM( ENV_VAL ) // ', greater than '
              WRITE(LOGDEV,'(A,I4)')TRIM( XMSG ), SIZE( VAL_LIST )
              XMSG = 'Above fatal error encountered '
              CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )                     
           END IF
           DO V = 1, NVARS
              VAL_LIST( V ) = ENV_VAL( JP( V ):KP( V ) )
           END DO

           RETURN
           
        END SUBROUTINE PARSE_STRING

c===============================================================

        SUBROUTINE GET_SPC_INDEX ()

C20140428  Map CGRID species index to ISAM tracer species index
C         Called by driver.F
C

        USE CGRID_SPCS     
        USE RXNS_DATA,    ONLY : MECHNAME, NUMB_MECH_SPC, CHEMISTRY_SPC 
!        USE DEGRADE_PARAMETERS
        USE RUNTIME_VARS, ONLY: MYPE
        USE UTILIO_DEFN    

        IMPLICIT NONE
      
        INTEGER J_SPC, N, ITAG, I2, ISUM
        INTEGER N_OZN     ! index of ozone in gc_spc list
        INTEGER ALLOCSTAT

        CHARACTER( 16 ), ALLOCATABLE, SAVE :: CMAQ_ADV_NAMES( : ) 
        
        CHARACTER( 16 ), ALLOCATABLE :: SPC_LIST( : )
        LOGICAL,         ALLOCATABLE :: SPC_KEEP( : )
        
        INTEGER FRST, LAST, CMAQ_ADV

        INTEGER I_SPC, K_SPC, NLIST
         
        CHARACTER( 132 ) :: XMSG

        LOGICAL FOUND
        LOGICAL TRANS_WARN
        LOGICAL EFLAG
c----------------------------------------------------------
        

        NLIST = NSPC_SA
        ALLOCATE ( SPC_LIST( NLIST ), STAT = ALLOCSTAT )
        ALLOCATE ( SPC_KEEP( NLIST ), STAT = ALLOCSTAT )
        SPC_LIST = ' '
        SPC_KEEP = .TRUE.
        
        J_SPC = 0
         
        EFLAG = .FALSE.
         
        IF (  L_CHLORINE ) THEN
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ACLJ'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ACLI'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'HCL'
        ENDIF
        
        IF (  L_EC ) THEN
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'AECJ'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'AECI'   
        ENDIF

        IF ( L_OC ) THEN
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'ALVPO1J'
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'ALVPO1I'
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'ASVPO1J'
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'ASVPO1I'
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'ASVPO2J'
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'ASVPO2I'
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'ASVPO3J'
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'AIVPO1J'
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'VLVPO1'
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'VSVPO1'
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'VSVPO2'
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'VSVPO3'
c            J_SPC = J_SPC + 1
c            SPC_LIST( J_SPC ) = 'VIVPO1'
            J_SPC = J_SPC + 1
            SPC_LIST( J_SPC ) = 'APOCJ'
            J_SPC = J_SPC + 1
            SPC_LIST( J_SPC ) = 'APOCI'
            J_SPC = J_SPC + 1
            SPC_LIST( J_SPC ) = 'APNCOMJ'
            J_SPC = J_SPC + 1
            SPC_LIST( J_SPC ) = 'APNCOMI'
        ENDIF

          
        IF ( L_SFATE ) THEN
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ASO4J'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ASO4I'    
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'SO2'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'SULF'    
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'SULRXN'
          IF ( MECHNAME .EQ. 'CB6R3M_AE7_KMTBR' ) THEN
             J_SPC = J_SPC + 1
             SPC_LIST( J_SPC ) = 'DMS'
          END IF
        ENDIF

        IF ( L_NITRATE ) THEN
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ANO3J'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ANO3I'    
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'HNO3'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'NO'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'NO2'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'NO3'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'HONO'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'N2O5'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'PAN'
          SELECT CASE( MECHNAME )
             CASE( 'CB6R3_AE6_AQ', 'CB6R3_AE7_AQ' )
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'NTR1' 
               J_SPC = J_SPC + 1  
               SPC_LIST( J_SPC ) = 'NTR2'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'INTR'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'PNA'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'PANX'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'CLNO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'CLNO3'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'XO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'XO2H'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'XO2N'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'ROR' 
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MEO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'ISO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'C2O3'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'CXO3'                
!                J_SPC = J_SPC + 1
!                SPC_LIST( J_SPC ) = 'OPO3'
!                J_SPC = J_SPC + 1
!                SPC_LIST( J_SPC ) = 'BZO2'
!                J_SPC = J_SPC + 1
!                SPC_LIST( J_SPC ) = 'TO2'
!                J_SPC = J_SPC + 1
!                SPC_LIST( J_SPC ) = 'XLO2'
!                J_SPC = J_SPC + 1
!                SPC_LIST( J_SPC ) = 'EPX2'
!                J_SPC = J_SPC + 1
!                SPC_LIST( J_SPC ) = 'HCO3'                
             CASE( 'CB6R3M_AE7_KMTBR' )
               J_SPC = J_SPC + 1  
               SPC_LIST( J_SPC ) = 'NTR1'   
               J_SPC = J_SPC + 1  
               SPC_LIST( J_SPC ) = 'NTR2'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'INTR'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'PNA'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'PANX'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'CLNO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'CLNO3'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'BRNO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'BRNO3'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'INO'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'INO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'INO3'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'XO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'XO2H'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'XO2N'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'ROR' 
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MEO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'ISO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'C2O3'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'CXO3'                
!                J_SPC = J_SPC + 1
!                SPC_LIST( J_SPC ) = 'OPO3'
!                J_SPC = J_SPC + 1
!                SPC_LIST( J_SPC ) = 'BZO2'
!                J_SPC = J_SPC + 1
!                SPC_LIST( J_SPC ) = 'TO2'
!                J_SPC = J_SPC + 1
!                SPC_LIST( J_SPC ) = 'XLO2'
!                J_SPC = J_SPC + 1
!                SPC_LIST( J_SPC ) = 'EPX2'
!                J_SPC = J_SPC + 1
!                SPC_LIST( J_SPC ) = 'HCO3'                
             CASE( 'SAPRC07TC_AE6_AQ' ) 
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'HNO4'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'RNO3'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'PAN2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'CLNO'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'CLONO'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'CLONO2'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'xNO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MEO2'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'xMEO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'RO2C'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'RO2XC'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MECO3'
               
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'xMECO3'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'RCO3'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'xRCO3'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'BZCO3'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'PBZN'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MACO3'
               
             CASE( 'SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ' ) 
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'HNO4'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'RNO3'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'PAN2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'CLNO'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'CLONO'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'CLONO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'ETHLN'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'IMAE'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'IMAPAN'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'ISOPNN'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MACRN'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'NIT1'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MVKN'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'PROPNN'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MTNO3'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'AMTNO3J'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'AISOPNNJ'  
                
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'xNO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MEO2'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'xMEO2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'RO2C'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'RO2XC'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MECO3'
               
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'xMECO3'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'RCO3'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'xRCO3'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'BZCO3'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'PBZN'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MACO3'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'TERPNRO2'  
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'ISOPO2'  

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'NISOPO2'  
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MVKOO'  

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MACROO'  
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'RNO3I'  

!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'DIBOO'  
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'HC5OO'  

!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'NIT1NO3OOA'  
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'NIT1NO3OOB'  


             CASE( 'RACM2_AE6_AQ') 
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'HNO4'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'ONIT'   
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MPAN'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'ACO3'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'MO2'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'HC3P'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'HC5P'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'HC8P'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'KETP'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'ISOP'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'ETHP'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'RCO3'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'XYLP'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'TR2'
               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'BENP'

               J_SPC = J_SPC + 1
               SPC_LIST( J_SPC ) = 'ETEP'

          END SELECT 
        ENDIF ! L_NITRATE

        IF ( L_VOC ) THEN
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'CO'
          SELECT CASE( MECHNAME )
             CASE( 'CB6R3_AE6_AQ' )
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ALD2'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ALDX'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ETH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ETHA'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ETOH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'FORM'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'IOLE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ISOP'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MEOH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'OLE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ECH4'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'PAR'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'TERP'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'TOL'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'XYLMN'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'NAPH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ETHY'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'PRPA'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ACET'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'KET'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'GLY'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'BENZENE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'GLYD'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MEPX'                
             CASE( 'CB6R3_AE7_AQ', 'CB6R3M_AE7_KMTBR' )
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ALD2'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ALDX'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ETH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ETHA'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ETOH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'FORM'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'IOLE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ISOP'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MEOH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'OLE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ECH4'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'PAR'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'TERP'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'TOL'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'XYLMN'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'NAPH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ETHY'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'PRPA'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ACET'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'KET'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'GLY'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'BENZENE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'APIN'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'GLYD'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MEPX'                
             CASE( 'SAPRC07TC_AE6_AQ', 'SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ' ) 
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'HCHO'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'CCHO'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'RCHO'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'xHCHO'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'xCCHO'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'xRCHO'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ALK1'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ALK2'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ALK3'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ALK4'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ALK5'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ETHENE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ACETYLENE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'PROPENE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ISOPRENE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'TERP'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'SESQ'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'APIN'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'OLE1'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'OLE2'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ETOH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MEOH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'GLY'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MGLY'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'BENZENE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'TMBENZ124'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'TOLUENE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MXYL'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'OXYL'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'PXYL'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'NAPHTHAL'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ARO1'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ARO2MN'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'BUTADIENE13'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ACROLEIN'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ACETONE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MEK'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MVK'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MACR'
             CASE( 'RACM2_AE6_AQ') 
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'HCHO'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ACD'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ALD'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'UALD'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'HC3'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'HC5'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'HC8'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'OLI'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'OLT'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ETH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ETE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ACE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ISO'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'SESQ'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'LIM'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'API'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'EOH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MOH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'GLY'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MGLY'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'BENZENE'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'TOL'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'XYM'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'XYO'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'XYP'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'NAPH'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'DIEN'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'ACT'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'HKET'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'KET'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MVK'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MEK'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'MACR'

                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'DCB1'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'DCB2'
                J_SPC = J_SPC + 1
                SPC_LIST( J_SPC ) = 'DCB3'

          END SELECT
        ENDIF ! L_VOC

        IF (  L_NH4 ) THEN
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ANH4J'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ANH4I'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'NH3'
        ENDIF

        IF ( L_PM25 ) THEN
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ANAJ'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ANAI' 
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'AMGJ'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'AKJ'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ACAJ'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'AFEJ'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'AALJ'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ASIJ'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'ATIJ'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'AMNJ'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'AOTHRJ'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'AOTHRI'
        ENDIF

!        IF ( L_CO ) THEN
!          J_SPC = J_SPC + 1
!          SPC_LIST( J_SPC ) = 'CO'
!        ENDIF

        IF ( L_OZONE ) THEN
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'HO2'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'O3'
          J_SPC = J_SPC + 1
          SPC_LIST( J_SPC ) = 'O1D'
          J_SPC = J_SPC + 1
          SELECT CASE( MECHNAME )
             CASE( 'CB6R3_AE6_AQ', 'CB6R3_AE7_AQ', 'CB6R3M_AE7_KMTBR')
                SPC_LIST( J_SPC ) = 'O'
             CASE( 'SAPRC07TC_AE6_AQ', 'RACM2_AE6_AQ', 'SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ' ) 
                SPC_LIST( J_SPC ) = 'O3P'
          END SELECT
        ENDIF


!        IF ( L_GAS_HAPS ) THEN
!          SELECT CASE( MECHNAME )
!             CASE( 'CB6R3_AE6_AQ', 'CB6R3_AE7_AQ', 'CB6R3M_AE7_KMTBR' )
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'FORM_PRIMARY'
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'ALD2_PRIMARY'
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'ACROLEIN'
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'BUTADIENE13'
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'ACRO_PRIMARY'
!             CASE( 'SAPRC07TC_AE6_AQ','SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ')
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'HCHO_PRIMARY'
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'CCHO_PRIMARY'
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'ACROLEIN'
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'BUTADIENE13'
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = 'ACRO_PRIMARY'
!            END SELECT
!            DO I_SPC = 1, NDEGRADED_FOUND
!               J_SPC = J_SPC + 1
!               SPC_LIST( J_SPC ) = DEGRADED_SPECIES( I_SPC )
!            END DO
!        END IF

!        IF ( L_AERO_HAPS ) THEN          
!          DO I_SPC = 1, N_AERO_HAPS_LIST
!             DO K_SPC = 1, N_AE_SPC
! find HAP aerosols in aerosol species and adjust NSPC_SA if in SPC_LIST
!                IF ( INDEX(  AE_SPC( K_SPC ), AERO_HAPS_LIST( I_SPC ) ) .GT. 0 ) THEN 
!                   J_SPC = J_SPC + 1
!                   SPC_LIST( J_SPC ) = AE_SPC( K_SPC )
!                END IF 
!             END DO            
!          END DO
!        ENDIF
        
!       IF ( L_CHEMISTRY ) THEN
!          SPC_LIST( ( J_SPC+1 ):( J_SPC+NUMB_MECH_SPC ) ) = CHEMISTRY_SPC( 1:NUMB_MECH_SPC )
!          J_SPC = J_SPC + NUMB_MECH_SPC 
!       END IF

C...Check if the above adds up to number of tagging species
        IF ( J_SPC .NE. NSPC_SA ) THEN
          WRITE(LOGDEV,'(A,I4,A,I4,3A)')'Tagged species found, ', J_SPC,  
     &    ', does not equal expected, ',  NSPC_SA,
     &    ', for the ', TRIM( MECHNAME ), ' mechanism.'
          EFLAG = .TRUE.
        ELSE
         WRITE(LOGDEV,'(A,I4,A,I4,3A)')'Tagged species found, ', J_SPC,
     &    ', equals expected, ',  NSPC_SA,
     &    ', for the ', TRIM( MECHNAME ), ' mechanism.'
        ENDIF

! check for redundant species names
        DO I_SPC = 1, NSPC_SA
             WRITE(LOGDEV,'(A,I4,1X,A16)')'ISAM_INIT: SPC_LIST: ',I_SPC,SPC_LIST( I_SPC )
        END DO
        DO I_SPC = 1, NSPC_SA
           IF ( .NOT. SPC_KEEP( I_SPC ) )CYCLE
           IF ( LEN_TRIM( SPC_LIST( I_SPC ) ) .LT. 1 ) THEN
c             WRITE(6,'(A,I4)')'ISAM_INIT ERROR: SPC_LIST blank at index: ',I_SPC
              WRITE(LOGDEV,'(A,I4)')'ISAM_INIT ERROR: SPC_LIST blank at index: ',I_SPC
              EFLAG = .TRUE.
              CYCLE
           ELSE                 
c             WRITE(6,'(A,I4,1X,A16)')'ISAM_INIT: SPC_LIST: ',I_SPC,SPC_LIST( I_SPC )
              WRITE(LOGDEV,'(A,I4,1X,A16)')'ISAM_INIT: SPC_LIST: ',I_SPC,SPC_LIST( I_SPC )
           END IF
           DO K_SPC = 1, NSPC_SA
              IF( I_SPC .EQ. K_SPC )CYCLE
              IF ( SPC_LIST( I_SPC ) .EQ. SPC_LIST( K_SPC ) ) THEN
                     SPC_KEEP( K_SPC ) = .FALSE.
                     J_SPC = J_SPC - 1
                     WRITE(LOGDEV,'(A,I4,1X,A16,A,I4)')
     &              'ISAM_INIT: Omitting redundant species ',
     &               K_SPC,SPC_LIST( K_SPC ),' NSPC_SA now = ',J_SPC
              END IF
           END DO
        END DO

        IF ( EFLAG ) THEN
          CALL M3EXIT( 'GET_SPC_INDEX', 0, 0, 'Above Fatal Error Encountered', XSTAT2 )
        END IF 

! reset number of track species        
        NSPC_SA  = J_SPC
        N_SPCTAG = NSPC_SA * NTAG_SA
    
        ALLOCATE ( SPC_NAME( NSPC_SA, NTAG_SA ), STAT = ALLOCSTAT )
        CALL CHECKMEM( ALLOCSTAT, 'SPC_NAME', 'GET_SPC_INDEX' )
        SPC_NAME = ' '
        ALLOCATE ( SPC_INDEX( NSPC_SA,20 ), STAT = ALLOCSTAT )
        CALL CHECKMEM( ALLOCSTAT, 'SPC_INDEX', 'GET_SPC_INDEX' )
        SPC_INDEX = -1
        ALLOCATE ( IS_SPC_AEROSOL( NSPC_SA,NTAG_SA ), STAT = ALLOCSTAT )
        CALL CHECKMEM( ALLOCSTAT, 'IS_SPC_AEROSOL', 'IS_SPC_AEROSOL' )
        IS_SPC_AEROSOL = .FALSE.
        ALLOCATE ( TRANSPORT_SPC( N_SPCTAG ), STAT = ALLOCSTAT )
        CALL CHECKMEM( ALLOCSTAT, 'TRANSPORT_SPC', 'TRANSPORT_SPC' )
        TRANSPORT_SPC = .TRUE.

! set species names
        I_SPC = 0
        WRITE(LOGDEV,*)'# FINAL List of ISAM species = ', NSPC_SA 
        DO K_SPC = 1, NLIST
           IF( SPC_KEEP( K_SPC ) )THEN
              I_SPC = I_SPC + 1
              SPC_NAME( I_SPC,: ) = SPC_LIST( K_SPC )
           END IF
        END DO
        DO I_SPC = 1, NSPC_SA 
           WRITE(LOGDEV,'(I4,1X,A16)')I_SPC, SPC_NAME( I_SPC,1 )
        END DO


C...Initialize species index
        SPC_INDEX = -1

C..Determine which ISAM species are aerosols and are transported
        NSPC_TRANSPORT = 0
        ISUM  = 0
        DO ITAG = 1, NTAG_SA
           DO J_SPC = 1, NSPC_SA
              ISUM = ISUM + 1
              DO N = 1, N_AE_SPC
                 IF( TRIM( SPC_NAME( J_SPC,ITAG ) ) .EQ. TRIM( AE_SPC( N ) ) )THEN
                     IS_SPC_AEROSOL( J_SPC,ITAG ) = .TRUE.
                 END IF
              END DO
              FOUND = .FALSE.        
              DO N = 1, N_GC_TRNS
                 IF( TRIM( SPC_NAME( J_SPC,ITAG ) ) .EQ. TRIM( GC_TRNS( N ) ) )FOUND = .TRUE.
              END DO
              DO N = 1, N_AE_TRNS
                 IF( TRIM( SPC_NAME( J_SPC,ITAG ) ) .EQ. TRIM( AE_TRNS( N ) ) )FOUND = .TRUE.
              END DO
              DO N = 1, N_NR_TRNS
                 IF( TRIM( SPC_NAME( J_SPC,ITAG ) ) .EQ. TRIM( NR_TRNS( N ) ) )FOUND = .TRUE.
              END DO
              DO N = 1, N_TR_ADV
                 IF( TRIM( SPC_NAME( J_SPC,ITAG ) ) .EQ. TRIM( TR_ADV( N ) ) )FOUND = .TRUE.
              END DO
              TRANSPORT_SPC( ISUM ) = FOUND
              IF( FOUND )NSPC_TRANSPORT = NSPC_TRANSPORT + 1
           END DO  
        END DO

C...Assign species index with CMAQ species mappings
        ALLOCATE( MAP_CGRtoSA( NSPCSD ) )
        MAP_CGRtoSA = 0

        LOOP_FIND: DO N = 1, NSPCSD
          IF ( N .GE. GC_STRT .AND. N .LE. GC_FINI ) THEN
            DO J_SPC = 1, NSPC_SA
              IF ( SPC_NAME( J_SPC,ICONTAG ) .EQ. GC_SPC( N ) ) THEN  
                SPC_INDEX( J_SPC,1 ) = 1
                SPC_INDEX( J_SPC,2 ) = N
                MAP_CGRtoSA( N ) = J_SPC
                CYCLE LOOP_FIND
              END IF ! spc_name and gc_spc match
            END DO
          ELSE IF ( N .GE. AE_STRT .AND. N .LE. AE_FINI ) THEN
            DO J_SPC = 1, NSPC_SA
              IF ( TRIM( SPC_NAME( J_SPC,ICONTAG ) ) .EQ. TRIM( AE_SPC( N-AE_STRT+1 ) ) ) THEN
                SPC_INDEX( J_SPC,1 ) = 1
                SPC_INDEX( J_SPC,2 ) = N
                MAP_CGRtoSA( N ) = J_SPC
                CYCLE LOOP_FIND
              END IF ! spc_name and ae_spc match
            END DO
          ELSE IF ( N .GE. NR_STRT .AND. N .LE. NR_FINI ) THEN
            DO J_SPC = 1, NSPC_SA
              IF ( SPC_NAME( J_SPC,ICONTAG ) .EQ. NR_SPC( N-NR_STRT+1 ) ) THEN
                SPC_INDEX( J_SPC,1 ) = 1
                SPC_INDEX( J_SPC,2 ) = N
                MAP_CGRtoSA( N ) = J_SPC
                CYCLE LOOP_FIND
              END IF
            END DO          
          END IF
        END DO LOOP_FIND ! number of tagging species

        DO J_SPC = 1, NSPC_SA
          IF ( SPC_INDEX( J_SPC,2 ) .LT. 1 ) THEN
             write(logdev,'(A)')'Tracked species '
     &       // TRIM( SPC_NAME( J_SPC,ICONTAG ) ) //
     &          ' not found among model species '
             EFLAG = .TRUE.
          END IF
        END DO 

C...Map SA species for Advection
        ALLOCATE( MAP_ADVtoSA ( N_SPCTAG ) )
 
        CMAQ_ADV = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV
        ALLOCATE( CMAQ_ADV_NAMES( CMAQ_ADV ) )

        FRST = 0
        LAST = 0
        IF ( N_GC_TRNS .NE. 0 ) THEN 
           FRST = 1
           LAST = N_GC_TRNS
           CMAQ_ADV_NAMES( FRST:LAST ) = GC_TRNS(1:N_GC_TRNS)
        ENDIF

        IF ( N_AE_TRNS .NE. 0 ) THEN
           FRST = LAST + 1
           LAST = LAST + N_AE_TRNS
           CMAQ_ADV_NAMES( FRST:LAST ) = AE_TRNS(1:N_AE_TRNS)
        END IF

        IF ( N_NR_TRNS .NE. 0 ) THEN
           FRST = LAST + 1
           LAST = LAST + N_NR_TRNS
           CMAQ_ADV_NAMES( FRST:LAST ) = NR_TRNS(1:N_NR_TRNS)
        END IF

        IF ( N_TR_ADV .NE. 0 ) THEN
           FRST = LAST + 1
           LAST = LAST + N_TR_ADV
           CMAQ_ADV_NAMES( FRST:LAST ) = TR_ADV(1:N_TR_ADV)
        END IF

        MAP_ADVtoSA = -1
        N = 0
        TRANS_WARN = .FALSE.
        DO ITAG = 1, NTAG_SA
           DO J_SPC = 1, NSPC_SA
              N = N + 1
              MAP_ADVtoSA(N) = INDEX1( SPC_NAME(J_SPC,1), CMAQ_ADV, CMAQ_ADV_NAMES )
              IF( MAP_ADVtoSA(N) .LT. 1 .AND. ITAG .LT. 2 )THEN
                 TRANS_WARN = .TRUE.
                 XMSG = 'ISAM WARNING: Tagged Species, ' // TRIM( SPC_NAME(J_SPC,1) ) 
     &               // ' not transported. Modify species namelist(s) to include process.'
                 WRITE(LOGDEV,'(A)')TRIM( XMSG )
              END IF
!              XMSG = 'For ' // SPC_NAME(J_SPC,1) // ' transported, adv_map = '
!              WRITE(LOGDEV,95000)NSPC_TRANSPORT, J_SPC, TRIM( XMSG ),
!     &        TRANSPORT_SPC(J_SPC), MAP_ADVtoSA(N)
           END DO
        END DO
        
        IF ( TRANS_WARN ) THEN
          CALL M3WARN( 'GET_SPC_INDEX', 0, 0, 'Above Problems Encountered' )
        END IF
        IF ( EFLAG ) THEN
          CALL M3EXIT( 'GET_SPC_INDEX', 0, 0, 'Above Fatal Error Encountered', XSTAT2 )
        END IF 

95000    FORMAT(2(I3,1X),A,L4,1X,I4)


C...Map SA species for DEPV
C
C...  DO THIS LATER IF NEEDED FOR THE GENERAL CASE - RIGHT NOW ONLY DO AMMONIA
c       ALLOCATE( MAP_DEPVtoSA ( N_SPCTAG ) )

c       CMAQ_DEPV = N_GC_DEPV + N_AE_DEPV + N_NR_DEPV + N_TR_DEPV
c       ALLOCATE( CMAQ_DEPV_NAMES( CMAQ_DEPV ) )

c       FRST = 0
c       LAST = 0
c       IF ( N_GC_DEPV .NE. 0 ) THEN
c          FRST = 1
c          LAST = N_GC_DEPV
c          CMAQ_DEPV_NAMES( FRST:LAST ) = GC_DEPV(1:N_GC_DEPV)
c       ENDIF

c       IF ( N_AE_DEPV .NE. 0 ) THEN
c          FRST = LAST + 1
c          LAST = LAST + N_AE_DEPV
c          CMAQ_DEPV_NAMES( FRST:LAST ) = AE_DEPV(1:N_AE_DEPV)
c       END IF

c       IF ( N_NR_DEPV .NE. 0 ) THEN
c          FRST = LAST + 1
c          LAST = LAST + N_NR_DEPV
c          CMAQ_DEPV_NAMES( FRST:LAST ) = NR_DEPV(1:N_NR_DEPV)
c       END IF

c       IF ( N_TR_DEPV .NE. 0 ) THEN
c          FRST = LAST + 1
c          LAST = LAST + N_TR_DEPV
c          CMAQ_DEPV_NAMES( FRST:LAST ) = TR_DEPV(1:N_TR_DEPV)
c       END IF

c       MAP_DEPVtoSA = -1
c       N = 0
!       DO ITAG = 1, NTAG_SA
c          DO J_SPC = 1, NSPC_SA
c             N = N + 1
c             MAP_DEPVtoSA(N) = INDEX1( SPC_NAME(J_SPC,1), CMAQ_DEPV, CMAQ_DEPV_NAMES )
c             IF( MAP_DEPVtoSA(N) .LT. 1 )THEN
c                XMSG = 'ISAM WARNING: Tagged Species, ' // TRIM( SPC_NAME(J_SPC,1) )
c    &               // ' not in DEPV list.'
c                CALL M3EXIT ( 'MAP_DEPVtoSA', 0, 0, XMSG, XSTAT1 )
c             END IF
c          END DO
!       END DO

        END SUBROUTINE GET_SPC_INDEX

      END MODULE SA_DEFN
