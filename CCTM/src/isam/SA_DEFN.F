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
! 10 Jun 21 G. Sarwar: Added CB6R3M
! 23 Jun 21 G. Sarwar: Replaced CB6R3M with CB6R5M
! 25 June 21 G. Sarwar: Removed CB6R3_AE6_AQ since it is not suported in CMAQ any more
! 26 Apr 22 G. Sarwar: Added DMS

      USE RUNTIME_VARS, ONLY : VOC_NOX_TRANS, ISAM_NOX_CASE, ISAM_VOC_CASE
      
      IMPLICIT NONE

      SAVE

      PUBLIC

      INTEGER ITAG

c...Tagging maps
      CHARACTER( 16 ), ALLOCATABLE :: ISAM_SPEC( :,: )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_SAtoCGR( : )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_CGRtoSA( : )
      LOGICAL, ALLOCATABLE, SAVE   :: L_ISAM_AERO( : )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_ISAMtoAERO( : )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_AEROtoSA( :,: )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_AEROGAStoSA( : )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_AEROCTRtoSA( : )
      LOGICAL, ALLOCATABLE, SAVE   :: IS_SPC_AEROSOL( :,: )
      LOGICAL, ALLOCATABLE, SAVE   :: TRANSPORT_SPC ( : )
      LOGICAL, ALLOCATABLE, SAVE   :: ISAM_SPECIES_BIAS( :,: )
      INTEGER, ALLOCATABLE, SAVE   :: STREAM_TO_TAG ( :,: )
      INTEGER, ALLOCATABLE, SAVE   :: TAGS_PER_STREAM( : )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_DIFFtoSA( : )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_ADVtoSA( : )
 
      INTEGER, ALLOCATABLE, SAVE   :: ISAM_TO_AERO( : )  ! maps tracked species to bulk concentration if aerosol species
      INTEGER, ALLOCATABLE, SAVE   :: SA_AERO_INDX( : )  ! index of tracked aerosol in ISAM concentration array

      INTEGER, ALLOCATABLE, SAVE   :: ISAM_PVO3_MAP( : ) ! potential vorticity map

      INTEGER ::     NSPC_SA_AE        ! number of tracked species that are aerosols
      INTEGER ::     N_SA_AEROSPC      ! number of unique aerospc in all tracked aerosols

      INTEGER, SAVE ::     NSPC_SA
      INTEGER, SAVE ::     NTAG_SA
      INTEGER, SAVE ::     NSPC_TRANSPORT
      INTEGER, PARAMETER :: NBIAS = 4 
      INTEGER, SAVE ::     N_VOC_BIAS
      INTEGER, SAVE ::     N_NOY_BIAS

c...Define Allocatable Apportionment Arrays
      REAL, POINTER :: ISAM  ( :,:,:,:,: )
      REAL, ALLOCATABLE :: AISAM ( :,:,:,:,: )
      REAL, ALLOCATABLE :: TOT_SADEP ( :,:,:,: )
      REAL, ALLOCATABLE :: CONV_SADEP ( :,:,:,: )
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
      LOGICAL, SAVE :: L_SO4     = .FALSE.
      LOGICAL, SAVE :: L_NO3     = .FALSE.
      LOGICAL, SAVE :: L_NH4     = .FALSE.
      LOGICAL, SAVE :: L_PM25    = .FALSE.
      LOGICAL, SAVE :: L_OZONE   = .FALSE.
      LOGICAL, SAVE :: L_VOC     = .FALSE.
      LOGICAL, SAVE :: L_CL      = .FALSE.
      
      LOGICAL, SAVE :: SA_BIDI = .FALSE.

! Chemical Mechanism Lists for Populating ISAM Species Arrays
      TYPE ISAM_LIST
         CHARACTER( 16 ), ALLOCATABLE  :: LIST( : )
      END TYPE ISAM_LIST

      TYPE( ISAM_LIST ), ALLOCATABLE :: ISAM_SPEC_OZONE( : ),
     &              ISAM_SPEC_NO3( : ), ISAM_SPEC_VOC( : ),
     &              ISAM_SPEC_SO4( : ), ISAM_SPEC_HAPGAS( : ),
     &              ISAM_BIAS_VOC( : ), ISAM_BIAS_NOY( : )

! Full chemistry commented out because it produced floating
! point error in advection when using debug flags.
!     LOGICAL, SAVE :: L_CHEMISTRY

! Cloud Module variables - sln 4 june 2018
      REAL, SAVE :: DEPSUM_SAVE = 0.0
      REAL, SAVE :: DS4_SAVE    = 0.0
      REAL, SAVE :: REMOV_SAVE  = 0.0

      REAL, SAVE :: DEPSUM_AORGC_SAVE = 0.0
      REAL, SAVE :: DGLY1_SAVE        = 0.0 
      REAL, SAVE :: DMGLY1_SAVE       = 0.0  
      REAL, SAVE :: REMOV_AORGC_SAVE  = 0.0

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
        INTEGER, PARAMETER, PRIVATE :: NUMB_CLASSES   = 19  ! number of defined tagclasses
        
        CHARACTER( MAX_STRING ), PARAMETER, PRIVATE :: DEFINED_CLASSES( NUMB_CLASSES ) = 
     & (/ 'AMMONIUM    ',
     &    'OA_TOT      ',
     &    'NVPOA       ',
     &    'CHLORINE    ',
     &    'EC          ',
     &    'SULFATE     ',
     &    'OZONE       ',
     &    'NITRATE     ',
     &    'VOC         ',
     &    'PM_IONS     ',
     &    'ALL         ',
     &    'PM_TOT      ',
     &    'HAP_GAS     ',
     &    'HAP_AERO    ',
     &    'HAP_TOT     ',
     &    'URBAN_TOXICS',
     &    'PAH_TEQ     ',
     &    'BENAPYRENE  ',
     &    'MERCURY     '/)
        
      
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
        XMSG = 'Error Opening ' // TRIM( EQNAME ) // ', the SA_IOLIST file'
        IF ( IOST .NE. 0 ) THEN
          CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
        ENDIF

        IF ( MYPE .EQ. 0 ) THEN        
          WRITE(LOGDEV, *) TRIM( EQNAME ) // ': Sucessfully Opened'
          WRITE(LOGDEV, *) 'Start counting the list...'
        ENDIF
        NTAGS = 0
        COUNTTAG: DO 
          READ ( INPUT_UNIT, '(A)' ) TXTLINE
          IF ( TXTLINE( 1:7 ) .EQ. 'ENDLIST' ) EXIT COUNTTAG
          IF ( TXTLINE( 1:7 ) .EQ. 'TAG NAM' ) THEN
            NTAGS = NTAGS + 1
            !IF ( MYPE .EQ. 0 ) PRINT*, TXTLINE
          ENDIF ! if tag_name
c check to see if bidirectional tag is necessary
          IF ( ABFLUX ) THEN
            IF ( INDEX( TXTLINE, 'TAG CLASSES') .GT. 0 ) THEN
              IOST = INDEX( TXTLINE,'AMMONIUM', BACK = .FALSE. )
              IF ( IOST .NE. 0 ) THEN
                SA_BIDI = .TRUE.
                NTAGS   = NTAGS + 1
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
        INTEGER DELIMINATOR_INDEX 
        INTEGER BGN_SG
        INTEGER I, J ,k
        CHARACTER(1) :: CHR

C-----------------------------------------------------------
        CALL NAMEVAL( 'SA_IOLIST', EQNAME )
        INPUT_UNIT = JUNIT()

        OPEN ( UNIT = INPUT_UNIT, FILE = EQNAME, STATUS = 'OLD', IOSTAT = IOST )
        XMSG = 'Error Opening ' // TRIM( EQNAME ) // ', the SA_IOLIST file'
        IF ( IOST .NE. 0 ) THEN
          CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
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
              IF( INDEX( TAGNAME( ITAG ), "_", BACK = .FALSE. ) .GT. 0 )THEN
                 XMSG = 'ISAM ERROR: TAG NAME: ' // TRIM( TAGNAME( ITAG ) ) 
     &               // ' cannot use the underscore character.'
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
        USE RXNS_DATA,  ONLY : MECHNAME  !Get Chemical Mechanism Name
        USE CGRID_SPCS, ONLY : NSPCSD, CGRID_NAME, N_AE_SPC, AE_SPC,
     &                         N_NR_SPC, NR_SPC, CGRID_MASK_GAS,
     &                         CGRID_MASK_NR 

        USE AERO_DATA
        USE SOA_DEFN
        USE UTILIO_DEFN

        IMPLICIT NONE

        INTEGER :: ITAG, IMECH, IAERO, JAERO, ISPEC, IMODE
        INTEGER :: I, J, K
        INTEGER :: N_ISAM_MECH, NVL_RESET

        LOGICAL,         ALLOCATABLE  :: MERCURY_MECH ( : )
        CHARACTER( 32 ), ALLOCATABLE  :: ISAM_MECHLIST( : )

        TYPE( ISAM_LIST ) :: ISAM_SPEC_HAPAERO, ISAM_SPEC_TOXICS,
     &                       ISAM_SPEC_PAHTEQ,  ISAM_SPEC_MERCURY

        LOGICAL :: EFLAG          = .FALSE. 
        LOGICAL :: FOUND_SPECIES  = .FALSE.
        INTEGER :: CLASSES_FOUND  = 0
        
        CHARACTER( 400 ) :: XMSG
        CHARACTER( 16 )  :: GASNAME, CTRNAME

        INTEGER :: IOST
        CHARACTER( MAX_STRING ), ALLOCATABLE :: CLASS_NAMES( : )
        INTEGER :: NCLASSES = 0
        
c----------------------------------------------------------

        N_ISAM_MECH = 9
        ALLOCATE( ISAM_MECHLIST( N_ISAM_MECH ),   MERCURY_MECH ( N_ISAM_MECH ),
     &            ISAM_SPEC_OZONE( N_ISAM_MECH ), ISAM_SPEC_VOC( N_ISAM_MECH ), 
     &            ISAM_SPEC_NO3  ( N_ISAM_MECH ), ISAM_SPEC_SO4( N_ISAM_MECH ), 
     &            ISAM_SPEC_HAPGAS( N_ISAM_MECH ),
     &            ISAM_BIAS_VOC( N_ISAM_MECH ),   ISAM_BIAS_NOY( N_ISAM_MECH ) )

       
        ISAM_MECHLIST = (/'CB6R3_AE7_AQ      ', 'CB6R5_AE7_AQ      ',
     &                    'CB6R5M_AE7_AQ     ', 
     &                    'SAPRC07TC_AE6_AQ  ', 'SAPRC07TIC_AE6I_AQ', 
     &                    'SAPRC07TIC_AE7I_AQ', 'RACM2_AE6_AQ      ',
     &                    'CRACMM1_AQ        ', 'CRACMM2           '/ )  

        MERCURY_MECH = (/  .TRUE.,  .TRUE.,  
     &                     .TRUE.,  .FALSE., 
     &                     .FALSE., .FALSE.,
     &                     .FALSE., .FALSE. / )  

        ! Detect Mechanism Number
        IMECH = INDEX1( MECHNAME, N_ISAM_MECH, ISAM_MECHLIST )

        ! When Prescribing Species for Ozone, Nitrate and VOCs, do not
        ! worry about double-counting. The algorithm will look for each
        ! species in the ISAM_SPEC array before adding it. Put all of
        ! the species in each category that you think you will need.

        ! PRESCRIBE OZONE SPECIES
        DO J = 1,N_ISAM_MECH
          ALLOCATE( ISAM_SPEC_OZONE( J )%LIST( 4 ) )
        END DO
        !CB6R3_AE7_AQ
        ISAM_SPEC_OZONE( 1 )%LIST = (/'O3 ','O1D','HO2','O  '/)
        
        !CB6R5_AE7_AQ
        ISAM_SPEC_OZONE( 2 )%LIST = (/'O3 ','O1D','HO2','O  '/)
        
        !CB6R5M_AE7_AQ
        ISAM_SPEC_OZONE( 3 )%LIST = (/'O3 ','O1D','HO2','O  '/)
        
        !SAPRC07TC_AE6_AQ
        ISAM_SPEC_OZONE( 4 )%LIST = (/'O3 ','O1D','HO2','O3P'/)
        
        !SAPRC07TIC_AE6I_AQ
        ISAM_SPEC_OZONE( 5 )%LIST = (/'O3 ','O1D','HO2','O3P'/)
        
        !SAPRC07TIC_AE7I_AQ
        ISAM_SPEC_OZONE( 6 )%LIST = (/'O3 ','O1D','HO2','O3P'/)
        
        !RACM2_AE6_AQ
        ISAM_SPEC_OZONE( 7 )%LIST = (/'O3 ','O1D','HO2','O3P'/)

        !CRACMM1_AQ
        ISAM_SPEC_OZONE( 8 )%LIST = (/'O3 ','O1D','HO2','O3P'/)

        !CRACMM2
        ISAM_SPEC_OZONE( 9 )%LIST = (/'O3 ','O1D','HO2','O3P'/)

        ! PRESCRIBE VOC SPECIES
        !CB6R3_AE7_AQ
        ALLOCATE( ISAM_SPEC_VOC( 1 )%LIST( 28 ) )
        ISAM_SPEC_VOC( 1 )%LIST = (/'ALD2   ','ALDX   ','ETH    ',
     &          'ETHA   ','ETOH   ','FORM   ','IOLE   ','ISOP   ',
     &          'MEOH   ','OLE    ','PAR    ','TERP   ','TOL    ',
     &          'XYLMN  ','NAPH   ','ETHY   ','PRPA   ','ACET   ',
     &          'KET    ','GLY    ','BENZENE','GLYD   ','MEPX   ',
     &          'APIN   ','SOAALK ','ECH4   ','CO     ','MGLY   '/)

        !CB6R5_AE7_AQ
        ALLOCATE( ISAM_SPEC_VOC( 2 )%LIST( 28 ) )
        ISAM_SPEC_VOC( 2 )%LIST = (/'ALD2   ','ALDX   ','ETH    ',
     &          'ETHA   ','ETOH   ','FORM   ','IOLE   ','ISOP   ',
     &          'MEOH   ','OLE    ','PAR    ','TERP   ','TOL    ',
     &          'XYLMN  ','NAPH   ','ETHY   ','PRPA   ','ACET   ',
     &          'KET    ','GLY    ','BENZENE','GLYD   ','MEPX   ',
     &          'APIN   ','SOAALK ','ECH4   ','CO     ','MGLY   '/)

        !CB6R5M_AE7_AQ
        ALLOCATE( ISAM_SPEC_VOC( 3 )%LIST( 28 ) )
        ISAM_SPEC_VOC( 3 )%LIST = (/'ALD2   ','ALDX   ','ETH    ',
     &          'ETHA   ','ETOH   ','FORM   ','IOLE   ','ISOP   ',
     &          'MEOH   ','OLE    ','PAR    ','TERP   ','TOL    ',
     &          'XYLMN  ','NAPH   ','ETHY   ','PRPA   ','ACET   ',
     &          'KET    ','GLY    ','BENZENE','GLYD   ','MEPX   ',
     &          'APIN   ','SOAALK ','ECH4   ','CO     ','MGLY   '/)

        !SAPRC07TC_AE6_AQ
        ALLOCATE( ISAM_SPEC_VOC( 4 )%LIST( 41 ) )
        ISAM_SPEC_VOC( 4 )%LIST = (/'HCHO       ','CCHO       ',
     &  'RCHO       ','xHCHO      ','xCCHO      ','xRCHO      ',
     &  'ALK1       ','ALK2       ','ALK3       ','ALK4       ',
     &  'ALK5       ','ETHENE     ','ACETYLENE  ','PROPENE    ',
     &  'ISOPRENE   ','TERP       ','SESQ       ','APIN       ',
     &  'OLE1       ','OLE2       ','ETOH       ','MEOH       ',
     &  'GLY        ','MGLY       ','BENZENE    ','TMBENZ124  ',
     &  'TOLUENE    ','MXYL       ','OXYL       ','PXYL       ',
     &  'NAPHTHAL   ','ARO1       ','ARO2MN     ','BUTADIENE13',
     &  'ACROLEIN   ','ACETONE    ','MEK        ','MVK        ',
     &  'MACR       ','SOAALK     ','CO         '/ )

        !SAPRC07TIC_AE6I_AQ
        ALLOCATE( ISAM_SPEC_VOC( 5 )%LIST( 41 ) )
        ISAM_SPEC_VOC( 5 )%LIST = (/'HCHO       ','CCHO       ',
     &  'RCHO       ','xHCHO      ','xCCHO      ','xRCHO      ',
     &  'ALK1       ','ALK2       ','ALK3       ','ALK4       ',
     &  'ALK5       ','ETHENE     ','ACETYLENE  ','PROPENE    ',
     &  'ISOPRENE   ','TERP       ','SESQ       ','APIN       ',
     &  'OLE1       ','OLE2       ','ETOH       ','MEOH       ',
     &  'GLY        ','MGLY       ','BENZENE    ','TMBENZ124  ',
     &  'TOLUENE    ','MXYL       ','OXYL       ','PXYL       ',
     &  'NAPHTHAL   ','ARO1       ','ARO2MN     ','BUTADIENE13',
     &  'ACROLEIN   ','ACETONE    ','MEK        ','MVK        ',
     &  'MACR       ','SOAALK     ','CO         '/ )

        !SAPRC07TIC_AE7I_AQ
        ALLOCATE( ISAM_SPEC_VOC( 6 )%LIST( 41 ) )
        ISAM_SPEC_VOC( 6 )%LIST = (/'HCHO       ','CCHO       ',
     &  'RCHO       ','xHCHO      ','xCCHO      ','xRCHO      ',
     &  'ALK1       ','ALK2       ','ALK3       ','ALK4       ',
     &  'ALK5       ','ETHENE     ','ACETYLENE  ','PROPENE    ',
     &  'ISOPRENE   ','TERP       ','SESQ       ','APIN       ',
     &  'OLE1       ','OLE2       ','ETOH       ','MEOH       ',
     &  'GLY        ','MGLY       ','BENZENE    ','TMBENZ124  ',
     &  'TOLUENE    ','MXYL       ','OXYL       ','PXYL       ',
     &  'NAPHTHAL   ','ARO1       ','ARO2MN     ','BUTADIENE13',
     &  'ACROLEIN   ','ACETONE    ','MEK        ','MVK        ',
     &  'MACR       ','SOAALK     ','CO         '/ )

        !RACM2_AE6_AQ
        ALLOCATE( ISAM_SPEC_VOC( 7 )%LIST( 38 ) )
        ISAM_SPEC_VOC( 7 )%LIST = (/'HCHO   ','ACD    ','ALD    ',
     &          'UALD   ','HC3    ','HC5    ','HC8    ','OLI    ',
     &          'OLT    ','ETH    ','ETE    ','ACE    ','ISO    ',
     &          'SESQ   ','LIM    ','API    ','EOH    ','MOH    ',
     &          'GLY    ','MGLY   ','BENZENE','TOL    ','XYM    ',
     &          'XYO    ','XYP    ','NAPH   ','DIEN   ','ACT    ',
     &          'HKET   ','KET    ','MVK    ','MEK    ','MACR   ',
     &          'DCB1   ','DCB2   ','DCB3   ','SOAALK ','CO     '/ )

        !CRACMM1_AQ
        ALLOCATE( ISAM_SPEC_VOC( 8 )%LIST( 96 ) )
        ISAM_SPEC_VOC( 8 )%LIST = (/'ACD       ','ACE       ','ACRO      ',
     &    'ACT       ','ALD       ','API       ','BALD      ','BDE13     ',
     &    'BEN       ','CSL       ','DCB1      ','DCB2      ','DCB3      ',
     &    'ELHOM     ','EOH       ','ETE       ','ETEG      ','ETH       ',
     &    'FURAN     ','FURANONE  ','GLY       ','HOM       ','HC3       ',
     &    'HC5       ','HC10      ','HCHO      ','HKET      ','IEPOX     ',
     &    'ISHP      ','IPN       ','IHN       ','IPC       ','ISO       ',
     &    'KET       ','LIM       ','LIMAL     ','MACR      ','MAHP      ',
     &    'MCT       ','MEK       ','MGLY      ','MOH       ','MVK       ',
     &    'NAPH      ','OLI       ','OLT       ','OP1       ','OP2       ',
     &    'OP3       ','OPB       ','ORA1      ','ORA2      ','PINAL     ',
     &    'PHEN      ','PROG      ','ROH       ','SESQ      ','SLOWROC   ',
     &    'TOL       ','UALD      ','XYM       ','XYE       ','PAA       ',
     &    'PAN       ','PPN       ','MPAN      ','ONIT      ','ISON      ',
     &    'TRPN      ','VROCIOXY  ','VROCP5ARO ','VROCP6ARO ','VROCN2ALK ',
     &    'VROCN1ALK ','VROCP0ALK ','VROCP1ALK ','VROCP2ALK ','VROCP3ALK ',
     &    'VROCP4ALK ','VROCP5ALK ','VROCP6ALK ','VROCN2OXY2','VROCN2OXY4',
     &    'VROCN2OXY8','VROCN1OXY1','VROCN1OXY3','VROCN1OXY6','VROCP0OXY2',
     &    'VROCP0OXY4','VROCP1OXY1','VROCP1OXY3','VROCP2OXY2','VROCP3OXY2',
     &    'VROCP4OXY2','VROCP5OXY1','VROCP6OXY1'/ )

        !CRACMM2
        ALLOCATE( ISAM_SPEC_VOC( 9 )%LIST( 97 ) )
        ISAM_SPEC_VOC( 9 )%LIST = (/'ACD       ','ACE       ','ACRO      ',
     &    'ACT       ','ALD       ','API       ','BALD      ','BDE13     ',
     &    'BEN       ','CSL       ','DCB1      ','DCB2      ','DCB3      ',
     &    'EBZ       ','ELHOM     ','EOH       ','ETE       ','ETEG      ',
     &    'ETH       ','FURAN     ','FURANONE  ','GLY       ','HOM       ',
     &    'HC3       ','HC5       ','HC10      ','HCHO      ','HKET      ',
     &    'HONIT     ','IEPOX     ','ISHP      ','IPX       ','ISO       ',
     &    'KET       ','LIM       ','LIMAL     ','MACR      ','MAHP      ',
     &    'MCT       ','MEK       ','MGLY      ','MOH       ','MVK       ',
     &    'NAPH      ','OLI       ','OLT       ','OP1       ','OP2       ',
     &    'OP3       ','OPB       ','ORA1      ','ORA2      ','PINAL     ',
     &    'PHEN      ','PROG      ','ROH       ','SESQ      ','SLOWROC   ',
     &    'STY       ','TOL       ','UALD      ','XYL       ','PAA       ',
     &    'PAN       ','PPN       ','MPAN      ','ONIT      ','ISON      ',
     &    'TRPN      ','VROCIOXY  ','VROCP5ARO ','VROCP6ARO ','VROCN2ALK ',
     &    'VROCN1ALK ','VROCP0ALK ','VROCP1ALK ','VROCP2ALK ','VROCP3ALK ',
     &    'VROCP4ALK ','VROCP5ALK ','VROCP6ALK ','VROCN2OXY2','VROCN2OXY4',
     &    'VROCN2OXY8','VROCN1OXY1','VROCN1OXY3','VROCN1OXY6','VROCP0OXY2',
     &    'VROCP0OXY4','VROCP1OXY1','VROCP1OXY3','VROCP2OXY2','VROCP3OXY2',
     &    'VROCP4OXY2','VROCP5OXY1','VROCP6OXY1','ECH4      '/ )

        !PRESCRIBE NITRATE SPECIES
        !CB6R3_AE7_AQ
        ALLOCATE( ISAM_SPEC_NO3( 1 )%LIST( 22 ) )
        ISAM_SPEC_NO3( 1 )%LIST = (/'HNO3 ','NO   ','NO2  ','NO3  ',
     &      'HONO ','N2O5 ','PAN  ','XO2N ','NTR1 ','NTR2 ','INTR ',
     &      'PNA  ','PANX ','CLNO2','CLNO3','XO2  ','XO2H ','MEO2 ',
     &      'ISO2 ','C2O3 ','CXO3 ','ROR  '/ )
        
        !CB6R5_AE7_AQ
        ALLOCATE( ISAM_SPEC_NO3( 2 )%LIST( 22 ) )
        ISAM_SPEC_NO3( 2 )%LIST = (/'HNO3 ','NO   ','NO2  ','NO3  ',
     &      'HONO ','N2O5 ','PAN  ','XO2N ','NTR1 ','NTR2 ','INTR ',
     &      'PNA  ','PANX ','CLNO2','CLNO3','XO2  ','XO2H ','MEO2 ',
     &      'ISO2 ','C2O3 ','CXO3 ','ROR  '/ )
        
        !CB6R5M_AE7_AQ
        ALLOCATE( ISAM_SPEC_NO3( 3 )%LIST( 27 ) )
        ISAM_SPEC_NO3( 3 )%LIST = (/'HNO3 ','NO   ','NO2  ','NO3  ',
     &      'HONO ','N2O5 ','PAN  ','XO2N ','NTR1 ','NTR2 ','INTR ',
     &      'PNA  ','PANX ','CLNO2','CLNO3','BRNO2','BRNO3','INO  ',
     &      'INO2 ','INO3 ','XO2  ','XO2H ','MEO2 ','ISO2 ','C2O3 ',
     &      'CXO3 ','ROR  '/ )

        !SAPRC07TC_AE6_AQ
        ALLOCATE( ISAM_SPEC_NO3( 4 )%LIST( 25 ) )
        ISAM_SPEC_NO3( 4 )%LIST = (/'HNO3  ','NO    ','NO2   ','NO3   ',
     &   'HONO  ','N2O5  ','PAN   ','HNO4  ','RNO3  ','PAN2  ','CLNO  ',
     &   'CLONO ','CLONO2','xNO2  ','MEO2  ','xMEO2 ','RO2C  ','RO2XC ',
     &   'MECO3 ','xMECO3','RCO3  ','xRCO3 ','BZCO3 ','PBZN  ','MACO3 '/ )

        !SAPRC07TIC_AE6I_AQ
        ALLOCATE( ISAM_SPEC_NO3( 5 )%LIST( 40 ) )
        ISAM_SPEC_NO3( 5 )%LIST = (/'HNO3    ','NO      ','NO2     ',
     &        'NO3     ','HONO    ','N2O5    ','PAN     ','HNO4    ',
     &        'RNO3    ','PAN2    ','CLNO    ','CLONO   ','CLONO2  ',
     &        'ETHLN   ','IMAE    ','IMAPAN  ','ISOPNN  ','MACRN   ',
     &        'NIT1    ','MVKN    ','PROPNN  ','MTNO3   ','xNO2    ',
     &        'MEO2    ','xMEO2   ','RO2C    ','RO2XC   ','MECO3   ',
     &        'xMECO3  ','RCO3    ','xRCO3   ','BZCO3   ','PBZN    ',
     &        'MACO3   ','TERPNRO2','ISOPO2  ','NISOPO2 ','MVKOO   ',
     &        'MACROO  ','RNO3I   '/)
        
        !SAPRC07TIC_AE7I_AQ
        ALLOCATE( ISAM_SPEC_NO3( 6 )%LIST( 40 ) )
        ISAM_SPEC_NO3( 6 )%LIST = (/'HNO3    ','NO      ','NO2     ',
     &        'NO3     ','HONO    ','N2O5    ','PAN     ','HNO4    ',
     &        'RNO3    ','PAN2    ','CLNO    ','CLONO   ','CLONO2  ',
     &        'ETHLN   ','IMAE    ','IMAPAN  ','ISOPNN  ','MACRN   ',
     &        'NIT1    ','MVKN    ','PROPNN  ','MTNO3   ','xNO2    ',
     &        'MEO2    ','xMEO2   ','RO2C    ','RO2XC   ','MECO3   ',
     &        'xMECO3  ','RCO3    ','xRCO3   ','BZCO3   ','PBZN    ',
     &        'MACO3   ','TERPNRO2','ISOPO2  ','NISOPO2 ','MVKOO   ',
     &        'MACROO  ','RNO3I   '/)

        !RACM2_AE6_AQ
        ALLOCATE( ISAM_SPEC_NO3( 7 )%LIST( 23 ) )
        ISAM_SPEC_NO3( 7 )%LIST = (/'HNO3','NO  ','NO2 ','NO3 ','HONO',
     &         'N2O5','PAN ','HNO4','ONIT','MPAN','ACO3','MO2 ','HC3P',
     &         'HC5P','HC8P','KETP','ISOP','ETHP','RCO3','XYLP','TR2 ',
     &         'BENP','ETEP'/ )

        !CRACMM1_AQ
        ALLOCATE( ISAM_SPEC_NO3( 8 )%LIST( 27 ) )
        ISAM_SPEC_NO3( 8 )%LIST = (/   'HNO3 ','NO   ','NO2  ','NO3  ','HONO ',
     &         'N2O5 ','PAN  ','HNO4 ','ONIT ','MPAN ','PPN  ','ISON ','TRPN ',
     &         'NALD ','ACO3 ','MO2  ','HC3P ','HC5P ','HC10P','KETP ','ISOP ',
     &         'ETHP ','RCO3 ','XYLP ','TR2  ','BENP ','ETEP '/ )

        !CRACMM2
        ALLOCATE( ISAM_SPEC_NO3( 9 )%LIST( 27 ) )
        ISAM_SPEC_NO3( 9 )%LIST = (/   'HNO3 ','NO   ','NO2  ','NO3  ','HONO ',
     &         'N2O5 ','PAN  ','HNO4 ','ONIT ','MPAN ','PPN  ','ISON ','TRPN ',
     &         'INALD','ACO3 ','MO2  ','HC3P ','HC5P ','HC10P','KETP ','ISOP ',
     &         'ETHP ','RCO3 ','XYLP ','TR2  ','BENP ','ETEP '/ )
        
        !PRESCRIBE SPECIES RELEVANT FOR ASSIGNING BIAS TO VOCs
        !CB6R3_AE7_AQ
        ALLOCATE( ISAM_BIAS_VOC( 1 )%LIST( 10 ) )
        ISAM_BIAS_VOC( 1 )%LIST = (/'ALD2','ALDX','FORM','ACET',
     &                'KET ','XO2 ','XO2H','ISO2','C2O3','CXO3'/ )
        
        !CB6R5_AE7_AQ
        ALLOCATE( ISAM_BIAS_VOC( 2 )%LIST( 10 ) )
        ISAM_BIAS_VOC( 2 )%LIST = (/'ALD2','ALDX','FORM','ACET',
     &                'KET ','XO2 ','XO2H','ISO2','C2O3','CXO3'/ )
        
        !CB6R5M_AE7_AQ
        ALLOCATE( ISAM_BIAS_VOC( 3 )%LIST( 10 ) )
        ISAM_BIAS_VOC( 3 )%LIST = (/'ALD2','ALDX','FORM','ACET',
     &                'KET ','XO2 ','XO2H','ISO2','C2O3','CXO3'/ )

        !SAPRC07TC_AE6_AQ
        ALLOCATE( ISAM_BIAS_VOC( 4 )%LIST( 12 ) )
        ISAM_BIAS_VOC( 4 )%LIST = (/'HCHO    ','CCHO    ','RCHO    ',
     &        'xHCHO   ','xCCHO   ','xRCHO   ','ACROLEIN','ACETONE ',
     &        'MECO3   ','xMECO3  ','RCO3    ','xRCO3   '/ )

        !SAPRC07TIC_AE6I_AQ
        ALLOCATE( ISAM_BIAS_VOC( 5 )%LIST( 14 ) )
        ISAM_BIAS_VOC( 5 )%LIST = (/'HCHO    ','CCHO    ','RCHO    ',
     &        'xHCHO   ','xCCHO   ','xRCHO   ','ACROLEIN','ACETONE ',
     &        'RO2C    ','RO2XC   ','MECO3   ','xMECO3  ','RCO3    ',
     &        'xRCO3   '/ )

        !SAPRC07TIC_AE7I_AQ
        ALLOCATE( ISAM_BIAS_VOC( 6 )%LIST( 14 ) )
        ISAM_BIAS_VOC( 6 )%LIST = (/'HCHO    ','CCHO    ','RCHO    ',
     &        'xHCHO   ','xCCHO   ','xRCHO   ','ACROLEIN','ACETONE ',
     &        'RO2C    ','RO2XC   ','MECO3   ','xMECO3  ','RCO3    ',
     &        'xRCO3   '/ )
        
        !RACM2_AE6_AQ
        ALLOCATE( ISAM_BIAS_VOC( 7 )%LIST( 12 ) )
        ISAM_BIAS_VOC( 7 )%LIST = (/'HCHO','ACD ','ALD ','UALD',
     &         'KET ','ACO3','HC3P','HC5P','HC8P','ISOP','RCO3',
     &         'XYLP'/ )

        !CRACMM1_AQ
        ALLOCATE( ISAM_BIAS_VOC( 8 )%LIST( 13 ) )
        ISAM_BIAS_VOC( 8 )%LIST = (/'HCHO ','ACD  ','ALD  ','UALD ',
     &      'KET  ','ACO3 ','HC3P ','HC5P ','HC10P','ISOP ','RCO3 ',
     &      'XYEP ', 'XYMP '/ )

        !CRACMM2
        ALLOCATE( ISAM_BIAS_VOC( 9 )%LIST( 12 ) )
        ISAM_BIAS_VOC( 9 )%LIST = (/'HCHO ','ACD  ','ALD  ','UALD ',
     &      'KET  ','ACO3 ','HC3P ','HC5P ','HC10P','ISOP ','RCO3 ',
     &      'XYLP '/ )

        !PRESCRIBE SPECIES RELEVANT FOR ASSIGNING BIAS TO NOx EMISSIONS
        !CB6R3_AE7_AQ
        ALLOCATE( ISAM_BIAS_NOY( 1 )%LIST( 6 ) )
        ISAM_BIAS_NOY( 1 )%LIST = (/'HNO3','NO  ','NO2 ','NO3 ',
     &                              'HONO','N2O5'/ )
        
        !CB6R5_AE7_AQ
        ALLOCATE( ISAM_BIAS_NOY( 2 )%LIST( 6 ) )
        ISAM_BIAS_NOY( 2 )%LIST = (/'HNO3','NO  ','NO2 ','NO3 ',
     &                              'HONO','N2O5'/ )

        !CB6R5M_AE7_AQ
        ALLOCATE( ISAM_BIAS_NOY( 3 )%LIST( 6 ) )
        ISAM_BIAS_NOY( 3 )%LIST = (/'HNO3','NO  ','NO2 ','NO3 ',
     &                              'HONO','N2O5'/ )

        !SAPRC07TC_AE6_AQ
        ALLOCATE( ISAM_BIAS_NOY( 4 )%LIST( 6 ) )
        ISAM_BIAS_NOY( 4 )%LIST = (/'HNO3','NO  ','NO2 ','NO3 ',
     &                              'HONO','N2O5'/ )

        !SAPRC07TIC_AE6I_AQ
        ALLOCATE( ISAM_BIAS_NOY( 5 )%LIST( 7 ) )
        ISAM_BIAS_NOY( 5 )%LIST = (/'HNO3','NO  ','NO2 ','NO3 ',
     &                              'HONO','N2O5','xNO2'/ )

        !SAPRC07TIC_AE7I_AQ
        ALLOCATE( ISAM_BIAS_NOY( 6 )%LIST( 7 ) )
        ISAM_BIAS_NOY( 6 )%LIST = (/'HNO3','NO  ','NO2 ','NO3 ',
     &                              'HONO','N2O5','xNO2'/ )

        !RACM2_AE6_AQ
        ALLOCATE( ISAM_BIAS_NOY( 7 )%LIST( 6 ) )
        ISAM_BIAS_NOY( 7 )%LIST = (/'HNO3','NO  ','NO2 ','NO3 ',
     &                              'HONO','N2O5' / )

        !CRACMM1_AQ
        ALLOCATE( ISAM_BIAS_NOY( 8 )%LIST( 6 ) )
        ISAM_BIAS_NOY( 8 )%LIST = (/'HNO3','NO  ','NO2 ','NO3 ',
     &                              'HONO','N2O5' / )

        !CRACMM2
        ALLOCATE( ISAM_BIAS_NOY( 9 )%LIST( 6 ) )
        ISAM_BIAS_NOY( 9 )%LIST = (/'HNO3','NO  ','NO2 ','NO3 ',
     &                              'HONO','N2O5' / )

        !PRESCRIBE SULFATE SPECIES
        !CB6R3_AE7_AQ
        ALLOCATE( ISAM_SPEC_SO4( 1 )%LIST( 1 ) )
        ISAM_SPEC_SO4( 1 )%LIST = (/'SO2'/ )
        
        !CB6R5_AE7_AQ
        ALLOCATE( ISAM_SPEC_SO4( 2 )%LIST( 1 ) )
        ISAM_SPEC_SO4( 2 )%LIST = (/'SO2', 'DMS'/ )
        
        !CB6R5M_AE7_AQ
        ALLOCATE( ISAM_SPEC_SO4( 3 )%LIST( 2 ) )
        ISAM_SPEC_SO4( 3 )%LIST = (/'SO2','DMS'/ )

        !SAPRC07TC_AE6_AQ
        ALLOCATE( ISAM_SPEC_SO4( 4 )%LIST( 1 ) )
        ISAM_SPEC_SO4( 4 )%LIST = (/'SO2'/ )

        !SAPRC07TIC_AE6I_AQ
        ALLOCATE( ISAM_SPEC_SO4( 5 )%LIST( 1 ) )
        ISAM_SPEC_SO4( 5 )%LIST = (/'SO2'/ )
        
        !SAPRC07TIC_AE7I_AQ
        ALLOCATE( ISAM_SPEC_SO4( 6 )%LIST( 1 ) )
        ISAM_SPEC_SO4( 6 )%LIST = (/'SO2'/ )

        !RACM2_AE6_AQ
        ALLOCATE( ISAM_SPEC_SO4( 7 )%LIST( 1 ) )
        ISAM_SPEC_SO4( 7 )%LIST = (/'SO2'/ )

        !CRACMM1_AQ
        ALLOCATE( ISAM_SPEC_SO4( 8 )%LIST( 1 ) )
        ISAM_SPEC_SO4( 8 )%LIST = (/'SO2'/ )

        !CRACMM2
        ALLOCATE( ISAM_SPEC_SO4( 9 )%LIST( 1 ) )
        ISAM_SPEC_SO4( 9 )%LIST = (/'SO2'/ )

        !PRESCRIBE HAP SPECIES
        !CB6R3_AE7_AQ
        ALLOCATE( ISAM_SPEC_HAPGAS( 1 )%LIST( 7 ) )
        ISAM_SPEC_HAPGAS( 1 )%LIST = (/'FORM_PRIMARY','ALD2_PRIMARY',
     &                  'ACROLEIN    ','BUTADIENE13 ','ACRO_PRIMARY',
     &                  'TOLU        ','CL2         '/)

        !CB6R5_AE7_AQ
        ALLOCATE( ISAM_SPEC_HAPGAS( 2 )%LIST( 7 ) )
        ISAM_SPEC_HAPGAS( 2 )%LIST = (/'FORM_PRIMARY','ALD2_PRIMARY',
     &                  'ACROLEIN    ','BUTADIENE13 ','ACRO_PRIMARY',
     &                  'TOLU        ','CL2         '/)

        !CB6R5M_AE7_AQ
        ALLOCATE( ISAM_SPEC_HAPGAS( 3 )%LIST( 7 ) )
        ISAM_SPEC_HAPGAS( 3 )%LIST = (/'FORM_PRIMARY','ALD2_PRIMARY',
     &                  'ACROLEIN    ','BUTADIENE13 ','ACRO_PRIMARY',
     &                  'TOLU        ','CL2         '/)

        !SAPRC07TC_AE6_AQ
        ALLOCATE( ISAM_SPEC_HAPGAS( 4 )%LIST( 6 ) )
        ISAM_SPEC_HAPGAS( 4 )%LIST = (/'HCHO_PRIMARY','CCHO_PRIMARY',
     &                  'ACROLEIN    ','BUTADIENE13 ','ACRO_PRIMARY',
     &                  'CL2         '/)

        !SAPRC07TIC_AE6I_AQ
        ALLOCATE( ISAM_SPEC_HAPGAS( 5 )%LIST( 6 ) )
        ISAM_SPEC_HAPGAS( 5 )%LIST = (/'HCHO_PRIMARY','CCHO_PRIMARY',
     &                  'ACROLEIN    ','BUTADIENE13 ','ACRO_PRIMARY',
     &                  'CL2         '/)

        !SAPRC07TIC_AE7I_AQ
        ALLOCATE( ISAM_SPEC_HAPGAS( 6 )%LIST( 6 ) )
        ISAM_SPEC_HAPGAS( 6 )%LIST = (/'HCHO_PRIMARY','CCHO_PRIMARY',
     &                  'ACROLEIN    ','BUTADIENE13 ','ACRO_PRIMARY',
     &                  'CL2         '/)

        !RACM2_AE6_AQ
        ALLOCATE( ISAM_SPEC_HAPGAS( 7 )%LIST( 1 ) )
        ISAM_SPEC_HAPGAS( 7 )%LIST = (/'DIEN        '/)

        !CRACMM1_AQ
        ALLOCATE( ISAM_SPEC_HAPGAS( 8 )%LIST( 1 ) )
        ISAM_SPEC_HAPGAS( 8 )%LIST = (/'HCHO        '/)

        !CRACMM2
        ALLOCATE( ISAM_SPEC_HAPGAS( 9 )%LIST( 1 ) )
        ISAM_SPEC_HAPGAS( 9 )%LIST = (/'HCHO        '/)

        !AEROSOL HAPS
        ALLOCATE( ISAM_SPEC_HAPAERO%LIST( 14 ) )
        ISAM_SPEC_HAPAERO%LIST = (/'ANI     ','ACR_VI  ','ACR_III ','ABE     ',
     &                             'APB     ','ADE_OTHR','ADE_EC  ','ADE_OC  ',
     &                             'ADE_NO3 ','ADE_SO4 ','ADE_CORS','ACD     ',
     &                             'AMN_HAPS','AAS     ' /)


        !NONREACTIVE TEQ TRACERS for Polycylic Aromatic Hydrocarbons
        ALLOCATE( ISAM_SPEC_PAHTEQ%LIST( 9 ) )
        ISAM_SPEC_PAHTEQ%LIST = ( /'PAH_000E0','PAH_176E5','PAH_880E5','PAH_176E4',
     &                             'PAH_176E3','PAH_192E3','PAH_101E2','PAH_176E2',
     &                             'PAH_114E1'/)   

       ALLOCATE( ISAM_SPEC_TOXICS%LIST( 30 ) )
       ISAM_SPEC_TOXICS%LIST( 1:30 ) = (/'ACRY_NITRILE','CARBONTET   ','PROPYL_DICL ',
     &     'DICL_PROPENE','CL4_ETHANE  ','CHCL3       ','BR2_C2_12   ','CL2_C2_12   ',
     &     'ETOX        ','ACET_NITRILE','XYLENE      ','STYRENE     ','ACRYACID    ',
     &     'HEXANE      ','METHCHLORIDE','CARBSULFIDE ','CHLOROPRENE ','ETHYLBENZENE',
     &     'HEXMETH_DIS ','HYDRAZINE   ','MAL_ANHYDRID','CL_ETHE     ','NAPHTHALENE ',
     &     'QUINOLINE   ','TOL_DIIS    ','TRIETHYLAMIN','DICL_BENZENE','CL2_ME      ',
     &     'CL4_ETHE    ','CL3_ETHE    '/)


        ALLOCATE( ISAM_SPEC_MERCURY%LIST( 4 ) )
        ISAM_SPEC_MERCURY%LIST = ( /'HG       ','HGIIGAS  ', 'HGIIAER  ',
     &                              'APHG     '/)
 
 
        ! Allocate Vecotrs for Species Names and Aerosol Precursors
        ALLOCATE ( ISAM_SPEC( NSPCSD, NTAG_SA ), STAT = IOST )
        CALL CHECKMEM( IOST, 'ISAM_SPEC', 'GET_NSPC_SA' )
        ISAM_SPEC = ''

        ALLOCATE ( MAP_ISAMtoAERO( NSPCSD ), STAT = IOST )
        CALL CHECKMEM( IOST, 'MAP_ISAMtoAERO', 'GET_NSPC_SA' )
        MAP_ISAMtoAERO = 0

        ALLOCATE ( MAP_AEROtoSA( N_AEROSPC, N_MODE ), STAT = IOST )
        CALL CHECKMEM( IOST, 'MAP_AEROtoSA', 'GET_NSPC_SA' )
        MAP_AEROtoSA = 0

        ALLOCATE ( MAP_AEROGAStoSA( N_AEROSPC ), STAT = IOST )
        CALL CHECKMEM( IOST, 'MAP_AEROGAStoSA', 'GET_NSPC_SA' )
        MAP_AEROGAStoSA = 0

        ALLOCATE ( MAP_AEROCTRtoSA( N_AEROSPC ), STAT = IOST )
        CALL CHECKMEM( IOST, 'MAP_AEROCTRtoSA', 'GET_NSPC_SA' )
        MAP_AEROCTRtoSA = 0

        ALLOCATE ( L_ISAM_AERO( N_AEROSPC ), STAT = IOST )
        CALL CHECKMEM( IOST, 'L_ISAM_AERO', 'GET_NSPC_SA' )
        L_ISAM_AERO = .FALSE.
 
        ! Allocate ISAM_SPECIES_BIAS array
        ALLOCATE ( ISAM_SPECIES_BIAS( NBIAS,NSPCSD ), STAT = IOST )
        ISAM_SPECIES_BIAS = .FALSE.
        IF ( IMECH .GT. 0 ) THEN
            N_VOC_BIAS = SIZE( ISAM_BIAS_VOC( IMECH )%LIST )
            N_NOY_BIAS = SIZE( ISAM_BIAS_NOY( IMECH )%LIST )
        END IF

        ! Retrieve User-Defined Tag-Classes and Print Errors/Crash CMAQ
        ! if the User is requesting tag-classes that aren't defined
        ALLOCATE( CLASS_NAMES( MAX_TAGCLASSES ) )
        CALL CHECKMEM( IOST, 'CLASS_NAMES', 'GET_NSPC_SA' )
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

        ! Initialize Control Variables
        L_VOC      = .FALSE.   
        L_NO3      = .FALSE.
        L_NH4      = .FALSE.
        L_OZONE    = .FALSE.
        L_SO4      = .FALSE.
        L_CL       = .FALSE.


        ! Initalize Number of ISAM Species
        NSPC_SA = 0

        ! Activate All Species if Requested
        IF ( INDEX( TAGCLASSES,'ALL' ) .NE. 0 ) THEN
          ! Add All Particle Species
          L_ISAM_AERO = .TRUE. 

          ! Add All Gas Species
          DO ISPEC = 1,NSPCSD
             IF ( INDEX1( CGRID_NAME(ISPEC), NSPC_SA, ISAM_SPEC(:,1) ) .EQ. 0
     &            .AND. ( CGRID_MASK_GAS( ISPEC ) .OR. 
     &                    CGRID_MASK_NR( ISPEC ) )      ) THEN
                NSPC_SA = NSPC_SA + 1
                ISAM_SPEC( NSPC_SA,: ) = CGRID_NAME( ISPEC )
             END IF
          END DO  
        ENDIF
 
        ! Activate All Particulate Species if Requested
        IF ( INDEX( TAGCLASSES,'PM_TOT' ) .NE. 0 ) THEN
          L_ISAM_AERO = .TRUE.
          L_VOC = .TRUE.
          L_NO3 = .TRUE.
          L_NH4 = .TRUE.
          L_SO4 = .TRUE.
          L_CL  = .TRUE.
        ENDIF
 
        ! Activate Particulate Ions if Requested
        IF ( INDEX( TAGCLASSES,'PM_IONS' ) .NE. 0 ) THEN
          FOUND_SPECIES = .FALSE.
          CALL ADD_ISAM_SPEC( 'ANA', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'AMG', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'AK', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'ACA', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'AFE', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'AAL', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'ASI', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'ATI', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'AMN', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'AOTHR', FOUND_SPECIES, IMECH )
          
          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'PM_IONS', FOUND_SPECIES, EFLAG )
        ENDIF
 
        ! Activate Particulate Chlorine if Requested
        IF ( INDEX( TAGCLASSES,'CHLORINE' ) .NE. 0 .OR. L_CL ) THEN
          FOUND_SPECIES = .FALSE.
          CALL ADD_ISAM_SPEC( 'ACL', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'HCL', FOUND_SPECIES, IMECH )
          
          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'CHLORINE', FOUND_SPECIES, EFLAG )
        ENDIF

        ! Activate Elemental Carbon if Requested
        IF ( INDEX( TAGCLASSES,'EC' ) .NE. 0 ) THEN
          FOUND_SPECIES = .FALSE.
          CALL ADD_ISAM_SPEC( 'AEC', FOUND_SPECIES, IMECH )
          
          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'EC', FOUND_SPECIES, EFLAG )
        ENDIF
 
        ! Activate Non-Volatile Primary Organic Aerosol if Requested
        IF ( INDEX( TAGCLASSES,'NVPOA' ) .NE. 0 ) THEN
          FOUND_SPECIES = .FALSE.

          CALL ADD_ISAM_SPEC( 'APOC', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'APNCOM', FOUND_SPECIES, IMECH )
          
          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'NVPOA', FOUND_SPECIES, EFLAG )
        END IF
 
        ! Activate Organic Aerosol if Requested
        IF ( INDEX( TAGCLASSES,'OA_TOT' ) .NE. 0 ) THEN
          L_VOC = .TRUE.
          FOUND_SPECIES = .FALSE.

          DO IAERO = 1,N_AEROSPC
             IF ( AEROSPC( IAERO )%OM )
     &          CALL ADD_ISAM_SPEC( AEROSPC(IAERO)%BULKNAME, 
     &                              FOUND_SPECIES, IMECH )
          END DO
          
          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'OA_TOT', FOUND_SPECIES, EFLAG )
        END IF

        ! Activate Particulate Sulfate if Requested
        IF ( INDEX( TAGCLASSES,'SULFATE' ) .NE. 0 .OR. L_SO4  ) THEN
          
          FOUND_SPECIES = .FALSE.
          
          CALL ADD_ISAM_SPEC( 'ASO4', FOUND_SPECIES, IMECH )
          CALL CHECK_MECH( 'SULFATE', IMECH, MECHNAME, EFLAG )
          DO ISPEC = 1,SIZE( ISAM_SPEC_SO4( IMECH )%LIST )
            CALL ADD_ISAM_SPEC( ISAM_SPEC_SO4(IMECH)%LIST(ISPEC), 
     &                          FOUND_SPECIES, IMECH )
          END DO
          
          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'SULFATE', FOUND_SPECIES, EFLAG )
        ENDIF

        ! Activate Particulate Ammonium if Requested
        IF ( INDEX( TAGCLASSES,'AMMONIUM' ) .NE. 0 .OR. L_NH4 ) THEN
          FOUND_SPECIES = .FALSE.
          CALL ADD_ISAM_SPEC( 'ANH4', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'NH3', FOUND_SPECIES, IMECH )
          
          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'AMMONIUM', FOUND_SPECIES, EFLAG )
        ENDIF
 
        ! Add Carbon Monoxide if Requested                       
        IF ( INDEX( TAGCLASSES,'CO' ) .NE. 0 ) THEN
          FOUND_SPECIES = .FALSE.
          CALL ADD_ISAM_SPEC( 'CO', FOUND_SPECIES, IMECH )
          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'CO', FOUND_SPECIES, EFLAG )
        ENDIF
 

        ! Add HAP Gases
        IF ( INDEX( TAGCLASSES,'HAP_GAS' ) .NE. 0 .OR. 
     &       INDEX( TAGCLASSES,'HAP_TOT' ) .NE. 0  ) THEN
          ! set ozone tracking to true for accurate tracking secondary production of HCHO and CH3CHO
          L_OZONE = .TRUE.
          FOUND_SPECIES = .FALSE.
          
          CALL CHECK_MECH( 'GASEOUS HAPS', IMECH, MECHNAME, EFLAG )

          ! Add each VOC Species according to fields ISAM_SPEC_GASHAPS arrays
          DO ISPEC = 1,SIZE( ISAM_SPEC_HAPGAS( IMECH )%LIST )
             CALL ADD_ISAM_SPEC( ISAM_SPEC_HAPGAS(IMECH)%LIST(ISPEC), 
     &                           FOUND_SPECIES, IMECH )
          END DO 

          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'GASEOUS HAPS', FOUND_SPECIES, EFLAG )
        ENDIF
 

        ! Add Ozone if Requested
        IF ( INDEX( TAGCLASSES,'OZONE' ) .NE. 0 .OR. L_OZONE ) THEN

          L_OZONE   = .TRUE.
          L_NO3     = .TRUE.
          L_VOC     = .TRUE.
          FOUND_SPECIES = .FALSE.
          
          CALL CHECK_MECH( 'OZONE', IMECH, MECHNAME, EFLAG )
          
          ! Add Ozone-Relevant Species
          DO ISPEC = 1,SIZE( ISAM_SPEC_OZONE( IMECH )%LIST )
             CALL ADD_ISAM_SPEC( ISAM_SPEC_OZONE(IMECH)%LIST(ISPEC), 
     &                           FOUND_SPECIES, IMECH )
          END DO

          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'OZONE', FOUND_SPECIES, EFLAG )
        END IF       

        ! Add Nitrate Species
        IF ( INDEX( TAGCLASSES,'NITRATE' ) .NE. 0 .OR. L_NO3 ) THEN 
          L_NO3 = .TRUE. 
          FOUND_SPECIES = .FALSE.

          ! Check that the chemical mechanism name is valid
          CALL CHECK_MECH( 'NITRATE', IMECH, MECHNAME, EFLAG )

          ! Add Aerosol Species
          CALL ADD_ISAM_SPEC( 'ANO3', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'AMTNO3', FOUND_SPECIES, IMECH )
          CALL ADD_ISAM_SPEC( 'AISOPNN', FOUND_SPECIES, IMECH )

          ! Add Nitrate-Relevant Gas Species
          DO ISPEC = 1,SIZE( ISAM_SPEC_NO3( IMECH )%LIST )
             CALL ADD_ISAM_SPEC( ISAM_SPEC_NO3(IMECH)%LIST(ISPEC), 
     &                           FOUND_SPECIES, IMECH )
          END DO 

          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'NITRATE', FOUND_SPECIES, EFLAG )
        ENDIF


        ! Add VOC Species for VOC Tag-Class
        IF ( INDEX( TAGCLASSES,'VOC' ) .NE. 0 .OR. L_VOC ) THEN
          CALL CHECK_MECH( 'VOC', IMECH, MECHNAME, EFLAG )

          ! Add each VOC Species according to fields ISAM_SPEC_VOC arrays
          DO ISPEC = 1,SIZE( ISAM_SPEC_VOC( IMECH )%LIST )
             CALL ADD_ISAM_SPEC( ISAM_SPEC_VOC(IMECH)%LIST(ISPEC), 
     &                           FOUND_SPECIES, IMECH )
          END DO 
          
          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'VOC', FOUND_SPECIES, EFLAG )
        END IF
 

        ! Add HAP Aerosols
        IF ( INDEX( TAGCLASSES,'HAP_AERO') .NE. 0 .OR. 
     &       INDEX( TAGCLASSES,'HAP_TOT' ) .NE. 0  ) THEN

          FOUND_SPECIES = .FALSE.
          DO I = 1,SIZE( ISAM_SPEC_HAPAERO%LIST )
             CALL ADD_ISAM_SPEC( ISAM_SPEC_HAPAERO%LIST(I), 
     &                           FOUND_SPECIES, IMECH )
          END DO

          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'HAP_AERO', FOUND_SPECIES, EFLAG )
        END IF
 

        ! Activate BaP tracking if Requested
        IF ( INDEX( TAGCLASSES,'BENAPYRENE' ) .NE. 0 .OR. 
     &       INDEX( TAGCLASSES,'HAP_TOT' ) .NE. 0 ) THEN
          
          FOUND_SPECIES = .FALSE.

          ! Find aerosol phase and add if found
          CALL ADD_ISAM_SPEC( 'ABENAPY', FOUND_SPECIES, IMECH )

          ! Add Vapor-Phase Benzo[a]Pyrene
          CALL ADD_ISAM_SPEC( 'BENAPY', FOUND_SPECIES, IMECH )
          
          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'BENAPYRENE', FOUND_SPECIES, EFLAG )
        END IF


        ! Activate TEQ PAHs tracking if Requested
        IF ( INDEX( TAGCLASSES,'PAH_TEQ' ) .NE. 0 .OR. 
     &       INDEX( TAGCLASSES,'HAP_TOT' ) .NE. 0 ) THEN
          
          FOUND_SPECIES = .FALSE.

          ! Scan for NonReactive PAH tracers
          DO I = 1,SIZE( ISAM_SPEC_PAHTEQ%LIST )
            CALL ADD_ISAM_SPEC( ISAM_SPEC_PAHTEQ%LIST(I), 
     &                          FOUND_SPECIES, IMECH )
          END DO

          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'PAH TEQ', FOUND_SPECIES, EFLAG )
        END IF
        
        ! Activate Classic Urban Air Toxics tracking if Requested
        IF ( INDEX( TAGCLASSES,'URBAN_TOXICS' ) .NE. 0 .OR. 
     &       INDEX( TAGCLASSES,'HAP_TOT' ) .NE. 0 ) THEN
        
          FOUND_SPECIES = .FALSE.

          ! Scan for NonReactive PAH tracers
          DO I = 1,SIZE( ISAM_SPEC_TOXICS%LIST )
             CALL ADD_ISAM_SPEC( ISAM_SPEC_TOXICS%LIST(I), 
     &                           FOUND_SPECIES, IMECH )
          END DO

          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'URBAN AIR TOXICS', FOUND_SPECIES, EFLAG )
        END IF


        ! Activate Mercury Toxics
        IF ( INDEX( TAGCLASSES,'MERCURY' ) .NE. 0 .OR. 
     &       INDEX( TAGCLASSES,'HAP_TOT' ) .NE. 0 ) THEN
          
          FOUND_SPECIES = .FALSE.

          ! Find aerosol phase and add if found
          CALL ADD_ISAM_SPEC( 'APHG', FOUND_SPECIES, IMECH )

          ! Add gas-phase mercury species
          DO ISPEC = 1,(SIZE( ISAM_SPEC_MERCURY%LIST ) ) 
            CALL ADD_ISAM_SPEC( ISAM_SPEC_MERCURY%LIST(ISPEC), FOUND_SPECIES, IMECH )
          END DO 

          ! Warn if all species are missing
          CALL ISAM_SPECIES_WARN( 'MERCURY', FOUND_SPECIES, EFLAG )
        END IF


        ! Add Requested Aerosol Species as defined in L_ISAM_AERO
        ! Part of this procedure involves distributing the aerosol bulknames 
        ! associated with L_ISAM_AERO into mode-dependent names needed
        ! for ISAM_SPEC. This section will also associate each ISAM
        ! aerosol species with its volatility type and gas-phase
        ! counterparts and precursors, if applicable.
        DO IAERO = 1,N_AEROSPC
           IF ( L_ISAM_AERO( IAERO ) ) THEN
              DO IMODE = 1,N_MODE
                 ! If the aerosol exists in a particular mode, then add
                 ! its name, the name of its counterpart vapor, any
                 ! reaction counters and the type of partitioning (i.e.
                 ! reversible, irreversible, or nonvolatile)
                 IF ( .NOT.AERO_MISSING( IAERO,IMODE ) ) THEN
                   MAP_ISAMtoAERO( NSPC_SA ) = IAERO

                   !Add aerosol name
                   NSPC_SA = NSPC_SA + 1
                   ISAM_SPEC( NSPC_SA,: ) = AEROSPC( IAERO )%NAME( IMODE )
                   MAP_AEROtoSA( IAERO,IMODE ) = NSPC_SA

                   !Populate Chemistry Bias Arrays if Applicable
                   IF ( IMECH .GT. 0 ) THEN
                     ISPEC = INDEX1( AEROSPC(IAERO)%BULKNAME, N_VOC_BIAS, 
     &                               ISAM_BIAS_VOC(IMECH)%LIST )
                     IF ( ISPEC .GT. 0 ) ISAM_SPECIES_BIAS( 3:4,NSPC_SA ) = .TRUE.
                     ISPEC = INDEX1( AEROSPC(IAERO)%BULKNAME, N_NOY_BIAS, 
     &                               ISAM_BIAS_NOY(IMECH)%LIST )
                     IF ( ISPEC .GT. 0 ) ISAM_SPECIES_BIAS( 2:3,NSPC_SA ) = .TRUE.
                   END IF

                   IF ( AEROSPC( IAERO )%VOLTYPE .NE. 'NVL' ) THEN 
                      NVL_RESET = 0

                      ! Add condensable vapor species
                      IF ( AEROSPC(IAERO)%GASNAME .NE. '' ) THEN
                         IF ( INDEX1( AEROSPC(IAERO)%GASNAME, 
     &                        NSPCSD, CGRID_NAME ) .GT. 0 ) THEN
                           IF ( INDEX1( AEROSPC(IAERO)%GASNAME, 
     &                               NSPC_SA, ISAM_SPEC(:,1) ) .EQ. 0 ) THEN
                              NSPC_SA = NSPC_SA + 1
                              ISAM_SPEC( NSPC_SA,:) = AEROSPC( IAERO )%GASNAME
                              MAP_AEROGAStoSA( IAERO ) = NSPC_SA
                           ELSE 
                              MAP_AEROGAStoSA( IAERO ) = 
     &                           INDEX1( AEROSPC(IAERO)%GASNAME, NSPC_SA, ISAM_SPEC(:,1) )
                           END IF
                         ELSE
                           NVL_RESET = NVL_RESET + 1
                         END IF
                      END IF
                      ! Add reaction counter species
                      IF ( AEROSPC(IAERO)%CTRNAME .NE. '' ) THEN
                         IF ( INDEX1( AEROSPC(IAERO)%CTRNAME, 
     &                             NSPCSD, CGRID_NAME ) .GT. 0 ) THEN
                           IF ( INDEX1( AEROSPC(IAERO)%CTRNAME, 
     &                               NSPC_SA, ISAM_SPEC(:,1) ) .EQ. 0 ) THEN
                              NSPC_SA = NSPC_SA + 1
                              ISAM_SPEC( NSPC_SA,:) = AEROSPC( IAERO )%CTRNAME
                              MAP_AEROCTRtoSA( IAERO ) = NSPC_SA
                           ELSE
                              MAP_AEROCTRtoSA( IAERO ) =
     &                          INDEX1( AEROSPC(IAERO)%CTRNAME, NSPC_SA, ISAM_SPEC(:,1) )

                           END IF
                         ELSE
                           NVL_RESET = NVL_RESET + 1
                         END IF
                      END IF

                      ! If neither the condensable vapor nor the
                      ! reaction counter are present in the gas species
                      ! list, then reset the aerosol species to
                      ! nonvolatile and print a note to the log
                      IF ( NVL_RESET .EQ. 2 ) THEN
                        AEROSPC( IAERO )%VOLTYPE = 'NVL'
                        GASNAME = AEROSPC(IAERO)%GASNAME
                        CTRNAME = AEROSPC(IAERO)%CTRNAME
                        IF ( GASNAME .EQ. '' ) GASNAME = 'NONE'
                        IF ( CTRNAME .EQ. '' ) CTRNAME = 'NONE'

                        XMSG = 'Attention: The ISAM aerosol species ' //
     &                         TRIM(AEROSPC( IAERO )%NAME( IMODE )) //
     &                         ' has been defined in AERO_DATA with condensable ' //
     &                         'vapor species ' // TRIM(GASNAME) // ' and/or '//
     &                         'reaction counter species ' // TRIM(CTRNAME) //
     &                         ' but neither of these are activated as ISAM ' //
     &                         'species (e.g. in the ISAM control file. The aerosol '//
     &                         'species will be assumed nonvolatile for ISAM.'
                        CALL LOG_MESSAGE( LOGDEV, XMSG )
                      END IF
                   END IF

                 END IF
              END DO
           END IF
        END DO
        
        ! Resize ISAM_SPEC to NSPC_SA
        ISAM_SPEC = ISAM_SPEC( 1:NSPC_SA,: )
        ISAM_SPECIES_BIAS = ISAM_SPECIES_BIAS( :,1:NSPC_SA )

        ! Exit CMAQ if There has been a fatal ISAM Error
        IF ( EFLAG ) THEN
          CALL M3EXIT( 'GET_NSPC_SA', 0, 0, 'Above Fatal ISAM ERROR(s) Encountered', XSTAT2 )
        END IF 

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

C do not count until 1st char in string
           
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

        SUBROUTINE GET_MAP_SAtoCGR ()

C20140428  Map CGRID species index to ISAM tracer species index
C         Called by driver.F
C

        USE CGRID_SPCS     
        USE RXNS_DATA,    ONLY : MECHNAME, NUMB_MECH_SPC, CHEMISTRY_SPC 
!        USE DEGRADE_PARAMETERS
        USE UTILIO_DEFN    

        IMPLICIT NONE
      
        INTEGER J_SPC, N, ITAG, I2, ISUM
        INTEGER N_OZN     ! index of ozone in gc_spc list
        INTEGER IOST

        CHARACTER( 16 ), ALLOCATABLE, SAVE :: CMAQ_ADV_NAMES( : ) 
        
        INTEGER FRST, LAST, CMAQ_ADV

        INTEGER I_SPC, K_SPC, NLIST
         
        CHARACTER( 132 ) :: XMSG
        INTEGER ALLOCSTAT

        LOGICAL FOUND
        LOGICAL TRANS_WARN
        LOGICAL EFLAG
c----------------------------------------------------------
        

        ! Initialize number of tags * species        
        N_SPCTAG = NSPC_SA * NTAG_SA
    
        ! Allocate vectors and arrays for ISAM species names and property
        ! fields
        ALLOCATE ( MAP_SAtoCGR( NSPC_SA ), STAT = ALLOCSTAT )
        CALL CHECKMEM( ALLOCSTAT, 'MAP_SAtoCGR', 'GET_MAP_SAtoCGR' )
        MAP_SAtoCGR = -1
        ALLOCATE ( IS_SPC_AEROSOL( NSPC_SA,NTAG_SA ), STAT = ALLOCSTAT )
        CALL CHECKMEM( ALLOCSTAT, 'IS_SPC_AEROSOL', 'IS_SPC_AEROSOL' )
        IS_SPC_AEROSOL = .FALSE.
        ALLOCATE ( TRANSPORT_SPC( N_SPCTAG ), STAT = ALLOCSTAT )
        CALL CHECKMEM( ALLOCSTAT, 'TRANSPORT_SPC', 'TRANSPORT_SPC' )
        TRANSPORT_SPC = .TRUE.

        ! Determine which ISAM species are aerosols and are transported
        ! set species names
        NSPC_TRANSPORT = 0
        ISUM  = 0
        DO ITAG = 1, NTAG_SA
           DO J_SPC = 1, NSPC_SA
              ISUM = ISUM + 1
              DO N = 1, N_AE_SPC
                 IF( TRIM( ISAM_SPEC( J_SPC,ITAG ) ) .EQ. TRIM( AE_SPC( N ) ))THEN
                     IS_SPC_AEROSOL( J_SPC,ITAG ) = .TRUE.
                     IF ( ITAG .LT. 2 ) NSPC_SA_AE = NSPC_SA_AE + 1
                 END IF
              END DO
              DO N = 1, N_AE_SPC
                 IF( TRIM( ISAM_SPEC( J_SPC,ITAG ) ) .EQ. TRIM( AE_SPC( N ) ) )THEN
                     IS_SPC_AEROSOL( J_SPC,ITAG ) = .TRUE.
                 END IF
              END DO
              FOUND = .FALSE.        
              DO N = 1, N_GC_TRNS
                 IF( ISAM_SPEC( J_SPC,ITAG ) .EQ. GC_TRNS( N ) )FOUND = .TRUE.
              END DO
              DO N = 1, N_AE_TRNS
                 IF( ISAM_SPEC( J_SPC,ITAG ) .EQ. AE_TRNS( N ) )FOUND = .TRUE.
              END DO
              DO N = 1, N_NR_TRNS
                 IF( ISAM_SPEC( J_SPC,ITAG ) .EQ. NR_TRNS( N ) )FOUND = .TRUE.
              END DO
              DO N = 1, N_TR_ADV
                 IF( ISAM_SPEC( J_SPC,ITAG ) .EQ. TR_ADV( N ) )FOUND = .TRUE.
              END DO
              TRANSPORT_SPC( ISUM ) = FOUND
              IF( FOUND )NSPC_TRANSPORT = NSPC_TRANSPORT + 1
           END DO  
        END DO

        ALLOCATE ( ISAM_TO_AERO( NSPC_SA_AE ), STAT = ALLOCSTAT )
        CALL CHECKMEM( ALLOCSTAT, 'ISAM_TO_AERO', 'GET_SPC_INDEX' )
        ISAM_TO_AERO = -1
        ALLOCATE ( SA_AERO_INDX( NSPC_SA_AE ), STAT = ALLOCSTAT )
        CALL CHECKMEM( ALLOCSTAT, 'SA_AERO_INDX', 'GET_SPC_INDEX' )
        SA_AERO_INDX = -1

        ITAG = 1
        N = 0
        DO J_SPC = 1, NSPC_SA
           IF( .NOT. IS_SPC_AEROSOL( J_SPC,ITAG ) )CYCLE
           N = N + 1
           DO I_SPC = 1, N_AE_SPC
               IF( TRIM( ISAM_SPEC( J_SPC,ITAG ) ) .NE. TRIM( AE_SPC( I_SPC ) ) ) CYCLE
               ISAM_TO_AERO( N ) = I_SPC ! + AE_STRT - 1
               SA_AERO_INDX( N ) = J_SPC
               WRITE(LOGDEV,'(A,I4,1X,I5,1X,A16,I3,1X,A16)')
     &         'ISAM SPECIES, AEROSOL SPECIES = ',N,SA_AERO_INDX( N ),ISAM_SPEC( J_SPC,ITAG ),
     &         ISAM_TO_AERO( N ),AE_SPC( I_SPC )
           END DO
        END DO


C...Assign species index with CMAQ species mappings
        ! Assign species index with CMAQ species mappings
        ALLOCATE( MAP_CGRtoSA( NSPCSD ) )
        MAP_CGRtoSA = 0

        DO N = 1, NSPCSD
           DO J_SPC = 1, NSPC_SA
             IF ( ISAM_SPEC( J_SPC,ICONTAG ) .EQ. CGRID_NAME( N ) ) THEN  
               MAP_SAtoCGR( J_SPC ) = N
               MAP_CGRtoSA( N ) = J_SPC
             END IF ! ISAM_SPEC and gc_spc match
           END DO
        END DO ! number of tagging species

        EFLAG = .FALSE.
        DO J_SPC = 1, NSPC_SA
          IF ( MAP_SAtoCGR( J_SPC ) .LT. 1 ) THEN
             write(logdev,'(A,A,A)')'Tracked species '
     &       // TRIM( ISAM_SPEC( J_SPC,ICONTAG ) ) //
     &          ' not found among model species '
             EFLAG = .TRUE.
          END IF
        END DO 
        IF ( EFLAG ) THEN
          CALL M3EXIT( 'GET_MAP_SAtoCGR', 0, 0, 'Above Fatal Error Encountered', XSTAT2 )
        END IF 

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
              MAP_ADVtoSA(N) = INDEX1( ISAM_SPEC(J_SPC,1), CMAQ_ADV, CMAQ_ADV_NAMES )
              IF( MAP_ADVtoSA(N) .LT. 1 .AND. ITAG .LT. 2 )THEN
                 TRANS_WARN = .TRUE.
                 XMSG = 'ISAM WARNING: Tagged Species, ' // TRIM( ISAM_SPEC(J_SPC,1) ) 
     &               // ' not transported. Modify species namelist(s) to include process.'
                 WRITE(LOGDEV,'(A)')TRIM( XMSG )
              END IF
           END DO
        END DO
        
        IF ( TRANS_WARN ) THEN
          CALL M3WARN( 'GET_MAP_SAtoCGR', 0, 0, 'Species above are not transported.' )
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
c             MAP_DEPVtoSA(N) = INDEX1( ISAM_SPEC(J_SPC,1), CMAQ_DEPV, CMAQ_DEPV_NAMES )
c             IF( MAP_DEPVtoSA(N) .LT. 1 )THEN
c                XMSG = 'ISAM WARNING: Tagged Species, ' // TRIM( ISAM_SPEC(J_SPC,1) )
c    &               // ' not in DEPV list.'
c                CALL M3EXIT ( 'MAP_DEPVtoSA', 0, 0, XMSG, XSTAT1 )
c             END IF
c          END DO
!       END DO

        END SUBROUTINE GET_MAP_SAtoCGR

      !-------------------------------------------------------------------------------------------   
        SUBROUTINE CHECK_MECH( TAG_NAME, IMECH, MECHNAME, EFLAG )
      !-------------------------------------------------------------------------------------------   
      !  This subroutine checks whether or not the the chemical mechanism
      !  that the user has supplied is active for ISAM and reports an
      !  error if not.
      !-------------------------------------------------------------------------------------------   
        USE RUNTIME_VARS
        
        IMPLICIT NONE

        INTEGER       :: IMECH
        CHARACTER(*)  :: TAG_NAME
        CHARACTER(32) :: MECHNAME
        LOGICAL       :: EFLAG
        CHARACTER(400):: XMSG

        IF ( IMECH .EQ. 0 ) THEN
           XMSG = "ISAM ERROR: The " // TRIM(TAG_NAME) // " tag group has been selected but the species "
     &         // "for the " // TRIM( MECHNAME ) // " mechanism are not defined. Modify GET_NSPC_SA "
     &         // "and GET_SPC_INDEX  subroutines to include MECHNAME cases for it."
                CALL LOG_MESSAGE( LOGDEV, XMSG )  
           EFLAG = .TRUE.
        END IF
        END SUBROUTINE CHECK_MECH
 
      !-------------------------------------------------------------------------------------------   
        SUBROUTINE ADD_ISAM_SPEC( CHEM_NAME, FOUND_SPECIES, IMECH )
      !-------------------------------------------------------------------------------------------   
      !  This subroutine adds an aerosol or gas-phase species to the
      !  ISAM species array and sends back a TRUE value for
      !  FOUND_SPECIES if the input CHEM_NAME is a CMAQ modeled
      !  species.
      !-------------------------------------------------------------------------------------------   
        USE CGRID_SPCS
        USE UTILIO_DEFN 
        USE AERO_DATA, only : N_AEROSPC, AEROSPC
        
        IMPLICIT NONE

        CHARACTER(*)  :: CHEM_NAME
        LOGICAL       :: FOUND_SPECIES

        INTEGER ISPEC, IAERO, IMECH, J_SPC
 
        ! Find Species Index
        ISPEC = INDEX1( CHEM_NAME, NSPCSD, CGRID_NAME )
        IAERO = INDEX1( CHEM_NAME, N_AEROSPC, AEROSPC%BULKNAME )
 
        IF ( IAERO .GT. 0 ) THEN
           ! Species is an aerosol model species
           L_ISAM_AERO( IAERO ) = .TRUE.
           FOUND_SPECIES = .TRUE.

        ELSE IF ( ISPEC .GT. 0 ) THEN
           IF ( .NOT. CGRID_MASK_AERO( ISPEC ) ) THEN
              ! Species is a gas model species
              IF ( INDEX1( CHEM_NAME, NSPC_SA, ISAM_SPEC(:,1) ) .EQ. 0 ) THEN
                 NSPC_SA = NSPC_SA + 1
                 ISAM_SPEC( NSPC_SA,: ) = CHEM_NAME

                 ! Populate Chemistry Bias Arrays, if Applicable
                 IF ( IMECH .NE. 0 ) THEN
                   J_SPC = INDEX1( CHEM_NAME, N_VOC_BIAS, ISAM_BIAS_VOC(IMECH)%LIST )
                   IF ( J_SPC .GT. 0 ) ISAM_SPECIES_BIAS( 3:4,NSPC_SA ) = .TRUE.
                   J_SPC = INDEX1( CHEM_NAME, N_NOY_BIAS, ISAM_BIAS_NOY(IMECH)%LIST )
                   IF ( J_SPC .GT. 0 ) ISAM_SPECIES_BIAS( 2:3,NSPC_SA ) = .TRUE.
                 END IF

              END IF 
              FOUND_SPECIES = .TRUE.
           END IF

        END IF   

        END SUBROUTINE ADD_ISAM_SPEC

      !-------------------------------------------------------------------------------------------   
        SUBROUTINE ISAM_SPECIES_WARN( TAG_NAME, FOUND_SPECIES, EFLAG )
      !-------------------------------------------------------------------------------------------   
      !  This subroutine prints a warning if no species in a tag class
      !  are found on CMAQ modeled species list.
      !-------------------------------------------------------------------------------------------   
        USE RUNTIME_VARS
        
        IMPLICIT NONE

        CHARACTER(*)  :: TAG_NAME
        LOGICAL       :: FOUND_SPECIES
        LOGICAL       :: EFLAG
        CHARACTER(400):: XMSG


        IF ( .NOT. FOUND_SPECIES ) THEN
           EFLAG = .TRUE.
           XMSG = " ISAM ERROR: No member of "//TRIM(TAG_NAME)//" tag class found "
     &          // "in species namelists but control file requests the class."
           CALL LOG_MESSAGE( LOGDEV, XMSG )
        END IF
 
        END SUBROUTINE ISAM_SPECIES_WARN
 


      END MODULE SA_DEFN
