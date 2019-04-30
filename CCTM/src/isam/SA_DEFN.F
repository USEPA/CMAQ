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

c...Tagging species, regular or combined
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

c...4th and 5th dimensions of ISAM array
      INTEGER ::     NSPC_SA
      INTEGER ::     NTAG_SA
      INTEGER ::     NSPC_TRANSPORT

c...Define Allocatable Apportionment Arrays
      REAL, POINTER :: ISAM  ( :,:,:,:,: )
      REAL, ALLOCATABLE :: AISAM ( :,:,:,:,: )
      REAL, ALLOCATABLE :: TOT_SADEP ( :,:,:,: )
      REAL, ALLOCATABLE :: CONV_SADEP ( :,:,:,: )
      REAL, ALLOCATABLE :: MAPFRAC( :,:,: )
      REAL, ALLOCATABLE :: MFRC_P( :,:,: )
      REAL, ALLOCATABLE :: VOC5D ( :,:,:,:,: )
      REAL, ALLOCATABLE :: SA_VDEMIS_DIFF_ALL( :,:,:,:,: ) ! full emissions array mapped to diffused species
      REAL, ALLOCATABLE :: SA_VDEMIS_DIFF_OTHER( :,:,:,: ) ! emissions left for 'OTHRTAG'
      REAL, ALLOCATABLE :: SA_VDEMIS_DIFF( :,:,:,:,: ) 

c...Variables regarding the tag list
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: TAGNAME( : )
      CHARACTER( 96 )                    :: TAGCLASSES
      CHARACTER( 96 ), ALLOCATABLE, SAVE :: TAGSTREAMS_TEMP( : )
      CHARACTER( 96 ), ALLOCATABLE, SAVE :: TAGSTREAMS( :, : )
      INTEGER, ALLOCATABLE, SAVE         :: TAGSTREAMS_NUM( : )

      CHARACTER( 96 ), ALLOCATABLE, SAVE :: ISAMRGN_TEMP( : )
      CHARACTER( 96 ), ALLOCATABLE, SAVE :: ISAMRGN( :, : )
      INTEGER, ALLOCATABLE, SAVE         :: ISAMRGN_NUM( : )
      INTEGER, ALLOCATABLE, SAVE         :: ISAMRGN_MAP( :, : )

c...Logical values for tagging species
      LOGICAL, SAVE :: L_EC
      LOGICAL, SAVE :: L_OC
      LOGICAL, SAVE :: L_SFATE
      LOGICAL, SAVE :: L_NITRATE
      LOGICAL, SAVE :: L_NH4
      LOGICAL, SAVE :: L_PM25
      LOGICAL, SAVE :: L_CO
      LOGICAL, SAVE :: L_OZONE
      LOGICAL, SAVE :: L_VOC

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
      INTEGER :: BCONTAG
      INTEGER :: OTHRTAG
      INTEGER :: ICONTAG

      REAL, PRIVATE, ALLOCATABLE :: BUFF2( :,: )

! Diffusion module variables
      REAL, ALLOCATABLE, SAVE :: SA_DDEP( :,:,: )

! Advection module variables
      LOGICAL, ALLOCATABLE, SAVE :: BCON_SPC( : )

      CONTAINS

C============================================================

        SUBROUTINE CNT_SA_IO_LIST ( NTAGS )

C20140428  Counts the number of emissions tags in the input control file
C         Called by sa_dim.F
C
C

        USE UTILIO_DEFN     ! 20120615
        USE HGRD_DEFN       ! just for mype0 20130702

        IMPLICIT NONE

!0615   INCLUDE SUBST_IOPARMS
C       INCLUDE SUBST_IODECL 

        CHARACTER( 16 ) :: PNAME = 'CNT_SA_IO_LIST'
        CHARACTER( 256 ) :: EQNAME 
        INTEGER INPUT_UNIT
        INTEGER IOST
        CHARACTER( 80 ) :: XMSG   
C external functions
        INTEGER LEN_TRIM

        INTEGER, INTENT( OUT ) :: NTAGS
        INTEGER ILINE
        CHARACTER( 21 ) :: TXTLINE
        
C-----------------------------------------------------------
        CALL NAMEVAL( 'SA_IOLIST', EQNAME )
        INPUT_UNIT = JUNIT()
         
        OPEN ( UNIT = INPUT_UNIT, FILE = EQNAME, STATUS = 'OLD', IOSTAT = IOST )
        XMSG = 'Error Opening SA_IO_LIST file'
        IF ( IOST .NE. 0 ) THEN
          CALL M3EXIT ( 'SA_IOLIST', 0, 0, XMSG, XSTAT1 )
        ENDIF

        IF ( MYPE .EQ. 0 ) THEN        
          PRINT*, 'SA_IO_LIST Sucessfully Opened'
          PRINT*, 'Start counting the list...'
        ENDIF
        NTAGS = 0
        COUNTTAG: DO 
          READ ( INPUT_UNIT, '(A)' ) TXTLINE
          IF ( TXTLINE( 1:7 ) .EQ. 'ENDLIST' ) EXIT COUNTTAG
          IF ( TXTLINE( 1:7 ) .EQ. 'TAG NAM' ) THEN
            NTAGS = NTAGS + 1
            IF ( MYPE .EQ. 0 ) PRINT*, TXTLINE
          ENDIF ! if tag_name
        ENDDO COUNTTAG

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

        USE UTILIO_DEFN      ! 20120615
        USE HGRD_DEFN        ! just for mype

        IMPLICIT NONE

        CHARACTER( 16 ) :: PNAME = 'RD_SA_IO_LIST'
        CHARACTER( 256 ) :: EQNAME 
        INTEGER INPUT_UNIT
        INTEGER IOST
        CHARACTER( 80 ) :: XMSG   
C external functions
        INTEGER LEN_TRIM

        INTEGER, INTENT( IN ) :: NTAGS
        INTEGER ILINE
        INTEGER ITAG
        CHARACTER( 128 ) :: TXTLINE

C...multi-sectors-for-a-single-tag 20130702
        INTEGER ISGSTK
        CHARACTER( 2 )  :: CSGSTK
        CHARACTER( 16 ) :: FNAME
        LOGICAL LBACK
        INTEGER BGN_SG

C-----------------------------------------------------------
        CALL NAMEVAL( 'SA_IOLIST', EQNAME )
        INPUT_UNIT = JUNIT()

        OPEN ( UNIT = INPUT_UNIT, FILE = EQNAME, STATUS = 'OLD', IOSTAT = IOST )
        XMSG = 'Error Opening SA_IO_LIST file'
        IF ( IOST .NE. 0 ) THEN
          CALL M3EXIT ( 'SA_IOLIST', 0, 0, XMSG, XSTAT1 )
        ENDIF
        
101     READ ( INPUT_UNIT, '(A)' ) TXTLINE
        IF ( TXTLINE(1:3) .EQ. '!!!' .OR. TXTLINE(1:3) .EQ. '' ) GOTO 101
        TAGCLASSES = TXTLINE(18:LEN_TRIM( TXTLINE ) )

        IF ( MYPE .eq. 0 ) then
          WRITE(LOGDEV, *) 'Reading ISAM control file'
          WRITE(LOGDEV, *) 'TAGCLASSES = ',TRIM(TAGCLASSES)
          WRITE(LOGDEV, *) '  for ', NTAGS, ' tags'
        ENDIF

        DO ITAG = 1, ( NTAGS )
102       READ ( INPUT_UNIT, '(A)' ) TXTLINE
          IF ( TXTLINE(1:3) .EQ. '!!!' .OR. TXTLINE(1:3) .EQ. '' ) GOTO 102
          TAGNAME( ITAG ) = TXTLINE(18:LEN_TRIM( TXTLINE ) )

103       READ ( INPUT_UNIT, '(A)' ) TXTLINE
          IF ( TXTLINE(1:3) .EQ. '!!!' .OR. TXTLINE(1:3) .EQ. '' ) GOTO 103
          ISAMRGN_TEMP( ITAG ) = TXTLINE(18:LEN_TRIM( TXTLINE ) )

104       READ ( INPUT_UNIT, '(A)' ) TXTLINE
          IF ( TXTLINE(1:3) .EQ. '!!!' .OR. TXTLINE(1:3) .EQ. '' ) GOTO 104
          TAGSTREAMS_TEMP( ITAG ) = TXTLINE(18:LEN_TRIM( TXTLINE ) )
        END DO

        END SUBROUTINE RD_SA_IO_LIST
C============================================================
        
        SUBROUTINE GET_NSPC_SA ()

C20140428  Determine number of ISAM species
C         Called by sa_dim.F
C

        USE RUNTIME_VARS
        USE GRID_CONF    ! just for mype 20140327
        USE RXNS_DATA, ONLY : MECHNAME !Get Chemical Mechanism Name
        USE UTILIO_DEFN  ! 20130627

        IMPLICIT NONE

        INTEGER :: ITAG
        INTEGER :: J

        LOGICAL :: LBACK
        LOGICAL :: DEFINED 
        INTEGER :: BGN_SP
        
        CHARACTER( 132 ) :: XMSG

        INTEGER :: IOST

c----------------------------------------------------------

        L_EC     = .FALSE. 
        L_OC     = .FALSE. 
        L_SFATE  = .FALSE. 
        L_NITRATE = .FALSE. 
        L_NH4    = .FALSE. 
        L_PM25   = .FALSE.   
        L_CO     = .FALSE.   
        L_OZONE  = .FALSE.   
        L_VOC    = .FALSE.   

        NSPC_SA = 0

        LBACK = .FALSE. 
        BGN_SP = INDEX( TAGCLASSES,'EC',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          NSPC_SA = NSPC_SA + 2  ! AECJ, AECI
          L_EC = .TRUE.
        ENDIF

        BGN_SP = INDEX( TAGCLASSES,'OC',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          IF( .NOT. ZERO_PCSOA ) THEN
            NSPC_SA = NSPC_SA + 13  ! ALVPO1I,ALVPO1J,ASVPO1I,ASVPO1J,ASVPO2I,ASVPO2J,ASVPO3J,AIVPO1J
                                    ! VLVPO1,VSVPO1,VSVPO2,VSVO3,VIVPO1
          ELSE
            NSPC_SA = NSPC_SA + 4  ! APOCJ, APOCI, APNCOMJ, APNCOMI  20120711
          ENDIF
          L_OC = .TRUE.
        ENDIF

        BGN_SP = INDEX( TAGCLASSES,'SULFATE',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          NSPC_SA = NSPC_SA + 5  ! ASO4J, ASO4I, SO2, SULF(Fb21), SULRXN(20130529)
          L_SFATE = .TRUE.
        ENDIF

        BGN_SP = INDEX( TAGCLASSES,'OZONE',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          NSPC_SA = NSPC_SA + 3  ! O3, O or O3P, HO2
          L_OZONE   = .TRUE.
          L_NITRATE = .TRUE.
          L_VOC     = .TRUE.
        END IF       

        BGN_SP = INDEX( TAGCLASSES,'NITRATE',LBACK )
        L_NITRATE = ( BGN_SP .NE. 0 .OR. L_NITRATE )

        BGN_SP = INDEX( TAGCLASSES,'VOC',LBACK )
        L_VOC = ( BGN_SP .NE. 0 .OR. L_VOC )

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
                NSPC_SA = NSPC_SA + 5  ! NTR1, NTR2, INTR, CLNO2, CLNO3
             CASE( 'CB6R3M_AE7_KMTBR' )
                NSPC_SA = NSPC_SA + 10  ! NTR1, NTR2, INTR, CLNO2, CLNO3, BRNO3, BRNO2, INO, INO3, INO2, 
             CASE( 'SAPRC07TC_AE6_AQ' ) 
                NSPC_SA = NSPC_SA + 4  ! RNO3, CLNO, CLONO, CLONO2
             CASE( 'RACM2_AE6_AQ') 
                NSPC_SA = NSPC_SA + 1  ! ONIT
             CASE( 'SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ' ) 
                NSPC_SA = NSPC_SA + 15 ! RNO3, CLNO, CLONO2, CLONO2, ETHLN, IMAE, IMAPAN, ISOPNN, MACRN, NIT1, MVKN, PROPNN, MTNO3, AMTNO3J
          END SELECT
! specices common to all mechanisms          
          NSPC_SA = NSPC_SA + 11  ! NO, NO2, NO3, ANO3J, ANO3I, HNO3, HONO, N2O5, PNA or HNO4, PAN, PANX or PAN2 or MPAN
        ENDIF

        IF ( L_VOC ) THEN
          SELECT CASE( MECHNAME )
             CASE( 'CB6R3_AE6_AQ' )
                NSPC_SA = NSPC_SA + 21
             CASE( 'CB6R3_AE7_AQ', 'CB6R3M_AE7_KMTBR' )
                NSPC_SA = NSPC_SA + 22
             CASE( 'RACM2_AE6_AQ') 
                NSPC_SA = NSPC_SA + 33
             CASE( 'SAPRC07TC_AE6_AQ', 'SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ' ) 
                NSPC_SA = NSPC_SA + 36 
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
          NSPC_SA = NSPC_SA + 14 ! ACLiJ,ANAiJ,AMGJ,AKJ,ACAJ,AFEJ,AALJ,ASIJ,ATIJ,AMNJ,AOTHRiJ(0705) 
          L_PM25 = .TRUE.
        ENDIF

        LBACK = .FALSE.
        BGN_SP = INDEX( TAGCLASSES,'CO',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          NSPC_SA = NSPC_SA + 1  ! CO
          L_CO = .TRUE.
        ENDIF

 
        END SUBROUTINE GET_NSPC_SA

c===============================================================

        SUBROUTINE GET_SPC_INDEX ()

C20140428  Map CGRID species index to ISAM tracer species index
C         Called by driver.F
C

        USE CGRID_SPCS       ! 20120615
        USE RXNS_DATA, ONLY : MECHNAME !Get Chemical Mechanism Name
        USE HGRD_DEFN        ! 20120710 just for print out from single processor
        USE UTILIO_DEFN      ! 20120725 for external functions such as INDEX1

        IMPLICIT NONE
      
        INTEGER J_SPC, N, ITAG, I2, ISUM
        INTEGER N_OZN     ! index of ozone in gc_spc list
        INTEGER ALLOCSTAT

        CHARACTER( 16 ), ALLOCATABLE, SAVE :: CMAQ_ADV_NAMES( : )  
        INTEGER FRST, LAST, CMAQ_ADV

        CHARACTER( 132 ) :: XMSG

        LOGICAL FOUND
        LOGICAL TRANS_WARN
        LOGICAL EFLAG
c----------------------------------------------------------
        

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

        J_SPC = 0
         
        EFLAG = .FALSE.
         
        IF (  L_EC ) THEN
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'AECJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'AECI'   
        ENDIF

        IF ( L_OC ) THEN
          IF( .NOT. ZERO_PCSOA ) THEN
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'ALVPO1J'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'ALVPO1I'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'ASVPO1J'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'ASVPO1I'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'ASVPO2J'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'ASVPO2I'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'ASVPO3J'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'AIVPO1J'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'VLVPO1'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'VSVPO1'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'VSVPO2'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'VSVPO3'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'VIVPO1'
          ELSE
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'APOCJ'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'APOCI'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'APNCOMJ'
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'APNCOMI'
          ENDIF
        ENDIF

          
        IF ( L_SFATE ) THEN
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ASO4J'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ASO4I'    
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'SO2'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'SULF'    
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'SULRXN'
        ENDIF

        IF ( L_NITRATE ) THEN
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ANO3J'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ANO3I'    
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'HNO3'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'NO'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'NO2'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'NO3'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'HONO'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'N2O5'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'PAN'
          SELECT CASE( MECHNAME )
             CASE( 'CB6R3_AE6_AQ', 'CB6R3_AE7_AQ' )
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'NTR1' 
               J_SPC = J_SPC + 1  
               SPC_NAME( J_SPC,: ) = 'NTR2'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'INTR'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'PNA'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'PANX'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'CLNO2'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'CLNO3'
             CASE( 'CB6R3M_AE7_KMTBR' )
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'NTR1' 
               J_SPC = J_SPC + 1  
               SPC_NAME( J_SPC,: ) = 'NTR2'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'INTR'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'PNA'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'PANX'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'CLNO2'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'CLNO3'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'BRNO2'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'BRNO3'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'INO'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'INO2'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'INO3'
             CASE( 'SAPRC07TC_AE6_AQ' ) 
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'HNO4'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'RNO3'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'PAN2'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'CLNO'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'CLONO'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'CLONO2'
             CASE( 'SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ' ) 
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'HNO4'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'RNO3'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'PAN2'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'CLNO'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'CLONO'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'CLONO2'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'ETHLN'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'IMAE'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'IMAPAN'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'ISOPNN'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'MACRN'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'NIT1'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'MVKN'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'PROPNN'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'MTNO3'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'AMTNO3J'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'AISOPNNJ'   
             CASE( 'RACM2_AE6_AQ') 
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'HNO4'
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'ONIT'   
               J_SPC = J_SPC + 1
               SPC_NAME( J_SPC,: ) = 'MPAN'
          END SELECT 
        ENDIF ! L_NITRATE

        IF ( L_VOC ) THEN
          SELECT CASE( MECHNAME )
             CASE( 'CB6R3_AE6_AQ' )
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ALD2'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ALDX'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ETH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ETHA'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ETOH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'FORM'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'IOLE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ISOP'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MEOH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'OLE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'PAR'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'TERP'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'TOL'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'XYLMN'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'NAPH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ETHY'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'PRPA'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ACET'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'KET'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'GLY'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'BENZENE'
             CASE( 'CB6R3_AE7_AQ', 'CB6R3M_AE7_KMTBR' )
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ALD2'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ALDX'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ETH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ETHA'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ETOH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'FORM'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'IOLE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ISOP'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MEOH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'OLE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'PAR'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'TERP'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'TOL'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'XYLMN'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'NAPH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ETHY'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'PRPA'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ACET'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'KET'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'GLY'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'BENZENE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'APIN'
             CASE( 'SAPRC07TC_AE6_AQ', 'SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ' ) 
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'HCHO'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'CCHO'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'RCHO'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ALK1'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ALK2'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ALK3'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ALK4'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ALK5'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ETHENE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ACETYLENE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'PROPENE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ISOPRENE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'TERP'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'SESQ'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'APIN'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'OLE1'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'OLE2'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ETOH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MEOH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'GLY'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MGLY'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'BENZENE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'TMBENZ124'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'TOLUENE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MXYL'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'OXYL'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'PXYL'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'NAPHTHAL'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ARO1'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ARO2MN'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'BUTADIENE13'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ACROLEIN'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ACETONE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MEK'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MVK'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MACR'
             CASE( 'RACM2_AE6_AQ') 
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'HCHO'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ACD'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ALD'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'UALD'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'HC3'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'HC5'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'HC8'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'OLI'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'OLT'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ETH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ETE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ACE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ISO'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'SESQ'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'LIM'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'API'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'EOH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MOH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'GLY'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MGLY'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'BENZENE'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'TOL'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'XYLM'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'XYLO'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'XYLP'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'NAPH'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'DIEN'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'ACT'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'HKET'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'KET'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MVK'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MEK'
                J_SPC = J_SPC + 1
                SPC_NAME( J_SPC,: ) = 'MACR'
          END SELECT
        ENDIF ! L_VOC

        IF (  L_NH4 ) THEN
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ANH4J'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ANH4I'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'NH3'
        ENDIF

        IF ( L_PM25 ) THEN
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ACLJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ACLI'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ANAJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ANAI' 
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'AMGJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'AKJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ACAJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'AFEJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'AALJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ASIJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ATIJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'AMNJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'AOTHRJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'AOTHRI'
        ENDIF

        IF ( L_CO ) THEN
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'CO'
        ENDIF

        IF ( L_OZONE ) THEN
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'O3'
          J_SPC = J_SPC + 1
          SELECT CASE( MECHNAME )
             CASE( 'CB6R3_AE6_AQ', 'CB6R3_AE7_AQ', 'CB6R3M_AE7_KMTBR')
                SPC_NAME( J_SPC,: ) = 'O'
             CASE( 'SAPRC07TC_AE6_AQ', 'RACM2_AE6_AQ', 'SAPRC07TIC_AE6I_AQ', 'SAPRC07TIC_AE7I_AQ' ) 
                SPC_NAME( J_SPC,: ) = 'O3P'
          END SELECT
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'HO2'
        ENDIF


C...Check if the above adds up to number of tagging species
        IF ( J_SPC .NE. NSPC_SA ) THEN
          WRITE(LOGDEV,'(A,I4,A,I4,3A)')'Tagged species found, ', J_SPC,  
     &    ', does not equal expected, ',  NSPC_SA,
     &    ', for the ', TRIM( MECHNAME ), ' mechanism.'
          EFLAG = .TRUE.
        ENDIF

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

        DO N = 1, NSPCSD
          IF ( N .GE. GC_STRT .AND. N .LE. GC_FINI ) THEN
            DO J_SPC = 1, NSPC_SA
              IF ( SPC_NAME( J_SPC,ICONTAG ) .EQ. GC_SPC( N ) ) THEN  
                SPC_INDEX( J_SPC,1 ) = 1
                SPC_INDEX( J_SPC,2 ) = N
                MAP_CGRtoSA( N ) = J_SPC
              END IF ! spc_name and gc_spc match
            END DO
          ELSE IF ( N .GE. AE_STRT .AND. N .LT. AE_FINI ) THEN
            DO J_SPC = 1, NSPC_SA
              IF ( SPC_NAME( J_SPC,ICONTAG ) .EQ. AE_SPC( N-AE_STRT+1 ) ) THEN
                SPC_INDEX( J_SPC,1 ) = 1
                SPC_INDEX( J_SPC,2 ) = N
                MAP_CGRtoSA( N ) = J_SPC
              END IF ! spc_name and ae_spc match
            END DO
          ELSE IF ( N .GE. NR_STRT .AND. N .LE. NR_FINI ) THEN
            DO J_SPC = 1, NSPC_SA
              IF ( SPC_NAME( J_SPC,ICONTAG ) .EQ. NR_SPC( N-NR_STRT+1 ) ) THEN
                SPC_INDEX( J_SPC,1 ) = 1
                SPC_INDEX( J_SPC,2 ) = N
                MAP_CGRtoSA( N ) = J_SPC
              END IF
            END DO          
          END IF          
        END DO ! number of tagging species

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

        END SUBROUTINE GET_SPC_INDEX

      END MODULE SA_DEFN
