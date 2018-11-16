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
! 
      USE RXNS_DATA, ONLY : MECHNAME !Get Chemical Mechanism Name

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
      INTEGER, ALLOCATABLE, SAVE   :: STREAM_TO_TAG ( : )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_DIFFtoSA( : )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_ADVtoSA( : )

c...4th and 5th dimensions of ISAM array
      INTEGER ::     NSPC_SA
      INTEGER ::     NTAG_SA
      INTEGER ::     NSPC_TRANSPORT

c...Define Allocatable Apportionment Arrays
      !REAL, ALLOCATABLE :: ISAMB4( :,:,:,:,: )
      !REAL, ALLOCATABLE :: ISAM  ( :,:,:,:,: )
      !REAL, ALLOCATABLE :: CBFOR ( :,:,:,: )
c     REAL, POINTER :: ISAMB4( :,:,:,:,: )
      REAL, POINTER :: ISAM  ( :,:,:,:,: )
c     REAL, POINTER :: CBFOR ( :,:,:,: )
      REAL, ALLOCATABLE :: AISAM ( :,:,:,:,: )
      REAL, ALLOCATABLE :: TOT_SADEP ( :,:,:,: )
      REAL, ALLOCATABLE :: CONV_SADEP ( :,:,:,: )
      REAL, ALLOCATABLE :: MAPFRAC( :,:,: )
      REAL, ALLOCATABLE :: MFRC_P( :,:,: )
      REAL, ALLOCATABLE :: VOC5D ( :,:,:,:,: )
      REAL, ALLOCATABLE :: SA_VDEMIS_DIFF_ALL( :,:,:,:,: ) ! full emissions array mapped to diffused species 
      REAL, ALLOCATABLE :: SA_VDEMIS_DIFF( :,:,:,:,: ) 

c...Variables regarding the tag list
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: TAGNAME( : )
      CHARACTER( 96 )                    :: TAGCLASSES
      CHARACTER( 40 ), ALLOCATABLE, SAVE :: TAGRGN( : )
      CHARACTER( 96 ), ALLOCATABLE, SAVE :: TAGSTREAMS_TEMP( : )
      CHARACTER( 96 ), ALLOCATABLE, SAVE :: TAGSTREAMS( :, : )
      INTEGER, ALLOCATABLE, SAVE         :: TAGSTREAMS_NUM( : )

c...Logical values for tagging species
      LOGICAL, SAVE :: L_EC
      LOGICAL, SAVE :: L_OC
      LOGICAL, SAVE :: L_SFATE
      LOGICAL, SAVE :: L_NTRATE
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

!20140321  Option to use Ox production-loss for ozone apportionment
c     LOGICAL :: FLAGOXSA

!20140416 Option to renormalise ISAM
      LOGICAL :: YES_RENORM

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
!0615   INTEGER JUNIT
        INTEGER LEN_TRIM
C external above

C Text lines
        INTEGER, INTENT( OUT ) :: NTAGS
        INTEGER ILINE
        CHARACTER( 21 ) :: TXTLINE
C text lines above     
        
C-----------------------------------------------------------
        CALL NAMEVAL( 'SA_IOLIST', EQNAME )
        INPUT_UNIT = JUNIT()
         
        OPEN ( UNIT = INPUT_UNIT, FILE = EQNAME, IOSTAT = IOST )
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

!0615   INCLUDE SUBST_IOPARMS
C       INCLUDE SUBST_IODECL 

        CHARACTER( 16 ) :: PNAME = 'RD_SA_IO_LIST'
        CHARACTER( 256 ) :: EQNAME 
        INTEGER INPUT_UNIT
        INTEGER IOST
        CHARACTER( 80 ) :: XMSG   
C external functions
!0615   INTEGER JUNIT
        INTEGER LEN_TRIM
C external above

C Text lines
        INTEGER, INTENT( IN ) :: NTAGS
        INTEGER ILINE
        INTEGER ITAG
        CHARACTER( 128 ) :: TXTLINE
C text lines above     

C...multi-sectors-for-a-single-tag 20130702
        INTEGER ISGSTK
        CHARACTER( 2 )  :: CSGSTK
        CHARACTER( 16 ) :: FNAME
        LOGICAL LBACK
        INTEGER BGN_SG

C...stack group id counting 20130731
        INTEGER CNTCMA ! number of commas on a text line

C-----------------------------------------------------------
        CALL NAMEVAL( 'SA_IOLIST', EQNAME )
        INPUT_UNIT = JUNIT()

        OPEN ( UNIT = INPUT_UNIT, FILE = EQNAME, IOSTAT = IOST )
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
          TAGRGN( ITAG ) = TXTLINE(18:LEN_TRIM( TXTLINE ) )

104       READ ( INPUT_UNIT, '(A)' ) TXTLINE
          IF ( TXTLINE(1:3) .EQ. '!!!' .OR. TXTLINE(1:3) .EQ. '' ) GOTO 104
          TAGSTREAMS_TEMP( ITAG ) = TXTLINE(18:LEN_TRIM( TXTLINE ) )

          IF ( MYPE .eq. 0 ) then
            WRITE(LOGDEV, *) '   ISAM Tag #   ',ITAG
            WRITE(LOGDEV, *) '   ISAM Region  ',TAGRGN( ITAG )
            WRITE(LOGDEV, *) '   ISAM Streams ',TAGSTREAMS_TEMP( ITAG )
          END IF

        END DO

        END SUBROUTINE RD_SA_IO_LIST
C============================================================
        
        SUBROUTINE MAP_FRAC( )

C20140428  Determine map fractions of regions on each grid cell
C         Called by driver.F
C

        USE GRID_CONF
        USE UTILIO_DEFN       ! 20120615
        
        IMPLICIT NONE


        CHARACTER( 16 ) :: MAPNAME

        CHARACTER( 16 ) :: PNAME = 'MAP_FRAC'

        INTEGER NRGNS
        INTEGER IRGN
        INTEGER ITAG
        INTEGER NTAGS
        INTEGER IOST
C       INTEGER COL1, ROW1
        INTEGER C, R
        CHARACTER( 80 ) :: XMSG
        INTEGER ALLOCSTAT

C Interim variables
        CHARACTER( 256 ) :: EQNAME

c External functions
!0615   CHARACTER( 16 ) :: PROMPTMFILE
        INTEGER LEN_TRIM

        !20140512
        INTEGER, EXTERNAL :: SETUP_LOGDEV  
c external above  
        
        INTEGER  GXOFF, GYOFF
        INTEGER, SAVE :: STRTCOL, ENDCOL, STRTROW, ENDROW              

C0709...optional test
        LOGICAL :: LTEST

!20130702 ...Multiple regions for single tags
        INTEGER YESRGN
        INTEGER LENRGN   ! 20130801

!20130715
        INTEGER LENVNAM  ! length of variable name from map frac file

!20130716
        LOGICAL YES_XTRAC ! true if sector is to be extracted

!...stack group id counting 20130731
        LOGICAL YES_INLN  ! true if inline group id request is ever picked up
        INTEGER CNTCMA ! number of commas on a text line
        INTEGER, ALLOCATABLE :: LOCATCMA( : )  ! commas' location on a text line
        INTEGER JCNT
        !SA_ID values
        CHARACTER( 6 ) CVALU    ! XXXX.X read off from SA_ID line
        REAL    VALU            ! converted from the characters XXX.X

        INTERFACE
          SUBROUTINE COUNTCOMMAS( TXTLN, NCMAS )
            IMPLICIT NONE
            CHARACTER( * ), INTENT( IN ) :: TXTLN
            INTEGER,       INTENT( OUT ) :: NCMAS
          END SUBROUTINE  COUNTCOMMAS
          SUBROUTINE LOCATECOMMAS( TXTLN, NCMAS, CMAPOS )
            IMPLICIT NONE
            CHARACTER( * ), INTENT( IN ) :: TXTLN
            INTEGER,        INTENT( IN ) :: NCMAS
            INTEGER,       INTENT( OUT ) :: CMAPOS( NCMAS )
          END SUBROUTINE  LOCATECOMMAS
        END INTERFACE

C------------------------------------------------------------


        NTAGS = NTAG_SA - 3

        ALLOCATE ( MAPFRAC( MY_NCOLS, MY_NROWS, NTAG_SA-3 ), STAT =ALLOCSTAT )
        CALL CHECKMEM( ALLOCSTAT, 'MAPFRAC', PNAME )

        CALL ENVSTR( 'SA_APPMAP','Source region ncf file','SA_APPMAP',
     &         EQNAME, IOST )
        if ( MYPE .eq. 0 ) print*,'EQNAME is ',EQNAME
        IF ( IOST .EQ. 1 ) THEN
          print*, 'Environment variable improperly formatted'
          stop
        ELSE IF ( IOST .EQ. -1 ) THEN
          MAPFRAC = 1.0
          if ( MYPE .eq. 0 ) print*, 
     &       'Environment variable set, but empty ... Carry On...'
!20140512          RETURN
        ELSE IF ( IOST .EQ. -2 ) THEN
          MAPFRAC = 1.0
          if ( MYPE .eq. 0 ) print*,
     &       'Environment variable not set ... Carry On...'
!20140512          RETURN
        ELSE IF ( IOST .EQ. 0 ) THEN
          MAPNAME = PROMPTMFILE( 'Enter name for source region ncf file',
     &       FSREAD3, 'SA_APPMAP', PNAME )
          !print*,'in MAP_FRAC, MAPNAME is ',MAPNAME
        
          ! Domain decomposition
          CALL SUBHFILE( MAPNAME, GXOFF, GYOFF, 
     &       STRTCOL, ENDCOL, STRTROW, ENDROW )
          NRGNS = NVARS3D
        END IF


        ALLOCATE ( BUFF2( NCOLS, NROWS ), STAT = IOST )
        MAPFRAC = 0.0
        YES_INLN = .FALSE.
        DO ITAG = 1, NTAGS
          LENRGN = LEN_TRIM( TAGRGN( ITAG ) )
          IF ( TAGRGN( ITAG )( 1:10 ) .EQ. 'EVERYWHERE' ) THEN
            DO R = 1, MY_NROWS
              DO C = 1, MY_NCOLS
                MAPFRAC( C,R,ITAG ) = 1.0
              ENDDO !C
            ENDDO !R
          ELSEIF ( TAGRGN( ITAG )( 1:6 ) .EQ. 'INLINE' ) THEN
            DO R = 1, MY_NROWS
              DO C = 1, MY_NCOLS
                MAPFRAC( C,R,ITAG ) = 1.0
              ENDDO !C
            ENDDO !R
            !20130731
c           IF ( .NOT. YES_INLN ) THEN
c             YES_INLN = .TRUE.
c             ALLOCATE( SAIDVALU( NTAGS, NINLN ) )
c             SAIDVALU = -666.6
c           ENDIF ! tagrgn = inline
            CALL COUNTCOMMAS( TAGRGN( ITAG )( 1:LENRGN ), CNTCMA )
            ALLOCATE( LOCATCMA( CNTCMA ) )
            CALL LOCATECOMMAS( TAGRGN( ITAG )( 1:LENRGN ), CNTCMA, LOCATCMA )
            DO JCNT = 1, CNTCMA
              IF ( CNTCMA .EQ. 1 .OR. JCNT .EQ. CNTCMA ) THEN
!20140512       CVALU = TAGRGN( ITAG )( LOCATCMA(JCNT)+1:LOCATCMA(JCNT)+6 )
                CVALU = TAGRGN( ITAG )( LOCATCMA(JCNT)+1:LENRGN )
              ELSE
                CVALU = TAGRGN( ITAG )( LOCATCMA(JCNT)+1:LOCATCMA(JCNT+1)-1 )
              ENDIF ! cntcma =1 or jcnt comes to cntcma
              READ( CVALU, '(F6.0)' ) VALU
c             SAIDVALU( ITAG,JCNT ) = VALU
            ENDDO ! jcnt            
            DEALLOCATE( LOCATCMA )

          ELSE
            DO IRGN = 1, NRGNS
              YESRGN = INDEX( TAGRGN(ITAG)(1:LEN_TRIM(TAGRGN(ITAG))),
     &           VNAME3D(IRGN)(1:LEN_TRIM(VNAME3D(IRGN))), .FALSE. )
              LENVNAM = LEN_TRIM(VNAME3D(IRGN))
              YES_XTRAC = .FALSE.
              IF ( YESRGN .EQ. 1 .AND.
     &  ( TAGRGN(ITAG)(YESRGN+LENVNAM:YESRGN+LENVNAM) .EQ. ',' .OR.
     &    TAGRGN(ITAG)(YESRGN+LENVNAM:YESRGN+LENVNAM) .EQ. ' ' .OR.
     &    YESRGN-1+LENVNAM .EQ. LEN_TRIM(TAGRGN(ITAG)) ) ) THEN
                YES_XTRAC = .TRUE.
              ELSEIF ( YESRGN .GT. 0 .AND. 
     &  ( TAGRGN(ITAG)(YESRGN-1:YESRGN-1) .EQ. ',' .OR. 
     &    TAGRGN(ITAG)(YESRGN-1:YESRGN-1) .EQ. ' ' )
     &  .AND. 
     &  ( TAGRGN(ITAG)(YESRGN+LENVNAM:YESRGN+LENVNAM) .EQ. ',' .OR.
     &    TAGRGN(ITAG)(YESRGN+LENVNAM:YESRGN+LENVNAM) .EQ. ' ' .OR.   
     &    YESRGN-1+LENVNAM .EQ. LEN_TRIM(TAGRGN(ITAG)) ) ) THEN
                YES_XTRAC = .TRUE.
              ENDIF !yesrgn = ?
              IF ( YES_XTRAC ) THEN
                IF ( .NOT. XTRACT3( MAPNAME, VNAME3D( IRGN ),
     &            1,1, STRTROW, ENDROW, STRTCOL, ENDCOL,
     &            SDATE3D, STIME3D, BUFF2 ) ) THEN
                   XMSG = 'Could not read ' //
     &                VNAME3D( IRGN )( 1:LEN_TRIM( VNAME3D( IRGN ) ) ) //
     &                ' from ' // MAPNAME
                   CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
                ENDIF
                DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                    MAPFRAC( C,R,ITAG ) = MAPFRAC( C,R,ITAG ) + BUFF2( C,R )
                  ENDDO !C
                ENDDO !R
              ENDIF !yes_xtrac
            ENDDO ! IRGN
          ENDIF ! TAGRGN is defined 
        ENDDO ! ITAG


        END SUBROUTINE MAP_FRAC

c===============================================================

        SUBROUTINE GET_NSPC_SA ()

C20140428  Determine number of ISAM species
C         Called by sa_dim.F
C

        USE GRID_CONF    ! just for mype 20140327
        USE UTILIO_DEFN  ! 20130627

        IMPLICIT NONE

C...External below
!0710   INTEGER, EXTERNAL :: TRIM_LEN
c...external above
      
        INTEGER ITAG
        INTEGER J

        LOGICAL LBACK
        INTEGER BGN_SP, BGN_NTRATE, BGN_VOC

        !20130627
        INTEGER IOST

c----------------------------------------------------------

        L_EC = .FALSE. 
        L_OC = .FALSE. 
        L_SFATE = .FALSE. 
        L_NTRATE = .FALSE. 
        L_NH4 = .FALSE. 
        L_PM25 = .FALSE.   ! 0705
        L_CO = .FALSE.   !0705
        L_OZONE = .FALSE.   !0705
        L_VOC = .FALSE.   !20131209

        NSPC_SA = 0
c       DO ITAG = 1, NTAG_SA-3
        LBACK = .FALSE. 
        BGN_SP = INDEX( TAGCLASSES,'EC',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          NSPC_SA = NSPC_SA + 2  ! AECJ, AECI
          L_EC = .TRUE.
        ENDIF

        LBACK = .FALSE.
        BGN_SP = INDEX( TAGCLASSES,'OC',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          IF( INDEX(MECHNAME, 'CB6R3_AE6_AQ') .GT. 0 ) THEN
            NSPC_SA = NSPC_SA + 13  ! ALVPO1I,ALVPO1J,ASVPO1I,ASVPO1J,ASVPO2I,ASVPO2J,ASVPO3J,AIVPO1J
                                    ! VLVPO1,VSVPO1,VSVPO2,VSVO3,VIVPO1
          ELSE
            NSPC_SA = NSPC_SA + 4  ! APOCJ, APOCI, APNCOMJ, APNCOMI  20120711
          ENDIF
          L_OC = .TRUE.
        ENDIF

        LBACK = .FALSE.
        BGN_SP = INDEX( TAGCLASSES,'SULFATE',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          NSPC_SA = NSPC_SA + 5  ! ASO4J, ASO4I, SO2, SULF(Fb21), SULRXN(20130529)
          L_SFATE = .TRUE.
        ENDIF

        LBACK = .FALSE.
        BGN_SP = INDEX( TAGCLASSES,'NITRATE',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          IF( INDEX(MECHNAME, 'CB6R3') .GT. 0) then
            NSPC_SA = NSPC_SA + 6  ! ANO3J, ANO3I, HNO3, NTR1, NTR2, INTR
          ELSE
            NSPC_SA = NSPC_SA + 4  ! ANO3J, ANO3I, HNO3, NTR(cb05) or RNO3(saprc99)
          ENDIF
          NSPC_SA = NSPC_SA + 8  ! NO, NO2, NO3, HONO, N2O5, PNA, PAN, PANX
          L_NTRATE = .TRUE.
        ENDIF

        LBACK = .FALSE.
        BGN_SP = INDEX( TAGCLASSES,'VOC',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          IF( INDEX(MECHNAME, 'CB6R3') .GT. 0) then
            NSPC_SA = NSPC_SA + 21 !see OZ_DEFN.F for lists of vocs.
          ELSE
            NSPC_SA = NSPC_SA + 14 !20131209 ald2,aldx,eth,etha,etoh,form,iole,isop,meoh,ole,par,terp,tol,xyl
          ENDIF
          L_VOC = .TRUE.
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

        LBACK = .FALSE.
        BGN_SP = INDEX( TAGCLASSES,'OZONE',LBACK )
        IF ( BGN_SP .NE. 0 ) THEN
          NSPC_SA = NSPC_SA + 3  ! O3, O, HO2
          L_OZONE = .TRUE.
          IF ( .NOT. L_NTRATE ) THEN 
            IF( INDEX(MECHNAME, 'CB6R3') .GT. 0) then
              NSPC_SA = NSPC_SA + 6  ! ANO3J, ANO3I, HNO3, NTR1, NTR2,INTR
            ELSE
              NSPC_SA = NSPC_SA + 4  ! ANO3J, ANO3I, HNO3, NTR(cb05) or RNO3(saprc99)
            ENDIF
            NSPC_SA = NSPC_SA + 8  ! NO, NO2, NO3, HONO, N2O5, PNA, PAN, PANX
            L_NTRATE = .TRUE.
          ENDIF
          IF ( .NOT. L_VOC ) THEN 
            IF( INDEX(MECHNAME, 'CB6R3') .GT. 0) then
              NSPC_SA = NSPC_SA + 21 !see OZ_DEFN.F for lists of vocs.
            ELSE
              NSPC_SA = NSPC_SA + 14 !20131209 ald2,aldx,eth,etha,etoh,form,iole,isop,meoh,ole,par,terp,tol,xyl
            ENDIF
            L_VOC = .TRUE.
          ENDIF
        END IF
 
        END SUBROUTINE GET_NSPC_SA

c===============================================================

        SUBROUTINE GET_SPC_INDEX ()

C20140428  Map CGRID species index to ISAM tracer species index
C         Called by driver.F
C

        USE CGRID_SPCS       ! 20120615
        USE HGRD_DEFN        ! 20120710 just for print out from single processor
        USE UTILIO_DEFN      ! 20120725 for external functions such as INDEX1

        IMPLICIT NONE

      
        INTEGER J_SPC, N, ITAG, I2, ISUM
        INTEGER N_OZN     ! index of ozone in gc_spc list
        INTEGER ALLOCSTAT

        CHARACTER( 16 ), ALLOCATABLE, SAVE :: CMAQ_ADV_NAMES( : )  
        INTEGER FRST, LAST, CMAQ_ADV

        LOGICAL FOUND
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

        IF (  L_EC ) THEN
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'AECJ'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'AECI'   
        ENDIF

        IF ( L_OC ) THEN
          IF( INDEX(MECHNAME, 'CB6R3_AE6_AQ') .GT. 0 ) THEN
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

        IF ( L_NTRATE ) THEN
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ANO3J'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'ANO3I'    
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'HNO3'

          J_SPC = J_SPC + 1
          IF( INDEX(MECHNAME, 'CB6R3') .GT. 0) then
            SPC_NAME( J_SPC,: ) = 'NTR1' 
            J_SPC = J_SPC + 1  
            SPC_NAME( J_SPC,: ) = 'NTR2'   
            J_SPC = J_SPC + 1
            SPC_NAME( J_SPC,: ) = 'INTR'   
          ELSE
            SPC_NAME( J_SPC,: ) = 'NTR'   
          ENDIF

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
          SPC_NAME( J_SPC,: ) = 'PNA'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'PAN'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'PANX'
        ENDIF

        IF ( L_VOC ) THEN
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
          IF( INDEX(MECHNAME, 'CB6R3') .GT. 0) then
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
          ELSE
            SPC_NAME( J_SPC,: ) = 'XYL'
          ENDIF
        ENDIF 

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
          SPC_NAME( J_SPC,: ) = 'O'
          J_SPC = J_SPC + 1
          SPC_NAME( J_SPC,: ) = 'HO2'
        ENDIF


C...Check if the above adds up to number of tagging species
        IF ( J_SPC .NE. NSPC_SA ) THEN
          CALL M3EXIT( 'GET_SPC_INDEX', 0, 0, 'Fatal Error Encountered', XSTAT2 )
        ENDIF

C...Initialize species index
        SPC_INDEX = -1

C...Obtain ozone index from GC_SPC list
        N_OZN = INDEX1( 'O3', N_GC_SPC, GC_SPC )

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
              IF ( SPC_NAME( J_SPC,ICONTAG ) .EQ. GC_SPC( N ) ) THEN  !20131209 14 voc species covered
                SPC_INDEX( J_SPC,1 ) = 1
                SPC_INDEX( J_SPC,2 ) = N
                MAP_CGRtoSA( N ) = J_SPC
              ELSEIF ( SPC_NAME( J_SPC,ICONTAG )(1:3) .EQ. 'O3A' ) THEN !1 ozone
                SPC_INDEX( J_SPC,1 ) = 1
                SPC_INDEX( J_SPC,2 ) = N_OZN
                MAP_CGRtoSA( N ) = J_SPC
              ELSEIF ( SPC_NAME( J_SPC,ICONTAG )(1:3) .EQ. 'O3N' .OR.
     &                 SPC_NAME( J_SPC,ICONTAG )(1:3) .EQ. 'O3V' ) THEN !2-regime ozone
                SPC_INDEX( J_SPC,1 ) = -50
                SPC_INDEX( J_SPC,2 ) = N_OZN
                MAP_CGRtoSA( N ) = J_SPC
              ENDIF ! spc_name and gc_spc match
            ENDDO

          ELSEIF ( N .GE. AE_STRT .AND. N .LT. AE_FINI ) THEN

            DO J_SPC = 1, NSPC_SA
              IF ( SPC_NAME( J_SPC,ICONTAG ) .EQ. AE_SPC( N-AE_STRT+1 ) ) THEN
                SPC_INDEX( J_SPC,1 ) = 1
                SPC_INDEX( J_SPC,2 ) = N
                MAP_CGRtoSA( N ) = J_SPC
              ENDIF ! spc_name and ae_spc match
            ENDDO

          ELSEIF ( N .GE. NR_STRT .AND. N .LE. NR_FINI ) THEN

            DO J_SPC = 1, NSPC_SA
              IF ( SPC_NAME( J_SPC,ICONTAG ) .EQ. NR_SPC( N-NR_STRT+1 ) ) THEN
                SPC_INDEX( J_SPC,1 ) = 1
                SPC_INDEX( J_SPC,2 ) = N
                MAP_CGRtoSA( N ) = J_SPC
              ENDIF
            ENDDO

          ENDIF
        ENDDO ! number of tagging species

C...Map SA species for Advection
        ALLOCATE( MAP_ADVtoSA ( N_SPCTAG ) )
 
        CMAQ_ADV = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV
        ALLOCATE( CMAQ_ADV_NAMES( CMAQ_ADV ) )

        FRST = 0
        LAST = 0
        IF ( N_GC_TRNS .NE. 0 ) THEN 
           FRST = 1
           LAST = N_GC_TRNS
           CMAQ_ADV_NAMES( FRST:LAST ) = GC_TRNS
        ENDIF

        IF ( N_AE_TRNS .NE. 0 ) THEN
           FRST = LAST + 1
           LAST = LAST + N_AE_TRNS
           CMAQ_ADV_NAMES( FRST:LAST ) = AE_TRNS
        END IF

        IF ( N_NR_TRNS .NE. 0 ) THEN
           FRST = LAST + 1
           LAST = LAST + N_NR_TRNS
           CMAQ_ADV_NAMES( FRST:LAST ) = NR_TRNS
        END IF

        IF ( N_TR_ADV .NE. 0 ) THEN
           FRST = LAST + 1
           LAST = LAST + N_NR_TRNS
           CMAQ_ADV_NAMES( FRST:LAST ) = TR_ADV
        END IF

        MAP_ADVtoSA = 0
        DO N = 1, N_SPCTAG
           MAP_ADVtoSA(N) = INDEX1( SPC_NAME(N,1), CMAQ_ADV, CMAQ_ADV_NAMES )
        END DO

c...check assigned spc_name and spc_index
c         if ( MYPE .eq. 0 ) then
c           DO J_SPC = 1, NSPC_SA
c             !print *, J_SPC, ICONTAG 
c             !print *, SPC_NAME(J_SPC,ICONTAG)
c             DO I2 = 1,20
c               !print *, SPC_INDEX(J_SPC,I2)
c             ENDDO
c           ENDDO
c         endif

        END SUBROUTINE GET_SPC_INDEX

      END MODULE SA_DEFN
