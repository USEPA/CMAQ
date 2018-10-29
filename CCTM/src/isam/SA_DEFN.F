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
!

      USE RXNS_DATA, ONLY : MECHNAME !Get Chemical Mechanism Name

      IMPLICIT NONE

      SAVE


      PUBLIC

c...Tagging species, regular or combined
      CHARACTER( 16 ), ALLOCATABLE :: SPC_NAME( :,: )
      INTEGER, ALLOCATABLE, SAVE   :: SPC_INDEX( :,: )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_CGRtoSA( : )
      INTEGER, ALLOCATABLE, SAVE   :: MAP_AEROtoSA( :,: )
      LOGICAL, ALLOCATABLE, SAVE   :: IS_SPC_AEROSOL( :,: )
      LOGICAL, ALLOCATABLE, SAVE   :: TRANSPORT_SPC ( : )

c...4th and 5th dimensions of ISAM array
      INTEGER ::     NSPC_SA
      INTEGER ::     NTAG_SA
      INTEGER ::     NSPC_TRANSPORT

c...Define Allocatable Apportionment Arrays
      !REAL, ALLOCATABLE :: ISAMB4( :,:,:,:,: )
      !REAL, ALLOCATABLE :: ISAM  ( :,:,:,:,: )
      !REAL, ALLOCATABLE :: CBFOR ( :,:,:,: )
      REAL, POINTER :: ISAMB4( :,:,:,:,: )
      REAL, POINTER :: ISAM  ( :,:,:,:,: )
      REAL, POINTER :: CBFOR ( :,:,:,: )
      REAL, ALLOCATABLE :: AISAM ( :,:,:,:,: )
      REAL, ALLOCATABLE :: TOT_SADEP ( :,:,:,: )
      REAL, ALLOCATABLE :: CONV_SADEP ( :,:,:,: )
      REAL, ALLOCATABLE :: MAPFRAC( :,:,: )
      REAL, ALLOCATABLE :: MFRC_P( :,:,: )
      REAL, ALLOCATABLE :: VOC5D ( :,:,:,:,: )
      REAL, ALLOCATABLE :: ISAMEM( :,:,:,:,: )

c...Variables regarding the tag list
      CHARACTER( 16 ), ALLOCATABLE :: TAGNAME( : )
      CHARACTER( 40 ), ALLOCATABLE :: TAGCLASSES( : )
      CHARACTER( 40 ), ALLOCATABLE :: TAGRGN( : )

c...multi-sectors-for-a-single-tag
      INTEGER,   PARAMETER :: N_SGSTACKS = 40
      LOGICAL, ALLOCATABLE :: YESSTK( :,: )
c...stack group id counting
      REAL,    ALLOCATABLE :: SAIDVALU( :,: )

c...Logical values for tagging species
      LOGICAL, ALLOCATABLE :: L_EC( : )
      LOGICAL, ALLOCATABLE :: L_OC( : )
      LOGICAL, ALLOCATABLE :: L_SFATE( : )
      LOGICAL, ALLOCATABLE :: L_NTRATE( : )
      LOGICAL, ALLOCATABLE :: L_NH4( : )
      LOGICAL, ALLOCATABLE :: L_PM25( : )  !0705
      LOGICAL, ALLOCATABLE :: L_CO( : )    !0705
      LOGICAL, ALLOCATABLE :: L_OZONE( : ) !0705
      LOGICAL, ALLOCATABLE :: L_VOC( : )   !20131209


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

!20130627  2-regime ozone or 1-ozone
      LOGICAL :: YES_2REGIME
      INTEGER :: NUMOZRGM

!20130709 optional printout to log files
      LOGICAL :: YES_PRINT

!20130731  stack group id counting
      INTEGER :: NINLN

!20140321  Option to use Ox production-loss for ozone apportionment
      LOGICAL :: FLAGOXSA

!20140416 Option to renormalise ISAM
      LOGICAL :: YES_RENORM

      REAL, PRIVATE, ALLOCATABLE :: BUFF2( :,: )

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
        CHARACTER( 80 ) :: TXTLINE
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
        CHARACTER( 80 ) :: TXTLINE
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
        
        if ( MYPE .eq. 0 ) then
          PRINT*, 'SA_IO_LIST Sucessfully Opened'
          PRINT*, 'Start reading the list...'
        endif

        !20130731 For stack group id counting
        NINLN = 0

        DO ITAG = 1, NTAGS 
          READ ( INPUT_UNIT, '(A)' ) TXTLINE
          TAGNAME( ITAG ) = TXTLINE(18:LEN_TRIM( TXTLINE ) )

          READ ( INPUT_UNIT, '(A)' ) TXTLINE
          TAGCLASSES( ITAG ) = TXTLINE(18:LEN_TRIM( TXTLINE ) )

          READ ( INPUT_UNIT, '(A)' ) TXTLINE
          TAGRGN( ITAG ) = TXTLINE(18:LEN_TRIM( TXTLINE ) )

          !20130731 For stack group id counting
          IF ( TAGRGN( ITAG )(1:6) .EQ. 'INLINE' ) THEN
            CALL COUNTCOMMAS( TXTLINE, CNTCMA )
            IF ( CNTCMA .GT. NINLN ) NINLN = CNTCMA
          ENDIF !tagrgn = inline

          READ ( INPUT_UNIT, '(A)' ) TXTLINE
!20130702          SGFILE( ITAG ) = TXTLINE(18:LEN_TRIM( TXTLINE ) )
          DO ISGSTK = 1, N_SGSTACKS
            WRITE( FNAME,'("SG",I2.2)' ) ISGSTK
            LBACK = .FALSE.
            BGN_SG = INDEX( TXTLINE(18:LEN_TRIM(TXTLINE)),
     &                FNAME(1:LEN_TRIM(FNAME)), LBACK )

            IF ( MYPE .EQ. 0 ) THEN
                print*,'Currently txtline18end, itag, sg file:',TXTLINE(18:LEN_TRIM(TXTLINE)), ITAG, FNAME
            ENDIF
            IF ( BGN_SG .NE. 0 ) THEN
              YESSTK( ITAG, ISGSTK ) = .TRUE.
              print*,'After screening, itag, sg file:', ITAG, FNAME
            ENDIF
          ENDDO ! isgstk

          READ ( INPUT_UNIT, '(A)' ) TXTLINE
!20130702          STKFILE( ITAG ) = TXTLINE(18:LEN_TRIM( TXTLINE ) )

          READ ( INPUT_UNIT, '(A)' ) TXTLINE

          if ( mype .eq. 0 )
     &         PRINT *, ITAG, TAGNAME( ITAG ), TAGCLASSES( ITAG ), TAGRGN( ITAG )
        ENDDO ! ITAG

        if ( mype .eq. 0 ) then
          print *,'max # inline sources among all tags, NINLN =',NINLN
          !krt20130702 To acknowledge exactly which stack emissions are to be read for each tag....
          print *,'itag, list_of_sg_files_for_each_tag'
          do ITAG = 1, NTAGS
            do ISGSTK = 1, N_SGSTACKS
              !if ( YESSTK( ITAG, ISGSTK ) ) print*,ITAG, ISGSTK
              print *,ITAG, ISGSTK, YESSTK( ITAG, ISGSTK )
            enddo ! isgstk
          enddo ! itag
        endif ! mype0

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
          IF ( YES_PRINT ) THEN
            IF ( MYPE .EQ. 0 ) print*, 'Tagrgn is ' // TAGRGN( ITAG )( 1:LENRGN )
          ENDIF
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
            IF ( .NOT. YES_INLN ) THEN
              YES_INLN = .TRUE.
              ALLOCATE( SAIDVALU( NTAGS, NINLN ) )
              SAIDVALU = -666.6
            ENDIF ! tagrgn = inline
            CALL COUNTCOMMAS( TAGRGN( ITAG )( 1:LENRGN ), CNTCMA )
            IF ( YES_PRINT ) THEN
              if ( MYPE .EQ. 0 ) print*,
     & 'For ITAG=',ITAG,', # commas in TAGRGN is CNTCMA =',CNTCMA
            ENDIF ! yes_print
            ALLOCATE( LOCATCMA( CNTCMA ) )
            CALL LOCATECOMMAS( TAGRGN( ITAG )( 1:LENRGN ), CNTCMA, LOCATCMA )
            IF ( YES_PRINT ) THEN 
             if ( MYPE .EQ. 0 ) then
              print*,'TAGRGN = ',TAGRGN(ITAG)
              print*,'Positions of commas in TAGRGN:'
              write( *,* )( LOCATCMA(JCNT), JCNT = 1,CNTCMA )
             endif ! mype0
            ENDIF ! yes_print
            DO JCNT = 1, CNTCMA
              IF ( CNTCMA .EQ. 1 .OR. JCNT .EQ. CNTCMA ) THEN
!20140512       CVALU = TAGRGN( ITAG )( LOCATCMA(JCNT)+1:LOCATCMA(JCNT)+6 )
                CVALU = TAGRGN( ITAG )( LOCATCMA(JCNT)+1:LENRGN )
              ELSE
                CVALU = TAGRGN( ITAG )( LOCATCMA(JCNT)+1:LOCATCMA(JCNT+1)-1 )
              ENDIF ! cntcma =1 or jcnt comes to cntcma
              READ( CVALU, '(F6.0)' ) VALU
              SAIDVALU( ITAG,JCNT ) = VALU
              if ( YES_PRINT ) then
                if ( MYPE .EQ. 0 ) 
     & print*,'itag=',ITAG,', jcnt=',JCNT,
     & ', saidvalu=',SAIDVALU( ITAG,JCNT )
              endif ! yes_print
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
              IF ( YES_PRINT ) THEN
                if ( MYPE .EQ. 0 ) print*,'Regions extracted: ',TAGRGN(ITAG)
              ENDIF !yes_print
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
        DO ITAG = 1, NTAG_SA-3
            LBACK = .FALSE. 
            BGN_SP = INDEX( TAGCLASSES( ITAG ),'EC',LBACK )
            IF ( BGN_SP .NE. 0 ) THEN
              IF ( .NOT. ANY( L_EC )  ) THEN
                NSPC_SA = NSPC_SA + 2  ! AECJ, AECI
              ENDIF
              L_EC( ITAG ) = .TRUE.
            ENDIF

            LBACK = .FALSE.
            BGN_SP = INDEX( TAGCLASSES( ITAG ),'OC',LBACK )
            IF ( BGN_SP .NE. 0 ) THEN
              IF ( .NOT. ANY( L_OC ) ) THEN
                IF( INDEX(MECHNAME, 'CB6R3_AE6_AQ') .GT. 0 ) THEN
                NSPC_SA = NSPC_SA + 13  ! ALVPO1I,ALVPO1J,ASVPO1I,ASVPO1J,ASVPO2I,ASVPO2J,ASVPO3J,AIVPO1J
                                        ! VLVPO1,VSVPO1,VSVPO2,VSVO3,VIVPO1
                ELSE
                NSPC_SA = NSPC_SA + 4  ! APOCJ, APOCI, APNCOMJ, APNCOMI  20120711
                ENDIF
               ENDIF
              L_OC( ITAG ) = .TRUE.
            ENDIF

            LBACK = .FALSE.
            BGN_SP = INDEX( TAGCLASSES( ITAG ),'SULFATE',LBACK )
            IF ( BGN_SP .NE. 0 ) THEN
              IF ( .NOT. ANY( L_SFATE ) ) THEN
                NSPC_SA = NSPC_SA + 5  ! ASO4J, ASO4I, SO2, SULF(Fb21), SULRXN(20130529)
              ENDIF
              L_SFATE( ITAG ) = .TRUE.
            ENDIF

            LBACK = .FALSE.
            BGN_SP = INDEX( TAGCLASSES( ITAG ),'NITRATE',LBACK )
            IF ( BGN_SP .NE. 0 ) THEN
              IF ( .NOT. ANY( L_NTRATE ) ) THEN
                IF( INDEX(MECHNAME, 'CB6R3') .GT. 0) then
                  NSPC_SA = NSPC_SA + 6  ! ANO3J, ANO3I, HNO3, NTR1, NTR2, INTR
                ELSE
                  NSPC_SA = NSPC_SA + 4  ! ANO3J, ANO3I, HNO3, NTR(cb05) or RNO3(saprc99)
                ENDIF
                NSPC_SA = NSPC_SA + 8  ! NO, NO2, NO3, HONO, N2O5, PNA, PAN, PANX
              ENDIF
              L_NTRATE( ITAG ) = .TRUE.
            ENDIF

            LBACK = .FALSE.
            BGN_SP = INDEX( TAGCLASSES( ITAG ),'VOC',LBACK )
            IF ( BGN_SP .NE. 0 ) THEN
              IF ( .NOT. ANY( L_VOC ) ) THEN
                IF( INDEX(MECHNAME, 'CB6R3') .GT. 0) then
                  NSPC_SA = NSPC_SA + 21 !see OZ_DEFN.F for lists of vocs.
                ELSE
                  NSPC_SA = NSPC_SA + 14 !20131209 ald2,aldx,eth,etha,etoh,form,iole,isop,meoh,ole,par,terp,tol,xyl
                ENDIF
              ENDIF
              L_VOC( ITAG ) = .TRUE.
            ENDIF

            LBACK = .FALSE.
            BGN_SP = INDEX( TAGCLASSES( ITAG ),'AMMONIUM',LBACK )
            IF ( BGN_SP .NE. 0 ) THEN
              IF ( .NOT. ANY( L_NH4 ) ) THEN
                NSPC_SA = NSPC_SA + 3  ! ANH4J, ANH4I, NH3
              ENDIF
              L_NH4( ITAG ) = .TRUE.
            ENDIF

            LBACK = .FALSE.
            BGN_SP = INDEX( TAGCLASSES( ITAG ),'PM25_IONS',LBACK )
            IF ( BGN_SP .NE. 0 ) THEN
              IF ( .NOT. ANY( L_PM25 ) ) THEN
                NSPC_SA = NSPC_SA + 14 ! ACLiJ,ANAiJ,AMGJ,AKJ,ACAJ,AFEJ,AALJ,ASIJ,ATIJ,AMNJ,AOTHRiJ(0705) 
              ENDIF
              L_PM25( ITAG ) = .TRUE.
            ENDIF

            LBACK = .FALSE.
            BGN_SP = INDEX( TAGCLASSES( ITAG ),'CO',LBACK )
            IF ( BGN_SP .NE. 0 ) THEN
              IF ( .NOT. ANY( L_CO ) ) THEN
                NSPC_SA = NSPC_SA + 1  ! CO
              ENDIF
              L_CO( ITAG ) = .TRUE.
            ENDIF

            LBACK = .FALSE.
            BGN_SP = INDEX( TAGCLASSES( ITAG ),'OZONE',LBACK )
            IF ( BGN_SP .NE. 0 ) THEN
              IF ( .NOT. ANY( L_OZONE ) ) THEN
!20140410                YES_2REGIME = ENVYN( 'OZ_2REGIME',
!20140410     & 'yes=2 ozone regimes; no=1 ozone from Jacobian',
!20140410     & .TRUE., IOST )
!20140410                IF ( YES_2REGIME ) THEN
                  YES_2REGIME = .TRUE.
                  NUMOZRGM = 3
!20140410                ELSE
!20140410                  NUMOZRGM = 1
!20140410                ENDIF ! yes_2regime
                NSPC_SA = NSPC_SA + NUMOZRGM   ! O3V, O3N
              ENDIF  ! any(l_ozone)
              L_OZONE( ITAG ) = .TRUE.
              BGN_NTRATE = INDEX( TAGCLASSES( ITAG ),'NITRATE',LBACK )
              IF ( BGN_NTRATE .EQ. 0 ) THEN
                IF ( .NOT. ANY( L_NTRATE ) ) THEN
                  IF( INDEX(MECHNAME, 'CB6R3') .GT. 0) then
                    NSPC_SA = NSPC_SA + 6  ! ANO3J, ANO3I, HNO3, NTR1, NTR2, INTR
                  ELSE
                    NSPC_SA = NSPC_SA + 4  ! ANO3J, ANO3I, HNO3, NTR(cb05) or RNO3(saprc99)
                  ENDIF
                  NSPC_SA = NSPC_SA + 8  ! NO, NO2, NO3, HONO, N2O5, PNA, PAN, PANX
                ENDIF  ! no nitrate tracked so far
                L_NTRATE( ITAG ) = .TRUE.
              ENDIF ! bgn_ntrate nonzero
              BGN_VOC = INDEX( TAGCLASSES( ITAG ),'VOC',LBACK )
              IF ( BGN_VOC .EQ. 0 ) THEN
                IF ( .NOT. ANY( L_VOC ) ) THEN
                  IF( INDEX(MECHNAME, 'CB6R3') .GT. 0) then
                    NSPC_SA = NSPC_SA + 21 !see OZ_DEFN.F for lists of vocs.
                  ELSE
                    NSPC_SA = NSPC_SA + 14 !20131209 ald2,aldx,eth,etha,etoh,form,iole,isop,meoh,ole,par,terp,tol,xyl
                  ENDIF
                ENDIF  ! no VOC tracked so far
                L_VOC( ITAG ) = .TRUE.
              ENDIF ! bgn_ntrate nonzero
            ENDIF
        ENDDO ! number of tags

        IF ( YES_PRINT ) THEN
          IF ( MYPE .EQ. 0 )  print*,'NSPC_SA = ', NSPC_SA
        ENDIF

c...assign tags to bcon, others or icon for any tagging species

        DO ITAG = NTAG_SA-2, NTAG_SA
          IF ( ANY( L_EC ) ) L_EC( ITAG ) = .TRUE.
          IF ( ANY( L_OC ) ) L_OC( ITAG ) = .TRUE.
          IF ( ANY( L_SFATE ) ) L_SFATE( ITAG ) = .TRUE.
          IF ( ANY( L_NTRATE ) ) L_NTRATE( ITAG ) = .TRUE.
          IF ( ANY( L_NH4 ) ) L_NH4( ITAG ) = .TRUE.
          IF ( ANY( L_PM25 ) ) L_PM25( ITAG ) = .TRUE.
          IF ( ANY( L_CO ) ) L_CO( ITAG ) = .TRUE.
          IF ( ANY( L_OZONE ) ) L_OZONE( ITAG ) = .TRUE.
          IF ( ANY( L_VOC ) ) L_VOC( ITAG ) = .TRUE.
        ENDDO

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
        IF ( ANY( L_EC ) ) THEN
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_EC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'AECJ'
          ENDDO ! itag
          J_SPC = J_SPC + 1 
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_EC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'AECI'   
          ENDDO ! itag 
        ENDIF

        IF ( ANY( L_OC ) ) THEN
        IF( INDEX(MECHNAME, 'CB6R3_AE6_AQ') .GT. 0 ) THEN
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ALVPO1J'   ! 20120615
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ALVPO1I'   ! 20120615
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ASVPO1J' ! 20120711
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ASVPO1I' ! 20120711
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ASVPO2J'   ! 20120615
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ASVPO2I'   ! 20120615
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ASVPO3J' ! 20120711
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'AIVPO1J' ! 20120711
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'VLVPO1'   ! 20120615
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'VSVPO1'   ! 20120615
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'VSVPO2'   ! 20120615
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'VSVPO3' ! 20120711
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'VIVPO1' ! 20120711
          ENDDO ! itag
         !if ( MYPE .EQ. 0 ) print*, '110', J_SPC, ITAG, SPC_NAME(J_SPC,ITAG)

        ELSE
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'APOCJ'   ! 20120615
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'APOCI'   ! 20120615
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'APNCOMJ' ! 20120711
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_OC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'APNCOMI' ! 20120711
          ENDDO ! itag
        ENDIF
        ENDIF


        IF ( ANY( L_SFATE ) ) THEN
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_SFATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ASO4J'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_SFATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ASO4I'    
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_SFATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'SO2'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_SFATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'SULF'  !Fb21    
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20130529
            IF ( L_SFATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'SULRXN' !20130529
          ENDDO ! itag
        ENDIF

        IF ( ANY( L_NTRATE ) ) THEN
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ANO3J'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ANO3I'    
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'HNO3'
          ENDDO ! itag
          J_SPC = J_SPC + 1

          IF( INDEX(MECHNAME, 'CB6R3') .GT. 0) then
            DO ITAG = 1, NTAG_SA  ! 20120718
              IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'NTR1' !CB05   
            ENDDO ! itag
            J_SPC = J_SPC + 1  
            DO ITAG = 1, NTAG_SA  ! 20120718
              IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'NTR2' !CB05   
            ENDDO ! itag
            J_SPC = J_SPC + 1
            DO ITAG = 1, NTAG_SA  ! 20120718
              IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'INTR' !CB05   
            ENDDO ! itag
          ELSE
            DO ITAG = 1, NTAG_SA  ! 20120718
              IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'NTR' !CB05   
            ENDDO ! itag
          ENDIF

          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'NO'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'NO2'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'NO3'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'HONO'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'N2O5'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'PNA'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'PAN'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NTRATE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'PANX'
          ENDDO ! itag
        ENDIF

        IF ( ANY( L_VOC ) ) THEN
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ALD2'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ALDX'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ETH'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ETHA'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ETOH'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'FORM'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'IOLE'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ISOP'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'MEOH'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'OLE'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'PAR'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'TERP'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20131209
            IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'TOL'
          ENDDO ! itag
          J_SPC = J_SPC + 1

          IF( INDEX(MECHNAME, 'CB6R3') .GT. 0) then
            DO ITAG = 1, NTAG_SA  ! 20131209
              IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'XYLMN'
            ENDDO ! itag
            J_SPC = J_SPC + 1
            DO ITAG = 1, NTAG_SA  ! 20131209
              IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'NAPH'
            ENDDO ! itag
            J_SPC = J_SPC + 1
            DO ITAG = 1, NTAG_SA  ! 20131209
              IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ETHY'
            ENDDO ! itag
            J_SPC = J_SPC + 1
            DO ITAG = 1, NTAG_SA  ! 20131209
              IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'PRPA'
            ENDDO ! itag
            J_SPC = J_SPC + 1
            DO ITAG = 1, NTAG_SA  ! 20131209
              IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ACET'
            ENDDO ! itag
            J_SPC = J_SPC + 1
            DO ITAG = 1, NTAG_SA  ! 20131209
              IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'KET'
            ENDDO ! itag
            J_SPC = J_SPC + 1
            DO ITAG = 1, NTAG_SA  ! 20131209
              IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'GLY'
            ENDDO ! itag
            J_SPC = J_SPC + 1
            DO ITAG = 1, NTAG_SA  ! 20131209
              IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'BENZENE'
            ENDDO ! itag
          ELSE
            DO ITAG = 1, NTAG_SA  ! 20131209
              IF ( L_VOC( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'XYL'
            ENDDO ! itag
          ENDIF
        ENDIF ! any l_voc

        IF ( ANY( L_NH4 ) ) THEN
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NH4( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ANH4J'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NH4( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ANH4I'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_NH4( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'NH3'
          ENDDO ! itag
        ENDIF

        IF ( ANY( L_PM25 ) ) THEN  ! 0705
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ACLJ'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ACLI'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ANAJ'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ANAI' 
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'AMGJ'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'AKJ'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ACAJ'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'AFEJ'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'AALJ'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ASIJ'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'ATIJ'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'AMNJ'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'AOTHRJ'
          ENDDO ! itag
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_PM25( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'AOTHRI'
          ENDDO ! itag
        ENDIF

        IF ( ANY( L_CO ) ) THEN  ! 0706
          J_SPC = J_SPC + 1
          DO ITAG = 1, NTAG_SA  ! 20120718
            IF ( L_CO( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'CO'
          ENDDO ! itag
        ENDIF

        IF ( ANY( L_OZONE ) ) THEN  ! 0706
          IF ( NUMOZRGM .EQ. 1 ) THEN
            J_SPC = J_SPC + 1
            DO ITAG = 1, NTAG_SA  ! 20120718
              IF ( L_OZONE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'O3A'
            ENDDO ! itag
          ELSEIF ( NUMOZRGM .EQ. 3 ) THEN
!           J_SPC = J_SPC + 1
!           DO ITAG = 1, NTAG_SA  ! 20120718
!             IF ( L_OZONE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'O3V'
!           ENDDO ! itag
!           J_SPC = J_SPC + 1
!           DO ITAG = 1, NTAG_SA  ! 20120718
!             IF ( L_OZONE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'O3N'
!           ENDDO ! itag
            J_SPC = J_SPC + 1
            DO ITAG = 1, NTAG_SA  ! 20120718
              IF ( L_OZONE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'O3'
            ENDDO ! itag
            J_SPC = J_SPC + 1
            DO ITAG = 1, NTAG_SA  ! 20120718
              IF ( L_OZONE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'O'
            ENDDO ! itag
!           J_SPC = J_SPC + 1
!           DO ITAG = 1, NTAG_SA  ! 20120718
!             IF ( L_OZONE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'O1D'
!           ENDDO ! itag
!Do not add disturbs SULF tracking because large fraction of OH comes
!from untracked precursor so add SULF SULFRXN to the other tag
!           J_SPC = J_SPC + 1
!           DO ITAG = 1, NTAG_SA  ! 20120718
!             IF ( L_OZONE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'OH'
!           ENDDO ! itag
            J_SPC = J_SPC + 1
            DO ITAG = 1, NTAG_SA  ! 20120718
              IF ( L_OZONE( ITAG ) ) SPC_NAME( J_SPC,ITAG ) = 'HO2'
            ENDDO ! itag
          ENDIF ! numozrgm = 1 or 2 ? 20130627
        ENDIF


C...Check if the above adds up to number of tagging species
        IF ( J_SPC .NE. NSPC_SA ) THEN
          CALL M3EXIT( 'GET_SPC_INDEX', 0, 0, 'Fatal Error Encountered', XSTAT2 )
        ENDIF

C...Initialize species index
        SPC_INDEX = -1

C...Obtain ozone index from GC_SPC list
        N_OZN = INDEX1( 'O3', N_GC_SPC, GC_SPC )
        IF ( YES_PRINT ) THEN
          if ( MYPE .eq. 0 )
     &   print*,'N_OZN, GC_SPC(N_OZN):',N_OZN, GC_SPC(N_OZN)
        ENDIF !yes_print

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
         

c...check assigned spc_name and spc_index
!        IF ( YES_PRINT ) THEN
          if ( MYPE .eq. 0 ) then
            DO J_SPC = 1, NSPC_SA
              !print *, J_SPC, ICONTAG 
              !print *, SPC_NAME(J_SPC,ICONTAG)
              DO I2 = 1,20
                !print *, SPC_INDEX(J_SPC,I2)
              ENDDO
            ENDDO
          endif
!        ENDIF  ! yes_print

        END SUBROUTINE GET_SPC_INDEX

      END MODULE SA_DEFN
