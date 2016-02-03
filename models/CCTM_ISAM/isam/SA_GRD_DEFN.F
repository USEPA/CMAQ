      MODULE SA_GRD_DEFN
C---------------------------------------------------------------------
C20140428  Has subroutines SA_GRD_EGAS and SA_GRD_E_PM
C
C---------------------------------------------------------------------

      USE SA_DEFN       ! Mc06

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE :: SA_VDEMIS_GRD( :,:,:,:,: ) ! tags for gas
      REAL, ALLOCATABLE, SAVE :: SA_PMEMIS_GRD( :,:,:,:,: ) ! tags for PM 
      REAL, ALLOCATABLE, SAVE :: SA_SULF_GRD( :,:,:,: ) ! tags for sulf
      REAL, ALLOCATABLE, SAVE :: SA_SULF_NEW( :,:,:,: ) ! tags for sulf converted from SO2
      REAL, ALLOCATABLE, SAVE :: SA_SULF_OLD( :,:,:,: ) ! backup of SA_SULF_OLD
      REAL, ALLOCATABLE, SAVE :: VOC_E_GRD( :,:,:,:,: ) ! 20120919 for gridded VOC emis
      INTEGER  TRUDATE  ! sdate3d of input file

C Extract and save aerospc%name(2), aerospc%emis, and associated ae_emis_fac 20130702
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: ANAMI( : )
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: ANAMJ( : )
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: PMNAM( : )
      REAL           , ALLOCATABLE, SAVE :: PMEFAC( : )

      CONTAINS

        SUBROUTINE SA_GRD_EGAS( JDATE, JTIME, TSTEP )

C---------------------------------------------------------------------
C20140428 Reads *GRIDDED* gaseous emissions from sectors
C       
C         Called by GET_EMIS in EMIS_DEFN.F  
C---------------------------------------------------------------------
        
        USE GRID_CONF
!       USE PMEM_DEFN           ! 20120713 not existent in cmaq50+; use aero_data instead
        USE CGRID_SPCS          ! 20120712 for ae_spc, ae_emis, etc
        USE AERO_DATA           ! 20120713
        USE UTILIO_DEFN         ! 20120712 replaces ioparms, iofdesc, iodecl
        USE SUBST_MODULES
        USE OZVOC_DEFN          ! 20120919 for VOC

        IMPLICIT NONE

C Includes:
        INCLUDE SUBST_CONST     ! physical and mathematical constants
        INCLUDE SUBST_FILES_ID  ! file name paramters
!20130702        INCLUDE SUBST_SALIST    ! List of SA inputs

C Arguments:
        INTEGER               :: JDATE, JTIME
        INTEGER, INTENT( IN ) :: TSTEP( 3 )

C Other variables:

        CHARACTER( 16 ) :: PNAME = 'SA_GRD_EGAS'  ! procedure name

        INTEGER          IOS        ! i/o and allocate memory status
        INTEGER          L, S, V    ! counters
        INTEGER          C, R, I, J, N

        INTEGER  JSPCTAG, ITAG, JSPC
        CHARACTER( 16 ) DUMMY
!20130702        CHARACTER( 16 ), ALLOCATABLE :: SGFILE( : )
        CHARACTER( 16 ) SGFILE
        INTEGER  IBGN
        CHARACTER( 120 ) :: XMSG = ' '

C Domain decomposition-related
        INTEGER          GXOFF, GYOFF         ! global origin offset from file
        INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
        INTEGER, SAVE :: LOGDEV
        LOGICAL, SAVE :: FIRSTGAS = .TRUE.

C For each gridded file....
        INTEGER              ELAYS
        REAL, ALLOCATABLE :: BUFF( :,:,: )


C NOX related....
        CHARACTER( 16 )  NAM_NOX
        INTEGER          JNOX

C SULF related....
        LOGICAL   LSULF
        LOGICAL,         SAVE :: FIRSTIME = .TRUE.

C VOC related.....20120919
        INTEGER   JV

C Multi-sectors-for-single-tag....20130702
        INTEGER   ISGSTK

C length of spc_name....20130702
        INTEGER   LENSPCNAME

C indices for gas emission, nr emission...20130702
        INTEGER         VGC, VNR
        CHARACTER( 16 ) NAMEMIS    ! emission species name
        REAL            FAC_EM     ! emission factor

C Diagnostic ...20130702
        INTEGER         CNTDBLCNT

C=============================================================================


Ckrt...Tagged gas emis
        IF ( .NOT. ALLOCATED( SA_VDEMIS_GRD ) ) 
     &    ALLOCATE( SA_VDEMIS_GRD( MY_NCOLS, MY_NROWS, NLAYS, NSPC_SA, NTAG_SA ) )
        SA_VDEMIS_GRD = 0.0

Ckrt...Aggregating VOC emis 20120919
        IF ( .NOT. ALLOCATED( VOC_E_GRD ) )
     &    ALLOCATE( VOC_E_GRD( MY_NCOLS, MY_NROWS, NLAYS, N_EVOC, NTAG_SA ) )
        VOC_E_GRD = 0.0

Ckrt...Tagged SULF emis
        LSULF = .FALSE.
        JSPCTAG = 1
        XTR_SULF: DO
          IF ( JSPCTAG .GT. N_SPCTAG ) EXIT
          IBGN = INDEX( VNAM_SPCTAG(JSPCTAG),'_', .FALSE. )
          IF ( VNAM_SPCTAG(JSPCTAG)(1:IBGN-2) .EQ. 'ASO4' ) THEN
            LSULF = .TRUE.
            EXIT
          ENDIF ! begin with aso4 ?
          JSPCTAG = JSPCTAG + 1
        ENDDO XTR_SULF

!0919   IF ( LSULF ) THEN
          IF ( .NOT. ALLOCATED( SA_SULF_GRD ) ) 
     &      ALLOCATE( SA_SULF_GRD( MY_NCOLS, MY_NROWS, NLAYS, NTAG_SA ) )
          SA_SULF_GRD = 0.0
          IF ( .NOT. ALLOCATED( SA_SULF_NEW ) ) 
     &      ALLOCATE( SA_SULF_NEW( MY_NCOLS, MY_NROWS, NLAYS, NTAG_SA ) )
          IF ( .NOT. ALLOCATED( SA_SULF_OLD ) ) 
     &      ALLOCATE( SA_SULF_OLD( MY_NCOLS, MY_NROWS, NLAYS, NTAG_SA ) )
          IF ( FIRSTIME ) THEN 
            FIRSTIME = .FALSE.
            SA_SULF_NEW = 0.0
            SA_SULF_OLD = 0.0
          ENDIF ! initialize sulf array just once
!0919   ELSE
        IF ( .NOT. LSULF ) THEN
          IF ( YES_PRINT ) THEN
            IF ( MYPE .EQ. 0 ) print*,"No sulfur in tags !"
          ENDIF !yes_print
        ENDIF ! lsulf

CKRT THE BIG LOOP BELOW......................
        LOOP_TAG: DO ITAG = 1, NTAG_SA-3

        !Mult-sectors-for-single-tags 20130307
        LOOP_MULTISECTORS: DO ISGSTK = 1, N_SGSTACKS

          IF ( YESSTK( ITAG, ISGSTK ) ) THEN   !sectors associated with the tag

!           CNTDBLCNT = CNTDBLCNT + 1

           LSULF = .FALSE.
           WRITE( DUMMY, '( "SG",I2.2 )' ) ISGSTK
           SGFILE =  PROMPTMFILE( 'SGFILE takes SG??...',FSREAD3,
     &                   DUMMY, PNAME )
           IF ( .NOT. DESC3( SGFILE ) ) THEN
              XMSG = 'Could not get description of file "' //
     &               SGFILE( 1:LEN_TRIM( SGFILE ) ) // '"'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
           END IF
           IF ( NCOLS3D .EQ. 1 ) THEN
             CYCLE
           ELSE ! This is where the ingestion takes place
             ELAYS = NLAYS3D
             TRUDATE = SDATE3D
             IF ( FIRSTGAS ) THEN ! decompose the domain
               FIRSTGAS = .FALSE.
               LOGDEV = INIT3()
               CALL SUBHFILE ( SGFILE, GXOFF, GYOFF, STARTCOL, ENDCOL, STARTROW, ENDROW )
             ENDIF ! firstime?
             IF ( ALLOCATED ( BUFF ) ) DEALLOCATE( BUFF )
             ALLOCATE( BUFF( MY_NCOLS, MY_NROWS, ELAYS ) )
Ckrt ...model gas species and composite gas species 20130319
             DO JSPC = 1, NSPC_SA

               LENSPCNAME = LEN_TRIM(SPC_NAME(JSPC,ITAG))

               IF ( LENSPCNAME .GT. 0 ) THEN  ! make sure no empty entry for spc_name 20130328


               !Deal with SULF first 20130319
               IF ( SPC_NAME(JSPC,ITAG)(1:LENSPCNAME)
     &             .EQ. 'ASO4J' .AND. .NOT. LSULF ) THEN
                 LSULF = .TRUE.
                 BUFF = 0.0
                 IF ( .NOT. INTERPX( SGFILE, 'SULF', PNAME,
     &             STARTCOL,ENDCOL, STARTROW,ENDROW, 1,ELAYS,
     &             TRUDATE, JTIME, BUFF ) ) THEN
!KRT &            JDATE, JTIME, BUFF ) ) THEN ! screwed up when model date-time stamp differs that of input file
                   XMSG = 'Could not read SULF '
     &              // ' from ' // SGFILE
                   CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
                 ELSE
                   DO L = 1, ELAYS
                     DO R = 1, MY_NROWS
                       DO C = 1, MY_NCOLS
                         SA_SULF_GRD( C,R,L,ITAG ) = SA_SULF_GRD( C,R,L,ITAG ) + BUFF( C,R,L )
                       ENDDO ! C
                     ENDDO ! R
                   ENDDO ! L
                 END IF ! SULF interpx-ed ?
               ENDIF ! begin with aso4 and never extracted SULF for current tag ?

               IF ( SPC_INDEX( JSPC, 1 ) .EQ. 1 ) THEN ! model species

                 V = 0   !hope this helps!  20130402
                 VGC = INDEX1( SPC_NAME(JSPC,ITAG)(1:LENSPCNAME),N_GC_EMIS, GC_EMIS )
                 VNR = INDEX1( SPC_NAME(JSPC,ITAG)(1:LENSPCNAME),N_NR_EMIS, NR_EMIS )
                 IF ( VGC .GT. 0 .AND.
     &       SPC_NAME(JSPC,ITAG)(1:LENSPCNAME) .NE. 'SULF' ) THEN ! tagged gas found
                   V = VGC
                   NAMEMIS = GC_EMIS( V )
                   FAC_EM = GC_EMIS_FAC( V )


                 ELSEIF ( VNR .GT. 0 ) THEN ! tagged nr found
                   V = VNR
                   NAMEMIS = NR_EMIS( V )
                   FAC_EM = NR_EMIS_FAC( V )


                 ENDIF
                 IF ( V .GT. 0 ) THEN
                   BUFF = 0.0
                   IF ( .NOT. INTERPX( SGFILE, NAMEMIS, PNAME,
     &                STARTCOL,ENDCOL, STARTROW,ENDROW, 1,ELAYS,
     &                TRUDATE, JTIME, BUFF ) ) THEN
                     XMSG = 'Could not read  '
     &                // NAMEMIS( 1:LEN_TRIM( NAMEMIS ) )
     &                // ' from ' // SGFILE
                     CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
                   ELSE
                     DO L = 1, ELAYS
                       DO R = 1, MY_NROWS
                         DO C = 1, MY_NCOLS
                           SA_VDEMIS_GRD( C,R,L,JSPC, ITAG ) =
     &                      SA_VDEMIS_GRD( C,R,L, JSPC, ITAG )
     &                      + BUFF( C,R,L ) * FAC_EM
                         ENDDO ! C
                       ENDDO ! R
                     ENDDO ! L
                   END IF ! gas data interpx-ed ?
                 ENDIF ! tagged gas or NR found in emis file ?

               ENDIF ! spc_index >= 1 ?

             ENDIF ! lenspcname > 0

             ENDDO ! jspc
           ENDIF ! ncols3d = 1 ?

          ENDIF ! sectors associated with the current tag

         ENDDO LOOP_MULTISECTORS

        ENDDO LOOP_TAG
Ckrt
   
        END SUBROUTINE SA_GRD_EGAS
C---------------------------------------------------------------------
        SUBROUTINE SA_GRD_E_PM( JDATE, JTIME, TSTEP )
        
C---------------------------------------------------------------------
C20140428 Reads *GRIDDED* PM emissions from sectors
C
C         Called by GET_EMIS in EMIS_DEFN.F  
C---------------------------------------------------------------------
        
        USE GRID_CONF
!       USE PMEM_DEFN           ! 20120713 not exist; use aero-data instead
        USE CGRID_SPCS          ! 20120712 for ae_spc, ae_emis, etc
        USE AERO_DATA           ! 20120713
        USE UTILIO_DEFN         ! 20120712 replaces ioparms, iofdesc, iodecl
        USE SUBST_MODULES

        IMPLICIT NONE

C Includes:
        INCLUDE SUBST_CONST     ! physical and mathematical constants
        INCLUDE SUBST_FILES_ID  ! file name paramters
!20130702        INCLUDE SUBST_SALIST    ! List of SA inputs

C Arguments:
        INTEGER               :: JDATE, JTIME
        INTEGER, INTENT( IN ) :: TSTEP( 3 )

C Other variables:

        CHARACTER( 16 ) :: PNAME = 'SA_GRD_E_PM'  ! procedure name

        INTEGER          IOS        ! i/o and allocate memory status
        INTEGER          L, S, V    ! counters
        INTEGER          C, R, I, J, N
        INTEGER          IDX        ! 20120725

        INTEGER  JSPCTAG, ITAG, JSPC
        CHARACTER( 16 ) DUMMY
!20130702        CHARACTER( 16 ), ALLOCATABLE :: SGFILE( : )
        CHARACTER( 16 ) SGFILE
        INTEGER  IBGN
        CHARACTER( 120 ) :: XMSG = ' '

C Domain decomposition-related
        INTEGER          GXOFF, GYOFF         ! global origin offset from file
        INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
        INTEGER, SAVE :: LOGDEV
        LOGICAL, SAVE :: FIRSTPM = .TRUE.

C For each gridded file....
        INTEGER              ELAYS
        REAL, ALLOCATABLE :: BUFF( :,:,: )

C Multi-sectors-for-single-tag...20130702
        INTEGER  ISGSTK
        LOGICAL        ,              SAVE :: FIRSTAEROSPC = .TRUE.

C diagnostic ....20130702
        REAL   SA_CHK



C=============================================================================

        ! 20130702
        IF ( FIRSTAEROSPC ) THEN

          FIRSTAEROSPC = .FALSE.
          ALLOCATE( ANAMI( N_EMIS_PM ) ) ; ANAMI = ' '
          ALLOCATE( ANAMJ( N_EMIS_PM ) ) ; ANAMJ = ' '
          ALLOCATE( PMNAM( N_EMIS_PM ) ) ; PMNAM = ' '
          ALLOCATE( PMEFAC( N_EMIS_PM ) ) ; PMEFAC = 0.0

          IF ( YES_PRINT ) THEN
            if ( MYPE .eq. 0 ) print*,'Available AE_EMIS:',AE_EMIS
          ENDIF !yes_print

          DO S = 1, N_EMIS_PM
            IDX = PMEM_MAP( S )
            ANAMI( S ) = AEROSPC( IDX )%NAME(1)
            ANAMJ( S ) = AEROSPC( IDX )%NAME(2)
            PMNAM( S ) = AEROSPC( IDX )%EMIS
            N = INDEX1( ANAMJ(S), N_AE_EMIS, AE_EMIS )
            IF ( N .GT. 0 ) PMEFAC( S ) = AE_EMIS_FAC( N )
            IF ( YES_PRINT ) THEN
              if ( MYPE .eq. 0 ) print*,'S =',S,'ANAMJ=',ANAMJ(S),
     & 'PMNAM=',PMNAM(S),'N=',N,'AE_EMIS=',AE_EMIS(N),
     & 'AE_EMIS_FAC=',AE_EMIS_FAC(N),'PMEFAC=',PMEFAC(S)
            ENDIF ! yes_print
          ENDDO ! S

        ENDIF ! firstaerospc

        IF ( .NOT. ALLOCATED( SA_PMEMIS_GRD ) ) 
     &    ALLOCATE( SA_PMEMIS_GRD( MY_NCOLS, MY_NROWS, NLAYS, N_EMIS_PM, NTAG_SA ) )
        SA_PMEMIS_GRD = 0.0

        IF ( YES_PRINT ) THEN 
          if ( MYPE .eq. 0 ) then
           print*,'Within subroutine SA_GRD_E_PM....'
           print*,'NTAG_SA:',NTAG_SA
          endif
        ENDIF ! yes_print

        LOOP_TAG4PM: DO ITAG = 1, NTAG_SA-3

        !Mult-sectors-for-single-tags 20130307
        LOOP_MULTISECTPM: DO ISGSTK = 1, N_SGSTACKS

          IF ( YESSTK( ITAG, ISGSTK ) ) THEN  ! sectors associated with the tag

           WRITE( DUMMY, '( "SG",I2.2 )' ) ISGSTK
           SGFILE =  PROMPTMFILE( 'SGFILE takes SG??...',FSREAD3,
     &                   DUMMY, PNAME )
           IF ( .NOT. DESC3( SGFILE ) ) THEN
              XMSG = 'Could not get description of file "' //
     &               SGFILE( 1:LEN_TRIM( SGFILE ) ) // '"'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
           ELSE
             IF ( YES_PRINT ) THEN
               IF ( MYPE .EQ. 0 ) THEN
                 print*,'ITAG = ',ITAG,', ISGSTK = ',ISGSTK
                 print*,'SG File read...',SGFILE( 1:LEN_TRIM( SGFILE ) )
               ENDIF ! mype0
             ENDIF !yes_print
           END IF

           IF ( NCOLS3D .EQ. 1 ) THEN
             CYCLE
           ELSE ! This is where the ingestion takes place

             ELAYS = NLAYS3D
             TRUDATE = SDATE3D
             IF ( YES_PRINT ) THEN
               if ( MYPE .eq. 0 ) print*,
     & 'No. layers in gridded PM emis file:',ELAYS
             ENDIF !yes_print

             IF ( FIRSTPM ) THEN ! decompose the domain
               FIRSTPM = .FALSE.
               LOGDEV = INIT3()
               CALL SUBHFILE ( SGFILE, GXOFF, GYOFF, STARTCOL, ENDCOL, STARTROW, ENDROW )
             ENDIF ! firstime?

             IF ( ALLOCATED ( BUFF ) ) DEALLOCATE( BUFF )
             ALLOCATE( BUFF( MY_NCOLS, MY_NROWS, ELAYS ) )

             LOOP_PMSPC : DO JSPC = 1, NSPC_SA

               V = INDEX1( SPC_NAME(JSPC,ITAG), N_EMIS_PM, ANAMJ )

               IF ( V .GT. 0 ) THEN
                 BUFF = 0.0
                 IF ( .NOT. INTERPX( SGFILE, PMNAM( V ), PNAME,
     &                STARTCOL,ENDCOL, STARTROW,ENDROW, 1,ELAYS,
     &                TRUDATE, JTIME, BUFF ) ) THEN
                   XMSG = 'Could not read  '
     &                // PMNAM( V )
     &                // ' from ' // SGFILE
                   CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
                 END IF ! gas data interpx-ed ?
                 DO L = 1, ELAYS
                   DO R = 1, MY_NROWS
                     DO C = 1, MY_NCOLS
                       SA_PMEMIS_GRD( C,R,L,V,ITAG ) =
     &    SA_PMEMIS_GRD( C,R,L,V,ITAG ) + BUFF( C,R,L ) * PMEFAC( V )
                     ENDDO ! C
                   ENDDO ! R
                 ENDDO ! L
               ENDIF ! pm emis matches with tagged pm

             ENDDO LOOP_PMSPC ! for tagged pm spc

           ENDIF ! ncols3d = 1 ?

          ENDIF ! sectors associated with the tag
        ENDDO LOOP_MULTISECTPM

        ENDDO LOOP_TAG4PM

Ckrt
   
        END SUBROUTINE SA_GRD_E_PM
C---------------------------------------------------------------------
      END MODULE SA_GRD_DEFN
