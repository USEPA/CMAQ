      MODULE OZVOC_DEFN

C20140428  has subroutines VOC_INIT, OP_O3INDICAT
c
c

      IMPLICIT NONE

!KRT20120911  Populates VOC Emission species, and
!KRT20120911  corresponding carbon numbers, 
!KRT20120911  maximum incremental reactivity (MIR, CAMx530 mostly),
!KRT20120911  and RKI numbers in cmaq501

      INTEGER, PARAMETER    :: N_EVOC = 21
      REAL,         SAVE    :: KOH( N_EVOC )
      INTEGER,      SAVE    :: KOH2RK( N_EVOC )
      REAL,         SAVE    :: MIR( N_EVOC )
      CHARACTER( 16 ), SAVE :: NAM_VOC( N_EVOC )
      INTEGER,      SAVE    :: NAM2SP_INDX( N_EVOC )
      INTEGER,      SAVE    :: NCARBON( N_EVOC )

      REAL, ALLOCATABLE, SAVE :: WCH_VOC( :,: )
      REAL, ALLOCATABLE, SAVE :: NVOCTRACKED( : )
      REAL, ALLOCATABLE, SAVE :: MIR_WEITD( : )
      REAL, ALLOCATABLE, SAVE :: KOH_WEITD( : )

      REAL, ALLOCATABLE, SAVE :: VOC5D( :,:,:,:,: )
      REAL, ALLOCATABLE, SAVE :: VOCEMIS( :,:,:,:,: )

!KRT O3 regime indicators - Type 1
      REAL,            SAVE :: P_H2O2
      REAL,            SAVE :: P_ROOH
      REAL,            SAVE :: P_HNO3

!KRT optional output of regime indicator - Type 1
      LOGICAL,           SAVE :: YES_INDIC
      INTEGER,      PARAMETER :: N_INDIC = 10
      REAL, ALLOCATABLE, SAVE :: INDICVALU( :,:,:,: )
      CHARACTER(16)           :: VNAM_INDIC( N_INDIC )
      DATA  VNAM_INDIC / 'PEROX2HNO3', 'PH2O2HNO3','H2O2_HNO3', 
     &                   'H2O2_O3NO2','NOY', 'O3_NOX', 'O3_NOY',
     &                   'O3_NOZ', 'FORM_NOY', 'FORM_NO2' /
      CHARACTER(16)           :: UNITS_INDIC( N_INDIC )
      DATA  UNITS_INDIC / 'unitless','unitless','unitless','unitless',
     &                    'ppmV', 'unitless', 'unitless',
     &                    'unitless', 'unitless', 'unitless' /
      CHARACTER(80)           :: DESC_INDIC( N_INDIC )
      DATA  DESC_INDIC / 'Ratio: productions (H2O2+ROOH)/HNO3',
     &                   'Ratio: productions H2O2/HNO3',
     &                   'Ratio: Concs H2O2/HNO3',
     &                   'Ratio: Concs H2O2/(O3+NO2)',
     &                   'NOY = NOX +NO3+HNO3+N2O5+PAN',  !Fahey et al 1985: J Atmos Chem, 3(4), pp 435-468
     &                   'Ratio: Concs O3/NOX',
     &                   'Ratio: Concs O3/NOY',
     &                   'Ratio: Concs O3/NOZ; NOZ = NOY-NOX',
     &                   'Ratio: Concs HCHO/NOY',
     &                   'Ratio: Concs HCHO/NO2' /
      !transition values Zhang et al 2009: JGR, 114, D22305, doi:10.1029/2009JD011900
      REAL                    :: TRANSV( N_INDIC )
      !DATA  TRANSV / 0.5, 0.35, 0.04, 0.02, 15.0, 7.0, 20.0, 0.28, 1.0 /
      DATA  TRANSV / 0.5, 0.35, 0.35, 0.02, 0.02, 15.0, 7.0, 20.0, 0.28, 1.0 / !Zhang's preference about H2O2/Ox

!20140305 optional output of indicators' original species
      INTEGER,      PARAMETER :: N_INDORIG = 12
      REAL, ALLOCATABLE, SAVE :: INDORIG( :,:,:,: )
      CHARACTER( 16 )         :: VNAM_INDORIG( N_INDORIG )
      DATA VNAM_INDORIG / 'PH2O2', 'PROOH', 'PHNO3',
     &                    'H2O2', 'HNO3', 'O3',
     &                    'NO', 'NO2', 'NO3',
     &                    'N2O5', 'PAN', 'FORM' /
      CHARACTER( 16 )         :: UNITS_INDORIG( N_INDORIG )
      DATA UNITS_INDORIG / 'ppmV/s','ppmV/s','ppmV/s',
     &                     'ppmV','ppmV','ppmV',
     &                     'ppmV','ppmV','ppmV',
     &                     'ppmV','ppmV','ppmV'/


!KRT destruction of ozone 20120928
      REAL,            SAVE :: OZ_VOC
      REAL,            SAVE :: O3P_VOC
      REAL,            SAVE :: O1D_VOC
      REAL,            SAVE :: OZ_HOX      
      !20140307
      REAL,            SAVE :: TROYO3  ! negative number

      CONTAINS

C===================================================================
       SUBROUTINE VOC_INIT

C20140428  Initialize parameters of the 14 VOC emission species
c        Called by driver.F
c

       USE SA_DEFN
       USE GRID_CONF             ! 20120918 for my_ncols, my_nrows, nlays
       USE UTILIO_DEFN           ! 20120918 for xstat2 ?

       IMPLICIT NONE

       LOGICAL,     SAVE :: FIRSTIME = .TRUE.
       INTEGER  JV
       INTEGER  IOST
       CHARACTER( 96 )   :: XMSG = ' '

C-------------------------------------------------------------------
       IF ( .NOT. FIRSTIME ) THEN
         RETURN
       ELSE
         FIRSTIME = .FALSE.
!...Assign the VOC names 
         NAM_VOC(  1 ) = 'ALD2' ! ncarbon= 2, rki = 84,  mir =  4.454
         NAM_VOC(  2 ) = 'ALDX' ! ncarbon= 2, rki = 99,  mir =  6.807
         NAM_VOC(  3 ) = 'ETH'  ! ncarbon= 2, rki = 121, mir =  4.370
         NAM_VOC(  4 ) = 'ETHA' ! ncarbon= 2, rki = 171, mir =  0.109
         NAM_VOC(  5 ) = 'ETOH' ! ncarbon= 2, rki = 170, mir =  1.042
         NAM_VOC(  6 ) = 'FORM' ! ncarbon= 1, rki = 73,  mir =  4.496
         NAM_VOC(  7 ) = 'IOLE' ! ncarbon= 4, rki = 125, mir = 13.109
         NAM_VOC(  8 ) = 'ISOP' ! ncarbon= 5, rki = 158, mir = 11.555
         NAM_VOC(  9 ) = 'MEOH' ! ncarbon= 1, rki = 72,  mir =  0.361
         NAM_VOC( 10 ) = 'OLE'  ! ncarbon= 2, rki = 117, mir =  8.235
         NAM_VOC( 11 ) = 'PAR'  ! ncarbon= 1, rki = 112, mir =  0.319
         NAM_VOC( 12 ) = 'TERP' ! ncarbon= 10, rki = 166, mir =  8.824
         NAM_VOC( 13 ) = 'TOL'  ! ncarbon= 7, rki = 128, mir =  2.941
         NAM_VOC( 14 ) = 'XYLMN'  ! ncarbon= 8, rki = 154, mir = 14.790 
!... new added species for cb6
         NAM_VOC( 15 ) = 'NAPH'  ! ncarbon= 8, rki = 154, mir = 14.790,
                                 ! a 'dummy species' to XYLMN 
         NAM_VOC( 16 ) = 'ETHY'  ! ncarbon= 8, rki = 154, mir = 14.790 
         NAM_VOC( 17 ) = 'PRPA'  ! ncarbon= 8, rki = 154, mir = 14.790 
         NAM_VOC( 18 ) = 'ACET'  ! ncarbon= 8, rki = 154, mir = 14.790 
         NAM_VOC( 19 ) = 'KET'  ! ncarbon= 8, rki = 154, mir = 14.790 
         NAM_VOC( 20 ) = 'GLY'  ! ncarbon= 8, rki = 154, mir = 14.790 
         NAM_VOC( 21 ) = 'BENZENE'  ! ncarbon= 8, rki = 154, mir = 14.790 

!...Number of carbons for each VOC species
         NCARBON(  1 ) =  2
         NCARBON(  2 ) =  2 
         NCARBON(  3 ) =  2 
         NCARBON(  4 ) =  2 
         NCARBON(  5 ) =  2 
         NCARBON(  6 ) =  1 
         NCARBON(  7 ) =  4 
         NCARBON(  8 ) =  5 
         NCARBON(  9 ) =  1 
         NCARBON( 10 ) =  2 
         NCARBON( 11 ) =  1 
         NCARBON( 12 ) = 10 
         NCARBON( 13 ) =  7 
         NCARBON( 14 ) =  8  
         NCARBON( 15 ) =  8  
         NCARBON( 16 ) =  2 
         NCARBON( 17 ) =  3 
         NCARBON( 18 ) =  3 
         NCARBON( 19 ) =  1 
         NCARBON( 20 ) =  2 
         NCARBON( 21 ) =  6 

!...Map OH reactive constants from cb05 ebi ordering
         KOH2RK(  1 ) = 106
         KOH2RK(  2 ) = 110
         KOH2RK(  3 ) = 138
         KOH2RK(  4 ) = 125
         KOH2RK(  5 ) = 127
         KOH2RK(  6 ) =  96
         KOH2RK(  7 ) = 146
         KOH2RK(  8 ) = 149
         KOH2RK(  9 ) = 126
         KOH2RK( 10 ) = 142
         KOH2RK( 11 ) = 132
         KOH2RK( 12 ) = 172
         KOH2RK( 13 ) = 180
         KOH2RK( 14 ) = 185
         KOH2RK( 15 ) = 185
         KOH2RK( 16 ) = 136
         KOH2RK( 17 ) = 131
         KOH2RK( 18 ) = 130
!         KOH2RK( 19 ) =  
         KOH2RK( 20 ) = 116
         KOH2RK( 21 ) = 175

!...Assign MIR to each VOC species
         MIR(  1 ) =  4.454
         MIR(  2 ) =  6.807
         MIR(  3 ) =  4.370
         MIR(  4 ) =  0.109
         MIR(  5 ) =  1.042
         MIR(  6 ) =  4.496
         MIR(  7 ) = 13.109
         MIR(  8 ) = 11.555
         MIR(  9 ) =  0.361
         MIR( 10 ) =  8.235
         MIR( 11 ) =  0.319
         MIR( 12 ) =  8.824
         MIR( 13 ) =  2.941
         MIR( 14 ) = 14.790 
         MIR( 15 ) = 8.92 
         MIR( 16 ) =  4.496
         MIR( 17 ) = 0.541
         MIR( 18 ) = 0.564
         MIR( 19 ) =  4.86
         MIR( 20 ) =  15.10
         MIR( 21 ) =  1.39

!...Allocate whch_voc and nvoctracked
         IF ( .NOT. ALLOCATED( WCH_VOC ) ) THEN
           ALLOCATE ( WCH_VOC( N_EVOC, NTAG_SA ), STAT = IOST )
           CALL CHECKMEM ( IOST, 'WCH_VOC', 'VOC_INIT' )
           WCH_VOC = 0.0
         ENDIF ! wch_voc allocated?
       
         IF ( .NOT. ALLOCATED( NVOCTRACKED ) ) THEN
           ALLOCATE ( NVOCTRACKED( NTAG_SA ), STAT = IOST )
           CALL CHECKMEM ( IOST, 'NVOCTRACKED', 'VOC_INIT' )
           NVOCTRACKED = 0.0
         ENDIF ! nvoctracked allocated?

!....Allocate mir_weitd, koh_weitd
         IF ( .NOT. ALLOCATED( MIR_WEITD ) ) THEN
           ALLOCATE ( MIR_WEITD( NTAG_SA ), STAT = IOST )
           CALL CHECKMEM ( IOST, 'MIR_WEITD', 'VOC_INIT' )
           MIR_WEITD = 0.0
         ENDIF ! nvoctracked allocated?

         IF ( .NOT. ALLOCATED( KOH_WEITD ) ) THEN
           ALLOCATE ( KOH_WEITD( NTAG_SA ), STAT = IOST )
           CALL CHECKMEM ( IOST, 'KOH_WEITD', 'VOC_INIT' )
           KOH_WEITD = 0.0
         ENDIF ! nvoctracked allocated?

       ENDIF ! firstime or not ?

       END SUBROUTINE VOC_INIT

C===========================================================================================
       SUBROUTINE OP_O3INDICAT( JDATE, JTIME, TSTEP )

C20140428
c       Opens O3_INDIC_1 file to store multiple O3-regime indicators' values.
c       However, under O3 production-destruction formulation, this subroutine is not used.
c
c       Called by SA_WRAP_OZ.F
c 

       USE GRID_CONF
       USE UTILIO_DEFN
       !USE SUBST_MODULES

       IMPLICIT NONE

       INCLUDE SUBST_FILES_ID    ! file name parameters

! Arguments
       INTEGER    JDATE          ! current model date, coded YYYYDDD
       INTEGER    JTIME          ! current model time, coded HHMMSS
       INTEGER    TSTEP(3)       ! format 10000, 0, 0
!       CHARACTER(16)  FILNAME    ! output filename

! Scratch
       CHARACTER(16)  :: PNAME = 'OP_O3INDICAT'
       CHARACTER(96)  :: XMSG = ' '
       INTEGER   S, L
       INTEGER   ALLOCSTAT

C-----------------------------------------------------------------------
!       CALL SUBST_BARRIER

       NVARS3D = N_INDIC + 1 + N_INDORIG

       DO S = 1, N_INDIC
         VNAME3D( S ) = VNAM_INDIC( S )
         VTYPE3D( S ) = M3REAL
         UNITS3D( S ) = UNITS_INDIC( S )
         VDESC3D( S ) = DESC_INDIC( S )
       ENDDO ! S
       ! vote for nox
       VNAME3D( N_INDIC + 1 ) = 'VOTE_NOX'
       VTYPE3D( N_INDIC + 1 ) = M3REAL
       UNITS3D( N_INDIC + 1 ) = 'unitless'
       VDESC3D( N_INDIC + 1 ) = 'Fraction of indicators '
     &               // 'in favor of NOx-limited regime'


!20140305 additional outputs of indicators' original variables
       DO S = N_INDIC + 1 + 1, N_INDIC + 1 + N_INDORIG
         VNAME3D( S ) = VNAM_INDORIG( S - N_INDIC - 1 )
         VTYPE3D( S ) = M3REAL
         UNITS3D( S ) = UNITS_INDORIG( S - N_INDIC -1 )
         VDESC3D( S ) = VNAM_INDORIG( S - N_INDIC - 1 )
       END DO ! indicators original variables

       SDATE3D = JDATE
       STIME3D = JTIME

       FTYPE3D = GRDDED3
!KRT20130131       TSTEP3D = TSTEP( 2 )     ! output sciproc sync time steps
       TSTEP3D = TSTEP( 1 )     ! output hourly time steps

       NCOLS3D = GL_NCOLS
       NROWS3D = GL_NROWS
       NLAYS3D =     1
       NTHIK3D =     1
       GDTYP3D = GDTYP_GD
       P_ALP3D = P_ALP_GD
       P_BET3D = P_BET_GD
       P_GAM3D = P_GAM_GD
       XORIG3D = XORIG_GD
       YORIG3D = YORIG_GD
       XCENT3D = XCENT_GD
       YCENT3D = YCENT_GD
       XCELL3D = XCELL_GD
       YCELL3D = YCELL_GD
       VGTYP3D = VGTYP_GD
       VGTOP3D = VGTOP_GD
       VGLVS3D( 1 ) = VGLVS_GD( 1 )
       VGLVS3D( 2 ) = VGLVS_GD( 2 )

       GDNAM3D = GRID_NAME  ! from HGRD_DEFN

       FDESC3D( 1 ) =
     &    " Output O3 regime indicator(s) "
       DO L = 2, MXDESC3
         FDESC3D( L ) = ' '
       END DO

       IF ( MYPE .EQ. 0 ) THEN
        IF ( .NOT. OPEN3( O3INDIC_1, FSRDWR3, PNAME ) ) THEN
         XMSG = 'Could not open ' // O3INDIC_1 // ' file for update - '
     &        // 'try to open new'
         CALL M3MESG( XMSG )
           IF ( .NOT. OPEN3( O3INDIC_1, FSNEW3, PNAME ) ) THEN
              XMSG = 'Could not create '// O3INDIC_1 // ' file'
              CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
           END IF
        ENDIF ! file existing ?
       END IF ! mype0

       ALLOCATE( INDICVALU( MY_NCOLS, MY_NROWS, NLAYS, N_INDIC+1 ), STAT=ALLOCSTAT )
       IF ( .NOT. ALLOCATED( INDICVALU ) ) THEN
         XMSG = 'Failure allocating INDICVALU'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       ENDIF

       ALLOCATE( INDORIG( MY_NCOLS, MY_NROWS, NLAYS, N_INDORIG ), STAT=ALLOCSTAT )
       IF ( .NOT. ALLOCATED( INDORIG ) ) THEN
         XMSG = 'Failure allocating INDICVALU'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       ENDIF

       END SUBROUTINE OP_O3INDICAT

      END MODULE OZVOC_DEFN
