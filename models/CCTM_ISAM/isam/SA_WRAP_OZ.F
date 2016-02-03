
      SUBROUTINE SA_WRAP_OZ( CGRID, CBFOR, ISAM, ISAMB4, JDATE, JTIME, TSTEP )

!*************************************************************
!20140428  Apportions ozone tags into either NOx- or VOC-sensitive regime
!          first with total O3 production then with O3 destruction.
!
!          Called by sciproc.F
!*************************************************************
      USE GRID_CONF
      USE CGRID_SPCS
      USE SA_GRD_DEFN  ! for SA_SULF_NEW array; also inherits SA_DEFN
      USE SA_IRR_DEFN  ! for apportioned reaction rates  inherits hrdata
      USE UTILIO_DEFN
      USE OZVOC_DEFN   ! 20120928 

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID

! Arguments
      REAL, POINTER             :: CGRID( :,:,:,: )
      REAL, POINTER             :: CBFOR( :,:,:,: )
      REAL, POINTER             :: ISAM( :,:,:,:,: )
      REAL, POINTER             :: ISAMB4( :,:,:,:,: )
      INTEGER, INTENT( IN )     :: JDATE, JTIME
      INTEGER, INTENT( IN )     :: TSTEP( 3 ) ! 1 local output timestep;
                                              ! 2 sciproc sync timestep.

! Scratch
      CHARACTER( 16 ), SAVE :: PNAME = 'SA_WRAP_OZ'
      INTEGER C,R,L,S,V,K
      REAL, ALLOCATABLE, SAVE :: TTLB4( :,:,:,: )
      INTEGER  LENSTR
      CHARACTER( 96 )  :: XMSG = ' '

! NOX related
      INTEGER, SAVE :: JNO, JNO2, JNO3, JHONO
      INTEGER, SAVE :: JN2O5, JPNA, JPAN, JPANX
      INTEGER, SAVE :: JHNO3, JNTR
      REAL, ALLOCATABLE, SAVE :: CGNEW( : )
      REAL, ALLOCATABLE, SAVE :: CGOLD( : )
      REAL, ALLOCATABLE, SAVE :: CGMID( : )

! SULF related Now Removed 20130606

! Ozone related 20120908
      INTEGER, SAVE :: JO3V, JO3N
      REAL             DENOMO3 ! sum over tags for either regime of O3 in each grid cell
      REAL,    SAVE :: R_OZTAGS  ! number of ozone tags in real type

! Regime indicator related
      CHARACTER(16)    INDICNAME   ! file name to hold indicator values
      CHARACTER(7)     CHDATE
!      INTEGER, SAVE :: C_H2O2, C_HNO3, C_O3, C_FORM
!      INTEGER, SAVE :: C_NO, C_NO2, C_NO3, C_N2O5, C_PAN
      LOGICAL          COND_INDIC
      REAL             DENOM1, DENOM2
      REAL             VOTE_NOX
!      REAL             P_H2O2, P_ROOH, P_HNO3   
!                        GST...are the above 3 varibles declared? Yes, in OZVOC_DEFN

! VOC related 20120928; voc species made explicit, 20131211
      INTEGER, ALLOCATABLE, SAVE :: JVOC( : )
       INTEGER, SAVE :: INDICTYP  ! Type of O3 regime indicator: 
      INTEGER          IOST
      REAL          :: INDICREG( N_INDIC )  ! indicators of ozone regimes
      REAL, ALLOCATABLE, SAVE ::  NUMERO( : )     ! For each tag, sum over all species each product of voc and respective mir
      REAL             DENOMINO     ! Tag-total of the product: VOC tag times weighted MIR tag
      INTEGER          JV        ! index running through VOC list from 1 to N_EVOC

! Logical
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

! 20140309 destruction-production
      CHARACTER( 10 )  CHPO3       ! O3 production term
      CHARACTER( 10 )  CHDO3       ! Delta O3
      CHARACTER( 10 )  CHTROY      ! O3 destruction term
      CHARACTER( 10 )  CHMJR       ! Major component of TROYO3; 20140311
      CHARACTER( 10 )  CHOXX       ! Other Ox loss in TROYO3; 20140311

      CHARACTER( 10 )  CHDOXNET    ! Ox error term
      CHARACTER(  3 )  CHCOL       ! column internally written
      CHARACTER(  3 )  CHROW       ! row internally written
      CHARACTER(  3 )  CHLAY       ! layerinternally written

!20140321      LOGICAL          FLAGOxSA    ! flag to select O3 or Ox SA

      REAL             DO3         ! net change of ozone  20140307
      REAL             PO3         ! Ox or O3 production (O3 inferred from DO3 and destruction)
!      REAL             TROYO3      ! Ox or O3 destruction term ; already declared in OZVOC_DEFN
      REAL             OxProd      ! Ox production
      REAL             OxLoss      ! Ox destruction
      REAL             DOxNet      ! net Ox loss calculated by summing conc change in chem step
      REAL             DOx_error   ! difference between POx from integrate reaction and DOxNet 

      REAL, ALLOCATABLE, SAVE :: SA_O3N( : )  ! Intermediate NOx-lim O3 array of ntag_sa; 20140314
      REAL, ALLOCATABLE, SAVE :: SA_O3V( : )  ! Intermediate VOC-lim O3 array of ntag_sa; 20140314


!-------------------------------------------------------------------

      IF ( .NOT. ANY( L_OZONE ) ) RETURN
      IF ( .NOT. YES_2REGIME ) RETURN

      IF ( FIRSTIME ) THEN
        FIRSTIME = .FALSE.
      
        !krt Identify species index in ISAM array
      
        JNO   = INDEX1( 'NO' ,  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JNO2  = INDEX1( 'NO2',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JNO3  = INDEX1( 'NO3',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JHONO = INDEX1( 'HONO', NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JN2O5 = INDEX1( 'N2O5', NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JPNA  = INDEX1( 'PNA',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JPAN  = INDEX1( 'PAN',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JPANX = INDEX1( 'PANX', NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JHNO3 = INDEX1( 'HNO3', NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JNTR  = INDEX1( 'NTR',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )

        JO3V  = INDEX1( 'O3V',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JO3N  = INDEX1( 'O3N',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )

        IF ( .NOT. ALLOCATED( JVOC ) ) ALLOCATE( JVOC( N_EVOC ) )
        IF ( YES_PRINT ) THEN
          if ( mype .eq. 0 ) print*,'jv, nam_voc(jv), jvoc(jv):'
        ENDIF ! yes_print
        DO JV = 1, N_EVOC
          JVOC( JV )  = INDEX1( NAM_VOC( JV ),  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
          if ( mype .eq. 0 ) print*,JV, NAM_VOC( JV ), JVOC( JV )
        ENDDO ! jv

        IF ( .NOT. ALLOCATED( TTLB4 ) )
     &    ALLOCATE( TTLB4( MY_NCOLS, MY_NROWS, NLAYS, NSPC_SA ) )
      
        IF ( .NOT. ALLOCATED( CGNEW ) ) ALLOCATE( CGNEW( N_SPEC ) )
      
        IF ( .NOT. ALLOCATED( CGOLD ) ) ALLOCATE( CGOLD( N_SPEC ) )

        IF ( .NOT. ALLOCATED( CGMID ) ) ALLOCATE( CGMID( N_SPEC ) )

        ! 20131211
        IF ( .NOT. ALLOCATED( NUMERO ) ) ALLOCATE( NUMERO( NTAG_SA ) )

        !Determine users' choice between  Ox and O3 SA
        FLAGOXSA = ENVYN( 'YES_OXLOSS', 'Y= use Ox; N= use O3', .FALSE., IOST )

        ! Determine O3 regime indicator type
!20140416        INDICTYP = ENVINT( 'O3INDICATOR_TYPE','2=P(H2O2)/P(HNO3), etc', 2, IOST )
        INDICTYP = 2
        IF ( IOST .NE. 0 )
     &    CALL M3EXIT( PNAME, 0, 0, 'O3 indicator type undetermined', 2 )

        ! Choose to write out the regime indicator
        YES_INDIC = ENVYN( 'WR_INDICATOR',
     &           'Want to output peroxides-to-hno3 as regime indicator?',
     &           .FALSE., IOST )
        IF ( YES_INDIC ) THEN
          CALL OP_O3INDICAT ( JDATE, JTIME, TSTEP )
        ENDIF ! yes_indic
  
        ! Count number of ozone tags in the first place
        R_OZTAGS = REAL( COUNT( L_OZONE ) )

        !Allocate intermediate either regime O3 arrays
        IF ( .NOT. ALLOCATED( SA_O3N ) ) ALLOCATE( SA_O3N( NTAG_SA ) )
        IF ( .NOT. ALLOCATED( SA_O3V ) ) ALLOCATE( SA_O3V( NTAG_SA ) )
        SA_O3N = 0.0
        SA_O3V = 0.0
    
      ENDIF ! firstime
      
      !krt Sum up pre-process tags for each species at each grid cell
      TTLB4 = 0.0
      CGNEW = 0.0
      CGOLD = 0.0
      DO S = 1, NSPC_SA
        DO K = 1, NTAG_SA
          DO L = 1, NLAYS
            DO R = 1, MY_NROWS
              DO C = 1, MY_NCOLS
                TTLB4( C,R,L,S ) = TTLB4( C,R,L,S ) + ISAMB4( C,R,L,S,K )
              ENDDO ! C
            ENDDO ! R
          ENDDO ! L
        ENDDO ! K
      ENDDO ! S

!KRT20130131 initialize ozone indicator
      IF ( YES_INDIC ) THEN
        INDICVALU = -1.0
        INDORIG = 1.E-30
      ENDIF ! yes_indic

Ckrt Apportion the conc
      DO L = 1, NLAYS
        DO R = 1, MY_NROWS
          DO C = 1, MY_NCOLS
            DO V = 1, N_SPEC
              CGNEW( V ) = CGRID( C,R,L,V )
              CGOLD( V ) = CBFOR( C,R,L,V )
            ENDDO ! loop of regular species
            CGMID = 0.5 * ( CGNEW + CGOLD )

! the call for pl_cb05 now removed  20130606

            ! Calculate production of H2O2, ROOH, and HNO3
            P_H2O2  =   RKI_INIT( C,R,L,34 ) * CGMID( HO2 ) * CGMID( HO2 )
     &            + RKI_INIT( C,R,L,35 ) * CGMID( HO2 ) * CGMID( HO2 )
!20140307     &            + RKI_INIT( C,R,L,42 ) * CGMID( OH ) * CGMID( OH )

            P_ROOH  =  RKI_INIT( C,R,L,56 ) * CGMID( XO2 ) * CGMID( HO2 )
     &            + RKI_INIT( C,R,L,57 ) * CGMID( XO2N ) * CGMID( HO2 )


            P_HNO3  =  RKI_INIT( C,R,L, 28 ) * CGMID( NO2 ) * CGMID( OH )


Ckrt....20120928 Ozone
            IF ( JO3V .GT. 0 ) THEN
              ! Ozone production attributed to either regime
              DENOM1 = CBFOR(C,R,L, NO)+CBFOR(C,R,L, NO2)
              DENOM2 = CBFOR(C,R,L, NO3) + CBFOR(C,R,L, HNO3)
     &                 + CBFOR(C,R,L, N2O5) + CBFOR(C,R,L, PAN)
              INDICREG( 1 ) = ( P_H2O2 + P_ROOH ) / MAX( P_HNO3, 1.E-30 )
              INDICREG( 2 ) = P_H2O2 / MAX( P_HNO3, 1.E-30 )
              INDICREG( 3 ) = CBFOR( C,R,L, H2O2 )/ MAX( CBFOR( C,R,L, HNO3 ) , 1.E-30 )
              INDICREG( 4 ) = CBFOR ( C,R,L, H2O2 )
     &             / MAX( CBFOR(C,R,L, O3)+CBFOR(C,R,L, NO2 ) , 1.E-30 )
              INDICREG( 5 ) = DENOM1 + DENOM2
              INDICREG( 6 ) = CBFOR( C,R,L, O3 ) / MAX( DENOM1, 1.E-30 )
              INDICREG( 7 ) = CBFOR( C,R,L, O3 ) 
     &              / MAX( DENOM1 + DENOM2, 1.E-30 )
              INDICREG( 8 ) = CBFOR( C,R,L, O3 ) / MAX( DENOM2, 1.E-30 )
              INDICREG( 9 ) = CBFOR( C,R,L, FORM ) 
     &              / MAX( DENOM1 + DENOM2, 1.E-30 )
              INDICREG( 10 ) = CBFOR( C,R,L, FORM ) 
     &                / MAX( CBFOR(C,R,L, NO2), 1.E-30 )



!20140321 Need to add a global logical flag to select O3 or Ox SA
              IF ( FLAGOXSA ) THEN
               ! We don't need to calculate net delta Ox because we calculate integrated 
               ! production and loss rates of Ox, but we can to a mass balance check
               ! to to see if OxProd - OxLoss = DOxNet
               DOxNet = CGRID( C,R,L, SPC_INDEX(JO3V,2)) - CBFOR( C,R,L, SPC_INDEX(JO3V,2))  !O3
     &             + CGRID( C,R,L, SPC_INDEX(JNO2,2)) - CBFOR( C,R,L, SPC_INDEX(JNO2,2))  !NO2
     &             + 2.*(CGRID( C,R,L, SPC_INDEX(JNO3,2)) - CBFOR( C,R,L, SPC_INDEX(JNO3,2)))   !NO3
     &             + 3.*(CGRID( C,R,L, SPC_INDEX(JN2O5,2)) - CBFOR( C,R,L, SPC_INDEX(JN2O5,2))) !N2O5
     &             + CGRID( C,R,L, SPC_INDEX(JPNA,2)) - CBFOR( C,R,L, SPC_INDEX(JPNA,2))    !PNA
     &             + CGRID( C,R,L, SPC_INDEX(JPAN,2)) - CBFOR( C,R,L, SPC_INDEX(JPAN,2))    !PAN
     &             + CGRID( C,R,L, SPC_INDEX(JPANX,2)) - CBFOR( C,R,L, SPC_INDEX(JPANX,2))  !PANX
              ELSE
                DO3 = CGRID( C,R,L, SPC_INDEX(JO3V,2)) - CBFOR( C,R,L, SPC_INDEX(JO3V,2))
              ENDIF

                !Update intermediate O3 with ISAMB4  20140320
              DO K = 1, NTAG_SA
                SA_O3N( K ) = ISAMB4( C,R,L,JO3N, K )
                SA_O3V( K ) = ISAMB4( C,R,L,JO3V, K )
              ENDDO ! loop K

              IF ( FLAGOXSA ) THEN
                OxProd = 
     &                2.000*RKI_INIT(C,R,L, 22)*CGMID(NO)*CGMID(NO)
     &              + 1.000*RKI_INIT(C,R,L, 26)*CGMID(OH)*CGMID(HONO)
     &              + 1.000*RKI_INIT(C,R,L, 27)*CGMID(HONO)*CGMID(HONO)
     &              + 2.000*RKI_INIT(C,R,L, 29)*CGMID(OH)*CGMID(HNO3)
     &              + 1.000*RKI_INIT(C,R,L, 30)*CGMID(HO2)*CGMID(NO)
     &              + 1.000*RKI_INIT(C,R,L, 41)*CGMID(OH)*CGMID(OH)
     &              + 0.390*RKI_INIT(C,R,L, 51)*CGMID(PNA)
     &              + 1.000*RKI_INIT(C,R,L, 52)*CGMID(HNO3)
     &              + 1.000*RKI_INIT(C,R,L, 54)*CGMID(XO2)*CGMID(NO)
     &              + 1.000*RKI_INIT(C,R,L, 62)*CGMID(NTR)
     &              + 1.000*RKI_INIT(C,R,L, 67)*CGMID(MEO2)*CGMID(NO)
     &              + 1.000*RKI_INIT(C,R,L, 80)*CGMID(HCO3)*CGMID(NO)
     &              + 1.000*RKI_INIT(C,R,L, 87)*CGMID(C2O3)*CGMID(NO)
     &              + 0.200*RKI_INIT(C,R,L, 91)*CGMID(C2O3)*CGMID(HO2)
     &              + 1.000*RKI_INIT(C,R,L,102)*CGMID(CXO3)*CGMID(NO)
     &              + 0.200*RKI_INIT(C,R,L,107)*CGMID(CXO3)*CGMID(HO2)
     &              + 0.860*RKI_INIT(C,R,L,129)*CGMID(TO2)*CGMID(NO)
     &              + 1.000*RKI_INIT(C,R,L,139)*CGMID(CRN2)*CGMID(NO)
     &              + 0.860*RKI_INIT(C,R,L,149)*CGMID(CAO2)*CGMID(NO)
     &              + 1.000*RKI_INIT(C,R,L,151)*CGMID(OPO3)*CGMID(NO)
     &              + 1.000*RKI_INIT(C,R,L,153)*CGMID(OPAN)
     &              + 1.000*RKI_INIT(C,R,L,177)*CGMID(CLO)*CGMID(NO)

                OxLoss = 
     &                2.000*RKI_INIT(C,R,L,  4)*CGMID(O)*CGMID(NO2)
     &              + 1.000*RKI_INIT(C,R,L, 11)*CGMID(O1D)
     &              + 1.000*RKI_INIT(C,R,L, 12)*CGMID(O3)*CGMID(OH)
     &              + 1.000*RKI_INIT(C,R,L, 13)*CGMID(O3)*CGMID(HO2)
     &              + 2.000*RKI_INIT(C,R,L, 15)*CGMID(NO3)
     &              + 2.000*RKI_INIT(C,R,L, 17)*CGMID(NO3)*CGMID(NO2)
     &              + 3.000*RKI_INIT(C,R,L, 19)*CGMID(N2O5)
     &              + 3.000*RKI_INIT(C,R,L, 20)*CGMID(N2O5)
     &              + 1.000*RKI_INIT(C,R,L, 23)*CGMID(NO)*CGMID(NO2)
     &              + 1.000*RKI_INIT(C,R,L, 28)*CGMID(NO2)*CGMID(OH)
     &              + 1.000*RKI_INIT(C,R,L, 38)*CGMID(O1D)
     &              + 1.000*RKI_INIT(C,R,L, 40)*CGMID(OH)*CGMID(O)
     &              + 1.000*RKI_INIT(C,R,L, 44)*CGMID(HO2)*CGMID(O)
     &              + 1.000*RKI_INIT(C,R,L, 45)*CGMID(H2O2)*CGMID(O)
     &              + 2.000*RKI_INIT(C,R,L, 46)*CGMID(NO3)*CGMID(O)
     &              + 1.000*RKI_INIT(C,R,L, 47)*CGMID(NO3)*CGMID(OH)
     &              + 2.000*RKI_INIT(C,R,L, 48)*CGMID(NO3)*CGMID(HO2)
     &              + 2.000*RKI_INIT(C,R,L, 49)*CGMID(NO3)*CGMID(O3)
     &              + 2.000*RKI_INIT(C,R,L, 50)*CGMID(NO3)*CGMID(NO3)
     &              + 1.000*RKI_INIT(C,R,L, 76)*CGMID(FORM)*CGMID(O)
     &              + 2.000*RKI_INIT(C,R,L, 77)*CGMID(FORM)*CGMID(NO3)
     &              + 1.000*RKI_INIT(C,R,L, 83)*CGMID(ALD2)*CGMID(O)
     &              + 2.000*RKI_INIT(C,R,L, 85)*CGMID(ALD2)*CGMID(NO3)
     &              + 1.000*RKI_INIT(C,R,L, 98)*CGMID(ALDX)*CGMID(O)
     &              + 2.000*RKI_INIT(C,R,L,100)*CGMID(ALDX)*CGMID(NO3)
     &              + 1.000*RKI_INIT(C,R,L,115)*CGMID(ROR)*CGMID(NO2)
     &              + 1.000*RKI_INIT(C,R,L,116)*CGMID(O)*CGMID(OLE)
     &              + 1.000*RKI_INIT(C,R,L,118)*CGMID(O3)*CGMID(OLE)
     &              + 1.000*RKI_INIT(C,R,L,119)*CGMID(NO3)*CGMID(OLE)
     &              + 1.000*RKI_INIT(C,R,L,120)*CGMID(O)*CGMID(ETH)
     &              + 1.000*RKI_INIT(C,R,L,122)*CGMID(O3)*CGMID(ETH)
     &              + 1.000*RKI_INIT(C,R,L,123)*CGMID(NO3)*CGMID(ETH)
     &              + 1.000*RKI_INIT(C,R,L,124)*CGMID(IOLE)*CGMID(O)
     &              + 0.500*RKI_INIT(C,R,L,126)*CGMID(IOLE)*CGMID(O3)
     &              + 1.000*RKI_INIT(C,R,L,127)*CGMID(IOLE)*CGMID(NO3)
     &              + 2.000*RKI_INIT(C,R,L,132)*CGMID(CRES)*CGMID(NO3)
     &              + 1.000*RKI_INIT(C,R,L,133)*CGMID(CRO)*CGMID(NO2)
     &              + 2.000*RKI_INIT(C,R,L,136)*CGMID(CRON)*CGMID(NO3)
     &              + 1.000*RKI_INIT(C,R,L,137)*CGMID(CRNO)*CGMID(NO2)
     &              + 1.000*RKI_INIT(C,R,L,138)*CGMID(CRNO)*CGMID(O3)
     &              + 1.000*RKI_INIT(C,R,L,145)*CGMID(OPEN)*CGMID(O3)
     &              + 2.000*RKI_INIT(C,R,L,146)*CGMID(OPEN)*CGMID(NO3)
     &              + 2.000*RKI_INIT(C,R,L,148)*CGMID(CAT1)*CGMID(NO3)
     &              + 1.000*RKI_INIT(C,R,L,152)*CGMID(OPO3)*CGMID(NO2)
     &              + 1.000*RKI_INIT(C,R,L,157)*CGMID(O)*CGMID(ISOP)
     &              + 1.000*RKI_INIT(C,R,L,159)*CGMID(O3)*CGMID(ISOP)
     &              + 1.800*RKI_INIT(C,R,L,160)*CGMID(NO3)*CGMID(ISOP)
     &              + 1.000*RKI_INIT(C,R,L,162)*CGMID(O3)*CGMID(ISPD)
     &              + 2.000*RKI_INIT(C,R,L,163)*CGMID(NO3)*CGMID(ISPD)
     &              + 1.000*RKI_INIT(C,R,L,165)*CGMID(TERP)*CGMID(O)
     &              + 1.000*RKI_INIT(C,R,L,167)*CGMID(TERP)*CGMID(O3)
     &              + 1.530*RKI_INIT(C,R,L,168)*CGMID(TERP)*CGMID(NO3)
     &              + 1.000*RKI_INIT(C,R,L,172)*CGMID(NO2)*CGMID(ISOP)
     &              + 1.000*RKI_INIT(C,R,L,175)*CGMID(CL)*CGMID(O3)

             !Flag for O3 SA, calculate destruction terms for O3
              ELSE
                TROYO3  =
     &          - 2.000*RKI_INIT(C,R,L,  4) *CGMID(O)*CGMID(NO2)
     &          - 1.000*RKI_INIT(C,R,L, 11) *CGMID(O1D)
     &          - 1.000*RKI_INIT(C,R,L, 12) *CGMID(O3)*CGMID(OH)
     &          - 1.000*RKI_INIT(C,R,L, 13) *CGMID(O3)*CGMID(HO2)
     &          - 1.000*RKI_INIT(C,R,L, 15) *CGMID(NO3)
     &          - 1.000*RKI_INIT(C,R,L, 17) *CGMID(NO3)*CGMID(NO2)
     &          - 1.000*RKI_INIT(C,R,L, 19) *CGMID(N2O5)
     &          - 1.000*RKI_INIT(C,R,L, 20) *CGMID(N2O5)
     &          - 1.000*RKI_INIT(C,R,L, 38) *CGMID(O1D)
     &          - 1.000*RKI_INIT(C,R,L, 40) *CGMID(OH)*CGMID(O)
     &          - 1.000*RKI_INIT(C,R,L, 44) *CGMID(HO2)*CGMID(O)
     &          - 1.000*RKI_INIT(C,R,L, 45) *CGMID(H2O2)*CGMID(O)
     &          - 1.000*RKI_INIT(C,R,L, 46) *CGMID(NO3)*CGMID(O)
     &          - 1.000*RKI_INIT(C,R,L, 47) *CGMID(NO3)*CGMID(OH)
     &          - 1.000*RKI_INIT(C,R,L, 48) *CGMID(NO3)*CGMID(HO2)
     &          - 2.000*RKI_INIT(C,R,L, 49) *CGMID(NO3)*CGMID(O3)
     &          - 2.000*RKI_INIT(C,R,L, 50) *CGMID(NO3)*CGMID(NO3)
     &          - 1.000*RKI_INIT(C,R,L, 76) *CGMID(FORM)*CGMID(O)
     &          - 1.000*RKI_INIT(C,R,L, 77) *CGMID(FORM)*CGMID(NO3)
     &          - 1.000*RKI_INIT(C,R,L, 83) *CGMID(ALD2)*CGMID(O)
     &          - 1.000*RKI_INIT(C,R,L, 85) *CGMID(ALD2)*CGMID(NO3)
     &          - 1.000*RKI_INIT(C,R,L, 98) *CGMID(ALDX)*CGMID(O)
     &          - 1.000*RKI_INIT(C,R,L,100) *CGMID(ALDX)*CGMID(NO3)
     &          - 1.000*RKI_INIT(C,R,L,115) *CGMID(ROR)*CGMID(NO2)
     &          - 1.000*RKI_INIT(C,R,L,116) *CGMID(O)*CGMID(OLE)
     &          - 1.000*RKI_INIT(C,R,L,118) *CGMID(O3)*CGMID(OLE)
     &          - 1.000*RKI_INIT(C,R,L,119) *CGMID(NO3)*CGMID(OLE)
     &          - 1.000*RKI_INIT(C,R,L,120) *CGMID(O)*CGMID(ETH)
     &          - 1.000*RKI_INIT(C,R,L,122) *CGMID(O3)*CGMID(ETH)
     &          - 1.000*RKI_INIT(C,R,L,123) *CGMID(NO3)*CGMID(ETH)
     &          - 1.000*RKI_INIT(C,R,L,124) *CGMID(IOLE)*CGMID(O)
     &          - 0.500*RKI_INIT(C,R,L,126) *CGMID(IOLE)*CGMID(O3)
     &          - 1.000*RKI_INIT(C,R,L,127) *CGMID(IOLE)*CGMID(NO3)
     &          - 1.000*RKI_INIT(C,R,L,132) *CGMID(CRES)*CGMID(NO3)
     &          - 1.000*RKI_INIT(C,R,L,136) *CGMID(CRON)*CGMID(NO3)
     &          - 1.000*RKI_INIT(C,R,L,138) *CGMID(CRNO)*CGMID(O3)
     &          - 1.000*RKI_INIT(C,R,L,145) *CGMID(OPEN)*CGMID(O3)
     &          - 1.000*RKI_INIT(C,R,L,146) *CGMID(OPEN)*CGMID(NO3)
     &          - 1.000*RKI_INIT(C,R,L,148) *CGMID(CAT1)*CGMID(NO3)
     &          - 1.000*RKI_INIT(C,R,L,152) *CGMID(OPO3)*CGMID(NO2)
     &          - 1.000*RKI_INIT(C,R,L,157) *CGMID(O)*CGMID(ISOP)
     &          - 1.000*RKI_INIT(C,R,L,159) *CGMID(O3)*CGMID(ISOP)
     &          - 1.800*RKI_INIT(C,R,L,160) *CGMID(NO3)*CGMID(ISOP)
     &          - 1.000*RKI_INIT(C,R,L,162) *CGMID(O3)*CGMID(ISPD)
     &          - 1.000*RKI_INIT(C,R,L,163) *CGMID(NO3)*CGMID(ISPD)
     &          - 1.000*RKI_INIT(C,R,L,165) *CGMID(TERP)*CGMID(O)
     &          - 1.000*RKI_INIT(C,R,L,167) *CGMID(TERP)*CGMID(O3)
     &          - 1.530*RKI_INIT(C,R,L,168) *CGMID(TERP)*CGMID(NO3)
     &          - 1.000*RKI_INIT(C,R,L,175) *CGMID(CL)*CGMID(O3)
              ENDIF !flag for Ox or O3 SA


c examples of possible cases:
c PO3 = DO3 - destroyed O3
c  8  =  5  - (-3)  PO3 greater than net postive DO3
c  2  = -4  - (-6)  PO3 postive when net DO3 negative        
c  0  = -4  - (-4)  PO3 zero
c
cGST...perform update for each time step after convergence 
c
c  Note that PO3 and TROYO3 are used below for either Ox or O3 SA, depending on flagOxSA

              IF ( FLAGOXSA ) THEN
                PO3 = OxProd * TIME2SEC( TSTEP(2) )/60.0
                TROYO3 = -OxLoss * TIME2SEC( TSTEP(2) )/60.0
                DOx_error = DOxNet - ( PO3 + TROYO3 )
                IF ( DOx_error .GT. 0.001) THEN
                  WRITE( CHDOXNET, FMT = '(E10.1E2)' ) DOxNet
                  WRITE( CHPO3, FMT = '(E10.1E2)' ) PO3
                  WRITE( CHTROY, FMT = '(E10.1E2)' ) TROYO3
                  WRITE( CHCOL, FMT ='(I3)' ) C
                  WRITE( CHROW, FMT ='(I3)' ) R
                  WRITE( CHLAY, FMT ='(I3)' ) L
                 XMSG = 'DOx_error at (' // CHCOL // ',' // CHROW // ','
     &  // CHLAY // ')' // CHDOXNET // ',' // CHPO3 // ',' // CHTROY
!20140324                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                ENDIF

              ELSE
                TROYO3 = TROYO3 * TIME2SEC( TSTEP(2) )/60.0
                TROYO3 = MIN( DO3,TROYO3 ) 
                PO3 = DO3 - TROYO3
              ENDIF          

!20140320              WRITE( CHPO3, FMT = '(E10.1E2)' ) PO3
!20140320              WRITE( CHDO3, FMT = '(E10.1E2)' ) DO3
!20140320              WRITE( CHTROY, FMT = '(E10.1E2)' ) TROYO3

              IF ( PO3 .GT. 0.0 ) THEN  !GST.. you can have some PO3 even if DO3 was negative

              !  3. Update ISAM tracers for production of O3 or Ox 

                IF ( YES_INDIC .OR. INDICTYP .EQ. N_INDIC + 1 ) THEN
                  VOTE_NOX = 0.0
                  DO S = 1 , N_INDIC
                   IF ( S .NE. 5 ) THEN
                    COND_INDIC = INDICREG( S ) .GE. TRANSV( S )
                   ELSE
                    COND_INDIC = INDICREG( S ) .LE. TRANSV( S )
                   ENDIF ! indictyp != 5 ?
                   ! nox-limited?
                   IF ( COND_INDIC ) VOTE_NOX = VOTE_NOX + 1  
                  ENDDO ! S 1:n_indic
                  INDICVALU( C,R,L, N_INDIC+1 ) = VOTE_NOX / REAL(N_INDIC)
                ENDIF ! yes_indic
                IF ( INDICTYP .EQ. 5 ) THEN
                  COND_INDIC = INDICREG(INDICTYP) .LE. TRANSV(INDICTYP)
                ELSEIF ( INDICTYP .EQ. N_INDIC + 1 ) THEN
                  COND_INDIC = VOTE_NOX .GT. N_INDIC / 2.0
                ELSE
                  COND_INDIC = INDICREG(INDICTYP) .GE. TRANSV(INDICTYP)
                ENDIF ! indictyp != 5 ?
                IF ( COND_INDIC ) THEN ! nox-limited in the abundance of VOC
                  DO K = 1, NTAG_SA
                    IF ( TTLB4( C,R,L,JNO ) + TTLB4( C,R,L,JNO2 ) .GT. 1.E-20 ) THEN
                      SA_O3N( K ) = ISAMB4( C,R,L, JO3N, K )
     &                    + PO3 
     &                     * ( ISAMB4(C,R,L, JNO, K) + ISAMB4(C,R,L, JNO2, K) )
     &                     / ( TTLB4( C,R,L, JNO ) + TTLB4( C,R,L, JNO2 ) )
                    ELSE
                      SA_O3N( K ) = ISAMB4( C,R,L, JO3N, K )
     &                                    + PO3 / R_OZTAGS
                    ENDIF ! bulk no + bulk no2 > 0 ?
                  ENDDO ! loop K
                ELSE  ! voc-limited in the abundance of NOx
                  NUMERO = 0.0
                  DENOMINO = 0.0
                  DO K = 1, NTAG_SA
                    DO JV = 1, N_EVOC
                      NUMERO( K ) = NUMERO( K ) 
     &                          + ISAMB4( C,R,L,JVOC(JV), K ) * MIR( JV )
                    ENDDO ! jv
                    DENOMINO = DENOMINO + NUMERO( K )
                  ENDDO ! loop K
                  DO K = 1, NTAG_SA
                    IF ( DENOMINO .GT. 1.E-20 ) THEN
                      SA_O3V( K ) = ISAMB4( C,R,L, JO3V, K )
     &                    + PO3 * NUMERO( K ) / DENOMINO
                    ELSE
                      SA_O3V( K ) = ISAMB4( C,R,L, JO3V, K )
     &                    + PO3 / R_OZTAGS
                    ENDIF ! vxmir > 0 ?
                  ENDDO ! loop K
                ENDIF ! indicreg >= 0.5 ?
                ! Optional storage of all kinds of indicators
                IF ( YES_INDIC ) THEN 
                  DO S = 1, N_INDIC
                    INDICVALU( C,R,L,S ) = INDICREG( S )
                  ENDDO ! S 1:n_indic
                  !20140305 outputs indicators' originating variables
                  INDORIG( C,R,L,1 ) = P_H2O2
                  INDORIG( C,R,L,2 ) = P_ROOH
                  INDORIG( C,R,L,3 ) = P_HNO3
                  INDORIG( C,R,L,4 ) = CBFOR( C,R,L, H2O2 )
                  INDORIG( C,R,L,5 ) = CBFOR( C,R,L, HNO3 )
                  INDORIG( C,R,L,6 ) = CBFOR( C,R,L, O3 )
                  INDORIG( C,R,L,7 ) = CBFOR( C,R,L, NO )
                  INDORIG( C,R,L,8 ) = CBFOR( C,R,L, NO2 )
                  INDORIG( C,R,L,9 ) = CBFOR( C,R,L, NO3 )
                  INDORIG( C,R,L,10 ) = CBFOR( C,R,L, N2O5 )
                  INDORIG( C,R,L,11 ) = CBFOR( C,R,L, PAN )
                  INDORIG( C,R,L,12 ) = CBFOR( C,R,L, FORM )
                ENDIF ! yes_indic

              ENDIF !done updating tracers for Production of O3 or Ox

                     
              !  4. Update ISAM tracers for destruction of O3 or Ox

              IF (TROYO3.LT.0.0) THEN 
                DENOMO3 = 0.0
                DO K = 1, NTAG_SA
                  DENOMO3 = DENOMO3 + SA_O3N( K ) + SA_O3V( K )
                ENDDO ! loop K for sum of tags
                DO K = 1, NTAG_SA
                  IF ( CBFOR( C,R,L, SPC_INDEX(JO3V,2) ) .LE. 1.E-20 ) THEN
                    ISAM( C,R,L, JO3N, K ) = 0.0
                    ISAM( C,R,L, JO3V, K ) = 0.0
                  ELSE
                    IF ( DENOMO3 .LE. 1.E-20 ) THEN
                      ISAM( C,R,L, JO3N, K ) = 0.0
                      ISAM( C,R,L, JO3V, K ) = 0.0
                    ELSE
                      ISAM( C,R,L, JO3N, K ) = SA_O3N( K )
     &                     + TROYO3 * SA_O3N( K ) / DENOMO3
                      ISAM( C,R,L, JO3V, K ) = SA_O3V( K )
     &                     + TROYO3 * SA_O3V( K ) / DENOMO3
                      IF ( ISAM( C,R,L,JO3N,K ) .LT. 0.0 ) ISAM( C,R,L,JO3N,K ) = 1.E-30
                      IF ( ISAM( C,R,L,JO3V,K ) .LT. 0.0 ) ISAM( C,R,L,JO3V,K ) = 1.E-30
                    ENDIF ! denomo3 > 0 ?
                  ENDIF ! cbfor > 0 ?
                ENDDO ! loop K

              ENDIF ! done updating for destruction of O3 or Ox

            ENDIF ! jo3v ( and hence jo3n ) > 0

          ENDDO ! C
        ENDDO ! R
      ENDDO ! L

      !Optional writeout of indicator values
      IF ( YES_INDIC ) THEN
        IF ( MOD( JTIME, TSTEP( 1 ) ) .EQ. 0 ) THEN
         DO S = 1, N_INDIC
          IF ( .NOT. WRITE3( O3INDIC_1, VNAM_INDIC(S), JDATE, JTIME,
     &              INDICVALU( :,:,1,S ) ) ) THEN
            XMSG = 'Could not write to ' // VNAM_INDIC(S)
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          ENDIF
         ENDDO ! 1 to n_indic
         IF ( .NOT. WRITE3( O3INDIC_1, 'VOTE_NOX', JDATE, JTIME,
     &              INDICVALU( :,:,1,N_INDIC+1 ) ) ) THEN
           XMSG = 'Could not write to ' // VNAME3D( N_INDIC + 1 )
           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         ENDIF
         !20140305 outputs indicators' originating variables
         DO S = 1, N_INDORIG
          IF ( .NOT. WRITE3( O3INDIC_1, VNAM_INDORIG(S), JDATE, JTIME,
     &              INDORIG( :,:,1,S ) ) ) THEN
            XMSG = 'Could not write to ' // VNAM_INDORIG(S)
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          ENDIF
         ENDDO ! 1 to n_indic
        ENDIF ! hour loop
      ENDIF ! yes_indic

! 20140309 Reinitialize the integrated rates
      INTRXN = 0.0


      END 

