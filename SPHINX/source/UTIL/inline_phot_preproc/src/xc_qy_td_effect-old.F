      SUBROUTINE XC_QY_TD_EFFECT(WC,NW,TLEV,DENS,NZ,
     &             JLABEL,XC,QY,SQ,REPLACE)

      USE JPROC_PHOT_DATA

      IMPLICIT NONE

! subroutine computes the product of the cross-section and
! quantum yield over the atmospheric levels 
! includes temperature and pressure effect for select rates


! input

      INTEGER,       INTENT( IN )    ::  nw
      REAL,          INTENT( IN )    ::  wc(:)
      INTEGER,       INTENT( IN )    ::  nz
      REAL,          INTENT( IN )    ::  tlev(:)  ! air temperature over model levels, deg K
      REAL,          INTENT( IN )    ::  dens(:)  ! air number density over level, 1/cm3
      CHARACTER(16), INTENT( IN )    ::  jlabel   ! name of photolysis rate
      REAL,          INTENT( INOUT ) ::  xc(:,:)  ! cross-section from file
      REAL,          INTENT( INOUT ) ::  qy(:,:)  ! quantum yield from file
      REAL,          INTENT( OUT )   ::  sq(:,:)  ! cross-section times quantum yield over model levels
      LOGICAL,       INTENT( OUT )   ::  REPLACE  ! flag to use sq values in calling routine 

! input/output:
      INTEGER I, J, N

! local:

      INTEGER IW, IJ, IZ

      LOGICAL, SAVE :: FIRSTCALL = .TRUE.

! output quantum yields

      LOGICAL EXISTS

      REAL PRESSURE
      REAL TDUM, QDUM, WDUM



! local
      REAL, EXTERNAL  :: OZONE_YIELD
      REAL, EXTERNAL  :: QY_ACETONE
      REAL, EXTERNAL  :: QY_ACETONE_TUV

      REAL NO2_XCROSS(KW,KZ), NO2_QUANT(KW,KZ)
      REAL O3_XCROSS(KW,KZ),O3_QUANT(KW,KZ)
      REAL HCHO_XCROSS(KW,KZ),HCHO_QUANTR(KW,KZ),HCHO_QUANTM(KW,KZ)
      REAL CLONO2_XCROSS(KW,KZ)
      REAL QYNO3_NO2(KW,KZ),QYNO3_NO(KW,KZ)
      REAL SIG, ALPHA, BETA, CHI

      INTERFACE
        SUBROUTINE WVBIN_AVERAGE(WL_CS_IN, CS_IN, NWL_CS_IN,  
     &                         WL_QY_IN, QY_IN, NWL_QY_IN,  
     &                         SPECTRA_TYPE,
     &                         WLL_AVE, WLU_AVE, NWL_AVE, 
     &                         CS_AVE, QY_AVE )
          USE JPROC_PROFILE
          USE BIN_DATA
          IMPLICIT NONE      
          CHARACTER(1), INTENT( IN ) :: SPECTRA_TYPE        ! spectra type
          INTEGER, INTENT( IN )      :: NWL_AVE             ! number of intervals average 
          INTEGER, INTENT( IN )      :: NWL_CS_IN           ! number of intervals CS_IN
          INTEGER, INTENT( IN )      :: NWL_QY_IN           ! number of intervals CS_IN
          REAL, INTENT( IN )         :: WL_CS_IN( : )  ! wl for CS_IN
          REAL, INTENT( IN )         :: WL_QY_IN( : )  ! wl for QY_IN
          REAL, INTENT( IN )         :: CS_IN( : )     ! cross-section as f(WLIN)
          REAL, INTENT( IN )         :: QY_IN( : )     ! quantum yield as f(WLIN)
          REAL, INTENT( OUT)         :: WLL_AVE( : )   ! lower limit on wl effective interval
          REAL, INTENT( OUT)         :: WLU_AVE( : )   ! upper limit on wl effective interval
          REAL, INTENT( OUT)         :: CS_AVE( : )    ! cross-section as f(WL_AVE)
          REAL, INTENT( OUT)         :: QY_AVE( : )    ! quantum yield as f(WL_AVE)
        END SUBROUTINE WVBIN_AVERAGE
      END INTERFACE  
!_______________________________________________________________________

! complete wavelength grid



      IF(FIRSTCALL)THEN

    
        CALL INIT_JPROC_DATA()

         DO 5 iw = 1, nw
C            print*,'wc(',iw,') = ',wc(iw)
5        CONTINUE
          
         PRINT*,'INITIALIZED XC_QY_TD_EFFECT '
!         pause

      ENDIF

! computing data used for multiple rates

      DO IW = 1, NW
         DO IZ = 1, NZ

           IF( TLEV(IZ) .LT. 293.0 .AND. TLEV(IZ) .GT. 218.0)THEN
             O3_XCROSS(IW,IZ) = (O3_XCROSS_293K(IW)-O3_XCROSS_218K(IW))
     &                        /  75.0
     &                        * (TLEV(IZ) - 218.0)
     &                        + O3_XCROSS_218K(IW)
           ELSEIF( TLEV(IZ) .LE. 218.0)THEN
             O3_XCROSS(IW,IZ) = O3_XCROSS_218K(IW)
           ELSEIF( TLEV(IZ) .GE. 293.0)THEN
             O3_XCROSS(IW,IZ) = O3_XCROSS_293K(IW)
           ENDIF

           O3_XCROSS(IW,IZ) = O3_XCROSS(IW,IZ)
           O3_QUANT( IW,IZ) = OZONE_YIELD(WC(IW),TLEV(IZ)) 

           TDUM = TLEV(IZ)-296.0
  
           CLONO2_XCROSS(IW,IZ) = CLONO2_XCROSS0(IW)
     &                          * ( 1.0
     &                          +  CLONO2_A1(IW)*TDUM 
     &                          +  CLONO2_A2(IW)*TDUM**2 )


            NO2_XCROSS(IW,IZ) = NO2_XCROSS_294K(IW)
             IF(TLEV(IZ) .GT. 220.0 .AND. TLEV(IZ) .LT. 294.0)THEN
               TDUM  = (NO2_XCROSS_294K(IW)-NO2_XCROSS_220K(IW))
     &               * (TLEV(IZ)-220.0)/74.0
                NO2_XCROSS(IW,IZ) =  NO2_XCROSS_220K(IW)
     &                           +  TDUM   
            ELSEIF(TLEV(IZ) .LE. 220.0)THEN
                NO2_XCROSS(IW,IZ) =  NO2_XCROSS_220K(IW)
             ENDIF

            NO2_QUANT(IW,IZ) = NO2_QUANT_298K(IW)
             IF(TLEV(IZ) .GT. 248.0 .AND. TLEV(IZ) .LT. 294.0)THEN
               TDUM  = (NO2_QUANT_298K(IW)-NO2_QUANT_248K(IW))
     &               * (TLEV(IZ)-248.0)/50.0
                NO2_QUANT(IW,IZ) =  NO2_QUANT_248K(IW)
     &                           +  TDUM   
            ELSEIF(TLEV(IZ) .LE. 248.0)THEN
                NO2_QUANT(IW,IZ) =  NO2_QUANT_248K(IW)
             ENDIF
            NO2_QUANT(IW,IZ) = MIN(MAX(NO2_QUANT(IW,IZ), 0.0), 1.0)

        ENDDO
      ENDDO


      CALL JHCHO_NASA_2006(NW,WC,NZ,TLEV,DENS, HCHO_XCROSS, 
     &                     HCHO_QUANTR, HCHO_QUANTM)


      CALL NASA_NO3_QUANTAS(NW,WC,NZ,TLEV,DENS,QYNO3_NO,
     &                            QYNO3_NO2)


      SQ = 0.0

      PRINT*,'ENTERING CASE SELECT FOR XC_QY_TD_EFFECT ', JLABEL

      SELECT CASE( JLABEL ) 
        CASE( 'IC3ONO2' )

            DO IW = 1, NW
               DO IZ = 1, NZ

                 IF((WC(IW) .GE. 240.) .AND. (WC(IW) .LE. 340.))THEN

                  IF( TLEV(IZ) .LT. 360.0 .AND. TLEV(IZ) .GT. 233.0)THEN
                     SIG = IC3ONO2_XCROSS_298K(IW)
     &                   * EXP(IC3ONO2_XCROSS_EXP(IW)*(TLEV(IZ)-298.0))
                   ELSEIF( TLEV(IZ) .LE. 240.0)THEN
                     SIG = IC3ONO2_XCROSS_298K(IW)
     &                   * EXP(IC3ONO2_XCROSS_EXP(IW)*(-58.0))
                   ELSEIF( TLEV(IZ) .GE. 360.)THEN
                     SIG = IC3ONO2_XCROSS_298K(IW)
     &                   * EXP(IC3ONO2_XCROSS_EXP(IW)*(62.0))
                   ENDIF
                 ELSE
                     SIG = IC3ONO2_XCROSS_298K(IW)
                 ENDIF

                  SQ(IZ, IW)  = SIG*QY(IZ,IW)
                  XC(IZ, IW)  = SIG
 
               ENDDO
             ENDDO

             REPLACE = .TRUE.

         CASE( 'NO2-06'   )    ! 'NO2 -> NO + O(3P)'

            DO iw = 1, nw
               DO i = 1, nz
                  sq(i, iw)  = NO2_XCROSS(iw,i)*NO2_QUANT(iw,i)
                  xc(i, iw)  = NO2_XCROSS(iw,i)
                  qy(i, iw)  = NO2_QUANT(iw,i)
               ENDDO
             ENDDO

             REPLACE = .TRUE.

         CASE( 'N2O5_IUPAC04' )

           DO iw = 1, nw
               DO i = 1, nz

                 tdum   = MAX(195.0, MIN(tlev(i), 300.0))
                 alpha  = N2O5_XCROSS_EXP(iw)*(1.0/tdum - 1.0/298.0)
                  sig   = N2O5_XCROSS_298K(iw)
     &                  * EXP( alpha )

                 sq(i,iw) = sig*QY(i,iw)
                 xc(i,iw) = sig

               ENDDO
             ENDDO

             REPLACE = .TRUE.

         CASE(  'NO2EX'   )    ! 'NO2 -> NO2(excited)'

            DO iw = 1, nw
               DO i = 1, nz

                  sq(i, iw) = NO2_XCROSS(iw,i)
     &                      * (1.0 - NO2_QUANT(iw,i))
                  xc(i, iw)  = NO2_XCROSS(iw,i)
                  qy(i, iw)  = 1.0 - NO2_QUANT(iw,i)

               ENDDO
             ENDDO

             REPLACE = .TRUE.

         CASE(   'HNO4-06' )    ! 'HNO4 -> HO2 + NO2'

            DO iw = 1, nw
               DO i = 1, nz
                 qdum = 1.0
                 qy(i, iw) = qdum
                 if(HO2NO2_XCROSS_A1(iw).gt. 0.0
     &              .and. HO2NO2_XCROSS_A2(iw) .gt. 0.0)then
                     tdum   = 1.0+EXP(-988.0/(0.69*tlev(i)))
                     sq(i, iw)  = (HO2NO2_XCROSS_A1(iw)/tdum
     &                          +  HO2NO2_XCROSS_A2(iw)*(1.0-1.0/tdum))
     &                          *  qdum
                      xc(i, iw) = (HO2NO2_XCROSS_A1(iw)/tdum
     &                          +  HO2NO2_XCROSS_A2(iw)*(1.0-1.0/tdum))
                 else
                     sq(i, iw)  = HO2NO2_XCROSS_296K(iw)*qdum
                     xc(i, iw)  = HO2NO2_XCROSS_296K(iw)
                 endif
               ENDDO
             ENDDO

             REPLACE = .TRUE.

         CASE(  'NO3NO-06') ! 'NO3 -> NO + O2'

      

           DO iw = 1, nw
               DO i = 1, nz
                  tdum = (1.0-exp(-1096.4/tlev(i))
     &                 -  2.0*exp(-529.5/tlev(i)))
     &                 / (1.0-exp(-1096.4/298.0)
     &                 -  2.0*exp(-529.5/298.0))
                   sq(i, iw)  = NO3_XCROSS_06(iw)*tdum*QYNO3_NO(iw,i)
                   xc(i, iw)  = NO3_XCROSS_06(iw)*tdum
                   qy(i, iw)  = QYNO3_NO(iw,i)
                   qy(i, iw) = MIN(1.0, qy(i, iw))
                   qy(i, iw) = MAX(0.0, qy(i, iw))
               ENDDO
           ENDDO


           REPLACE = .TRUE.

         CASE(  'CLONO2-2' )    ! 'ClONO2 -> Cl + NO3'

            DO iw = 1, nw
               DO iz = 1, nz
                  sq(iz, iw)  = CLONO2_XCROSS(iw,iz)*qy(iz,iw)
                  xc(iz, iw)  = CLONO2_XCROSS(iw,iz)
               ENDDO
            ENDDO

            REPLACE = .TRUE.

         CASE( 'CLONO2-1' )     ! 'ClONO2 -> ClO + NO2'

            DO iw = 1, nw
               DO iz = 1, nz
                  sq(iz, iw)  = CLONO2_XCROSS(iw,iz)*qy(iz,iw)
                  xc(iz, iw)  = CLONO2_XCROSS(iw,iz)
               ENDDO
            ENDDO


             REPLACE = .TRUE.

         CASE( 'CCHO_R'  )      ! 'CH3CHO -> CH3 + HCO'

           DO iw = 1, nw
               DO i = 1, nz
                   qdum =  qy(i,iw) 
     &                  * (1. + CCHO_YIELD_COEFF(iw))
     &                  / (1. + CCHO_YIELD_COEFF(iw)*dens(i)/2.465E19)
                   sq(i, iw)  = xc(i,iw)*qdum
                   qy(i, iw)  = qdum
                   qy(i, iw) = MIN(1.0, qy(i, iw))
                   qy(i, iw) = MAX(0.0, qy(i, iw))

               ENDDO
           ENDDO

           REPLACE = .TRUE.


         CASE(  'PAN'  )  ! 'PAN + hv -> PRODUCTS'

           qdum = 1.0

           DO iw = 1, nw
               DO i = 1, nz
                    sig = PAN_XCROSS(iw) 
     &                  * EXP(PAN_XCROSS_B(iw)*(tlev(i)-298.0))
                    sq(i, iw)  = sig*qdum
                    xc(i, iw)  = sig
               ENDDO
           ENDDO


           REPLACE = .TRUE.

         CASE( 'C2CHO' )  ! 'C2H5CHO -> C2H5 + HCO'

           DO iw = 1, nw
               DO i = 1, nz
                  IF (qy(i,iw) .LT. 1.0E-5) THEN
                      qdum = 0.0
                  ELSE
                      qdum =  1.0
     &                     / (1.0 + (1.0/qy(i,iw) - 1.0)
     &                     *  dens(i)/2.465e19)
                  ENDIF
                  qdum = MIN(qdum,1.0)
                  sq(i, iw)  = xc(i,iw)*qdum
                  qy(i, iw)  = qdum
                  qy(i, iw) = MIN(1.0, qy(i, iw))
                  qy(i, iw) = MAX(0.0, qy(i, iw))

               ENDDO
           ENDDO


           REPLACE = .TRUE.

         CASE(  'NO3NO2-6') ! 'NO3 -> NO2 + O(3P)'

           DO iw = 1, nw
               DO i = 1, nz
                  tdum = (1.0-exp(-1096.4/tlev(i))
     &                 -  2.0*exp(-529.5/tlev(i)))
     &                 / (1.0-exp(-1096.4/298.0)
     &                 -  2.0*exp(-529.5/298.0))
                  sq(i, iw)  = NO3_XCROSS_06(iw)*tdum*QYNO3_NO2(iw,i)
                  xc(i, iw)  = NO3_XCROSS_06(iw)*tdum
                  qy(i, iw)  = QYNO3_NO2(iw,i)
                  qy(i, iw) = MIN(1.0, qy(i, iw))
                  qy(i, iw) = MAX(0.0, qy(i, iw))
               ENDDO
           ENDDO

           REPLACE = .TRUE.

         CASE( 'HNO3' )

            DO iw = 1, nw
               DO iz = 1, nz

C                 IF((wc(iw) .GT. 192.0) .AND. (wc(iw) .LT. 350.))THEN

                     sig = HNO3_XCROSS_298K(iw)
     &                   * EXP(HNO3_XCROSS_EXP(iw)*(tlev(iz)-298.0))

C                 ELSE

C                     sig = xc(i,iw)

C                 ENDIF

C assume quantum yield equal to one 

                  sq(iz, iw)  = sig*qy(iz,iw)
                  xc(iz, iw)  = sig

               ENDDO
             ENDDO


             REPLACE = .TRUE.

         CASE( 'MVK-06')

C quantum yield from
C Gierczak, T., J. B. Burkholder, R. K. Talukdar, A. Mellouki, S. B. Barone,
C and A. R. Ravishankara, Atmospheric fate of methyl vinyl ketone and methacrolein,
C J. Photochem. Photobiol A: Chemistry, 110 1-10, 1997.
C depends on pressure and wavelength, set upper limit to 1.0
C However, chamber evaluations for SAPRC07T require a pressure correction where
C number density coefficient is five times higher.
         DO iw = 1, nw
            DO i = 1, nz

               qdum = exp(-0.055*(wc(iw)-308.)) / 
     &               (5.5 + 5.0*9.2e-19*dens(i))
               qdum = min(qdum, 1.0)
               sq(i, iw)  = xc(i,iw)* qdum
               qy(i, iw)  = qdum
               qy(i, iw) = MIN(1.0, qy(i, iw))
               qy(i, iw) = MAX(0.0, qy(i, iw))

            ENDDO
         ENDDO


         REPLACE = .TRUE.

       CASE( 'MACR-06')

C quantum yield based on 2.76 times MVK from
C Gierczak, T., J. B. Burkholder, R. K. Talukdar, A. Mellouki, S. B. Barone,
C and A. R. Ravishankara, Atmospheric fate of methyl vinyl ketone and methacrolein,
C J. Photochem. Photobiol A: Chemistry, 110 1-10, 1997.
C depends on pressure and wavelength, set upper limit to 1.0
C However, chamber evaluations for SAPRC07T require a pressure correction where
C number density coefficient is five times higher.
         DO iw = 1, nw
            DO i = 1, nz
               qdum = 2.76*exp(-0.055*(wc(iw)-308.)) / 
     &               (5.5 + 5.0*9.2e-19*dens(i))
               qdum = min(qdum, 1.0)
               sq(i, iw)  = xc(i,iw)* qdum
               qy(i, iw)  = qdum
               qy(i, iw) = MIN(1.0, qy(i, iw))
               qy(i, iw) = MAX(0.0, qy(i, iw))

            ENDDO
         ENDDO


         REPLACE = .TRUE.

       CASE(  'MEK-06')
C Quantum Yields from 
C Raber, W.H. (1992) PhD Thesis, Johannes Gutenberg-Universitaet, Mainz, Germany.
C other channels assumed negligible (less than 10%).
C Total quantum yield  = 0.38 at 760 Torr. but Carter
C adjusts to 0.175 based on chamber tests and sets the values in
C mechanism definition file.
C NOTE: SAPRC07T includes this quantum yield in the mechanism definition
C       file as factor times the photolysis rates where the quantum yield is 
C       set one.

C Stern-Volmer form given:  1/phi = 0.96 + 2.22e-3*P(torr)
C     compute local pressure in torr
         DO iw = 1, nw
            DO i = 1, nz
!              ptorr = (760.*dens(i)/2.69e19)
!              ptorr = (1.03547E-19*dens(i)*tlev(i))
               PRESSURE = (1.03547E-19*dens(i)*tlev(i)) ! torr 
!               sig = 1.0 !        (0.96 + 2.22E-3*760.0)
!     &             / (0.96 + 2.22E-3*(1.03547E-19*dens(i)*tlev(i)))
   
               IF( PRESSURE  .LT. 181.0 )THEN 
                 qdum = 1.0
                 sig  =  2.645
     &                / (0.96 + 2.22E-3*(181.0))
               ELSE
                 sig  =  2.645
     &                / (0.96 + 2.22E-3*PRESSURE)
                 qdum = 1.0
     &                / (0.96 + 2.22E-3*PRESSURE)
               ENDIF
C               sig = MIN(sig, 1.0)/2.649078
               sq(i, iw)  = xc(i,iw)* sig
               qy(i, iw)  = sig
               qy(i, iw) = MIN(1.0, qy(i, iw))
               qy(i, iw) = MAX(0.0, qy(i, iw))

            ENDDO
         ENDDO

         REPLACE = .TRUE.

      CASE( 'H2O2')

*=  Provide cross section and quantum yield for H2O2 photolysis              =*
*=         H2O2 + hv -> 2 OH  between 260 and 350 nm                                               =*
*=  Otherwise use Cross section from JPL97, tabulated values @ 298K 
*=  Quantum yield:  Assumed to be unity                                      =*

            DO i = 1, nz
               DO iw = 1, nw
                  qdum = qy(i,iw)
 
                  IF(wc(iw) .GE. 260.0 .AND. wc(iw) .LT. 350.0) THEN
                     CALL JH2O2_260t350nm(wc(iw),tlev(i),dens(i),
     &                              sig,qdum)
                     xc(i, iw)  = sig
                  ELSE
                     sig  = xc(i,iw)
                  ENDIF
                  sq(i, iw)  = sig*qdum


                  IF(WC(IW) .GE. 260.0 .AND. WC(IW) .LE. 350.0)THEN 
                     CHI =  1.0
     &                   / (1.0 + EXP(-1265.0/MAX(200.0, MIN(TLEV(I),400.0))) )
                     SIG = CHI*H2O2_XCROSS_A(IW)
     &                   + (1.0 - CHI)*H2O2_XCROSS_B(IW)

                     XC(I, IW) = SIG

                  ELSE

                     SIG = XC(I, IW)

                  ENDIF

                  SQ(I, IW)  = XC(I, IW)*QDUM

               ENDDO
            ENDDO

             REPLACE = .TRUE.

      CASE( 'MGLY-06' , 'BACL-07')

         DO iw = 1, nw
C              qy(i,iw) = MIN( qy(i,iw), 1.0)
C              qy(i,iw) = MAX( qy(i,iw), 0.0)

            DO i = 1, nz

               PRESSURE  = (1.03547E-19*dens(i)*tlev(i)) ! torr 
               PRESSURE  = MIN(472.0, PRESSURE)
               qy(i,iw) = MIN( qy(i,iw), 1.0)
               qy(i,iw) = MAX( qy(i,iw), 0.0)

C  Pressure dependence based on Koch and Moortgat (1998), 
C  J. Phys. Chem. A, vol 102, pages 9142. The application contradicts
C  NASA (2006) & IUPAC (2005) and is used based recommendations for
C  SAPRC07T photolysis rates by William Carter (2009)

               IF(wc(iw) .LT. 500.0 .AND. wc(iw) .GT. 240.0)THEN
                 IF( qy(i,iw) .GT. 0.0 .AND. qy(i,iw) .LT. 1.0)THEN
                      qdum = 1.36e8*(472.0)*EXP(-8793/wc(iw))
     &                     / ( 1.0/qy(i,iw) - 1.0 )
                      sig  = qdum
     &                     /(qdum+1.36e8*EXP(-8793/wc(iw))*PRESSURE)
                 ENDIF
               ELSEIF(wc(iw) .LE. 240.0)THEN
                   sig = qy(i,iw)
               ELSEIF(wc(iw) .GE. 500.0)THEN
                   sig = 0.0
               ENDIF

C               sig = qy(i,iw)
               sq(i, iw)  = xc(i,iw)*sig                           
               qy(i, iw)  = sig
               qy(i, iw) = MIN(1.0, qy(i, iw))
               qy(i, iw) = MAX(0.0, qy(i, iw))

            ENDDO
         ENDDO


         REPLACE = .TRUE.

      CASE(  'ACRO-09')

         DO iw = 1, nw

            DO i = 1, nz

               qy(i,iw) = MIN( qy(i,iw), 1.0)
               qy(i,iw) = MAX( qy(i,iw), 0.0)

C  Number density dependence based on Gardner et. al (1997), 
C  J. Phys. Chem., vol 91, pages 1922. The application uses
C  the quantum yields set in in cross-section file. For 
C  SAPRC07T, yields set approximation four times NASA (2006)
C  because the mechanism developer sums over all possible channels and
C  Gardner et. al may support this conclusion. 

               IF(dens(i) .ge. 8.0e+17)THEN
                 qdum = (4.0E-3+1.0/(8.6E-2+1.613E-17*dens(i)))
     &                /  0.006384
               ELSEIF(dens(i) .lt. 8.0e+17)THEN
                 qdum = 12.00713
               ENDIF

               sig  = qy(i,iw)*qdum

               sq(i, iw)  = xc(i,iw)*sig                           
               qy(i, iw)  = sig
               qy(i, iw) = MIN(1.0, qy(i, iw))
               qy(i, iw) = MAX(0.0, qy(i, iw))

             ENDDO
         ENDDO

         REPLACE = .TRUE.

      CASE(  'HCHOR-06')  ! 'CH2O -> H + HCO' 



          DO iw = 1, nw
             DO iz = 1, nz

               sig = HCHO_XCROSS_300K(iw) 
               if(tlev(iz) .lt. 300.0 .and. tlev(iz) .gt. 195.0)then
                   sig = sig + HCHO_XCROSS_A(iw)*(tdum-300.0)
               elseif( tlev(iz) .le. 195.0)then
                   sig = sig - HCHO_XCROSS_A(iw)*105.0
               endif
               sq(iz, iw)  = sig*HCHO_QUANTR_STP(iw)
               xc(iz, iw)  = HCHO_XCROSS(iw,iz)
               qy(iz, iw)  = HCHO_QUANTR_STP(iw)
           ENDDO
          ENDDO


          REPLACE = .TRUE.

      CASE(  'HCHOM-06')  ! 'CH2O -> H2 + CO'

          DO iw = 1, nw
             DO iz = 1, nz
               sig = HCHO_XCROSS_300K(iw) 
               if(tlev(iz) .lt. 300.0 .and. tlev(iz) .gt. 195.0)then
                   sig = sig + HCHO_XCROSS_A(iw)*(tdum-300.0)
               elseif( tlev(iz) .le. 195.0)then
                   sig = sig - HCHO_XCROSS_A(iw)*105.0
               endif
               sq(iz, iw)  = HCHO_XCROSS(iw,iz)*HCHO_QUANTM(iw,iz)
               xc(iz, iw)  = HCHO_XCROSS(iw,iz)
               qy(iz, iw)  = HCHO_QUANTM(iw,iz)

               if(WC(IW) .GE. 330.0 .AND. HCHO_QUANTM_STP(IW) .GT. 0.0)THEN

                  QDUM = 1.0/HCHO_QUANTM_STP(IW)
                  BETA = 1.0/(1.0-HCHO_QUANTR_STP(IW))

                  if( tlev(iz) .lt. 300.0 .and. tlev(iz) .gt. 220.0)then
                      PRESSURE = 82.06*(DENS(iz)/6.02E+23)*tlev(iz)
                      ALPHA = (QDUM - BETA)
     &                      * (1.+0.05*(WC(IW)-329.0)*((tlev(iz)-80.0)/80.0))                   
                  elseif( tlev(iz) .le. 220.0)then
                      PRESSURE = 3.0E-20*DENS(iz)
                      ALPHA = (QDUM - BETA) 
     &                      * (1.+0.0875*(WC(IW)-329.0))
                  elseif( tlev(iz) .ge. 300.)then
                      PRESSURE = 4.09E-20*DENS(iz)
                      ALPHA = (QDUM - BETA)
     &                      * (1.+0.1375*(WC(IW)-329.0))
                  endif

                  qy(iz, iw)  = 1.0/(BETA + PRESSURE*ALPHA)

               ELSE

                  qy(iz, iw)  = HCHO_QUANTM_STP(iw)

               ENDIF
               qy(iz, iw)  = MIN(1.0, qy(iz, iw))
               qy(iz, iw)  = MAX(0.0, qy(iz, iw))
               sq(iz, iw)  = sig*qy(iz, iw)

           ENDDO
          ENDDO

          REPLACE = .TRUE.

      CASE(  'O3O1D-06') ! 'O3 -> O2 + O(1D)'

          DO iw = 1, nw
             DO iz = 1, nz
               sq(iz, iw)  = O3_XCROSS(iw,iz)*O3_QUANT(iw,iz)
               xc(iz, iw)  = O3_XCROSS(iw,iz)
               qy(iz, iw)  = O3_QUANT(iw,iz)
               print*,iw,O3_XCROSS(iw,iz),O3_QUANT(iw,iz)
           ENDDO
          ENDDO


          REPLACE = .TRUE.

      CASE(  'O3O3P-06') ! 'O3 -> O2 + O(3P)'

          DO iw = 1, nw
             DO iz = 1, nz
               sq(iz, iw)  =  O3_XCROSS(iw,iz)
     &                      * (1.0 - O3_QUANT(iw,iz))
               xc(iz, iw)  = O3_XCROSS(iw,iz)
               qy(iz, iw)  = 1.0 - O3_QUANT(iw,iz)
           ENDDO
          ENDDO


          REPLACE = .TRUE.

      CASE(  'ACET-06' ) ! 'CH3COCH3 -> products'

          DO iw = 1, nw
             DO iz = 1, nz

              sig   = (XC_D_ACETONE(IW)
     &              +  XC_A_ACETONE(IW)*tlev(iz)
     &              +  XC_B_ACETONE(IW)*tlev(iz)**2
     &              +  XC_C_ACETONE(IW)*tlev(iz)**3)
     &              *  XCROSS_ACETONE_298K( IW )

              sig   =  XCROSS_ACETONE_298K( IW )

              qdum = QY_ACETONE(tlev(iz),dens(iz),wc(iw))

              sq(iz, iw)  = sig*qdum
              xc(iz, iw)  = sig
              qy(iz, iw)  = qdum
              qy(iz, iw)  = MIN(1.0, qy(iz, iw))
              qy(iz, iw)  = MAX(0.0, qy(iz, iw))


           ENDDO
          ENDDO

          REPLACE = .TRUE.

       CASE(  'CL2')

! NASA (2006) and IUPAC(2005) recommended cross-section as a function of
! wavelength and temperature taken from
! D. Maric et al. (1993) J. Photochem. Photobiol. A: Chem. 70, 205.

          DO iw = 1, nw
             DO iz = 1, nz

               tdum = tlev(iz)
               if(tlev(iz) .gt. 300.0)then
                  tdum = 300.0
               elseif(tlev(iz) .lt. 195.0)then
                  tdum = 195.0
               else
                  tdum = tlev(iz)
               endif    

               alpha = TANH(470.676/tdum)
               if(wc(iw) .gt. 550.0)then
                  sig = 0.0
               elseif(wc(iw) .lt. 250.0)then
                  sig = 0.0
               else
                  wdum = wc(iw)
                  sig = sqrt(alpha)
     &                * (27.3 *exp(-99.0*alpha*(log(329.5/wdum))**2)
     &                +  0.932*exp(-91.5*alpha*(log(406.5/wdum))**2))
               endif  

! IUPAC (2005) and NASA (2006) recommend quantum yield equal to one when
! cross-section is nonzero

                sq(iz, iw)  = 1.0E-20*sig
                xc(iz, iw)  = 1.0E-20*sig

           ENDDO
         ENDDO


         REPLACE = .TRUE.

      CASE DEFAULT

          DO iw = 1, nw
             DO iz = 1, nz
               sq(iz, iw)  = xc(iz,iw)*qy(iz,iw)
           ENDDO
          ENDDO

          REPLACE = .FALSE.

      END SELECT 

      
C          DO iw = 1, nw
C             DO iz = 1, nz
C               sq(iz, iw)  = xc(iz,iw)*qy(iz,iw)
C             ENDDO
C          ENDDO
      
    
      FIRSTCALL = .FALSE.

****************************************************************

      RETURN
      END
C
      FUNCTION OZONE_YIELD(W, T)
*-----------------------------------------------------------------------------*
*=    taken from Tropospheric Ultraviolet-Visible (TUV) radiation model                 =*
*=    Version 4.5                                                            =*
*=    Sep 2007                                                               =*
*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
* function to calculate the quantum yield O3 + hv -> O(1D) + O2,             =*
* according to:                                                             
* Matsumi, Y., F. J. Comes, G. Hancock, A. Hofzumanhays, A. J. Hynes,
* M. Kawasaki, and A. R. Ravishankara, QUantum yields for production of O(1D)
* in the ultraviolet photolysis of ozone:  Recommendation based on evaluation
* of laboratory data, J. Geophys. Res., 107, 10.1029/2001JD000510, 2002.
*-----------------------------------------------------------------------------*
*= TUV model developed by Sasha Madronich with important contributions from:           =*
*= Chris Fischer, Siri Flocke, Julia Lee-Taylor, Bernhard Meyer,             =*
*= Irina Petropavlovskikh,  Xuexi Tie, and Jun Zen.                          =*
*=              To contact the author, write to:                             =*
*= Sasha Madronich, NCAR/ACD, P.O.Box 3000, Boulder, CO, 80307-3000, USA  or =*
*= send email to:  sasha@ucar.edu  or tuv@acd.ucar.edu                       =*
*-----------------------------------------------------------------------------*
*= This program is free software; you can redistribute it and/or modify      =*
*= it under the terms of the GNU General Public License as published by the  =*
*= Free Software Foundation;  either version 2 of the license, or (at your   =*
*= option) any later version.                                                =*
*= The TUV package is distributed in the hope that it will be useful, but    =*
*= WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTIBI-  =*
*= LITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public     =*
*= License for more details.                                                 =*
*= To obtain a copy of the GNU General Public License, write to:             =*
*= Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.   =*
*-----------------------------------------------------------------------------*
*= Copyright (C) 1994,95,96,97,98,99,2000,01,02,03, 04, 05, 06, 07           =*
*= by the University Corporation for Atmospheric Research                    =*
*-----------------------------------------------------------------------------*
      IMPLICIT NONE

      REAL W           ! wavelength, nm
      REAL T           ! temperature, deg K
      REAL OZONE_YIELD ! dimensionaless

C local variables

      REAL KT
      REAL A(3), X(3), OM(3)
      REAL Q1, Q2 

      DATA A/ 0.8036, 8.9061, 0.1192/
      DATA X/ 304.225, 314.957, 310.737/
      DATA OM/ 5.576, 6.601, 2.187/
      
      OZONE_YIELD = 0.0
      KT = 0.695 * T
      Q1 = 1.0
      Q2 = EXP(-825.518/KT)
      
      IF(W .LE. 305.0) THEN
         OZONE_YIELD = 0.90
      ELSEIF(W .GT. 305.0 .AND. W .LE. 328.0) THEN

         OZONE_YIELD = 0.0765 + 
     &  A(1)*              (Q1/(Q1+Q2))*EXP(-((X(1)-W)/OM(1))**4.0)+ 
     &  A(2)*(T/300.)**2.0*(Q2/(Q1+Q2))*EXP(-((X(2)-W)/OM(2))**2.0)+
     &  A(3)*(T/300.)**1.5             *EXP(-((X(3)-W)/OM(3))**2.0)

      ELSEIF(W .GT. 328.0 .AND. W .LE. 340.0) THEN
         OZONE_YIELD = 0.08
      ELSEIF(W .GT. 340.) THEN
         OZONE_YIELD = 0.0
      ENDIF

      END

*=============================================================================*

      SUBROUTINE JH2O2_260T350NM(WC,TEMP,AIRDEN,XCROSS,QUANT)
*-----------------------------------------------------------------------------*
*=    taken from Tropospheric Ultraviolet-Visible (TUV) radiation model                 =*
*=    Version 4.5                                                            =*
*=    Sep 2007                                                               =*
*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide cross section and quantum yield for H2O2 photolysis              =*
*=         H2O2 + hv -> 2 OH  between 260 and 350 nm                                               =*
*=  Otherwise use Cross section from JPL97, tabulated values @ 298K 
*=  Quantum yield:  Assumed to be unity                                      =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  WC     - REAL, center points of wavelength interval                   (I)=*
*=  TLEV   - REAL, temperature (K) at  altitude level                     (I)=*
*=  AIRDEN - REAL, air density (molec/cc) at altitude level               (I)=*
*=  xcross - cross section (cm^2) for each                               (IO)=*
*=           photolysis reaction defined, at  input wavelength       and     =*
*=           at  defined altitude level                                      =*
*=  quant -  quantum yield for each                                      (IO)=*
*=           photolysis reaction defined, at  input wavelength       and     =*
*=           at  defined altitude level                                      =*
*=  JLABEL - CHARACTER*50, string identifier for each photolysis reaction (L)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*

      IMPLICIT NONE

! input

      INTEGER NW
      REAL WC
      
      INTEGER NZ

      REAL TEMP
      REAL AIRDEN

! weighting functions

      CHARACTER(50) JLABEL
      REAL          XCROSS,QUANT

! local

      REAL YG
      REAL QY
      REAL A0, A1, A2, A3, A4, A5, A6, A7
      REAL B0, B1, B2, B3, B4
      REAL XS
      REAL T
      INTEGER I, IW, N, IDUM
      INTEGER IERR
      REAL LAMBDA
      REAL SUMA, SUMB, CHI

**************** H2O2 photodissociation

! cross section from Lin et al. 1978

      JLABEL = 'H2O2            ' ! 'H2O2 -> 2 OH'
! quantum yield = 1


      QY = 1.0
      XS = 0.0
    
! Parameterization (JPL06)

      A0 = 6.4761E+04            
      A1 = -9.2170972E+02        
      A2 = 4.535649              
      A3 = -4.4589016E-03        
      A4 = -4.035101E-05         
      A5 = 1.6878206E-07
      A6 = -2.652014E-10
      A7 = 1.5534675E-13

      B0 = 6.8123E+03
      B1 = -5.1351E+01
      B2 = 1.1522E-01
      B3 = -3.0493E-05
      B4 = -1.0924E-07

! Range 260-350 nm; 200-400 K

         IF ((WC .GE. 260.) .AND. (WC .LT. 350.)) THEN

           LAMBDA = WC
           SUMA = ((((((A7*LAMBDA + A6)*LAMBDA + A5)*LAMBDA + 
     &                  A4)*LAMBDA +A3)*LAMBDA + A2)*LAMBDA + 
     &                  A1)*LAMBDA + A0
           SUMB = (((B4*LAMBDA + B3)*LAMBDA + B2)*LAMBDA + 
     &               B1)*LAMBDA + B0

!           sumA = 1.5534675E-13*lambda**7.0 - 2.652014E-10*lambda**6.0 
!     &          + 1.6878206E-07*lambda**5.0 - 4.035101E-05*lambda**4.0
!     &          - 4.4589016E-03*lambda**3.0 + 4.535649E+00*lambda**2.0 
!     &          - 9.2170972E+02*lambda      + 6.4761E+04

!           sumB = -1.0924E-07*lambda**4.0 - 3.0493E-05*lambda**3.0 
!     &          +  1.1522E-01*lambda**2.0 - 5.1351E+01*lambda 
!     &          +  6.8123E+03

              T = MIN(MAX(TEMP,200.),400.)            
              CHI = 1./(1.+EXP(-1265./T))
              XS = (CHI * SUMA + (1.-CHI)*SUMB)*1E-21

         ENDIF

         XCROSS = XS
         QUANT  = QY

      RETURN
      END


      SUBROUTINE JHCHO_NASA_2006(NW, WC, NZ, TLEV, AIRDEN, XCROSS, QUANTR, QUANTM)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide cross section  and quantum yields for CH2O photolysis =*
*=        (a) CH2O + hv -> H + HCO                                           =*
*=        (b) CH2O + hv -> H2 + CO                                           =*
*=  Based on recommendations from NASA JPL (2006) 
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRDEN - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*50, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*

      USE JPROC_PHOT_DATA

      IMPLICIT NONE

      INTEGER KDATA
      PARAMETER(KDATA=16000)

! input
      INTEGER NW
      REAL WL(KW), WC(KW)
      
      INTEGER NZ

      REAL TLEV(KZ)
      REAL AIRDEN(KZ)

! weighting functions

      CHARACTER(50) JLABEL(3)
      REAL          XCROSS(KW,KZ)
      REAL          QUANTR(KW,KZ), QUANTM(KW,KZ)

! input/output:

      INTEGER J, IZ, IW

! data arrays

      INTEGER N
      REAL    X(KDATA), Y(KDATA)
      REAL    XL(KDATA), XC(KDATA), XU(KDATA)
      INTEGER N1, N2, N3, N4, N5
      REAL    X1(KDATA), X2(KDATA), X3(KDATA), X4(KDATA), X5(KDATA)
      REAL    Y1(KDATA), Y2(KDATA), Y3(KDATA), Y4(KDATA), Y5(KDATA)

! local

      REAL YG(KW), YG1(KW), YG2(KW), YG3(KW), YG4(KW), YG5(KW)
      REAL A, B, C
      REAL A0, A1, A2, A3, A4, A5, A6, A7
      REAL B0, B1, B2, B3, B4
      REAL QY, QY1, QY2, QY3

      REAL SIGMA, SIG, SLOPE
      REAL XS
      REAL T
      REAL DUM
      INTEGER IDUM

      INTEGER I
      INTEGER IROW, ICOL, IREV
      INTEGER IERR

      INTEGER MOPT1, MOPT2

      CHARACTER(LEN=120) :: FILE_LINE
      LOGICAL EXISTS
      REAL WU(KW)
      REAL PRESSURE
      REAL PHI1, PHI2, PHI20, AK300, AKT
      REAL TDUM

      LOGICAL :: FIRSTCALL  = .TRUE.


!            HCHO photodissociatation

      J = 1
      JLABEL(J) = 'HCHOR-06        ' ! 'CH2O -> H + HCO' 

      J = J+1
      JLABEL(J) = 'HCHOM-06        ' ! 'CH2O -> H2 + CO'

! compute upper limit of wavelength bins
       DO I = 1, NW
          WU(I) = 2.0*WC(I) - WL(I)
       ENDDO


      N = 150

      YG1 = 1.0E-20*YG1
      YG2 = 1.0E-24*YG2

      N = 112
        
      IF( FIRSTCALL )THEN

!        FIRSTCALL  = .false.

  
      
      DO IW = 1, NW
C            write(6,'(i3,1X,f6.2,6(1x,es12.4))')iw,wc(iw), ! sig,qy1,qy2,
C     &         HCHO_XC_300K(iw) ,HCHO_QUANTR_STP(iw),HCHO_QUANTM_STP(iw)

            TDUM = 265.0
            SIG = HCHO_XCROSS_300K(IW) 
            IF(TDUM .LT. 300.0 .AND. TDUM .GT. 195.0)THEN
               SIG = SIG + HCHO_XCROSS_A(IW)*(TDUM-300.0)
            ELSEIF( TLEV(I) .LE. 195.0)THEN
               SIG = SIG - HCHO_XCROSS_A(IW)*105.0
            ENDIF
            
            QY1 = HCHO_QUANTR_STP(IW)
            IF ( (WC(IW) .GE. 330.) .AND. (HCHO_QUANTM_STP(IW) .GT. 0.) ) THEN
               PHI1 = HCHO_QUANTR_STP(IW)
               PHI2 = HCHO_QUANTM_STP(IW)
               PHI20 = 1. - PHI1
               AK300=((1./PHI2)-(1./PHI20)) ! IS DIVIDED BY 1 ATM
               IF( TDUM .LT. 300.0 .AND. TDUM .GT. 220.0)THEN
                   PRESSURE = 82.06*(AIRDEN(I)/6.02E+23)*TDUM
                   AKT = AK300
     &                 * (1.+0.05*(WC(IW)-329.0)*((TDUM-80.0)/80.0))
               ELSEIF( TDUM .LE. 220.0)THEN
                   PRESSURE = 3.0E-20*AIRDEN(I)
                   AKT = AK300
     &                 * (1.+0.0875*(WC(IW)-329.0))
               ELSEIF( TDUM .GE. 300)THEN
                   PRESSURE = 4.09E-20*AIRDEN(I)
                   AKT = AK300
     &                 * (1.+0.1375*(WC(IW)-329.0))
               ENDIF
C               PRINT*,PRESSURE, AIRDEN(I)/2.54E+19
C      PAUSE

               QY2 = 1. / ( (1./PHI20) + PRESSURE*AKT)

            ELSE
               QY2 = HCHO_QUANTM_STP(IW)
            ENDIF
            
            QY2 = MAX(0.0,QY2)
            QY2 = MIN(1.0,QY2)

C           WRITE(6,'(I3,1X,F6.2,6(1X,ES12.4))')IW,WC(IW),SIG,QY1,QY2,
C     &         HCHO_XCROSS_300K(IW),HCHO_QUANTR_STP(IW),
C     &         HCHO_QUANTM_STP(IW)

      ENDDO

      ENDIF

      DO IW = 1, NW
         DO I = 1, NZ
! cross-section correction
            SIG = HCHO_XCROSS_300K(IW) 
            IF(TLEV(I) .LT. 300.0 .AND. TLEV(I) .GT. 195.0)THEN
               SIG = SIG + HCHO_XCROSS_A(IW)*(TLEV(I)-300.0)
            ELSEIF( TLEV(I) .LE. 195.0)THEN
               SIG = SIG - HCHO_XCROSS_A(IW)*105.0
            ENDIF
! corrections to quantum yields
            QY1 = HCHO_QUANTR_STP(IW)
            IF(WC(IW) .GE. 330.0 .AND. HCHO_QUANTM_STP(IW) .GT. 0.0)THEN
               PHI1 = HCHO_QUANTR_STP(IW)
               PHI2 = HCHO_QUANTM_STP(IW)
               PHI20 = 1.0 - PHI1
               AK300=((1./PHI2)-(1./PHI20)) ! IS DIVIDED BY 1 ATM
               IF(TLEV(I) .LT. 300.0 .AND. TLEV(I) .GT. 220.0)THEN
                   PRESSURE = 82.06*(AIRDEN(I)/6.02E+23)*TLEV(I)
                   AKT = AK300
     &                 * (1.+0.05*(WC(IW)-329.0)*((TLEV(I)-80.0)/80.0))
               ELSEIF(TLEV(I) .LE. 220.0)THEN
                   PRESSURE = 3.0E-20*AIRDEN(I)
                   AKT = AK300
     &                 * (1.+0.0875*(WC(IW)-329.0))
               ELSEIF(TLEV(I) .GE. 300.0)THEN
                   PRESSURE = 4.09E-20*AIRDEN(I)
                   AKT = AK300
     &                 * (1.+0.1375*(WC(IW)-329.0))
               ENDIF
               QY2 = 1.0/( 1.0/PHI20 + PRESSURE*AKT )
            ELSE
               QY2 = HCHO_QUANTM_STP(IW)
            ENDIF

            QY2 = MAX(0.0,QY2)
            QY2 = MIN(1.0,QY2)
            XCROSS(IW, I) = SIG
            QUANTR(IW, I) = QY1
            QUANTM(IW, I) = QY2

         ENDDO
      ENDDO


      RETURN
      END

*=============================================================================*

      SUBROUTINE NASA_NO3_QUANTAS(NW,WC,NZ,TLEV,AIRDEN,QYNO3_NO,
     &                            QYNO3_NO2)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide the quantum yield for                                            =*
*=  both channels of NO3 photolysis:                                         =*
*=          (a) NO3 + hv -> NO2 + O(3P)                                      =*
*=          (b) NO3 + hv -> NO + O2                                          =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRDEN - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*50, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*

      USE JPROC_PHOT_DATA

      IMPLICIT NONE

C      INCLUDE 'params'

! input

      INTEGER NW
      REAL WL(KW), WC(KW), WU(KW)
      
      INTEGER NZ

      REAL TLEV(KZ)
      REAL AIRDEN(KZ)

! weighting functions

      CHARACTER*50 JLABEL(3)
      REAL SQ(1,KZ,KW)

! input/output:
      INTEGER J

! data arrays

      INTEGER KDATA
      PARAMETER(KDATA=350)

      REAL X1(KDATA)
      REAL Y1(KDATA),Y2(KDATA),Y3(KDATA)
      REAL Z1(KDATA),Z2(KDATA),Z3(KDATA)
      REAL QY1(KDATA),QY2(KDATA)
      REAL SLOPE

! local

      REAL YG(KW), YG1(KW), YG2(KW)
      REAL TEMP_ADJ(KZ)
      REAL QYNO3_NO2(KW,KZ),QYNO3_NO(KW,KZ)
      REAL QY
      INTEGER IROW, ICOL
      INTEGER I, IW, N, IDUM
      INTEGER IERR
      INTEGER MABS
      CHARACTER(LEN=120) :: FILE_LINE
      LOGICAL            :: EXISTS 
      LOGICAL, SAVE      :: FIRSTCALL = .TRUE.

! for   NO3 ->NO+O2
      J = 0
      J = J + 1
      JLABEL(J) = 'NO3NO-06        ' ! 'NO3 -> NO + O2'
! for  NO3 ->NO2+O
      J = J + 1
      JLABEL(J) = 'NO3NO2-6        ' ! 'NO3 -> NO2 + O(3P)'

      TEMP_ADJ = 1.0
      DO I = 1, NZ
        TEMP_ADJ(I) = (1.0-EXP(-1096.4/TLEV(I))
     &              -  2.0*EXP(-529.5/TLEV(I)))
     &              / (1.0-EXP(-1096.4/298.0)
     &              -  2.0*EXP(-529.5/298.0))
      ENDDO

      DO I = 1, NW
! compute upper limit of wavelength bins
          WU(I) = 2.0*WC(I) - WL(I)
      ENDDO

      DO I = 1, NZ
         DO IW = 1, NW
            QY1(IW) = NO3NO_QUANT_298K(IW)
            QY2(IW) = NO3NO2_QUANT_298K(IW)
             IF(TLEV(I) .LT. 298.0 .AND. TLEV(I) .GE. 230.0)THEN
                SLOPE   = (NO3NO_QUANT_298K(IW)-NO3NO_QUANT_230K(IW))
     &                 /  68.0
               QY1(IW) =  NO3NO_QUANT_230K(IW) + SLOPE*(TLEV(I)-230.0)
                SLOPE   = (NO3NO2_QUANT_298K(IW)-NO3NO2_QUANT_230K(IW))
     &                 /  68.0
               QY2(IW) =  NO3NO2_QUANT_230K(IW) + SLOPE*(TLEV(I)-230.0)
            ELSEIF(TLEV(I) .LT. 230.0 .AND. TLEV(I) .GE. 190.0)THEN
                SLOPE   = (NO3NO_QUANT_230K(IW)-NO3NO_QUANT_190K(IW))
     &                 /  40.0
               QY1(IW) =  NO3NO_QUANT_190K(IW) + SLOPE*(TLEV(I)-190.0)
                SLOPE   = (NO3NO2_QUANT_230K(IW)-NO3NO2_QUANT_190K(IW))
     &                 /  40.0
               QY2(IW) =  NO3NO2_QUANT_190K(IW) + SLOPE*(TLEV(I)-190.0)
            ELSEIF( TLEV(I) .LT. 190)THEN
               QY1(IW)  =  NO3NO_QUANT_190K(IW)
               QY2(IW)  =  NO3NO2_QUANT_190K(IW)
             ENDIF
         ENDDO

         DO IW = 1, NW
            QYNO3_NO(IW,I)  = QY1(IW)*0.001
!???ignor because the band averaging already accounts the below effect????
C             if(wc(iw) .le. WV_NO3_QY(1))then
C               QYNO3_NO(iw,i)  = NO3NO_QUANT_298K(1)*0.001
C              QYNO3_NO2(iw,i) = NO3NO2_QUANT_298K(1)*0.001
C             else
               QYNO3_NO2(IW,I) = QY2(IW)*0.001
C             endif
         ENDDO
      ENDDO
      RETURN
      END
C
      REAL FUNCTION QY_ACETONE(TEMP, DENS_NUMB, LAMBDA)

! Computes acetone quantum yields according to:
! IUPAC (2005) recommendation based on
! Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold, and M. P. Chipperfield 
!       (2004), Pressure and temperature-dependent quantum yields for the 
!       photodissociation of acetone between 279 and 327.5 nm, Geophys. 
!       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.
      IMPLICIT NONE

C inputs
      REAL         TEMP               ! air temperature, K
      REAL         DENS_NUMB          ! air number density, 1/cm^3
      REAL         LAMBDA             ! wavelength, nm
! local
      REAL         A0                 ! 1st coef for qy
      REAL         A1                 ! 2nd coef for qy
      REAL         A2                 ! 3rd coef for qy
      REAL         A3                 ! 4th coef for qy
      REAL         A4                 ! 5th coef for qy
      REAL         A5                 ! 6th coef for qy
      REAL         A6                 ! 7th coef for qy

      REAL         PHI_CO              ! CO branch of IUPAC (2005) acetone QYZ
      REAL         PHI_CH3CO           ! CH3CO branch of IUPAC (2005) acetone QYZ
      REAL         AA                  ! scratch variable for IUPAC (2005) acetone QYZ
      REAL         BB                  ! scratch variable for IUPAC (2005) acetone QYZ
      REAL         CC                  ! scratch variable for IUPAC (2005) acetone QYZ

         IF( LAMBDA .GE. 248.0 .AND. LAMBDA .LE. 349.0)THEN

             AA = 0.350*(TEMP/295.0)**(-1.28)
             BB = 0.068*(TEMP/295.0)**(-2.65)
             A0 = (AA / (1.0 - AA))*exp(BB*(LAMBDA-248.0))
             PHI_CO = 1.0 / (1.0 + A0)

             IF( LAMBDA .LE. 302.0 ) THEN
C 248-302 nm
                 AA = 1.600*1.0E-19 *(TEMP/295.0)**(-2.38)
                 BB =  0.55*1.0E-03 *(TEMP/295.0)**(-3.19)
                 A1 = AA*exp( -BB*((1.0E+07/LAMBDA)-33113.0) )
                 PHI_CH3CO = (1.0 - PHI_CO) / (1.0 + A1*DENS_NUMB)

C 302-349 nm
             ELSE
                 AA = 1.62*1.0E-17 *(TEMP/295.0)**(-10.03)
                 BB = 1.79*1.0E-3 *(TEMP/295.0)**(-1.364)
                 A2 = AA*exp(-BB*((1.0E+07/LAMBDA) - 30488.0))

                 AA = 26.29* (TEMP/295.0)**(-6.59)
                 BB = 5.72 *1.0E-7 *(TEMP/295.0)**(-2.93)
                 CC = (30006.0) *(TEMP/295.0)**(-0.064)
                 A3 = AA*exp(-BB*((1.0E+07/LAMBDA) - CC)**2.0)

                 AA = 1.67*1.0E-15 *(TEMP/295.0)**(-7.25)
                 BB = 2.08*1.0E-3 *(TEMP/295.0)**(-1.16)
                 A4 = AA*exp(-BB *((1.0E+07/LAMBDA) - 30488.0))

                 PHI_CH3CO = (1.0 - PHI_CO)
     &                     * (1.0 + A4*DENS_NUMB + A3) 
     &                     / ( (1.0 + A2*DENS_NUMB + A3)
     &                     *   (1.0 + A4*DENS_NUMB) ) 
             ENDIF


             QY_ACETONE = PHI_CO
     &                  + PHI_CH3CO

         ELSEIF(LAMBDA .LT. 248.0 .AND. LAMBDA .GT. 0.0)THEN ! set QY to 1.0
! based on IUPAC (2005) data sheet

               PHI_CO    = 0.05
               
               PHI_CH3CO = 0.95

               QY_ACETONE = PHI_CO+PHI_CH3CO

         ELSEIF(LAMBDA .GT. 349.0)THEN

               QY_ACETONE = 0.0

        ENDIF

         QY_ACETONE = MAX(0.0,MIN(1.0, QY_ACETONE))


        RETURN
        END

* This file contains subroutines used for calculation of quantum yields for 
* various photoreactions:
*     qyacet - q.y. for acetone, based on Blitz et al. (2004)

********************************************************************************

      REAL FUNCTION QY_ACETONE_TUV(T, M, w)
*-----------------------------------------------------------------------------*
*=    taken from Tropospheric Ultraviolet-Visible (TUV) radiation model      =*
*=    Version 4.6                                                            =*
*-----------------------------------------------------------------------------*
* Compute acetone quantum yields according to the parameterization of:
* Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold, and M. P. Chipperfield 
*       (2004), Pressure and temperature-dependent quantum yields for the 
*       photodissociation of acetone between 279 and 327.5 nm, Geophys. 
*       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.

      IMPLICIT NONE

* input:
* T = temperature, K
* m = air number density, molec. cm-3
* w = wavelength, nm

      REAL w, T, M

* internal:

      REAL a0, a1, a2, a3, a4
      REAL b0, b1, b2, b3, b4
      REAL c3
      REAL cA0, cA1, cA2, cA3, cA4

* output
* fco = quantum yield for product CO
* fac = quantum yield for product CH3CO (acetyl radical)

      REAL fco, fac

*** set out-of-range values:
* use low pressure limits for shorter wavelengths
* set to zero beyound 327.5

      IF(w .LT. 279. .AND. w .GE. 1.0) THEN
         fco = 0.05
         fac = 0.95
         QY_ACETONE_TUV = MAX(0.0,MIN(1.0, fco+fac))
         RETURN
      ENDIF

      IF(w .GT. 327.5 .OR. w .LT. 1.0) THEN
         fco = 0.
         fac = 0.
         QY_ACETONE_TUV = MAX(0.0,MIN(1.0, fco+fac))
         RETURN
      ENDIF

*** CO (carbon monoxide) quantum yields:

      a0 = 0.350 * (T/295.)**(-1.28)
      b0 = 0.068 * (T/295.)**(-2.65)
      cA0 = exp(b0*(w - 248.)) * a0 / (1. - a0)

      fco = 1. / (1 + cA0)

*** CH3CO (acetyl radical) quantum yields:

      IF(w .GE. 279. .AND. w .LT. 302.) THEN

         a1 = 1.600E-19 * (T/295.)**(-2.38)
         b1 = 0.55E-3   * (T/295.)**(-3.19)
         cA1 = a1 * EXP(-b1*((1.e7/w) - 33113.))
 
         fac = (1. - fco) / (1 + cA1 * M)

      ENDIF

      IF(w .GE. 302. .AND. w .LT. 327.5) THEN

         a2 = 1.62E-17 * (T/295.)**(-10.03)
         b2 = 1.79E-3  * (T/295.)**(-1.364)
         cA2 = a2 * EXP(-b2*((1.e7/w) - 30488.))


         a3 = 26.29   * (T/295.)**(-6.59)
         b3 = 5.72E-7 * (T/295.)**(-2.93)
         c3 = 30006   * (T/295.)**(-0.064)
         ca3 = a3 * EXP(-b3*((1.e7/w) - c3)**2)


         a4 = 1.67E-15 * (T/295.)**(-7.25)
         b4 = 2.08E-3  * (T/295.)**(-1.16)
         cA4 = a4 * EXP(-b4*((1.e7/w) - 30488.))

         fac = (1. - fco) * (1. + cA3 + cA4 * M) /
     $        ((1. + cA3 + cA2 * M)*(1. + cA4 * M))

      ENDIF

         QY_ACETONE_TUV = MAX(0.0,MIN(1.0, fco+fac))

      RETURN
      END

********************************************************************************
