      SUBROUTINE XC_QY_TD_EFFECT(wl,wu,nw,tlev,dens,nz,
     &             jlabel,xc,qy,sq)

      USE JPROC_PHOT_DATA

      IMPLICIT NONE

! subroutine computes the product of the cross-section and
! quantum yield over the atmospheric levels 
! includes temperature and pressure effect for select rates


! input

      INTEGER nw
      REAL wl(kw)
      REAL wu(kw)
      
      INTEGER nz
      REAL tlev(kz)    ! air temperature over model levels, deg K
      REAL dens(kz)    ! air number density over level, 1/cm3

      CHARACTER*50 jlabel ! name of photolysis rate
      REAL xc(kw)         ! cross-section from file
      REAL qy(kw)         ! quantum yield from file
      REAL sq(kz,kw)      ! cross-section times quantum yield over model levels

! input/output:
      INTEGER i, j, n

! local:

      INTEGER iw, ij, iz

      LOGICAL, SAVE :: FIRSTCALL = .TRUE.

! output quantum yields

      CHARACTER(LEN= 60) :: cdum
      LOGICAL EXISTS
      REAL wc(kw)
       REAL PRESSURE
      real tdum, qdum, wdum



* local


      REAL, EXTERNAL  :: OZONE_YIELD
      REAL, EXTERNAL  :: QY_ACETONE
      REAL, EXTERNAL  :: QY_ACETONE_TUV

      REAL NO2_XCROSS(kw,kz), NO2_QUANT(kw,kz)
      REAL O3_XCROSS(kw,kz),O3_QUANT(kw,kz)
      REAL HCHO_XCROSS(kw,kz),HCHO_QUANTR(kw,kz),HCHO_QUANTM(kw,kz)
      REAL CLONO2_XCROSS(kw,kz)
       REAL QYNO3_NO2(kw,kz),QYNO3_NO(kw,kz)
      REAL sig, alpha

*_______________________________________________________________________

* complete wavelength grid

      DO 5, iw = 1, nw - 1
         wc(iw) = (wl(iw) + wl(iw+1))/2.
 5    CONTINUE
C define wc(nw) to floating point errors
      wc(nw) = wu(nw)
      

C     print*,'in XC_QY_TS_EFFECT for ', jlabel

      IF(FIRSTCALL)THEN

        CALL INIT_JPROC_DATA(wl,wu,wc,nw)

      ENDIF

! computing data used for multiple rates

      DO iw = 1, nw-1
         DO iz = 1, nz

           if( tlev(iz) .lt. 293.0 .and. tlev(iz) .gt. 218.0)then
             O3_XCROSS(iw,iz) = (O3_XCROSS_293K(iw)-O3_XCROSS_218K(iw))
     &                        /  75.0
     &                        * (tlev(iz) - 218.0)
     &                        + O3_XCROSS_218K(iw)
           elseif( tlev(iz) .le. 218.0)then
             O3_XCROSS(iw,iz) = O3_XCROSS_218K(iw)
           elseif( tlev(iz) .ge. 293.0)then
             O3_XCROSS(iw,iz) = O3_XCROSS_293K(iw)
           endif

           O3_XCROSS(iw,iz) = O3_XCROSS(iw,iz)
           O3_QUANT( iw,iz) = OZONE_YIELD(wc(iw),tlev(iz)) 

C           if(iz .eq. 1)then
C              write(6,2222),TLEV(IZ),' wv = ',wc(iw),' O3_XCROSS = ',
C     &               O3_XCROSS(iw,iz),
C     &             ' O3_QUANT = ',O3_QUANT(iw,iz)
C2222          format('temp = ',f6.1,A,f6.1,5(A,ES12.4),A,ES12.4)
C           endif
           
           tdum = tlev(iz)-296.0
  
           CLONO2_XCROSS(iw,iz) = CLONO2_XCROSS0(iw)
     &                          * ( 1.0
     &                          +  CLONO2_A1(iw)*tdum 
     &                          +  CLONO2_A2(iw)*tdum**2 )

C           if(iz .eq. 1)then
C              print*,TLEV(IZ),' wv = ',wc(iw),' CLONO2_XCROSS = ',
C     &               CLONO2_XCROSS(iw,iz)
C           endif

            NO2_XCROSS(iw,iz) = NO2_XCROSS_294K(iw)
             IF(tlev(iz) .gt. 220.0 .and. tlev(iz) .lt. 294.0)THEN
               tdum  = (NO2_XCROSS_294K(iw)-NO2_XCROSS_220K(iw))
     &               * (tlev(iz)-220.0)/74.0
                NO2_XCROSS(iw,iz) =  NO2_XCROSS_220K(iw)
     &                           +  tdum   
            ELSEIF(tlev(iz) .le. 220.0)THEN
                NO2_XCROSS(iw,iz) =  NO2_XCROSS_220K(iw)
             ENDIF

            NO2_QUANT(iw,iz) = NO2_QUANT_298K(iw)
             IF(tlev(iz) .gt. 248.0 .and. tlev(iz) .lt. 294.0)THEN
               tdum  = (NO2_QUANT_298K(iw)-NO2_QUANT_248K(iw))
     &               * (tlev(iz)-248.0)/50.0
                NO2_QUANT(iw,iz) =  NO2_QUANT_248K(iw)
     &                           +  tdum   
            ELSEIF(tlev(iz) .le. 248.0)THEN
                NO2_QUANT(iw,iz) =  NO2_QUANT_248K(iw)
             ENDIF
            NO2_QUANT(iw,iz) = MIN(MAX(NO2_QUANT(iw,iz), 0.0), 1.0)
C           if(iz .eq. 1)then
C              write(6,2222),TLEV(IZ),' wv = ',wc(iw),' NO2_XCROSS = ',
C     &               NO2_XCROSS(iw,iz),' NO2_QUANT = ',
C     &               NO2_QUANT(iw,iz)
C2222          format('temp = ',f6.1,A,f6.1,A,ES12.4,A,ES12.4)
C           endif


        ENDDO
      ENDDO



      CALL JHCHO_NASA_2006(nw,wl,wc,nz,tlev,dens, HCHO_XCROSS, 
     &                     HCHO_QUANTR, HCHO_QUANTM)

      CALL NASA_NO3_QUANTAS(nw,wl,wc,nz,tlev,dens,QYNO3_NO,
     &                            QYNO3_NO2)


      sq = 0.0

      SELECT CASE( JLABEL ) 
        CASE( 'IC3ONO2' , 'NTR_IUPAC04' )

            DO iw = 1, nw-1
               DO iz = 1, nz


                 IF((wc(iw) .GE. 240.) .AND. (wc(iw) .Le. 340.))THEN

                  if( tlev(iz) .lt. 360.0 .and. tlev(iz) .gt. 233.0)then
                     sig = IC3ONO2_XCROSS_298K(iw)
     &                   * EXP(IC3ONO2_XCROSS_EXP(iw)*(tlev(iz)-298.0))
                   elseif( tlev(iz) .le. 240.0)then
                     sig = IC3ONO2_XCROSS_298K(iw)
     &                   * EXP(IC3ONO2_XCROSS_EXP(iw)*(-58.0))
                   elseif( tlev(iz) .ge. 360.)then
                     sig = IC3ONO2_XCROSS_298K(iw)
     &                   * EXP(IC3ONO2_XCROSS_EXP(iw)*(62.0))
                   endif
                 ELSE
                     sig = IC3ONO2_XCROSS_298K(iw)
                 ENDIF

                  sq(iz, iw)  = sig*qy(iw)
C           if(iz .eq. 1)then
C              write(6,2222),TLEV(IZ),' wv = ',wc(iw),
C     &               ' IC3ONO2_XCROSS = ',sig
C2222          format('temp = ',f6.1,A,f6.1,A,ES12.4,A,ES12.4)
C           endif

               ENDDO
             ENDDO

C       stop
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE( 'NO2-06'   )    ! 'NO2 -> NO + O(3P)'

            DO iw = 1, nw-1
               DO i = 1, nz
                  sq(i, iw)  = NO2_XCROSS(iw,i)*NO2_QUANT(iw,i)
               ENDDO
             ENDDO

C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE( 'N2O5_IUPAC04' )

           DO iw = 1, nw-1
               DO i = 1, nz

                 tdum   = MAX(195.0, MIN(tlev(i), 300.0))
                 alpha  = N2O5_XCROSS_EXP(iw)*(1.0/tdum - 1.0/298.0)
                  sig    = N2O5_XCROSS_298K(iw)
     &                  * EXP( alpha )

                 sq(i,iw) = sig*QY(iw)

               ENDDO
             ENDDO

C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE(  'NO2EX'   )    ! 'NO2 -> NO2(excited)'

            DO iw = 1, nw-1
               DO i = 1, nz

                  sq(i, iw)  = NO2_XCROSS(iw,i)
     &                      * (1.0 - NO2_QUANT(iw,i))

               ENDDO
             ENDDO

C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE(   'HNO4-06', 'HO2NO2_IUPAC04' )    ! 'HNO4 -> HO2 + NO2'

            DO iw = 1, nw-1
               DO i = 1, nz
                 qdum = 1.0
                 if(HO2NO2_XCROSS_A1(iw).gt. 0.0
     &              .and. HO2NO2_XCROSS_A2(iw) .gt. 0.0)then
                     tdum   = 1.0+EXP(-988.0/(0.69*tlev(i)))
                     sq(i, iw)  = (HO2NO2_XCROSS_A1(iw)/tdum
     &                          +  HO2NO2_XCROSS_A2(iw)*(1.0-1.0/tdum))
     &                          *  qdum
                 else
                     sq(i, iw)  = HO2NO2_XCROSS_296K(iw)*qdum
                 endif
               ENDDO
             ENDDO
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel


         CASE(  'NO3NO-06') ! 'NO3 -> NO + O2'

      

           DO iw = 1, nw - 1
               DO i = 1, nz
                  tdum = (1.0-exp(-1096.4/tlev(i))
     &                 -  2.0*exp(-529.5/tlev(i)))
     &                 / (1.0-exp(-1096.4/298.0)
     &                 -  2.0*exp(-529.5/298.0))
                   sq(i, iw)  = xc(iw)*tdum*QYNO3_NO(iw,i)
               ENDDO
           ENDDO

C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE(  'CLONO2-2' )    ! 'ClONO2 -> Cl + NO3'

            DO iw = 1, nw-1
               DO iz = 1, nz
                  sq(iz, iw)  = CLONO2_XCROSS(iw,iz)*qy(iw)
               ENDDO
             ENDDO

C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE( 'CLONO2-1' )     ! 'ClONO2 -> ClO + NO2'

            DO iw = 1, nw-1
               DO iz = 1, nz
                  sq(iz, iw)  = CLONO2_XCROSS(iw,iz)*qy(iw)
               ENDDO
             ENDDO

C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE( 'CCHO_R'  )      ! 'CH3CHO -> CH3 + HCO'

           DO iw = 1, nw - 1
               DO i = 1, nz
                   qdum =  qy(iw) 
     &                  * (1. + CCHO_YIELD_COEFF(iw))
     &                  / (1. + CCHO_YIELD_COEFF(iw)*dens(i)/2.465E19)
                   sq(i, iw)  = xc(iw)*qdum
C            if(i.eq.1)print*,dens(i),tlev(i),wl(iw),qy(iw),qdum

               ENDDO
           ENDDO
C         STOP
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE(  'PAN', 'PAN_IUPAC04'  )  ! 'PAN + hv -> PRODUCTS'

           qdum = 1.0

           DO iw = 1, nw - 1
               DO i = 1, nz
                    sig = PAN_XCROSS(iw) 
     &                  * EXP(PAN_XCROSS_B(iw)*(tlev(i)-298.0))
                    sq(i, iw)  = sig*qdum

C            if(i.eq.1)print*,dens(i),tlev(i),wl(iw),xc(iw),sig
C           if(i .eq. 1)then
C              write(6,2222),TLEV(I),' wv = ',wc(iw),
C     &               ' PAN_XCROSS = ',sig,
C     &               ' XCROSS_FACTOR = ',
C     &                EXP(PAN_XCROSS_B(iw)*(tlev(i)-298.0))
C2222          format('temp = ',f6.1,A,f6.1,A,ES12.4,A,ES12.4)
C           endif

               ENDDO
           ENDDO
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

C         stop

         CASE( 'C2CHO' )  ! 'C2H5CHO -> C2H5 + HCO'

           DO iw = 1, nw - 1
               DO i = 1, nz
                  IF (qy(iw) .LT. 1.0E-5) THEN
                      qdum = 0.0
                  ELSE
                      qdum =  1.0
     &                     / (1.0 + (1.0/qy(iw) - 1.0)
     &                     *  dens(i)/2.45e19)
                  ENDIF
                  qdum = MIN(qdum,1.0)
                  sq(i, iw)  = xc(iw)*qdum
C            if(i.eq.10)print*,tlev(i),wl(iw),qy(iw),qdum
               ENDDO
           ENDDO
C           STOP
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE(  'NO3NO2-6') ! 'NO3 -> NO2 + O(3P)'

           DO iw = 1, nw - 1
               DO i = 1, nz
                  tdum = (1.0-exp(-1096.4/tlev(i))
     &                 -  2.0*exp(-529.5/tlev(i)))
     &                 / (1.0-exp(-1096.4/298.0)
     &                 -  2.0*exp(-529.5/298.0))
                  sq(i, iw)  = xc(iw)*tdum*QYNO3_NO2(iw,i)
               ENDDO
           ENDDO
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE( 'HNO3', 'HNO3_IUPAC04'  )


            DO iw = 1, nw-1
               DO iz = 1, nz

C                 IF((wc(iw) .GT. 192.0) .AND. (wc(iw) .LT. 350.))THEN

                     sig = HNO3_XCROSS_298K(iw)
     &                   * EXP(HNO3_XCROSS_EXP(iw)*(tlev(iz)-298.0))

C                 ELSE

C                     sig = yg1(iw)

C                 ENDIF

! assumes a quantum yield equal to one 

                  sq(iz, iw)  = sig

C           if(iz .eq. 1)then
C       write(*,'(f6.2,1X,A6,f6.2,A12,ES12.4,A,f6.2)'),TLEV(IZ),
C     &    ' wv = ',wc(iw),' HNO3_XCROSS = ',
C     &     sig,' Interpolated HNO3_QUANT = ',qy(iw)
C           endif


               ENDDO
             ENDDO
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

C        stop

         CASE( 'MVK-06')
* quantum yield from
* Gierczak, T., J. B. Burkholder, R. K. Talukdar, A. Mellouki, S. B. Barone,
* and A. R. Ravishankara, Atmospheric fate of methyl vinyl ketone and methacrolein,
* J. Photochem. Photobiol A: Chemistry, 110 1-10, 1997.
* depends on pressure and wavelength, set upper limit to 1.0
C However, chamber evaluations for SAPRC07T require a pressure correction where
C number density coefficient is five times higher.
         DO iw = 1, nw - 1
            DO i = 1, nz
               sig = exp(-0.055*(wc(iw)-308.)) / 
     $               (5.5 + 5.0*9.2e-19*dens(i))
               sig = min(sig, 1.)
               sq(i, iw)  = xc(iw)* sig
C            if(i.eq.1)print*,wc(iw),exp(-0.055*(wc(iw)-308.)),
C     &            dens(i),(5.5 + 5.0*9.2e-19*dens(i)),
C     &             sig

            ENDDO
         ENDDO
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE( 'MACR-06')

* quantum yield based on 2.76 times MVK from
* Gierczak, T., J. B. Burkholder, R. K. Talukdar, A. Mellouki, S. B. Barone,
* and A. R. Ravishankara, Atmospheric fate of methyl vinyl ketone and methacrolein,
* J. Photochem. Photobiol A: Chemistry, 110 1-10, 1997.
* depends on pressure and wavelength, set upper limit to 1.0
C However, chamber evaluations for SAPRC07T require a pressure correction where
C number density coefficient is five times higher.
         DO iw = 1, nw - 1
            DO i = 1, nz
               sig = 2.76*exp(-0.055*(wc(iw)-308.)) / 
     $               (5.5 + 5.0*9.2e-19*dens(i))
               sig = min(sig, 1.)
               sq(i, iw)  = xc(iw)* sig
C            if(i.eq.1)print*,wc(iw),exp(-0.055*(wc(iw)-308.)),
C     &            dens(i),(5.5 + 5.0*9.2e-19*dens(i)),
C     &             sig

            ENDDO
         ENDDO

C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE(  'MEK-06')
* Quantum Yields from 
* Raber, W.H. (1992) PhD Thesis, Johannes Gutenberg-Universitaet, Mainz, Germany.
* other channels assumed negligible (less than 10%).
* Total quantum yield  = 0.38 at 760 Torr. but Carter
* adjusts to 0.175 based on chamber tests and sets the values in
* mechanism definition file.
* Stern-Volmer form given:  1/phi = 0.96 + 2.22e-3*P(torr)
*     compute local pressure in torr
         DO iw = 1, nw-1

C            print*, jlabel(j),xc(iw),qy(iw)

            DO i = 1, nz
!              ptorr = (760.*dens(i)/2.69e19)
!              ptorr = (1.03547E-19*dens(i)*tlev(i))
               PRESSURE = (1.03547E-19*dens(i)*tlev(i)) ! torr 
!               sig = 1.0 !        (0.96 + 2.22E-3*760.0)
!     &             / (0.96 + 2.22E-3*(1.03547E-19*dens(i)*tlev(i)))
   
               IF( PRESSURE  .lt. 181.0 )THEN 
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
               sq(i, iw)  = xc(iw)* sig
C               print*,sig,2.22E-3*1.03547E-19*dens(i)*tlev(i)+0.96,
C     &                2.22E-3*(760.*dens(i)/2.69e19)+0.96

            ENDDO
         ENDDO
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE( 'H2O2', 'H2O2_SAPRC99' )


         DO i = 1, nz
            DO iw = 1, nw
               IF(wc(iw) .GE. 260.0 .AND. wc(iw) .LT. 350.0) THEN

                 CALL JH2O2_260t350nm(wc(iw),tlev(i),dens(i),
     &                              sig,qdum)
!                 qdum = 1.0

               ELSE
                  sig  = xc(iw)
                  qdum = qy(iw)
               ENDIF

               sq(i, iw)  = sig*qdum
 
!           if(i .eq. 1)then
!              write(6,2222),TLEV(IZ),' wv = ',wc(iw),
!     &               ' H2O2_XCROSS = ',sig,
!     &               ' H2O2_QUANT = ',qdum
!2222          format('temp = ',f6.1,A,f6.1,A,ES12.4,A,ES12.4)
!           endif


            ENDDO
         ENDDO


C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel


         CASE( 'MGLY-06' , 'BACL-07', 'MGLY_IUPAC04' )

C        print*,jlabel
         DO iw = 1, nw-1
C              qy(iw) = MIN( qy(iw), 1.0)
C              qy(iw) = MAX( qy(iw), 0.0)

            DO i = 1, nz

               PRESSURE  = (1.03547E-19*dens(i)*tlev(i)) ! torr 
               PRESSURE  = MIN(472.0, PRESSURE)
               qy(iw) = MIN( qy(iw), 1.0)
               qy(iw) = MAX( qy(iw), 0.0)

C  Pressure dependence based on Koch and Moortgat (1998), 
C  J. Phys. Chem. A, vol 102, pages 9142. The application contradicts
C  NASA (2006) & IUPAC (2005) and is used based recommendations for
C  SAPRC07T photolysis rates by William Carter (2009)

               IF(wc(iw) .LT. 500.0 .AND. wc(iw) .gt. 240.0)THEN
                 IF( qy(iw) .GT. 0.0 .AND. qy(iw) .LT. 1.0)THEN
                      qdum = 1.36e8*(472.0)*EXP(-8793/wc(iw))
     &                     / ( 1.0/qy(iw) - 1.0 )
                      sig  = qdum
     &                     /(qdum+1.36e8*EXP(-8793/wc(iw))*PRESSURE)
                 ENDIF
               ELSEIF(wc(iw) .le. 240.0)THEN
                   sig = qy(iw)
               ELSEIF(wc(iw) .ge. 500.0)THEN
                   sig = 0.0
               ENDIF

C               sig = qy(iw)
               sq(i, iw)  = xc(iw)*sig                           
C       if(wc(iw).lt.290 .and. wc(iw) .gt. 285.)then
C         print*,pressure,wc(iw),xc(iw),qy(iw),
C     &          sig,sq(iz, iw) 
C       endif

            ENDDO
         ENDDO
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

C      stop

         CASE(  'ACRO-09', 'ACROLEIN_SAPRC99')

         DO iw = 1, nw-1

            DO i = 1, nz

               qy(iw) = MIN( qy(iw), 1.0)
               qy(iw) = MAX( qy(iw), 0.0)

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

               sig  = qy(iw)*qdum
C               sig = qy(iw)
               sq(i, iw)  = xc(iw)*sig                           
C        if(i .eq. 1 .and. (wc(iw).lt.290 .and. wc(iw) .gt. 285.))then
C        if(i .eq. 1 )then
C          print*,wc(iw),xc(iw),qy(iw),qdum,
C     &          sig,sq(iz, iw) 
C       endif

            ENDDO
         ENDDO

C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE(  'HCHOR-06', 'HCHO_R_SAPRC99' )  ! 'CH2O -> H + HCO' 

          DO iw = 1, nw-1
             DO iz = 1, nz
               if( jlabel .eq. 'HCHOR-06' )then
                   sq(iz, iw)  = HCHO_XCROSS(iw,iz)*HCHO_QUANTR(iw,iz)
               else
                   sq(iz, iw)  = HCHO_XCROSS(iw,iz)*qy(iw)
               endif
C           if(iz .eq. 1)then
C              write(6,2222),TLEV(IZ),' wv = ',wc(iw),
C     &               ' HCHO_XCROSS = ',HCHO_XCROSS(iw,iz),
C     &               ' HCHOR_QUANT = ',HCHO_QUANTR(iw,iz)
C2222          format('temp = ',f6.1,A,f6.1,A,ES12.4,A,ES12.4)
C           endif
           ENDDO
          ENDDO

C         stop
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE(  'HCHOM-06', 'HCHO_M_SAPRC99' )  ! 'CH2O -> H2 + CO'

          DO iw = 1, nw-1
             DO iz = 1, nz
               if( jlabel .eq. 'HCHOM-06' )then
                   sq(iz, iw)  = HCHO_XCROSS(iw,iz)*HCHO_QUANTM(iw,iz)
               else
                   sq(iz, iw)  = HCHO_XCROSS(iw,iz)*qy(iw)
               endif
           ENDDO
          ENDDO
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE(  'O3O1D-06',  'O3_O1D_IUPAC04') ! 'O3 -> O2 + O(1D)'

          DO iw = 1, nw-1
             DO iz = 1, nz
               sq(iz, iw)  = O3_XCROSS(iw,iz)*O3_QUANT(iw,iz)
           ENDDO
          ENDDO
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE(  'O3O3P-06', 'O3_O3P_IUPAC04' ) ! 'O3 -> O2 + O(3P)'

          DO iw = 1, nw-1
             DO iz = 1, nz
               sq(iz, iw)  =  O3_XCROSS(iw,iz)
     &                      * (1.0 - O3_QUANT(iw,iz))
           ENDDO
          ENDDO
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

         CASE(  'ACET-06' ) ! 'CH3COCH3 -> products'


          DO iw = 1, nw-1
             DO iz = 1, nz

              sig   = (XC_D_ACETONE(IW)
     &              +  XC_A_ACETONE(IW)*tlev(iz)
     &              +  XC_B_ACETONE(IW)*tlev(iz)**2
     &              +  XC_C_ACETONE(IW)*tlev(iz)**3)
     &              *  XCROSS_ACETONE_298K( IW )

              sig   =  XCROSS_ACETONE_298K( IW )

              qdum = QY_ACETONE(tlev(iz),dens(iz),wc(iw))
C              qdum = QY_ACETONE_TUV(tlev(iz),dens(iz),wc(iw))

              sq(iz, iw)  = sig*qdum

C           if(iz .eq. 1)then
C              write(6,2222),TLEV(IZ),' wv = ',wc(iw),
C     &               ' ACETONE_XCROSS = ',sig,
C     &               ' ACETONE_QUANT = ',qdum
C2222          format('temp = ',f6.1,A,f6.1,A,ES12.4,A,ES12.4)
C           endif

           ENDDO
          ENDDO
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

C
         CASE(  'CL2', 'CL2_IUPAC04' )

! NASA (2006) and IUPAC(2005) recommended cross-section as a function of
! wavelength and temperature taken from
! D. Maric et al. (1993) J. Photochem. Photobiol. A: Chem. 70, 205.

          DO iw = 1, nw-1
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

!           if(iz .eq. 1)then
!       write(*,'(f6.2,1X,A6,f6.2,A12,ES12.4,A,f6.2)'),TLEV(IZ),
!     &    ' wv = ',wc(iw),' CL2_XCROSS = ',
!     &     sig,' Interpolated CL2_QUANT = ',qy(iw)
!           endif

           ENDDO
         ENDDO
C      print*,'XC_QY_TD_EFFECT: set sq for ',jlabel

      CASE DEFAULT


C        print*,' using default case for ',jlabel
 
          DO iw = 1, nw-1
             DO iz = 1, nz
               sq(iz, iw)  = xc(iw)*qy(iw)
           ENDDO
          ENDDO

      END SELECT 

      

C          DO iw = 1, nw
C             DO iz = 1, nz
C               sq(iz, iw)  = xc(iw)*qy(iw)
C             ENDDO
C          ENDDO
      
    
      FIRSTCALL = .FALSE.

****************************************************************

C        pause

      RETURN
      END
C
      FUNCTION OZONE_YIELD(w,t)
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

      REAL w           ! wavelength, nm
      REAL t           ! temperature, deg K
      REAL OZONE_YIELD ! dimensionaless

C local variables

      REAL kt
      REAL A(3), X(3), om(3)
      REAL q1, q2 

      DATA A/ 0.8036, 8.9061, 0.1192/
      DATA X/ 304.225, 314.957, 310.737/
      DATA om/ 5.576, 6.601, 2.187/
      
      OZONE_YIELD = 0.
      kt = 0.695 * t
      q1 = 1.
      q2 = exp(-825.518/kt)
      
      IF(w .LE. 305.) THEN
         OZONE_YIELD = 0.90
      ELSEIF(w .GT. 305. .AND. w .LE. 328.) THEN

         OZONE_YIELD = 0.0765 + 
     $  a(1)*             (q1/(q1+q2))*EXP(-((x(1)-w)/om(1))**4)+ 
     $  a(2)*(T/300.)**2 *(q2/(q1+q2))*EXP(-((x(2)-w)/om(2))**2)+
     $  a(3)*(T/300.)**1.5            *EXP(-((x(3)-w)/om(3))**2)

      ELSEIF(w .GT. 328. .AND. w .LE. 340.) THEN
         OZONE_YIELD = 0.08
      ELSEIF(w .GT. 340.) THEN
         OZONE_YIELD = 0.
      ENDIF

      END

*=============================================================================*

      SUBROUTINE JH2O2_260t350nm(wc,temp,airden,xcross,quant)
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

* input

      INTEGER nw
      REAL wc
      
      INTEGER nz

      REAL temp
      REAL airden

* weighting functions

      CHARACTER*50 jlabel
      REAL xcross,quant

* local

      REAL yg
      REAL qy
      REAL a0, a1, a2, a3, a4, a5, a6, a7
      REAL b0, b1, b2, b3, b4
      REAL xs
      REAL t
      INTEGER i, iw, n, idum
      INTEGER ierr
      REAL lambda
      REAL sumA, sumB, chi

**************** H2O2 photodissociation

* cross section from Lin et al. 1978

      jlabel = 'H2O2            ' ! 'H2O2 -> 2 OH'


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

* quantum yield = 1


      qy = 1.0
      xs = 0.0

      
* Parameterization (JPL06)
* Range 260-350 nm; 200-400 K

         IF ((wc .GE. 260.) .AND. (wc .LT. 350.)) THEN

           lambda = wc
           sumA = ((((((A7*lambda + A6)*lambda + A5)*lambda + 
     >                  A4)*lambda +A3)*lambda + A2)*lambda + 
     >                  A1)*lambda + A0
           sumB = (((B4*lambda + B3)*lambda + B2)*lambda + 
     >               B1)*lambda + B0

              t = MIN(MAX(temp,200.),400.)            
              chi = 1./(1.+EXP(-1265./t))
              xs = (chi * sumA + (1.-chi)*sumB)*1E-21

         ENDIF

          xcross = xs
          quant  = qy

      RETURN

      END


      SUBROUTINE JHCHO_NASA_2006(nw,wl,wc,nz,tlev,airden, xcross, 
     &                           quantr, quantm)

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

      INTEGER kdata
      PARAMETER(kdata=16000)

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airden(kz)

* weighting functions

      CHARACTER*50 jlabel(1)
      REAL xcross(kw,kz)
      REAL quantr(kw,kz), quantm(kw,kz)

* input/output:

      INTEGER j, iz, iw

* data arrays

      INTEGER n
      real x(kdata), y(kdata)
      real xl(kdata), xc(kdata), xu(kdata)
      INTEGER n1, n2, n3, n4, n5
      REAL x1(kdata), x2(kdata), x3(kdata), x4(kdata), x5(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata), y4(kdata), y5(kdata)

* local

      REAL yg(kw), yg1(kw), yg2(kw), yg3(kw), yg4(kw), yg5(kw)
      REAL a, b, c
      REAL a0, a1, a2, a3, a4, a5, a6, a7
      REAL b0, b1, b2, b3, b4
      REAL qy, qy1, qy2, qy3

      REAL sigma, sig, slope
      REAL xs
      REAL t
      REAL dum
      INTEGER idum

      INTEGER i
      INTEGER irow, icol, irev
      INTEGER ierr

      INTEGER mopt1, mopt2

       CHARACTER(LEN=120) :: FILE_LINE
      LOGICAL EXISTS
      LOGICAL :: FIRSTCALL  = .TRUE.
      REAL wu(kw)
       REAL PRESSURE
      REAL phi1, phi2, phi20, ak300, akt
      real tdum


*_______________________________________________________________________

      DO 5, iw = 1, nw - 1
         wc(iw) = (wl(iw) + wl(iw+1))/2.
 5    CONTINUE

C            HCHO photodissociatation

      j = 1
      jlabel(j) = 'HCHOR-06        ' ! 'CH2O -> H + HCO' 

      j = j+1
      jlabel(j) = 'HCHOM-06        ' ! 'CH2O -> H2 + CO'

       do i = 1, nw-1
! compute upper limit of wavelength bins
          wu(i) = 2.0*wc(i) - wl(i)
      enddo
       wu(nw)=wu(nw-1)+2*(wc(nw-1) - wl(nw-1))
      
!      INQUIRE(FILE='DATAJ1/JPROC_CSQY/NASA_2006-HO2NO2_photolysis.dat',
!     &        EXIST=exists)


!      IF( .NOT. EXISTS)THEN
!          print*,' NASA_2006-HCHO_photolysis.dat not found '
!       stop
!       else
C          print*,' NASA_2006-HO2NO2_photolysis.dat found '
!       endif

!      OPEN(UNIT=kin,
!     &     FILE='DATAJ1/JPROC_CSQY/NASA_2006-HCHO_photolysis.dat',
!     &     STATUS='old')
          
        
!       print*,'trying to read NASA HCHO data'
!       DO i = 1, 6
!         read(kin,'(a)', err = 3045)FILE_LINE
!         IF(i .lt. 6 .AND. FIRSTCALL)WRITE(UNIT_BLOCK,'(a)')FILE_LINE
C       print*,file_line
!      ENDDO

C      i = 1
C      read(kin,'(a)', err = 3013)FILE_LINE
C      read(FILE_LINE,*)x1(i),y1(i),y2(i),y3(i)
C            print*,x1(i),y1(i),y2(i),y3(i),


      n = 150
!       DO i = 1, n
!         read(kin,'(a)',err = 3045)FILE_LINE
!          IF(FILE_LINE(1:4) .EQ. '!END')THEN
!             print*,'ERROR: in file: '
!                print*,'DATAJ1/JPROC_CSQY/NASA_2006-HCHO_photolysis.dat'
!            print*,'Expect numerical input, but read,'
!            print*,FILE_LINE
!            STOP
!         ELSE
!            read(FILE_LINE,*)x1(i),y1(i),y2(i)
C            print*,x1(i),y1(i),y2(i),y3(i)
!
!         ENDIF
!      ENDDO



!      CALL INTAVG (x1, y1, n, 'C', wl, wu, yg1, nw)
!      CALL INTAVG (x1, y2, n, 'C', wl, wu, yg2, nw)

!      IF(FIRSTCALL)THEN
!           y1 = 1.0e-20*y1
!           y2 = 1.0e-24*y2
!
!      CALL INTAVG (WV_HCHO_XC, HCHO_XC_300K, N_HCHO_XC,
!     &             'C', wl, wu, HCHO_XCROSS_300K, nw)
!      CALL INTAVG (WV_HCHO_XC, HCHO_XC_A, N_HCHO_XC,
!     &             'C', wl, wu, HCHO_XCROSS_A, nw)
          
!           CALL WRBF12D_HEADER (UNIT_BLOCK,5,N,x1,'N_HCHO_XC',
!     &                           'WV_HCHO_XC','E')

!           CALL WRBF12D_HEADERB(UNIT_BLOCK,5,N,y1,'N_HCHO_XC',
!     &                           'HCHO_XC_300K','E')

!           CALL WRBF12D_HEADERB(UNIT_BLOCK,5,N,y2,'N_HCHO_XC',
!     &                           'HCHO_XC_A','E')
!      ENDIF

      yg1 = 1.0e-20*yg1
      yg2 = 1.0e-24*yg2

C       pause
!       DO i = 1, 7
!         read(kin,'(a)', err = 3045)FILE_LINE
!         IF(i .lt. 7 .AND. FIRSTCALL)WRITE(UNIT_BLOCK,'(a)')FILE_LINE
C        print*,file_line
!      ENDDO
      n = 112
!       DO i = 1, n
!         read(kin,'(a)',err = 3045)FILE_LINE
!          IF(FILE_LINE(1:4) .EQ. '!END')THEN
!             print*,'ERROR: in file: '
!                print*,'DATAJ1/JPROC_CSQY/NASA_2006-HCHO_photolysis.dat'
!            print*,'Expect numerical input, but read,'
!            print*,FILE_LINE
!            STOP
!         ELSE
!            read(FILE_LINE,*)x1(i),y1(i),y2(i)
C            print*,x1(i),y1(i),y2(i)

!         ENDIF
!      ENDDO

!      CALL INTAVG (x1, y1, n, 'C', wl, wu, yg3, nw)
!     CALL INTAVG (x1, y2, n, 'C', wl, wu, yg4, nw)


!      IF(FIRSTCALL)THEN

!         CALL WRBF12D_HEADER (UNIT_BLOCK,5,N,x1,'N_HCHO_QY',
!     &                           'WV_HCHO_QY','E')

!         CALL WRBF12D_HEADERB(UNIT_BLOCK,5,N,y1,'N_HCHO_QY',
!     &                           'HCHO_QYR','E')

!         CALL WRBF12D_HEADERB(UNIT_BLOCK,5,N,y2,'N_HCHO_QY',
!     &                           'HCHO_QYM','E')


!         CALL INTAVG (WV_HCHO_QY, HCHO_QYR, N_HCHO_QY, 'C',
!     &                wl, wu, HCHO_QUANTR_STP, nw)
!         CALL INTAVG (WV_HCHO_QY, HCHO_QYM, N_HCHO_QY, 'C', 
!     &                wl, wu, HCHO_QUANTM_STP, nw)

!      ENDIF
        
      if( FIRSTCALL )THEN

!        FIRSTCALL  = .false.

  
      
      DO iw = 1, nw - 1
C            write(6,'(i3,1X,f6.2,6(1x,es12.4))')iw,wc(iw), ! sig,qy1,qy2,
C     &         HCHO_XC_300K(iw) ,HCHO_QUANTR_STP(iw),HCHO_QUANTM_STP(iw)

           tdum = 265.0
           sig = HCHO_XCROSS_300K(iw) 
            if(tdum .lt. 300.0 .and. tdum .gt. 195.0)then
               sig = sig + HCHO_XCROSS_A(iw)*(tdum-300.0)
            elseif( tlev(i) .le. 195.0)then
               sig = sig - HCHO_XCROSS_A(iw)*105.0
            endif
            qy1 = HCHO_QUANTR_STP(iw)
            IF ( (wc(iw) .GE. 330.) .AND. (yg3(iw) .GT. 0.) ) THEN
               phi1 = HCHO_QUANTR_STP(iw)
               phi2 = HCHO_QUANTM_STP(iw)
               phi20 = 1. - phi1
               ak300=((1./phi2)-(1./phi20)) ! is divided by 1 atm
               if( tdum .lt. 300.0 .and. tdum .gt. 220.0)then
                   PRESSURE = 82.06*(airden(i)/6.02E+23)*tdum
                   akt = ak300
     &                 * (1.+0.05*(wc(iw)-329.0)*((tdum-80.0)/80.0))
               elseif( tdum .le. 220.0)then
                   PRESSURE = 3.0E-20*airden(i)
                   akt = ak300
     &                 * (1.+0.0875*(wc(iw)-329.0))
               elseif( tdum .ge. 300)then
                   PRESSURE = 4.09E-20*airden(i)
                   akt = ak300
     &                 * (1.+0.1375*(wc(iw)-329.0))
               endif
C               print*,pressure, airden(i)/2.54E+19
C      pause

               qy2 = 1. / ( (1./phi20) + pressure*akt)

            ELSE
               qy2 = HCHO_QUANTM_STP(iw)
            ENDIF
            qy2 = MAX(0.,qy2)
            qy2 = MIN(1.,qy2)

C           write(6,'(i3,1X,f6.2,6(1x,es12.4))')iw,wc(iw),sig,qy1,qy2,
C     &         HCHO_XCROSS_300K(iw),HCHO_QUANTR_STP(iw),
C     &         HCHO_QUANTM_STP(iw)

      ENDDO

      endif

      DO iw = 1, nw - 1
         DO i = 1, nz
            sig = HCHO_XCROSS_300K(iw) 
            if(tlev(i) .lt. 300.0 .and. tlev(i) .gt. 195.0)then
               sig = sig + HCHO_XCROSS_A(iw)*(tlev(i)-300.0)
            elseif( tlev(i) .le. 195.0)then
               sig = sig - HCHO_XCROSS_A(iw)*105.0
            endif
            qy1 = HCHO_QUANTR_STP(iw)
            IF ( (wc(iw) .GE. 330.) .AND. (yg3(iw) .GT. 0.) ) THEN
               phi1 = HCHO_QUANTR_STP(iw)
               phi2 = HCHO_QUANTM_STP(iw)
               phi20 = 1. - phi1
               ak300=((1./phi2)-(1./phi20)) ! is divided by 1 atm
               if( tlev(i) .lt. 300.0 .and. tlev(i) .gt. 220.0)then
                   PRESSURE = 82.06*(airden(i)/6.02E+23)*tlev(i)
                   akt = ak300
     &                 * (1.+0.05*(wc(iw)-329.0)*((tlev(i)-80.0)/80.0))
               elseif( tlev(i) .le. 220.0)then
                   PRESSURE = 3.0E-20*airden(i)
                   akt = ak300
     &                 * (1.+0.0875*(wc(iw)-329.0))
               elseif( tlev(i) .ge. 300.)then
                   PRESSURE = 4.09E-20*airden(i)
                   akt = ak300
     &                 * (1.+0.1375*(wc(iw)-329.0))
               endif
C               print*,pressure, airden(i)/2.54E+19
C      pause

               qy2 = 1. / ( (1./phi20) + pressure*akt)

            ELSE
               qy2 = HCHO_QUANTM_STP(iw)
            ENDIF
            qy2 = MAX(0.,qy2)
            qy2 = MIN(1.,qy2)

            xcross(iw, i) = sig
            quantr(iw, i) = qy1
            quantm(iw, i) = qy2

         ENDDO
      ENDDO


!        STOP

          RETURN
3045   print*,'ERROR: in file: '
        print*,i,'DATAJ1/JPROC_CSQY/NASA_2006-HCHO_photolysis.dat'
       STOP
      END

*=============================================================================*

      SUBROUTINE NASA_NO3_QUANTAS(nw,wl,wc,nz,tlev,airden,QYNO3_NO,
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

* input

      INTEGER nw
      REAL wl(kw), wc(kw), wu(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airden(kz)

* weighting functions

      CHARACTER*50 jlabel(1)
      REAL sq(1,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=350)

      REAL x1(kdata)
      REAL y1(kdata),y2(kdata),y3(kdata)
      REAL z1(kdata),z2(kdata),z3(kdata)
       REAL qy1(kdata),qy2(kdata)
       REAL slope

* local

      REAL yg(kw), yg1(kw), yg2(kw)
       REAL temp_adj(kz)
       REAL QYNO3_NO2(kw,kz),QYNO3_NO(kw,kz)
      REAL qy
      INTEGER irow, icol
      INTEGER i, iw, n, idum
      INTEGER ierr
       INTEGER mabs
       CHARACTER(LEN=120) :: FILE_LINE
       LOGICAL            :: EXISTS 
      LOGICAL, SAVE      :: FIRSTCALL = .TRUE.

* quantum yield:

* for   NO3 ->NO+O2

      j = 0
      j = j + 1
      jlabel(j) = 'NO3NO-06        ' ! 'NO3 -> NO + O2'


* for  NO3 ->NO2+O
      j = j + 1
      jlabel(j) = 'NO3NO2-6        ' ! 'NO3 -> NO2 + O(3P)'



      temp_adj = 1.0


      DO i = 1, nz
        temp_adj(i) = (1.0-exp(-1096.4/tlev(i))
     &              -  2.0*exp(-529.5/tlev(i)))
     &              / (1.0-exp(-1096.4/298.0)
     &              -  2.0*exp(-529.5/298.0))
      ENDDO

       do i = 1, nw-1
! compute upper limit of wavelength bins
          wu(i) = 2.0*wc(i) - wl(i)
      enddo
       wu(nw)=wu(nw-1)+2*(wc(nw-1) - wl(nw-1))
      
!      INQUIRE(FILE='DATAJ1/JPROC_CSQY/NASA_2006-NO3_photolysis.dat',
!     &        EXIST=exists)


!      IF( .NOT. EXISTS)THEN
!          print*,' NASA_2006-NO3_photolysis.dat not found '
!       stop
!       else
C          print*,' NASA_2006-NO3_photolysis.dat found '
!       endif

!      OPEN(UNIT=kin,
!     &     FILE='DATAJ1/JPROC_CSQY/NASA_2006-NO3_photolysis.dat',
!     &     STATUS='old')
          
        
!       DO i = 1, 5
!         read(kin,'(a)', err = 3013)FILE_LINE
C         IF( i .lt. 5 .AND. FIRSTCALL)write(UNIT_BLOCK,'(a)')FILE_LINE
C       print*,file_line
!      ENDDO

C      i = 1
C      read(kin,'(a)', err = 3013)FILE_LINE
C      read(FILE_LINE,*)x1(i),y1(i),y2(i),y3(i),
C     &                       z1(i),z2(i),z3(i)
C            print*,x1(i),y1(i),y2(i),y3(i),
C     &                             z1(i),z2(i),z3(i)


!      n = 57
!       DO i = 1, n
!         read(kin,'(a)',err = 3013)FILE_LINE
!          IF(FILE_LINE(1:4) .EQ. '!END')THEN
!             print*,'ERROR: in file: '
!                print*,'DATAJ1/JPROC_CSQY/NASA_2006-NO3_photolysis.dat'
!            print*,'Expect numerical input, but read,'
!            print*,FILE_LINE
!            STOP
!         ELSE
!            read(FILE_LINE,*)x1(i),y1(i),y2(i),y3(i),
!     &                             z1(i),z2(i),z3(i)
C            print*,x1(i),y1(i),y2(i),y3(i),
C     &                             z1(i),z2(i),z3(i)

!         ENDIF
!      ENDDO

!      IF(FIRSTCALL)THEN

!         CALL WRBF12D_HEADER (UNIT_BLOCK,5,N,x1,'N_NO3_QY',
!     &                           'WV_NO3_QY','E')

!         CALL WRBF12D_HEADERB(UNIT_BLOCK,5,N,y1,'N_NO3_QY',
!     &                           'NO2NO_QY_298K','E')

!         CALL WRBF12D_HEADERB(UNIT_BLOCK,5,N,z1,'N_NO3_QY',
!     &                           'NO2NO2_QY_298K','E')

!         CALL WRBF12D_HEADERB(UNIT_BLOCK,5,N,y2,'N_NO3_QY',
!     &                           'NO2NO_QY_230K','E')

!         CALL WRBF12D_HEADERB(UNIT_BLOCK,5,N,z2,'N_NO3_QY',
!     &                           'NO2NO2_QY_230K','E')

!         CALL WRBF12D_HEADERB(UNIT_BLOCK,5,N,y3,'N_NO3_QY',
!     &                           'NO2NO_QY_190K','E')

!         CALL WRBF12D_HEADERB(UNIT_BLOCK,5,N,z3,'N_NO3_QY',
!     &                           'NO2NO2_QY_190K','E')

!         CALL INTAVG (WV_NO3_QY, NO2NO_QY_298K, N_NO3_QY, 'C',
!     &                wl, wu, NO3NO_QUANT_298K, nw)
!         CALL INTAVG (WV_NO3_QY, NO2NO2_QY_298K, N_NO3_QY, 'C',
!     &                wl, wu, NO3NO2_QUANT_298K, nw)

!         CALL INTAVG (WV_NO3_QY, NO2NO_QY_230K, N_NO3_QY, 'C', 
!     &                wl, wu, NO3NO_QUANT_230K, nw)
!         CALL INTAVG (WV_NO3_QY, NO2NO2_QY_230K, N_NO3_QY, 'C',
!     &                wl, wu, NO3NO2_QUANT_230K, nw)

!         CALL INTAVG (WV_NO3_QY, NO2NO_QY_190K, N_NO3_QY, 'C', 
!     &                wl, wu, NO3NO_QUANT_190K, nw)
!         CALL INTAVG (WV_NO3_QY, NO2NO2_QY_190K, N_NO3_QY, 'C',
!     &                wl, wu, NO3NO2_QUANT_190K, nw)

!         FIRSTCALL = .TRUE.

!      ENDIF


!     close(kin)

      DO i = 1, nz
         DO iw = 1, nw
            qy1(iw) = NO3NO_QUANT_298K(iw)
            qy2(iw) = NO3NO2_QUANT_298K(iw)
             IF(tlev(i) .lt. 298.0 .and. tlev(i) .ge. 230.0)THEN
                slope   = (NO3NO_QUANT_298K(iw)-NO3NO_QUANT_230K(iw))
     &                 /  68.0
               qy1(iw) =  NO3NO_QUANT_230K(iw) + slope*(tlev(i)-230.0)
                slope   = (NO3NO2_QUANT_298K(iw)-NO3NO2_QUANT_230K(iw))
     &                 /  68.0
               qy2(iw) =  NO3NO2_QUANT_230K(iw) + slope*(tlev(i)-230.0)
            ELSEIF(tlev(i) .lt. 230.0 .and. tlev(i) .ge. 190.0)THEN
                slope   = (NO3NO_QUANT_230K(iw)-NO3NO_QUANT_190K(iw))
     &                 /  40.0
               qy1(iw) =  NO3NO_QUANT_190K(iw) + slope*(tlev(i)-190.0)
                slope   = (NO3NO2_QUANT_230K(iw)-NO3NO2_QUANT_190K(iw))
     &                 /  40.0
               qy2(iw) =  NO3NO2_QUANT_190K(iw) + slope*(tlev(i)-190.0)
            ELSEIF( tlev(i) .lt. 190)THEN
               qy1(iw)  =  NO3NO_QUANT_190K(iw)
               qy2(iw)  =  NO3NO2_QUANT_190K(iw)
             ENDIF
C            if(i.eq.1)print*,tlev(i),x1(iw),QY1(iw),QY2(iw)
         ENDDO


         DO iw = 1, nw
            QYNO3_NO(iw,i)  = qy1(iw)*0.001
             if(wl(iw) .le. WV_NO3_QY(1))then
               QYNO3_NO(iw,i)  = NO3NO_QUANT_298K(1)*0.001
               QYNO3_NO2(iw,i) = NO3NO2_QUANT_298K(1)*0.001
             else
               QYNO3_NO2(iw,i) = qy2(iw)*0.001
            endif
C            if(i.eq.1)print*,tlev(i),wl(iw),x1(1),
C     &                     QYNO3_NO(iw,i),QYNO3_NO2(iw,i)
         ENDDO
      ENDDO

C       stop


       RETURN
3013   print*,'ERROR: in file: '
        print*,'DATAJ1/JPROC_CSQY/NASA_2006-NO3_photolysis.dat'
       STOP
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
