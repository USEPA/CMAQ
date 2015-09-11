      SUBROUTINE XC_QY_TD_EFFECT(WC,NW,TLEV,DENS,NZ,
     &             JLABEL,XC,QY,SQ,REPLACE)

      USE CSQY_REFER_DATA

      IMPLICIT NONE

! subroutine computes the product of the cross-section and
! quantum yield over the atmospheric levels 
! includes temperature and pressure effect for select rates
! inputs:

      INTEGER,       INTENT( IN )    ::  NW
      REAL,          INTENT( IN )    ::  WC(:)
      INTEGER,       INTENT( IN )    ::  NZ
      REAL,          INTENT( IN )    ::  TLEV(:)  ! air temperature over model levels, deg K
      REAL,          INTENT( IN )    ::  DENS(:)  ! air number density over level, 1/cm3
      CHARACTER(16), INTENT( IN )    ::  JLABEL   ! name of photolysis rate
      REAL,          INTENT( INOUT ) ::  XC(:,:)  ! cross-section from file
      REAL,          INTENT( INOUT ) ::  QY(:,:)  ! quantum yield from file
      REAL,          INTENT( OUT )   ::  SQ(:,:)  ! cross-section times quantum yield over model levels
      LOGICAL,       INTENT( OUT )   ::  REPLACE  ! flag to use sq values in calling routine 


! local:
      INTEGER IW, IJ, IZ
      INTEGER I, J, N

      LOGICAL, SAVE :: FIRSTCALL = .TRUE.

! output quantum yields

      LOGICAL EXISTS

      REAL PRESSURE
      REAL TEMP( 1 )
      REAL TDUM, QDUM, WDUM, FACTOR



! local
      REAL, EXTERNAL  :: OZONE_YIELD
      REAL, EXTERNAL  :: QY_ACETONE
      REAL, EXTERNAL  :: QY_ACETONE_TUV

      REAL NO2_XCROSS_298K( KW )
!      REAL NO2_QUANT_298K ( KW )
      REAL O3_XCROSS_298K ( KW )
      REAL O3_QUANT_298K  ( KW )
      REAL HCHO_XCROSS_298K( KW )
      REAL HCHO_QUANTR_298K( KW )
      REAL HCHO_QUANTM_298K( KW )
      REAL QY_NO3_NO2_298K( KW ) 
      REAL QY_NO3_NO_298K ( KW )
      REAL CLONO2_XCROSS_298K( KW )

      REAL NO2_XCROSS( KW, KZ )
      REAL NO2_QUANT ( KW, KZ )
      REAL O3_XCROSS(KW,KZ)
      REAL O3_QUANT (KW,KZ)
      REAL HCHO_XCROSS(KW,KZ)
      REAL HCHO_QUANTR(KW,KZ)
      REAL HCHO_QUANTM(KW,KZ)
      REAL CLONO2_XCROSS(KW,KZ)
      REAL QYNO3_NO2(KW,KZ)
      REAL QYNO3_NO(KW,KZ)

      REAL ADJ_NO2_XCROSS( KW, KZ )
      REAL ADJ_NO2O3P_QUANT ( KW, KZ )
      REAL ADJ_NO2EXC_QUANT ( KW, KZ )
      REAL ADJ_O3_XCROSS(KW,KZ)
      REAL ADJ_O3O1D_QUANT (KW,KZ)
      REAL ADJ_O3O3P_QUANT (KW,KZ)
      REAL ADJ_HCHO_XCROSS(KW,KZ)
      REAL ADJ_HCHO_QUANTR(KW,KZ)
      REAL ADJ_HCHO_QUANTM(KW,KZ)
      REAL ADJ_CLONO2_XCROSS(KW,KZ)
      REAL ADJ_QY_NO3_NO2(KW,KZ)
      REAL ADJ_QY_NO3_NO(KW,KZ)
      
      LOGICAL, SAVE :: COMPUTED_NO2O3P = .FALSE.
      LOGICAL, SAVE :: COMPUTED_NO2EXC = .FALSE.
      LOGICAL, SAVE :: COMPUTED_O3O1D  = .FALSE.
      LOGICAL, SAVE :: COMPUTED_O3O3P  = .FALSE.
      
      REAL,    SAVE :: NO2O3P_YIELD( KZ, KW )
      REAL,    SAVE :: NO2EXC_YIELD( KZ, KW )
      REAL,    SAVE :: O3O1D_YIELD ( KZ, KW )
      REAL,    SAVE :: O3O3P_YIELD ( KZ, KW )

      REAL,    SAVE :: PREV_TLEV( KZ ) = 0.0 ! previous air temperature over model levels, deg K
      REAL,    SAVE :: PREV_DENS( KZ ) = 0.0 ! previous air number density over level, 1/cm3
      REAL,    SAVE :: PREV_WC  ( KW ) = 0.0 ! previous wavelenghts, nm
      
      INTEGER, SAVE :: NZ_PREV = 0
      INTEGER, SAVE :: NW_PREV = 0
      LOGICAL       :: RESET_WC
      LOGICAL       :: RESET_TD

      REAL          :: SIG, ALPHA, BETA, CHI
      REAL          :: XCROSS( NZ, NW), QUANT( NZ,NW )

      INTERFACE
        SUBROUTINE WVBIN_AVERAGE(WL_CS_IN, CS_IN, NWL_CS_IN,  
     &                         WL_QY_IN, QY_IN, NWL_QY_IN,  
     &                         SPECTRA_TYPE,
     &                         WLL_AVE, WLU_AVE, NWL_AVE, 
     &                         CS_AVE, QY_AVE )
          USE CSQY_PARAMETERS
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

    
        CALL INIT_CSQY_REFER_DATA()

        FIRSTCALL = .FALSE.
          
      ENDIF
      

      RESET_WC  = .FALSE.
      RESET_TD  = .FALSE.

      IF( NW_PREV .NE. NW .OR. NZ_PREV .NE. NZ )THEN
          RESET_WC  = .TRUE.
          RESET_TD  = .TRUE.
       ELSE 
          DO I = 1, NZ
             IF( ABS(PREV_TLEV( I )- TLEV( I )) .GT. 1.0E-4 )THEN
                 RESET_TD  = .TRUE.
             END IF
             IF( ABS(PREV_DENS( I )- DENS( I )) .GT. 1.0E-4 )THEN
                 RESET_TD  = .TRUE.
             END IF
          END DO
          DO I = 1, NW
             IF( ABS(PREV_WC( I )- WC( I )) .GT. 1.0E-4 )THEN
                 RESET_WC  = .TRUE.
             END IF
          END DO
       END IF
       
       IF( RESET_WC .OR. RESET_TD )THEN
          WRITE(6,*)"RESETTING IN XC_QY_TD_EFFECT FOR " // TRIM( JLABEL )
          COMPUTED_NO2O3P = .FALSE.
          COMPUTED_NO2EXC = .FALSE.
          COMPUTED_O3O3P  = .FALSE.
          COMPUTED_O3O1D  = .FALSE.
          PREV_TLEV = 0.0
          PREV_DENS = 0.0
          PREV_WC   = 0.0
          NW_PREV = NW
          NZ_PREV = NZ
          DO I = 1, NZ
             PREV_TLEV( I ) = TLEV( I )
             PREV_DENS( I ) = DENS( I )
          END DO
          DO I = 1, NW
             PREV_WC( I ) = WC( I )
          END DO
        END IF
          

! computing data used for multiple rates
      CALL JHCHO_NASA_2006(NW,WC,NZ,TLEV,DENS, HCHO_XCROSS, HCHO_QUANTR, HCHO_QUANTM)

      CALL NASA_NO3_QUANTAS(NW,WC,NZ,TLEV,DENS,QYNO3_NO, QYNO3_NO2)

 
      DO IW = 1, NW

! calculation at 298K      
         O3_XCROSS_298K( IW ) = O3_XCROSS_293K( IW )
         O3_QUANT_298K ( IW ) = OZONE_YIELD( WC( IW ), T298K )

         CLONO2_XCROSS_298K( IW ) = CLONO2_XCROSS0( IW )
     &                            * ( 1.0 
     &                            +  CLONO2_A1( IW )*( T298K - 296 ) 
     &                            +  CLONO2_A2( IW )*( T298K - 296 )**2 )
          
         NO2_XCROSS_298K(IW )  =  NO2_XCROSS_294K(IW)
         NO2_QUANT_298K( IW )  =  NO2_QUANT_298K(IW)
         
         QY_NO3_NO_298K( IW )  = 1.0E-3*NO3NO_QUANT_298K( IW )
         QY_NO3_NO2_298K( IW ) = 1.0E-3*NO3NO2_QUANT_298K( IW )


         HCHO_XCROSS_298K( IW ) = HCHO_XCROSS_300K(IW) + HCHO_XCROSS_A(IW)*(T298K-300.0)
         HCHO_XCROSS_298K( IW ) = MAX( HCHO_XCROSS_298K(IW), 0.0)


         IF( HCHO_QUANTR_STP(IW) .LT. 0.999999 )THEN
            IF(WC(IW) .GE. 330.0 .AND. HCHO_QUANTM_STP(IW) .GT. 0.0)THEN
              PRESSURE = 1.0 ! atmospheres
              FACTOR = ((1.0/HCHO_QUANTM_STP(IW))-(1.0/(1.0 - HCHO_QUANTR_STP(IW))))
     &               * (1.0 + 0.05*( WC(IW)-329.0)*((T298K-80.0)/80.0))

              HCHO_QUANTM_298K( IW ) = 1.0/( 1.0/(1.0 - HCHO_QUANTR_STP(IW)) + PRESSURE*FACTOR )
           ELSE
              HCHO_QUANTM_298K( IW ) = HCHO_QUANTM_STP(IW)
           ENDIF
         ELSE
           HCHO_QUANTM_298K( IW ) = 0.000001
         END IF

         HCHO_QUANTR_298K( IW ) = HCHO_QUANTR_STP(IW)
           
         HCHO_QUANTM_298K( IW ) = MIN( 1.0, MAX( HCHO_QUANTM_298K( IW ), 0.0))
 
         DO I = 1, NZ
         
            XCROSS( I, IW ) = XC( I, IW )
            QUANT( I, IW )  = QY( I, IW )

           IF( TLEV(I) .LT. 293.0 .AND. TLEV(I) .GT. 218.0)THEN
             O3_XCROSS(IW,I) = (O3_XCROSS_293K(IW)-O3_XCROSS_218K(IW))
     &                        /  75.0
     &                        * (TLEV(I) - 218.0)
     &                        + O3_XCROSS_218K(IW)
           ELSEIF( TLEV(I) .LE. 218.0)THEN
             O3_XCROSS(IW,I) = O3_XCROSS_218K(IW)
           ELSEIF( TLEV(I) .GE. 293.0)THEN
             O3_XCROSS(IW,I) = O3_XCROSS_293K(IW)
           ENDIF


           O3_QUANT( IW,I) = OZONE_YIELD(WC(IW),TLEV(I)) 


           TDUM = TLEV(I)-296.0
  
           CLONO2_XCROSS(IW,I) = CLONO2_XCROSS0(IW)
     &                          * ( 1.0
     &                          +  CLONO2_A1(IW)*TDUM 
     &                          +  CLONO2_A2(IW)*TDUM**2 )



            NO2_XCROSS(IW,I) = NO2_XCROSS_294K(IW)
             IF(TLEV(I) .GT. 220.0 .AND. TLEV(I) .LT. 294.0)THEN
               TDUM  = (NO2_XCROSS_294K(IW)-NO2_XCROSS_220K(IW))
     &               * (TLEV(I)-220.0)/74.0
                NO2_XCROSS(IW,I) =  NO2_XCROSS_220K(IW)
     &                           +  TDUM   
            ELSEIF(TLEV(I) .LE. 220.0)THEN
                NO2_XCROSS(IW,I) =  NO2_XCROSS_220K(IW)
            ENDIF

            NO2_QUANT(IW,I) = NO2_QUANT_298K(IW)
            IF(TLEV(I) .GT. 248.0 .AND. TLEV(I) .LT. 294.0)THEN
               TDUM  = (NO2_QUANT_298K(IW)-NO2_QUANT_248K(IW))
     &               * (TLEV(I)-248.0)/50.0
                NO2_QUANT(IW,I) =  NO2_QUANT_248K(IW)
     &                           +  TDUM   
            ELSEIF(TLEV(I) .LE. 248.0)THEN
                NO2_QUANT(IW,I) =  NO2_QUANT_248K(IW)
            ENDIF

            NO2_QUANT(IW,I) = MIN(MAX(NO2_QUANT(IW,I), 0.0), 1.0)

! compute adjustment factors
            
           IF( QY_NO3_NO_298K( IW ) .GT. 0.0 )THEN
               ADJ_QY_NO3_NO(IW,I) = QYNO3_NO(IW,I) / QY_NO3_NO_298K(IW )
               print*,'QYNO3_NO(IW,I), QY_NO3_NO_298K(IW ) = ',QYNO3_NO(IW,I), QY_NO3_NO_298K(IW )
           ELSE
               ADJ_QY_NO3_NO(IW,I) = 1.0
           END IF

           IF( QY_NO3_NO2_298K( IW ) .GT. 0.0 )THEN
               ADJ_QY_NO3_NO2(IW,I) = QYNO3_NO2(IW,I) / QY_NO3_NO2_298K(IW )
               print*,'QYNO3_NO2(IW,I), QY_NO3_NO2_298K(IW ) = ',QYNO3_NO2(IW,I), QY_NO3_NO2_298K(IW )
           ELSE
               ADJ_QY_NO3_NO2(IW,I) = 1.0
           END IF

           IF( HCHO_XCROSS_298K( IW ) .GT. 0.0 )THEN
               ADJ_HCHO_XCROSS(IW,I) = HCHO_XCROSS(IW,I) / HCHO_XCROSS_298K(IW )
           ELSE
               ADJ_HCHO_XCROSS(IW,I) = 1.0
           END IF

           IF( HCHO_QUANTR_298K( IW ) .GT. 0.0 )THEN
               ADJ_HCHO_QUANTR(IW,I) = HCHO_QUANTR(IW,I) / HCHO_QUANTR_298K(IW )
           ELSE
               ADJ_HCHO_QUANTR(IW,I) = 1.0
           END IF
           
           IF( HCHO_QUANTM_298K( IW ) .GT. 0.0 )THEN
               ADJ_HCHO_QUANTM(IW,I) = HCHO_QUANTM(IW,I) / HCHO_QUANTM_298K(IW )
           ELSE
               ADJ_HCHO_QUANTM(IW,I) = 1.0
           END IF

           IF( O3_XCROSS_298K( IW ) .GT. 0.0 )THEN
               ADJ_O3_XCROSS(IW,I) = O3_XCROSS(IW,I) / O3_XCROSS_298K(IW )
           ELSE
               ADJ_O3_XCROSS(IW,I) = 1.0
           END IF

           IF( O3_QUANT_298K( IW ) .GT. 0.0 )THEN
               ADJ_O3O1D_QUANT(IW,I) = O3_QUANT(IW,I) / O3_QUANT_298K(IW )
           ELSE
               ADJ_O3O1D_QUANT(IW,I) = 1.0
           END IF

           IF( O3_QUANT_298K( IW ) .LT. 1.0 .AND. O3_QUANT_298K( IW ) .GT. 0.0 )THEN
               ADJ_O3O3P_QUANT(IW,I) = MAX((1.0 - ADJ_O3O1D_QUANT(IW,I)*O3_QUANT_298K( IW )), 0.0)
     &                               / (1.0 - O3_QUANT_298K( IW ))

             IF( O3_QUANT(IW,I) .LT. 1.0 )THEN
             
               ALPHA = (1.0 - O3_QUANT(IW,I))
               BETA  = (1.0 - ( ADJ_O3O3P_QUANT(IW,I)*ALPHA + ADJ_O3O1D_QUANT(IW,I)*O3_QUANT(IW,I)))
     &               /  ALPHA
             ELSE
               BETA  = 0.0
             END IF
     
               ADJ_O3O3P_QUANT(IW,I) = ADJ_O3O3P_QUANT(IW,I) + BETA
           ELSE
               ADJ_O3O3P_QUANT(IW,I) = 1.0
           END IF

           IF( CLONO2_XCROSS_298K( IW ) .GT. 0.0 )THEN
               ADJ_CLONO2_XCROSS(IW,I) = CLONO2_XCROSS(IW,I) / CLONO2_XCROSS_298K(IW )
           ELSE
               ADJ_CLONO2_XCROSS(IW,I) = 1.0
           END IF

            IF( NO2_XCROSS_298K(IW ) .GT. 0.0 )THEN
               ADJ_NO2_XCROSS(IW,I) =  NO2_XCROSS(IW,I) /  NO2_XCROSS_298K(IW )
            ELSE
               ADJ_NO2_XCROSS(IW,I) = 1.0
            END IF

           IF( NO2_QUANT_298K( IW ) .GT. 0.0 )THEN
               ADJ_NO2O3P_QUANT(IW,I) = NO2_QUANT(IW,I) / NO2_QUANT_298K(IW )
            ELSE
               ADJ_NO2O3P_QUANT(IW,I) = 1.0
           END IF

           IF( NO2_QUANT_298K( IW ) .LT. 1.0 .AND. NO2_QUANT_298K( IW ) .GT. 0.0 )THEN
               ADJ_NO2EXC_QUANT(IW,I) = MAX((1.0 - ADJ_NO2O3P_QUANT(IW,I)*NO2_QUANT_298K( IW )), 0.0)
     &                                / (1.0 - NO2_QUANT_298K( IW ))
     
             IF( NO2_QUANT(IW,I) .LT. 1.0 )THEN
             
               ALPHA = (1.0 - NO2_QUANT(IW,I))
               BETA  = (1.0 - ( ADJ_NO2EXC_QUANT(IW,I)*ALPHA + ADJ_NO2O3P_QUANT(IW,I)*NO2_QUANT(IW,I)))
     &               /  ALPHA
     
             ELSE
               BETA  = 0.0
             END IF
     
               ADJ_NO2EXC_QUANT(IW,I) = ADJ_NO2EXC_QUANT(IW,I) + BETA
           ELSE
               ADJ_NO2EXC_QUANT(IW,I) = 1.0
           END IF


        ENDDO
      ENDDO


      SQ = 0.0

      SELECT CASE( JLABEL ) 
        CASE( 'IC3ONO2', 'NTR_IUPAC10', 'NTR_IUPAC04', 'ONIT_RACM2' )

! temperature correction to cross-section

            DO IW = 1, NW
               DO I = 1, NZ
                 IF((WC(IW) .GE. 240.) .AND. (WC(IW) .LE. 340.))THEN
                  IF( TLEV(I) .LT. 360.0 .AND. TLEV(I) .GT. 233.0)THEN

                     FACTOR = EXP(IC3ONO2_XCROSS_EXP(IW)*(TLEV(I)-T298K))

                   ELSEIF( TLEV(I) .LE. 240.0)THEN

                     FACTOR = EXP(IC3ONO2_XCROSS_EXP(IW)*(-58.0))

                   ELSEIF( TLEV(I) .GE. 360.)THEN

                     FACTOR = EXP(IC3ONO2_XCROSS_EXP(IW)*(62.0))

                   ENDIF
                 ELSE
                     FACTOR = 1.0
                 ENDIF

                 print*,JLABEL,TLEV(I)-T298K, factor*XC(I,IW), XCROSS(I, IW)
                  XC(I, IW)  = FACTOR * XC(I, IW)
                  SQ(I, IW)  = XC(I, IW) * QY(I,IW)
 
               ENDDO
             ENDDO

             REPLACE = .TRUE.

         CASE( 'NO2-06', 'NO2_06', 'NO2_RACM2', 'NO2_IUPAC10' )    ! 'NO2 -> NO + O(3P)'

! temperature correction to cross-section and quantum yield

             DO IW = 1, NW
               DO I = 1, NZ

                  FACTOR     = MAX( ADJ_NO2_XCROSS(IW,I), 0.0)
                  XC(I, IW)  = FACTOR * XC(I, IW)

! the below IF block forces the yields of O3P and NO2EXC to add to one
! assumes that NZ and NW does not change between subroutine calls                  

                  IF( .NOT. COMPUTED_NO2O3P .AND. .NOT. COMPUTED_NO2EXC )THEN
                     FACTOR     = MAX( ADJ_NO2O3P_QUANT(IW,I), 0.0)
                  print*,JLABEL,TLEV(I)-T298K, ADJ_NO2O3P_QUANT(IW,I), QY(I, IW), QUANT(I,IW)
                     QY(I, IW)  = FACTOR * QY(I, IW)
                     QY(I, IW)  = MIN( QY(I, IW), 1.0)
                     NO2O3P_YIELD( I, IW ) = QY(I, IW)
                     IF( IW .GE. NW .AND. I .GE. NZ )COMPUTED_NO2O3P = .TRUE.
                  ELSE IF( .NOT. COMPUTED_NO2O3P .AND. COMPUTED_NO2EXC )THEN
                     FACTOR = -2.0
                     QY(I, IW) = MAX((1.0-NO2EXC_YIELD( I, IW )), 0.0)
                  print*,JLABEL,TLEV(I)-T298K, FACTOR, QY(I, IW), QUANT(I,IW)
                     NO2O3P_YIELD( I, IW ) = QY(I, IW)
                     IF( IW .GE. NW .AND. I .GE. NZ )COMPUTED_NO2O3P = .TRUE.
                  ELSE IF( COMPUTED_NO2O3P )THEN
                     FACTOR = -3.0
                     QY(I, IW) = NO2O3P_YIELD( I, IW )
                  print*,JLABEL,TLEV(I)-T298K, FACTOR, QY(I, IW), QUANT(I,IW)
                  END IF
                  
                  print*,JLABEL,TLEV(I)-T298K, FACTOR, QY(I, IW), QUANT(I,IW)
                  SQ(I, IW)  = XC(I, IW) * QY(I, IW)
               ENDDO
             ENDDO

             REPLACE = .TRUE.

         CASE( 'N2O5_IUPAC04', 'N2O5_IUPAC10' )

! temperature correction to cross-section 

           DO IW = 1, NW
               DO I = 1, NZ

                 TDUM   = MAX(195.0, MIN(TLEV(I), 300.0))

                 ALPHA  = N2O5_XCROSS_EXP(IW)*(1.0/TDUM - 1.0/T298K)
                 FACTOR = EXP( ALPHA )

                  print*,JLABEL,TLEV(I)-T298K,factor* XC(I, IW), XCROSS( I,IW) 
                 XC(I,IW) = FACTOR  * XC(I,IW)
                 SQ(I,IW) = XC(I,IW) * QY(I,IW)

               ENDDO
             ENDDO

             REPLACE = .TRUE.

         CASE(  'NO2EX'   )    ! 'NO2 -> NO2(excited)'

! temperature correction to cross-section and quantum yield

            DO IW = 1, NW
               DO I = 1, NZ

                  FACTOR     = MAX( ADJ_NO2_XCROSS(IW,I), 0.0)
                  XC(I, IW)  = FACTOR * XC(I, IW)

! the below IF block forces the yields of O3P and NO2EXC to add to one
! assumes that NZ and NW does not change between subroutine calls                  
                  IF( .NOT. COMPUTED_NO2O3P .AND. .NOT. COMPUTED_NO2EXC )THEN
                     FACTOR     = MAX( ADJ_NO2EXC_QUANT(IW,I), 0.0)
                     QY(I, IW)  = FACTOR * QY(I, IW)
                     QY(I, IW)  = MIN( QY(I, IW), 1.0)
                     NO2EXC_YIELD( I, IW ) = QY(I, IW)
                     IF( IW .GE. NW .AND. I .GE. NZ )COMPUTED_NO2EXC = .TRUE.
                  ELSE IF( COMPUTED_NO2O3P .AND. .NOT. COMPUTED_NO2EXC )THEN
                     QY(I, IW) = MAX((1.0-NO2O3P_YIELD( I, IW )), 0.0)
                     NO2EXC_YIELD( I, IW ) = QY(I, IW)
                     IF( IW .GE. NW .AND. I .GE. NZ )COMPUTED_NO2EXC = .TRUE.
                  ELSE IF( COMPUTED_NO2EXC )THEN
                     QY(I, IW) = NO2EXC_YIELD( I, IW )
                  END IF
                          
                  print*,JLABEL,TLEV(I)-T298K, WC(IW), QY(I, IW), QUANT(I,IW),NO2O3P_YIELD( I, IW )

                  SQ(I, IW) = XC(I, IW) * QY(I, IW)
               ENDDO
             ENDDO

             REPLACE = .TRUE.

         CASE(   'HNO4-06', 'HNO4_06', 'HO2NO2_IUPAC04', 'PNA_IUPAC10', 'HNO4_RACM2' )    ! 'HNO4 -> HO2 + NO2'

! temperature correction to cross-section
            DO IW = 1, NW
               DO I = 1, NZ
                 QDUM = QY(I, IW)

                 IF(HO2NO2_XCROSS_A1(IW).GT.0.0.AND.HO2NO2_XCROSS_A2(IW).GT.0.0)THEN

                     TDUM   = 1.0+EXP( -988.0/(0.69*TLEV(I)) )
                     CHI    = (HO2NO2_XCROSS_A1(IW)/TDUM + HO2NO2_XCROSS_A2(IW)*(1.0-1.0/TDUM))

                     TDUM   = 1.0+EXP(-988.0/(0.69*T298K) )
                     FACTOR = CHI
     &                      / (HO2NO2_XCROSS_A1(IW)/TDUM + HO2NO2_XCROSS_A2(IW)*(1.0-1.0/TDUM))

                 ELSE

                     FACTOR = 1.0

                 ENDIF

                  print*,JLABEL,TLEV(I)-T298K,factor* XC(I, IW), XCROSS(I,IW)
                 XC(I, IW)  = FACTOR * XC(I, IW)
                 SQ(I, IW)  = XC(I,IW) * QY(I,IW)

               ENDDO
             ENDDO

             REPLACE = .TRUE.

         CASE(  'NO3NO-06', 'NO3NO_06', 'NO3NO_RACM2' ) ! 'NO3 -> NO + O2'

! temperature correction to cross-section and quantum yield

           DO IW = 1, NW
               DO I = 1, NZ
                  FACTOR = (1.0-EXP(-1096.4/TLEV(I)) - 2.0*EXP(-529.5/TLEV(I)))
     &                   / (1.0-EXP(-1096.4/ T298K ) - 2.0*EXP(-529.5/ T298K ))

                  XC(I, IW) = FACTOR * XC(I, IW)
                  print*,JLABEL,TLEV(I)-T298K,XC(I, IW), XCROSS(I,IW)
                  QY(I, IW) = ADJ_QY_NO3_NO(IW,I) * QY(I, IW)
                  QY(I, IW) = MIN(1.0, QY(I, IW))
                  QY(I, IW) = MAX(0.0, QY(I, IW))
                  SQ(I, IW) = XC(I,IW) * QY(I,IW)

               ENDDO
           ENDDO

           REPLACE = .TRUE.

         CASE(  'CLONO2-2', 'CLONO2_2' )    ! 'ClONO2 -> Cl + NO3'

! temperature correction to cross-section 

            DO IW = 1, NW
               DO I = 1, NZ
                  XC(I, IW) = ADJ_CLONO2_XCROSS(IW,I) * XC(I, IW)
              print*,JLABEL,TLEV(I)-T298K,XC(I, IW), XCROSS(I,IW)
                  SQ(I, IW) = XC(I,IW) * QY(I,IW)
               ENDDO
            ENDDO

            REPLACE = .TRUE.

         CASE( 'CLONO2-1', 'CLONO2_1' )     ! 'ClONO2 -> ClO + NO2'

! temperature correction to cross-section 

            DO IW = 1, NW
               DO I = 1, NZ
                  XC(I, IW) = ADJ_CLONO2_XCROSS(IW,I) * XC(I, IW)
              print*,JLABEL,TLEV(I)-T298K,XC(I, IW), XCROSS(I,IW)

                  SQ(I, IW) = XC(I,IW) * QY(I,IW)
               ENDDO
            ENDDO

            REPLACE = .TRUE.

         CASE( 'CCHO_R', 'ALD2_R_IUPAC10', 'CH3CHO_RACM2'  )      ! 'CH3CHO -> CH3 + HCO'

! density correction to quantum yield

           DO IW = 1, NW
               DO I = 1, NZ
               
                   FACTOR = (1. + CCHO_YIELD_COEFF(IW))
     &                    / (1. + CCHO_YIELD_COEFF(IW)*DENS(I)/2.465E19)

                   QY(I, IW) = FACTOR * QY(I, IW)
                   QY(I, IW) = MIN(1.0, QY(I, IW))
                   QY(I, IW) = MAX(0.0, QY(I, IW))

                   SQ(I, IW)  = XC(I,IW) * QY(I, IW)
               ENDDO
           ENDDO

           REPLACE = .TRUE.

         CASE(  'PAN', 'PAN_IUPAC04', 'PAN_IUPAC10' )  ! 'PAN + hv -> PRODUCTS'

! temperature correction to cross-section 

           DO IW = 1, NW
               DO I = 1, NZ
                    FACTOR    = EXP(PAN_XCROSS_B(IW)*(TLEV(I)-T298K))
              print*,JLABEL,TLEV(I)-T298K,factor* XC(I, IW), XCROSS(I,IW) 
                    XC(I, IW) = FACTOR * XC(I, IW)
                    SQ(I, IW) = XC(I,IW) * QY(I, IW)
               ENDDO
           ENDDO


           REPLACE = .TRUE.

         CASE(  'PAN1_RACM2', 'PAN2_RACM2'  )  ! 'PAN + hv -> PRODUCTS'


! temperature correction to cross-section 

           DO IW = 1, NW
               DO I = 1, NZ
                    FACTOR    = EXP(PAN_XCROSS_B(IW)*(TLEV(I)-T298K))
              print*,JLABEL,TLEV(I)-T298K,factor* XC(I, IW), XCROSS(I,IW) 
                    XC(I, IW) = FACTOR * XC(I, IW)
                    SQ(I, IW) = XC(I,IW) * QY(I, IW)
               ENDDO
           ENDDO


           REPLACE = .TRUE.

         CASE( 'C2CHO', 'ALD_RACM2', 'ALDX_R_IUPAC10' )  ! 'C2H5CHO -> C2H5 + HCO'

! density correction to quantum yield

           DO IW = 1, NW
               DO I = 1, NZ

                  IF(QY(I,IW) .LE. 1.0E-6) THEN
                      FACTOR = 1.0
                  ELSE
                      FACTOR = 1.0
     &                       / (1.0 + (1.0/QY(I,IW) - 1.0)*DENS(I)/2.465E19)
                  ENDIF

                  QY(I, IW) = FACTOR * QY(I, IW)
                  QY(I, IW) = MIN(1.0, QY(I, IW))
                  QY(I, IW) = MAX(0.0, QY(I, IW))
                  SQ(I, IW) = XC(I,IW) * QY(I, IW)

               ENDDO
           ENDDO


           REPLACE = .TRUE.

         CASE(  'NO3NO2-6', 'NO3NO2_6', 'NO3NO2_06', 'NO3NO2_RACM2' ) ! 'NO3 -> NO2 + O(3P)'

! temperature correction to cross-section and quantum yield

           DO IW = 1, NW
               DO I = 1, NZ
                  FACTOR = (1.0-EXP(-1096.4/TLEV(I)) - 2.0*EXP(-529.5/TLEV(I)))
     &                   / (1.0-EXP(-1096.4/ T298K ) - 2.0*EXP(-529.5/ T298K ))

                  XC(I, IW) = FACTOR * XC(I, IW)

              print*,JLABEL,TLEV(I)-T298K,XC(I, IW) , XCROSS(I,IW)
                  QY(I, IW) = ADJ_QY_NO3_NO2(IW,I) * QY(I, IW)
                  QY(I, IW) = MIN(1.0, QY(I, IW))
                  QY(I, IW) = MAX(0.0, QY(I, IW))

                  SQ(I, IW) = XC(I,IW) * QY(I, IW)

               ENDDO
           ENDDO

           REPLACE = .TRUE.

         CASE( 'HNO3', 'HNO3_IUPAC04', 'HNO3_IUPAC10', 'HNO3_RACM2' )

! temperature correction to cross-section 

            DO IW = 1, NW
               DO I = 1, NZ

                  FACTOR = EXP(HNO3_XCROSS_EXP(IW)*(TLEV(I)-T298K))

              print*,JLABEL,TLEV(I)-T298K,factor* XC(I, IW), XCROSS(I,IW) 

                  XC(I, IW)  = FACTOR * XC(I, IW)

                  SQ(I, IW)  = XC(I, IW) * QY(I,IW)
 
               ENDDO
             ENDDO


             REPLACE = .TRUE.

         CASE( 'MVK-06', 'ISPD', 'MVK_06', 'MVK_RACM2' )

C quantum yield correction from
C Gierczak, T., J. B. Burkholder, R. K. Talukdar, A. Mellouki, S. B. Barone,
C and A. R. Ravishankara, Atmospheric fate of methyl vinyl ketone and methacrolein,
C J. Photochem. Photobiol A: Chemistry, 110 1-10, 1997.
C depends on pressure and wavelength, set upper limit to 1.0
C However, chamber evaluations for SAPRC07T require a pressure correction where
C number density coefficient is five times higher.
         DO IW = 1, NW
            DO I = 1, NZ

               FACTOR = (5.5 + 5.0*9.2E-19*2.465E+19)
     &                / (5.5 + 5.0*9.2E-19*DENS(I))
               
 
               QY(I, IW) = FACTOR * QY(I, IW)
               QY(I, IW) = MIN(1.0, QY(I, IW))
               QY(I, IW) = MAX(0.0, QY(I, IW))
               SQ(I, IW) = XC(I,IW)* QY(I, IW)

            ENDDO
         ENDDO


         REPLACE = .TRUE.

       CASE( 'MACR-06', 'MACR_06', 'MACR_RACM2' )

C quantum yield based on 2.76 times MVK from
C Gierczak, T., J. B. Burkholder, R. K. Talukdar, A. Mellouki, S. B. Barone,
C and A. R. Ravishankara, Atmospheric fate of methyl vinyl ketone and methacrolein,
C J. Photochem. Photobiol A: Chemistry, 110 1-10, 1997.
C depends on pressure and wavelength, set upper limit to 1.0
C However, chamber evaluations for SAPRC07T require a pressure correction where
C number density coefficient is five times higher.

! density correction to quantum yield

         DO IW = 1, NW
            DO I = 1, NZ

               FACTOR = (5.5 + 5.0*9.2E-19*2.465E+19)
     &                / (5.5 + 5.0*9.2E-19*DENS(I))
               
               QY(I, IW) = FACTOR * QY(I, IW)
               QY(I, IW) = MIN(1.0, QY(I, IW))
               QY(I, IW) = MAX(0.0, QY(I, IW))
               SQ(I, IW) = XC(I,IW)* QY(I, IW)

            ENDDO
         ENDDO


         REPLACE = .TRUE.

       CASE(  'MEK-06', 'MEK_06')
C Quantum Yields from 
C Raber, W.H. (1992) PhD Thesis, Johannes Gutenberg-Universitaet, Mainz, Germany.
C other channels assumed negligible (less than 10%).
C Total quantum yield  = 0.38 at 760 Torr. but Carter
C adjusts to 0.175 based on chamber tests and sets the values in
C mechanism definition file.
! temperature/density correction to quantum yield

C Stern-Volmer form given:  1/phi = 0.96 + 2.22e-3*P(torr)
C     compute local pressure in torr
         DO IW = 1, NW
            DO I = 1, NZ
               IF( QY(I, IW) .GE. 1.0 )THEN
                   FACTOR = 1.0
C NOTE: Case for  SAPRC07T where quantum yield is one and adjusts the photolysis rates
C       by a multiplication factor in the mechanism definition file
               ELSE
                 PRESSURE = (1.03547E-19*DENS(I)*TLEV(I)) ! units TORR 
                 FACTOR   = (0.96 + 2.22E-3*760.0 )
     &                    / (0.96 + 2.22E-3*PRESSURE ) 
               ENDIF

               QY(I, IW) = FACTOR * QY(I, IW)
               QY(I, IW) = MIN(1.0, QY(I, IW))
               QY(I, IW) = MAX(0.0, QY(I, IW))
               SQ(I, IW) = XC(I,IW) * QY(I, IW)

            ENDDO
         ENDDO

         REPLACE = .TRUE.

      CASE( 'H2O2', 'H2O2_SAPRC99', 'H2O2_RACM2', 'H2O2_IUPAC10' )

!  Provide cross section and quantum yield for H2O2 photolysis 
!         H2O2 + hv -> 2 OH  between 260 and 350 nm            
!  Otherwise use Cross section from file
            DO I = 1, NZ
               DO IW = 1, NW

                  IF(WC(IW) .GE. 260.0 .AND. WC(IW) .LE. 350.0)THEN 
!  compute cross section at TLEV
                     CHI = 1.0 / (1.0 + EXP(-1265.0/MAX(200.0, MIN(TLEV(I),400.0))) )
                     SIG = CHI*H2O2_XCROSS_A(IW) + (1.0 - CHI)*H2O2_XCROSS_B(IW)
!  compute correction factor normalized by value at T298K
                     CHI    = 1.0 / (1.0 + EXP(-1265.0/T298K) )
                     FACTOR = SIG
     &                      / (CHI*H2O2_XCROSS_A(IW) + (1.0 - CHI)*H2O2_XCROSS_B(IW))

                  ELSE

                     FACTOR = 1.0

                  ENDIF

              print*,JLABEL,TLEV(I)-T298K,factor* XC(I, IW), XCROSS(I,IW) 
                  XC(I, IW) = FACTOR * XC(I, IW)
                  SQ(I, IW) = XC(I, IW) * QY(I,IW)

               ENDDO
            ENDDO

            REPLACE = .TRUE.

      CASE( 'MGLY-06' , 'BACL-07', 'MGLY_06' , 'BACL_07', 'MGLY_IUPAC04')

! temperature/density correction to quantum yield

         DO IW = 1, NW
        
            DO I = 1, NZ

C               QY(I,IW) = MIN( QY(I,IW), 1.0)
C               QY(I,IW) = MAX( QY(I,IW), 0.0)

! Recommendation in NASA JPL (2011).  Computed pressures are differences 
! between layer pressures and pressure of interpolated values. Pressure units are Torrs.
               PRESSURE  = (1.03547E-19*DENS(I)*TLEV(I)) ! TORR 
               PRESSURE  = MIN(472.0, PRESSURE)

               IF(WC(IW) .LT. 500.0 .AND. WC(IW) .GT. 240.0)THEN
                 IF( WC( IW ) .LE. 370.0 )THEN                        
                    PRESSURE = MAX( 400.0, 1.03547E-19 * DENS(I) * TLEV(I)  )
     &                       - 2.5524 * TLEV(I)
                 ELSE
                    PRESSURE = ( 1.03547E-19 * DENS(I) - 2.5524 ) * TLEV(I)
                 END IF
                 IF( QY(I,IW) .GT. 0.0001 .AND. QY(I,IW) .LT. 0.9999 )THEN
                    CHI = QY(I,IW) 
                    SIG = 1.0 
     &                  /( 1.0 / QY( I, IW ) + 1.93E4 * EXP( -5639.0 / WC(IW) ) * PRESSURE )

                    FACTOR = SIG / CHI 
                 ELSE
                    FACTOR = 1.0
                 ENDIF
               ELSE   
                 FACTOR = 1.0
               ENDIF

               QY(I, IW) = FACTOR * QY(I, IW)
               QY(I, IW) = MIN(1.0, QY(I, IW))
               QY(I, IW) = MAX(0.0, QY(I, IW))
               SQ(I, IW) = XC(I,IW) * QY(I, IW)

            ENDDO
         ENDDO


         REPLACE = .TRUE.

      CASE(  'ACRO-09', 'ACRO_09', 'ACROLEIN_SAPRC99')

! density correction quantum yield

         DO IW = 1, NW

            DO I = 1, NZ

               QY(I,IW) = MIN( QY(I,IW), 1.0)
               QY(I,IW) = MAX( QY(I,IW), 0.0)

C  Number density dependence based on Gardner et. al (1997), 
C  J. Phys. Chem., vol 91, pages 1922. The application uses
C  the quantum yields set in in cross-section file. For 
C  SAPRC07T, yields set approximation four times NASA (2006)
C  because the mechanism developer sums over all possible channels and
C  Gardner et. al may support this conclusion. 

               IF(DENS(I) .GE. 8.0E+17)THEN
!!!!!!            QDUM = (4.0E-3 + 1.0/(8.6E-2 + 1.613E-17*DENS(I)))
!!!!!!&                /  0.006384
                 FACTOR = (4.0E-3 + 1.0/(8.6E-2 + 1.613E-17*DENS(I)))
     &                  / (4.0E-3 + 1.0/(8.6E-2 + 1.613E-17*DENS0  ))
               ELSE IF(DENS(I) .LT. 8.0E+17)THEN
!!!!!!!           FACTOR = 12.00713
                 FACTOR = (4.0E-3 + 1.0/(8.6E-2 + 1.613E-17*8.0E+17))
     &                  / (4.0E-3 + 1.0/(8.6E-2 + 1.613E-17*DENS0  ))
               ENDIF

               QY(I, IW) = FACTOR * QY(I, IW)
               QY(I, IW) = MIN(1.0, QY(I, IW))
               QY(I, IW) = MAX(0.0, QY(I, IW))
               SQ(I, IW) = XC(I,IW) * QY(I, IW)                           

             ENDDO
         ENDDO

         REPLACE = .TRUE.

      CASE(  'HCHOR-06', 'HCHOR_06', 'FORM_R_IUPAC10', 'HCHO_R_SAPRC99', 'HCHO_RAD_RACM2' )  ! 'CH2O -> H + HCO' 


! temperature correction to cross-section 

          DO IW = 1, NW
             DO I = 1, NZ

               CHI = HCHO_XCROSS_300K(IW) + HCHO_XCROSS_A(IW)*(T298K-300.0)

               IF(TLEV(I) .LT. 300.0 .AND. TLEV(I) .GT. 195.0)THEN
                   SIG = HCHO_XCROSS_300K(IW) + HCHO_XCROSS_A(IW)*(TLEV(I)-300.0)
               ELSE IF( TLEV(I) .LE. 195.0)THEN
                   SIG = HCHO_XCROSS_300K(IW) - HCHO_XCROSS_A(IW)*105.0
               ELSE
                   SIG = HCHO_XCROSS_300K(IW)
               ENDIF
               
               IF( CHI .GT. 0.0 )THEN
                   FACTOR = MAX( SIG, 0.0)/ CHI
               ELSE
                   FACTOR = 1.0
               END IF

               XC(I, IW) = FACTOR * XC(I, IW)
              print*,JLABEL,TLEV(I)-T298K,XC(I, IW), XCROSS(I,IW) 

               QY(I, IW) = ADJ_HCHO_QUANTR(IW, I) * QY(I, IW)
               QY(I, IW) = MIN(1.0, QY(I, IW))
               QY(I, IW) = MAX(0.0, QY(I, IW))

               SQ(I, IW) = XC(I,IW) * QY(I, IW)                           

             ENDDO
          ENDDO


          REPLACE = .TRUE.

      CASE(  'HCHOM-06', 'HCHOM_06', 'FORM_M_IUPAC10', 'HCHO_M_SAPRC99', 'HCHO_MOL_RACM2' )  ! 'CH2O -> H2 + CO'

! temperature correction to cross-section 

          DO IW = 1, NW
             DO I = 1, NZ
               CHI = HCHO_XCROSS_300K(IW) + HCHO_XCROSS_A(IW)*(T298K-300.0)

               IF(TLEV(I) .LT. 300.0 .AND. TLEV(I) .GT. 195.0)THEN
                   SIG = HCHO_XCROSS_300K(IW) + HCHO_XCROSS_A(IW)*(TLEV(I)-300.0)
               ELSE IF( TLEV(I) .LE. 195.0)THEN
                   SIG = HCHO_XCROSS_300K(IW) - HCHO_XCROSS_A(IW)*105.0
               ELSE
                   SIG = HCHO_XCROSS_300K(IW)
               ENDIF
               
               IF( CHI .GT. 0.0 )THEN
                   FACTOR = MAX( SIG, 0.0)/ CHI
               ELSE
                   FACTOR = 1.0
               END IF
              print*,JLABEL,TLEV(I)-T298K,factor* XC(I, IW), XCROSS(I,IW) 

               XC(I, IW) = FACTOR * XC(I, IW)

!               QY(I, IW) = ADJ_HCHO_QUANTM(IW,I) * QY(I, IW)
               IF(WC(IW) .GE. 330.0 .AND. HCHO_QUANTM_STP(IW) .GT. 0.0)THEN

                  QDUM = 1.0/HCHO_QUANTM_STP(IW)        ! need to subst actual value in QY 
                  BETA = 1.0/(1.0-HCHO_QUANTR_STP(IW))  ! need to subst actual value in QY 

                  IF( TLEV(I) .LT. 300.0 .AND. TLEV(I) .GT. 220.0)THEN
                      PRESSURE = 1.36312E-22*DENS(I)*TLEV(I)
                      ALPHA = (QDUM - BETA)
     &                      * (1.+0.05*(WC(IW)-329.0)*((TLEV(I)-80.0)/80.0))                   
                  ELSEIF( TLEV(I) .LE. 220.0)THEN
                      PRESSURE = 3.0E-20*DENS(I)
                      ALPHA = (QDUM - BETA) 
     &                      * (1.+0.0875*(WC(IW)-329.0))
                  ELSEIF( TLEV(I) .GE. 300.)THEN
                      PRESSURE = 4.09E-20*DENS(I)
                      ALPHA = (QDUM - BETA)
     &                      * (1.+0.1375*(WC(IW)-329.0))
                  ENDIF
                  
! reduce wavelength dependence
                  FACTOR      = (BETA + 3.3601E-3 * 298.0 *ALPHA)
     &                        / (BETA + PRESSURE*ALPHA)

               ELSE

                  FACTOR = 1.0
               ENDIF

               QY(I, IW) = FACTOR * QY(I, IW)
               QY(I, IW) = MIN(1.0, QY(I, IW))
               QY(I, IW) = MAX(0.0, QY(I, IW))

               SQ(I, IW) = XC(I,IW) * QY(I, IW)                           

           ENDDO
          ENDDO

          REPLACE = .TRUE.

      CASE(  'O3O1D-06', 'O3O1D_06', 'O3_O1D_IUPAC10', 'O3_O1D_IUPAC04','O3O1D_NASA06' ) ! 'O3 -> O2 + O(1D)'

! temperature correction to cross-section 

          DO IW = 1, NW
             DO I = 1, NZ

               FACTOR     = MAX( ADJ_O3_XCROSS(IW,I), 0.0)
               XC(I, IW)  = FACTOR * XC(I, IW)

               print*,JLABEL,TLEV(I)-T298K,WC(IW),XC(I,IW), XCROSS(I,IW),ADJ_O3_XCROSS(IW,I) 

! the below IF block forces the yields of O3P and O1D to add to one
! assumes that NZ and NW does not change between subroutine calls                  

               IF( .NOT. COMPUTED_O3O3P .AND. .NOT. COMPUTED_O3O1D )THEN
                  FACTOR     = MAX( ADJ_O3O1D_QUANT(IW,I), 0.0)
                  FACTOR     = 1.0
                  QY(I, IW)  = FACTOR * QY(I, IW)
                  QY(I, IW)  = MIN( QY(I, IW), 1.0)
                  O3O1D_YIELD( I, IW ) = QY(I, IW)
                  IF( IW .GE. NW .AND. I .GE. NZ )COMPUTED_O3O1D = .TRUE.
               ELSE IF( COMPUTED_O3O3P .AND. .NOT. COMPUTED_O3O1D )THEN
                  QY(I, IW) = MAX((1.0-O3O3P_YIELD( I, IW )), 0.0)
                  O3O1D_YIELD( I, IW ) = QY(I, IW)
                  IF( IW .GE. NW .AND. I .GE. NZ )COMPUTED_O3O1D = .TRUE.
               ELSE IF( COMPUTED_O3O1D )THEN
                  QY(I, IW) = O3O1D_YIELD( I, IW )
               END IF
 
                  print*,JLABEL,TLEV(I)-T298K,WC(IW), QY(I, IW), QUANT(I,IW)
               SQ(I, IW) = XC(I,IW) * QY(I, IW)                           

           ENDDO
          ENDDO


          REPLACE = .TRUE.

      CASE(  'O3O3P-06', 'O3O3P_06', 'O3_O3P_IUPAC10', 'O3_O3P_IUPAC04', 'O3O3P_NASA06' ) ! 'O3 -> O2 + O(3P)'

! temperature correction to cross-section 

          DO IW = 1, NW
             DO I = 1, NZ

               FACTOR     = MAX( ADJ_O3_XCROSS(IW,I), 0.0)
               XC(I, IW)  = FACTOR * XC(I, IW)

               print*,JLABEL,TLEV(I)-T298K,WC(IW),XC(I,IW), XCROSS(I,IW),ADJ_O3_XCROSS(IW,I) 

! the below IF block forces the yields of O3P and O1D to add to one
! assumes that NZ and NW does not change between subroutine calls                  

               IF( .NOT. COMPUTED_O3O3P .AND. .NOT. COMPUTED_O3O1D )THEN
                  FACTOR     = MAX( ADJ_O3O3P_QUANT(IW,I), 0.0)
                  QY(I, IW)  = FACTOR * QY(I, IW)
                  QY(I, IW)  = MIN( QY(I, IW), 1.0)
                  O3O3P_YIELD( I, IW ) = QY(I, IW)
                  IF( IW .GE. NW .AND. I .GE. NZ )COMPUTED_O3O3P = .TRUE.
                  print*,JLABEL,TLEV(I)-T298K, QY(I, IW), QUANT(I,IW)
               ELSE IF( .NOT. COMPUTED_O3O3P .AND. COMPUTED_O3O1D )THEN
                  QY(I, IW) = MAX((1.0-O3O1D_YIELD( I, IW )), 0.0)
                  O3O3P_YIELD( I, IW ) = QY(I, IW)
                  IF( IW .GE. NW .AND. I .GE. NZ )COMPUTED_O3O3P = .TRUE.
                  print*,JLABEL,TLEV(I)-T298K, WC(IW), QY(I, IW), QUANT(I,IW), O3O1D_YIELD( I, IW )
               ELSE IF( COMPUTED_O3O3P )THEN
                  QY(I, IW) = O3O3P_YIELD( I, IW )
                  print*,JLABEL,TLEV(I)-T298K, QY(I, IW), QUANT(I,IW)
               END IF
                             
              SQ(I, IW) = XC(I,IW) * QY(I, IW)                           

           ENDDO
          ENDDO


          REPLACE = .TRUE.

      CASE(  'ACET-06', 'ACET_06', 'ACET_IUPAC10', 'ACETONE', 'ACT_RACM2' ) ! 'CH3COCH3 -> products'

          DO IW = 1, NW
             DO I = 1, NZ

! temperature correction to cross-section 

              SIG = XC_D_ACETONE(IW)
     &            + XC_A_ACETONE(IW)*TLEV(I)
     &            + XC_B_ACETONE(IW)*TLEV(I)**2
     &            + XC_C_ACETONE(IW)*TLEV(I)**3

              CHI = XC_D_ACETONE(IW)
     &            + XC_A_ACETONE(IW)*T298K
     &            + XC_B_ACETONE(IW)*T298K**2
     &            + XC_C_ACETONE(IW)*T298K**3
     
              IF( CHI .GT. 0.0 )FACTOR = MAX(SIG/CHI, 0.0)
              
              XC(I, IW) = FACTOR * XC(I, IW)
              print*,JLABEL,TLEV(I)-T298K,XC(I, IW) , XCROSS(I,IW)

! temperature/density correction to quantum yield

              IF( QY_ACETONE(T298K, DENS0, WC(IW)) .GT. 1.0E-5)THEN

                  FACTOR = QY_ACETONE(TLEV(I),DENS(I),WC(IW))
     &                   / QY_ACETONE(T298K, DENS0, WC(IW))

              ELSE
                  FACTOR = 0.0
              END IF

              QY(I, IW) = MAX(FACTOR, 0.0) * QY(I, IW)
              QY(I, IW) = MIN(1.0, QY(I, IW))

              SQ(I, IW) = XC(I,IW) * QY(I, IW)                           

           ENDDO
          ENDDO

          REPLACE = .TRUE.

       CASE(  'CL2', 'CL2_IUPAC04')

! NASA (2006) and IUPAC(2005) recommended cross-section as a function of
! wavelength and temperature taken from
! D. Maric et al. (1993) J. Photochem. Photobiol. A: Chem. 70, 205.

          DO IW = 1, NW
             DO I = 1, NZ

! temperature correction to cross-section 

               TDUM = TLEV(I)
               IF(TLEV(I) .GT. 300.0)THEN
                  TDUM = 300.0
               ELSEIF(TLEV(I) .LT. 195.0)THEN
                  TDUM = 195.0
               ELSE
                  TDUM = TLEV(I)
               ENDIF    

! may be able to simplfy using definition of tanh and wavelength independence
               ALPHA = TANH(470.676/TDUM)
               BETA  = TANH(470.676/T298K)
               
               IF(WC(IW) .GT. 550.0)THEN
                  FACTOR = 1.0
               ELSEIF(WC(IW) .LT. 250.0)THEN
                  FACTOR = 1.0
               ELSE
                  WDUM = WC(IW)
                  SIG = SQRT(ALPHA)
     &                * (27.3 *EXP(-99.0*ALPHA*(LOG(329.5/WDUM))**2)
     &                +  0.932*EXP(-91.5*ALPHA*(LOG(406.5/WDUM))**2))
                  CHI = SQRT(BETA)
     &                * (27.3 *EXP(-99.0*BETA*(LOG(329.5/WDUM))**2)
     &                +  0.932*EXP(-91.5*BETA*(LOG(406.5/WDUM))**2))
                  FACTOR = SIG / CHI
               ENDIF 
               print*,JLABEL,TLEV(I)-T298K,factor * XC(I, IW), XCROSS(I,IW)

! IUPAC (2005) and NASA (2006) recommend quantum yield equal to one when
! cross-section is nonzero but file's values

               XC(I, IW) = FACTOR * XC(I, IW)
               SQ(I, IW) = XC(I,IW) * QY(I, IW)                           

           ENDDO
         ENDDO

         REPLACE = .TRUE.

      CASE DEFAULT

          DO IW = 1, NW
             DO I = 1, NZ
               SQ(I, IW)  = XC(I,IW)*QY(I,IW)
           ENDDO
          ENDDO

          REPLACE = .FALSE.

      END SELECT 

      FIRSTCALL = .FALSE.

      RETURN
      END
C
      FUNCTION OZONE_YIELD(W, T)
!-----------------------------------------------------------------------------*
!    taken from Tropospheric Ultraviolet-Visible (TUV) radiation model                 =*
!    Version 4.5                                                            =*
!    Sep 2007                                                               =*
!-----------------------------------------------------------------------------*
!  PURPOSE:                                                                 =*
* function to calculate the quantum yield O3 + hv -> O(1D) + O2,             =*
* according to:                                                             
* Matsumi, Y., F. J. Comes, G. Hancock, A. Hofzumanhays, A. J. Hynes,
* M. Kawasaki, and A. R. Ravishankara, QUantum yields for production of O(1D)
* in the ultraviolet photolysis of ozone:  Recommendation based on evaluation
* of laboratory data, J. Geophys. Res., 107, 10.1029/2001JD000510, 2002.
!-----------------------------------------------------------------------------*
! TUV model developed by Sasha Madronich with important contributions from:           =*
! Chris Fischer, Siri Flocke, Julia Lee-Taylor, Bernhard Meyer,             =*
! Irina Petropavlovskikh,  Xuexi Tie, and Jun Zen.                          =*
!              To contact the author, write to:                             =*
! Sasha Madronich, NCAR/ACD, P.O.Box 3000, Boulder, CO, 80307-3000, USA  or =*
! send email to:  sasha@ucar.edu  or tuv@acd.ucar.edu                       =*
!-----------------------------------------------------------------------------*
! This program is free software; you can redistribute it and/or modify      =*
! it under the terms of the GNU General Public License as published by the  =*
! Free Software Foundation;  either version 2 of the license, or (at your   =*
! option) any later version.                                                =*
! The TUV package is distributed in the hope that it will be useful, but    =*
! WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTIBI-  =*
! LITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public     =*
! License for more details.                                                 =*
! To obtain a copy of the GNU General Public License, write to:             =*
! Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.   =*
!-----------------------------------------------------------------------------*
! Copyright (C) 1994,95,96,97,98,99,2000,01,02,03, 04, 05, 06, 07           =*
! by the University Corporation for Atmospheric Research                    =*
!-----------------------------------------------------------------------------*
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

!============================================================================*

      SUBROUTINE JH2O2_260T350NM(WC,TEMP,AIRDEN,XCROSS,QUANT)
!-----------------------------------------------------------------------------*
!    taken from Tropospheric Ultraviolet-Visible (TUV) radiation model                 =*
!    Version 4.5                                                            =*
!    Sep 2007                                                               =*
!-----------------------------------------------------------------------------*
!  PURPOSE:                                                                 =*
!  Provide cross section and quantum yield for H2O2 photolysis              =*
!         H2O2 + hv -> 2 OH  between 260 and 350 nm                                               =*
!  Otherwise use Cross section from JPL97, tabulated values @ 298K 
!  Quantum yield:  Assumed to be unity                                      =*
!-----------------------------------------------------------------------------*
!  PARAMETERS:                                                              =*
!  WC     - REAL, center points of wavelength interval                   (I)=*
!  TLEV   - REAL, temperature (K) at  altitude level                     (I)=*
!  AIRDEN - REAL, air density (molec/cc) at altitude level               (I)=*
!  xcross - cross section (cm^2) for each                               (IO)=*
!           photolysis reaction defined, at  input wavelength       and     =*
!           at  defined altitude level                                      =*
!  quant -  quantum yield for each                                      (IO)=*
!           photolysis reaction defined, at  input wavelength       and     =*
!           at  defined altitude level                                      =*
!  JLABEL - CHARACTER*50, string identifier for each photolysis reaction (L)=*
!           defined                                                         =*
!-----------------------------------------------------------------------------*

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

!-----------------------------------------------------------------------------*
!  PURPOSE:                                                                 =*
!  Provide cross section  and quantum yields for CH2O photolysis =*
!        (a) CH2O + hv -> H + HCO                                           =*
!        (b) CH2O + hv -> H2 + CO                                           =*
!  Based on recommendations from NASA JPL (2006) 
!-----------------------------------------------------------------------------*
!  PARAMETERS:                                                              =*
!  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
!           wavelength grid                                                 =*
!  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
!           working wavelength grid                                         =*
!  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
!           working wavelength grid                                         =*
!  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
!  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
!  AIRDEN - REAL, air density (molec/cc) at each altitude level          (I)=*
!  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
!  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
!           photolysis reaction defined, at each defined wavelength and     =*
!           at each defined altitude level                                  =*
!  JLABEL - CHARACTER*50, string identifier for each photolysis reaction (O)=*
!           defined                                                         =*
!-----------------------------------------------------------------------------*

      USE CSQY_REFER_DATA

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

!============================================================================*

      SUBROUTINE NASA_NO3_QUANTAS(NW,WC,NZ,TLEV,AIRDEN,QYNO3_NO,
     &                            QYNO3_NO2)

!-----------------------------------------------------------------------------*
!  PURPOSE:                                                                 =*
!  Provide the quantum yield for                                            =*
!  both channels of NO3 photolysis:                                         =*
!          (a) NO3 + hv -> NO2 + O(3P)                                      =*
!          (b) NO3 + hv -> NO + O2                                          =*
!-----------------------------------------------------------------------------*
!  PARAMETERS:                                                              =*
!  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
!           wavelength grid                                                 =*
!  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
!           working wavelength grid                                         =*
!  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
!           working wavelength grid                                         =*
!  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
!  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
!  AIRDEN - REAL, air density (molec/cc) at each altitude level          (I)=*
!  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
!  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
!           photolysis reaction defined, at each defined wavelength and     =*
!           at each defined altitude level                                  =*
!  JLABEL - CHARACTER*50, string identifier for each photolysis reaction (O)=*
!           defined                                                         =*
!-----------------------------------------------------------------------------*

      USE CSQY_REFER_DATA

      IMPLICIT NONE


! Arguments:

      INTEGER, INTENT( IN )  :: NW
      INTEGER, INTENT( IN )  :: NZ
      REAL,    INTENT( IN )  :: TLEV(KZ)
      REAL,    INTENT( IN )  :: AIRDEN(KZ)
      REAL,    INTENT( IN )  :: WC(KW)
      REAL,    INTENT( OUT ) :: QYNO3_NO2(KW,KZ)
      REAL,    INTENT( OUT ) :: QYNO3_NO(KW,KZ)      


! Local: 

      INTEGER, PARAMETER :: KDATA = 350

      CHARACTER(LEN=120) :: FILE_LINE
      CHARACTER( 50 )    :: JLABEL(3)

      REAL               :: SQ(1,KZ,KW)
      REAL               :: X1(KDATA)
      REAL               :: Y1(KDATA),Y2(KDATA),Y3(KDATA)
      REAL               :: Z1(KDATA),Z2(KDATA),Z3(KDATA)
      REAL               :: QY1(KDATA),QY2(KDATA)
      REAL               :: WL(KW), WU(KW)
      REAL               :: YG(KW), YG1(KW), YG2(KW)
      REAL               :: TEMP_ADJ(KZ)
      REAL               :: SLOPE
      REAL               ::  QY

      INTEGER            :: IROW, ICOL
      INTEGER            :: I, IW, J, N, IDUM
      INTEGER            :: IERR
      INTEGER            :: MABS

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
      END DO

      DO I = 1, NW
! compute upper limit of wavelength bins
          WU(I) = 2.0*WC(I) - WL(I)
      END DO

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
         END DO

         DO IW = 1, NW ! correction factor accounts for units of interpolated data
            QYNO3_NO(IW,I)  = QY1(IW)*0.001
            QYNO3_NO2(IW,I) = QY2(IW)*0.001
         END DO
         
      END DO

      RETURN
      END
C
      REAL FUNCTION QY_ACETONE_OLD(TEMP, DENS_NUMB, LAMBDA)

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


             QY_ACETONE_OLD = PHI_CO + PHI_CH3CO

         ELSEIF(LAMBDA .LT. 248.0 .AND. LAMBDA .GT. 0.0)THEN ! set QY to 1.0
! based on IUPAC (2005) data sheet

               PHI_CO    = 0.05
               
               PHI_CH3CO = 0.95

               QY_ACETONE_OLD = PHI_CO + PHI_CH3CO

         ELSEIF(LAMBDA .GT. 349.0)THEN

               QY_ACETONE_OLD = 0.0

        ENDIF

         QY_ACETONE_OLD = MAX(0.0,MIN(1.0, QY_ACETONE_OLD))


        RETURN
        END FUNCTION QY_ACETONE_OLD
!///////////////////////////////////////////////////////////////////////

      REAL FUNCTION QY_ACETONE( TEMP, DENS_NUMB, LAMBDA )

!-----------------------------------------------------------------------
! Computes acetone quantum yields according to:
! IUPAC (2005) recommendation based on
! Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold, and M. P. Chipperfield
!       (2004), Pressure and temperature-dependent quantum yields for the
!       photodissociation of acetone between 279 and 327.5 nm, Geophys.
!       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!***arguments

      REAL, INTENT(IN) :: TEMP        ! air temperature, K
      REAL, INTENT(IN) :: DENS_NUMB   ! air number density, 1/cm^3
      REAL, INTENT(IN) :: LAMBDA      ! wavelength, nm

!***local

      REAL :: A0           ! 1st coef for qy
      REAL :: A1           ! 2nd coef for qy
      REAL :: A2           ! 3rd coef for qy
      REAL :: A3           ! 4th coef for qy
      REAL :: A4           ! 5th coef for qy
      REAL :: A5           ! 6th coef for qy
      REAL :: A6           ! 7th coef for qy

      REAL :: PHI_CO       ! CO branch of IUPAC (2005) acetone QYZ
      REAL :: PHI_CH3CO    ! CH3CO branch of IUPAC (2005) acetone QYZ
      REAL :: AA           ! scratch variable for IUPAC (2005) acetone QYZ
      REAL :: BB           ! scratch variable for IUPAC (2005) acetone QYZ
      REAL :: CC           ! scratch variable for IUPAC (2005) acetone QYZ

      REAL :: TEMP_ARGUE   ! temperature over 295 K
      REAL :: INV_LAMBDA   ! reciprocal of wavelength, 1.E+7/nm

      REAL, PARAMETER :: T295K = 295

      TEMP_ARGUE = T295K / TEMP
      INV_LAMBDA = 1.0E+7 / LAMBDA
      IF ( LAMBDA .GE. 248.0 .AND. LAMBDA .LE. 349.0 ) THEN

         AA = 0.350 * TEMP_ARGUE**(1.28)
         BB = 0.068 * TEMP_ARGUE**(2.65)
         A0 = ( AA / ( 1.0 - AA ) ) * EXP( BB * ( LAMBDA - 248.0 ) )
         PHI_CO = 1.0 / ( 1.0 + A0 )

         IF ( LAMBDA .LE. 302.0 ) THEN

!***wavelengths 248-302 nm

            AA = 1.600 * 1.0E-19 * TEMP_ARGUE**(2.38)
            BB =  0.55 * 1.0E-03 * TEMP_ARGUE**(3.19)
            A1 = AA * EXP( -BB * ( INV_LAMBDA - 33113.0 ) )
            PHI_CH3CO = ( 1.0 - PHI_CO ) / ( 1.0 + A1*DENS_NUMB )

!***wavelengths 302-349 nm

         ELSE

            AA = 1.62 * 1.0E-17 * TEMP_ARGUE**(10.03)
            BB = 1.79 * 1.0E-3  * TEMP_ARGUE**(1.364)
            A2 = AA * EXP( -BB * ( INV_LAMBDA  - 30488.0 ) )

            AA = 26.29 * TEMP_ARGUE**(6.59)
            BB = 5.72 * 1.0E-7 * TEMP_ARGUE**(2.93)
            CC =   30006.0     * TEMP_ARGUE**(0.064)
            A3 = AA * EXP( -BB * (INV_LAMBDA - CC )**2.0 )

            AA = 1.67 * 1.0E-15 * TEMP_ARGUE**(7.25)
            BB = 2.08 * 1.0E-3  * TEMP_ARGUE**(1.16)
            A4 = AA * EXP( -BB * ( INV_LAMBDA - 30488.0 ) )

            PHI_CH3CO = ( 1.0 - PHI_CO )
     &                * ( 1.0 + A4 * DENS_NUMB + A3 )
     &                / ( ( 1.0 + A2 * DENS_NUMB + A3 )
     &                * ( 1.0 + A4 * DENS_NUMB ) )
         END IF

         QY_ACETONE = PHI_CO + PHI_CH3CO

      ELSE IF ( LAMBDA .LT. 248.0 .AND. LAMBDA .GT. 0.0 ) THEN ! set QY to 1.0

!***based on IUPAC (2005) data sheet

         PHI_CO    = 0.05
         PHI_CH3CO = 0.95
         QY_ACETONE = PHI_CO + PHI_CH3CO

      ELSE IF ( LAMBDA .GT. 349.0 ) THEN

         QY_ACETONE = 0.0

      END IF

      QY_ACETONE = MAX( 0.0, MIN( 1.0, QY_ACETONE ) )

      RETURN
      END FUNCTION QY_ACETONE


! This file contains subroutines used for calculation of quantum yields for 
! various photoreactions:
!     qyacet - q.y. for acetone, based on Blitz et al. (2004)

!*****************************************************************************

      REAL FUNCTION QY_ACETONE_TUV(T, M, w)
!-----------------------------------------------------------------------------*
!    taken from Tropospheric Ultraviolet-Visible (TUV) radiation model      =*
!    Version 4.6                                                            =*
!-----------------------------------------------------------------------------*
! Compute acetone quantum yields according to the parameterization of:
! Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold, and M. P. Chipperfield 
!       (2004), Pressure and temperature-dependent quantum yields for the 
!       photodissociation of acetone between 279 and 327.5 nm, Geophys. 
!       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.

      IMPLICIT NONE

! input:
! T = temperature, K
! m = air number density, molec. cm-3
! w = wavelength, nm

      REAL w, T, M

! internal:

      REAL a0, a1, a2, a3, a4
      REAL b0, b1, b2, b3, b4
      REAL c3
      REAL cA0, cA1, cA2, cA3, cA4

! output
! fco = quantum yield for product CO
! fac = quantum yield for product CH3CO (acetyl radical)

      REAL fco, fac

!** set out-of-range values:
! use low pressure limits for shorter wavelengths
! set to zero beyound 327.5

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

!** CO (carbon monoxide) quantum yields:

      a0 = 0.350 * (T/295.)**(-1.28)
      b0 = 0.068 * (T/295.)**(-2.65)
      cA0 = exp(b0*(w - 248.)) * a0 / (1. - a0)

      fco = 1. / (1 + cA0)

!** CH3CO (acetyl radical) quantum yields:

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

!*******************************************************************************
