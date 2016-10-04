      SUBROUTINE CREATE_MODULE( WLIN, CS_IN, QY_IN, NWLIN, SPECTRA_NAME, SPECTRA_TYPE,
     &                    WLL_AVE, WLU_AVE, CS_AVE, QY_AVE, NWL_AVE )

      USE JPROC_PROFILE

      IMPLICIT NONE      
      
!      INCLUDE 'JVALPARMS.EXT'         ! jproc parameters


C...........ARGUMENTS and their descriptions

      CHARACTER( 1) SPECTRA_TYPE        ! spectra type
      CHARACTER(16) SPECTRA_NAME        ! spectra name or PHOTAB label

      INTEGER      NWL_AVE             ! number of intervals average 
      INTEGER      NWLIN               ! number of intervals CQin

      REAL         WLIN ( MXWLIN )     ! wl for CQin
      REAL         CS_IN( MXWLIN )     ! cross-section as f(WLIN)
      REAL         QY_IN( MXWLIN )     ! quantum yield as f(WLIN)
      REAL         WLL_AVE( MXWL )     ! lower limit on wl effective interval
      REAL         WLU_AVE( MXWL )     ! upper limit on wl effective interval
      REAL         CS_AVE(  MXWL )     ! cross-section as f(WL_AVE)
      REAL         QY_AVE(  MXWL )     ! quantum yield as f(WL_AVE)

C...........LOCAL VARIABLES and their descriptions:
      
      CHARACTER(16)   ::  PNAME  = 'CREATE_MODULE'    ! program name
      CHARACTER(80)   ::  MSG    = ' '                ! message

C...........PARAMETERS and their descriptions

      INTEGER, PARAMETER :: XSTAT2  = 2       ! Program ERROR exit status
      integer, parameter :: NBO = 100
      integer, parameter :: NSO = 40000
      integer, parameter :: NZO = 13550
      integer, parameter :: NJO = 18 

      INTEGER, SAVE      :: NB, J1, J2, K1, K2  ! array limits and markers
      INTEGER            :: I, J, K             ! index counters

      REAL         WLIN1( NSO )     ! lower limit on wl int CQin
      REAL         WLIN2( NSO )     ! upper limit on wl int CQin

      REAL(8), SAVE    :: SRB(15,NJO)
      REAL(8), SAVE    :: WBIN(NBO + 1 )
      REAL(8)          :: FBIN(NBO)
      REAL(8)          :: FFBIN(NJO)
      REAL(8)          :: ABIN(NBO)
      REAL(8)          :: AABIN(NJO)
      REAL(8)          :: CBIN(NBO)
      REAL(8)          :: CCBIN(NJO)
      REAL(8)          :: TEMP        ! temperature, K
      REAL(8)          :: WW       


      REAL(8), PARAMETER      :: TINY = 0.06

      INTEGER, SAVE    :: IJX(NBO)
      INTEGER          :: ITT
      INTEGER, SAVE    :: IJX_CALC(NBO)
      INTEGER          :: ITT_CALC

      REAL(8), SAVE    :: W(NSO),F(NSO)
      REAL             :: XCOUT(NSO), QYOUT(NSO)
      REAL,    SAVE    :: WL(NSO), WU(NSO), WC(NSO)
      INTEGER, SAVE    :: IBINJ(NSO)
      REAL(8)          :: XNEW

      CHARACTER(8)     :: TITLNEW

! FASTJX has 18 bins but the wavelength interval of the bins can overlap

      REAL, SAVE   :: STR_WV_FASTJX( NJO + 2 )  ! nm, starting wavelength of FASTJ intervals
      DATA STR_WV_FASTJX  
     &  /  177.5,  177.5, 177.5, 177.5, 202.5, 206.5, 209.5,  
     &     212.5,  215.5, 221.5, 233.0, 275.5, 286.5, 291.0,
     &     298.3,  307.5, 312.5, 320.3, 345.0, 412.5 /

      REAL, SAVE    :: END_WV_FASTJX( NJO + 2 )  ! nm, ending wavelength of FASTJ intervals
      DATA END_WV_FASTJX  
     &  /  202.5,  202.5, 202.5, 202.5, 206.5, 209.5,  212.5,  
     &     215.5,  221.5, 233.0, 275.5, 286.5, 291.0,  298.3,  
     &     307.5,  312.5, 320.3, 345.0, 412.5, 850.0 /

      INTEGER, SAVE  :: FASTJX_BIN( NJO + 2 )   ! FASTJX bin number for the intervals
      DATA FASTJX_BIN 
     &  /      1,      2,     3,     4,     5,     6,       7,
     &         8,     11,    10,     9,    10,    11,      12,
     &        13,     14,    15,    16,    17,    18 /
! Effective Center wavelengths for 18 bins of FASTJX, not mean bin edges
!w-eff (nm)      187.      191.      193.      196.      202.      208.
!                211.      214.      261.      267.      277.      295.
!                303.      310.      316.      333.      380.      574.

      REAL, SAVE   :: EFF_WV_FASTJX( NJO )  ! nm 
      DATA EFF_WV_FASTJX  
     &  / 187.0,   191.0, 193.0, 196.0, 202.0, 208.0,
     &    211.0,   214.0, 261.0, 267.0, 277.0, 295.0,
     &    303.0,   310.0, 316.0, 333.0, 380.0, 574.0 /

  
      integer, parameter :: nwv_regress = 27

      REAL, SAVE  ::  ENDWL_REGRESS( nwv_regress )       ! wavelength band upper limit
      REAL, SAVE  ::  MIDWL_REGRESS( nwv_regress )       ! wavelength midpoints
      REAL, SAVE  ::  STWL_REGRESS(  nwv_regress )       ! wavelength band lower limit

      REAL, ALLOCATABLE, SAVE  :: ENDWL_NEW( : )       ! wavelength band upper limit
      REAL, ALLOCATABLE, SAVE  :: MIDWL_NEW( : )       ! wavelength midpoints
      REAL, ALLOCATABLE, SAVE  :: STWL_NEW(  : )       ! wavelength band lower limit
      INTEGER, SAVE            :: NWV_NEW              ! number of new bins

      INTEGER                  :: STRT, FINI

      INTEGER, SAVE            :: MODULE_UNIT = 75

      LOGICAL, SAVE            :: FIRSTCALL  = .TRUE.

      REAL,    SAVE            :: TEMPERATURE(3)

      INTEGER                  :: IRXN

      CHARACTER(16), SAVE      :: PHOT_DONE( NPHOTAB )
      INTEGER,       SAVE      :: NPHOT_DONE
      LOGICAL,       SAVE      :: PHOT_PROCESS( NPHOTAB )

      IF( FIRSTCALL )THEN

          FIRSTCALL = .FALSE.

          PHOT_PROCESS  = .TRUE.
          
          open (1, file='wavel-bins.dat', status='OLD')

          SRB = 0.d0
          read(1,'(i5)') NB

          if (NB .gt. NBO)THEN
             PRINT*,'NB exceeds ',NBO,' in file wavel-bins.dat '
             stop
          ENDIF

          read(1,'(5x,f8.3)') (WBIN(I), I=1,NB+1)
          read(1,*)
          read(1,*)
          read(1,'(2x,15f5.1)') ((SRB(I,J),I=1,15),J=1,8)
          read(1,*)
          read(1,'(5x,i5)') (IJX(I),I=16,NB)
          close (1)

          write(6,'(2x,15f5.1)') ((SRB(I,J),I=1,15),J=1,8)

          open (2, file='solar-p05nm-UCI.dat', status='OLD')
          read(2,*)
          read(2,*)
          read(2,'(f10.4,e10.3)') (W(J),F(J), J=1,NSO)
          close (2)


! initialize the regress wavelength bins
         STWL_REGRESS(1)  = 287.5
         MIDWL_REGRESS(1) = 290.0
         ENDWL_REGRESS(1) = 292.5

         do i = 2, 23
            STWL_REGRESS(i) = STWL_REGRESS(i-1) + 5.0
            MIDWL_REGRESS(i) = MIDWL_REGRESS(i-1) + 5.0
            ENDWL_REGRESS(i) = ENDWL_REGRESS(i-1) + 5.0
         enddo

         STWL_REGRESS(24)  = ENDWL_REGRESS(23)
         MIDWL_REGRESS(24) = 450.0
         ENDWL_REGRESS(24) = 500.0
         do i = 25, nwv_regress
            STWL_REGRESS(i)  = ENDWL_REGRESS(i-1)
            MIDWL_REGRESS(i) = MIDWL_REGRESS(i-1) + 100.0
            ENDWL_REGRESS(i) = ENDWL_REGRESS(i-1) + 100.0
         enddo

         do i = 1, nwv_regress
            WRITE(6,'(i3,2(2x,f6.2))')i,STWL_REGRESS(i),ENDWL_REGRESS(i)
         enddo



! find where regress bins start and stop in FASTJX bins

         if( STR_WV_FASTJX( 1 ) .gt. STWL_REGRESS(1) )THEN
             print*,'ERROR 1 in finding starting point '
             stop
         endif

         if( END_WV_FASTJX( NJO + 2 ) .lt. STWL_REGRESS(1) )THEN
             print*,'ERROR 2 in finding starting point '
             stop
         endif
     
         LOOP_START: do J = 1, NJO + 2
             if( STR_WV_FASTJX( J ) .ge. STWL_REGRESS(1) )THEN
                 STRT = J - 1
                 EXIT LOOP_START
             ENDIF
            if( J .eq. (NJO + 2) )THEN
               print*,'ERROR 3 in finding starting point '
               stop
            endif
         ENDDO LOOP_START


         FINI = NJO + 2
         LOOP_STOP: do K = 1, NJO + 2
             if( END_WV_FASTJX( J ) .gt. ENDWL_REGRESS(nwv_regress) )THEN
                 FINI = K 
                 EXIT LOOP_STOP
             ENDIF
         ENDDO LOOP_STOP


         IF( FINI .EQ. ( NJO + 2 ) )THEN

             IF(INT(END_WV_FASTJX(FINI))
     &           .NE.INT(ENDWL_REGRESS(nwv_regress)))THEN
                print*,'Resetting ENDWL_REGRESS(last) to END_WV_FASTJX(last) '
                print*,' INT(END_WV_FASTJX(FINI)) = ',INT(END_WV_FASTJX(FINI))
                print*,' INT(ENDWL_REGRESS(nwv_regress)) = ',
     &                INT(ENDWL_REGRESS(nwv_regress))
                ENDWL_REGRESS(nwv_regress) = END_WV_FASTJX(FINI)
             ENDIF
      
         ENDIF
       
         NWV_NEW = STRT + nwv_regress + ( NJO + 2 - FINI )
      
         ALLOCATE(  STWL_NEW ( NWV_NEW ) )      
         ALLOCATE(  MIDWL_NEW ( NWV_NEW ) )      
         ALLOCATE(  ENDWL_NEW ( NWV_NEW ) ) 

         IF( FINI .EQ. ( NJO + 2 ) )THEN 
         
             STWL_NEW( 1:STRT )            = STR_WV_FASTJX( 1:STRT )
             STWL_NEW( (STRT+1):NWV_NEW )  = STWL_REGRESS( 1:nwv_regress )

             ENDWL_NEW( 1:(STRT-1) )       = END_WV_FASTJX( 1:(STRT-1) )
             ENDWL_NEW( STRT )             = STWL_REGRESS( 1 )
             ENDWL_NEW( (STRT+1):NWV_NEW ) = ENDWL_REGRESS( 1:nwv_regress )

         ELSE

             J = STRT + nwv_regress
             STWL_NEW( 1:STRT )      = STR_WV_FASTJX( 1:STRT )
             STWL_NEW( (STRT+1): J ) = STWL_REGRESS( 1:nwv_regress )
             STWL_NEW( J + 1 )     = ENDWL_REGRESS( nwv_regress )
             STWL_NEW( (J+2):NWV_NEW )  =  STR_WV_FASTJX( FINI:(NBO + 2 ) )

          
             ENDWL_NEW( 1:(STRT-1) )   = END_WV_FASTJX( 1:(STRT-1) )
             ENDWL_NEW( STRT )         = STWL_REGRESS( 1 )
             ENDWL_NEW( (STRT+1):J )   = ENDWL_REGRESS( 1:nwv_regress )
             ENDWL_NEW( J+1:NWV_NEW ) = END_WV_FASTJX( FINI:(NBO + 2) )

         ENDIF

         do i = 1, NWV_NEW
            WRITE(6,'(i3,2(2x,f6.2))')i,STWL_NEW(i),ENDWL_NEW(i)
         enddo


         DO I = 1, NSO
            WC( I ) = REAL(W( I ))
            WL( I ) = WC( I ) - 0.05
            WU( I ) = WC( I ) + 0.05
         ENDDO
          
         K1 = 1
         K2 = NSO
 
c---now assign bin #(I=1:77) to each p05nm microbin J (1:40000)

        IBINJ = 0
        do I=1,NB
           do J=1,NSO
              if (W(J) .gt. WBIN(I)) goto 11
           enddo
           J = NSO + 1
11         J1 = J
           do J=J1,NSO
              if (W(J) .gt. WBIN(I+1)) goto 12
           enddo
           J = NSO + 1
12         J2 = J-1
           do J=J1,J2
              IBINJ(J) = I
           enddo
        enddo

        IJX_CALC = 0
  
        DO I = 16, NB
           DO J = 1, NJO + 2
              IF(WBIN(I)+TINY .GE. STR_WV_FASTJX(J) 
     &                 .AND. WBIN(I)-TINY .LT. END_WV_FASTJX(J))THEN
                   IJX_CALC( I ) = FASTJX_BIN ( J )
              ENDIF
           ENDDO
           print*,I,' IJX_CALC(I) - IJX(I) = ', IJX_CALC(I) - IJX(I)
        ENDDO
        WRITE(6,'(i5,2x,i5,2x,F6.2,2x,F6.2)') 
     &       (I,IJX_CALC(I),WBIN(I),WBIN(I+1),I=16,NB)


        OPEN(MODULE_UNIT, FILE = 'INLINE_CSQY.DAT', STATUS = 'UNKNOWN')

         ITT = 0
         DO I = 240, 300, 30
            ITT = ITT + 1
            TEMPERATURE(ITT) = FLOAT(I)
         ENDDO

        PHOT_DONE   = ' '
        NPHOT_DONE  = 0
        DO J = 1, NPHOTAB
         DO I = 1, NPHOT_DONE
            IF( PHOTAB(J) .EQ. PHOT_DONE(I) )THEN
                WRITE(6,*)TRIM(PHOTAB(J)),' already treated by ',TRIM(PHOT_DONE(I))
                PHOT_PROCESS( J ) = .FALSE.
            ENDIF
         ENDDO
         NPHOT_DONE = NPHOT_DONE + 1
         PHOT_DONE(NPHOT_DONE) = PHOTAB(J)
        ENDDO


      WRITE(MODULE_UNIT,'(6X,A)')'MODULE CSQY_DATA'
      write(MODULE_UNIT,2003)      

      WRITE(MODULE_UNIT,'(6X,A)')'IMPLICIT NONE'
      write(MODULE_UNIT,2003)      

      WRITE(MODULE_UNIT,'(A)')'C.....PARAMETERS and their descriptions:'
      write(MODULE_UNIT,2003)      


      WRITE(MODULE_UNIT,1996)NPHOT_DONE
1996  format(6X,'INTEGER, PARAMETER :: NPHOT_REF = ',I3,' ! # ref phot reactions ')
      write(MODULE_UNIT,2003)

      WRITE(MODULE_UNIT,1997)N_TEMPERATURE
1997  format(6X,'INTEGER, PARAMETER :: NTEMP     = ',I3,' ! # ref temperatures ')
      write(MODULE_UNIT,2003)

      WRITE(MODULE_UNIT,1998)N_INLINE_BAND
1998  format(6X,'INTEGER, PARAMETER :: NWL_REF   = ',I3,' ! # ref wavelengths ')
      write(MODULE_UNIT,2003)

      WRITE(MODULE_UNIT,'(A)')'C...Names of the mapped photolysis reactions (available to chemical)'
      WRITE(MODULE_UNIT,'(A)')'C... mechanisms) and their pointers to the reference photolysis rxn'
      write(MODULE_UNIT,2003)
      write(MODULE_UNIT,2003)

         DO I = 1, NPHOT_DONE

            write(MODULE_UNIT,1999) PHOT_DONE(I), I, PHOT_DONE(I)
1999        FORMAT(6X,'INTEGER, PARAMETER :: I',A16, ' = ', I3, ' ! pointer to ', A16)

         ENDDO

         write(MODULE_UNIT,2003)

         DO I = 1, NPHOT_DONE

            write(MODULE_UNIT,2000) PHOT_DONE(I), PHOT_DONE(I)
2000        FORMAT(6X,'DATA PNAME_REF( I',A16, ' ) / ''', A16, ''' /')

        ENDDO

         write(MODULE_UNIT,2003)

         WRITE(MODULE_UNIT,'(A)')'C...Setup the Mapping from CMAQ chemical reactions to the reference data'
         write(MODULE_UNIT,2003)      

         WRITE(MODULE_UNIT,2024)NPHOT_DONE
2024     format(6X,'INTEGER, PARAMETER :: NPHOT_MAP = ',I3,' ! #  phot mapped reactions ')
         write(MODULE_UNIT,2003)

         WRITE(MODULE_UNIT,2025)
2025     format(6X,'CHARACTER(16), SAVE :: PNAME_MAP( NPHOT_MAP )')
         WRITE(MODULE_UNIT,2026)
2026     format(6X,'CHARACTER(16), SAVE :: PHOT_MAP( NPHOT_MAP )')
         write(MODULE_UNIT,2003)

         DO I = 1, NPHOT_DONE

            write(MODULE_UNIT,2010) I, I, PHOT_DONE(I), PHOT_DONE(I)
2010        FORMAT(6X,'DATA PNAME_MAP( ', I3, ' ), ', ' PHOT_MAP( ', I3, ' )  / ''',
     &             A16, ''', I', A16, ' / ')

         ENDDO

        write(MODULE_UNIT,2021) 
2021    format(6X,'REAL, SAVE :: TEMP_REF( NTEMP_REF, NPHOT_REF )    ! temperatures')
        write(MODULE_UNIT,2021) 
2022    format(6X,'REAL, SAVE :: CS_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! cross sections')
        write(MODULE_UNIT,2021) 
2023    format(6X,'REAL, SAVE :: QY_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! quantum yields')

        write(MODULE_UNIT,2003)
        write(MODULE_UNIT,2003)
        write(MODULE_UNIT,'(6X,A)')'INTEGER  :: IWLR  ! wavelength loop variable'
        write(MODULE_UNIT,'(6X,A)')'INTEGER  :: ITT   ! temperature loop variable'

        PHOT_DONE   = ' '
        NPHOT_DONE  = 0
         

      ENDIF ! FIRSTCALL


      IRXN = -1

      DO I = 1, NPHOTAB
         IF( SPECTRA_NAME .EQ. PHOTAB(I) )THEN
             IRXN  = I
             EXIT
         ENDIF
      ENDDO
      IF( IRXN .LE. 0 )THEN
        WRITE(6,*)'ERROR IN CREATE_MODULE: SPECTRA_NAME NOT in PHOTAB array '
        STOP
      ENDIF

      DO I = 1, NPHOT_DONE
         IF( SPECTRA_NAME .EQ. PHOT_DONE(I) )THEN
             WRITE(6,*)TRIM(SPECTRA_NAME),' already processed by ',TRIM(PHOT_DONE(I))
             RETURN
         ENDIF
      ENDDO
      NPHOT_DONE = NPHOT_DONE + 1
      PHOT_DONE(NPHOT_DONE) = SPECTRA_NAME


      CALL INTAVG(WLIN, CS_IN, NWLIN, SPECTRA_TYPE, WL, WU, XCOUT, NSO)
      CALL INTAVG(WLIN, QY_IN, NWLIN, SPECTRA_TYPE, WL, WU, QYOUT, NSO)

      DO I = 1, NSO
         IF(XCOUT(I).GT.0.0 .AND. AMOD(WL(I),2.0) .EQ. 0.0)THEN
!            WRITE(6,'(A16,2(1X,ES12.4))')SPECTRA_NAME,WL(I),XOUT(I)
         ENDIF
      ENDDO

      do ITT = 1, 3
        TEMP = TEMPERATURE(ITT)
!      do ITT = 240, 300, 30
!        TEMP = FLOAT(ITT)

c---now ready to do any flux-weighted means over the bins
         FBIN(:) = 0.d0
         ABIN(:) = 0.0d0  
         CBIN(:) = 0.0d0  

      do J=K1,K2
        K = J - K1 + 1

        call X_NEW(W(J), TEMP, XNEW, TITLNEW)


        I = IBINJ(J)
        if (I .gt. 0) then
          WW = W(J)
          FBIN(I) = FBIN(I) + F(J)
          ABIN(I) = ABIN(I) + F(J)*DBLE(XCOUT(J))
          CBIN(I) = CBIN(I) + F(J)*DBLE(QYOUT(J))
!          ABIN(I) = ABIN(I) + F(J)*XNEW
        endif
      enddo



      do I=1,NB
        if (FBIN(I) .gt. 0.d0) then
            ABIN(I) = ABIN(I)/FBIN(I)
            CBIN(I) = CBIN(I)/FBIN(I)
        endif
      enddo

c---write out UCI std 77-bin data
c      write(6,'(a10,f10.2,a)') ' Temp=',TT,' flx  O3T  O3D  NO2  NO2x'
c      write(6,'(i5,0p,2f10.3,1p,6e10.3)')  (I,WBIN(I),WBIN(I+1),FBIN(I)
c     &      ,XBIN(I),QBIN(I),YBIN(I),ZBIN(I),ABIN(I),I=1,NB)


c---combine fast-JX bins: 
c---    non-SR bands (16:NB) are assigned a single JX bin
c---    SR bands are split (by Opacity Distrib Fn) into a range of JX bins
        FFBIN(:) = 0.d0
        AABIN(:) = 0.d0
        CCBIN(:) = 0.d0



      FFBIN(:) = 0.d0
      do I=16,NB
!        J = IJX(I)
        J = IJX_CALC(I)
        FFBIN(J) = FFBIN(J) + FBIN(I)
        AABIN(J) = AABIN(J) + FBIN(I)*ABIN(I)
        CCBIN(J) = CCBIN(J) + FBIN(I)*CBIN(I)
      enddo
      do I=1,15
        do J=1,NJO
          FFBIN(J) = FFBIN(J) + FBIN(I)*SRB(I,J)
          AABIN(J) = AABIN(J) + FBIN(I)*ABIN(I)*SRB(I,J)
          CCBIN(J) = CCBIN(J) + FBIN(I)*CBIN(I)*SRB(I,J)
        enddo
      enddo


 

      NWL_AVE = NJO
      WLL_AVE = 0.0
      WLU_AVE = 0.0
      CS_AVE  = 0.0
      QY_AVE  = 0.0



      do J = 1, 8
        WLL_AVE( J ) = STR_WV_FASTJX( J )
        WLU_AVE( J ) = END_WV_FASTJX( J )
      enddo 



      do J = 9, NJO
        WLL_AVE( J ) = STR_WV_FASTJX( J + 2 )
        WLU_AVE( J ) = END_WV_FASTJX( J + 2 )
      enddo 


      do J=1,NJO
        if (FFBIN(J) .gt. 0.d0)THEN
            AABIN(J)    = AABIN(J)/FFBIN(J)
            CCBIN(J)    = CCBIN(J)/FFBIN(J)
            CS_AVE( J ) = AABIN(J)
            QY_AVE( J ) = CCBIN(J)
        endif
      enddo

      do J= 1, 7
         CS_PHOT( ITT, J, IRXN) = CS_AVE( NJO - 7 + J )
         QY_PHOT( ITT, J, IRXN) = QY_AVE( NJO - 7 + J ) 
      enddo

c---write out UCI fast-JX data bins
!      if (ITT .eq. 180) then
!      write(6,'(a6,i4,1p,6e10.3/10x,6e10.3/10x,6e10.3)')
!     &    'solflx',ITT, FFBIN
!      write(6,*) '==========fast-JX 18-bin cross-sections============='
!      endif

      write(6,'(a16,i4,1p,6e10.3/20x,6e10.3/20x,6e10.3)')
     &    TRIM(SPECTRA_NAME), INT(TEMP), AABIN

      enddo


      write(MODULE_UNIT,2003) 
2003  format(1x)
      write(MODULE_UNIT,2005)
2005  format(1x,'C...  reference temperatures (K)')
      write(MODULE_UNIT,2003) 

      write(MODULE_UNIT,2001) trim(PHOTAB(IRXN)), TEMPERATURE(1:3)
2001  format(6x,'DATA ( TEMP_REF( ITT, I',A,' ), ITT=1,3 ) / ',f5.1,', ',f5.1,', ',f5.1, ' /' )
      write(MODULE_UNIT,2003) 
      write(MODULE_UNIT,2007)

2007  format(1x,'C...  absorption cross sections')
      write(MODULE_UNIT,2003) 

      do i = 1, 3
        write(MODULE_UNIT,2011) trim(SPECTRA_NAME)
2011    format(6x,'DATA ( CS_REF( I', A,', 1, IWLR ), IWLR = 1, 7 ) /')
        write(MODULE_UNIT,2013)(CS_PHOT( I, J, IRXN), J = 1, 4)
2013    format(5x, '& ', 1pE12.6,', ', 1pE12.6, ', ',1pE12.6, ', ', 1pE12.6, ', ')
        write(MODULE_UNIT,2013)(CS_PHOT( I, J, IRXN), J = 5, 7)
2015    format('     & ', 1pE12.6, ', ', 1pE12.6, ', ', 1pE12.6, ' /')
      end do

      write(MODULE_UNIT,2003) 
      write(MODULE_UNIT,2009)
2009  format(1x,'C...  quantum yields')
      write(MODULE_UNIT,2003) 

      do i = 1, 3
        write(MODULE_UNIT,2017) trim(SPECTRA_NAME)
2017    format(6x,'DATA ( QY_REF( I', A,', 1, IWLR ), IWLR = 1, 7 ) /')
        write(MODULE_UNIT,2013)(QY_PHOT( I, J, IRXN), J = 1, 4)
        write(MODULE_UNIT,2013)(QY_PHOT( I, J, IRXN), J = 5, 7)
      end do

      IF(IRXN .GE. NPHOTAB)THEN
        write(MODULE_UNIT,2003)
        write(MODULE_UNIT,2003)
        WRITE(MODULE_UNIT,'(6X,A)')'END MODULE CSQY_DATA'
      ENDIF

1001  format(A16,22X,F5.1,2X,F5.1)
1003  format(61x,f9.1)


        print*, ' returning for effective average ',SPECTRA_NAME
      RETURN
      END
