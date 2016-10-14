      SUBROUTINE WVBIN_AVERAGE(WL_CS_IN, CS_IN, NWL_CS_IN,  
     &                         WL_QY_IN, QY_IN, NWL_QY_IN,  
     &                         SPECTRA_TYPE,
     &                         WLL_AVE, WLU_AVE, NWL_AVE, 
     &                         CS_AVE, QY_AVE )

      USE CSQY_PARAMETERS
      USE BIN_DATA

      IMPLICIT NONE      
      
!      INCLUDE 'JVALPARMS.EXT'         ! jproc parameters


C...........ARGUMENTS and their descriptions

          CHARACTER(1), INTENT( IN ) :: SPECTRA_TYPE        ! spectra type
          INTEGER, INTENT( IN )      :: NWL_AVE             ! number of intervals average 
          INTEGER, INTENT( IN )      :: NWL_CS_IN           ! number of intervals CS_IN
          INTEGER, INTENT( IN )      :: NWL_QY_IN           ! number of intervals CS_IN
          REAL, INTENT( IN )         :: WL_CS_IN( : )  ! wl for CS_IN
          REAL, INTENT( IN )         :: WL_QY_IN( : )  ! wl for QY_IN
          REAL, INTENT( IN )         :: CS_IN( : )     ! cross-section as f(WLIN)
          REAL, INTENT( IN )         :: QY_IN( : )     ! quantum yield as f(WLIN)
          REAL, INTENT( INOUT )      :: WLL_AVE( : )   ! lower limit on wl effective interval
          REAL, INTENT( INOUT )      :: WLU_AVE( : )   ! upper limit on wl effective interval
          REAL, INTENT( INOUT )      :: CS_AVE( : )    ! cross-section as f(WL_AVE)
          REAL, INTENT( INOUT )      :: QY_AVE( : )    ! quantum yield as f(WL_AVE)

C...........LOCAL VARIABLES and their descriptions:
      
      CHARACTER(16)   ::  PNAME  = 'WVBIN_AVERAGE'    ! program name
      CHARACTER(80)   ::  MSG    = ' '                ! message

!      REAL(8)   :: FBIN(NBO)
!      REAL(8)   :: ABIN(NBO)
!      REAL(8)   :: CBIN(NBO)
!      REAL(8)   :: DBIN(NBO)
!      REAL(8)   :: EBIN(NBO)

      REAL(8), ALLOCATABLE, SAVE :: FBIN( : )
      REAL(8), ALLOCATABLE, SAVE :: ABIN( : )
      REAL(8), ALLOCATABLE, SAVE :: CBIN( : )
      REAL(8), ALLOCATABLE, SAVE :: DBIN( : )
      REAL(8), ALLOCATABLE, SAVE :: EBIN( : )

      REAL(8)          :: WW

      REAL(8), ALLOCATABLE, SAVE :: FFBIN( : )
      REAL(8), ALLOCATABLE, SAVE :: AABIN( : )
      REAL(8), ALLOCATABLE, SAVE :: CCBIN( : )
      REAL(8), ALLOCATABLE, SAVE :: DDBIN( : )
      REAL(8), ALLOCATABLE, SAVE :: EEBIN( : )
      

      REAL, ALLOCATABLE, SAVE :: XCOUT(:), QYOUT(:)
     
      INTEGER          :: I, J, K

      LOGICAL, SAVE    :: FIRSTCALL = .TRUE.

      INTERFACE 
        SUBROUTINE INTAVG ( WLIN, CQIN, NWLIN, SPECTRA_TYPE,
     &                    NWLOUT, WLOUT1, WLOUT2, CQOUT )
          CHARACTER(1), INTENT( IN )  :: SPECTRA_TYPE                ! spectra type
          INTEGER, INTENT( IN )  ::      NWLOUT              ! number of intervals ETin
          INTEGER, INTENT( IN )  ::      NWLIN               ! number of intervals CQin
          REAL, INTENT( IN )  ::         WLIN ( : )     ! wl for CQin
          REAL, INTENT( IN )  ::         CQIN( : )      ! quantity (CS or QY) as f(WLIN)
          REAL, INTENT( INOUT ) ::       WLOUT1( : )      ! lower limit on wl int ETin
          REAL, INTENT( INOUT ) ::       WLOUT2( : )      ! upper limit on wl int ETin
          REAL, INTENT( OUT ) ::         CQOUT ( : )      ! quantity (CS or QY) as f(WLOUT)
        END SUBROUTINE INTAVG
        SUBROUTINE INTAVG_C ( WLIN, CQIN, NWLIN, SPECTRA_TYPE,
     &                    NWLOUT, WLOUT1, WLOUT2, CQOUT )
          CHARACTER(1), INTENT( IN ) ::   SPECTRA_TYPE                ! spectra type
          INTEGER, INTENT( IN )      ::   NWLOUT              ! number of intervals ETin
          INTEGER, INTENT( IN )      ::   NWLIN               ! number of intervals CQin
          REAL, INTENT( IN )   ::         WLIN ( : )     ! wl for CQin
          REAL, INTENT( IN )   ::         CQIN( : )      ! quantity (CS or QY) as f(WLIN)
          REAL, INTENT( INOUT ) ::       WLOUT1( : )      ! lower limit on wl int ETin
          REAL, INTENT( INOUT ) ::       WLOUT2( : )      ! upper limit on wl int ETin
          REAL, INTENT( OUT )  ::         CQOUT ( : )      ! quantity (CS or QY) as f(WLOUT)
        END SUBROUTINE INTAVG_C
      END INTERFACE

      IF( FIRSTCALL )THEN

          FIRSTCALL = .FALSE.

          CALL INIT_BIN_DATA

          IF( CHANGE_WBIN )THEN


              ALLOCATE( FBIN( NB_NEW + 1 ) )
              ALLOCATE( ABIN( NB_NEW + 1 ) )
              ALLOCATE( CBIN( NB_NEW + 1 ) )
              ALLOCATE( DBIN( NB_NEW + 1 ) )
              ALLOCATE( EBIN( NB_NEW + 1 ) )

           ELSE
              
              ALLOCATE( FBIN( NBO ) )
              ALLOCATE( ABIN( NBO ) )
              ALLOCATE( CBIN( NBO ) )
              ALLOCATE( DBIN( NBO ) )
              ALLOCATE( EBIN( NBO ) )

           ENDIF

      
          ALLOCATE( XCOUT(NSO), QYOUT(NSO))

          ALLOCATE( AABIN(NJO_NEW), CCBIN( NJO_NEW), DDBIN(NJO_NEW), EEBIN(NJO_NEW), FFBIN(NJO_NEW))

      ENDIF ! FIRSTCALL

c---now ready to do any flux-weighted means over the bins
         FBIN(:) = 0.d0
         ABIN(:) = 0.0d0  
         CBIN(:) = 0.0d0  
         DBIN(:) = 0.0d0  
         EBIN(:) = 0.0d0  

      CALL INTAVG_C(WL_CS_IN, CS_IN, NWL_CS_IN, SPECTRA_TYPE, NSO, WL, WU, XCOUT)
      CALL INTAVG_C(WL_QY_IN, QY_IN, NWL_QY_IN, SPECTRA_TYPE, NSO, WL, WU, QYOUT)



      do J=K1,K2
        K = J - K1 + 1

        I = IBINJ_NEW(J)
        if (I .gt. 0) then
          WW = W(J)
          FBIN(I) = FBIN(I) + F(J)
          ABIN(I) = ABIN(I) + F(J)*DBLE(XCOUT(J))
          CBIN(I) = CBIN(I) + F(J)*DBLE(QYOUT(J))
          DBIN(I) = DBIN(I) + F(J)*DBLE(XCOUT(J))*DBLE(QYOUT(J))
          EBIN(I) = DBIN(I)
!          ABIN(I) = ABIN(I) + F(J)*XNEW
        endif
      enddo



      do I=1,NB_NEW
        if (ABIN(I) .gt. 0.d0)EBIN(I) = EBIN(I)/ABIN(I)
        if (FBIN(I) .gt. 0.d0) then
            ABIN(I) = ABIN(I)/FBIN(I)
            CBIN(I) = CBIN(I)/FBIN(I)
            DBIN(I) = DBIN(I)/FBIN(I)
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
        DDBIN(:) = 0.d0
        EEBIN(:) = 0.d0



      FFBIN(:) = 0.d0
      do I=16,NB_NEW
!        J = IJX(I)
!        J = IJX_CALC(I)
        J = IJX_BIN_NEW( I )
        FFBIN(J) = FFBIN(J) + FBIN(I)
        AABIN(J) = AABIN(J) + FBIN(I)*ABIN(I)
        CCBIN(J) = CCBIN(J) + FBIN(I)*CBIN(I)
        DDBIN(J) = DDBIN(J) + FBIN(I)*DBIN(I)
        EEBIN(J) = EEBIN(J) + FBIN(I)*ABIN(I)*EBIN(I)
      enddo


      do I=1,15
        do J=1,NJO_NEW
          FFBIN(J) = FFBIN(J) + FBIN(I)*SRB_NEW(I,J)
          AABIN(J) = AABIN(J) + FBIN(I)*ABIN(I)*SRB_NEW(I,J)
          CCBIN(J) = CCBIN(J) + FBIN(I)*CBIN(I)*SRB_NEW(I,J)
          DDBIN(J) = DDBIN(J) + FBIN(I)*DBIN(I)*SRB_NEW(I,J)
          EEBIN(J) = EEBIN(J) + FBIN(I)*ABIN(I)*EBIN(I)*SRB_NEW(I,J)
        enddo
      enddo


 

!      NWL_AVE = NJO_NEW
!      WLL_AVE = 0.0
!      WLU_AVE = 0.0
!      CS_AVE  = 0.0
!      QY_AVE  = 0.0




      do J = 1, 8
!        WLL_AVE( J ) = STR_WV_FASTJX( J )
!        WLU_AVE( J ) = END_WV_FASTJX( J )
        WLL_AVE( J ) = STWL_NEW( J )
        WLU_AVE( J ) = ENDWL_NEW( J )
      enddo 



      do J = 9, NJO_NEW
!        WLL_AVE( J ) = STR_WV_FASTJX( J + 2 )
!        WLU_AVE( J ) = END_WV_FASTJX( J + 2 )
        WLL_AVE( J ) = STWL_NEW( J + 2 )
        WLU_AVE( J ) = ENDWL_NEW( J + 2)
      enddo 


      do J=1,NJO_NEW
        if (AABIN(J) .gt. 0.d0) EEBIN(J) = EEBIN(J)/AABIN(J)
        if (FFBIN(J) .gt. 0.d0)THEN
            AABIN(J)    = AABIN(J)/FFBIN(J)
            CCBIN(J)    = CCBIN(J)/FFBIN(J)
            DDBIN(J)    = DDBIN(J)/FFBIN(J)
            CS_AVE( J ) = AABIN(J)
            QY_AVE( J ) = EEBIN(J)
        endif
      enddo


         RETURN
      END
