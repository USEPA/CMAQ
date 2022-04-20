
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE FIRE_PLMRIS( EMLAYS, LSTK, HFX, HMIX,
     &                        BFLX,
     &                        TSTK, USTAR, DTHDZ, TA, WSPD, 
     &                        ZF,           WSTK, ZPLM )

C-----------------------------------------------------------------------
 
C Description:  
C     computes final effective plume centerline height.
 
C Preconditions:
C     meteorology and stack parameters
 
C Subroutines and Functions called:
 
C Revision History:
C     Prototype 12/95 by CJC, based on Briggs algorithm adapted from
C     RADM 2.6 subroutine PLUMER() (but with completely different 
C     data structuring).
C     Copied from plmris.F 4.4 by M Houyoux 3/99 
C     Aug 2015, D. Wong: Used assumed shape array declaration
C     Jan 2022, D. Wong: variable FIRSTIME is obsolete
 
C-----------------------------------------------------------------------
C Modified from:

C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling System
C File: @(#)$Id: fire_plmris.F,v 1.2 2011/10/21 16:11:30 yoj Exp $
C COPYRIGHT (C) 2002, MCNC Environmental Modeling Center
C All Rights Reserved
C See file COPYRIGHT for conditions of use.
C Environmental Modeling Center
C MCNC
C P.O. Box 12889
C Research Triangle Park, NC  27709-2889
C smoke@emc.mcnc.org
C Pathname: $Source: /project/yoj/arc/CCTM/src/plrise/smoke/fire_plmris.F,v $
C Last updated: $Date: 2011/10/21 16:11:30 $ 
C-----------------------------------------------------------------------
      USE RUNTIME_VARS, ONLY : LOGDEV

      IMPLICIT NONE

C Includes:
      INCLUDE SUBST_CONST     ! physical and mathematical constants

C Arguments:
      INTEGER, INTENT( IN )  :: EMLAYS          ! no. of emission layers
      INTEGER, INTENT( IN )  :: LSTK            ! lyr of top of fire, = RADM's KSTK
      REAL,    INTENT( IN )  :: HFX             ! sensible heat flux [m K/s]
      REAL,    INTENT( IN )  :: HMIX            ! mixing height [m]

      REAL,    INTENT( IN )  :: BFLX            ! Buoyancy Flux [m**4/s**3]

      REAL,    INTENT( IN )  :: TSTK            ! tmptr at top of fire [deg K]
      REAL,    INTENT( IN )  :: USTAR           ! friction velocity [m/s]
      REAL,    INTENT( IN )  :: DTHDZ( : )      ! gradient of THETV
      REAL,    INTENT( IN )  :: TA   ( : )      ! temperature [deg K]
      REAL,    INTENT( IN )  :: WSPD ( : )      ! wind speed [m/s]
      REAL,    INTENT( IN )  :: ZF  ( 0: )      ! layer surface height [m]
      REAL,    INTENT( INOUT ) :: WSTK          ! wind speed @ top of fire [m/s]
                                                ! OUT for reporting, only
      REAL,    INTENT( OUT ) :: ZPLM            ! initially, plume top height above fire,
                                                ! finally, plume centerline height [m]
                                                ! can be greater than the top of EMLAYS

C Parameters:
      REAL, PARAMETER :: HCRIT   =  1.0E-4 * 0.03  ! hfx min * tolerance
      REAL, PARAMETER :: SMALL   =  3.0E-5         ! Criterion for stability
      REAL, PARAMETER :: D3      =  1.0 /  3.0     ! 1/ 3
      REAL, PARAMETER :: D6      =  1.0 /  6.0     ! 1/6
      REAL, PARAMETER :: D45     =  1.0 / 45.0     ! 1/45
      REAL, PARAMETER :: D2664   =  1.0 /  2.664   ! 1/ 2.664
      REAL, PARAMETER :: D59319  =  1.0 / 59.319   ! 1/59.319
      REAL, PARAMETER :: TWOTHD  =  2.0 /  3.0     ! 2/3
      REAL, PARAMETER :: FIVETHD =  5.0 /  3.0     ! 5/3
      REAL, PARAMETER :: NODIV0  =  1.0            ! Prevent divide by zero
      
      REAL, PARAMETER :: ZERO = 0.0

C Local Variables:
      INTEGER IQ              ! stability class: 1-unstable, 2-neutral, 3-stable
      INTEGER LPLM            ! first L: ZH(L) > Plume height ! same as RADM's KPR
      INTEGER NN              ! Counter for interations through layers
      REAL    DH              ! plume rise increment to center of the plume
      REAL    DHM             ! plume rise from momentum
      REAL    DHSM            ! stable momentum plume rise
      REAL    DHN             ! plume rise for neutral case
      REAL    DHT             ! plume rise increment to the top of the plume
      REAL    HSTAR           ! convective scale at fire (m**2/s**3)
      REAL    PX, RX, SX      ! scratch coefficients
      REAL    RBFLX           ! residual buoyancy flux (m**4/s**3)
      REAL    TPLM            ! temperature at top of plume (m/s)
      REAL    WPLM            ! wind speed  at top of plume (m/s)
      REAL    ZMIX            ! hmix - hs

C Statement Functions:
      REAL    B,    S, U, US  ! arguments
       
      REAL    NEUTRL          ! neutral-stability plume rise function
      REAL    STABLE          ! stable            plume rise function
      REAL    UNSTBL          ! unstable          plume rise function

      NEUTRL( B, U, US ) = 1.3327804 * B / ( U * US * US )

      STABLE( B, U, S ) =  2.6 * ( B / ( U * S ) ) ** D3
      UNSTBL( B, U )    = 30.0 * ( B / U ) ** 0.6

C-----------------------------------------------------------------------

C Compute convective scale.

      HSTAR = GRAV * HFX / TA( 1 )   ! Using surface temperature is correct

C Initialize layer of plume
      LPLM  = LSTK

C Compute momentum rise ( set min wind speed to 1 m/s)
      WSTK = MAX( WSTK, 1.0 )
      DHM  = ZERO

C Compute initial plume rise from fire top to next level surface:

      IF ( HSTAR .GT. HCRIT ) THEN           ! unstable case:
         ZMIX = HMIX 

         IF ( ZMIX .LE. 0.0 ) THEN           ! Fire above mixing height:
            SX = MAX( GRAV * DTHDZ( LPLM ) / TSTK, SMALL )

C Reset the wind speed at fire to the wind speed at plume when the layer
C of the plume is not equal to the layer of the fire.
            IF ( LPLM .NE. LSTK ) THEN
               WSTK = WSPD( LPLM )
               IF ( WSTK .EQ. 0.0 ) WSTK = NODIV0
            END IF

C Compute the stable momentum rise, for layer of the fire.
            IF ( DTHDZ( LPLM ) .GT. 0.001 ) THEN
               DHSM = ZERO
            ELSE
               DHSM = DHM     ! Set it to DHM, if THGRAD too small
            END IF
            DHM = MIN( DHSM, DHM )

C Compute the neutral and stable plume rises.         
            DHN = NEUTRL( BFLX, WSTK, USTAR )
            DH  = STABLE( BFLX, WSTK, SX )

            IF ( DHN .LT. DH ) THEN  ! Take the minimum of neutral and stable
               DH = DHN
               IQ = 2
            ELSE 
               IQ = 3 
            END IF

            IF ( DHM .GT. DH .AND. WSTK .GT. 1.0 ) THEN
               DH = DHM
               IQ = 4
            END IF
            DHT = 1.5 * DH

         ELSE                        !  unstable case:
            DH  = UNSTBL( BFLX, WSTK )
            DHN = NEUTRL( BFLX, WSTK, USTAR )

            IF ( DHN .LT. DH ) THEN
               DH = DHN
               IQ = 2
            ELSE
               IQ = 1
            END IF

            IF ( DHM .GT. DH .AND. WSTK .GT. 1.0 ) THEN
               DH = DHM
               IQ = 4 
            END IF

            DHT = 1.5 * DH

         END IF

      ELSE IF ( HSTAR .LT. -HCRIT .OR. DTHDZ( LSTK ) .GT. 0.001 ) THEN   ! stable case:

         SX = MAX( GRAV * DTHDZ( LSTK ) / TSTK, SMALL )
         DHT = 1.5 * STABLE( BFLX, WSTK, SX )
         DHN = 1.5 * NEUTRL( BFLX, WSTK, USTAR )
         IF ( DHN .LT. DHT ) THEN
             DHT = DHN
             IQ = 2
         ELSE
             IQ = 3
         END IF

      ELSE                             !  neutral case:

         DHT = 1.5 * NEUTRL( BFLX, WSTK, USTAR )
         IQ  = 2

      END IF                 !  hstar ==> unstable, stable, or neutral
  
C Compute further plume rise from between level surfaces:
      NN = 0
      RBFLX = BFLX
      ZPLM  = DHT

C End calculations if the momentum rise was used in the calculation

      IF ( IQ .EQ. 4 ) GO TO 199  ! to point past iterative buoyancy loop

      DO       !  loop computing further plume rise

         RX = ZPLM - ZF( LPLM )
         IF ( RX .LE. 0.0 ) THEN
            EXIT  ! exit plume rise loop
         END IF

         IF ( LPLM .EQ. EMLAYS ) THEN   ! we're finished
            ZPLM = MIN( ZPLM, ZF( EMLAYS ) )
            WRITE( LOGDEV,'(5X, A, I3, F10.3)' )
     &                    'Plume rise reached EMLAYS with ZPLM:', EMLAYS, ZPLM
            EXIT  ! exit plume rise loop
         END IF

C Reset met data. NOTE- the original RADM code submitted the 
C WSPD and TA to an interpolator, but then requested the height of
C interpolation identical to ZH( LPLM ).
         NN = NN + 1
         IF ( NN .GT. 1 ) THEN
            WPLM = WSPD( LPLM )
            TPLM = TA  ( LPLM )
         ELSE                  ! 1st time, use stack values ...
            WPLM = WSTK
            TPLM = TSTK
         END IF
 
C Compute residual bflx by stability case IQ:

         IF ( IQ .EQ. 1 ) THEN
            RX = D45 * RX      ! Includes the 1.5 factor for plume top
            RBFLX = WPLM * ( RX ** FIVETHD )
         ELSE IF ( IQ .EQ. 2 ) THEN
            PX = TWOTHD * ZPLM         
            RBFLX = D2664 * ( RX ** FIVETHD ) * WPLM * ( USTAR * USTAR ) / PX ** TWOTHD
         ELSE      !  else iq = 3:
            RBFLX = D59319 * WPLM * SX * RX ** 3
         END IF      !  if stability flag iq is 1, 2, or 3

C Increment the layer number below.
         IF ( LPLM .LT. EMLAYS ) LPLM = LPLM + 1
         WPLM = WSPD( LPLM )
         TPLM = TA  ( LPLM )

C Prevent divide-by-zero by WPLM
         IF ( WPLM .EQ. 0.0 ) WPLM = NODIV0

C Process according to stability cases:
         SX = GRAV * DTHDZ( LPLM ) / TPLM
         IF ( SX .GT. SMALL ) THEN               ! stable case:
C Use the theta gradient to determine pr eqn.
            DHT = 1.5 * STABLE( RBFLX, WPLM, SX )
            DHN = 1.5 * NEUTRL( RBFLX, WPLM, USTAR )
            IF ( DHN .LT. DHT ) THEN
               DHT = DHN
               IQ  = 2
            ELSE
               IQ  = 3
            END IF
            DH = DHT / 1.5
         ELSE          ! if upper layer is not stable, use neutral formula
                         
            DHN = NEUTRL( RBFLX, WPLM, USTAR )
            DH  = UNSTBL( RBFLX, WPLM )
            IQ = 1
            IF ( DHN .LT. DH ) THEN
               DH = DHN
               IQ  = 2
            END IF
            DHT = 1.5 * DH

         END IF
  
         ZPLM = ZF( LPLM-1 ) + DHT
         DH   = ZF( LPLM-1 ) + DH 
        
      END DO  ! end loop computing further plume rise

199   CONTINUE

C Compute plume rise amount (DH) and actual final plume centerline height (ZPLM):
      DH  = TWOTHD * ZPLM
      ZPLM = DH            

      RETURN

      END SUBROUTINE FIRE_PLMRIS
