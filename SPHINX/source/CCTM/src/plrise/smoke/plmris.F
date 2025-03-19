
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
      SUBROUTINE PLMRIS( EMLAYS, LSTK, HFX, HMIX,
     &                   STKDM, STKHT, STKTK, STKVE,
     &                   TSTK, USTAR, DTHDZ, TA, WSPD,
     &                   ZF, ZH, ZSTK, WSTK, ZPLM )

C-----------------------------------------------------------------------
 
C Description:  
C     computes final effective plume centerline height.
 
C Preconditions:
C     meteorology and stack parameters
 
C Subroutines and Functions Called:
 
C Revision History:
C     Prototype 12/95 by CJC, based on Briggs algorithm adapted from
C     RADM 2.6 subroutine PLUMER() (but with completely different 
C     data structuring).
C     Copied from plmris.F 4.4 by M Houyoux 3/99 
C     Aug 2015, D. Wong: Used assumed shape array declaration
C     Jan 2022, D. Wong: Variable FIRSTIME is obsolete
 
C-----------------------------------------------------------------------
C Modified from:
   
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling System
C File: @(#)$Id: plmris.F,v 1.2 2011/10/21 16:11:31 yoj Exp $
C COPYRIGHT (C) 2002, MCNC Environmental Modeling Center
C All Rights Reserved
C See file COPYRIGHT for conditions of use.
C Environmental Modeling Center
C MCNC
C P.O. Box 12889
C Research Triangle Park, NC  27709-2889
C smoke@emc.mcnc.org
C Pathname: $Source: /project/yoj/arc/CCTM/src/plrise/smoke/plmris.F,v $
C Last updated: $Date: 2011/10/21 16:11:31 $ 
   
C-----------------------------------------------------------------------
      USE RUNTIME_VARS, ONLY : LOGDEV

      IMPLICIT NONE

C Includes:
      INCLUDE SUBST_CONST     ! physical and mathematical constants

C Arguments:
      INTEGER, INTENT( IN )  :: EMLAYS          ! no. of emission layers
      INTEGER, INTENT( IN )  :: LSTK            ! lyr of top of stack, = RADM's KSTK
      REAL,    INTENT( IN )  :: HFX             ! sensible heat flux [m K/s]
      REAL,    INTENT( IN )  :: HMIX            ! mixing height [m]
      REAL,    INTENT( IN )  :: STKDM           ! stack diameter [m]
      REAL,    INTENT( IN )  :: STKHT           ! stack height [m]
      REAL,    INTENT( IN )  :: STKTK           ! exhaust temperature [deg K]
      REAL,    INTENT( IN )  :: STKVE           ! exhaust velocity [m/s]
      REAL,    INTENT( IN )  :: TSTK            ! tmptr at top of stack [deg K]
      REAL,    INTENT( IN )  :: USTAR           ! friction velocity [m/s]
      REAL,    INTENT( IN )  :: DTHDZ( : )      ! gradient of THETV
      REAL,    INTENT( IN )  :: TA   ( : )      ! temperature [deg K]
      REAL,    INTENT( IN )  :: WSPD ( : )      ! wind speed [m/s]
      REAL,    INTENT( IN )  :: ZF ( 0:  )      ! layer surface height [m]
      REAL,    INTENT( IN )  :: ZH   ( : )      ! layer center height [m]
      REAL,    INTENT( IN )  :: ZSTK ( : )      ! zf( l ) - stkht [m]
      REAL,    INTENT( INOUT ) :: WSTK          ! wind speed @ top of stack [m/s]
                                                ! OUT for reporting, only
      REAL,    INTENT( OUT ) :: ZPLM            ! temporarily, plume top height
                                                ! above stack, finally plume centerline
                                                ! height [m] (can be greater than the
                                                ! height of the top of the EMLAYS layer)

C Parameters:
      REAL, PARAMETER :: HCRIT   = 1.0E-4 * 0.03  ! hfx min * tolerance
      REAL, PARAMETER :: SMALL   = 3.0E-5         ! Criterion for stability
      REAL, PARAMETER :: D3      = 1.0 / 3.0
      REAL, PARAMETER :: D6      = 1.0 / 6.0
      REAL, PARAMETER :: D45     = 1.0 / 45.0
      REAL, PARAMETER :: D2664   = 1.0 / 2.664
      REAL, PARAMETER :: D59319  = 1.0 / 59.319
      REAL, PARAMETER :: TWOTHD  = 2.0 / 3.0
      REAL, PARAMETER :: FIVETHD = 5.0 / 3.0

C Local Variables:
      INTEGER IQ              ! stability class:  1=unstbl, 2=neut, 3=stbl, 4=use DHM
      INTEGER LPLM            ! first L: ZH(L) > Plume height ! same as RADM's KPR
      INTEGER NN              ! counter for interations through layers
      REAL    BFLX            ! buoyancy flux (m**4/s**3)
      REAL    DH              ! plume rise increment to center of the plume
      REAL    DHM             ! plume rise from momentum
      REAL    DHSM            ! stable momentum plume rise
      REAL    DHN             ! plume rise for neutral case
      REAL    DHT             ! plume rise increment to the top of the plume
      REAL    HSTAR           ! convective scale at stack (m**2/s**3)
      REAL    PX, RX, SX      ! scratch coefficients
      REAL    RBFLX           ! residual buoyancy flux (m**4/s**3)
      REAL    TPLM            ! temperature at top of plume (m/s)
      REAL    WPLM            ! wind speed  at top of plume (m/s)
      REAL    ZMIX            ! hmix - hs

C Statement Functions:
      REAL    B, H, S, U, US  ! arguments
      REAL    NEUTRL          ! neutral-stability plume rise function
      REAL    STABLE          ! stable            plume rise function
      REAL    UNSTBL          ! unstable          plume rise function

      NEUTRL( H, B, U, US ) =
     &        MIN( 10.0 * H, 
     &        1.2 * (           ( B / ( U * US * US ) ) ** 0.6     ! pwr 3 * 0.2
     &              * ( H + 1.3 * B / ( U * US * US ) ) ** 0.4 ) ) ! pwr 2 * 0.2
      STABLE( B, U, S ) =  2.6 * ( B / ( U * S ) ) ** D3
      UNSTBL( B, U )    = 30.0 * ( B / U ) ** 0.6

C-----------------------------------------------------------------------

C Compute convective scale, buoyancy flux.

      HSTAR = GRAV * HFX / TA( 1 )   ! Using surface temperature is correct
      BFLX  = 0.25 * GRAV * ( STKTK - TSTK ) * STKVE * STKDM * STKDM / STKTK

C Initialize layer of plume
      LPLM  = LSTK

C Compute momentum rise ( set min wind speed to 1 m/s)
      WSTK = MAX( WSTK, 1.0 )
      DHM  = 3.0 * STKDM * STKVE / WSTK

C When BFLX <= zero, use momentum rise only
C NOTE: This part of algorithm added based on Models-3 plume rise

      IF ( BFLX .LE. 0.0 ) THEN
C (06/02) Set the ZPLM plume rise height to the momentum value DHM above
         ZPLM = STKHT + MAX( DHM, 2.0 )
         RETURN
      END IF

C Compute initial plume rise from stack top to next level surface:

      IF ( HSTAR .GT. HCRIT ) THEN           ! unstable case:
         ZMIX = HMIX - STKHT

         IF ( ZMIX .LE. 0.0 ) THEN           ! Stack at or above mixing height:
            SX = MAX( GRAV * DTHDZ( LPLM ) / TSTK, SMALL )

C Reset the wind speed at stack to the wind speed at plume when the layer
C of the plume is not equal to the layer of the stack.
            IF ( LPLM .NE. LSTK ) THEN
               WSTK = MAX( WSPD( LPLM ), 1.0 )
            END IF
            IF ( DTHDZ( LPLM ) .GT. 0.001 ) THEN
C Compute the stable momentum rise, for layer of the stack
               DHSM = 0.646 * ( STKVE * STKVE * STKDM * STKDM
     &              / ( STKTK * WSTK ) ) ** D3 * SQRT( TSTK )
     &              / DTHDZ( LPLM ) ** D6
            ELSE
               DHSM = DHM    ! set it to DHM, if THGRAD too small
            END IF
            DHM = MIN( DHSM, DHM )
          
C Compute the neutral and stable plume rises          
            DHN = NEUTRL( STKHT, BFLX, WSTK, USTAR )
            DH  = STABLE( BFLX, WSTK, SX )

            IF ( DHN .LT. DH ) THEN  ! Take the minimum of neutral and stable
               DH = DHN
               IQ = 2
            ELSE 
               IQ = 3
            END IF

!           IF ( DHM .GT. DH .AND. WSTK .GT. 1.0 ) THEN
            IF ( DH .LT. DHM ) THEN  ! Take the minimum of the above and momentum rise
               DH = DHM
               IQ = 4
            END IF
            DHT = 1.5 * DH

         ELSE                        !  unstable case:
            DHN = NEUTRL( STKHT, BFLX, WSTK, USTAR )
            DH  = UNSTBL( BFLX, WSTK )

            IF ( DHN .LT. DH ) THEN  ! Take the minimum of neutral and unstable
               DH = DHN
               IQ = 2
            ELSE
               IQ = 1
            END IF

!           IF ( DHM .GT. DH .AND. WSTK .GT. 1.0 ) THEN
            IF ( DH .LT. DHM ) THEN  ! Take the minimum of the above and momentum rise
               DH = DHM
               IQ = 4
            END IF
            DHT = 1.5 * DH
           
         END IF

      ELSE IF ( HSTAR .LT. -HCRIT .OR. DTHDZ( LSTK ) .GT. 0.001 ) THEN   ! stable case:

         SX  = MAX( GRAV * DTHDZ( LSTK ) / TSTK, SMALL )
         DHN = 1.5 * NEUTRL( STKHT, BFLX, WSTK, USTAR )
         DHT = 1.5 * STABLE( BFLX, WSTK, SX )
         IF ( DHN .LT. DHT ) THEN  ! Take the minimum of neutral and stable
            DHT = DHN
            IQ = 2
         ELSE
            IQ = 3
         END IF

      ELSE                              !  neutral case:

         DHT = 1.5 * NEUTRL( STKHT, BFLX, WSTK, USTAR )
         IQ  = 2

      END IF                  !  hstar ==> unstable, stable, or neutral
  
      ZPLM  = DHT

C End calculations if the momentum rise was used in the calculation
      IF ( IQ .EQ. 4 ) GO TO 199  ! to point past iterative buoyancy loop

C Compute further plume rise from between level surfaces:
      NN = 0
      RBFLX = BFLX

      DO       ! infinite loop computing further plume rise
       
         RX = ZPLM - ZSTK( LPLM )
         IF ( RX .LE. 0.0 ) THEN
            EXIT  ! exit plume rise loop
         END IF

         IF ( LPLM .EQ. EMLAYS ) THEN   ! we're finished
            ZPLM = MIN( ZPLM, ZSTK( EMLAYS ) )
            WRITE( LOGDEV,'(5X, A, I3, F10.3)' )
     &                    'Plume rise reached EMLAYS with ZPLM:', EMLAYS, ZPLM
            EXIT  ! exit plume rise loop
         END IF

C Reset met data. NOTE - the original RADM code interpolated WSPD and TA,
C but then set the height of interpolation identical to ZH( LPLM ).
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
            PX = STKHT + TWOTHD * ZPLM         
            RBFLX = D2664 * ( RX ** FIVETHD ) * WPLM * ( USTAR * USTAR ) / PX ** TWOTHD
         ELSE        !  else iq = 3:
            RBFLX = D59319 * WPLM * SX * RX ** 3
         END IF      !  if stability flag iq is 1, 2, or 3

C Increment the layer number below
         IF ( LPLM .LT. EMLAYS ) LPLM = LPLM + 1
         WPLM = WSPD( LPLM )
         TPLM = TA( LPLM )

C Prevent divide-by-zero by WPLM
         WPLM = MAX( WPLM, 1.0 )

C Process according to stability cases:
         SX = GRAV * DTHDZ( LPLM ) / TPLM
         IF ( SX .GT. SMALL ) THEN               ! stable case:
            DHN = 1.5 * NEUTRL( STKHT, RBFLX, WPLM, USTAR )
            DHT = 1.5 * STABLE( RBFLX, WPLM, SX )
            IF ( DHN .LT. DHT ) THEN  ! Take the minimum of neutral and stable
               DHT = DHN
               IQ  = 2
            ELSE
               IQ  = 3
            END IF
            DH = DHT / 1.5

         ELSE          ! if upper layer is not stable, use neutral formula

            DHN = NEUTRL( STKHT, RBFLX, WPLM, USTAR )
            DH = UNSTBL( RBFLX, WPLM )
            IF ( DHN .LT. DH ) THEN  ! Take the minimum of neutral and unstable
               DH = DHN
               IQ  = 2
            ELSE
               IQ  = 1
            END IF
            DHT = 1.5 * DH

         END IF
  
         ZPLM = ZSTK( LPLM-1 ) + DHT
!        DH   = ZSTK( LPLM-1 ) + DH 
        
      END DO   ! end loop computing further plume rise

199   CONTINUE

C Adjustment for layer 1 combustion pt. source stacks with plume rise limited
C to layer 1; put plume height in middle of layer 2:
      IF ( STKHT + TWOTHD * ZPLM .LE. ZF( 1 ) .AND. STKTK .GT. TA( 1 ) ) THEN
         ZPLM = ZH( 2 )
      END IF

C set final plume centerline height (ZPLM):
      ZPLM = STKHT + TWOTHD * ZPLM 

      RETURN

      END SUBROUTINE PLMRIS
