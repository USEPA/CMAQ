
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


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/arc/CCTM/src/vadv/wrf/vppm.F,v 1.5 2011/10/21 16:11:40 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#ifdef isam
      SUBROUTINE VPPM ( NI, DT, DS, FLX, VEL, CON, SA_CON  )
#elif sens
      SUBROUTINE VPPM ( NI, DT, DS, FLX, VEL, CON, SEN )
#else
      SUBROUTINE VPPM ( NI, DT, DS, FLX, VEL, CON )
#endif
      
C----------------------------------------------------------------------
C Function      
C   This is the one-dimensional implementation of piecewise parabolic
C   method.  Variable grid spacing is allowed. The scheme is positive
C   definite and monotonic. It is conservative, and causes small
C   numerical diffusion.
      
C   A piecewise continuous parabola is used as the intepolation polynomial.
C   The slope of the parabola at cell edges are computed from a cumulative
C   function of the advected quantity.  These slopes are further modified
C   so that the interpolation function is monotone. For more detailed
C   information see:
      
C   Colella, P., and P. L. Woodward, (1984), "The Piecewise Parabolic
C   Method (PPM) for Gas-Dynamical Simulations," J. Comput. Phys. 54,
C   174-201.
      
C   The concentrations at boundary cells (i.e., at 1 and NI) are not
C   computed here.  They should be updated according to the boundary
C   conditions.
      
C   The following definitions are used:
     
C              |---------------> Positive direction
C     
C  -->|Boundary|<----------------Main Grid----------------->|Boundary|<--
C     
C     |---><---|---><---|       ~|---><---|~       |---><---|---><---|
C       CON(0)   CON(1)            CON(i)            CON(n)  CON(n+1)
C     
C     VEL(1)<->|        VEL(i)<->|        |<->VEL(i+1)      |<->VEL(n+1)
C    
C      FP(0)-->|       FP(i-1)-->|        |-->FP(i)         |-->FP(n)
C     
C      FM(1)<--|         FM(i)<--|        |<--FM(i+1)       |<--FM(n+1)
C    
C                             -->| DS(i)  |<--
      
C----------------------------------------------------------------------
      
C Revision History:
      
C   20 April, 1993 by M. Talat Odman at NCSC: 
C   Created based on Colella and Woodward (1984)
      
C   15 Sept., 1993 by Daewon Byun at EPA:
C   Original code obtained from Phillip Colella at Berkeley
      
C   29 Nov.,  1993 by M. Talat Odman at NCSC:
C   Found no difference from original code
      
C   05 Oct.,  1993 by M. Talat Odman at NCSC:
C   Modified for EDSS archive, made discontinuity capturing an option
      
C   Sep 97 - Jeff
C   Aug 98 - Jeff - optimize for mesh coefficients

C   06/16/04 by Peter Percell & Daewon Byun at UH-IMAQS:
C     - Fixed bug in using fluxes in non-uniform grids to update concentrations

C   07 Dec 04 J.Young: vert dyn alloc - Use VGRD_DEFN
C   08 May 09 J.Young: dimension CON with species; eliminate "STEEPEN" option

C   18 Nov 09 J.Young: combine PPM velocity adjustment and vertically advected
C                      concentrations on one code
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   31 Jul 12 J.Bash: Changed the adjustment to VEL to be the square root
C                     of the flux*dt over the estimated flux*dt because
C                     the relationship between the flux and velocity is 
C                     roughly a second order polynomial and adjustments
C                     under conditions with CFL near 1 could result in 
C                     errors using a linear approximation. 
C   11 Dec 19 S.L.Napelenok: ddm-3d implementation for version 5.3.1
C----------------------------------------------------------------------
      
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
#ifdef isam
      USE SA_DEFN
#elif sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX
#endif

      IMPLICIT NONE
      
C Includes:
      
      INTEGER, SAVE :: N_SPC_ADV

C Arguments:
 
      INTEGER, INTENT(  IN )   :: NI          ! number of zones (cells)
      REAL,    INTENT(  IN )   :: DS ( NI )   ! distance between zone (cell) boundaries
      REAL,    INTENT(  IN )   :: FLX( NI+1 ) ! fluxes at zone (cell) boundaries
      REAL,    INTENT(  IN )   :: DT          ! time step
      REAL,    INTENT( INOUT ) :: VEL( NI+1 ) ! velocities at zone (cell) boundaries
      REAL,    INTENT( INOUT ) :: CON( :,: ) ! concs in a vertical column
#ifdef isam
Ckrt
      REAL,    INTENT( INOUT ) :: SA_CON( NI,N_SPCTAG )
#endif

!!! NOTE: Even tho' VEL is not used on output, declaring it as INTENT(IN) causes the
!!!       code to fail. ifort compiler error?
      
C Parameters:
      
      REAL, PARAMETER :: TWO3RDS = 2.0 / 3.0
      REAL, PARAMETER :: EPSF = 0.001
      
C Local variables:
      
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER( 120 ) :: XMSG = ' '

      REAL    :: FM ( 1:NI+1 ) ! outflux from left or bottom of cell
      REAL    :: FP ( 0:NI )   ! outflux from right or top of cell
      REAL    :: CR ( 1:NI )   ! zone R.H. intercept
      REAL    :: CL ( 1:NI )   ! zone L.H. intercept
      REAL    :: DC ( 1:NI )   ! CR - CL
      REAL    :: C6 ( 1:NI )   ! coefficient of second-order term
      REAL    :: CN ( 1:NI )   ! local con

      REAL X                   ! Courant number
      REAL Y                   ! removed zone slab
      REAL FDN, FUP            ! upstream donor cell versions of fm, fp
      INTEGER ICNT             ! no. of times fdn, fup exceeds fm, fp error range
      
      INTEGER I, S             ! loop index
      INTEGER, PARAMETER :: MAXCNT = 50

#ifdef sens
      REAL,    INTENT( INOUT ) :: SEN( :,:,: )
#endif

C----------------------------------------------------------------------
      
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         N_SPC_ADV = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV + 1 ! for advecting Rho_J
      END IF   ! FIRSTIME

      DO I = 1, NI
         CN( I ) = CON( I,N_SPC_ADV )   ! N_SPC_ADV is transported RhoJ index
      END DO

      CALL PPM ( NI, DT, DS, CN, CR, CL, DC, C6 )

c set all fluxes to zero. either positive or negative flux will
c remain zero depending on the sign of the velocity.

c fm: function for mass leaving interval i at lower face (i-1/2)
c = length of segment leaving * integral average concentration in that segment:
c   length of segment leaving = y = -v(i)dt
c   segment integral ave. conc. = cl(i) + 1/2[c6(i)+dc(i)]dx -1/3[c6(i)]dx**2
c fp: function for mass leaving interval i at upper face (i+1/2)
c = length of segment leaving * integral average concentration in that segment:
c   length of segment leaving = y = v(i+1)dt
c   segment integral ave. conc. = cr(i) + 1/2[c6(i)-dc(i)]dx -1/3[c6(i)]dx**2

      FM( 1:NI+1 ) = 0.0
      FP( 0:NI ) = 0.0     

      DO I = 1, NI
         IF ( VEL( I ) .LT. 0.0 ) THEN
            FDN = -FLX( I ) * DT
            ICNT = 0
66          CONTINUE
            Y = -VEL( I ) * DT
            X = Y / DS( I )
            FM( I ) = Y * ( CL( I ) + 0.5 * X
     &              * ( DC( I ) + C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
            IF ( ABS( FM( I ) - FDN ) .GT. EPSF * FDN ) THEN
               VEL( I ) = VEL( I ) * SQRT( FDN / FM( I ) )
               ICNT = ICNT + 1
!              if ( vel( i ) .ge. 0.0 ) then
!                 write( *,2013 ) '@#@ icnt,i,fdn,fm,vel changed sign: ',
!    &                            icnt, i, fdn, fm( i ), vel( i )
2013              format( a, 2i4, 2( f15.5 ), 1pe15.3 )
!              end if
               IF ( ICNT .GT. MAXCNT ) THEN
                  XMSG = ' max iterations exceeded in vppm at 66'
                  CALL M3EXIT( 'VPPM', 0, 0, XMSG, XSTAT1 )                     
               END IF
               GO TO 66
            END IF
         END IF
         IF ( VEL( I+1 ) .GT. 0.0 ) THEN
            FUP = FLX( I+1 ) * DT
            ICNT = 0
77          CONTINUE
            Y = VEL( I+1 ) * DT
            X = Y / DS( I )
            FP( I ) = Y * ( CR( I ) - 0.5 * X
     &              * ( DC( I ) - C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
            IF ( ABS( FP( I ) - FUP ) .GT. EPSF * FUP ) THEN
               VEL( I+1 ) = VEL( I+1 ) * SQRT( FUP / FP( I ) )
               ICNT = ICNT + 1
!              if ( vel( i+1 ) .le. 0.0 ) then
!                 write( *,2013 ) '@#@ icnt,i,fup,fp,vel changed sign: ',
!    &                            icnt, i, fdn, fm( i ), vel( i+1 )
!              end if
               IF ( ICNT .GT. MAXCNT ) THEN
                  XMSG = ' max iterations exceeded in vppm at 77'
                  CALL M3EXIT( 'VPPM', 0, 0, XMSG, XSTAT1 )                     
               END IF
               GO TO 77
            END IF
         END IF
      END DO

      DO 501 S = 1, N_SPC_ADV

         DO I = 1, NI
            CN( I ) = CON( I,S )
         END DO

         CALL PPM ( NI, DT, DS, CN, CR, CL, DC, C6 )

         FM( 1:NI+1 ) = 0.0
         FP( 0:NI ) = 0.0

         DO I = 1, NI

            IF ( VEL( I ) .LT. 0.0 ) THEN
               Y = -VEL( I ) * DT
               X = Y / DS( I )
               FM( I ) = Y * ( CL( I ) + 0.5 * X
     &                 * ( DC( I ) + C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
            END IF

            IF ( VEL( I+1 ) .GT. 0.0 ) THEN
               Y = VEL( I+1 ) * DT
               X = Y / DS( I )
               FP( I ) = Y * ( CR( I ) - 0.5 * X
     &                 * ( DC( I ) - C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
            END IF

         END DO

c compute fluxes for top face

         I = NI+1
         IF ( VEL( I ) .LT. 0.0 ) THEN
            Y = -VEL( I ) * DT
            FM( I ) = Y * CON( I-1,S )
         END IF

         DO I = 1, NI
            CON( I,S ) = CON( I,S )
     &               + ( FP( I-1 ) - FP( I ) + FM( I+1 ) - FM( I ) ) / DS( I )
         END DO

501   CONTINUE

#ifdef isam
Ckrt Back up the SA_CON.....
      DO 602 S = 1, N_SPCTAG

         DO I = 1, NI
            CN( I ) = SA_CON( I,S )
         END DO

         CALL PPM ( NI, DT, DS, CN, CR, CL, DC, C6 )

         FM( 1:NI+1 ) = 0.0
         FP( 0:NI ) = 0.0

         DO I = 1, NI

            IF ( VEL( I ) .LT. 0.0 ) THEN
               Y = -VEL( I ) * DT
               X = Y / DS( I )
               FM( I ) = Y * ( CL( I ) + 0.5 * X
     &                 * ( DC( I ) + C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
            END IF ! vel < 0 ?

            IF ( VEL( I+1 ) .GT. 0.0 ) THEN
               Y = VEL( I+1 ) * DT
               X = Y / DS( I )
               FP( I ) = Y * ( CR( I ) - 0.5 * X
     &                 * ( DC( I ) - C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
            END IF ! vel > 0 ?

         END DO ! loop I

c compute fluxes for top face

         I = NI+1
         IF ( VEL( I ) .LT. 0.0 ) THEN
            Y = -VEL( I ) * DT
            FM( I ) = Y * SA_CON( I-1,S )
         END IF

Ckrt... Advect the apportioning tags ......
         DO I = 1, NI
            SA_CON( I,S ) = SA_CON( I,S )
     &              + ( FP( I-1 ) - FP( I ) + FM( I+1 ) - FM( I ) ) / DS( I )
         END DO

602   CONTINUE
#endif

#ifdef sens
      DO NP = 1, NPMAX

         DO 701 S = 1, N_SPC_ADV

            DO I = 1, NI
               CN( I ) = SEN( I,S,NP )
            END DO

            CALL PPM ( NI, DT, DS, CN, CR, CL, DC, C6 )

            FM( 1:NI+1 ) = 0.0
            FP( 0:NI ) = 0.0

            DO I = 1, NI

               IF ( VEL( I ) .LT. 0.0 ) THEN
                  Y = -VEL( I ) * DT
                  X = Y / DS( I )
                  FM( I ) = Y * ( CL( I ) + 0.5 * X
     &                    * ( DC( I ) + C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
               END IF

               IF ( VEL( I+1 ) .GT. 0.0 ) THEN
                  Y = VEL( I+1 ) * DT
                  X = Y / DS( I )
                  FP( I ) = Y * ( CR( I ) - 0.5 * X
     &                    * ( DC( I ) - C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
               END IF

            END DO

c compute fluxes for top face

            I = NI+1
            IF ( VEL( I ) .LT. 0.0 ) THEN
               Y = -VEL( I ) * DT
               FM( I ) = Y * SEN ( I-1,S,NP )
            END IF

            DO I = 1, NI
               SEN( I,S,NP ) = SEN( I,S,NP )
     &                       + ( FP( I-1 ) - FP( I ) + FM( I+1 ) - FM( I ) ) / DS( I )
            END DO

701      CONTINUE

      END DO
#endif



      RETURN
      END SUBROUTINE VPPM

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PPM ( NI, DT, DS, CN, CR, CL, DC, C6 )

C get ppm coefficients CR, CL, DC, and C6

      USE UTILIO_DEFN

      IMPLICIT NONE

C arguments:

      INTEGER, INTENT(    IN ) :: NI       ! number of zones (cells)
      REAL,    INTENT(    IN ) :: DT       ! time step
      REAL,    INTENT(    IN ) :: DS( NI ) ! distance between zone (cell) boundaries
      REAL,    INTENT(    IN ) :: CN( NI ) ! concentrations in a vertical column
      REAL,    INTENT( INOUT ) :: CR( NI ) ! zone r.h. intercept
      REAL,    INTENT( INOUT ) :: CL( NI ) ! zone l.h. intercept
      REAL,    INTENT( INOUT ) :: DC( NI ) ! CR - CL
      REAL,    INTENT( INOUT ) :: C6( NI ) ! coefficient of second-order term

C local variables:

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER ALLOCSTAT
      CHARACTER( 120 ) :: XMSG = ' '

      REAL A, B, C              ! temp lattice vars.

      REAL, ALLOCATABLE, SAVE :: ALPHA ( : )  ! temp lattice var.
      REAL,              SAVE :: BETA
      REAL, ALLOCATABLE, SAVE :: CHI   ( : )  ! lattice var. for dc
      REAL, ALLOCATABLE, SAVE :: PSI   ( : )  ! lattice var. for dc
      REAL, ALLOCATABLE, SAVE :: MU    ( : )  ! lattice var. for cm
      REAL, ALLOCATABLE, SAVE :: NU    ( : )  ! lattice var. for cm
      REAL, ALLOCATABLE, SAVE :: LAMBDA( : )  ! lattice var. for cm
      REAL, ALLOCATABLE, SAVE :: CM    ( : )  ! zone r.h. trial intercept

      INTEGER I

C----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         ALLOCATE ( ALPHA ( 2:NI-1 ),
     &              CHI   ( 2:NI-1 ),
     &              PSI   ( 2:NI-1 ),
     &              MU    ( 2:NI-2 ),
     &              NU    ( 2:NI-2 ),
     &              LAMBDA( 2:NI-2 ),
     &              CM    ( 1:NI+1 ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating '
     &           // 'ALPHA, MU, NU, LAMBDA, CHI, PSI, OR CM'
            CALL M3EXIT( 'VPPM', 0, 0, XMSG, XSTAT1 )
         END IF

         DO I = 2, NI-1
            ALPHA( I ) = DS( I ) + DS( I+1 )
            BETA = DS( I-1 ) + DS( I )
            C = DS( I ) / ( BETA + DS( I+1 ) )
            CHI( I ) = C * ( DS( I-1 ) + BETA ) / ALPHA( I )
            PSI( I ) = C * ( ALPHA( I ) + DS( I+1 ) ) / BETA
         END DO
         DO I = 2, NI-2
            A = DS( I ) / ALPHA( I )
            B = 2.0 * DS( I+1 ) / ALPHA( I )
            C = 1.0 / ( DS( I-1 ) + ALPHA( I ) + DS( I+2 ) )
            MU( I ) = C * DS( I )
     &              * ( DS( I-1 ) + DS( I ) )   / ( DS( I )   + ALPHA( I ) )
            NU( I ) = C * DS( I+1 )
     &              * ( DS( I+1 ) + DS( I+2 ) ) / ( DS( I+1 ) + ALPHA( I ) )
            LAMBDA( I ) = A + MU( I ) * B - 2.0 * NU( I ) * A
         END DO

      END IF   ! FIRSTIME

C zeroth order polynomial at the boundary cells
C first order polynomial at the next cells, no monotonicity constraint needed

      CM( 1 )    = CN( 1 )
      CM( 2 )    = ( DS( 1 ) * CN( 2 ) + DS( 2 ) * CN( 1 ) )
     &           / ( DS( 1 ) + DS( 2 ) )
      CM( NI+1 ) = CN( NI )
      CM( NI )   = ( DS( NI-1 ) * CN( NI ) + DS( NI ) * CN( NI-1 ) )
     &           / ( DS( NI-1 ) + DS( NI ) )

C second order polynomial inside the domain

      DO 101 I = 2, NI-1

C compute average slope in zone i

      DC( I ) = CHI( I ) * ( CN( I+1 ) - CN( I ) )
     &        + PSI( I ) * ( CN( I )   - CN( I-1 ) )      ! equation (1.7)

C guarantee that cm lies between con(i) and con(i+1) - monotonicity constraint

         IF ( ( CN( I+1 ) - CN( I ) ) * ( CN( I ) - CN( I-1 ) ) .GT. 0.0 ) THEN
            DC( I ) = SIGN( 1.0, DC( I ) ) * MIN(
     &                                      ABS( DC( I ) ),
     &                                2.0 * ABS( CN( I+1 ) - CN( I ) ),
     &                                2.0 * ABS( CN( I ) - CN( I-1 ) ) )
         ELSE
            DC( I ) = 0.0
         END IF                                    ! equation (1.8)

101   CONTINUE

      DO I = 2, NI-2                                ! equation (1.6)
         CM( I+1 ) = CN( I ) + LAMBDA( I ) * ( CN( I+1 ) - CN( I ) )
     &             - MU( I ) * DC( I+1 ) + NU( I ) * DC( I )
      END DO

C generate piecewise parabolic distributions

      DO 301 I = 1, NI

         CR( I ) = CM( I+1 )         ! equation (1.15)
         CL( I ) = CM( I )

C monotonicity

         IF ( ( CR( I ) - CN( I ) ) * ( CN( I ) - CL( I ) ) .GT. 0.0 ) THEN

            DC( I ) = CR( I ) - CL( I )  ! temporary computation of dc and c6
            C6( I ) = 6.0 * ( CN( I ) - 0.5 * ( CL( I ) + CR( I ) ) )

C overshoot cases

            IF ( DC( I ) * C6( I ) .GT. DC( I ) * DC( I ) ) THEN
               CL( I ) = 3.0 * CN( I ) - 2.0 * CR( I )
            ELSE IF ( -DC( I ) * DC( I ) .GT. DC( I ) * C6( I ) ) THEN
               CR( I ) = 3.0 * CN( I ) - 2.0 * CL( I )
            END IF

         ELSE     ! local extremum: interpolation function is set to be a constant
            CL( I ) = CN( I )
            CR( I ) = CN( I )

         END IF

         DC( I ) = CR( I ) - CL( I )      ! EQUATION (1.5)
         C6( I ) = 6.0 * ( CN( I ) - 0.5 * ( CL( I ) + CR( I ) ) )

301   CONTINUE

      RETURN
      END SUBROUTINE PPM

