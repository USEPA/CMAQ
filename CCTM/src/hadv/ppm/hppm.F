
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
      SUBROUTINE HPPM ( NI, NJ, CON, VEL, DT, DS, ORI,
     &                  F_LO_IN, F_LO_OUT, F_HI_IN, F_HI_OUT )
      
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
C     |<------>|<------>|       ~|<------>|~       |<------>|<------>|
C       CON(0)   CON(1)            CON(i)            CON(n)  CON(n+1)
C     
C     VEL(1)-->|        VEL(i)-->|        |-->VEL(i+1)      |-->VEL(n+1)
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

C   Sep 97 Jeff
C   Aug 98 - Jeff - optimize for mesh coefficients      

C   David Wong - Sep. 1998
C     -- parallelized the code
C     -- Expanded the one-level nested loop which involves either with row or
C        column, into a three-level nested loop with layers and species.
C        Corresponding arrays' dimensions were adjusted accordingly
C   Jeff - optimize for mesh coefficients
C
C   David Wong - 1/8/99
C     -- BARRIER is removed
C
C   David Wong - 1/12/99
C     -- inside BNDY_HI_PE conditional code segment, NI is changed to MY_NI
C
C   David Wong - 1/12/99
C     -- change se_loop_index argument list
C     -- add new subroutine call to determine lo and hi boundary processor

C   22 Nov 00 J.Young: PE_COMM2E -> Dave Wong's f90 stenex COMM
C                      PE_COMM3E -> Dave Wong's f90 stenex COMM

C   23 Feb 01 J.Young: allocatable arrays ...
C                      Since F90 does not preserve dummy argument array
C                      indices, CONI( 1:NI+2,, ) is copied into local array
C                      CON( 0:NI+1,, ).
C                      The caller of HPPM dimensions the actual argument,
C                      as CON( -NTHIK+1:MY_NCOLS+NTHIK,, ).

C   3 Sep 01 David Wong
C     -- use "dynamic" data structure instead of F90 ALLOCATE statement to
C        avoid memory fragmentation which eventually leads to not enough
C        contigous memory (F90 bug?)
C   24 Mar 04 G.Hammond: moved all mpi communication to caller

C   06/16/04 by Peter Percell & Daewon Byun at UH-IMAQS: 
C     - Fixed bug in using fluxes in non-uniform grids to update concentrations

C   14 Feb 05 J.Young: fix DS dimension bug
C   11 Oct 05 J.Young: re-dimension lattice arrays to one
C    1 Nov 06 J.Young: Following Glenn Hammond, moved all communication
C   out of HPPM; using "swap_sandia" communication in caller; update only
C   local values in the CGRID array within a time step, discarding previous
C   ghost values.
C    1 May 07 J.Young: Following Peter Percell, eliminate CONI,DSI using interface
C   specification in caller
C   11 May 09 J.Young: Simplify - remove STEEPEN option (never used); assume constant
C                      cell widths, DS( i )
C   11 May 10 D.Wong: Change local dynamic arrays: make allocatable to enable proper
C                     PGI compiliation; fix a max first dimension
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN

C----------------------------------------------------------------------
      
      USE HGRD_DEFN
      USE UTILIO_DEFN
      USE PA_DEFN, ONLY : BUDGET_DIAG, BUDGET_HPPM
#ifdef parallel
      USE SE_MODULES          ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES        ! stenex (using NOOP_UTIL_MODULE)
#endif

      IMPLICIT NONE

C Includes:
      
! #ifdef parallel
      INTEGER, PARAMETER :: SWP = 3
      INTEGER, PARAMETER :: X1 = 1
      INTEGER, PARAMETER :: X2 = 2
      INTEGER, PARAMETER :: X3 = 3
! #else
!     INTEGER, PARAMETER :: SWP = 1
!     INTEGER, PARAMETER :: X1 = 0
!     INTEGER, PARAMETER :: X2 = 0
!     INTEGER, PARAMETER :: X3 = 0
! #endif

C Arguments:
 
      INTEGER, INTENT( IN )    :: NI, NJ      ! number of zones (cells)
      REAL,    INTENT( INOUT ) :: CON( 1-SWP:,1: ) ! conc's in the zones (cells)
      REAL,    INTENT( IN )    :: VEL( : )    ! velocities at zone (cell) boundaries
      REAL,    INTENT( IN )    :: DT          ! time step
      REAL,    INTENT( IN )    :: DS          ! distance between zone (cell) boundaries
      CHARACTER, INTENT( IN )  :: ORI         ! orientation of advection ('C'-x or 'R'-y)

C Parameters:
      
      REAL, PARAMETER :: TWO3RDS = 2.0 / 3.0
      REAL, PARAMETER :: SIXTH   = 1.0 / 6.0

C Local variables:

      CHARACTER, SAVE :: FIRSTORI = ' '   ! for test if Col or Row orientation change
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      
      INTEGER, SAVE :: NSPCS
      INTEGER, SAVE :: NSPCS_BUDGET = 0

!     REAL :: FM    (    1:NI+1,   SIZE( CON,2 ) ) ! outflux from left or bottom of cell
!     REAL :: FP    (    0:NI,     SIZE( CON,2 ) ) ! outflux from right or top of cell

!     REAL :: CM    ( 1-X1:NI+X1+1,SIZE( CON,2 ) ) ! zone R.H. trial intercept
!     REAL :: CL    ( 1-X1:NI+X1 )                 ! zone L.H. intercept
!     REAL :: CR    ( 1-X1:NI+X1 )                 ! zone R.H. intercept
!     REAL :: DC    ( 0-X1:NI+X1+1,SIZE( CON,2 ) ) ! CR - CL
!     REAL :: C6    ( 1-X1:NI+X1 )                 ! coefficient of second-order term

      REAL, ALLOCATABLE, SAVE :: FM( :,: ) ! outflux from left or bottom of cell
      REAL, ALLOCATABLE, SAVE :: FP( :,: ) ! outflux from right or top of cell

      REAL, ALLOCATABLE, SAVE :: CM( :,: ) ! zone R.H. trial intercept
      REAL, ALLOCATABLE, SAVE :: CL( : )   ! zone L.H. intercept
      REAL, ALLOCATABLE, SAVE :: CR( : )   ! zone R.H. intercept
      REAL, ALLOCATABLE, SAVE :: DC( :,: ) ! CR - CL
      REAL, ALLOCATABLE, SAVE :: C6( : )   ! coefficient of second-order term
      REAL C0, C1

      ! Boundary fluxes returned for budget calculations
      ! Units of boundary fluxes are assumed to be 
      ! density (aerosols) or mixing ratio (gases). These values
      ! are in 'coupled' space, meaning they need to be decoupled
      ! using the Jacobian and map scale factor in order to properly
      ! contribute to total mass calculations.
      REAL, INTENT( OUT) :: F_LO_IN( : )  ! influx to domain from left of row or bottom of column
      REAL, INTENT( OUT) :: F_LO_OUT(: ) ! outflux to domain from left of row or bottom of column
      REAL, INTENT( OUT) :: F_HI_IN( : )  ! influx to domain from right of row or bottom of top
      REAL, INTENT( OUT) :: F_HI_OUT(: ) ! outflux to domain from right of row or bottom of top

      LOGICAL, SAVE :: BNDY_LO_PE, BNDY_HI_PE

      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 16 ) :: PNAME = 'HPPM'

      REAL X, Y                 ! Courant number
      INTEGER NMX, ASTAT
      
      INTEGER I, S              ! loop indices

C----------------------------------------------------------------------

#ifdef isam
      FIRSTIME = .TRUE.
#else
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
#endif
         NMX = MAX( NI,NJ )
         NSPCS = SIZE ( CON,2 )
         ALLOCATE( FM(     1:NMX+1,    NSPCS ),
     &             FP(     0:NMX,      NSPCS ),
     &             CM(  1-X1:NMX+X1+1, NSPCS ),
     &             CL(  1-X1:NMX+X1 ),
     &             CR(  1-X1:NMX+X1 ),
     &             DC(  0-X1:NMX+X1+1, NSPCS ),
     &             C6(  1-X1:NMX+X1 ), 
     &             STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = '*** Error allocating FM, FP, CM, CL, CR, DC, or C6'
            CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF
         
#ifdef isam
#else
      END IF   ! Firstime
#endif

      IF ( ORI .NE. FIRSTORI ) THEN
         FIRSTORI = ORI
         CALL SUBST_HI_LO_BND_PE ( ORI, BNDY_LO_PE, BNDY_HI_PE )
      END IF   ! FIRSTORI

C Set all fluxes to zero. Either positive or negative flux will remain zero
C depending on the sign of the velocity.

      FM( 1:NI+1,: ) = 0.0
      FP( 0:NI,: ) = 0.0

! #ifndef parallel
C If PE near bottom or left domain boundary...
C Zeroth order polynomial at the boundary cells
C First order polynomial at the next cells, no monotonicity constraint needed
!     IF ( BNDY_LO_PE ) THEN
!        DO S = 1, NSPCS
!           CM( 1,S ) = CON( 1,S )
!           CM( 2,S ) = 0.5 * ( CON( 1,S ) + CON( 2,S ) )
!        END DO
!     END IF

C If PE near top or right domain boundary...
C Zeroth order polynomial at the boundary cells
C First order polynomial at the next cells, no monotonicity constraint needed
!     IF ( BNDY_HI_PE ) THEN
!        DO S = 1, NSPCS
!           CM( NI+1,S ) = CON( NI,S )
!           CM( NI,S )   = 0.5 * ( CON( NI,S ) + CON( NI-1,S ) )
!        END DO
!     END IF
! #endif
      
C Second order polynomial inside the domain
      
      DO S = 1, NSPCS
         DO I = 2 - X3, NI + X3 - 1
      
C Compute average slope in the i'th zone
      
C Equation (1.7)
            C0 = CON( I,S )   - CON( I-1,S )
            C1 = CON( I+1,S ) - CON( I,S )
            DC( I,S ) = 0.5 * ( C0 + C1 )
      
C Guarantee that CM lies between CON(I) and CON(I+1) - monotonicity constraint

C Equation (1.8)
            IF ( C0 * C1 .GT. 0.0 ) THEN
               DC( I,S ) = SIGN( 1.0, DC( I,S ) )
     &                   * MIN(      ABS( DC( I,S ) ),
     &                         2.0 * ABS( C0 ),
     &                         2.0 * ABS( C1 ) )
            ELSE
               DC( I,S ) = 0.0
            END IF

         END DO   ! I

C Equation (1.6)
         DO I = 3 - X3, NI + X3 - 1
            CM( I,S ) = 0.5 * ( CON( I,S ) + CON( I-1,S ) )
     &                - SIXTH * ( DC( I,S ) - DC( I-1,S ) )
         END DO

      END DO   ! S

C Generate piecewise parabolic distributions

      DO S = 1, NSPCS

         DO I = 1 - X1, NI + X1

C Equation (1.15)
            CR( I ) = CM( I+1,S )
            CL( I ) = CM( I,S )
 
C Monotonicity
 
            IF ( ( CR( I ) - CON( I,S ) )
     &        * ( CON( I,S ) - CL( I ) ) .GT. 0.0 ) THEN

C Temporary computation of DC and C6
               DC( I,S ) = CR( I ) - CL( I )
               C6( I ) = 6.0 * ( CON( I,S ) - 0.5 * ( CL( I ) + CR( I ) ) )

C overshoot cases - Equation (1.10)
               IF ( DC( I,S ) * C6( I ) .GT.
     &              DC( I,S ) * DC( I,S ) ) THEN
                  CL( I ) = 3.0 * CON( I,S ) - 2.0 * CR( I )
               ELSE IF ( -DC( I,S ) * DC( I,S ) .GT.
     &                    DC( I,S ) * C6( I ) ) THEN
                  CR( I ) = 3.0 * CON( I,S ) - 2.0 * CL( I )
               END IF

            ELSE                   ! Local extremum: Interpolation  
                                   ! function is set to be a constant
               CL( I ) = CON( I,S )
               CR( I ) = CL( I )

            END IF

            DC( I,S ) = CR( I ) - CL( I )      ! Equation (1.5)
            C6( I ) = 6.0 * ( CON( I,S ) - 0.5 * ( CL( I ) + CR( I ) ) )

         END DO   ! I

C Compute fluxes from the parabolic distribution as in Equation (1.12)

! #ifdef parallel
!        I = 0
!        IF ( VEL( I+1 ) .GT. 0.0 ) THEN
!           Y = VEL( I+1 ) * DT
!           X = Y / DS
!           FP( I,S ) = Y * ( CR( I ) - 0.5 * X * ( DC( I,S )
!    &                - C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
!        END IF
! #endif
      
!        IF ( BNDY_LO_PE ) THEN
            I = 0
            IF ( VEL( I+1 ) .GT. 0.0 ) THEN
               Y = VEL( I+1 ) * DT
               X = Y / DS
               FP( I,S ) = Y * ( CR( I ) - 0.5 * X * ( DC( I,S )
     &                   - C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
            END IF
!        END IF

         DO I = 1, NI

C function for mass leaving interval I at lower face (I-1/2)
C = length of segment leaving * integral average concentration in that segment
            IF ( VEL( I ) .LT. 0.0 ) THEN
               Y = -VEL( I ) * DT
               X = Y / DS
               FM( I,S ) = Y * ( CL( I ) + 0.5 * X * ( DC( I,S )
     &                   + C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
            END IF

C function for mass leaving interval I at upper face (I+1/2)
            IF ( VEL( I+1 ) .GT. 0.0 ) THEN
               Y = VEL( I+1 ) * DT
               X = Y / DS
               FP( I,S ) = Y * ( CR( I ) - 0.5 * X * ( DC( I,S )
     &                   - C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
            END IF

         END DO   ! I

! #ifdef parallel
!        I = NI + 1
!        IF ( VEL( I ) .LT. 0.0 ) THEN
!           Y = -VEL( I ) * DT
!           X = Y / DS
!           FM( I,S ) = Y * ( CL( I ) + 0.5 * X * ( DC( I,S )
!    &                + C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
!        END IF
! #endif
!        IF ( BNDY_HI_PE ) THEN
            I = NI + 1
            IF ( VEL( I ) .LT. 0.0 ) THEN
               Y = -VEL( I ) * DT
               X = Y / DS
               FM( I,S ) = Y * ( CL( I ) + 0.5 * X * ( DC( I,S )
     &                   + C6( I ) * ( 1.0 - TWO3RDS * X ) ) )
            END IF
!        END IF

      END DO   ! S

C Compute fluxes from boundary cells
      
C If PE near top or left boundary...
      IF ( BNDY_LO_PE ) THEN
         IF ( VEL( 1 ) .GT. 0.0 ) THEN
            Y = VEL( 1 ) * DT
            DO S = 1, NSPCS
               FP( 0,S ) = Y * CON( 0,S )
            END DO
         END IF
      END IF

C If PE near bottom or right boundary...
      IF ( BNDY_HI_PE ) THEN
         IF ( VEL( NI+1 ) .LT. 0.0 ) THEN
            Y = -VEL( NI+1 ) * DT
            DO S = 1, NSPCS
               FM( NI+1,S ) = Y * CON( NI+1,S )
            END DO
         END IF
      END IF

C Update concentrations as in Equation (1.13)
      DO S = 1, NSPCS
         DO I = 1, NI
            CON( I,S ) = CON( I,S )
     &                 + ( FP( I-1,S ) - FP( I,S ) + FM( I+1,S ) - FM( I,S ) ) / DS
         END DO
      END DO

C Store boundary fluxes in budget variables
C   Units: change in density (aerosols) or mixing ratio (gases)
      IF ( BUDGET_HPPM ) THEN
         NSPCS_BUDGET = MIN( NSPCS,SIZE( F_LO_IN ) )
         F_LO_IN ( 1:NSPCS_BUDGET ) = FP( 0,1:NSPCS_BUDGET ) / DS
         F_LO_OUT( 1:NSPCS_BUDGET ) = FM( 1,1:NSPCS_BUDGET ) / DS
         F_HI_OUT( 1:NSPCS_BUDGET ) = FP( NI,1:NSPCS_BUDGET ) / DS
         F_HI_IN ( 1:NSPCS_BUDGET ) = FM( NI+1,1:NSPCS_BUDGET ) / DS
      END IF

#ifdef isam
Ckrt...deallocate local arrays....20140126
         IF ( ALLOCATED( FM ) ) DEALLOCATE( FM )

         IF ( ALLOCATED( FP ) ) DEALLOCATE( FP )

         IF ( ALLOCATED( CM ) ) DEALLOCATE( CM )

         IF ( ALLOCATED( CL ) ) DEALLOCATE( CL )

         IF ( ALLOCATED( CR ) ) DEALLOCATE( CR )

         IF ( ALLOCATED( DC ) ) DEALLOCATE( DC )

         IF ( ALLOCATED( C6 ) ) DEALLOCATE( C6 )
#endif

      RETURN
      END
