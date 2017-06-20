
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
C $Header: /project/work/rep/PARIO/src/interpol.f,v 1.5 2011/03/30 18:13:01 sjr Exp $

      LOGICAL FUNCTION INTERPOL ( DATE, TIME, DATE1, TIME1, DATE2, TIME2,
     &                            NUMVALS, VALSIN1, VALSIN2, VALSOUT, S_IND, E_IND )
C ....................................................................
 
C  PURPOSE:  Interpolates data given at times DATE1:TIME1 and
C            DATE2:TIME2 to data at time DATE:TIME, using the
C            linear formula:
 
C       VALSOUT = P*VALSIN1 + Q*VALSIN2  where
 
C       P = ( DATE2:TIME2 - DATE:TIME ) / ( DATE2:TIME2 - DATE1:TIME1 )
C       Q = ( DATE:TIME - DATE1:TIME1 ) / ( DATE2:TIME2 - DATE1:TIME1 )
 
C       If DATE1:TIME1 = DATE2:TIME2, the data is assumed time independent
C       and VALSOUT is set to VALSIN1.
 
C  RETURN VALUE:  TRUE if DATE1:TIME1 = DATE2:TIME2, or if DATE:TIME
C                 is after DATE1:TIME1 and before DATE2:TIME2.
 
C  REVISION HISTORY: 
C       Original version  4/96 by Al Bourgeois for parallel implementation.
C       Modified 02/23/2011 by Shawn Roselle
C          -- Replaced I/O API include files with M3UTILIO
 
C  ARGUMENT LIST DESCRIPTION:
C  IN:
C        INTEGER    DATE               ! Date to interpolate to (YYYYDDD).
C        INTEGER    TIME               ! Time to interpolate to (HHMMSS).
C        INTEGER    DATE1              ! Beginning date (for VALSIN1).
C        INTEGER    TIME1              ! Beginning time (for VALSIN1).
C        INTEGER    DATE2              ! Ending date (for VALSIN2).
C        INTEGER    TIME2              ! Ending time (for VALSIN2).
C        INTEGER    NUMVALS            ! Number of values to interpolate.
C        REAL       VALSIN1( NUMVALS ) ! Values at DATE1:TIME1.
C        REAL       VALSIN2( NUMVALS ) ! Values at DATE2:TIME2.
C  OUT:
C        REAL       VALSOUT( NUMVALS ) ! Interpolated values.
 
C  PRECONDITIONS:  Either DATE1:TIME1 = DATE2:TIME2 (which designates
C                  time independent data), or else DATE:TIME must
C                  be after DATE1:TIME1 and before DATE2:TIME2. 
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  CALLS: SECSDIFF
  
C .......................................................................

      USE M3UTILIO              ! i/o api

      IMPLICIT  NONE

C ARGUMENTS:

      INTEGER, INTENT(IN) :: DATE                  ! Date to interpolate to (YYYYDDD).
      INTEGER, INTENT(IN) :: TIME                  ! Time to interpolate to (HHMMSS).
      INTEGER, INTENT(IN) :: DATE1                 ! Beginning date (for VALSIN1).
      INTEGER, INTENT(IN) :: TIME1                 ! Beginning time (for VALSIN1).
      INTEGER, INTENT(IN) :: DATE2                 ! Ending date (for VALSIN2).
      INTEGER, INTENT(IN) :: TIME2                 ! Ending time (for VALSIN2).
      INTEGER, INTENT(IN) :: NUMVALS               ! Number of values to interpolate.
      REAL, INTENT(IN)    :: VALSIN1( NUMVALS )    ! Values at DATE1:TIME1.
      REAL, INTENT(IN)    :: VALSIN2( NUMVALS )    ! Values at DATE2:TIME2.
      REAL, INTENT(OUT)   :: VALSOUT( NUMVALS )    ! Interpolated values.
      INTEGER, INTENT(IN), OPTIONAL :: S_IND, E_IND

C LOCAL VARIABLES: 

      INTEGER       I            ! Loop counter.
      INTEGER       DTVALSIN     ! Seconds between DATE1:TIME1 and DATE2:TIME2.
      INTEGER       DT1          ! Seconds between DATE1:TIME1 and DATE:TIME.
      INTEGER       DT2          ! Scratch variable for time differences.
      REAL          P            ! First-term fraction for interpolation.
      REAL          Q            ! Second-term fraction for interpolation.

C .......................................................................

C Check for time dependency of data

      DTVALSIN = SECSDIFF ( DATE1, TIME1, DATE2, TIME2 )
      IF ( DTVALSIN .EQ. 0 ) THEN

C Data is time independent. Use values in start buffer
         P = 1.0
         Q = 0.0
         INTERPOL = .TRUE.

      ELSE

C Check that DATE:TIME is after DATE1:TIME1 and before DATE2:TIME2

         DT1 = SECSDIFF ( DATE1, TIME1, DATE,  TIME )
         DT2 = SECSDIFF ( DATE,  TIME,  DATE2, TIME2 )

         IF ( DT1 .LT. 0  .OR. DT2 .LT. 0 ) THEN
            INTERPOL = .FALSE.
            RETURN
         ELSE
            INTERPOL = .TRUE.
         END IF

C Set interpolation factors

         IF ( ( DATE .EQ. DATE1 ) .AND. ( TIME .EQ. TIME1 ) ) THEN

            P = 1.0
            Q = 0.0

         ELSE IF ( ( DATE .EQ. DATE2 ) .AND. ( TIME .EQ. TIME2 ) ) THEN

            P = 0.0
            Q = 1.0

         ELSE   

C Logic above ensures DTVALSIN positive and P,Q in [0,1]
 
            Q = FLOAT( DT1 ) / FLOAT( DTVALSIN )
            P = 1.0 - Q

         END IF

      END IF      ! If data is time independent

C Interpolate the data

      IF (PRESENT(S_IND)) THEN
         DO I = S_IND, E_IND
            VALSOUT( I ) = P * VALSIN1( I ) + Q * VALSIN2( I )
         END DO
      ELSE
         DO I = 1, NUMVALS
            VALSOUT( I ) = P * VALSIN1( I ) + Q * VALSIN2( I )
         END DO
      END IF

      RETURN
      END
