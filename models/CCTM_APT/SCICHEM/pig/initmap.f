      SUBROUTINE INITMAP (JDATE, JTIME, TSTEP)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Initialize map factors from host
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!    Updated April 2005 for CMAQ-APT-PM (PKK, AER)
!
!******************************************************************************
 
! --- MODULES AND INCLUDES

      USE HGRD_DEFN             ! horizontal domain specifications
 
!     USE COMMON_PUF
      USE COMMON_MET
      USE HOST_INC

      IMPLICIT NONE

! --- ARGUMENTS
 
      INTEGER      JDATE                   ! current date (YYYYDDD)
      INTEGER      JTIME                   ! current time (HHMMSS)
      INTEGER      TSTEP                   ! model time step (HHMMSS)

! --- LOCALS
      CHARACTER( 16 ) :: VARNM

      CHARACTER( 16 ) :: PNAME = 'INITMAP'
      CHARACTER( 120 ) :: XMSG = ' '

C.......   Nonlayered variables

      REAL, DIMENSION(:,:),ALLOCATABLE :: MSFX2  ! square of map scale factor

      INTEGER      COL, ROW, IJ, JJ            ! loop variables
      INTEGER :: IOS

      ALLOCATE ( MSFX2( GL_NCOLS, GL_NROWS ), STAT = IOS)
      IF (IOS /= 0) THEN

         XMSG = 'Could not allocate MSFX2 array '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      VARNM = 'MSFX2'
      IF ( .NOT. INTERPX( GRID_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    JDATE, JTIME, MSFX2 ) ) THEN
!     IF ( .NOT. INTERP3( GRID_CRO_2D, 'MSFX2' , PNAME,
!    &                    JDATE, JTIME, GL_NCOLS * GL_NROWS,
!    &                    MSFX2 ) ) THEN

         XMSG = 'Could not read MSFX2 from ' // GRID_CRO_2D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF      !  if INTERPX failed

C...... Set map factors
      DO ROW = 1, NYB
         JJ = ( ROW - 1 ) * NXB
         DO COL = 1, NXB
            IJ = JJ + COL
            XMAP2D( IJ ) = SQRT( MSFX2( COL, ROW ) )
            YMAP2D( IJ ) = XMAP2D( IJ )
         END DO
      END DO

! --- Release memory
      DEALLOCATE( MSFX2 )

      RETURN
      END
