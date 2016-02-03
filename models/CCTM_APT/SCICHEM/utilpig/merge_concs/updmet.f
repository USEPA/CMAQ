      SUBROUTINE UPDMET( JDATE, JTIME, TSTEP )

!      USE HGRD_DEFN             ! horizontal domain specifications
!      USE VGRD_DEFN             ! vertical layer specifications
      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE COMMON_PUF
      USE MULTCOMP_INC
      USE COMMON_MET
      USE HOST_INC
!      USE M3UTILIO

!------ Update meteorological fields for JDATE and JTIME

      IMPLICIT NONE

      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS
      INTEGER         TSTEP     !  model time step,  format HHMMSS

!....... Local variables
      INTEGER         MDATE     !  date at mid-time step, coded YYYYDDD
      INTEGER         MTIME     !  time at mid-time step, coded HHMMSS
      INTEGER         MSTEP     !  time step, format seconds

      INTEGER :: IOS

      REAL, PARAMETER :: STDATMPA = 101325.  ! Standard atmospheric pressure (Pa)
      REAL, PARAMETER :: PA2ATM = 1.0 / STDATMPA   ! conv. factor from Pascals to atmospheres

!     
!     Scratch local variables and their descriptions:
!     
      INTEGER ROW                   ! Row index
      INTEGER COL                   ! Column index
      INTEGER LVL                   ! Layer index
      INTEGER JJ, IJ, JK, KK, IJK   ! Indices for SCICHEM 1-D arrays

!.......   Layered variables from host model
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: TA    ! temperature (K)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: PRES  ! pressure (Pa)

      CHARACTER( 16 ) :: PNAME = 'UPDMET'
      CHARACTER( 120 ) :: XMSG = ' '

! ... External Functions (not already declared by IODECL3.EXT):

!     INTEGER, EXTERNAL :: SEC2TIME, TIME2SEC

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      MDATE = JDATE
      MTIME = JTIME
!!      MSTEP = TIME2SEC( TSTEP )
!!      CALL NEXTIME ( MDATE, MTIME, SEC2TIME( MSTEP / 2 ) )

!........  Read & Interpolate TA
      ALLOCATE(TA(GL_NCOLS, GL_NROWS, NLAYS), STAT = IOS)
      IF (IOS /= 0) THEN

         XMSG = 'Could not allocate TA array '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'TA', PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                    MDATE, MTIME, TA ) ) THEN

         XMSG = 'Could not read TA from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF      !  if INTERPX failed

C Read & Interpolate PRES
      ALLOCATE(PRES(GL_NCOLS, GL_NROWS, NLAYS), STAT = IOS)
      IF (IOS /= 0) THEN

         XMSG = 'Could not allocate PRES array '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'PRES', PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                    MDATE, MTIME, PRES ) ) THEN

         XMSG = 'Could not read PRES from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed

!------ SCICHEM 3D fields
      DO LVL = 1, NZB
!pk start at layer 2 and go to layer nzb + 1
!pk         KK = (LVL - 1)*NXYB
         KK = LVL*NXYB
         DO ROW = 1, NYB
            JK = KK + (ROW - 1)*NXB
            DO COL = 1, NXB
!pk               IJK = LVL*NXYB + (ROW - 1)*NXB + COL
               IJK = JK + COL

               P_UA(IJK)  = PRES ( COL  , ROW  , LVL ) * PA2ATM
               T_UA(IJK)  = TA ( COL  , ROW  , LVL ) /
     &                      P_UA(IJK)**0.28571  !POTENTIAL TEMPERATURE (K)
            END DO
         END DO
      END DO

      DO IJ = 1, NXYB               !FIRST LAYER IN THE GROUND
         T_UA(IJ) = T_UA(IJ + NXYB)
         P_UA(IJ) = P_UA(IJ + NXYB)
      END DO

! --- Release memory
      DEALLOCATE(PRES)
      DEALLOCATE(TA)

9999  RETURN
      END
