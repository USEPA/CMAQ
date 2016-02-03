      MODULE PISAMB4_DEFN
c
c20140428 Has function PISAMB4_INIT
c
c 
c-----------------------------------------------------------------------

!      USE HGRD_DEFN    ! horizontal domain specifications
      USE GRID_CONF    ! horizontal domain specifications
      USE UTILIO_DEFN  ! 20120615

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE, TARGET :: PISAMB4( :,:,:,:,: )

      CONTAINS
        FUNCTION PISAMB4_INIT (NSPC_ISAM, NTAG_ISAM) RESULT ( SUCCESS )

C-----------------------------------------------------------------------
C20140428   Create ISAMB4 target, PISAMB4; just like ISAM on PISAM.
C           ISAMB4 backs up ISAM before the latter gets changed.
C            Called by driver.F
C      
C-----------------------------------------------------------------------

!         INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
!0615    INCLUDE SUBST_IODECL      ! I/O definitions and declarations

         INTEGER :: NSPC_ISAM
         INTEGER :: NTAG_ISAM

         LOGICAL :: SUCCESS
         INTEGER :: ALLOCSTAT
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 120 ) :: XMSG = ' '

C PISAMB4 extents
         INTEGER :: LCOL
         INTEGER :: HCOL
         INTEGER :: LROW
         INTEGER :: HROW

         SUCCESS = .TRUE.

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.

#ifdef parallel
            LCOL = 1 - NTHIK
            HCOL = NCOLS + NTHIK
            LROW = 1 - NTHIK
            HROW = NROWS + NTHIK
#else
            LCOL = 0
            HCOL = NCOLS+1
            LROW = 0
            HROW = NROWS+1
#endif
            
            ALLOCATE ( PISAMB4( LCOL:HCOL,
     &                        LROW:HROW,
     &                        NLAYS,
     &                        NSPC_ISAM,
     &                        NTAG_ISAM ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating PISAMB4'
               CALL M3WARN ( 'PISAMB4_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
            
            PISAMB4 = 0.0
            
         ELSE
            XMSG = 'PISAMB4 already ALLOCATED'
            CALL M3WARN ( 'PISAMB4_INIT', 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         
         RETURN
         END FUNCTION PISAMB4_INIT
      
      END MODULE PISAMB4_DEFN
