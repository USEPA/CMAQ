      MODULE PMFRC_DEFN
c
c20140428  Has function PMFRC_INIT
c 
c-----------------------------------------------------------------------

!      USE HGRD_DEFN    ! horizontal domain specifications
      USE GRID_CONF    ! horizontal domain specifications
      USE UTILIO_DEFN  ! 20120615

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE, TARGET :: PMFRC( :,:,: )

      CONTAINS
        FUNCTION PMFRC_INIT ( NTAG_ISAM ) RESULT ( SUCCESS )

C-----------------------------------------------------------------------
C20140428   Create MFRC_P target, PMFRC; just like CGRID on PCGRID.
C           MFRC_P map fractions from region map file.
C            Called by driver.F
C      
C-----------------------------------------------------------------------

!         INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
!0615    INCLUDE SUBST_IODECL      ! I/O definitions and declarations

         INTEGER :: NTAG_ISAM

         LOGICAL :: SUCCESS
         INTEGER :: ALLOCSTAT
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 120 ) :: XMSG = ' '

C PMFRC extents
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
            
            ALLOCATE ( PMFRC( LCOL:HCOL,
     &                        LROW:HROW,
     &                        NTAG_ISAM ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating PMFRC'
               CALL M3WARN ( 'PMFRC_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
            
            PMFRC = 0.0
            
         ELSE
            XMSG = 'PMFRC already ALLOCATED'
            CALL M3WARN ( 'PMFRC_INIT', 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         
         RETURN
         END FUNCTION PMFRC_INIT
      
      END MODULE PMFRC_DEFN
