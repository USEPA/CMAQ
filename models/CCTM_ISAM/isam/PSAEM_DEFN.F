      MODULE PSAEM_DEFN
c
c20140428  Has function PSAEM_INIT
c 
c-----------------------------------------------------------------------

!      USE HGRD_DEFN    ! horizontal domain specifications
      USE GRID_CONF    ! horizontal domain specifications
      USE UTILIO_DEFN  ! 20120712

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE, TARGET :: PSAEM( :,:,:,:,: )

      CONTAINS
        FUNCTION PSAEM_INIT (NSPC_TSSA, NTAG_TSSA) RESULT ( SUCCESS )

C-----------------------------------------------------------------------
C20140428   Create ISAMEM target, PSAEM; just like ISAM on PISAM.
C           Stores emission rates of individual sectors.
C            Called by vdiffacm2.F
C      
C-----------------------------------------------------------------------

!         INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
!0712    INCLUDE SUBST_IODECL      ! I/O definitions and declarations

         INTEGER :: NSPC_TSSA
         INTEGER :: NTAG_TSSA

         LOGICAL :: SUCCESS
         INTEGER :: ALLOCSTAT
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 120 ) :: XMSG = ' '

C PSAEM extents
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
            
            ALLOCATE ( PSAEM( LCOL:HCOL,
     &                        LROW:HROW,
     &                        NLAYS,
     &                        NSPC_TSSA,
     &                        NTAG_TSSA ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating PSAEM'
               CALL M3WARN ( 'PSAEM_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF
            
            PSAEM = 0.0
            
         ELSE
            XMSG = 'PSAEM already ALLOCATED'
            CALL M3WARN ( 'PSAEM_INIT', 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         
         RETURN
         END FUNCTION PSAEM_INIT
      
      END MODULE PSAEM_DEFN
