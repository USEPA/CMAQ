
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
      MODULE STK_PRMS

C-----------------------------------------------------------------------
C Function: stack parameters from stack groups file

C Revision History:
C     29 Dec 2006 J.Young: initial implementation
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C      3 Nov 2011 D.Wong: fix source location within grid determination bug
C     30 Apr 2016 J.Young: add multiple fire source capability, in affiliation with
C                          Yongtao Hu (Georgia Tech)
C-----------------------------------------------------------------------

      USE UDTYPES, ONLY: IARRY1, RARRY1

      IMPLICIT NONE

      INTEGER,        ALLOCATABLE, SAVE :: MY_NSRC( : )
      INTEGER,        ALLOCATABLE, SAVE :: NSRC( : )

c     TYPE :: IARRY1
c        INTEGER, POINTER :: ARRY( : )
c        INTEGER          :: LEN
c     END TYPE IARRY1

c     TYPE :: RARRY1
c        REAL,    POINTER :: ARRY( : )
c        INTEGER          :: LEN
c     END TYPE RARRY1

      TYPE( IARRY1 ), ALLOCATABLE, SAVE :: SOURCE     ( : ) ! my stack source index
      TYPE( IARRY1 ), ALLOCATABLE, SAVE :: MY_STKCOL  ( : )
      TYPE( IARRY1 ), ALLOCATABLE, SAVE :: MY_STKROW  ( : )
      TYPE( IARRY1 ), ALLOCATABLE, SAVE :: MY_STKID   ( : )
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: MY_XLOCA   ( : ) ! X-location [grid coord]
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: MY_YLOCA   ( : ) ! Y-location [grid coord]
      INTEGER,        ALLOCATABLE, SAVE :: MY_STRT_SRC( : )
      INTEGER,        ALLOCATABLE, SAVE :: MY_END_SRC ( : )

      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: STKDIAM( : )
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: STKHT  ( : )
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: STKTK  ( : )
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: STKVEL ( : )
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: ACRES_BURNED( : )

      CONTAINS

         FUNCTION STK_PRMS_INIT ( STKPRMS ) RESULT ( SUCCESS )

         USE HGRD_DEFN             ! horizontal domain specifications
         USE UTILIO_DEFN

         IMPLICIT NONE

         CHARACTER( 16 ), INTENT( IN ) :: STKPRMS( : )   ! stack groups file names
         LOGICAL SUCCESS

         REAL,    ALLOCATABLE :: XLOCA( : )
         REAL,    ALLOCATABLE :: YLOCA( : )
         INTEGER, ALLOCATABLE :: STKID( : )
         INTEGER, ALLOCATABLE :: LOC_MAP( : )
         LOGICAL, ALLOCATABLE :: FIRE_ON( : )

         INTEGER SDATE, STIME
         INTEGER SCOL, ECOL, SROW, EROW
         INTEGER NLAYS, C, R, STRT_SRC, END_SRC, MY_DELTA, N, NGRPS
         REAL    X, Y

         INTEGER M, S, SRC, V, ASTAT
         CHARACTER( 96 ) :: XMSG = ' '
         CHARACTER( 16 ) :: PNAME = 'STK_PRMS_INIT'

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         M = MYPE + 1
         SCOL = COLSX_PE( 1,M ); ECOL = COLSX_PE( 2,M )
         SROW = ROWSX_PE( 1,M ); EROW = ROWSX_PE( 2,M )

         NGRPS = SIZE( STKPRMS )

         ALLOCATE ( NSRC( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'NSRC', PNAME )

         ALLOCATE ( MY_NSRC( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'MY_NSRC', PNAME )

         ALLOCATE ( SOURCE( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'SOURCE', PNAME )

         ALLOCATE ( MY_STKCOL( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'MY_STKCOL', PNAME )

         ALLOCATE ( MY_STKROW( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'MY_STKROW', PNAME )

         ALLOCATE ( MY_STKID( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'MY_STKID', PNAME )

         ALLOCATE ( MY_XLOCA( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'MY_XLOCA', PNAME )

         ALLOCATE ( MY_YLOCA( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'MY_YLOCA', PNAME )

         ALLOCATE ( MY_STRT_SRC( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'MY_STRT_SRC', PNAME )

         ALLOCATE ( MY_END_SRC( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'MY_END_SRC', PNAME )

         ALLOCATE ( STKDIAM( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'STKDIAM', PNAME )

         ALLOCATE ( STKHT( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'STKHT', PNAME )

         ALLOCATE ( STKTK( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'STKTK', PNAME )

         ALLOCATE ( STKVEL( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'STKVEL', PNAME )

         ALLOCATE ( FIRE_ON( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'FIRE_ON', PNAME )
         FIRE_ON = .FALSE.   ! array assignment

         ALLOCATE ( ACRES_BURNED( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'ACRES_BURNED', PNAME )

         DO N = 1, NGRPS

            IF ( .NOT. OPEN3( STKPRMS( N ), FSREAD3, PNAME ) ) THEN
               XMSG = 'Could not open '// TRIM( STKPRMS( N ) ) // ' file'
               CALL M3MESG( XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            IF ( .NOT. DESC3( STKPRMS( N ) ) ) THEN
               XMSG = 'Could not get ' // TRIM( STKPRMS( N ) ) // ' file description'
               CALL M3MESG( XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            NSRC( N ) = NROWS3D
            NLAYS = NLAYS3D
            SDATE = SDATE3D
            STIME = STIME3D

            DO V = 1, NVARS3D
               IF ( VNAME3D( V ) .EQ. 'ACRESBURNED' ) FIRE_ON( N ) = .TRUE.
            END DO

            ALLOCATE ( XLOCA( NSRC( N ) ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'XLOCA', PNAME )

            ALLOCATE ( YLOCA( NSRC( N ) ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'YLOCA', PNAME )

            ALLOCATE ( STKID( NSRC( N ) ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'STKID', PNAME )

            ALLOCATE ( LOC_MAP( NSRC( N ) ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'LOC_MAP', PNAME )

            IF ( .NOT. READ3( STKPRMS( N ), 'XLOCA', ALLAYS3,
     &                        SDATE, STIME, XLOCA ) ) THEN
               XMSG = 'Could not read XLOCA from ' // TRIM( STKPRMS( N ) )
               CALL M3MESG( XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            IF ( .NOT. READ3( STKPRMS( N ), 'YLOCA', ALLAYS3,
     &                        SDATE, STIME, YLOCA ) ) THEN
               XMSG = 'Could not read YLOCA from ' // TRIM( STKPRMS( N ) )
               CALL M3MESG( XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            IF ( .NOT. READ3( STKPRMS( N ), 'ISTACK', ALLAYS3,
     &                        SDATE, STIME, STKID ) ) THEN
               XMSG = 'Could not read ISTACK from ' // TRIM( STKPRMS( N ) )
               CALL M3MESG( XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            SOURCE( N )%LEN = NSRC( N )
            ALLOCATE ( SOURCE( N )%ARRY( NSRC( N ) ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'SOURCE', PNAME )

            MY_STKCOL( N )%LEN = NSRC( N )
            ALLOCATE ( MY_STKCOL( N )%ARRY( NSRC( N ) ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'MY_STKCOL', PNAME )

            MY_STKROW( N )%LEN = NSRC( N )
            ALLOCATE ( MY_STKROW( N )%ARRY( NSRC( N ) ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'MY_STKROW', PNAME )

            MY_STKID( N )%LEN = NSRC( N )
            ALLOCATE ( MY_STKID( N )%ARRY( NSRC( N ) ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'MY_STKID', PNAME )

            MY_NSRC( N ) = 0
            STRT_SRC = 0; END_SRC = 0
            DO S = 1, NSRC( N )
               X = ( XLOCA( S ) - REAL( XORIG_GD, 4 ) ) / REAL( XCELL_GD, 4 )
               IF ( X .GE. 0.0 ) THEN
                  C = 1 + INT( X )
               ELSE
                  C = -1 - INT( -X )
               END IF
               IF ( C .GE. SCOL .AND. C .LE. ECOL ) THEN
                  Y = ( YLOCA( S ) - REAL( YORIG_GD, 4 ) ) / REAL( YCELL_GD, 4 )
                  IF ( Y .GE. 0.0 ) THEN
                     R = 1 + INT( Y )
                  ELSE
                     R = -1 - INT( -Y )
                  END IF
                  IF ( R .GE. SROW .AND. R .LE. EROW ) THEN
                     IF ( STRT_SRC .LE. 0 ) STRT_SRC = S
                     END_SRC = S
                     MY_NSRC( N ) = MY_NSRC( N ) + 1
                     LOC_MAP( MY_NSRC( N ) ) = S
                     SOURCE( N )%ARRY( MY_NSRC( N ) ) = S - STRT_SRC + 1  !**
                     MY_STKCOL( N )%ARRY( MY_NSRC( N ) ) = C - SCOL + 1
                     MY_STKROW( N )%ARRY( MY_NSRC( N ) ) = R - SROW + 1
                     MY_STKID( N )%ARRY( MY_NSRC( N ) ) = STKID( S )
                  END IF
               END IF
            END DO

C** SOURCE is the mapping from STKEMIS read by INTERPX starting at one.
C INTERPX reads a contiguous set from MY_STRT_SRC to MY_END_SRC into STKEMIS.
C So MY_STRT_SRC maps into the first index of STKEMIS.
C SOURCE must map skips in data between MY_STRT_SRC and MY_END_SRC.
C SOURCE maps STKEMIS read from stk emis file to local STKEM in GET_PT3D_EMIS.

            MY_STRT_SRC( N ) = STRT_SRC
            MY_END_SRC( N ) = END_SRC
            MY_DELTA = END_SRC - STRT_SRC + 1

            STKDIAM( N )%LEN = MY_DELTA
            ALLOCATE ( STKDIAM( N )%ARRY( MY_DELTA ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'STKDIAM', PNAME )

            STKHT( N )%LEN = MY_DELTA
            ALLOCATE ( STKHT( N )%ARRY( MY_DELTA ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'STKHT', PNAME )

            STKTK( N )%LEN = MY_DELTA
            ALLOCATE ( STKTK( N )%ARRY( MY_DELTA ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'STKTK', PNAME )

            STKVEL( N )%LEN = MY_DELTA
            ALLOCATE ( STKVEL( N )%ARRY( MY_DELTA ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'STKVEL', PNAME )

            IF ( FIRE_ON( N ) ) THEN
               ACRES_BURNED( N )%LEN = MY_DELTA
               ALLOCATE ( ACRES_BURNED( N )%ARRY( MY_DELTA ), STAT = ASTAT )
               CALL CHECKMEM( ASTAT, 'ACRES_BURNED', PNAME )
            END IF

            MY_XLOCA( N )%LEN = MY_NSRC( N )
            ALLOCATE ( MY_XLOCA( N )%ARRY( MY_NSRC( N ) ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'MY_XLOCA', PNAME )
            MY_XLOCA( N )%ARRY = 0.0   ! array

            MY_YLOCA( N )%LEN = MY_NSRC( N )
            ALLOCATE ( MY_YLOCA( N )%ARRY( MY_NSRC( N ) ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'MY_YLOCA', PNAME )
            MY_YLOCA( N )%ARRY = 0.0   ! array

            IF ( MY_NSRC( N ) .GT. 0 ) THEN

               IF ( .NOT. XTRACT3( STKPRMS( N ), 'STKDM', 1,NLAYS,
     &                             MY_STRT_SRC( N ),MY_END_SRC( N ),
     &                             1,1, SDATE, STIME, STKDIAM( N )%ARRY ) ) THEN
                  XMSG = 'Could not read STKDM from ' // TRIM( STKPRMS( N ) )
                  CALL M3MESG( XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

               IF ( .NOT. XTRACT3( STKPRMS( N ), 'STKHT', 1,NLAYS,
     &                             MY_STRT_SRC( N ),MY_END_SRC( N ),
     &                             1,1, SDATE, STIME, STKHT( N )%ARRY ) ) THEN
                  XMSG = 'Could not read STKHT from ' // TRIM( STKPRMS( N ) )
                  CALL M3MESG( XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

               IF ( .NOT. XTRACT3( STKPRMS( N ), 'STKTK', 1,NLAYS,
     &                             MY_STRT_SRC( N ),MY_END_SRC( N ),
     &                             1,1, SDATE, STIME, STKTK( N )%ARRY ) ) THEN
                  XMSG = 'Could not read STKTK from ' // TRIM( STKPRMS( N ) )
                  CALL M3MESG( XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

               IF ( .NOT. XTRACT3( STKPRMS( N ), 'STKVE', 1,NLAYS,
     &                             MY_STRT_SRC( N ),MY_END_SRC( N ),
     &                             1,1, SDATE, STIME, STKVEL( N )%ARRY ) ) THEN
                  XMSG = 'Could not read STKVE from ' // TRIM( STKPRMS( N ) )
                  CALL M3MESG( XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

               IF ( FIRE_ON( N ) ) THEN
                  IF ( .NOT. XTRACT3( STKPRMS( N ), 'ACRESBURNED', 1,NLAYS,
     &                                MY_STRT_SRC( N ),MY_END_SRC( N ),
     &                                1,1, SDATE, STIME, ACRES_BURNED( N )%ARRY ) ) THEN
                     XMSG = 'Could not read ACRESBURNED from ' // TRIM( STKPRMS( N ) )
                     CALL M3MESG( XMSG )
                     SUCCESS = .FALSE.; RETURN
                  END IF
               END IF

            ELSE

               MY_DELTA = 0

               MY_XLOCA( N )%LEN = MY_NSRC( N )
               MY_YLOCA( N )%LEN = MY_NSRC( N )

!              IF ( FIRE_ON( N ) ) THEN
!                 ACRES_BURNED( MY_STRT_SRC( N ):MY_END_SRC( N ) ) = 0.0
!              END IF

            END IF   ! MY_NSRC( N ) .GT. 0
        
            DO S = 1, MY_NSRC( N )
               SRC = LOC_MAP( S )
               MY_XLOCA( N )%ARRY( S ) = XLOCA( SRC )
               MY_YLOCA( N )%ARRY( S ) = YLOCA( SRC )
            END DO

            DEALLOCATE ( XLOCA, YLOCA, STKID, LOC_MAP )

         END DO   ! NGRPS

         RETURN

         END FUNCTION STK_PRMS_INIT

      END MODULE STK_PRMS
