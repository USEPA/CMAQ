
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

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GAS_DEPV_MAP ( N_DEPV_NAMES, DEPV_NAMES, DEPV_SUR )

C----------------------------------------------------------------------
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C----------------------------------------------------------------------

      USE CGRID_SPCS            ! CGRID mechanism species
      USE DEPVVARS
      USE UTILIO_DEFN
#ifdef mpas
      USE util_module, only : index1
#endif

      IMPLICIT NONE

C Arguments:

      INTEGER,         INTENT( IN )    :: N_DEPV_NAMES  ! number of unique dep vel species
      CHARACTER( 16 ), INTENT( INOUT ) :: DEPV_NAMES( : ) ! names of dep vel species
      INTEGER,         INTENT( OUT )   :: DEPV_SUR( : ) ! pointers to model species for
                                                        ! calculated deposition velocities
                                                        ! (surrogate dep vels [m/s**2])

C Local variables:

      INTEGER :: JDATE = 0
      INTEGER :: JTIME = 0

      CHARACTER( 16 ), SAVE :: PNAME = 'GAS_DEPV_MAP'

      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 16 ) :: VNAME            ! scratch string

      INTEGER ALLOCSTAT
      INTEGER V, N
      INTEGER STRT, FINI

C----------------------------------------------------------------------

      IF ( ( N_GC_SPC + N_NR_SPC + N_TR_SPC ) .LE. 0 ) THEN
         XMSG = 'Model not compiled for gases'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C Set up names and indices

      IF ( .NOT. DEPVVARS_INIT( ) ) THEN
         XMSG = 'Failure initializing depvvars module'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C Set up the DEPV pointers to calculated deposition velocities

#ifdef Verbose
      WRITE( LOGDEV,* ) ' '
      WRITE( LOGDEV,* ) ' > Unique DEPV versus DEPSPC'
#endif

      DO V = 1, N_DEPV_NAMES
         VNAME = DEPV_NAMES( V )( 4:LEN_TRIM( DEPV_NAMES( V ) ) )
         N = INDEX1( VNAME, LTOTG, DEPSPC )
         IF ( N .NE. 0 ) THEN
            IF ( .NOT. USE_DEPSPC( N ) ) THEN
               USE_DEPSPC( N ) = .TRUE.
               N_USE_DEPSPC = N_USE_DEPSPC + 1
#ifdef Verbose
               WRITE( LOGDEV,1005 ) V, DEPV_NAMES( V ), N, DEPSPC( N )
#endif
            END IF    
         ELSE
            XMSG = 'Could not find ' // TRIM( DEPV_NAMES( V ) ) // 
     &             ' in '            // TRIM( DEPV_METHOD )     //
     &             ' surrogate table.'
!    &             ' surrogate table. >>> Dep vel set to zero <<< '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END DO

C Sort DEPV_NAMES based on the order in DEPSPC (DEPVVARS module)

#ifdef Verbose
      WRITE( LOGDEV,* ) ' '
      WRITE( LOGDEV,* ) ' > Sorted DEPV names (1st index from total m3dry list)'
#endif

      N = 0
      DO V = 1, LTOTG            
!        IF ( .NOT. USE_DEPSPC( V ) ) CYCLE
         IF ( .NOT. USE_DEPSPC( V ) ) THEN
#ifdef Verbose
            WRITE( LOGDEV,1009 ) V, USE_DEPSPC( v ), 0, ' '
#endif
         ELSE
            N = N + 1
            VNAME = 'VD_' // DEPSPC( V )( 1:LEN_TRIM( DEPSPC( V ) ) )
            DEPV_NAMES( N ) = VNAME
#ifdef Verbose
            WRITE( LOGDEV,1009 ) V, USE_DEPSPC( V ), N, DEPV_NAMES( N )
#endif
         END IF
      END DO

C Locate model species' DEPVs in the unique DEPV_NAMES list

      DO V = 1, N_GC_DEPV
         VNAME = GC_DEPV( V )
         N = INDEX1( VNAME, N_DEPV_NAMES, DEPV_NAMES )
         IF ( N .NE. 0 ) THEN
            DEPV_SUR( V ) = N
         ELSE
            XMSG = 'Could not find ' // TRIM( GC_DEPV( V ) )
     &           // ' in DEPV table.'  
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END DO

      STRT = N_GC_DEPV + 1
      FINI = N_GC_DEPV + N_NR_DEPV 
      DO V = STRT, FINI
         VNAME = NR_DEPV( V - ( STRT-1 ) )
         N = INDEX1( VNAME, N_DEPV_NAMES, DEPV_NAMES )
         IF ( N .NE. 0 ) THEN
            DEPV_SUR( V ) = N
         ELSE
            XMSG = 'Could not find ' // TRIM( NR_DEPV( V ) )
     &           // ' in DEPV table.'  
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END DO

      STRT = N_GC_DEPV + N_NR_DEPV + 1
      FINI = N_GC_DEPV + N_NR_DEPV + N_TR_DEPV
      DO V = STRT, FINI
         VNAME = TR_DEPV( V - (STRT-1) )
         N = INDEX1( VNAME, N_DEPV_NAMES, DEPV_NAMES )
         IF ( N .NE. 0 ) THEN
            DEPV_SUR( V ) = N
         ELSE
            XMSG = 'Could not find ' // TRIM( TR_DEPV( V ) )
     &           // ' in DEPV table.'  
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END DO

#ifdef Verbose
      WRITE( LOGDEV,* ) ' '
      WRITE( LOGDEV,* ) ' > ', PNAME, 'Final Gas DEPV Map'

      STRT = 0
      DO V = 1, N_GC_DEPV
         WRITE( LOGDEV,1019 ) V, GC_DEPV( V ), DEPV_SUR( V+STRT ),
     &                        DEPV_NAMES( DEPV_SUR( V+STRT) )
      END DO

      STRT = N_GC_DEPV
      DO V = 1, N_NR_DEPV
         WRITE( LOGDEV,1019 ) V, NR_DEPV( V ), DEPV_SUR( V+STRT ),
     &                        DEPV_NAMES( DEPV_SUR( V+STRT) )
      END DO

      STRT = N_GC_DEPV + N_NR_DEPV 
      DO V = 1, N_TR_DEPV
         WRITE( LOGDEV,1019 ) V, TR_DEPV( V ), DEPV_SUR( V+STRT ),
     &                        DEPV_NAMES( DEPV_SUR( V+STRT) )
      END DO
#endif

      RETURN
 
      RETURN

1005  FORMAT( I5, 1X, A16, I3, 1X, A16 )

!009  FORMAT( I5, 1X, I4, 1X, A16 )
1009  FORMAT( I5, 1X, L3, 1X, I4, 1X, A16 )

1019  FORMAT( 3( I3, 1X, A16, 1X ) )

      END SUBROUTINE GAS_DEPV_MAP

