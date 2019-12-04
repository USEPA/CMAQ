
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
      MODULE STK_EMIS

C-----------------------------------------------------------------------
C Function: stack emissions by source from stack emissions file and
C           3d point source emissions

C Revision History:
C     16 Jan 2007 J.Young: initial implementation
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C     30 Apr 2016 J.Young: add multiple fire source capability, in affiliation with
C                          Yongtao Hu (Georgia Tech)
C-----------------------------------------------------------------------

      USE UDTYPES, ONLY: RARRY1, CARRY1
      USE EMIS_VARS

      IMPLICIT NONE

      INTEGER,        ALLOCATABLE, SAVE :: NSRC_EMIS( : )   ! no. of pt sources
      INTEGER,        ALLOCATABLE, SAVE :: NVARS_EMIS( : )  ! no. of species
      INTEGER, PARAMETER                :: NVARS_FIRE = 1   ! default

      LOGICAL,        ALLOCATABLE, SAVE :: FIREFLAG( : )    ! which is the fire file?

      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: STKEMIS( : )   ! stack emissions [mol/s]
      TYPE( CARRY1 ), ALLOCATABLE, SAVE :: STKSPC( : )    ! emitted species names
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: FIREVAR( : )   ! fire variables, just H-Flux

      INTEGER FGRPS

      CONTAINS

C-----------------------------------------------------------------------

         FUNCTION STK_EMIS_INIT ( STKENAME, JDATE, JTIME ) RESULT ( SUCCESS )

         USE UTILIO_DEFN
         USE AERO_DATA, ONLY : MGPG, GPKG

         IMPLICIT NONE

         CHARACTER( 16 ), INTENT( IN ) :: STKENAME( : )  ! stack groups emis file names
         INTEGER, INTENT( IN )         :: JDATE, JTIME
         LOGICAL SUCCESS

         INTEGER I, J, N, NGRPS, LEN1, LEN2, ISRM, X

         INTEGER ASTAT
         CHARACTER( 96 ) :: XMSG = ' '
         CHARACTER( 16 ) :: PNAME = 'STK_EMIS_INIT'
         CHARACTER( 16 ) :: UNITSCK

         SUCCESS = .TRUE.

         NGRPS = SIZE( STKENAME )
         FGRPS = NGRPS

         ALLOCATE ( NSRC_EMIS( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'NSRC_EMIS', PNAME )

         ALLOCATE ( NVARS_EMIS( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'NVARS_EMIS', PNAME )

         ALLOCATE ( STKEMIS( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'STKEMIS', PNAME )

         ALLOCATE ( STKSPC( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'STKSPC', PNAME )

         ALLOCATE ( FIREFLAG( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'FIREFLAG', PNAME )
         FIREFLAG = .FALSE.   ! array

         ALLOCATE ( FIREVAR( FGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'FIREVAR', PNAME )

         DO N = 1, NGRPS
            ISRM = MAP_PTtoISRM( N )

            IF ( .NOT. OPEN3( STKENAME( N ), FSREAD3, PNAME ) ) THEN
               XMSG = 'Could not open '// TRIM( STKENAME( N ) ) // ' file'
               CALL M3MESG( XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            IF ( .NOT. DESC3( STKENAME( N ) ) ) THEN
               XMSG = 'Could not get ' // TRIM( STKENAME( N ) ) // ' file description'
               CALL M3MESG( XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            ! Each file will use the default jdate, unless specified by the user 
            ! that the file is a representative day file
            EM_FILE_DATE( ISRM ) = JDATE    
            IF ( EM_FILE_SYM_DATE( ISRM ) ) EM_FILE_DATE( ISRM ) = SDATE3D

            NSRC_EMIS( N ) = NROWS3D
            NVARS_EMIS( N ) = NVARS3D
            LEN1 = NSRC_EMIS( N )
            LEN2 = NVARS_EMIS( N )
            DO I = 1, LEN2
               ! Use HFLUX to determine Fire sources in PT3D_DEFN via
               ! the FIRENAM variable
               IF ( VNAME3D( I ) .EQ. 'HFLUX' ) THEN
                   FIREFLAG( N ) = .TRUE.
                   EM_FILE_FIRE( ISRM ) = .TRUE.
               END IF
            END DO
            IF ( FIREFLAG( N ) ) THEN   ! subtract off 'HFLUX'
               LEN2 = LEN2 - NVARS_FIRE
               NVARS_EMIS( N ) = LEN2
            END IF

            STKEMIS( N )%LEN = LEN1
            ALLOCATE ( STKEMIS( N )%ARRY( LEN1 ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'STKEMIS', PNAME )
            STKEMIS( N )%ARRY = 0.0   ! array

            EM_FILE_SURR( ISRM )%LEN = LEN2
            ALLOCATE ( EM_FILE_SURR( ISRM )%ARRY( LEN2 ), STAT = ASTAT )
            ALLOCATE ( EM_FILE_SURR( ISRM )%UNITS( LEN2 ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'EM_FILE_SURR', PNAME )

            IF ( FIREFLAG( N ) ) THEN   ! subtract and save off 'HFLUX'
               ALLOCATE ( FIREVAR( N )%ARRY( LEN1 ), STAT = ASTAT )
               CALL CHECKMEM( ASTAT, 'FIREVAR', PNAME )
               I = 0
               DO J = 1, NVARS3D
                  IF ( VNAME3D( J ) .NE. 'HFLUX' ) THEN
                     I = I + 1
                     EM_FILE_SURR( ISRM )%ARRY( I ) = VNAME3D( J )
                     EM_FILE_SURR( ISRM )%UNITS( I )= UNITS3D( J )
                  END IF
               END DO
               FIREVAR( N )%LEN = LEN1
            ELSE
               DO J = 1, EM_FILE_SURR( ISRM )%LEN
                  EM_FILE_SURR( ISRM )%ARRY( J ) = VNAME3D( J )
                  EM_FILE_SURR( ISRM )%UNITS( J )= UNITS3D( J )
               END DO
            END IF

         END DO

         RETURN

         END FUNCTION STK_EMIS_INIT

      END MODULE STK_EMIS
