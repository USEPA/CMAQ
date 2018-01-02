
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/STK_EMIS.F,v 1.4 2012/01/19 15:25:35 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

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
C     22 Jun 2016 B.Murphy & J.Young: Added STKEMIS_TOO to accomodate
C                                     semivolatile POA mechanism
C-----------------------------------------------------------------------

      USE UDTYPES, ONLY: RARRY1, CARRY1
      USE EMIS_VARS

      IMPLICIT NONE

      INTEGER,        ALLOCATABLE, SAVE :: NSRC_EMIS( : )   ! no. of pt sources
      INTEGER,        ALLOCATABLE, SAVE :: NVARS_EMIS( : )  ! no. of species
      INTEGER, PARAMETER                :: NVARS_FIRE = 1   ! default

      LOGICAL,        ALLOCATABLE, SAVE :: FIREFLAG( : )    ! which is the fire file?

      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: STKEMIS( : )   ! stack emissions [mol/s]
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: STKEMIS_TOO( : )   ! stack emissions [mol/s]
      TYPE( CARRY1 ), ALLOCATABLE, SAVE :: STKSPC( : )    ! emitted species names
      TYPE( RARRY1 ), ALLOCATABLE, SAVE :: FIREVAR( : )   ! fire variables, just H-Flux

      INTEGER FGRPS

      INTEGER,        ALLOCATABLE, SAVE :: STKDATE( : )   ! src grp timestamp date
      INTEGER,        ALLOCATABLE, SAVE :: SYNCOFF( : )   ! src grp timestamp date offset

      CONTAINS

         FUNCTION STK_EMIS_INIT ( STKENAME, JDATE, JTIME ) RESULT ( SUCCESS )

         USE UTILIO_DEFN

         IMPLICIT NONE

         CHARACTER( 16 ), INTENT( IN ) :: STKENAME( : )  ! stack groups emis file names
         INTEGER, INTENT( IN )         :: JDATE, JTIME
         LOGICAL SUCCESS

         INTEGER I, J, N, NGRPS, LEN1, LEN2

         INTEGER ASTAT
         CHARACTER( 96 ) :: XMSG = ' '
         CHARACTER( 16 ) :: PNAME = 'STK_EMIS_INIT'

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         NGRPS = SIZE( STKENAME )
         FGRPS = NGRPS

         ALLOCATE ( NSRC_EMIS( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'NSRC_EMIS', PNAME )

         ALLOCATE ( NVARS_EMIS( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'NVARS_EMIS', PNAME )

         ALLOCATE ( STKDATE( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'STKDATE', PNAME )

         ALLOCATE ( SYNCOFF( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'SYNCOFF', PNAME )

         ALLOCATE ( STKEMIS( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'STKEMIS', PNAME )
         ALLOCATE ( STKEMIS_TOO( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'STKEMIS_TOO', PNAME )

         ALLOCATE ( STKSPC( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'STKSPC', PNAME )

         ALLOCATE ( FIREFLAG( NGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'FIREFLAG', PNAME )
         FIREFLAG = .FALSE.   ! array

         ALLOCATE ( FIREVAR( FGRPS ), STAT = ASTAT )
         CALL CHECKMEM( ASTAT, 'FIREVAR', PNAME )

         DO N = 1, NGRPS

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

            STKDATE( N ) = SDATE3D
            SYNCOFF( N ) = SECSDIFF( JDATE,JTIME, STKDATE( N ),JTIME )
            NSRC_EMIS( N ) = NROWS3D
            NVARS_EMIS( N ) = NVARS3D
            LEN1 = NSRC_EMIS( N )
            LEN2 = NVARS_EMIS( N )
            DO I = 1, LEN2
               ! Use HFLUX to determine Fire sources in PT3D_DEFN via
               ! the FIRENAM variable
               IF ( VNAME3D( I ) .EQ. 'HFLUX' ) FIREFLAG( N ) = .TRUE.
            END DO
            IF ( FIREFLAG( N ) ) THEN   ! subtract off 'HFLUX'
               LEN2 = LEN2 - NVARS_FIRE
               NVARS_EMIS( N ) = LEN2
            END IF

            STKEMIS( N )%LEN = LEN1
            STKEMIS_TOO( N )%LEN = LEN1
            ALLOCATE ( STKEMIS( N )%ARRY( LEN1 ), STAT = ASTAT )
            ALLOCATE ( STKEMIS_TOO( N )%ARRY( LEN1 ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'STKEMIS', PNAME )
            STKEMIS( N )%ARRY = 0.0   ! array

            EM_FILE_SURR( MAP_PTtoISRC(N) )%LEN = LEN2
            ALLOCATE ( EM_FILE_SURR( MAP_PTtoISRC(N) )%ARRY( LEN2 ), STAT = ASTAT )
            CALL CHECKMEM( ASTAT, 'EM_FILE_SURR', PNAME )

            IF ( FIREFLAG( N ) ) THEN   ! subtract and save off 'HFLUX'
               ALLOCATE ( FIREVAR( N )%ARRY( LEN1 ), STAT = ASTAT )
               CALL CHECKMEM( ASTAT, 'FIREVAR', PNAME )
               I = 0
               DO J = 1, NVARS3D
                  IF ( VNAME3D( J ) .NE. 'HFLUX' ) THEN
                     I = I + 1
                     EM_FILE_SURR( MAP_PTtoISRC(N) )%ARRY( I ) = VNAME3D( J )
                  END IF
               END DO
               FIREVAR( N )%LEN = LEN1
            ELSE
               DO J = 1, EM_FILE_SURR( MAP_PTtoISRC(N) )%LEN
                  EM_FILE_SURR( MAP_PTtoISRC(N) )%ARRY( J ) = VNAME3D( J )
               END DO
            END IF

         END DO

         RETURN

         END FUNCTION STK_EMIS_INIT

      END MODULE STK_EMIS
