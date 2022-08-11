
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

      SUBROUTINE SA_ARRAY_INIT(CGRID )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c20140428 If cold start, write ICON values to ISAM; otherwise,
c         write to ISAM those ISAM values from last hour of previous day
c           Called by initscen.F
c
C     01 Nov 2018: S.Napelenok Updates for cmaq5.3 release 
C     09 May 2019: D.Wong Removed all MY_ clauses
C     13 May 2019: D.Wong Moved environment variable ISAM_NEW_START to RUNTIME_VARS
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      USE GRID_CONF
      USE UTILIO_DEFN           ! replaces ioparms, iofdesc, iodecl
      USE CGRID_SPCS            ! covers all kinds of species mapping
      USE SA_DEFN               ! Mc06
      USE RUNTIME_VARS, only : ISAM_NEW_START
      
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif


      implicit none
      
      INCLUDE SUBST_FILES_ID    ! file name parameters

c.....arguments

      REAL, POINTER :: CGRID( :,:,:,: ) ! for initial CONC

c.....local variables

      INTEGER          C, R, L, S, K              ! loop index
      INTEGER          N, NSPC, JVOC              ! 20140123

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     program entry
c.........................................................
c.....update isam array according to isam ICON source tag


      IF( ISAM_NEW_START == 'Y') THEN
                                !.....map N_SPC_ADV to the user-query species
        DO S = 1, NSPC_SA

          N = MAP_SAtoCGR( S )
          DO L = 1, NLAYS
            DO R = 1, NROWS
              DO C = 1, NCOLS
                ISAM(C,R,L,S,ICONTAG) = CGRID(C,R,L,N)
              END DO ! R
            END DO ! C
          END DO ! L
        END DO ! S
      ELSE
        ! Initialize ISAM array by isam output from previous day
        CALL CREATE_ICON( )

      END IF

      return

      end

C=======================================================================

      SUBROUTINE CREATE_ICON( )
      
C**********************************************************************
C
C  FUNCTION: [1] read in a ISAM IOAPI output file
C            [2] Load the last step's ISAM data into ISAM buffer array 
C
C
C**********************************************************************

      USE GRID_CONF
      USE UTILIO_DEFN   ! replaces ioparms, iofdesc, iodecl
      USE SA_DEFN   ! Mc06
      USE RUNTIME_VARS
      
      IMPLICIT NONE 

      
C.....argument:

c.....local variables:
      
      CHARACTER( 16 ), PARAMETER :: PNAME   = 'create_icon'
      CHARACTER( 16 ), PARAMETER :: INNAME  = 'ISAM_PREVDAY'
      
      
      INTEGER       ::  JDATE   ! current model date, coded YYYYDDD
      INTEGER       ::   JTIME   ! midpoint model time, coded HHMMSS

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER       :: GXOFF, GYOFF              ! global origin offset from file
      INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW

      INTEGER       ::   LOGENV
      INTEGER       ::   C,R,L,S,K,I

      INTEGER       :: IBGN, JSPCTAG, NSPCTAG
      
      CHARACTER( 16 )    :: SPCNAME1( NSPC_SA ) 
      CHARACTER( 16 )    :: NAME_TAG
      CHARACTER( 99 )    :: XMSG
      
      LOGICAL            :: EFLAG = .FALSE.
      
      REAL, ALLOCATABLE  :: TEMP( :,:,: )      

C**********************************************************************
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGENV = INIT3()
C Get domain decomp info from the emissions file
         CALL SUBHFILE ( INNAME, GXOFF, GYOFF,
     &                   STARTCOL, ENDCOL, STARTROW, ENDROW )
      END IF                    ! FIRSTIME
      
      EFLAG = .FALSE.
      
      IF ( .NOT. OPEN3( INNAME, FSREAD3, PNAME) )THEN 
         WRITE(LOGDEV, *) 'Error: open file failed', INNAME
         CALL M3EXIT( 'READ_ISAM_ICON', 0, 0, 'Above Fatal Error Encountered', XSTAT2 )
      ENDIF 
      
      
      IF ( .NOT. DESC3( INNAME ) ) THEN
         WRITE(LOGDEV, *) 'Error: read', INNAME, 'failed'
         CALL M3EXIT( 'READ_ISAM_ICON', 0, 0, 'Above Fatal Error Encountered', XSTAT2 )
      ENDIF 

cyqwang...begin  Get The last step data

      DO I = 1, MXREC3D-1
        CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D )
      END DO

cyqwang...end
      
      JDATE = SDATE3D
      JTIME = STIME3D
      NSPCTAG = NVARS3D

Ckrt....assign ISAM_SPEC( nspc_sa, ntag_sa ) to spcname1( nspc_sa )
      DO S = 1, NSPC_SA
        SPCNAME1( S ) = ISAM_SPEC( S, NTAG_SA )
      ENDDO ! s

Ckrt allocate data-loading array 
      ALLOCATE( TEMP( NCOLS, NROWS, NLAYS3D ) )
      TEMP = 0.0
      
      DO JSPCTAG = 1, NSPCTAG
        !KRT: extract sa_conc_1 from previous day
        IF ( .NOT. XTRACT3( INNAME, VNAME3D( JSPCTAG ), 
     &       1, NLAYS3D, STARTROW, ENDROW, STARTCOL, ENDCOL,
     &       JDATE, JTIME, TEMP ) ) THEN
               XMSG = 'Read failure:  file ' // INNAME // 
     &         ' variable ' // VNAME3D( JSPCTAG ) //
     &         ' end of file...'
                WRITE(LOGDEV,*) PNAME, JDATE, JTIME,
     &          TRIM( XMSG )
             CALL M3EXIT( 'READ_ISAM_ICON', 0, 0, 'Above Fatal Error Encountered', XSTAT2 )             
        END IF
        !KRT: identify isam indices from input species names
        IBGN = INDEX( VNAME3D( JSPCTAG ),'_', BACK = .TRUE. )
        S = INDEX1( VNAME3D( JSPCTAG )( 1:IBGN-1 ),NSPC_SA,SPCNAME1 )
        K = INDEX1( VNAME3D( JSPCTAG )( IBGN+1:LEN_TRIM(VNAME3D(JSPCTAG))),
     &        NTAG_SA, TAGNAME )
        NAME_TAG = VNAME3D(JSPCTAG)(IBGN+1:LEN_TRIM(VNAME3D(JSPCTAG)))
        IF ( S .GT. 0 ) THEN
          IF ( K .GT. 0 ) THEN
            DO C = 1, NCOLS
              DO R = 1, NROWS
                DO L = 1, NLAYS3D
                  ISAM( C,R,L,S,K ) = TEMP( C,R,L )
                END DO
              END DO
            END DO
          ELSE ! bcon, othr, or icon tags            
            CK_SUFFX: SELECT CASE ( NAME_TAG )
            CASE ( 'BCON', 'BCO' )
              K = NTAG_SA - 2
            CASE ( 'OTHR', 'OTH' )
              K = NTAG_SA - 1
            CASE ( 'ICON', 'ICO' )
              K = NTAG_SA
            END SELECT CK_SUFFX
            IF ( K .LE. 0 ) THEN
              WRITE(LOGDEV,*)'ISAM ICON Species ', TRIM( VNAME3D(JSPCTAG) ), ' has bad tagname set to ',
     &        TRIM( NAME_TAG )            
              EFLAG = .TRUE.      
            ELSE
               DO L = 1, NLAYS3D
                  DO R = 1, NROWS
                     DO C = 1, NCOLS
                        ISAM( C,R,L,S,K ) = TEMP( C,R,L )
                     END DO
                  END DO
               END DO
            END IF
          ENDIF ! emission tags
        ENDIF ! species exist in tags
      END DO ! jspctag

      IF ( EFLAG ) THEN
         CALL M3EXIT( 'READ_ISAM_ICON', 0, 0, 'Above Fatal Error Encountered', XSTAT2 )
      END IF
      
      DEALLOCATE(TEMP)

      If ( .NOT. CLOSE3( INNAME ) ) Then
         WRITE ( *,* )
     &     '>>--> ISAM PREVDAY output not closed properly <--<<'
      End if
      RETURN
           
      END
     
