
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

      SUBROUTINE SA_ARRAY_INIT(CGRID, ISAM )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c20140428 If cold start, write ICON values to ISAM; otherwise,
c         write to ISAM those ISAM values from last hour of previous day
c           Called by initscen.F
c
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      USE GRID_CONF
      USE UTILIO_DEFN           ! replaces ioparms, iofdesc, iodecl
      USE CGRID_SPCS            ! covers all kinds of species mapping
      USE SA_DEFN               ! Mc06
      USE OZVOC_DEFN            ! 20120911 populates VOC emission species
      
      USE SUBST_MODULES         ! stenex

      implicit none
      
!      INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters

      INCLUDE SUBST_FILES_ID    ! file name parameters
!      INCLUDE SUBST_COORD_ID    ! coord. and domain definitions (req IOPARMS)
            

c.....arguments

      REAL, POINTER :: CGRID( :,:,:,: ) ! for initial CONC

      REAL, POINTER :: ISAM( :,:,:,:,: )


c.....local variables

      INTEGER          C, R, L, S, K              ! loop index
      INTEGER          N, NSPC, JVOC              ! 20140123

      CHARACTER*1      NEW_START

      INTERFACE
        SUBROUTINE CREATE_ICON( ISAM )
          IMPLICIT NONE
          REAL, POINTER :: ISAM( :,:,:,:,: )
        END SUBROUTINE CREATE_ICON
      END INTERFACE


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     program entry
c.........................................................
c.....update isam array according to isam ICON source tag

      CALL GETENV('ISAM_NEW_START', NEW_START)

Ckrt  print*,'Entered SA_ARRAY_INIT....'
Ckrt  print*,'NSPC_SA, NLAYS, MY_NROWS, MY_NCOLS, ICONTAG'
Ckrt  print*, NSPC_SA, NLAYS, MY_NROWS, MY_NCOLS, ICONTAG  
      IF( NEW_START == 'Y') THEN
                                !.....map N_SPC_ADV to the user-query species
        DO S = 1, NSPC_SA

          N = SPC_INDEX( S, 1 )
          IF ( YES_PRINT ) THEN
            if ( mype .eq. 0 ) then
              print*,'SPC_INDEX( S,1 ) = ',N,'SPC_INDEX( S,2 )=',SPC_INDEX( S,2 )
              if ( SPC_INDEX( S,2 ) .LE. GC_FINI ) then 
                print*,"cgrid specs =",GC_SPC(SPC_INDEX(S,2))
              elseif ( SPC_INDEX( S,2 ) .LE. AE_FINI ) then 
                print*,"cgrid specs =",AE_SPC(SPC_INDEX(S,2)-AE_STRT+1)
              elseif ( SPC_INDEX( S,2 ) .LE. NR_FINI ) then 
                print*,"cgrid specs =",NR_SPC(SPC_INDEX(S,2)-NR_STRT+1)
              endif ! position of spc_index( s,2 ) in cgrid species list
            endif ! mype0 ?
          ENDIF ! yes_print

          IF( N .EQ. 1) THEN  ! query is for an original species, includes O3A 20130627
            DO L = 1, NLAYS
              DO C = 1, MY_NCOLS
                DO R = 1, MY_NROWS
                  ISAM(C,R,L,S,ICONTAG) = CGRID(C,R,L,SPC_INDEX(S,2))
                END DO ! R
              END DO ! C
            END DO ! L
            JVOC = INDEX1( SPC_NAME( S, ICONTAG ), N_EVOC, NAM_VOC ) ! 14 voc spcs, 20140123
            IF ( JVOC .GT. 0 ) THEN
              WCH_VOC( JVOC, ICONTAG ) = 1.0
            ENDIF ! spc_name is voc ?
          ELSEIF( N .EQ. -50) THEN  ! 50/50 for each of VOC- and NOx limited ozone
            DO L = 1, NLAYS
              DO C = 1, MY_NCOLS
                DO R = 1, MY_NROWS
                  ISAM(C,R,L,S,ICONTAG) = 0.5 * CGRID(C,R,L,SPC_INDEX(S,2))
                END DO ! R
              END DO ! C
            END DO ! L
          ELSEIF(N .NE. -1) THEN ! query is for constructed species
            DO NSPC = 1, N
              DO L = 1, NLAYS
                DO C = 1, MY_NCOLS
                  DO R = 1, MY_NROWS
                    ISAM(C,R,L,S, ICONTAG) =
     &                  ISAM(C,R,L,S, ICONTAG)+ CGRID(C,R,L,SPC_INDEX(S, 1+NSPC))
                  END DO ! R
                END DO ! C
              END DO ! L
            END DO ! NSPC of constructed species
          END IF ! original species, or constructed species ?
        END DO ! loop of isam species NSPC_SA

      ELSE
        ! Initialize ISAM array by isam output from previous day
        IF ( YES_PRINT ) THEN
          if ( mype .eq. 0 )
     &      print*, "    Create Initial Conc from PREVDAY data"
        ENDIF ! yes_print
        CALL CREATE_ICON( ISAM )

      END IF

      return

      end

C=======================================================================

      SUBROUTINE CREATE_ICON( ISAM )
      
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

      IMPLICIT NONE 

      
C.....argument:

      REAL, POINTER :: ISAM( :,:,:,:,: )


c.....local variable
      
      CHARACTER( 16 )  PNAME, INNAME
      REAL, ALLOCATABLE:: TEMP( :,:,: )      
      INTEGER          JDATE   ! current model date, coded YYYYDDD
      INTEGER          JTIME   ! midpoint model time, coded HHMMSS

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER      GXOFF, GYOFF              ! global origin offset from file
      INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW

      INTEGER          LOGENV
      INTEGER          C,R,L,S,K,I

Ckrt...External
!0710 INTEGER, EXTERNAL :: INDEX1     

Ckrt...others
      INTEGER IBGN, JSPCTAG, NSPCTAG
      CHARACTER( 16 ) SPCNAME1( NSPC_SA ) ! KRT: one dimensional array of specie names
 
czsw_hkust
Ckrt  LOGICAL          PSHUT3
czsw_hkust

      DATA INNAME  /  'ISAM_PREVDAY    ' /   !need to be defined
      DATA PNAME   /  'create_icon'/     

C**********************************************************************
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGENV = INIT3 ()
C Get domain decomp info from the emissions file
         CALL SUBHFILE ( INNAME, GXOFF, GYOFF,
     &                   STARTCOL, ENDCOL, STARTROW, ENDROW )
      END IF                    ! FIRSTIME
      
      IF ( .NOT. OPEN3( INNAME, FSREAD3, PNAME) )THEN 
         WRITE(*, *) 'Error: open file failed', INNAME
      ENDIF 
      
      IF ( .NOT. DESC3( INNAME ) ) THEN
         WRITE(*, *) 'Error: read', INNAME, 'failed'
         STOP 'Bad exit'
      ENDIF 

cyqwang...begin  Get The last step data

      DO I = 1, MXREC3D-1
        CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D )
      END DO

cyqwang...end
      
      JDATE = SDATE3D
      JTIME = STIME3D
      NSPCTAG = NVARS3D

Ckrt....assign spc_name( nspc_sa, ntag_sa ) to spcname1( nspc_sa )
      DO S = 1, NSPC_SA
        SPCNAME1( S ) = SPC_NAME( S, NTAG_SA )
      ENDDO ! s

Ckrt allocate data-loading array 
      IF ( .NOT. ALLOCATED( TEMP ) ) 
     &  ALLOCATE( TEMP( MY_NCOLS, MY_NROWS, NLAYS3D ) )
      TEMP = 0.0
      
      DO JSPCTAG = 1, NSPCTAG
        !KRT: extract sa_conc_1 from previous day
        IF ( .NOT. XTRACT3( INNAME, VNAME3D( JSPCTAG ), 
     &       1, NLAYS3D, STARTROW, ENDROW, STARTCOL, ENDCOL,
     &       JDATE, JTIME, TEMP ) ) THEN
          Print*,  PNAME, JDATE, JTIME,
     &       'Read failure:  file ' // INNAME //
     &       ' variable ' // VNAME3D( JSPCTAG ) 
          stop ' end of file...'
        END IF
        !KRT: identify isam indices from input species names
        IBGN = INDEX( VNAME3D( JSPCTAG ),'_', .FALSE. )
        S = INDEX1( VNAME3D( JSPCTAG )( 1:IBGN-1 ),NSPC_SA,SPCNAME1 )
        K = INDEX1( VNAME3D( JSPCTAG )( IBGN+1:LEN_TRIM(VNAME3D(JSPCTAG))),
     &        NTAG_SA, TAGNAME )
Ckrt    print*,'In SA_ARRAY_INIT, ',VNAME3D( JSPCTAG ), SPCNAME1( S ),TAGNAME( K )
Ckrt    print*,'In SA_ARRAY_INIT, S =',S,' K =',K
        IF ( S .GT. 0 ) THEN
          IF ( K .GT. 0 ) THEN
            DO C = 1, MY_NCOLS
              DO R = 1, MY_NROWS
                DO L = 1, NLAYS3D
                  ISAM( C,R,L,S,K ) = TEMP( C,R,L )
                END DO
              END DO
            END DO
          ELSE ! bcon, othr, or icon tags
            CK_SUFFX: SELECT CASE ( VNAME3D(JSPCTAG)(IBGN+1:LEN_TRIM(VNAME3D(JSPCTAG))) )
            CASE ( 'BCON' )
              K = NTAG_SA - 2
            CASE ( 'OTHR' )
              K = NTAG_SA - 1
            CASE ( 'ICON' )
              K = NTAG_SA
            END SELECT CK_SUFFX
            DO C = 1, MY_NCOLS
              DO R = 1, MY_NROWS
                DO L = 1, NLAYS3D
                  ISAM( C,R,L,S,K ) = TEMP( C,R,L )
                END DO
              END DO
            END DO
          ENDIF ! emission tags
        ENDIF ! species exist in tags
      END DO ! jspctag

      IF ( ALLOCATED( TEMP ) ) DEALLOCATE(TEMP)

      If ( .NOT. CLOSE3( INNAME ) ) Then
         WRITE ( *,* )
     &     '>>--> ISAM PREVDAY output not closed properly <--<<'
      End if
      RETURN
           
      END
     
