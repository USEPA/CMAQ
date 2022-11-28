
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

      SUBROUTINE OP_SA ( JDATE, JTIME, TSTEP, NSTEPS, FILE_NAME )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c20140428  Opens ISAM's SA_CONC_1 file
c       op_sa.F called by initscen.F
c
C     01 Nov 2018: S.Napelenok Updates for cmaq5.3 release 
C     09 May 2019: D.Wong Removed all MY_ clauses
c
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


!      USE HGRD_DEFN             ! horizontal domain specifications
      USE GRID_CONF
c     USE SA_LAYERS
      USE SA_DEFN               ! Mc06
      USE UTILIO_DEFN           ! replaces ioparms, iofdesc, iodecl
c     USE SUBST_MODULES         ! stenex
      USE RXNS_DATA, ONLY : MECHNAME !Get Chemical Mechanism Name
      USE RUNTIME_VARS

#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! I/O definitions and declarations

C...Arguments Declaration
      INTEGER    JDATE          ! current model date, coded YYYYDDD
      INTEGER    JTIME          ! current model time, coded HHMMSS
      INTEGER    TSTEP(3)       ! format 10000, 0, 0
cyqw
      INTEGER    NSTEPS         ! Number of time steps of 3D output
cyqw
      CHARACTER( 16 )       :: FILE_NAME

   
C...End of argument declaration

C Local variables:
      
c     INTEGER          ALLOCSTAT, LOGDEV
      INTEGER          ALLOCSTAT

      CHARACTER( 16 ), DIMENSION( :,: ), ALLOCATABLE::VNAME
      CHARACTER( 1 ),  DIMENSION(:), ALLOCATABLE::SPECIE_NAME
      CHARACTER( 16 ) :: TEMP, CSUFFX
      INTEGER          LENGTH

      CHARACTER( 16 ) :: PNAME = 'OP_SA'
      CHARACTER( 96 ) :: XMSG = ' '
      
      INTEGER   C,R,S,K,N, L    ! loop induction variables
c     INTEGER   ITAG, JSPC, JSPCTAG
      INTEGER   JSPC, JSPCTAG


      REAL, ALLOCATABLE :: ISAM_BUFF ( :,:,:,: )

Ckrt Diagnostic
      CHARACTER( 256 ) EQNAME

C-----------------------------------------------------------------------



Ckrt Create variable names for tag output


        LOGDEV = INIT3()

        IF ( ISAM_ELEV .EQ. -1 ) ISAM_ELEV = NLAYS
        SA_NLAYS = ISAM_ELEV - ISAM_BLEV + 1

        ALLOCATE( VNAME(NSPC_SA,NTAG_SA) )
        VNAME = ''
        JSPCTAG = 0
        DO ITAG = 1, NTAG_SA
          DO JSPC = 1, NSPC_SA
            JSPCTAG = JSPCTAG + 1
            LENGTH = LEN_TRIM( ISAM_SPEC( JSPC,ITAG ) )
            IF ( LENGTH .GT. 0 ) THEN
              TEMP = ISAM_SPEC( JSPC,ITAG )( 1:LENGTH )
              ALLOCATE ( SPECIE_NAME( LENGTH ) )
              DO N = 1, LENGTH
                SPECIE_NAME(N:N) = TEMP(N:N)
              ENDDO
              IF ( ITAG .EQ. BCONTAG ) THEN
                CSUFFX = 'BCO             '
                CALL WR_VARNAME( SPECIE_NAME, LENGTH, NTAG_SA, NSPC_SA,
     &             CSUFFX, VNAME( JSPC,ITAG ) )
              ELSEIF ( ITAG .EQ. OTHRTAG ) THEN
                CSUFFX = 'OTH             '
                CALL WR_VARNAME( SPECIE_NAME, LENGTH, NTAG_SA, NSPC_SA,
     &             CSUFFX, VNAME( JSPC,ITAG ) )
              ELSEIF ( ITAG .EQ. ICONTAG ) THEN
                CSUFFX = 'ICO             '
                CALL WR_VARNAME( SPECIE_NAME, LENGTH, NTAG_SA, NSPC_SA,
     &             CSUFFX, VNAME( JSPC,ITAG ) )
              ELSE
                CALL WR_VARNAME( SPECIE_NAME, LENGTH, NTAG_SA, NSPC_SA,
     &             TAGNAME(ITAG), VNAME( JSPC,ITAG ) )
              ENDIF ! Is itag bcon, othr, icon, or emis ?
!              WRITE(LOGDEV,'(I3,2(1X,I2),2(1X,A16))')JSPCTAG,JSPC,ITAG,ISAM_SPEC( JSPC,ITAG ),VNAME( JSPC,ITAG )
              DEALLOCATE ( SPECIE_NAME )
            ENDIF ! length > 0?
          ENDDO ! jspc
        ENDDO ! itag

Ckrt Set nvars3d for SA_CONC_1 or SA_CGRID_1 file
        NVARS3D = N_SPCTAG

Ckrt Set vname3d for SA_CONC_1 file
        JSPCTAG = 0
!       write (logdev, *) NSPC_SA, NTAG_SA

        DO ITAG = 1, NTAG_SA
           DO JSPC = 1, NSPC_SA
              JSPCTAG = JSPCTAG + 1
              VTYPE3D( JSPCTAG ) = M3REAL
              VNAME3D( JSPCTAG ) = VNAME(JSPC,ITAG)
              IF( IS_SPC_AEROSOL( JSPC,ITAG ) )THEN 
                UNITS3D( JSPCTAG ) = "micrograms/m**3 "
              ELSE 
                UNITS3D( JSPCTAG ) = "ppmV            "
              END IF
              VDESC3D( JSPCTAG ) = "tracer conc."
              S_SPCTAG  ( JSPCTAG ) = JSPC
              T_SPCTAG  ( JSPCTAG ) = ITAG
              VNAM_SPCTAG ( JSPCTAG ) = VNAME(JSPC,ITAG)
!              WRITE(LOGDEV,'(I4,2(1X,I3),2(1X,A16),2(1X,I3))')
!     &        JSPCTAG,JSPC,ITAG,  
!     &        VNAME3D( JSPCTAG ), UNITS3D( JSPCTAG ),
!     &        S_SPCTAG( JSPCTAG ), T_SPCTAG( JSPCTAG )
           END DO
        END DO

        SDATE3D = JDATE
        STIME3D = JTIME

        FTYPE3D = GRDDED3
cyqw    TSTEP3D = TSTEP(1)
        TSTEP3D = TSTEP ( 1 ) * NSTEPS
         
        NCOLS3D = GL_NCOLS
        NROWS3D = GL_NROWS
        NLAYS3D = SA_NLAYS
        NTHIK3D =     1
        GDTYP3D = GDTYP_GD
        P_ALP3D = P_ALP_GD
        P_BET3D = P_BET_GD 
        P_GAM3D = P_GAM_GD
        XORIG3D = XORIG_GD
        YORIG3D = YORIG_GD
        XCENT3D = XCENT_GD
        YCENT3D = YCENT_GD
        XCELL3D = XCELL_GD
        YCELL3D = YCELL_GD
        VGTYP3D = VGTYP_GD
        VGTOP3D = VGTOP_GD


        K = 0
        DO L = ISAM_BLEV, ISAM_ELEV
           K = 1 + K
           VGLVS3D( K ) = VGLVS_GD( L )
        END DO

!       GDNAM3D = GDNAME_GD
        GDNAM3D = GRID_NAME  ! from HGRD_DEFN

        FDESC3D( 1 ) = 'SA_CONC_1 stores contributing srcs '
        DO L = 2, MXDESC3
            FDESC3D( L ) = ' '
        END DO
   
               
        IF ( MYPE .EQ. 0 ) THEN
!           CALL M3EXIT( 'OP_SA', 0, 0, 'Fatal Error Encountered', XSTAT2 )
           IF ( .NOT. OPEN3( FILE_NAME, FSNEW3, PNAME ) ) THEN
              XMSG = 'Could not create '// FILE_NAME // ' file'
              CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
           END IF
        END IF

Ckrt...Write initial conditions to sa_conc_1 ( or last hour sa_cgrid_1 )
          ALLOCATE ( ISAM_BUFF( NCOLS, NROWS, SA_NLAYS, N_SPCTAG ), 
     &     STAT = ALLOCSTAT )
          IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ISAM_BUFF'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          ENDIF
          ISAM_BUFF = 0.0
C
         ! print *, S, N_SPCTAG,ISAM_BLEV, ISAM_ELEV
          DO S = 1, N_SPCTAG
            K = 0
            DO L = ISAM_BLEV, ISAM_ELEV
              K = K + 1
              ISAM_BUFF( :,:,K,S ) = ISAM( :,:,L,S_SPCTAG(S),T_SPCTAG(S) )
            ENDDO
          END DO ! loop over total tags
          IF ( .NOT. WRITE3( FILE_NAME, ALLVAR3, JDATE, JTIME, 
     &     ISAM_BUFF ) ) THEN
            XMSG = 'Could not write to '
     &         // FILE_NAME
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
          if ( MYPE .eq. 0 ) then
            !print*,'Finished write3 S =',S
            !print*,'While N_SPCTAG =',N_SPCTAG
          endif
          DEALLOCATE( ISAM_BUFF )

      
c.....couple ISAM after writing

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   this subroutine must be implemented to overcome Character*1 X 16
c   to Character*16 conversion.
c   new varname will be created by this way.
c
c   by bo wang
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wr_varname(input, length, ntag_tssa, nspc_tssa, 
     &     src_name, varname)
     
c     ...argument
      integer                     length
      character*1              :: input(length)
      integer                     ntag_tssa, nspc_tssa
      character*16                src_name
      character*16                varname

c     ...local variable
      integer                     i
      CHARACTER*7                 CHAR1
Ckrt  CHARACTER(len=16-length) :: CHAR1
      character(len=length)       output
      character*16                temp

c     ...external function
      integer len_trim
      
c.....................................................................
      
      do i = 1, length
         output(i:i) = input(i)
      end do

      temp = src_name

!0711 do i = 1, 7
      do i = 1, min( 7, 16-length-1 )
        char1(i:i) = temp(i:i)
      end do
         
      VARNAME = output // '_' // char1

      end
