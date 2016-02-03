
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

      SUBROUTINE OP_SA ( JDATE, JTIME, TSTEP, NSTEPS, ISAM, FILE_NAME )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c20140428  Opens ISAM's SA_CONC_1 file
c       op_sa.F called by initscen.F
c
c
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


!      USE HGRD_DEFN             ! horizontal domain specifications
      USE GRID_CONF
      USE SA_LAYERS
      USE SA_DEFN               ! Mc06
      USE UTILIO_DEFN           ! replaces ioparms, iofdesc, iodecl
      USE SUBST_MODULES         ! stenex

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
      REAL, POINTER :: ISAM( :,:,:,:,: )
      CHARACTER( 16 )       :: FILE_NAME

   
C...End of argument declaration

C Local variables:
      
      INTEGER          ALLOCSTAT, LOGDEV

      CHARACTER( 16 ), DIMENSION( :,: ), ALLOCATABLE::VNAME
      CHARACTER( 1 ),  DIMENSION(:), ALLOCATABLE::SPECIE_NAME
      CHARACTER( 16 ) :: TEMP, CSUFFX
      INTEGER          LENGTH

      CHARACTER( 16 ) :: PNAME = 'OP_SA'
      CHARACTER( 96 ) :: XMSG = ' '
      
      INTEGER   C,R,S,K,N, L    ! loop induction variables
      INTEGER   ITAG, JSPC, JSPCTAG


      REAL, ALLOCATABLE :: ISAM_BUFF ( :,:,:,: )

Ckrt Diagnostic
      CHARACTER( 256 ) EQNAME

C-----------------------------------------------------------------------



Ckrt Create variable names for tag output


        LOGDEV = INIT3()

        ALLOCATE( VNAME(NSPC_SA,NTAG_SA) )
        VNAME = ''
        DO ITAG = 1, NTAG_SA
          DO JSPC = 1, NSPC_SA
            LENGTH = LEN_TRIM( SPC_NAME( JSPC,ITAG ) )
            IF ( LENGTH .GT. 0 ) THEN
              TEMP = SPC_NAME( JSPC,ITAG )( 1:LENGTH )
              ALLOCATE ( SPECIE_NAME( LENGTH ) )
              DO N = 1, LENGTH
                SPECIE_NAME(N:N) = TEMP(N:N)
              ENDDO
              IF ( ITAG .EQ. BCONTAG ) THEN
                CSUFFX = 'BCON            '
                CALL WR_VARNAME( SPECIE_NAME, LENGTH, NTAG_SA, NSPC_SA,
     &             CSUFFX, VNAME( JSPC,ITAG ) )
              ELSEIF ( ITAG .EQ. OTHRTAG ) THEN
                CSUFFX = 'OTHR            '
                CALL WR_VARNAME( SPECIE_NAME, LENGTH, NTAG_SA, NSPC_SA,
     &             CSUFFX, VNAME( JSPC,ITAG ) )
              ELSEIF ( ITAG .EQ. ICONTAG ) THEN
                CSUFFX = 'ICON            '
                CALL WR_VARNAME( SPECIE_NAME, LENGTH, NTAG_SA, NSPC_SA,
     &             CSUFFX, VNAME( JSPC,ITAG ) )
              ELSE
                CALL WR_VARNAME( SPECIE_NAME, LENGTH, NTAG_SA, NSPC_SA,
     &             TAGNAME(ITAG), VNAME( JSPC,ITAG ) )
              ENDIF ! Is itag bcon, othr, icon, or emis ?
              DEALLOCATE ( SPECIE_NAME )
            ENDIF ! length > 0?
          ENDDO ! jspc
        ENDDO ! itag

Ckrt Set nvars3d for SA_CONC_1 or SA_CGRID_1 file
        NVARS3D = N_SPCTAG

Ckrt Set vname3d for SA_CONC_1 file
        JSPCTAG = 0
        DO JSPC = 1, NSPC_SA
          DO ITAG = 1, NTAG_SA
           IF ( ( SPC_NAME(JSPC,ITAG) .EQ. 'AECJ' .OR.
     &        SPC_NAME(JSPC,ITAG) .EQ. 'AECI' ) .AND.
     &          L_EC( ITAG ) ) THEN
             JSPCTAG = JSPCTAG + 1
             VTYPE3D( JSPCTAG ) = M3REAL   
             VNAME3D( JSPCTAG ) = VNAME(JSPC,ITAG)
             UNITS3D( JSPCTAG ) = "micrograms/m**3 "
             VDESC3D( JSPCTAG ) = "tracer conc."
             S_SPCTAG  ( JSPCTAG ) = JSPC
             T_SPCTAG  ( JSPCTAG ) = ITAG
             VNAM_SPCTAG ( JSPCTAG ) = VNAME(JSPC,ITAG)
           ENDIF ! spc_name is AEC?
           IF ( ( SPC_NAME(JSPC,ITAG) .EQ. 'APOCJ' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'APOCI' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'APNCOMJ' .OR. 
     &         SPC_NAME(JSPC,ITAG) .EQ. 'APNCOMI' ) .AND.
     &         L_OC( ITAG ) ) THEN
             JSPCTAG = JSPCTAG + 1
             VTYPE3D( JSPCTAG ) = M3REAL   
             VNAME3D( JSPCTAG ) = VNAME(JSPC,ITAG)
             UNITS3D( JSPCTAG ) = "micrograms/m**3 "
             VDESC3D( JSPCTAG ) = "tracer conc."
             S_SPCTAG  ( JSPCTAG ) = JSPC
             T_SPCTAG  ( JSPCTAG ) = ITAG
             VNAM_SPCTAG ( JSPCTAG ) = VNAME(JSPC,ITAG)
           ENDIF ! spc_name is APOC?
           IF ( ( SPC_NAME(JSPC,ITAG) .EQ. 'ASO4J' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'ASO4I' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'SO2'   .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'SULF' .OR. 
     &         SPC_NAME(JSPC,ITAG) .EQ. 'SULRXN' ) .AND.    !20130529
     &         L_SFATE( ITAG ) ) THEN
             JSPCTAG = JSPCTAG + 1
             VTYPE3D( JSPCTAG ) = M3REAL   
             VNAME3D( JSPCTAG ) = VNAME(JSPC,ITAG)
             IF ( SPC_NAME(JSPC,ITAG) .EQ. 'SO2' .OR.
     &             SPC_NAME(JSPC,ITAG) .EQ. 'SULF' .OR.
     &             SPC_NAME(JSPC,ITAG) .EQ. 'SULRXN' ) THEN
               UNITS3D( JSPCTAG ) = "ppmV            "
             ELSE
               UNITS3D( JSPCTAG ) = "micrograms/m**3 "
             ENDIF
             VDESC3D( JSPCTAG ) = "tracer conc."
             S_SPCTAG  ( JSPCTAG ) = JSPC
             T_SPCTAG  ( JSPCTAG ) = ITAG
             VNAM_SPCTAG ( JSPCTAG ) = VNAME(JSPC,ITAG)
           ENDIF ! spc_name is ASO4? or SO2
           IF ( ( SPC_NAME(JSPC,ITAG) .EQ. 'ANO3J' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'ANO3I' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'HNO3' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'NTR' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'NO' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'NO2' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'NO3' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'HONO' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'N2O5' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'PNA' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'PAN' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'PANX' ) .AND.
     &          L_NTRATE( ITAG ) ) THEN
             JSPCTAG = JSPCTAG + 1
             VTYPE3D( JSPCTAG ) = M3REAL   
             VNAME3D( JSPCTAG ) = VNAME(JSPC,ITAG)
             IF ( SPC_NAME(JSPC,ITAG)(1:4) .EQ. 'ANO3' ) THEN
               UNITS3D( JSPCTAG ) = "micrograms/m**3 "
             ELSE
               UNITS3D( JSPCTAG ) = "ppmV            "
             ENDIF
             VDESC3D( JSPCTAG ) = "tracer conc."
             S_SPCTAG  ( JSPCTAG ) = JSPC
             T_SPCTAG  ( JSPCTAG ) = ITAG
             VNAM_SPCTAG ( JSPCTAG ) = VNAME(JSPC,ITAG)
           ENDIF ! spc_name is ANO3?
           IF ( ( SPC_NAME(JSPC,ITAG) .EQ. 'ANH4J' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'ANH4I' .OR.
     &         SPC_NAME(JSPC,ITAG) .EQ. 'NH3' ) .AND.
     &         L_NH4( ITAG ) ) THEN
             JSPCTAG = JSPCTAG + 1
             VTYPE3D( JSPCTAG ) = M3REAL   
             VNAME3D( JSPCTAG ) = VNAME(JSPC,ITAG)
             IF ( SPC_NAME(JSPC,ITAG) .EQ. 'NH3' ) THEN
               UNITS3D( JSPCTAG ) = "ppmV            "
             ELSE
               UNITS3D( JSPCTAG ) = "micrograms/m**3 "
             ENDIF
             VDESC3D( JSPCTAG ) = "tracer conc."
             S_SPCTAG  ( JSPCTAG ) = JSPC
             T_SPCTAG  ( JSPCTAG ) = ITAG
             VNAM_SPCTAG ( JSPCTAG ) = VNAME(JSPC,ITAG)
           ENDIF ! spc_name is ANH4? or NH3
           IF ( ( SPC_NAME(JSPC,ITAG) .EQ. 'ACLJ' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'ACLI' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'ANAJ' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'ANAI' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'AMGJ' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'AKJ' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'ACAJ' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'AFEJ' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'AALJ' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'ASIJ' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'ATIJ' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'AMNJ' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'AOTHRJ' .OR.
     &          SPC_NAME(JSPC,ITAG) .EQ. 'AOTHRI' ) .AND.
     &          L_PM25( ITAG ) ) THEN
             JSPCTAG = JSPCTAG + 1
             VTYPE3D( JSPCTAG ) = M3REAL
             VNAME3D( JSPCTAG ) = VNAME(JSPC,ITAG)
             UNITS3D( JSPCTAG ) = "micrograms/m**3 "
             S_SPCTAG  ( JSPCTAG ) = JSPC
             T_SPCTAG  ( JSPCTAG ) = ITAG
             VNAM_SPCTAG ( JSPCTAG ) = VNAME(JSPC,ITAG)
           ENDIF ! spc_name is one of the pm25 ions
           IF ( SPC_NAME(JSPC,ITAG) .EQ. 'CO' .AND.
     &          L_CO( ITAG ) ) THEN
             JSPCTAG = JSPCTAG + 1
             VTYPE3D( JSPCTAG ) = M3REAL
             VNAME3D( JSPCTAG ) = VNAME(JSPC,ITAG)
             UNITS3D( JSPCTAG ) = "ppmV            "
             VDESC3D( JSPCTAG ) = "tracer conc."
             S_SPCTAG  ( JSPCTAG ) = JSPC
             T_SPCTAG  ( JSPCTAG ) = ITAG
             VNAM_SPCTAG ( JSPCTAG ) = VNAME(JSPC,ITAG)
           ENDIF
           IF ( SPC_NAME(JSPC,ITAG) .EQ. 'O3A' .AND.
     &          L_OZONE( ITAG ) ) THEN   ! 20130627
             JSPCTAG = JSPCTAG + 1
             VTYPE3D( JSPCTAG ) = M3REAL
             VNAME3D( JSPCTAG ) = VNAME(JSPC,ITAG)
             UNITS3D( JSPCTAG ) = "ppmV            "
             VDESC3D( JSPCTAG ) = "tracer conc."
             S_SPCTAG  ( JSPCTAG ) = JSPC
             T_SPCTAG  ( JSPCTAG ) = ITAG
             VNAM_SPCTAG ( JSPCTAG ) = VNAME(JSPC,ITAG)
           ENDIF  ! 1-ozone
           IF ( SPC_NAME(JSPC,ITAG) .EQ. 'O3V' .AND. 
     &          L_OZONE( ITAG ) ) THEN
             JSPCTAG = JSPCTAG + 1
             VTYPE3D( JSPCTAG ) = M3REAL
             VNAME3D( JSPCTAG ) = VNAME(JSPC,ITAG)
             UNITS3D( JSPCTAG ) = "ppmV            "
             VDESC3D( JSPCTAG ) = "tracer conc."
             S_SPCTAG  ( JSPCTAG ) = JSPC
             T_SPCTAG  ( JSPCTAG ) = ITAG
             VNAM_SPCTAG ( JSPCTAG ) = VNAME(JSPC,ITAG)
           ENDIF  ! VOC-limited ozone
           IF ( SPC_NAME(JSPC,ITAG) .EQ. 'O3N' .AND.    
     &          L_OZONE( ITAG ) ) THEN
             JSPCTAG = JSPCTAG + 1
             VTYPE3D( JSPCTAG ) = M3REAL
             VNAME3D( JSPCTAG ) = VNAME(JSPC,ITAG)
             UNITS3D( JSPCTAG ) = "ppmV            "
             VDESC3D( JSPCTAG ) = "tracer conc."
             S_SPCTAG  ( JSPCTAG ) = JSPC
             T_SPCTAG  ( JSPCTAG ) = ITAG
             VNAM_SPCTAG ( JSPCTAG ) = VNAME(JSPC,ITAG)
           ENDIF ! NOx-limited ozone
           IF ( (SPC_NAME(JSPC,ITAG) .EQ. 'ALD2' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'ALDX' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'ETH' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'ETHA' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'ETOH' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'FORM' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'IOLE' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'ISOP' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'MEOH' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'OLE' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'PAR' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'TERP' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'TOL' .OR.
     &           SPC_NAME(JSPC,ITAG) .EQ. 'XYL' ) .AND.
     &          L_VOC( ITAG ) ) THEN
             JSPCTAG = JSPCTAG + 1
             VTYPE3D( JSPCTAG ) = M3REAL
             VNAME3D( JSPCTAG ) = VNAME(JSPC,ITAG)
             UNITS3D( JSPCTAG ) = "ppmV            "
             VDESC3D( JSPCTAG ) = "tracer conc."
             S_SPCTAG  ( JSPCTAG ) = JSPC
             T_SPCTAG  ( JSPCTAG ) = ITAG
             VNAM_SPCTAG ( JSPCTAG ) = VNAME(JSPC,ITAG)
           ENDIF

           if ( MYPE .EQ. 0 ) print*, JSPCTAG,  
     &        VNAME3D( JSPCTAG ), UNITS3D( JSPCTAG ),
     &        S_SPCTAG( JSPCTAG ), T_SPCTAG( JSPCTAG )
          END DO ! tag loop
        END DO ! species loop
        IF ( YES_PRINT ) THEN
          if ( MYPE .eq. 0 ) then
           print*,'In OP_SA, after assigning vname3d, units3d, etc...'
           print*,'JSPCTAG becomes...',JSPCTAG
          endif
        ENDIF !yes_print
       
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


        DO L = ISAM_BLEV, ISAM_ELEV !1, NLAYS3D + 1
            VGLVS3D( L ) = VGLVS_GD( L )
        END DO

!       GDNAM3D = GDNAME_GD
        GDNAM3D = GRID_NAME  ! from HGRD_DEFN

        FDESC3D( 1 ) = 'SA_CONC_1 stores contributing srcs '
        DO L = 2, MXDESC3
            FDESC3D( L ) = ' '
        END DO
   
               
        IF ( MYPE .EQ. 0 ) THEN
           IF ( .NOT. OPEN3( FILE_NAME, FSNEW3, PNAME ) ) THEN
              XMSG = 'Could not create '// FILE_NAME // ' file'
              CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
           END IF
        END IF

Ckrt...Write initial conditions to sa_conc_1 ( or last hour sa_cgrid_1 )
          ALLOCATE ( ISAM_BUFF( MY_NCOLS, MY_NROWS, SA_NLAYS, N_SPCTAG ), 
     &     STAT = ALLOCSTAT )
          IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ISAM_BUFF'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          ENDIF
          ISAM_BUFF = 0.0
C
          DO S = 1, N_SPCTAG
            DO L = ISAM_BLEV, ISAM_ELEV
              ISAM_BUFF( :,:,L,S ) = ISAM( :,:,L,S_SPCTAG(S),T_SPCTAG(S) )
            ENDDO
          END DO ! loop over total tags
          IF ( .NOT. WRITE3( FILE_NAME, ALLVAR3, JDATE, JTIME, 
     &     ISAM_BUFF ) ) THEN
            XMSG = 'Could not write to '
     &         // FILE_NAME
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
          if ( MYPE .eq. 0 ) then
            print*,'Finished write3 S =',S
            print*,'While N_SPCTAG =',N_SPCTAG
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
