
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
      SUBROUTINE OPEMIS ( JDATE, JTIME, EM_TRAC )

C  7 Mar 02 - J.Young: add units string variations
C 29 Oct 05 - J.Young: dyn. layers
C 19 Feb 08 - David Wong: add EM_TRAC = .TRUE. when EMIS_TRAC_1 exists
C 21 Jun 10 - J.Young: convert for Namelist redesign
C 16 Feb 11 - S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN

      USE RUNTIME_VARS    
      USE VGRD_DEFN           ! vertical layer specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
!     USE RXNS_DATA, only : MECHNAME 
      USE EMIS_VARS
      USE EMIS_DEFN
      USE AERO_DATA, only : AEROMODE, N_MODE, MGPG, GPKG
      USE VDIFF_MAP, only : N_SPC_DIFF, DIFF_SPC, DIFF_MASK_AERO

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

      INTEGER      JDATE      ! current model date, coded YYYYDDD
      INTEGER      JTIME      ! current model time, coded HHMMSS
      LOGICAL      EM_TRAC    ! are there tracer emissions?
      INTEGER      NLAY_FILE  ! keep a running maximum of the layers from the gridded files

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'OPEMIS'
      CHARACTER(200 ) :: XMSG
      CHARACTER( 16 ) :: UNITSCK

      LOGICAL ::   LAERO
      LOGICAL ::   WRFLG = .FALSE.
      INTEGER      STATUS, IOS
      INTEGER      V, N, S, ISRM, ITRAC, IGR, IVAR, X    

C-----------------------------------------------------------------------
 
      CALL LOG_SUBHEADING( LOGDEV,'Open Gridded Emissions' )

      EM_GRID_LAYS = 0

C Open All Tracer Emission Files
      DO ISRM = 1,N_EM_SRM
        IF ( EM_FILE_TYPE( ISRM ) .EQ. 'TRAC' ) THEN
           ITRAC = ITSRM( ISRM )

           IF ( .NOT. OPEN3( EM_FILE_NAME( ISRM ), FSREAD3, PNAME ) ) THEN
              XMSG = 'Could not open tracer file'
              CALL M3MESG( XMSG )
           ELSE
             ! Save Tracer Variables for Use in Emissions Species Check
             ! Routine
             EM_FILE_SURR( ISRM )%LEN = NVARS3D
             ALLOCATE ( EM_FILE_SURR( ISRM )%ARRY( NVARS3D ), STAT = STATUS )
             ALLOCATE ( EM_FILE_SURR( ISRM )%UNITS( NVARS3D ), STAT = STATUS )
             EM_FILE_SURR( ISRM )%ARRY = VNAME3D( 1:NVARS3D )

             ! Assign Tracer Emissions Species
             IF ( .NOT. DESC3( EM_FILE_NAME( ISRM ) ) ) THEN
                XMSG = 'Could not get '// EM_FILE_NAME( ISRM ) // ' file description'
                CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
             END IF

             ! Assign Layers to Common Vector
             EM_GRID_LAYS( ISRM ) = NLAYS3D
              
             ! Check Units For Consistency
             UNITSCK = 'BLANK'
             DO N = 1,NVARS3D
                V = INDEX1( VNAME3D( N ), N_SPC_DIFF, DIFF_SPC )
                IF ( V .NE. 0 ) THEN
                  IF ( UNITSCK .EQ. 'BLANK' ) UNITSCK = UNITS3D( N )
                  IF ( UNITS3D( N ) .NE. UNITSCK ) THEN
                     XMSG = 'Units not uniform on ' // EM_FILE_LAB( ISRM )
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                  END IF
                END IF
                EM_FILE_SURR( ISRM )%UNITS( N ) = UNITS3D( N )
             END DO 
           END IF

        END IF   ! tracer emissions
      END DO


C Open Gridded Emission Files (for gas chem, aerosols and non-reactive species)
      DO ISRM = 1,N_EM_SRM
        IF ( EM_FILE_TYPE( ISRM ) .EQ. 'GRID' ) THEN
          IGR = IGSRM( ISRM )
 
          IF ( .NOT. OPEN3( EM_FILE_NAME( ISRM ), FSREAD3, PNAME ) ) THEN
              XMSG = 'Could not open file ' // EM_FILE_NAME( ISRM )
              CALL M3MESG( XMSG )
          ELSE
 
            IF ( .NOT. DESC3( EM_FILE_NAME( ISRM ) ) ) THEN
              XMSG = 'Could not get '// EM_FILE_NAME( ISRM ) // ' file description'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
            
            ! Let the date for this emission stream come from the model
            ! by default or the file if the user wishes to override the
            ! default for representative day files
            EM_FILE_DATE( ISRM ) = JDATE
            IF ( EM_FILE_SYM_DATE( ISRM ) )  EM_FILE_DATE( ISRM ) = SDATE3D

            ! Save Area Source Species Names For Use in Emissions Species
            ! Check Routine
            EM_FILE_SURR( ISRM )%LEN = NVARS3D
            ALLOCATE ( EM_FILE_SURR( ISRM )%ARRY( NVARS3D ), STAT = STATUS )
            ALLOCATE ( EM_FILE_SURR( ISRM )%UNITS( NVARS3D ), STAT = STATUS )
            EM_FILE_SURR( ISRM )%ARRY = VNAME3D( 1:NVARS3D )
           
            ! Assign Area Source Emission Species
            EM_GRID_LAYS( ISRM ) = NLAYS3D

            ! Assign Units
            DO IVAR = 1,NVARS3D
                  ! A match has been found
                  UNITSCK = UNITS3D( IVAR )
                  CALL UPCASE( UNITSCK )
                  EM_FILE_SURR( ISRM )%UNITS( IVAR ) = UNITS3D( IVAR )
            END DO
          END IF
        END IF    ! (gridded emission file)
      END DO

      RETURN
      END
