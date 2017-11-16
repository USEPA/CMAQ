
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/opemis.F,v 1.3 2011/10/21 16:10:47 yoj Exp $
 
C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPEMIS ( JDATE, JTIME, EM_TRAC, CONVEM )

C  7 Mar 02 - J.Young: add units string variations
C 29 Oct 05 - J.Young: dyn. layers
C 19 Feb 08 - David Wong: add EM_TRAC = .TRUE. when EMIS_TRAC_1 exists
C 21 Jun 10 - J.Young: convert for Namelist redesign
C 16 Feb 11 - S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN

      USE VGRD_DEFN           ! vertical layer specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
      USE RXNS_DATA, only : MECHNAME 
      USE EMIS_GLOBAL
      USE EMIS_DEFN
      USE AERO_DATA, only : AEROMODE, N_MODE, PMEM_UNITS

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

      INTEGER      JDATE      ! current model date, coded YYYYDDD
      INTEGER      JTIME      ! current model time, coded HHMMSS
      LOGICAL      EM_TRAC    ! are there tracer emissions?
      REAL         CONVEM     ! conversion for emissions rates

C External Functions:

      INTEGER, EXTERNAL :: SETUP_LOGDEV

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'OPEMIS'
      CHARACTER( 96 ) :: XMSG
      CHARACTER( 16 ) :: UNITSCK

      LOGICAL ::   SOME_GAS_EMIS
      LOGICAL ::   WRFLG = .FALSE.
      INTEGER      LOGDEV
      INTEGER      STATUS
      INTEGER      V, N, S, ISRC        ! induction variables

C-----------------------------------------------------------------------
 
      LOGDEV = SETUP_LOGDEV()

      EM_GRID_LAYS = 0

C Open All Tracer Emission Files
      DO ISRC = 1,N_EM_SRC
        IF ( EM_FILE_TYPE( ISRC ) .EQ. 'TRAC' ) THEN
           ITRAC = ITSRC( ISRC )

           WRITE( VLAB,'( "TR_EMIS_LABEL",I2.2 )' ) ITRAC
           EM_FILE_LAB( ISRC ) = ENVINT( VLAB,
     &                     'Tracer Emission File Label',VLAB, IOS )

           WRITE( EM_FILE_NAME( ISRC ),'( "TR_EMIS_",I2.2 )' ) ITRAC
           IF ( .NOT. OPEN3( EM_FILE_NAME( ISRC ), FSREAD3, PNAME ) ) THEN
              XMSG = 'Could not open tracer file'
              CALL M3MESG( XMSG )
           ELSE

             ! Save Tracer Variables for Use in Emissions Species Check
             ! Routine
             EM_FILE_SPEC( ISRC )%LEN = NVARS3D
             ALLOCATE ( EM_FILE_SPEC( ISRC )%ARRY( NVARS3D ), STAT = STATUS )
             EM_FILE_SPEC( ISRC )%ARRY = VNAME3D( 1:NVARS3D )

             ! Assign Tracer Emissions Species
             IF ( .NOT. DESC3( EM_FILE_NAME( ISRC ) ) ) THEN
                XMSG = 'Could not get '// EM_FILE_NAME( ISRC ) // ' file description'
                CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
             END IF

             ! Assign Layers to Common Vector
             EM_GRID_LAYS( ISRC ) = NLAYS3D

             ! Check Units For Consistency
             UNITSCK = 'BLANK'
             DO N = 1, N_TR_EMIS
                V = INDEX1( TR_EMIS( N ), NVARS3D, VNAME3D )
                IF ( V .NE. 0 ) THEN
                  IF ( UNITSCK .EQ. 'BLANK' ) UNITSCK = UNITS3D( V )
                  IF ( UNITS3D( V ) .NE. UNITSCK ) THEN
                     XMSG = 'Units not uniform on ' // EMIS_TRAC_1
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                  END IF
                END IF
             END DO

           END IF

        END IF   ! tracer emissions
      END DO


C Open the emissions file (for gas chem, aerosols and non-reactive species)
      DO ISRC = 1,N_EM_SRC
        IF ( EM_FILE_TYPE( ISRC ) .EQ. 'GRID' ) THEN
           IGR = IGSRC( ISRC )
 
           WRITE( VLAB,'( "GR_EMIS_LABEL",I2.2 )' ) IGR
           EM_FILE_LAB( ISRC ) = ENVINT( VLAB,
     &                     'Gridded Emission File Label',VLAB, IOS )

           WRITE( EM_FILE_NAME( ISRC ),'( "GR_EMIS_",I2.2 )' ) IGR
           IF ( .NOT. OPEN3( EM_FILE_NAME( ISRC ), FSREAD3, PNAME ) ) THEN
              XMSG = 'Could not open file ' // EM_FILE_NAME( ISRC )
              CALL M3MESG( XMSG )
           ELSE
 
           IF ( .NOT. DESC3( EM_FILE_NAME( ISRC ) ) ) THEN
              XMSG = 'Could not get '// EM_FILE_NAME( ISRC ) // ' file description'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
           END IF

           ! Save Area Source Species Names For Use in Emissions Species
           ! Check Routine
           EM_FILE_SPEC( ISRC )%LEN = NVARS3D
           ALLOCATE ( EM_FILE_SPEC( ISRC )%ARRY( NVARS3D ), STAT = STATUS )
           EM_FILE_SPEC( ISRC )%ARRY = VNAME3D( 1:NVARS3D )

           ! Assign Area Source Emission Species
           EM_GRID_LAYS( ISRC ) = NLAYS3D

           ! Check for Consistency in Gas-Phase Emission Units
           DO N = 1, N_GC_EMIS
              V = INDEX1( GC_EMIS( N ), NVARS3D, VNAME3D )
              IF ( V .NE. 0 ) THEN
                 UNITSCK = UNITS3D( V )
                 CALL UPCASE ( UNITSCK )
                 IF ( UNITSCK .NE. 'MOLES/S'   .AND.
     &              UNITSCK .NE. 'MOLE/S'    .AND.
     &              UNITSCK .NE. 'MOL/S'     .AND.
     &              UNITSCK .NE. 'MOLES/SEC' .AND.
     &              UNITSCK .NE. 'MOLE/SEC'  .AND.
     &              UNITSCK .NE. 'MOL/SEC'   .AND.
     &              ( INDEX( MECHNAME, 'NVPOA' ) .NE. 0 .OR.  ! If the semivolatile POA mechanism
     &                GC_EMIS( N ) .NE. 'POC' )   ) THEN      ! is active, then some gas-phase species
                                                              ! will be scaled off of POC
                    XMSG = 'GC units incorrect on ' // EM_FILE_NAME( ISRC ) //' for species ' // GC_EMIS( N )
                    !CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                    CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
                 END IF
              END IF
           END DO

           ! Check For Consistency Among Aerosol Emission Units
           UNITSCK = 'BLANK'
           DO N = 1, N_AE_EMIS
              IF ( ANY( AEROMODE( : )%NUM_NAME .EQ. AE_EMIS( N ) ) .OR.
     &             ANY( AEROMODE( : )%SRF_NAME .EQ. AE_EMIS( N ) ) ) CYCLE
              V = INDEX1( AE_EMIS( N ), NVARS3D, VNAME3D )
              IF ( V .NE. 0 ) THEN
                 IF ( UNITSCK .EQ. 'BLANK' ) UNITSCK = UNITS3D( V )
                 IF ( UNITSCK .NE. UNITS3D( V ) ) THEN
                    XMSG = 'AE units are inconsistent on ' // EM_FILE_NAME( ISRC )
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                 END IF
                 PMEM_UNITS = UNITSCK
              END IF
           END DO
 
           ! Check For Consistency Among Non-Reactive Species
           DO N = 1, N_NR_EMIS
              V = INDEX1( NR_EMIS( N ), NVARS3D, VNAME3D )
              IF ( V .NE. 0 ) THEN
                 UNITSCK = UNITS3D( V )
                 CALL UPCASE ( UNITSCK )
                 IF ( UNITSCK .NE. 'MOLES/S'   .AND.
     &                UNITSCK .NE. 'MOLE/S'    .AND.
     &                UNITSCK .NE. 'MOL/S'     .AND.
     &                UNITSCK .NE. 'MOLES/SEC' .AND.
     &                UNITSCK .NE. 'MOLE/SEC'  .AND.
     &                UNITSCK .NE. 'MOL/SEC' ) THEN
                    XMSG = 'NR units incorrect on ' // EM_FILE_NAME( ISRC )
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                 END IF
              END IF
           END DO

           ! Assume units = mol/s across gas and non-reactive species classes
           CONVEM = 1.0E-03  ! assuming gram-moles

         END IF  ! (gas, aerosol or non-react emissions)
      END DO

C Print Alerts that Species Emissions Will Be Treated 
C Comment these alerts out because it is non-essential information.
      IF ( N_GC_EMIS .GT. 0 ) THEN
         WRFLG = .TRUE.
         !WRITE( LOGDEV,1001 )
1001     FORMAT( / 10X, 'Gas Chemistry Emissions Processing in',
     &              1X, 'Vertical diffusion ...' )
      END IF

1003     FORMAT( / 10X, 'Aerosol Emissions Processing in',
     &              1X, 'Vertical diffusion ...' )

      IF ( N_NR_EMIS .GT. 0 ) THEN
         WRFLG = .TRUE.
         !WRITE( LOGDEV,1005 )
1005     FORMAT( / 10X, 'Non-reactives Emissions Processing in',
     &              1X, 'Vertical diffusion ...' )
      END IF

      IF ( N_TR_EMIS .GT. 0 .AND. EM_TRAC ) THEN
         WRFLG = .TRUE.
         !WRITE( LOGDEV,1007 )
1007     FORMAT( / 10X, 'Tracer Emissions Processing in',
     &              1X, 'Vertical diffusion ...' )
      END IF

      RETURN
      END
