
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
      SUBROUTINE OPEMIS ( JDATE, JTIME, EM_TRAC )

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
      USE EMIS_VARS
      USE EMIS_DEFN
      USE AERO_DATA, only : AEROMODE, N_MODE, MGPG, GPKG,
     &                      N_MAP_AEROtoPTCL, MAP_AEROtoPTCL
      USE VDIFF_MAP, only : N_SPC_DIFF, DIFF_SPC, DIFF_MASK_AERO

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

      INTEGER      JDATE      ! current model date, coded YYYYDDD
      INTEGER      JTIME      ! current model time, coded HHMMSS
      LOGICAL      EM_TRAC    ! are there tracer emissions?

C External Functions:

      INTEGER, EXTERNAL :: SETUP_LOGDEV

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'OPEMIS'
      CHARACTER( 96 ) :: XMSG
      CHARACTER( 16 ) :: UNITSCK

      LOGICAL ::   LAERO
      LOGICAL ::   WRFLG = .FALSE.
      INTEGER      LOGDEV
      INTEGER      STATUS, IOS
      INTEGER      V, N, S, ISRC, ITRAC, IGR, IVAR, X    

C-----------------------------------------------------------------------
 
      LOGDEV = SETUP_LOGDEV()

      EM_GRID_LAYS = 0

C Open All Tracer Emission Files
      DO ISRC = 1,N_EM_SRC
        IF ( EM_FILE_TYPE( ISRC ) .EQ. 'TRAC' ) THEN
           ITRAC = ITSRC( ISRC )

           IF ( .NOT. OPEN3( EM_FILE_NAME( ISRC ), FSREAD3, PNAME ) ) THEN
              XMSG = 'Could not open tracer file'
              CALL M3MESG( XMSG )
           ELSE

             ! Save Tracer Variables for Use in Emissions Species Check
             ! Routine
             EM_FILE_SURR( ISRC )%LEN = NVARS3D
             ALLOCATE ( EM_FILE_SURR( ISRC )%ARRY( NVARS3D ), STAT = STATUS )
             EM_FILE_SURR( ISRC )%ARRY = VNAME3D( 1:NVARS3D )

             ! Assign Tracer Emissions Species
             IF ( .NOT. DESC3( EM_FILE_NAME( ISRC ) ) ) THEN
                XMSG = 'Could not get '// EM_FILE_NAME( ISRC ) // ' file description'
                CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
             END IF

             ! Assign Layers to Common Vector
             EM_GRID_LAYS( ISRC ) = NLAYS3D
              
             ! Check Units For Consistency
             UNITSCK = 'BLANK'
             DO N = 1,NVARS3D
                V = INDEX1( VNAME3D( N ), N_SPC_DIFF, DIFF_SPC )
                IF ( V .NE. 0 ) THEN
                  IF ( UNITSCK .EQ. 'BLANK' ) UNITSCK = UNITS3D( V )
                  IF ( UNITS3D( V ) .NE. UNITSCK ) THEN
                     XMSG = 'Units not uniform on ' // EM_FILE_LAB( ISRC )
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                  END IF
                END IF
             END DO 
           END IF

        END IF   ! tracer emissions
      END DO


C Open Gridded Emission Files (for gas chem, aerosols and non-reactive species)
      DO ISRC = 1,N_EM_SRC
        IF ( EM_FILE_TYPE( ISRC ) .EQ. 'GRID' ) THEN
          IGR = IGSRC( ISRC )
 
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
            EM_FILE_SURR( ISRC )%LEN = NVARS3D
            ALLOCATE ( EM_FILE_SURR( ISRC )%ARRY( NVARS3D ), STAT = STATUS )
            EM_FILE_SURR( ISRC )%ARRY = VNAME3D( 1:NVARS3D )
           
            ! Assign Area Source Emission Species
            EM_GRID_LAYS( ISRC ) = NLAYS3D

            ! Check Units for Consistency 
            ! EMIS_GAS_UNITS and EMIS_AER_UNITS are initalized in
            ! EMIS_VARS
            DO IVAR = 1,NVARS3D
              ! Check if Diffused Species are on the file list
              V = INDEX1( VNAME3D( IVAR ), N_SPC_DIFF, DIFF_SPC )
              X = INDEX1( VNAME3D( IVAR ), N_MAP_AEROtoPTCL, MAP_AEROtoPTCL( : )%PTCL )
              IF ( V + X .NE. 0 ) THEN
                  ! A match has been found
                  UNITSCK = UNITS3D( IVAR )
                  CALL UPCASE( UNITSCK )

                  LAERO = .FALSE.
                  IF ( V .NE. 0 ) LAERO = DIFF_MASK_AERO( V )
                  IF ( X .NE. 0 .OR. LAERO ) THEN
                     ! Aerosol Species
                     IF ( EMIS_AER_UNITS( ISRC ) .EQ. 'NONE' ) 
     &                    EMIS_AER_UNITS( ISRC ) = UNITSCK
                     IF ( UNITSCK .NE. EMIS_AER_UNITS( ISRC ) ) THEN
                         WRITE( XMSG, '(A,A,A,A,A)' ), 'The aerosol units for ', 
     &                          EM_FILE_DESC( ISRC ),' are inconsistent: ',
     &                          EMIS_AER_UNITS( ISRC ),' | ',UNITSCK
                         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                     END IF

                     IF ( EMIS_AER_UNITS( ISRC ) .EQ. 'G/S' )   
     &                    CONVEM_PM_MASS( ISRC ) = MGPG  ! g/s -> ug/s
                     IF ( EMIS_AER_UNITS( ISRC ) .EQ. 'KG/HR' ) 
     &                    CONVEM_PM_MASS( ISRC ) = GPKG * MGPG / 3600. ! kg/hr -> ug/s
                  ELSE
                     !Gas Species
                     IF ( EMIS_GAS_UNITS( ISRC ) .EQ. 'NONE' ) 
     &                    EMIS_GAS_UNITS( ISRC ) = UNITSCK
                     IF ( UNITSCK .NE. EMIS_GAS_UNITS( ISRC ) ) THEN
                         WRITE( XMSG, '(A,A,A,A,A)' ), 'The gas units for ', 
     &                          EM_FILE_DESC( ISRC ),' are inconsistent: ',
     &                          EMIS_GAS_UNITS( ISRC ),' | ',UNITSCK
                         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                     END IF

                     IF ( EMIS_GAS_UNITS( ISRC ) .EQ. 'MOLES/S'   .OR.
     &                    EMIS_GAS_UNITS( ISRC ) .EQ. 'MOLES/SEC' .OR.
     &                    EMIS_GAS_UNITS( ISRC ) .EQ. 'MOLE/S'    .OR.
     &                    EMIS_GAS_UNITS( ISRC ) .EQ. 'MOLE/SEC'  .OR.
     &                    EMIS_GAS_UNITS( ISRC ) .EQ. 'MOL/S'     .OR.
     &                    EMIS_GAS_UNITS( ISRC ) .EQ. 'MOL/SEC'   
     &                ) THEN
                           CONVEM( ISRC ) = 1.0E-3  ! mol/s -> kmol/s
                     ELSE
                         WRITE( XMSG, '(A,A,A,A,A)' ), 'The gas units for ', 
     &                          EM_FILE_DESC( ISRC ),' are not expected: ',
     &                          EMIS_GAS_UNITS( ISRC ),'. They should be MOLE/S.'
                         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                     END IF
                   END IF
                 END IF
            END DO

          END IF  ! (gas, aerosol or non-react emissions)
        END IF    ! (gridded emission file)
      END DO

      RETURN
      END
