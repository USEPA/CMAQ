
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

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPWDEP ( JDATE, JTIME, TSTEP )

!-----------------------------------------------------------------------
!
!  FUNCTION:  Open wet dep file(s)
!
!  PRECONDITIONS:
!       Dates and times represented YYYYDDD:HHMMSS.
!
!  REVISION  HISTORY:
!      Date   Who             What
!    -------- ---             -----------------------------------------
!     07/17/15 D. Wong        initial version
!     12/30/19 S. Napelenok   ddm-3d implemenation for v5.3.1
!-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX, SEN_PAR, SENNUM, S_WETDEPSPC
#endif

      IMPLICIT NONE

!...........INCLUDES

      INCLUDE SUBST_CONST            ! constants
      INCLUDE SUBST_FILES_ID         ! file name parameters

      CHARACTER( 120 ) :: XMSG = ' ' ! exit message string

!...........PARAMETERS

!...........ARGUMENTS

      INTEGER, INTENT (IN) :: JDATE            ! current model date, coded YYYYDDD
      INTEGER, INTENT (IN) :: JTIME            ! current model time, coded HHMMSS
      INTEGER, INTENT (IN) :: TSTEP            ! model time step, coded HHMMSS

!...........Local Variables

      CHARACTER( 16 ), SAVE :: PNAME = 'OPWDEP' ! driver program name

      INTEGER       FINI             ! ending position
      INTEGER       L                ! loop counter
      INTEGER       SPC              ! species subscript indices
      INTEGER       STRT             ! starting position
      INTEGER       VAR              ! variable subscript indices

!-----------------------------------------------------------------------

!...Initialization

!...first check to make sure that some species in CGRID were specified
!...for output in the wet deposition array, otherwise notify the user
!...and return

        IF ( N_SPC_WDEP .LE. 0 ) THEN

          XMSG = 'No species were specified for wet deposition ' //
     &           'tracking'
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )

          XMSG = 'ONLY CLOUD DIAGNOSTICS WILL BE WRITTEN TO THE ' //
     &           'WETDEP OUTPUT FILE!'
          CALL M3MESG ( XMSG )

        END IF

#ifndef mpas
C Try to open existing wet deposition file for update

        IF ( IO_PE_INCLUSIVE ) THEN
           WRITE( LOGDEV, * )
           IF ( .NOT. OPEN3( CTM_WET_DEP_1, FSRDWR3, PNAME ) ) THEN

              XMSG = 'Could not open ' // CTM_WET_DEP_1 // ' file for update - '
     &               // 'try to open new'
              CALL M3MESG( XMSG )

!...Create wet deposition file:
!...   copy most of header description from
!...   MET_CRO_2D and initialize depositions to zero.

!...open MET_CRO_2D

              IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
                XMSG = 'Could not open '// MET_CRO_2D // ' file'
                CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
              END IF

!...get description from the met file

              IF ( .NOT. DESC3( MET_CRO_2D ) ) THEN
                XMSG = 'Could not get ' // MET_CRO_2D //' file description'
                CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
              END IF

              NCOLS3D = GL_NCOLS
              NROWS3D = GL_NROWS

              XORIG3D = XORIG_GD
              YORIG3D = YORIG_GD

!...advance to time variable for the output file to the next hour

              SDATE3D = JDATE
              STIME3D = 10000 * ( JTIME / 10000 )     !  clear MMSS fields
              TSTEP3D = TSTEP     
              CALL NEXTIME ( SDATE3D, STIME3D, TSTEP3D ) !  on the next hour

              NVARS3D = N_SPC_WDEP + 1

!...define wet deposition output descriptors for gaseous species

              SPC = 0
              STRT = 1
              FINI = N_GC_WDEP
              DO VAR = STRT, FINI
                 SPC = SPC + 1
                 VTYPE3D ( VAR ) = M3REAL
                 VNAME3D ( VAR ) = GC_WDEP( SPC )
                 UNITS3D ( VAR ) = 'kg ha-1'
                 VDESC3D ( VAR ) = 'hourly wet deposition values'
              END DO

!...define wet deposition output descriptors for aerosol species

              SPC = 0
              STRT = N_GC_WDEP + 1
              FINI = N_GC_WDEP + N_AE_WDEP
              DO VAR = STRT, FINI
                 SPC = SPC + 1
                 VTYPE3D ( VAR ) = M3REAL
                 VNAME3D ( VAR ) = AE_WDEP( SPC )
                 IF ( INDEX( AE_WDEP( SPC ), 'NUM' ) .GT. 0 ) THEN
                    UNITS3D ( VAR ) = 'ha-1'
                 ELSE IF ( INDEX( AE_WDEP( SPC ), 'SRF' ) .GT. 0 ) THEN
                    UNITS3D ( VAR ) = 'm2 ha-1'
                 ELSE
                    UNITS3D ( VAR ) = 'kg ha-1'
                 END IF
                 VDESC3D ( VAR ) = 'hourly wet deposition values'
              END DO

!...define wet deposition output descriptors for non-reactive species

              SPC = 0
              STRT = N_GC_WDEP + N_AE_WDEP + 1
              FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP
              DO VAR = STRT, FINI
                 SPC = SPC + 1
                 VTYPE3D ( VAR ) = M3REAL
                 VNAME3D ( VAR ) = NR_WDEP( SPC )
                 UNITS3D ( VAR ) = 'kg ha-1'
                 VDESC3D ( VAR ) = 'hourly wet deposition values'
              END DO

!...define wet deposition output descriptors for tracer species

              SPC = 0
              STRT = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + 1
              FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP
              DO VAR = STRT, FINI
                 SPC = SPC + 1
                 VTYPE3D ( VAR ) = M3REAL
                 VNAME3D ( VAR ) = TR_WDEP( SPC )
                 UNITS3D ( VAR ) = 'kg ha-1'
                 VDESC3D ( VAR ) = 'hourly wet deposition values'
              END DO

!...define output descriptor for hydrogen concentration

              VAR = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP + 1
              VTYPE3D( VAR ) = M3REAL
              VNAME3D( VAR ) = 'HPLUS'
              UNITS3D( VAR ) = 'kg ha-1'
              VDESC3D( VAR ) = 'hourly wet deposition values'

!...initialize the file descriptors

              FDESC3D( 1 ) = 'hourly 1-layer cross-point wet deposition'
     &                       // ' from all cloud types'
              DO L = 2 , MXDESC3
                 FDESC3D( L ) = ' '
              END DO

              GDNAM3D = GRID_NAME  ! from HGRD_DEFN

!...try to open the output file new

              IF ( .NOT. OPEN3( CTM_WET_DEP_1, FSNEW3, PNAME ) ) THEN
                 XMSG = 'Could not create '// CTM_WET_DEP_1 // ' file'
                 CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
              END IF

           END IF   ! open CTM_WET_DEP_1 read/write

#ifdef sens
           IF ( .NOT. OPEN3( CTM_SWETDEP_1, FSRDWR3, PNAME ) ) THEN
               XMSG = 'Could not open ' // CTM_SWETDEP_1 //
     &                ' file for update - try to open new'
               CALL M3MESG( XMSG )

C...Create sensitivity wet deposition file:
               NVARS3D = ( N_SPC_WDEP + 1 ) * NPMAX

               ALLOCATE ( S_WETDEPSPC( NVARS3D ) )
               S_WETDEPSPC = 'blank'

               VTYPE3D = M3REAL
               VDESC3D = 'Sens of hourly wet dep values'              

C...define sens wet deposition output descriptors for gaseous species
               SPC = 0
               STRT = 1
               FINI = N_GC_WDEP
               DO VAR = STRT, FINI
                 SPC = SPC + 1
                 DO NP = 1, NPMAX
                   SENNUM = ( SPC - 1 ) * NPMAX + NP
                   VNAME3D ( SENNUM ) = TRIM( GC_WDEP( SPC )( 1:12 ) ) //'_'// SEN_PAR( NP )
                   UNITS3D ( SENNUM ) = 'kg/hectare'
                   S_WETDEPSPC ( SENNUM ) = VNAME3D ( SENNUM )
                 END DO
               END DO

C...define wet deposition output descriptors for aerosol species
               SPC = 0
               STRT = N_GC_WDEP + 1
               FINI = N_GC_WDEP + N_AE_WDEP
               DO VAR = STRT, FINI
                  SPC = SPC + 1
                  DO NP = 1, NPMAX
                     SENNUM = ( NPMAX * N_GC_WDEP ) + ( SPC - 1 ) *NPMAX + NP
                     VNAME3D ( SENNUM ) = TRIM( AE_WDEP( SPC )( 1:12 ) ) //'_'// SEN_PAR( NP )
                     IF ( INDEX( AE_WDEP( SPC ), 'NUM' ) .GT. 0 ) THEN
                        UNITS3D ( SENNUM ) = 'number/hectare'
                     ELSE IF ( INDEX( AE_WDEP( SPC ), 'SRF' ) .GT. 0 ) THEN
                        UNITS3D ( SENNUM ) = 'm2/hectare'
                     ELSE
                        UNITS3D ( SENNUM ) = 'kg/hectare'
                     END IF
                     S_WETDEPSPC ( SENNUM ) = VNAME3D ( SENNUM )
                  END DO
               END DO

C...define wet deposition output descriptors for non-reactive species
               SPC = 0
               STRT = N_GC_WDEP + N_AE_WDEP + 1
               FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP
               DO VAR = STRT, FINI
                  SPC = SPC + 1
                  DO NP = 1, NPMAX
                     SENNUM = NPMAX * ( N_GC_WDEP + N_AE_WDEP ) + ( SPC - 1 ) * NPMAX + NP
                     VNAME3D ( SENNUM ) = TRIM( NR_WDEP( SPC )( 1:12 ) ) //'_'// SEN_PAR( NP )
                     UNITS3D ( SENNUM ) = 'kg/hectare'
                     S_WETDEPSPC ( SENNUM ) = VNAME3D ( SENNUM )
                  END DO
               END DO

C...define wet deposition output descriptors for tracer species
               SPC = 0
               STRT = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + 1
               FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP
               DO VAR = STRT, FINI
                  SPC = SPC + 1
                  DO NP = 1, NPMAX
                     SENNUM = NPMAX * ( N_GC_WDEP + N_AE_WDEP + N_NR_WDEP ) + ( SPC - 1 ) * NPMAX + NP
                     VNAME3D ( SENNUM ) = TRIM( TR_WDEP( SPC )( 1:12 ) ) //'_'// SEN_PAR( NP )
                     UNITS3D ( SENNUM ) = 'kg/hectare'
                     S_WETDEPSPC ( SENNUM ) = VNAME3D ( SENNUM )
                  END DO
               END DO

C...define output descriptor for hydrogen concentration
               VAR = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP
               DO NP = 1, NPMAX
                  SENNUM = NPMAX * VAR + NP
                  VNAME3D ( SENNUM ) = 'HPLUS_' // SEN_PAR( NP )
                  UNITS3D ( SENNUM ) = 'kg/hectare'
                  S_WETDEPSPC ( SENNUM ) = VNAME3D ( SENNUM )
               END DO

C...initialize the file descriptors
               FDESC3D = ' '
               FDESC3D( 1 ) = 'Sensitivity of hourly 1-layer '
     &                        // 'cross-point wet deposition '
     &                        // 'from all cloud types'
C...try to open the output file new
               IF ( .NOT. OPEN3( CTM_SWETDEP_1, FSNEW3, PNAME ) ) THEN
                  XMSG = 'Could not create '// CTM_SWETDEP_1 // ' file'
                  CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
               END IF

           END IF   ! open CTM_SWETDEP_1 read/write
#endif

!...open the diagnostic file if user asked for it

           IF ( CLD_DIAG ) THEN

!...Try to open existing file for update
              WRITE( LOGDEV, * )
              IF ( .NOT. OPEN3( CTM_WET_DEP_2, FSRDWR3, PNAME ) ) THEN

                 XMSG = 'Could not open ' // CTM_WET_DEP_2 // ' file for update - '
     &                  // 'try to open new'
                 CALL M3MESG( XMSG )

!...copy most of header description from
!...CTM_WET_DEP_1 and initialize depositions to zero.

!...get description from the other wet dep file

                 IF ( .NOT. DESC3( CTM_WET_DEP_1 ) ) THEN
                    XMSG = 'Could not get ' // CTM_WET_DEP_1 //' file description'
                    CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                 END IF

!...advance to time variable for the output file to the next hour

                 SDATE3D = JDATE
                 STIME3D = 10000 * ( JTIME / 10000 )     !  clear MMSS fields
                 TSTEP3D = TSTEP     
                 CALL NEXTIME ( SDATE3D, STIME3D, TSTEP3D ) !  on the next hour

                 NVARS3D = N_SPC_WDEP + 8 + 4

!...define output descriptor for pressure at the lifting condensation
!...level (LCL)

                 VAR = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP + 2
                 VTYPE3D( VAR ) = M3REAL
                 VNAME3D( VAR ) = 'PLCL'
                 UNITS3D( VAR ) = 'Pa'
                 VDESC3D( VAR ) = 'pressure at lifting condensation level, '
     &                            // 'or MISSING = -9999.9'

!...define output descriptor for cloud bottom layer number

                 VAR = VAR + 1
                 VTYPE3D( VAR ) = M3REAL
                 VNAME3D( VAR ) = 'CLOUD_BOTTOM'
                 UNITS3D( VAR ) = ''
                 VDESC3D( VAR ) = 'layer number containing bottom of cloud, or MISSING = -9999.9'

!...define output descriptor for precipitating cloud top layer number

                 VAR = VAR + 1
                 VTYPE3D( VAR ) = M3REAL
                 VNAME3D( VAR ) = 'PR_CLOUDTOP'
                 UNITS3D( VAR ) = ''
                 VDESC3D( VAR ) = 'layer number containing top of PR cloud, or MISSING = -9999.9'

!...define output descriptor for non-precipitating cloud top layer number

                 VAR = VAR + 1
                 VTYPE3D( VAR ) = M3REAL
                 VNAME3D( VAR ) = 'NP_CLOUDTOP'
                 UNITS3D( VAR ) = ''
                 VDESC3D( VAR ) = 'layer number containing top of NP cloud, or MISSING = -9999.9'

!...define output descriptor for rain event flag

                 VAR = VAR + 1
                 VTYPE3D( VAR ) = M3REAL
                 VNAME3D( VAR ) = 'RAIN_FLAG'
                 UNITS3D( VAR ) = ''
                 VDESC3D( VAR ) = 'Rain-event flag (values: 1,0)'
 
!...define output descriptor for precitating cloud fraction

                 VAR = VAR + 1
                 VTYPE3D( VAR ) = M3REAL
                 VNAME3D( VAR ) = 'PR_CLDFRAC'
                 UNITS3D( VAR ) = '1'
                 VDESC3D( VAR ) = 'Precipitating cloud fraction'

!...define output descriptor for non-precipitating cloud fraction

                 VAR = VAR + 1
                 VTYPE3D( VAR ) = M3REAL
                 VNAME3D( VAR ) = 'NP_CLDFRAC'
                 UNITS3D( VAR ) = '1'
                 VDESC3D( VAR ) = 'Non-precipitating cloud fraction'

!...define output descriptor for precipitating cloud transmissivity

                 VAR = VAR + 1
                 VTYPE3D( VAR ) = M3REAL
                 VNAME3D( VAR ) = 'PR_TRANS'
                 UNITS3D( VAR ) = '1'
                 VDESC3D( VAR ) = 'Precipitating cloud transmissivity'

!...define output descriptor for non-precipitating cloud transmissivity

                 VAR = VAR + 1
                 VTYPE3D( VAR ) = M3REAL
                 VNAME3D( VAR ) = 'NP_TRANS'
                 UNITS3D( VAR ) = '1'
                 VDESC3D( VAR ) = 'Non-precipitating cloud transmissivity'

!...define output descriptor for resolved cloud transmissivity

                 VAR = VAR + 1
                 VTYPE3D( VAR ) = M3REAL
                 VNAME3D( VAR ) = 'RES_TRANS'
                 UNITS3D( VAR ) = '1'
                 VDESC3D( VAR ) = 'Resolved cloud transmissivity'

!...define output descriptor for overall cloud transmissivity

                 VAR = VAR + 1
                 VTYPE3D( VAR ) = M3REAL
                 VNAME3D( VAR ) = 'CLD_TRANS'
                 UNITS3D( VAR ) = '1'
                 VDESC3D( VAR ) = 'Total cloud transmissivity'

!...initialize the file descriptors

                 FDESC3D( 1 ) = 'hourly 1-layer cross-point wet deposition '
     &                           // 'from sub-grid clouds and cloud data'
                 DO L = 2 , MXDESC3
                    FDESC3D( L ) = ' '
                 END DO

!...try to open the diagnostic output file

                 IF ( .NOT. OPEN3( CTM_WET_DEP_2, FSNEW3, PNAME ) ) THEN
                    XMSG = 'Could not create '// CTM_WET_DEP_2 // ' file'
                    CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
                 END IF

              END IF   ! open CTM_WET_DEP_2 read/write

           END IF   ! CLD_DIAG

        END IF   ! IO_PE_INCLUSIVE
#endif

      END SUBROUTINE OPWDEP
