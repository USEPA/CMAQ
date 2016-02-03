      SUBROUTINE OUTPUT_DGN( JDATE, JTIME, TSTEP )
!******************************************************************************
! Developed by Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Output diagnostic (process analysis) info to netcdf
!            files for JDATE and JTIME
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!    April 2005: Updated for CMAQ-APT-PM, PKK, AER
!    April 2011: Updated for CMAQ 5.0, PK, ENVIRON
!******************************************************************************
 
! --- MODULES AND INCLUDES

      USE HOST_INC
      USE MULTCOMP_INC
      USE DIAGNOSTICS

      IMPLICIT NONE

! --- ARGUMENTS
 
      INTEGER ::      JDATE     !  current model date, coded YYYYDDD
      INTEGER ::      JTIME     !  current model time, coded HHMMSS
      INTEGER, DIMENSION( 3 ) :: TSTEP   ! time step vector (HHMMSS)
                                         ! TSTEP(1) = local output step
                                         ! TSTEP(2) = sciproc sync. step (chem)
                                         ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                         !            step and wrf/cmaq call frequency

! --- LOCALS

      INTEGER ::      MDATE     ! Date for output (YYMMDD)
      INTEGER ::      MTIME     ! Time for output (HHMMSS)
      INTEGER ::      NFL       ! Loop index for files
      INTEGER ::      SPC       ! Loop index for species
      INTEGER ::      NOUT      ! No. of output variables

! ... Diagnostic fields
      REAL, DIMENSION(  MAX_MC, NUMFLS ) ::  DIAGOUT

! ... Names of variables saved to diagnostic files
      CHARACTER( 16 ), DIMENSION( MAX_MC ), SAVE :: VNM

      CHARACTER( 16 ) :: PNAME = 'OUTPUT_DGN'
      CHARACTER( 120 ) :: XMSG = ' '

! ... External Functions (not already declared by IODECL3.EXT):

!     INTEGER, EXTERNAL :: SEC2TIME, TIME2SEC
!
! Above now from host_inc, which includes m3utilio 

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      IF ( FIRSTIME ) THEN

!  set up variable names

         DO SPC = 1, nspectot
            VNM( SPC ) = species(spc)%name
         END DO
         VNM( nspectot + 1 ) = 'TRAC'

         FIRSTIME = .FALSE.

      END IF

!...... Assign diagnostic variables
      DO SPC = 1, nspectot + 1
         DIAGOUT( SPC, 1 ) = EMISS_CURRENT( SPC )
         DIAGOUT( SPC, 2 ) = STATICS_CURRENT( SPC )
         DIAGOUT( SPC, 3 ) = BNDRY_CURRENT( SPC )
         DIAGOUT( SPC, 4 ) = DDEPOS_CURRENT( SPC )
         DIAGOUT( SPC, 5 ) = WDEPOS_CURRENT( SPC )
         DIAGOUT( SPC, 6 ) = CHEM_CURRENT( SPC )
         DIAGOUT( SPC, 7 ) = ACTIVE( SPC )
         DIAGOUT( SPC, 8 ) = TRANS_CURRENT( SPC )
      END DO

! ... write diagnostics to netcdf files
! ... update date and time to output step
      MDATE = JDATE
      MTIME = JTIME
      CALL NEXTIME( MDATE, MTIME, TSTEP( 2 ) )
      NOUT = nspectot + 1
      DO NFL = 1, NUMFLS
         DO SPC = 1, NOUT
            IF ( .NOT. WRITE3( DGNFNAME( NFL ), VNM( SPC ), MDATE, MTIME,
     &                         DIAGOUT( SPC, NFL ) ) ) THEN

               XMSG = 'Could not write "' // VNM( SPC ) // 
     &                '" to file "' // DGNFNAME( NFL ) // '".'

               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

            END IF

         END DO

         IF ( NFL == NUMFLS ) THEN

            IF ( .NOT. WRITE3( DGNFNAME( NFL ), 'NDUMP', MDATE, MTIME,
     &                         NDUMP_CURRENT ) ) THEN

               XMSG = 'Could not write NDUMP' // VNM( SPC ) // 
     &                ' to file "' // DGNFNAME( NFL ) // '".'

               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

            END IF

         END IF

      END DO

      RETURN
      END
