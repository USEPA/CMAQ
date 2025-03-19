
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
      SUBROUTINE WR_INIT ( CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function:
C   Create the IO/API netCDF header and open the output CONC file

C Revision history:
C   Dec 15 D.Wong: Created for writing initial data to CONC file
C   Apr 16 D.Wong: Added INTEN attribute for arguments JDATE, JTIME, and TSTEP
C                  so they match with the interface block in INITSCEN.F
C   Feb 19 D.Wong: removed all MY_N clauses
C   June 19 F. Sidi: Replaced BADVAL3 for TA, RH, & PRES with MET DATA
C   Sept 19 F. Sidi: Replaced L_CONC_WVEL(depreciated) with W_VEL 
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE WVEL_DEFN             ! derived vertical velocity component
      USE STD_CONC              ! standard CONC
      USE UTILIO_DEFN
#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif
      USE CENTRALIZED_IO_MODULE
      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID    ! file name parameters
      
C Arguments:

      REAL, POINTER :: CGRID( :,:,:,: )  ! for initial CONC
      INTEGER, INTENT( IN ) :: JDATE     ! starting date (YYYYDDD)
      INTEGER, INTENT( IN ) :: JTIME     ! starting time (HHMMSS)
      INTEGER, INTENT( IN ) :: TSTEP     ! output timestep (HHMMSS)

C Local Variables:

      REAL, ALLOCATABLE :: DBUFF( :,:,: ), DBUFFP( :, :, :),
     &                     DBUFFTA( :, :, :), DBUFFQV(:, :, :)

      INTEGER      ALLOCSTAT

      CHARACTER( 16 ) :: PNAME = 'OPCONC'
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 28 ) :: SSTR = ' species saved to CONC file:'

C environment variable for no. of layers from bottom to save on CONC file
      CHARACTER( 16 ) :: NLAYS_CONC = 'NLAYS_CONC'

C  environment variable description
      CHARACTER( 80 ) :: VARDESC

      INTEGER      K, KD, L, SPC, V   ! loop counters
      INTEGER      STRT, FINI         ! loop counters
      INTEGER      INDX

      LOGICAL, EXTERNAL :: FLUSH3

C-----------------------------------------------------------------------

#ifndef mpas
#ifdef parallel_io
      CALL SUBST_BARRIER
      IF ( .NOT. IO_PE_INCLUSIVE ) THEN
         IF ( .NOT. OPEN3( CTM_CONC_1, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF
#endif

C write the initial concentrations as step 0 on the conc file
C (inital data assumed to be in correct output units)

      ALLOCATE ( DBUFF( NCOLS,NROWS,C_NLAYS ), DBUFFP( NCOLS, NROWS,
     &           C_NLAYS), DBUFFTA( NCOLS, NROWS, C_NLAYS), 
     &           DBUFFQV( NCOLS, NROWS, C_NLAYS), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating DBUFF(s)'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      WRITE( LOGDEV,* ) ' '

      V = 0

      IF ( N_C_GC_SPC .NE. 0 ) WRITE( LOGDEV,'( /5X, "Gas Chem", A )' ) SSTR

      DO SPC = 1, N_C_GC_SPC
         V = V + 1
         INDX = CONC_MAP( V )

         DBUFF = CGRID( 1:NCOLS,1:NROWS,CONC_BLEV:CONC_ELEV,INDX )

         IF ( .NOT. WRITE3( CTM_CONC_1, C_GC_SPC( SPC ),
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write ' //
     &             TRIM( C_GC_SPC( SPC ) ) //
     &             ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         WRITE( LOGDEV,'( 5X, I4, " (", I3, ") ", A )' )
     &                SPC, V, TRIM( C_GC_SPC( SPC ) )

      END DO

      IF ( N_C_AE_SPC .NE. 0 ) WRITE( LOGDEV,'( /5X, "Aerosol", A )' ) SSTR

      DO SPC = 1, N_C_AE_SPC
         V = V + 1
         INDX = CONC_MAP( V )

         DBUFF = CGRID( 1:NCOLS,1:NROWS,CONC_BLEV:CONC_ELEV,INDX )

         IF ( .NOT. WRITE3( CTM_CONC_1, C_AE_SPC( SPC ),
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write ' //
     &             TRIM( C_AE_SPC( SPC ) ) //
     &             ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         WRITE( LOGDEV,'( 5X, I4, " (", I3, ") ", A )' )
     &                SPC, V, TRIM( C_AE_SPC( SPC ) )

      END DO

      IF ( N_C_NR_SPC .NE. 0 ) WRITE( LOGDEV,'( /5X, "Non-reactive", A )' ) SSTR

      DO SPC = 1, N_C_NR_SPC
         V = V + 1
         INDX = CONC_MAP( V )

         DBUFF = CGRID( 1:NCOLS,1:NROWS,CONC_BLEV:CONC_ELEV,INDX )

         IF ( .NOT. WRITE3( CTM_CONC_1, C_NR_SPC( SPC ),
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write ' //
     &             TRIM( C_NR_SPC( SPC ) ) //
     &             ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         WRITE( LOGDEV,'( 5X, I4, " (", I3, ") ", A )' )
     &                SPC, V, TRIM( C_NR_SPC( SPC ) )
      END DO

      IF ( N_C_TR_SPC .NE. 0 ) WRITE( LOGDEV,'( /5X, "Inert tracer", A )' ) SSTR

      DO SPC = 1, N_C_TR_SPC
         V = V + 1
         INDX = CONC_MAP( V )

         DBUFF = CGRID( 1:NCOLS,1:NROWS,CONC_BLEV:CONC_ELEV,INDX )

         IF ( .NOT. WRITE3( CTM_CONC_1, C_TR_SPC( SPC ),
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write ' //
     &             TRIM( C_TR_SPC( SPC ) ) //
     &             ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         WRITE( LOGDEV,'( 5X, I4, " (", I3, ") ", A )' )
     &                SPC, V, TRIM( C_TR_SPC( SPC ) )

      END DO
 
      IF ( W_VEL ) THEN   ! Vertical Velocity Calculated from Advection

         DBUFF = BADVAL3

         IF ( .NOT. WRITE3( CTM_CONC_1, 'W_VEL',
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write Vertical Velocity (W_VEL) to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF
 
       IF ( L_CONC_TA ) THEN   ! for Temperature
      
      CALL interpolate_var ('TA', JDATE, JTIME, DBUFFTA)
         
         IF ( .NOT. WRITE3( CTM_CONC_1, 'TA',
     &                      JDATE, JTIME, DBUFFTA ) ) THEN
            XMSG = 'Could not write Temperature to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF

      IF ( L_CONC_PRES ) THEN   ! for Pressure
      
      CALL interpolate_var ('PRES', JDATE, JTIME, DBUFFP)
      
         IF ( .NOT. WRITE3( CTM_CONC_1, 'PRES',
     &                      JDATE, JTIME, DBUFFP ) ) THEN
            XMSG = 'Could not write Pressure to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF


      IF ( L_CONC_RH ) THEN   ! for Relative Humidity

      CALL interpolate_var ('QV', JDATE, JTIME, DBUFFQV)

         DBUFF = DBUFFQV * DBUFFP / ( DBUFFQV + 0.622015 ) /
     &                 ( 610.94 * EXP( 17.625 * ( DBUFFTA - 273.15 ) / 
     &                                 ( DBUFFTA - 273.15 + 243.04 ) ) )
         DBUFF = MIN( 0.9999, MAX( 0.001, DBUFF) )

         IF ( .NOT. WRITE3( CTM_CONC_1, 'RH',
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write Relative Humidity to ' // CTM_CONC_1

            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF


      DEALLOCATE ( DBUFF, DBUFFP, DBUFFTA, DBUFFQV)

      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &  'Timestep written to', CTM_CONC_1,
     &  'for date and time', JDATE, JTIME
      WRITE( LOGDEV, '(  5X,  A, 1X, I8, ":", I6.6 )' )
     &  'from timestep on initial data files for date and time',
     &   JDATE, JTIME
#endif

      RETURN
      END SUBROUTINE WR_INIT
