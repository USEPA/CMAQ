
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

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE TMPBEIS( JDATE, JTIME, TSTEP, BEIS_MAP, EMISL, L_DESID_DIAG )

C-----------------------------------------------------------------------
 
C Description:
C   Computes hourly time stepped gridded biogenic emissions using 
C   normalized gridded emissions from Normbeis3 (3.12) and postprocessed MM5
C   meteorology.
 
C Preconditions:
C   Postprocessed MM5 meteorology that contains temperature, 
C   solar radiation, and pressure data. 
C   Normalized gridded emissions BEIS_NORM_EMIS from Normbeis 
 
C Subroutines and Functions Called:
C   BEIS, HRNO,  
C   CURRSTEP, HHMMSS, UPCASE, CHECKMEM, M3EXIT, M3MSG2,
C   WRITE3, OPEN3, CLOSE3, NEXTIME, TIME2SEC
 
C Revision History:
C   3/01: Prototype by Jeff Vukovich
C         Tested only on 36km Lambert domain 
C         Summer/winter switch file option not tested
C   8/04: Updated for BEIS v3.12
C  10/06: J.Young - CMAQ inline
C                 - assumes first call is scenario start date/time
C                 - optional integral average mass diagnostic emissions file
C  10/09: J.Young - intel compiler problem; move last EMISS calculation as
C                   separate assignment
C  01/10: J.Young - move soil NO data and required input data processing to hrno
C  01/10: D. Wong - Eliminate potential race condition at the MYPE = 0
C                   to open the diagnostic file. Eliminate the BARRIER
C                   and the CLOSE3 and OPEN3.
C  02/11: S.Roselle-Replaced I/O API include files with UTILIO_DEFN
C  05/11: D.Wong-incorporated twoway model implementation
C  07/14: J.Bash-Added meteorological variables needed for leaf temperature
C                calculations.
C  08/14: J.Bash-Added backwards compatibility for earlier versions of MCIP
C  07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C  12 Aug 15 D.Wong: Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O implementation
C  07 May 18 D. Schwede: Removed call to CZANGLE. COSZEN now calculated in ASX_DATA_MOD
C  01 Feb 19 D. Wong: Implemented centralized I/O approach, removed all MY_N clauses
C-----------------------------------------------------------------------
C Modified from:
 
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id: tmpbeis.F,v 1.6 2011/10/21 16:10:18 yoj Exp $
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C smoke@unc.edu
C Pathname: $Source: /project/yoj/arc/CCTM/src/biog/beis3/tmpbeis.F,v $
C Last updated: $Date: 2011/10/21 16:10:18 $ 
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
      USE BIOG_EMIS             ! beis
      USE centralized_io_module, only : AVGEMIS, interpolate_var
      Use ASX_DATA_MOD, only: MET_DATA

#ifdef mpas
      USE util_module, only : hhmmss, time2sec
#else
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif

      IMPLICIT NONE

C Includes:
        
C Arguments:
      INTEGER, INTENT(  IN ) :: JDATE   ! current simulation date (YYYYDDD)
      INTEGER, INTENT(  IN ) :: JTIME   ! current simulation time (HHMMSS)
      INTEGER, INTENT(  IN ) :: TSTEP( 3 )      ! timestep vector
      INTEGER, INTENT(  IN ) :: BEIS_MAP( : )   ! mapping from beis3 to model species
      REAL,    INTENT( OUT ) :: EMISL( :,:,: )  ! emissions in moles/sec
      LOGICAL, INTENT(  IN ) :: L_DESID_DIAG ! flag determining whether or not DESID
                                             !   is in diagnostic mode              


C External Functions
      LOGICAL,         EXTERNAL :: CHKGRID

C Parameters:
      REAL,    PARAMETER :: HR2SEC = 1.0 / 3600.0

C Gridded meteorology data:
      REAL, ALLOCATABLE, SAVE :: FSEAS( :,: )     ! Seasonal function

C Gridded normalized emissions:
      REAL, ALLOCATABLE, SAVE :: SEMIS( :,:,: )    ! Normailized Beis emissions

C Mole and mass factors:
      REAL MLFC, MSFC, EMPL

C Diagnostic file avg factor
      REAL DIVFAC

C BEIS3 internal, output species:
      REAL,    ALLOCATABLE, SAVE :: EMPOL( :,:,: )  ! emissions of biogenic emissions (mechanism specific)
      REAL,    ALLOCATABLE, SAVE :: BIPOL( :,:,: )  ! emissions of beis emissions (all beis BVOCs)
      REAL,    ALLOCATABLE, SAVE :: NOPOL( :,: )    ! emissions of NO
      REAL,    ALLOCATABLE, SAVE :: EMISS( :,:,: )  ! emissions in g/s

C Logical names and unit numbers:
            
      CHARACTER( 16 ), SAVE :: SNAME   ! logical name for diagnostic emis output (mass)
      CHARACTER( 16 ), SAVE :: NNAME   ! logical name for normalized-emissions input
      CHARACTER( 16 ), SAVE :: GNAME   ! logical name for GRID_CRO_2D
      CHARACTER( 16 ), SAVE :: BNAME   ! logical name for frost switch input
      CHARACTER( 16 ), SAVE :: MNAME   ! logical name for MET_CRO_2D

C Other variables:
      INTEGER, SAVE :: IHR        ! current simulation hour
      INTEGER          IOS        ! IO or memory allocation status
      INTEGER, SAVE :: LDATE      ! previous simulation date
      INTEGER, SAVE :: MDATE      ! test date to update beis diag avg
      INTEGER, SAVE :: MTIME      ! test time to update beis diag avg
      INTEGER, SAVE :: NSTEPS     ! run duration
      INTEGER, SAVE :: WSTEP  = 0 ! local write counter
      INTEGER          PARTYPE    ! method number to calculate PAR
      INTEGER          C, R, I, J, K, S  ! loop counters and subscripts

      LOGICAL          OK        ! check flag
      LOGICAL, SAVE :: LASTTIC                ! true: last sync step this output tstep
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      CHARACTER( 256 ) :: MESG       ! message buffer
      CHARACTER( 256 ) :: EQNAME     ! equivalent filename
      CHARACTER(  16 ) :: VAR        ! variable name
      CHARACTER(  16 ) :: PNAME = 'TMPBEIS'   ! prodedure name

#ifdef verbose_tmpbeis
      REAL,    ALLOCATABLE, SAVE :: EMISX( : )
      INTEGER, ALLOCATABLE, SAVE :: CX( : ), RX( : )
#endif

      INTERFACE
         SUBROUTINE BEIS( JDATE, JTIME, NX, NY, MSPCS, SEMIS, BIPOL )
            INTEGER, INTENT( IN ) :: JDATE
            INTEGER, INTENT( IN ) :: JTIME
            INTEGER, INTENT( IN ) :: NX
            INTEGER, INTENT( IN ) :: NY
            INTEGER, INTENT( IN ) :: MSPCS
            REAL,    INTENT( IN ) :: SEMIS ( :,:,: )
            REAL,    INTENT( OUT ) :: BIPOL( :,:,: )
         END SUBROUTINE BEIS
         SUBROUTINE HRNO( JDATE, JTIME, TSTEP, NOPOL, L_DESID_DIAG )
            INTEGER, INTENT( IN )  :: JDATE
            INTEGER, INTENT( IN )  :: JTIME
            INTEGER, INTENT( IN )  :: TSTEP( 3 )
            REAL,    INTENT( OUT ) :: NOPOL( :,: )
            LOGICAL, INTENT( IN )  :: L_DESID_DIAG
         END SUBROUTINE HRNO
      END INTERFACE

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

C Determine last timestamp
         NSTEPS = RUNLEN / TSTEP( 1 )           ! initscen guarantees divisibility

C Allocate seasonality array
         ALLOCATE( FSEAS( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'FSEAS', PNAME )
         FSEAS = 0   ! array

#ifndef mpas
C Ensure met data time and grid for this run match beis emissions data files
         MNAME = PROMPTMFILE( 
     &           'Enter name for gridded met input file',
     &           FSREAD3, 'MET_CRO_2D', PNAME )

C Get description of met file 
         IF ( .NOT. DESC3( MNAME ) ) THEN
            MESG = 'Could not get description of file "'
     &           // TRIM( MNAME ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF

C Check that grid description matches BGRD file
         IF ( .NOT. CHKGRID( MNAME ) ) THEN
            MESG = 'Grid in file "' // TRIM( MNAME )
     &           // '" does not match previously set grid.'
            CALL M3WARN( PNAME, 0, 0, MESG )
         END IF

         IF ( BEMIS_DIAG ) THEN
C Build description for, and open beis diagnostic file
            MDATE = STDATE; MTIME = STTIME
            CALL NEXTIME( MDATE, MTIME, TSTEP( 1 ) )

            GDNAM3D = GRID_NAME  ! from HGRD_DEFN
            SDATE3D = MDATE
            STIME3D = MTIME
            TSTEP3D = TSTEP( 1 )
            NCOLS3D = GL_NCOLS
            NROWS3D = GL_NROWS
            NLAYS3D =     1
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

            DO I = 1, NLAYS3D + 1
               VGLVS3D( I ) = VGLVS_GD( I )
            END DO
  

            J = 0
            DO I = 1, MSPCS
               IF ( BEIS_MAP( I ) .GT. 0 ) THEN
                  J = J + 1
                  VNAME3D( J ) = EMSPC( I )
                  VDESC3D( J ) = 'biogenic emissions of the indicated species'
                  VTYPE3D( J ) = M3REAL
                  UNITS3D( J ) = 'gm s-1'
               END IF
            END DO

            NVARS3D = J
            FDESC3D = ' '   ! array

            FDESC3D( 1 ) = 'Gridded biogenic emissions from CMAQ-BEIS3'
            FDESC3D( 2 ) = '/from/ ' // PNAME
            FDESC3D( 3 ) = '/Version/ CMAQ'

C Open mass output file (tons/hour)

            SNAME = 'B3GTS_S'
            CALL SUBST_BARRIER
            IF ( IO_PE_INCLUSIVE ) THEN
               IF ( .NOT. OPEN3( SNAME, FSNEW3, PNAME ) ) THEN
                  MESG = 'Could not open "' // TRIM( SNAME ) // '" file'
                  CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT1 )
               END IF
            END IF

         END IF
#endif

C Allocate memory for arrays
         
         ALLOCATE( EMPOL( NCOLS,NROWS,NSEF ), STAT=IOS )
         CALL CHECKMEM( IOS, 'EMPOL', PNAME )

         ALLOCATE( BIPOL( NCOLS,NROWS,NSEF-1 ), STAT=IOS )
         CALL CHECKMEM( IOS, 'BIPOL', PNAME )

         ALLOCATE( NOPOL( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'NOPOL', PNAME )

         IF ( BEMIS_DIAG ) THEN
            ALLOCATE( EMISS( NCOLS,NROWS,MSPCS ), STAT=IOS )
            CALL CHECKMEM( IOS, 'EMISS', PNAME )
         END IF

         ALLOCATE( SEMIS( NCOLS,NROWS,NSEF-1 ), STAT=IOS )
         CALL CHECKMEM( IOS, 'SEMIS', PNAME )

         LDATE = 0

#ifdef verbose_tmpbeis
         ALLOCATE( EMISX( MSPCS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'EMISX', PNAME )
         ALLOCATE( CX( MSPCS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'CX', PNAME )
         ALLOCATE( RX( MSPCS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'RX', PNAME )
#endif

#ifndef mpas
         IF ( BEMIS_DIAG ) THEN
            EMISS = 0.0   ! array
#ifdef parallel_io
            IF ( .NOT. IO_PE_INCLUSIVE ) THEN
               IF ( .NOT. OPEN3( SNAME, FSREAD3, PNAME ) ) THEN
                  MESG = 'Could not open ' // TRIM(SNAME)
                  CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
               END IF
            END IF
#endif
         END IF
#endif

      END IF   ! FIRSTIME

C Loop thru the number of time steps (hourly)

      EMISL = 0.0   ! array
      EMPOL = 0.0   ! array
#ifdef verbose_tmpbeis
      EMISX = 0.0   ! array
#endif

      IF ( JDATE .NE. LDATE ) THEN

         CALL WRDAYMSG( JDATE, MESG )               

C calculate fseas based on deep soil temperature following WRF 3.8.1 PX 
         Where( Met_Data%SOIT2 .Lt. 290.0 .And. Met_Data%SOIT2 .Gt. 282.0 )
            FSEAS = 1.0 - 0.015625 * ( 290.0 - Met_Data%SOIT2 ) ** 2
         Elsewhere( Met_Data%SOIT2 .Ge. 290.0 )
            FSEAS = 1.0
         Elsewhere
            FSEAS = 0.0
         End where

         DO S = 1, NSEF-1 ! normalized emission factors
            SEMIS( :,:,S ) =         FSEAS   * AVGEMIS( :,:,S,NSUMMER ) + ! Deciduous fraction
     &                       ( 1.0 - FSEAS ) * AVGEMIS( :,:,S,NWINTER )   ! Evergreen fraction
         END DO

      END IF   ! if new day

      WRITE( MESG,94030 ) HHMMSS( JTIME )
      CALL M3MSG2( MESG )

#ifndef mpas
C repopulate the met_cro_2d variable attributes
      IF ( .NOT. DESC3( MNAME ) ) THEN
         MESG = 'Could not get description of file "'
     &           // TRIM( MNAME ) // '"'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
      END IF
#endif

C Calculate temporal non-speciated beis emissions
      CALL BEIS( JDATE, JTIME, NCOLS, NROWS, MSPCS, SEMIS, BIPOL )

C Calculate temporal non-speciated soil NO emissions
      CALL HRNO( JDATE, JTIME, TSTEP, NOPOL, L_DESID_DIAG )

      EMPOL( :,:,1:NSEF-1 ) = BIPOL
      EMPOL( :,:,NSEF )     = NOPOL

C Speciate emissions
      DO K = 1, NSEF
         DO R = 1, NROWS
            DO C = 1, NCOLS
               EMPL = EMPOL( C,R,K ) * HR2SEC
               IF ( EMPL .GT. 0.0 ) 
     &            EMISL( :,C,R ) = EMISL( :,C,R ) + EMPL * MLFAC( :,K )
            END DO
         END DO
      END DO

#ifdef verbose_tmpbeis
      WRITE( LOGDEV,* ) ' '
      WRITE( LOGDEV,* ) '       from TMPBEIS'
      WRITE( LOGDEV,* ) '    Spc   EMISL  MaxC MaxR'
      DO S = 1, MSPCS
         WRITE( LOGDEV,'( 4X, I3, F10.5, 2I4 )' ) S, EMISX( S ), CX( S ), RX( S )
      END DO

      EMISX = 0.0    ! array
      WRITE( LOGDEV,* ) ' '
      WRITE( LOGDEV,* ) '                   from TMPBEIS'
      WRITE( LOGDEV,* ) '    Spc SpcName  Pol  MLFAC     EMPOL     EMISL( Spc,32,7 )'
      C = 32; R = 7
      DO K = 1, NSEF
         EMPL = EMPOL( C,R,K ) * HR2SEC
         IF ( EMPL .LE. 0.0 ) CYCLE
         DO S = 1, MSPCS
            IF ( BEIS_MAP( S ) .GT. 0 ) THEN
               MLFC = MLFAC( S,K )
               EMISX( S ) = EMISX( S ) + EMPL * MLFC
               IF ( MLFC .NE. 0.0 )
     &            WRITE( LOGDEV,'( 4X, I3, 2X, A6, 2X, I3, 1X, 3( 1PE10.3 ) )' )
     &                              S, EMSPC( S ), K,  MLFC, EMPL, EMISX( S )
            END IF
         END DO
      END DO
#endif

      IF ( BEMIS_DIAG .AND. .NOT. L_DESID_DIAG ) THEN
         WSTEP   =   WSTEP + TIME2SEC( TSTEP( 2 ) )
         LASTTIC = ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) )

#ifndef mpas
         DO K = 1, NSEF
            DO S = 1, MSPCS
! Unit conversion for the emission and fraction of the sync time step to the output time step
                  MSFC = MSFAC( S,K ) * FLOAT( TIME2SEC( TSTEP( 2 ) ) ) / FLOAT( TIME2SEC( TSTEP( 1 ) ) )
! Add the emission for the model output time step
                  EMISS( :,:,S ) = EMISS( :,:,S ) + EMPOL( :,:,K ) * MSFC
            END DO
         END DO
         IF ( LASTTIC ) THEN
            EMISS = EMISS * HR2SEC   ! convert from gm/h to gm/s
            DO S = 1,MSPCS
               IF ( BEIS_MAP( S ) .GT. 0 ) THEN
                  IF ( .NOT. WRITE3( SNAME, EMSPC( S ), MDATE, MTIME, EMISS(:,:,S) ) ) THEN
                     MESG = 'Could not write to output file "' // TRIM( SNAME ) // '"'
                     CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
                  END IF
               END IF
            END DO
            WRITE( LOGDEV,94040 )
     &            'Timestep written to', SNAME,
     &            'for date and time', MDATE, MTIME
            EMISS = 0.0   ! array
            WSTEP = 0
            CALL NEXTIME( MDATE, MTIME, TSTEP( 1 ) )
         END IF
      END IF
#endif

C Save date
      LDATE = JDATE

      RETURN

C------------------  FORMAT  STATEMENTS   ------------------------------

94010 FORMAT( 10( A, ':', I8, ':', 1X ) )
94020 FORMAT( 1X, I7, ":", I6.7 )
94030 FORMAT( 5X, 'Temporal BEIS at time ', A8 )
94040 FORMAT( /5X, 3( A, :, 1X ), I8, ":", I6.6 )

      END SUBROUTINE TMPBEIS  

