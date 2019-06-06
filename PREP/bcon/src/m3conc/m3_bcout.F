
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
C $Header: /project/yoj/arc/BCON/src/m3conc/m3_bcout.F,v 1.2 2011/10/21 16:52:34 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE M3_BCOUT( LOGUNIT,
     &                     N_CTM_FLS,
     &                     SDATE, STIME, NSTEPS,
     &                     NCOLS_IN, NROWS_IN, NLAYS_IN, NSPCS_IN,
     &                     CTM_FL_NAME, INFL_SP_NAME,
     &                     VTYPE_IN, UNITS_IN, VDESC_IN )

C*************************************************************************
 
C  Function: Reads the input models-3 concentration file(s) and opens and
C            writes the output BC file
              
C  Preconditions: None
  
C  Key Subroutines/Functions Called:   
 
C  Revision History:
C    Prototype created by Jerry Gipson, January, 1998
C    Modified by JG May, 1999 to treat PinG concs
C    02/25/02 Steve Howard (Jeff Young) - dynamic allocation
C    01/05/05 J.Young: vert dyn alloc - Use VGRD_DEFN eliminate malloc calls
C    13 Jul 11 J.Young: Replaced I/O API include files with M3UTILIO and
C                       Namelist for species definitions
C    23 May 12 J.Young: Replaced BC_PARMS include file with an F90 module
C    14 Sep 18 S.Roselle: Removed species mapping
 
C*************************************************************************

      USE HGRD_DEFN   ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN   ! vertical layer specifications
      USE M3UTILIO    ! IOAPI module
      USE BC_PARMS    ! BCON parameters

      IMPLICIT NONE     

C Arguments: 
      INTEGER, INTENT( IN ) :: LOGUNIT           ! Unit number for output log
      INTEGER, INTENT( IN ) :: N_CTM_FLS         ! Number of input CTM files
      INTEGER, INTENT( IN ) :: SDATE             ! Date for BC Output
      INTEGER, INTENT( IN ) :: STIME             ! Time for BC output
      INTEGER, INTENT( IN ) :: NSTEPS            ! Run duration, as number of output time steps
      INTEGER, INTENT( IN ) :: NCOLS_IN          ! No. of columns in input conc file
      INTEGER, INTENT( IN ) :: NLAYS_IN          ! No. of layers in input conc file
      INTEGER, INTENT( IN ) :: NROWS_IN          ! No. of rows in input conc file
      INTEGER, INTENT( IN ) :: NSPCS_IN          ! Total No. of species in input conc file(s)
      CHARACTER( 16 ), INTENT( IN ) :: CTM_FL_NAME( : )   ! Name of CTM_CONC file
      CHARACTER( 16 ), INTENT( IN ) :: INFL_SP_NAME( : )  ! Name of input CTM species
      CHARACTER( 16 ), INTENT( IN ) :: UNITS_IN( : ) ! Units for CTM species
      CHARACTER( 80 ), INTENT( IN ) :: VDESC_IN( : ) ! Variable description for CTM species

      INTEGER, INTENT( IN ) :: VTYPE_IN( : ) ! variable type for CTM species

C Parameters:
      REAL, PARAMETER :: CMIN = 1.0E-30  ! Minimum output concentration

C External Functions: None
 
C Local Variables:
      CHARACTER( 16 ) :: PNAME = 'M3_BCOUT'  ! Procedure Name
      CHARACTER( 80 ) :: MSG              ! Log message
      CHARACTER( 16 ) :: VNAME            ! Species name on CTM conc file
      CHARACTER( 16 ), ALLOCATABLE :: BC_FNAME( : )  ! Logical names of BC Output file(s)

      INTEGER C             ! Column loop indices
      INTEGER FLN           ! BC output file number
      INTEGER ISP           ! Array indices for species
      INTEGER JDATE         ! Current date
      INTEGER JTIME         ! Current time
      INTEGER L             ! Layer loop index
      INTEGER N             ! Loop indices for species
      INTEGER NBND          ! Bndry cell loop index
      INTEGER NCELL         ! Boundary cell index
      INTEGER R             ! Row loop indices
      INTEGER STEP          ! Time step loop index
      INTEGER TSTEP         ! Time step on CTM file, hhmmss
      INTEGER ALLOCSTAT     ! Status returned from array allocation

      INTEGER, ALLOCATABLE :: COL_LOC( : )  ! Output BC col corresponding to
                                            ! a cell in the input CTM file
      INTEGER, ALLOCATABLE :: ROW_LOC( : )  ! Output BC row corresponding to
                                            ! a cell in the input CTM file

      LOGICAL LNEG                 ! Flag for negative concentration

      REAL    DMIN   ! Smallest distance between cell centers
      REAL    LAT    ! Latitude of center of cell for the BC output file
      REAL    LON    ! Longitude of center of cell for the BC output file
      REAL    X1     ! longitudenal distance between cell centers
      REAL    Y1     ! latitudinal distance between cell centers

      REAL, ALLOCATABLE :: LAT_OUT( : )    ! Lat of cells in BC output file
      REAL, ALLOCATABLE :: LON_OUT( : )    ! Lon of cells in BC output file

      REAL, ALLOCATABLE :: LAT_IN( :,: )   ! Lat of cells in CTM conc file
      REAL, ALLOCATABLE :: LON_IN( :,: )   ! Lon of cells in CTM conc file

      REAL, ALLOCATABLE :: COUT( :,: )     ! Substituted output BC conc
      REAL, ALLOCATABLE :: CONCIN( :,:,: ) ! Input concs
      REAL, ALLOCATABLE :: BCIN( :,:,: )   ! Horizontally set BCs
      REAL, ALLOCATABLE :: BCVI( :,:,: )   ! Vertically interpolated BCs

      INTERFACE

         SUBROUTINE LAT_LON ( COL, ROW, GDTYP, XORIG, YORIG, XCELL, YCELL,
     &                        XCENT, YCENT, P_ALP, P_BET, P_GAM, LAT, LON )
            INTEGER, INTENT( IN ) :: GDTYP
            INTEGER, INTENT( IN ) :: COL
            INTEGER, INTENT( IN ) :: ROW
            REAL( 8 ), INTENT( IN ) :: P_ALP
            REAL( 8 ), INTENT( IN ) :: P_BET
            REAL( 8 ), INTENT( IN ) :: P_GAM
            REAL( 8 ), INTENT( IN ) :: XCELL
            REAL( 8 ), INTENT( IN ) :: XCENT
            REAL( 8 ), INTENT( IN ) :: XORIG
            REAL( 8 ), INTENT( IN ) :: YCELL
            REAL( 8 ), INTENT( IN ) :: YCENT
            REAL( 8 ), INTENT( IN ) :: YORIG
            REAL, INTENT( OUT ) :: LAT
            REAL, INTENT( OUT ) :: LON
         END SUBROUTINE LAT_LON

         SUBROUTINE M3_VINTERP ( LOGUNIT, JDATE, JTIME,
     &                           NCOLS_IN, NROWS_IN, NLAYS_IN, NSPCS_IN,
     &                           COL_LOC, ROW_LOC,
     &                           BCIN, BCVI, CTM_FL_NAME )
            INTEGER, INTENT( IN ) :: LOGUNIT
            INTEGER, INTENT( IN ) :: JDATE
            INTEGER, INTENT( IN ) :: JTIME
            INTEGER, INTENT( IN ) :: NCOLS_IN
            INTEGER, INTENT( IN ) :: NROWS_IN
            INTEGER, INTENT( IN ) :: NLAYS_IN
            INTEGER, INTENT( IN ) :: NSPCS_IN
            INTEGER, INTENT( IN ) :: COL_LOC( : )
            INTEGER, INTENT( IN ) :: ROW_LOC( : )
            REAL, INTENT( IN )    :: BCIN( :,:,: )
            REAL, INTENT( OUT )   :: BCVI( :,:,: )
            CHARACTER( 16 ), INTENT( IN ) :: CTM_FL_NAME( : )
         END SUBROUTINE M3_VINTERP

         SUBROUTINE OPN_BC_FILE ( LOGUNIT, SDATE, STIME, TSTEP, NSPCS_OUT,
     &                            SPNAME_OUT, VTYPE_OUT, UNITS_OUT,
     &                            VDESC_OUT, BC_FNAME, RINDX )
            CHARACTER( 16 ), INTENT( OUT ) :: BC_FNAME( : )
            CHARACTER( 16 ), INTENT( IN )  :: SPNAME_OUT( : )
            CHARACTER( 16 ), INTENT( IN )  :: UNITS_OUT( : )
            CHARACTER( 80 ), INTENT( IN )  :: VDESC_OUT( : )
            INTEGER, INTENT( IN ) :: LOGUNIT
            INTEGER, INTENT( IN ) :: NSPCS_OUT
            INTEGER, INTENT( IN ) :: RINDX
            INTEGER, INTENT( IN ) :: SDATE
            INTEGER, INTENT( IN ) :: STIME
            INTEGER, INTENT( IN ) :: TSTEP
            INTEGER, INTENT( IN ) :: VTYPE_OUT( : )
         END SUBROUTINE OPN_BC_FILE

      END INTERFACE

C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  allocate arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ALLOCATE( BC_FNAME( MXCTMS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating BC_FNAME'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( COL_LOC( NBNDY ), ROW_LOC( NBNDY ),
     &          STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating COL_LOC, ROW_LOC'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( LAT_OUT( NBNDY ), LON_OUT( NBNDY ),
     &          STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating LAT_OUT, LON_OUT'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( LAT_IN( NCOLS_IN,NROWS_IN ), LON_IN( NCOLS_IN,NROWS_IN ),
     &          STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating LAT_IN, LON_IN'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( COUT( NBNDY,NLAYS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating COUT'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( CONCIN( NCOLS_IN,NROWS_IN,NLAYS_IN ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating CONCIN'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( BCIN( NBNDY,NLAYS_IN,NSPCS_IN ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating BCIN'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( BCVI( NBNDY,NLAYS,NSPCS_IN ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating BCVI'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      write( logunit,* ) ' '
      write( logunit,* ) '    NCOLS_IN: ', NCOLS_IN
      write( logunit,* ) '    NROWS_IN: ', NROWS_IN
      write( logunit,* ) '    NLAYS_IN: ', NLAYS_IN
      write( logunit,* ) '    NSPCS_IN: ', NSPCS_IN
      write( logunit,* ) '    NBNDY:    ', NBNDY
      write( logunit,* ) '    NLAYS:    ', NLAYS
      write( logunit,* ) ' '

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute the lat and lon of the center of each input cell
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. DESC3( CTM_FL_NAME( 1 ) ) ) THEN
          MSG = 'Could not read DESC of  ' // CTM_FL_NAME( 1 )
     &       // ' file'
         CALL M3EXIT ( PNAME, SDATE, STIME, MSG, XSTAT2 )
      END IF

      DO C = 1, NCOLS_IN
         DO R = 1, NROWS_IN
            CALL LAT_LON ( C, R, GDTYP3D, XORIG3D, YORIG3D,
     &                     XCELL3D, YCELL3D, XCENT3D, YCENT3D,
     &                     P_ALP3D, P_BET3D, P_GAM3D,
     &                     LAT_IN( C,R ), LON_IN( C,R ) )
         END DO
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute the lat and lon of the center of each output BC cell
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C South border
      NCELL = 0
      DO C = 1, NCOLS + 1
         R = 0
         NCELL = NCELL + 1
         CALL LAT_LON ( C, R, GDTYP_GD, XORIG_GD, YORIG_GD,
     &                  XCELL_GD, YCELL_GD, XCENT_GD, YCENT_GD,
     &                  P_ALP_GD, P_BET_GD, P_GAM_GD,
     &                  LAT_OUT( NCELL ), LON_OUT( NCELL ) )
      END DO

C East border
      DO R = 1, NROWS + 1
         C = NCOLS + 1
         NCELL = NCELL + 1
         CALL LAT_LON ( C, R, GDTYP_GD, XORIG_GD, YORIG_GD,
     &                  XCELL_GD, YCELL_GD, XCENT_GD, YCENT_GD,
     &                  P_ALP_GD, P_BET_GD, P_GAM_GD,
     &                  LAT_OUT( NCELL ), LON_OUT( NCELL ) )
      END DO

C North border
      DO C = 0, NCOLS
         R = NROWS + 1
         NCELL = NCELL + 1
         CALL LAT_LON ( C, R, GDTYP_GD, XORIG_GD, YORIG_GD,
     &                  XCELL_GD, YCELL_GD, XCENT_GD, YCENT_GD,
     &                  P_ALP_GD, P_BET_GD, P_GAM_GD,
     &                  LAT_OUT( NCELL ), LON_OUT( NCELL ) )
      END DO

C West border
      DO R = 0, NROWS
         C = 0
         NCELL = NCELL + 1
         CALL LAT_LON ( C, R, GDTYP_GD, XORIG_GD, YORIG_GD,
     &                  XCELL_GD, YCELL_GD, XCENT_GD, YCENT_GD,
     &                  P_ALP_GD, P_BET_GD, P_GAM_GD,
     &                  LAT_OUT( NCELL ), LON_OUT( NCELL ) )
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Map the CTM file LAT/LONs to the BC file LAT/LONs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO N = 1, NBNDY
         LAT = LAT_OUT( N )
         LON = LON_OUT( N )
         DMIN = 1.0E+30
         DO C = 1, NCOLS_IN
            DO R = 1, NROWS_IN
               Y1 = ( LAT - LAT_IN( C,R ) ) ** 2
               X1 = ( LON - LON_IN( C,R ) ) ** 2
               IF ( ( X1 + Y1 ) .LT. DMIN ) THEN           
                  DMIN = X1 + Y1
                  COL_LOC( N ) = C
                  ROW_LOC( N ) = R
               END IF
             END DO
         END DO
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Call the spatial interpolation routine to log info only 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL M3_VINTERP ( LOGUNIT, SDATE, STIME, NCOLS_IN, NROWS_IN, NLAYS_IN,
     &                  NSPCS_IN, COL_LOC, ROW_LOC, BCIN, BCVI, CTM_FL_NAME )
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Call the routine to open the Models3 BC output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. DESC3( CTM_FL_NAME( 1 ) ) ) THEN
         MSG = 'Could not read DESC of  ' // CTM_FL_NAME( 1 ) // ' file'
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

C...if the input conc file in time independent, then make the BC file
C...  time independent as well

      IF ( TSTEP3D .EQ. 0 ) THEN
         TSTEP = 0

C...if the input conc file is time dependent, then set the timestep to be same
C...  as the target domain's met input file

      ELSE
         IF ( .NOT. DESC3( MET_BDY_3D_FIN ) ) THEN
            MSG = 'Could not read DESC of  ' // MET_BDY_3D_FIN // ' file'
            CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
         END IF

         TSTEP = TSTEP3D
      END IF

      CALL OPN_BC_FILE ( LOGUNIT, SDATE, STIME, TSTEP, NSPCS_IN,
     &                   INFL_SP_NAME, VTYPE_IN, UNITS_IN, VDESC_IN,
     &                   BC_FNAME, 1 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Top of loop over output time steps 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      JDATE = SDATE
      JTIME = STIME

      DO STEP = 1, NSTEPS

C Read the concentration file(s)
         LNEG = .FALSE.
         ISP = 0

         DO N = 1, N_CTM_FLS

            IF ( .NOT. DESC3( CTM_FL_NAME( N ) ) ) THEN
               MSG = 'Could not read DESC of  ' // CTM_FL_NAME( N ) 
     &              // ' file'
               CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT2 )
            END IF

            DO ISP = 1, NSPCS_IN

               VNAME = INFL_SP_NAME( ISP )

               IF ( .NOT. INTERP3( CTM_FL_NAME( N ), VNAME, PNAME, JDATE, JTIME,
     &                          NCOLS_IN*NROWS_IN*NLAYS_IN, CONCIN ) ) THEN
                  MSG = 'Could not read input CTM Conc file ' //
     &                   CTM_FL_NAME( N )         
                  CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT2 )
               END IF


               DO NBND = 1, NBNDY
                  C = COL_LOC( NBND )
                  R = ROW_LOC( NBND )
                  DO L = 1, NLAYS_IN
                     BCIN( NBND,L,ISP ) = CONCIN( C,R,L )
                  END DO
               END DO

            END DO 

         END DO

C Do the spatial interpolation
         CALL M3_VINTERP ( LOGUNIT, JDATE, JTIME, NCOLS_IN, NROWS_IN,
     &                     NLAYS_IN, NSPCS_IN, COL_LOC, ROW_LOC, BCIN,
     &                     BCVI, CTM_FL_NAME )
      
C Write the output BC concentrations 
         DO ISP = 1, NSPCS_IN

            FLN = ( ISP - 1 ) / MXVARS3 + 1
            VNAME = INFL_SP_NAME( ISP )

            DO L = 1, NLAYS
               DO NBND = 1, NBNDY
                  COUT( NBND,L ) = BCVI( NBND,L,ISP )
!                  IF ( COUT( NBND,L ) .LT. 0.0 ) THEN
!                     LNEG = .TRUE.
!                  ELSE IF ( COUT( NBND,L ) .LT. CMIN ) THEN
!                     COUT( NBND,L ) = CMIN 
!                  END IF
               END DO
            END DO

            IF ( .NOT. WRITE3( BC_FNAME( FLN ), VNAME, JDATE, JTIME,
     &                        COUT( 1,1 ) ) ) THEN
               MSG =  'Could not WRITE species ' //  VNAME // 
     &                'to file ' // BC_FNAME( FLN ) 
               CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT2 )
            END IF

         END DO

!         IF ( LNEG ) THEN
!            MSG = 'Negative BCs output'
!            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 ) 
!         END IF

         WRITE( LOGUNIT, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &         'Timestep written to', BC_FNAME( FLN ),
     &         'for date and time', JDATE, JTIME

         CALL NEXTIME ( JDATE, JTIME, TSTEP )

      END DO

      RETURN

      END
