
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
C $Header: /project/yoj/arc/ICON/src/m3conc/m3_icout.F,v 1.2 2011/10/21 16:41:54 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE M3_ICOUT( LOGUNIT,
     &                     N_CTM_FLS,
     &                     SDATE, STIME,
     &                     NCOLS_IN, NROWS_IN, NLAYS_IN, NSPCS_IN,
     &                     CTM_FL_NAME, INFL_SP_NAME,
     &                     VTYPE_IN, UNITS_IN, VDESC_IN )

C***********************************************************************
 
C  Function: Reads the input CMAQ concentration file(s) and opens and
C            writes the output IC file

C  Preconditions: None
  
C  Key Subroutines/Functions Called:   
 
C  Revision History: Prototype created by Jerry Gipson, January, 1998
 
C                    02/09/00 David Wong, LM
C                      -- block all HPALLOC or HPDALLOC calls by using a CPP
C                         flag F90 when the code is running on T3E
C                      -- renamed two original arguments NROWS_IN and NCOLS_IN
C                         to GL_NROWS_IN and GL_NCOLS_IN, respectively
C                      -- declared my_nrows_in, my_ncols_in, nrows_in, and
C                         ncols_in
C                      -- used MY_NROWS and MY_NCOLS in loops
C                      -- developed a new algorithm to perform the mapping
C                      -- introduced calls to new routines to aid 
C                         parallelization of this routine
 
C                    02/05/00 David Wong, LM
C                      -- added two more arguments GL_NCOLS_IN, GL_ROWS_IN when 
C                         m3_ping is called
 
C                    01/24/02 Steve Howard (Jeff Young) - dynamic allocation
 
C                    02/20/02 David Wong, LM
C                      -- modified the alogrithm to determine the row/column
C                         position of fine grid relative to the coarse grid by
C                         considering the case the fine and coarse grids have
C                         the same resolution, i.e. TEMP1 = 0.0
C                      -- used XTRACT3 to input data. With this new change, 
C                         variables STRTROW, ENDROW, STRTCOL, and ENDCOL are
C                         computed from ROWSX_PE and COLSX_PE, respectively.
 
C                    04/10/02 David Wong, LM
C                      -- XTRACT3, which is used to read in data for a PE, does
C                         not allow any empty space in data structure. The data
C                         declaration for the allocatable array CONCIN requires
C                         MY_NCOLS_IN and MY_NROWS_IN for dimensioning.
C                      -- Arguments NCOLS_IN and NROWS_IN are replaced by 
C                         MY_NCOLS_IN and MY_NROWS_IN in calling subroutines
C                         SHIFT_MAP, M3_PING, and VINTERP, so array dimensions 
C                         of CONCIN are correct inside those routines.
 
C                    02/06/09 David Wong
C                      -- declared CONCVI as an allocatable array to avoid stacksize
C                         limit problem in some machine setup
C                      -- applied deallocate statement to arrays CONCVI and CONCIN

C Description of the new algorithm:
 
C   The new algorithm involves the following steps:
    
C     -- compute the lat and lon of the (1,1) cell of the fine domain, 
C        lat_out_1 and lon_out_1, respectively
C     -- compute the lat and lon of the (1,1) cell of the coarse domain,
C        lat_in_1 and lon_in_1, respectively
C     -- locate the column and row position of the fine domain (1,1) cell with 
C        respect to the coarse domain by using an iterative method
C     -- locate the column and row position of the fine domain (1,1) cell 
C        within the coarse grid cell found from above step in terms of fine 
C        grid scale. The following is an illustration; let the resolution of
C        the fine grid be 5 in the x and y directions, c denotes the center of
C        the coarse grid cell which coincides with the (1,1) cell of the fine
C        domain, x denotes the (1,1) cell`s position where the step is going to
C        determine.
 
C               * * * * *
C               * * * * *
C               * x c * *
C               * * * * *
C               * * * * *
 
C        In this example COL_DISP = 3 and ROW_DISP = 2
 
C     -- call mapping_init to determine the starting position of the fine
C        domain in each processor
C     -- perform the remapping in each processor
 
C  12/13/04 J.Young: vert dyn alloc - Use VGRD_DEFN
C                    eliminate malloc calls
C  06 Jun 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
 
C  05 Jul 11 David Wong: added PRE_COL_LOC and PRE_ROW_LOC to hold
C                        before normalized value of COL_LOC and ROW_LOC,
C                        respectively

C  21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
C  02 Nov 18 S.Roselle: Removed species mapping
C  06 Nov 18 S.Roselle: Removed parallel processing code;
C                       Replaced UTILIO_DEFN with M3UTILIO
C  11 June 19 F. Sidi : Resolved Memory Issue by changing loop structure to loop
C                       over spcies instead of saving species

C***********************************************************************

      USE HGRD_DEFN   ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN   ! vertical layer specifications
      USE M3UTILIO    ! IOAPI module
      USE IC_PARMS    ! ICON parameters

      IMPLICIT NONE     

C Arguments:
      INTEGER, INTENT( IN ) :: LOGUNIT           ! Unit number for output log
      INTEGER, INTENT( IN ) :: N_CTM_FLS         ! Number of input CTM files
      INTEGER, INTENT( IN ) :: SDATE             ! Date for IC Output
      INTEGER, INTENT( IN ) :: STIME             ! Time for IC output
      INTEGER, INTENT( IN ) :: NCOLS_IN          ! No. of columns in input conc file
      INTEGER, INTENT( IN ) :: NROWS_IN          ! No. of rows in input conc file
      INTEGER, INTENT( IN ) :: NLAYS_IN          ! No. of layers in input conc file
      INTEGER, INTENT( IN ) :: NSPCS_IN          ! Total No. of species in input conc file(s)
      CHARACTER( 16 ), INTENT( IN ) :: CTM_FL_NAME( : )   ! Name of CTM file
      CHARACTER( 16 ), INTENT( IN ) :: INFL_SP_NAME( : )  ! Name of input CTM species
      CHARACTER( 16 ), INTENT( IN ) :: UNITS_IN( : ) ! Units for CTM species
      CHARACTER( 80 ), INTENT( IN ) :: VDESC_IN( : ) ! Variable description for CTM species

      INTEGER, INTENT( IN ) :: VTYPE_IN( : ) ! variable type for CTM species

C Parameters:
      REAL, PARAMETER :: CMIN = 1.0E-30   ! Minimum output concentration

C External Functions: None
 
C Local Variables:
      CHARACTER( 16 ) :: PNAME = 'M3_ICOUT' ! Program Name
      CHARACTER( 80 ) :: MSG                ! Log message
      CHARACTER( 16 ) :: VNAME              ! Species name on CTM conc file
      CHARACTER( 16 ), ALLOCATABLE :: IC_FNAME( : ) ! Logical names of IC Output file(s)

      INTEGER C, CIN, CIC   ! Column loop indices
      INTEGER FLN           ! IC output file number
      INTEGER ISP           ! Array indices for species
      INTEGER L             ! Layer loop index
      INTEGER N             ! Loop indices for species
      INTEGER R, RIN, RIC   ! Row loop indices
      INTEGER TSTEP         ! Timestep for IC Output (time dependent)
      INTEGER ALLOCSTAT     ! Status returned from array allocation

      INTEGER, ALLOCATABLE :: COL_LOC( :,: )  ! Output IC col corresponding to
                                              ! a cell in the input CTM file
      INTEGER, ALLOCATABLE :: ROW_LOC( :,: )  ! Output IC row corresponding to
                                              ! a cell in the input CTM file

      LOGICAL LNEG          ! Flag for negative concentrations

      REAL DMIN   ! Smallest distance between cell centers
      REAL LAT    ! Latitude of center of cell for the IC output file
      REAL LON    ! Longitude of center of cell for the IC output file
      REAL X1     ! longitudenal distance between cell centers
      REAL Y1     ! latitudinal distance between cell centers

      REAL, ALLOCATABLE :: LAT_OUT( :,: )   ! Lat of cells in BC output file
      REAL, ALLOCATABLE :: LON_OUT( :,: )   ! Lon of cells in BC output file

      REAL, ALLOCATABLE :: LAT_IN( :,: )    ! Lat of cells in CTM conc file
      REAL, ALLOCATABLE :: LON_IN( :,: )    ! Lon of cells in CTM conc file

      REAL, ALLOCATABLE :: COUT( :,:,: )    ! Substituted output IC conc
      REAL, ALLOCATABLE :: CONCIN( :,:,: )  ! Input concs
      REAL, ALLOCATABLE :: ICIN( :,:,: )  ! Horizontally set ICs
      REAL, ALLOCATABLE :: ICVI( :,:,: )  ! Vertically interpolated ICs

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

         SUBROUTINE OPN_IC_FILE ( LOGUNIT, SDATE, STIME, TSTEP, NSPCS_OUT,
     &                            SPNAME_OUT, VTYPE_OUT, UNITS_OUT,
     &                            VDESC_OUT, IC_FNAME, RINDX )
            CHARACTER( 16 ), INTENT( OUT ) :: IC_FNAME( : )
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
         END SUBROUTINE OPN_IC_FILE

         SUBROUTINE M3_VINTERP ( LOGUNIT, SDATE, STIME,
     &                           NCOLS_IN, NROWS_IN, NLAYS_IN, NSPCS_IN,
     &                           COL_LOC, ROW_LOC,
     &                           ICIN, ICVI, CTM_FL_NAME )
            INTEGER, INTENT( IN ) :: LOGUNIT
            INTEGER, INTENT( IN ) :: SDATE
            INTEGER, INTENT( IN ) :: STIME
            INTEGER, INTENT( IN ) :: NCOLS_IN
            INTEGER, INTENT( IN ) :: NROWS_IN
            INTEGER, INTENT( IN ) :: NLAYS_IN
            INTEGER, INTENT( IN ) :: NSPCS_IN
            INTEGER, INTENT( IN ) :: COL_LOC( :,: )
            INTEGER, INTENT( IN ) :: ROW_LOC( :,: )
            REAL, INTENT( IN )  :: ICIN( :,:,:)
            REAL, INTENT( OUT ) :: ICVI( :,:,:)
            CHARACTER( 16 ), INTENT( IN ) :: CTM_FL_NAME( : )
         END SUBROUTINE M3_VINTERP

      END INTERFACE

C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  allocate arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ALLOCATE( IC_FNAME( MXCTMS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating IC_FNAME'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( COL_LOC( NCOLS,NROWS ), ROW_LOC( NCOLS,NROWS ),
     &          STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating COL_LOC, ROW_LOC'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( LAT_OUT( NCOLS,NROWS ), LON_OUT( NCOLS,NROWS ),
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

      ALLOCATE( COUT( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating COUT'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( CONCIN( NCOLS_IN,NROWS_IN,NLAYS_IN ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating CONCIN'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( ICIN( NCOLS,NROWS,NLAYS_IN ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating ICIN'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( ICVI( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating ICVI'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      write( logunit,* ) ' '
      write( logunit,* ) '    NCOLS_IN: ', NCOLS_IN
      write( logunit,* ) '    NROWS_IN: ', NROWS_IN
      write( logunit,* ) '    NLAYS_IN: ', NLAYS_IN
      write( logunit,* ) '    NSPCS_IN: ', NSPCS_IN
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
c  Compute the lat and lon of the center of the fine domain (1,1) cell
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO C = 1, NCOLS
         DO R = 1, NROWS
            CALL LAT_LON ( C, R, GDTYP_GD, XORIG_GD, YORIG_GD,
     &                     XCELL_GD, YCELL_GD, XCENT_GD, YCENT_GD,
     &                     P_ALP_GD, P_BET_GD, P_GAM_GD,
     &                     LAT_OUT( C,R ), LON_OUT( C,R ) )
         END DO
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Map the CTM file LAT/LONs to the IC file LAT/LONs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO CIC = 1, NCOLS
         DO RIC = 1, NROWS
            LAT = LAT_OUT( CIC,RIC )
            LON = LON_OUT( CIC,RIC )
            DMIN = 1.0E+30
            DO CIN = 1, NCOLS_IN
               DO RIN = 1, NROWS_IN
                  Y1 = ( LAT - LAT_IN( CIN,RIN ) ) ** 2
                  X1 = ( LON - LON_IN( CIN,RIN ) ) ** 2
                  IF ( ( X1 + Y1 ) .LT. DMIN ) THEN           
                     DMIN = X1 + Y1
                     COL_LOC( CIC,RIC ) = CIN
                     ROW_LOC( CIC,RIC ) = RIN
                  END IF
               END DO
            END DO
         END DO
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Call the routine to open the Models3 IC output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. DESC3( CTM_FL_NAME( 1 ) ) ) THEN
         MSG = 'Could not read DESC of  ' // CTM_FL_NAME( 1 ) // ' file'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

C...if the input conc file in time independent, then make the IC file
C...  time independent as well

      IF ( TSTEP3D .EQ. 0 ) THEN
         TSTEP = 0

C...if the input conc file is time dependent, then set the timestep to be same
C...  as the target domain's met input file

      ELSE
         IF ( .NOT. DESC3( MET_CRO_3D_FIN ) ) THEN
            MSG = 'Could not read DESC of  ' // MET_CRO_3D_FIN // ' file'
            CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
         END IF

         TSTEP = TSTEP3D
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open the IC output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL OPN_IC_FILE ( LOGUNIT, SDATE, STIME, TSTEP, NSPCS_IN,
     &                   INFL_SP_NAME, VTYPE_IN, UNITS_IN, VDESC_IN,
     &                   IC_FNAME, 1 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Read the concentration file(s)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LNEG = .FALSE.
      ISP = 0

      DO N = 1, N_CTM_FLS

         IF ( .NOT. DESC3( CTM_FL_NAME( N ) ) ) THEN
            MSG = 'Could not read DESC of  ' // CTM_FL_NAME( N ) 
     &           // ' file'
            CALL M3EXIT ( PNAME, SDATE, STIME, MSG, XSTAT2 )
         END IF

         DO ISP = 1, NSPCS_IN

            VNAME = INFL_SP_NAME( ISP )

            IF ( MXREC3D .EQ. 1 ) THEN
               IF ( .NOT. READ3( CTM_FL_NAME( N ), VNAME, ALLAYS3, SDATE,
     &                           STIME, CONCIN( 1,1,1 ) ) ) THEN
                  MSG = 'Could not read input CTM Conc file ' //
     &                  CTM_FL_NAME( N )
                  CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT2 )
               END IF
            ELSE
               IF ( .NOT. INTERP3( CTM_FL_NAME( N ), VNAME, PNAME, SDATE, STIME,
     &                             NCOLS_IN*NROWS_IN*NLAYS_IN, CONCIN ) ) THEN
                  MSG = 'Could not read input CTM Conc file ' //
     &                  CTM_FL_NAME( N )
                  CALL M3EXIT ( PNAME, SDATE, STIME, MSG, XSTAT2 )
               END IF
            END IF

            DO CIC = 1, NCOLS
               DO RIC = 1, NROWS
                  CIN = COL_LOC( CIC,RIC )
                  RIN = ROW_LOC( CIC,RIC )
                  DO L = 1, NLAYS_IN
                     ICIN( CIC,RIC,L ) = CONCIN( CIN,RIN,L )
                  END DO
               END DO
            END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Do vertical interpolation/extrapolation
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         
         CALL M3_VINTERP ( LOGUNIT, SDATE, STIME,
     &                  NCOLS_IN, NROWS_IN, NLAYS_IN, NSPCS_IN,
     &                  COL_LOC, ROW_LOC,
     &                  ICIN, ICVI, CTM_FL_NAME )


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Write the output IC concentrations
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         
!         LNEG = .FALSE.
         FLN = ( N - 1 ) / MXVARS3 + 1
           
         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  COUT( C,R,L ) = ICVI( C,R,L )
!                  IF ( COUT( C,R,L ) .LT. 0.0 ) THEN
!                     LNEG = .TRUE.
!                  ELSE IF ( COUT( C,R,L ) .LT. CMIN ) THEN
!                     COUT( C,R,L ) = CMIN
!                  END IF 
               END DO
            END DO
         END DO

         IF ( .NOT. WRITE3( IC_FNAME( FLN ), VNAME, SDATE, STIME,
     &                      COUT( 1,1,1 ) ) ) THEN
            MSG =  'Could not WRITE species ' //  VNAME // 
     &             'to file ' // IC_FNAME( FLN ) 
            CALL M3EXIT ( PNAME, SDATE, STIME, MSG, XSTAT1 )
         END IF

         END DO

      END DO
        
!      IF ( LNEG ) THEN
!         MSG = 'Negative ICs output'
!         CALL M3EXIT ( PNAME, SDATE, STIME, MSG, XSTAT2 )
!      END IF

      WRITE( LOGUNIT, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', IC_FNAME( FLN ),
     &      'for date and time', SDATE, STIME

      RETURN

      END


