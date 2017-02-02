
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

      SUBROUTINE ICOUT( LOGUNIT,
     &                  N_CTM_FLS,
     &                  SDATE, STIME,
     &                  GL_NCOLS_IN, GL_NROWS_IN, NLAYS_IN,
     &                  NSPCS_IN, NSPCS_OUT, SPMAP, 
     &                  CTM_FL_NAME,
     &                  INFL_SP_NAME,
     &                  SPNAME_OUT )

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
C***********************************************************************

      USE HGRD_DEFN     ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN     ! vertical layer specifications
      USE UTILIO_DEFN
      USE IC_PARMS      ! ICON parameters

      IMPLICIT NONE     

C Include Files;

C Arguments;
      INTEGER LOGUNIT           ! Unit number for output log
      INTEGER N_CTM_FLS         ! Number of input CTM files
      INTEGER SDATE             ! Date for IC Output
      INTEGER STIME             ! Time for IC output
      INTEGER GL_NCOLS_IN       ! No. of columns in input conc file
      INTEGER GL_NROWS_IN       ! No. of rows in input conc file
      INTEGER NLAYS_IN          ! No. of layers in input conc file
      INTEGER NSPCS_IN          ! Total No. of species in input conc file(s)
      INTEGER NSPCS_OUT         ! Number of IC species on output file(s)
      INTEGER :: SPMAP( * )     ! Map to CTM species from IC species
      CHARACTER( 16 ) :: CTM_FL_NAME( * )         ! Name of CTM file
      CHARACTER( 16 ) :: INFL_SP_NAME( * )        ! Name of input CTM species
      CHARACTER( 16 ) :: SPNAME_OUT( * )          ! Output file species names

C Parameters:
      REAL, PARAMETER :: CMIN = 1.0E-30   ! Minimum output concentration

C Local Variables:
      CHARACTER(  2 ) :: COLROW = 'CR'      ! col/row arg list order
      CHARACTER( 16 ) :: PNAME = 'M3_ICOUT' ! Program Name
      CHARACTER( 80 ) :: MSG                ! Log message
      CHARACTER( 16 ) :: VNAME              ! Species name on CTM conc file
      CHARACTER( 16 ) :: IC_FNAME( MXCTMS ) ! Logical names of IC Output file(s) 

      INTEGER C, CIN, CIC   ! Column loop indices
      INTEGER FLN           ! IC output file number
      INTEGER IND, ISP, V   ! Array indices for species
      INTEGER SPC           ! Species loop index 
      INTEGER L             ! Layer loop index
      INTEGER N             ! Loop indices for species
      INTEGER R, RIN, RIC   ! Row loop indices

      INTEGER NCOLS_IN          ! Max No. of columns in a processor
      INTEGER NROWS_IN          ! Max No. of rows in a processor

      INTEGER MY_NCOLS_IN       ! No. of columns which has work in a processor
      INTEGER MY_NROWS_IN       ! No. of rows which has work in a processor

      INTEGER :: COL_LOC( NCOLS,NROWS )  ! Output IC col corresponding to
                                         ! a cell in the input CTM file
      INTEGER :: ROW_LOC( NCOLS,NROWS )  ! Output IC row corresponding to
                                         ! a cell in the input CTM file
      INTEGER :: PRE_COL_LOC( NCOLS,NROWS )  ! pre_normalized COL_LOC
      INTEGER :: PRE_ROW_LOC( NCOLS,NROWS )  ! pre_normalized ROW_LOC

      LOGICAL LNEG                       ! Flag for negative concentrations

      REAL DMIN   ! Smallest distance between cell centers
      REAL LAT    ! Latitude of center of cell for the IC output file
      REAL LON    ! Longitude of center of cell for the IC output file
      REAL X1     ! longitudenal distance between cell centers
      REAL Y1     ! latitudinal distance between cell centers

      REAL :: COUT( NCOLS,NROWS,NLAYS )   ! Substituted output IC conc

!     REAL CONCIN( ( GL_NCOLS_IN - 1 ) / NPCOL + 1, 
!    &             ( GL_NROWS_IN - 1 ) / NPROW + 1, 
!    &               NLAYS_IN, NSPCS_IN )  ! Input conc
      REAL, ALLOCATABLE :: CONCIN ( :,:,:,: )

!     REAL CONCVI( NCOLS,NROWS,NLAYS,NSPCS_IN ) ! Spatially interpolated
      REAL, ALLOCATABLE :: CONCVI( :,:,:,: )  ! CONC array for vertical interpolation

      INTEGER PAR_ERR  ! Error code from parallel initialization.
      INTEGER ALST     ! Allocation status

      REAL LAT_IN_1    ! the lat of a cell in the (input) coarse domain
      REAL LON_IN_1    ! the lon of a cell in the (input) coarse domain
      REAL LAT_IN_2    ! the lat of a cell in the (input) coarse domain
      REAL LON_IN_2    ! the lon of a cell in the (input) coarse domain
      REAL LAT_OUT_1   ! the lat of a cell in the (output) fine domain
      REAL LON_OUT_1   ! the lon of a cell in the (output) fine domain

      REAL TMIN, TEMP1, TEMP2   ! temporary variables

      INTEGER COL_LOC_1       ! column location corresponding to LON_IN_1
      INTEGER ROW_LOC_1       ! row location corresponding to LAT_IN_1
      INTEGER COL_LOC_1_SAVE  ! storage for saved value of COL_LOC_1
      INTEGER ROW_LOC_1_SAVE  ! storage for saved value of ROW_LOC_1

      INTEGER LOW_NCOLS_IN, UP_NCOLS_IN  ! range of columns for fine tune purposes
      INTEGER LOW_NROWS_IN, UP_NROWS_IN  ! range of rows for fine tune purposes
      INTEGER COL_POS, ROW_POS      ! column and row location of the cell in the
                                    ! fine domain scale in each processor
      INTEGER COL_RATIO, ROW_RATIO  ! column and row resolution ratio between
                                    ! the fine and coarse domains
      INTEGER COL_DISP, ROW_DISP    ! column and row displacement of the first
                                    ! cell in each processor from the coarse
                                    ! domain grid
      INTEGER ORIG_ROW_DISP         ! storage for saved ROW_DISP  
      INTEGER COUNT                 ! loop count
      LOGICAL FOUND, QUIT           ! boolean flags

      INTEGER GXOFF, GYOFF          ! global origin offset from file for INTERPX
      INTEGER STRTCOL, ENDCOL, STRTROW, ENDROW

!     real time, start
!     intrinsic rtc

      INTERFACE
         SUBROUTINE SHIFT_MAP ( NCOLS, NROWS, MY_NCOLS, MY_NROWS,
     &                          GL_NCOLS_IN, GL_NROWS_IN,
     &                          NCOLS_IN, NROWS_IN, NLAYS_IN, NSPCS_IN,
     &                          COL_LOC, ROW_LOC, CONCIN )
         IMPLICIT NONE
         INTEGER, INTENT( IN ) :: NCOLS, NROWS, MY_NCOLS, MY_NROWS
         INTEGER, INTENT( IN ) :: GL_NCOLS_IN, GL_NROWS_IN
         INTEGER, INTENT( IN ) :: NCOLS_IN, NROWS_IN, NLAYS_IN, NSPCS_IN
         INTEGER, INTENT( INOUT ) :: COL_LOC( NCOLS,NROWS ),
     &                               ROW_LOC( NCOLS,NROWS )
         REAL,    INTENT( INOUT ) :: CONCIN( NCOLS_IN, NROWS_IN,
     &                                       NLAYS_IN, NSPCS_IN )
         END SUBROUTINE SHIFT_MAP
      END INTERFACE

C***********************************************************************
      ALLOCATE ( CONCVI( NCOLS,NROWS,NLAYS,NSPCS_IN ), STAT = ALST )
      IF ( ALST .NE. 0 ) THEN
         MSG = 'Error allocating CONCVI'
         CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT2 ) 
      END IF
      NCOLS_IN = ( GL_NCOLS_IN - 1 ) / NPCOL + 1
      NROWS_IN = ( GL_NROWS_IN - 1 ) / NPROW + 1

      IF( .NOT. DESC3 ( CTM_FL_NAME( 1 ) ) ) THEN
         MSG = 'Could not read DESC of  ' // CTM_FL_NAME( 1 ) 
     &         // ' file'
         CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT2 ) 
      END IF

!     start = rtc()

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute the lat and lon of the center of the fine domain (1,1) cell
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL LAT_LON( 1, 1, GDTYP_GD, XORIG_GD, YORIG_GD, 
     &              XCELL_GD, YCELL_GD, XCENT_GD, YCENT_GD,
     &              P_ALP_GD, P_BET_GD, P_GAM_GD, LAT_OUT_1, LON_OUT_1 )

      COUNT = 0
      QUIT = .FALSE.
      FOUND = .FALSE.
      ROW_LOC_1_SAVE = 1 
      COL_LOC_1_SAVE = 1 

      DO WHILE ( .NOT. QUIT )
         COUNT = COUNT + 1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Approximate the lat of the center of the fine domain (1,1) cell in 
c  the coarse domain
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         TMIN = 1.0E30
         ROW_LOC_1 = 1
         DO R = 1, GL_NROWS_IN
            CALL LAT_LON( COL_LOC_1_SAVE, R, GDTYP3D, XORIG3D, YORIG3D,
     &                    XCELL3D, YCELL3D, XCENT3D, YCENT3D,
     &                    P_ALP3D, P_BET3D, P_GAM3D, LAT_IN_1, LON_IN_1 )
            TEMP1 = ABS( LAT_IN_1 - LAT_OUT_1 )
            IF ( TEMP1 .LT. TMIN ) THEN
               TMIN = TEMP1
               ROW_LOC_1 = R
            END IF
         END DO
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Approximate the lon of the center of the fine domain (1,1) cell in 
c  the coarse domain
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         TMIN = 1.0E30
         COL_LOC_1 = 1
         DO C = 1, GL_NCOLS_IN
            CALL LAT_LON( C, ROW_LOC_1, GDTYP3D, XORIG3D, YORIG3D,
     &                    XCELL3D, YCELL3D, XCENT3D, YCENT3D,
     &                    P_ALP3D, P_BET3D, P_GAM3D, LAT_IN_1, LON_IN_1 )
            TEMP1 = ABS( LON_IN_1 - LON_OUT_1 )
            IF ( TEMP1 .LT. TMIN ) THEN
               TMIN = TEMP1
               COL_LOC_1 = C
            END IF
         END DO

         IF ( ( COL_LOC_1 .EQ. COL_LOC_1_SAVE ) .AND.
     &        ( ROW_LOC_1 .EQ. ROW_LOC_1_SAVE ) ) THEN
            QUIT = .TRUE.
            FOUND = .TRUE.
         ELSE
            IF ( COUNT .EQ. 10 ) THEN
               QUIT = .TRUE.
            ELSE
               COL_LOC_1_SAVE = COL_LOC_1
               ROW_LOC_1_SAVE = ROW_LOC_1
            END IF
         END IF
      END DO   ! DO WHILE

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c if not converge, then perform fine tuning
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. FOUND ) THEN

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set the fine tune range
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LOW_NCOLS_IN = MAX( COL_LOC_1 - 2, 1 )
         UP_NCOLS_IN  = MIN( COL_LOC_1 + 2, GL_NCOLS_IN )
         LOW_NROWS_IN = MAX( ROW_LOC_1 - 2, 1 )
         UP_NROWS_IN  = MIN( ROW_LOC_1 + 2, GL_NROWS_IN )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Fine tune the center approximation of the lat and lon of the (1,1) 
c  cell in the coarse domain
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         TMIN = 1.0E30
         DO R = LOW_NROWS_IN, UP_NROWS_IN
            DO C = LOW_NCOLS_IN, UP_NCOLS_IN
               CALL LAT_LON( C, R, GDTYP3D, XORIG3D, YORIG3D,
     &                       XCELL3D, YCELL3D, XCENT3D, YCENT3D,
     &                       P_ALP3D, P_BET3D, P_GAM3D, LAT_IN_1, LON_IN_1 )
               TEMP1 = ( LAT_IN_1 - LAT_OUT_1 ) ** 2
     &               + ( LON_IN_1 - LON_OUT_1 ) ** 2
               IF ( TEMP1 .LT. TMIN ) THEN
                  TMIN = TEMP1
                  ROW_LOC_1 = R
                  COL_LOC_1 = C
               END IF
            END DO
         END DO
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c determine the row positioning of the fine grid relative to the coarse 
c grid
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL LAT_LON( COL_LOC_1, ROW_LOC_1, GDTYP3D, XORIG3D, YORIG3D,
     &              XCELL3D, YCELL3D, XCENT3D, YCENT3D,
     &              P_ALP3D, P_BET3D, P_GAM3D, LAT_IN_1, LON_IN_1 )

      IF ( ROW_LOC_1 .LT. GL_NROWS_IN ) THEN
         CALL LAT_LON( COL_LOC_1, ROW_LOC_1+1, GDTYP3D, XORIG3D, YORIG3D,
     &                 XCELL3D, YCELL3D, XCENT3D, YCENT3D,
     &                 P_ALP3D, P_BET3D, P_GAM3D, LAT_IN_2, LON_IN_2 )

         FOUND = .FALSE.
         ROW_POS = 1
         DO WHILE ( .NOT. FOUND )
            CALL LAT_LON( 1, ROW_POS, GDTYP_GD, XORIG_GD, YORIG_GD,
     &                    XCELL_GD, YCELL_GD, XCENT_GD, YCENT_GD,
     &                    P_ALP_GD, P_BET_GD, P_GAM_GD, LAT_OUT_1, LON_OUT_1 )

            TEMP1 = ABS( LAT_IN_1 - LAT_OUT_1 )
            TEMP2 = ABS( LAT_IN_2 - LAT_OUT_1 )
!           IF ( TEMP1 .GT. TEMP2 ) THEN
            IF ( ( TEMP1 .EQ. 0.0 ) .OR. ( TEMP1 .GT. TEMP2 ) ) THEN
               FOUND = .TRUE.
            ELSE
               ROW_POS = ROW_POS + 1
            END IF
         END DO   ! DO WHILE
      ELSE
         ROW_POS = 1
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c determine the col positioning of the fine grid relative to the coarse 
c grid
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( COL_LOC_1 .LT. GL_NCOLS_IN ) THEN
         CALL LAT_LON( COL_LOC_1+1, ROW_LOC_1, GDTYP3D, XORIG3D, YORIG3D,
     &                 XCELL3D, YCELL3D, XCENT3D, YCENT3D,
     &                 P_ALP3D, P_BET3D, P_GAM3D, LAT_IN_2, LON_IN_2 )

         FOUND = .FALSE.
         COL_POS = 1
         DO WHILE ( .NOT. FOUND )

            CALL LAT_LON( COL_POS, 1, GDTYP_GD, XORIG_GD, YORIG_GD,
     &                    XCELL_GD, YCELL_GD, XCENT_GD, YCENT_GD,
     &                    P_ALP_GD, P_BET_GD, P_GAM_GD, LAT_OUT_1, LON_OUT_1 )

            TEMP1 = ABS( LON_IN_1 - LON_OUT_1 )
            TEMP2 = ABS( LON_IN_2 - LON_OUT_1 )
!           IF ( TEMP1 .GT. TEMP2 ) THEN
            IF ( ( TEMP1 .EQ. 0.0 ) .OR. ( TEMP1 .GT. TEMP2 ) ) THEN
               FOUND = .TRUE.
            ELSE
               COL_POS = COL_POS + 1
            END IF
         END DO   ! DO WHILE
      ELSE
         COL_POS = 1
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize the fine domain starting point in a processor
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!!!!!!!!!!                  -- fine --        --  coarse --
      CALL MAPPING_INIT ( XCELL_GD, YCELL_GD, XCELL3D, YCELL3D,
     &                    COL_POS, ROW_POS, COL_RATIO, ROW_RATIO,
     &                    COL_DISP, ROW_DISP, COL_LOC_1, ROW_LOC_1 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Map the CTM file cols/rows to the IC file cols/rows
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ORIG_ROW_DISP = ROW_DISP

      CIN = COL_LOC_1
      DO CIC = 1, MY_NCOLS
         RIN = ROW_LOC_1
         ROW_DISP = ORIG_ROW_DISP
         DO RIC = 1, MY_NROWS
            COL_LOC( CIC,RIC ) = CIN
            ROW_LOC( CIC,RIC ) = RIN
            ROW_DISP = ROW_DISP + 1
            RIN = RIN + ROW_DISP / ROW_RATIO
            ROW_DISP = MOD( ROW_DISP, ROW_RATIO )
         END DO
         COL_DISP = COL_DISP + 1
         CIN = CIN + COL_DISP / COL_RATIO
         COL_DISP = MOD( COL_DISP, COL_RATIO )
      END DO

!     time = rtc() - start
!     print *, ' ----------- ', time

#ifdef parallel
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Configure the coarse domain
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL RECONFIG_DOMAIN( COLROW, GL_NCOLS_IN, GL_NROWS_IN, NLAYS_IN, NSPCS_IN,
     &                      NTHIK, NCOLS_IN, NROWS_IN, NPCOL, NPROW,
     &                      MNDIS, MEDIS, MSDIS, MWDIS,
     &                      COLSX_PE, ROWSX_PE, MY_NCOLS_IN, MY_NROWS_IN,
     &                      MY_NBNDY, PAR_ERR )
#else
      COLSX_PE( 1,1 ) = 1
      COLSX_PE( 2,1 ) = GL_NCOLS_IN
      ROWSX_PE( 1,1 ) = 1
      ROWSX_PE( 2,1 ) = GL_NROWS_IN

      MY_NCOLS_IN = GL_NCOLS_IN
      MY_NROWS_IN = GL_NROWS_IN
      MY_NBNDY = 2*NTHIK * ( MY_NCOLS_IN + MY_NROWS_IN + 2*NTHIK )
#endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Determine local starting and ending column and row indices for a file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ISP = 0

      STRTCOL = COLSX_PE( 1, MYPE+1 )
      ENDCOL  = COLSX_PE( 2, MYPE+1 )
      STRTROW = ROWSX_PE( 1, MYPE+1 )
      ENDROW  = ROWSX_PE( 2, MYPE+1 )

      ALLOCATE ( CONCIN( MY_NCOLS_IN,MY_NROWS_IN,NLAYS_IN,NSPCS_IN ), 
     &           STAT = ALST )
      IF ( ALST .NE. 0 ) THEN
         MSG = 'Error allocating CONCIN'
         CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT2 ) 
      END IF

      DO N = 1, N_CTM_FLS

         IF ( .NOT. DESC3 ( CTM_FL_NAME( N ) ) ) THEN
            MSG = 'Could not read DESC of  ' // CTM_FL_NAME( N ) 
     &            // ' file'
            CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT2 ) 
         END IF
                  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Read the input CTM Conc file(s) 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO V = 1, NVARS3D

            ISP = ISP + 1
            VNAME = VNAME3D( V )

!           IF ( .NOT. READ3( CTM_FL_NAME( N ), VNAME, ALLAYS3, SDATE, 
!    &                        STIME, CONCIN( 1,1,1,ISP ) ) ) THEN

            IF ( .NOT. XTRACT3( CTM_FL_NAME( N ), VNAME, 1, NLAYS_IN, 
     &                          STRTROW, ENDROW, STRTCOL, ENDCOL,
     &                          SDATE, STIME, CONCIN( 1,1,1,ISP ) ) ) THEN

               MSG = 'Could not read input CTM Conc file ' //
     &                CTM_FL_NAME( N )         
               CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT1 ) 
            END IF

         END DO

      END DO 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c save the pre-normalized COL_ and ROW_LOC
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PRE_COL_LOC = COL_LOC   ! array assignment
      PRE_ROW_LOC = ROW_LOC   ! array assignment

#ifdef parallel
      CALL SHIFT_MAP ( NCOLS, NROWS, MY_NCOLS, MY_NROWS,
     &                 GL_NCOLS_IN, GL_NROWS_IN, MY_NCOLS_IN, MY_NROWS_IN,
     &                 NLAYS_IN, NSPCS_IN, COL_LOC, ROW_LOC, CONCIN )

C In:
C    NCOLS       -- number of fine domain columns in this processor
C    NROWS       -- number of fine domain rows    in this processor
C    MY_NCOLS    -- number of fine domain data columns in this processor
C    MY_NROWS    -- number of fine domain data    rows in this processor
C    GL_NCOLS_IN -- number of coarse domain columns
C    GL_NROWS_IN -- number of coarse domain rows
C    MY_NCOLS_IN -- number of coarse domain columns in this processor
C    MY_NROWS_IN -- number of coarse domain rows    in this processor
C    NLAYS_IN    -- number of coarse domain layers
C    NSPCS_IN    -- number of coarse domain species
C InOut:
C    COL_LOC     -- column position of a grid point of the fine domain
C                   relative to the coarse domain in this processor
C    ROW_LOC     -- row position of a grid point of the fine domain
C                   relative to the coarse domain in this processor
C    CONCIN      -- input concentration field, modified out

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Re-configure the fine domain
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL RECONFIG_DOMAIN( COLROW, GL_NCOLS, GL_NROWS, NLAYS, NSPCS_IN,
     &                      NTHIK, NCOLS, NROWS, NPCOL, NPROW,
     &                      MNDIS, MEDIS, MSDIS, MWDIS,
     &                      COLSX_PE, ROWSX_PE, MY_NCOLS, MY_NROWS,
     &                      MY_NBNDY, PAR_ERR )
#else
      COLSX_PE( 1,1 ) = 1
      COLSX_PE( 2,1 ) = GL_NCOLS
      ROWSX_PE( 1,1 ) = 1
      ROWSX_PE( 2,1 ) = GL_NROWS

      MY_NCOLS = GL_NCOLS
      MY_NROWS = GL_NROWS
      MY_NBNDY = 2*NTHIK * ( MY_NCOLS + MY_NROWS + 2*NTHIK )
#endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do vertical interpolation/extrapolation
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL VINTERP( CTM_FL_NAME, LOGUNIT, SDATE, STIME,
     &              MY_NCOLS_IN, MY_NROWS_IN, NLAYS_IN, NSPCS_IN,
     &              COL_LOC, ROW_LOC, PRE_COL_LOC, PRE_ROW_LOC,
     &              CONCIN, CONCVI )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open the IC output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL OPN_IC_FILE( LOGUNIT, SDATE, STIME, NSPCS_OUT, SPNAME_OUT,
     &                  IC_FNAME, 1 )  

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write the output IC concentrations
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LNEG = .FALSE.
      DO N = 1, NSPCS_OUT

         FLN = ( N - 1 ) / MXVARS3 + 1
         VNAME = SPNAME_OUT( N )
           
         IND = SPMAP( N )

         DO L = 1, NLAYS
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  COUT( C,R,L ) = CONCVI( C,R,L,IND )
                  IF ( COUT( C,R,L ) .LT. 0.0 ) THEN
                     LNEG = .TRUE.
                  ELSE IF ( COUT( C,R,L ) .LT. CMIN ) THEN
                     COUT( C,R,L ) = CMIN
                  END IF 
               END DO
            END DO
         END DO

         IF ( .NOT. WRITE3( IC_FNAME( FLN ), VNAME, SDATE, STIME,
     &                      COUT( 1,1,1 ) ) ) THEN
            MSG =  'Could not WRITE species ' //  VNAME // 
     &             'to file ' // IC_FNAME( FLN ) 
            CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT1 )
         END IF

      END DO

      IF ( LNEG ) THEN
         MSG = 'Negative ICs output'
         CALL M3EXIT( PNAME, SDATE, STIME, MSG, XSTAT2 ) 
      END IF

      DEALLOCATE ( CONCVI, CONCIN )

      RETURN

      END
