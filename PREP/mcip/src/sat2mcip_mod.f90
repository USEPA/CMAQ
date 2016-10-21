!------------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in           !
!  continuous development by various groups and is based on information        !
!  from these groups: Federal Government employees, contractors working        !
!  within a United States Government contract, and non-Federal sources         !
!  including research institutions.  These groups give the Government          !
!  permission to use, prepare derivative works of, and distribute copies       !
!  of their work in the CMAQ system to the public and to permit others         !
!  to do so.  The United States Environmental Protection Agency                !
!  therefore grants similar permission to use the CMAQ system software,        !
!  but users are requested to provide copies of derivative works or            !
!  products designed to operate in the CMAQ system to the United States        !
!  Government without restrictions as to use by others.  Software              !
!  that is used with the CMAQ system but distributed under the GNU             !
!  General Public License or the GNU Lesser General Public License is          !
!  subject to their copyright restrictions.                                    !
!------------------------------------------------------------------------------!

MODULE sat2mcip

  USE mcipparm  ! metcol, metrow, ncols_x, nrows_x, ncols, nrows, nlays
  USE xvars     ! xlatc, xlonc, xcfract, xcldtop, xcldbot

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Run Options.
!-------------------------------------------------------------------------------

  INTEGER,       PARAMETER        :: max_sat  = 100
  CHARACTER (LEN=256), PUBLIC     :: file_sat ( max_sat )

  INTEGER, PUBLIC                 :: lsat ! user input: 
                                        !   0 = no sat. photo adjustment
                                        !   1 =    sat. photo adjustment

!-------------------------------------------------------------------------------
! Variables that will be used in MCIP.
! These are a subset of available data from sat2mcip file.
!-------------------------------------------------------------------------------

  INTEGER, PUBLIC               :: sat_nx, sat_ny, sat_nt
  REAL,    PUBLIC, ALLOCATABLE  :: sat_time(:)  ! secs from MM5 T0
  CHARACTER (LEN=24), PUBLIC,     &
                ALLOCATABLE     :: sat_time_str(:) ! time: yyyy-mm-dd_hh:mm:ss.ssss

  REAL,    PUBLIC, ALLOCATABLE  :: xsat_lon(:,:)
  REAL,    PUBLIC, ALLOCATABLE  :: xsat_lat(:,:)
  INTEGER, PUBLIC, ALLOCATABLE  :: xsat_cldflg(:,:)
  REAL,    PUBLIC, ALLOCATABLE  :: xsat_cldfrac(:,:)
  REAL,    PUBLIC, ALLOCATABLE  :: xsat_cldtr(:,:)
  REAL,    PUBLIC, ALLOCATABLE  :: xsat_cldtopt(:,:)
  REAL,    PUBLIC, ALLOCATABLE  :: xsat_cldtopp(:,:)

  REAL,    PUBLIC               :: mm5_time_now ! mm5 time, min. from T0

!-------------------------------------------------------------------------------
! Cloud Transmissivity Array to be used in metcro.F program (output)
!-------------------------------------------------------------------------------

  REAL, PUBLIC, ALLOCATABLE     :: xcldtr   ( : , : )     ! X   grid
  REAL, PUBLIC, ALLOCATABLE     :: cldtr_c  ( : , : )     ! CTM grid

!-------------------------------------------------------------------------------
! Variables in sat2mcip file (private to sat2mcip).
!-------------------------------------------------------------------------------

  ! dimensions KX, KY, and NT were defined in the public section
  ! TIME was defined in the public section
  ! TIME_STR was defined in the public section

  REAL, PRIVATE, ALLOCATABLE, SAVE :: sat_x(:)
  REAL, PRIVATE, ALLOCATABLE, SAVE :: sat_y(:)
  REAL, PRIVATE, ALLOCATABLE, SAVE :: sat_lon(:,:)
  REAL, PRIVATE, ALLOCATABLE, SAVE :: sat_lat(:,:)
  REAL, PRIVATE, ALLOCATABLE       :: sat_cldfrac(:,:)
  REAL, PRIVATE, ALLOCATABLE       :: sat_cldalb(:,:)
  REAL, PRIVATE, ALLOCATABLE       :: sat_cldtr(:,:)
  REAL, PRIVATE, ALLOCATABLE       :: sat_cldtopt(:,:)
  REAL, PRIVATE, ALLOCATABLE       :: sat_cldtopp(:,:)
  REAL, PRIVATE, ALLOCATABLE       :: sat_sfcinsol(:,:)
  REAL, PRIVATE, ALLOCATABLE       :: sat_skint(:,:)
  REAL, PRIVATE, ALLOCATABLE       :: sat_sfcalb(:,:)

  ! Variables used in this module

  INTEGER, PRIVATE, ALLOCATABLE    :: sat_cldflg(:,:)

  INTEGER, PRIVATE, SAVE           :: sat_ncId
  INTEGER, PRIVATE, SAVE           :: xID, yID, timeID, timestrID
  INTEGER, PRIVATE, SAVE           :: lonID, latID
  INTEGER, PRIVATE, SAVE           :: cldfracID, cldalbID, cldtrID
  INTEGER, PRIVATE, SAVE           :: cldtoptID, cldtoppID
  INTEGER, PRIVATE, SAVE           :: sfcinsolID, skintID, sfcalbID

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE getsat ( mcip_now )

! This subroutine reads in satellite data for time mcip_now
!
! CALLS:
!       find_sat_indx
!       init_sat  
!
  IMPLICIT NONE

! input variables
  CHARACTER (LEN=*), INTENT (IN) :: mcip_now   ! YYYY-MO-DD-HH:MI:SS.SSSS
  
  INTEGER, SAVE         :: sat_file_indx = 0
  INTEGER, SAVE         :: sat_time_indx = 0
  
 WRITE (*,*) "in subroutine getsat ..."

!-------------------------------------------------------------------------------
! Return if no sat. input is required.  LSAT is user input defined in mcipparm.
!-------------------------------------------------------------------------------

  IF ( lsat /= 1 ) THEN
    WRITE (*,*) " lsat /= 1, I should not be here..."
    CALL graceful_stop ("GETSAT")
  END IF

!-------------------------------------------------------------------------------
! Get the satellite file that contains the data for this time.
!-------------------------------------------------------------------------------

  ! Find the correct satellite file, init time string, return time_index.

  CALL get_sat_file (mcip_now, sat_file_indx, sat_time_indx)

  WRITE (*,*) "getsat: sat_ncid = ", sat_ncid

  IF ( sat_file_indx == 0 ) THEN
    WRITE (*,*) "GETSAT: no satellite data for this time"
    WRITE (*,*) "        time: ", mcip_now(1:24)
    IF ( lsat > 0 ) THEN
      WRITE (*,*) "GETSAT: ERROR -- LSAT not zero"
      WRITE (*,*) "GETSAT:          LSAT set to zero"
      lsat = 0
    END IF
    RETURN
  END IF

!-------------------------------------------------------------------------------
! Read the satellite data for this time and copy it into X domain variables.
!-------------------------------------------------------------------------------

  CALL readsat (sat_time_indx)

END SUBROUTINE getsat

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_sat_file ( mcip_now, sat_file_indx, sat_time_indx )
  
  IMPLICIT NONE

  ! declare calling parameters:
  CHARACTER (LEN=*), INTENT (IN)     :: mcip_now
  INTEGER,           INTENT (INOUT)  :: sat_file_indx, sat_time_indx

  INTEGER :: n
  LOGICAL :: file_was_found = .FALSE.

!-------------------------------------------------------------------------------
! Check if sat file already contains the data for this time.
!-------------------------------------------------------------------------------

  WRITE(*,*) "in subroutine get_sat_file ..."

  IF ( ALLOCATED (sat_time_str) ) THEN
  ! Returns sat_time_indx=-999 if mcip_now is outside the time range
    CALL find_sat_indx (sat_time_str, mcip_now, sat_time_indx) 
    IF ( sat_time_indx >= 0 ) RETURN
  END IF    

!-------------------------------------------------------------------------------
! Data for mcip_now was not found, loop over sat files to find the right file.
!-------------------------------------------------------------------------------

  sat_file_indx = 0
  n = 0
  file_was_found = .FALSE.

  DO WHILE ( .NOT. file_was_found .AND. n <= SIZE(file_sat) )
    n = n + 1
    CALL init_sat (n)
    IF ( lsat /= 1 ) EXIT
    CALL find_sat_indx (sat_time_str, mcip_now, sat_time_indx) 
    
    IF ( sat_time_indx < 0 ) THEN
      file_was_found = .FALSE.
    ELSE
      file_was_found = .TRUE.
    END IF
  END DO

  IF ( file_was_found ) sat_file_indx = n

  WRITE (*,*) "get_sat_file: sat_file_indx, sat_ncid = ", sat_file_indx, sat_ncid

END SUBROUTINE get_sat_file
  
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE find_sat_indx (time_array, curr_time, tindx)

  USE date_pack
  
  IMPLICIT NONE

  CHARACTER (LEN=*), DIMENSION(:),  INTENT(IN)  :: time_array(:)
  CHARACTER (LEN=*),                INTENT(IN)  :: curr_time
  INTEGER,                          INTENT(OUT) :: tindx

  ! Local variables.

  INTEGER       :: tol = 2700   ! 45 min tolerance
  INTEGER       :: arr_size     ! size of time_array
  INTEGER       :: tdiff        ! time difference in seconds
  INTEGER       :: tdiff1       ! time difference in seconds
  INTEGER       :: tdiff2       ! time difference in seconds
  INTEGER       :: i
  LOGICAL       :: found = .FALSE.

  WRITE (*,*) "in subroutine find_sat_indx ..."

  tindx = 0
  arr_size = size(time_array)
  found = .FALSE.
  i = 0

  find_loop: DO WHILE ( .NOT. found .AND. i < arr_size)

    i = i + 1
    CALL geth_idts (time_array(i)(1:19), curr_time(1:19), tdiff2)
    IF ( tdiff2 < 0 ) CYCLE find_loop  ! loop until after curr_time

    tdiff = tdiff2
    tindx = i
    found = .TRUE.

    IF ( i > 1 ) THEN
      CALL geth_idts (time_array(i-1)(1:19), curr_time(1:19), tdiff1)
      IF ( ABS(tdiff1) < tdiff ) THEN  ! i-1 is closer to current time
        tdiff = ABS(tdiff1)
        tindx = i - 1
      END IF
    END IF    

    IF ( tdiff > tol ) THEN
      tindx = 0
    END IF 

  END DO find_loop

  ! If curr_time is outside the time range of time_array, 
  ! return a negative number.

  IF ( tindx == 0 .AND. i >=  arr_size ) tindx = -999

END SUBROUTINE find_sat_indx

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE init_sat ( sat_file_ID )

!-------------------------------------------------------------------------------
! This subroutine initializes satellite variables
!
! It opens an existing satellite file (NetCDF),
! reads one of the variables, and gets the dimensions.
! It then checks to verify that the dimensions agree
! with met. dimensions.
! Variables to be set are:
! sat_nx, sat_ny, sat_nt
! allocate memory for arrays to be used
! making sure all the variables are in the file.
! get varIDs: timeID, timestrID, xID, yID,
!             lonID, latID, cldfracID, cldtrID,
!             cldtoptID, cldtoppID, sfcinsolID,
!             skintID, sfcalbID
! And finally read the following variables:
!     sat_time, sat_time_str
!     sat_x, sat_y, sat_lon, sat_lat
!-------------------------------------------------------------------------------

  USE metinfo,  ONLY: mm5_nx => met_nx, mm5_ny => met_ny
  USE mcipparm, ONLY: ncols_x, nrows_x
  USE netcdf

  IMPLICIT NONE

  INTEGER, INTENT (IN)          :: sat_file_ID

  INTEGER                       :: ncID
  INTEGER                       :: istat
  CHARACTER (LEN=256)           :: path         ! file name
  INTEGER                       :: varid        ! variable id
  CHARACTER (LEN=40)            :: dimname      ! dimension name

  INTEGER, DIMENSION(nf90_max_var_dims) :: dimIDs

  INTEGER                       :: timestr_size
  CHARACTER (LEN=24), ALLOCATABLE :: lcl_time_str(:)
  INTEGER                       :: i, j
  INTEGER                       :: v1dimids(10), v2dimids(10)

!-------------------------------------------------------------------------------
! Open NetCDF file.
!-------------------------------------------------------------------------------

  WRITE (*,*) "in subroutine init_sat ..."

  IF ( sat_NCID > 0 ) THEN
    istat = nf90_close (sat_ncid)
    CALL my_check_stat (istat, "nf_close")
  END IF

!~~~Use UPCASE from I/O API module M3UTILIO.  TLO 12 Aug 2011
!!!  path=upcase(TRIM(file_sat(sat_file_ID)))
  path = TRIM(file_sat(sat_file_ID))
  CALL upcase (path)
  IF ( path == "NO_FILE" .OR. path == " " ) then
    WRITE (*,*) "INIT_SAT: NO SATELLITE FILE TO PROCESS"
    lsat = 0
    RETURN
  END IF

  path = TRIM(file_sat(sat_file_ID))
  
  WRITE (*,*) path

  istat = nf90_open (path, nf90_nowrite, sat_ncid)
  CALL my_check_stat (istat, "nf_open")

  ncID = sat_ncID
  
!-------------------------------------------------------------------------------
! Get dimensions from a 3D variable (CLD_TR) in the netCDF file.
!-------------------------------------------------------------------------------

  ! CLD_TR is a 3-dimensional variable (X,Y,TIME) in this file

  istat = nf90_inq_varid        (ncid, "CLD_TR", VarId)
  istat = nf90_inquire_variable (ncid, VarId, dimids=v1dimIDs)
  CALL my_check_stat (istat,"failed to get dims for CLD_TR")

  ! TIME_STR is a 2-dimensional variable (TIME_STR,TIME) in this file
  ! where time_str is the length of the character string for time.

  istat = nf90_inq_varid        (ncid, "TIME_STR", VarId)
  istat = nf90_inquire_variable (ncid, VarId, dimids=v2dimIDs)
  CALL my_check_stat (istat,"failed to get dims for TIME_STR")

  dimIDs(1) = v1dimIDs(1)       ! dim ID for X
  dimIDs(2) = v1dimIDs(2)       ! dim ID for Y
  dimIDs(3) = v1dimIDs(3)       ! dim ID for TIME
!             v2dimIDs(1)	! dim ID for TIME_STR (# of characters)
!             v2dimIDs(2)	! dim ID for TIME
  istat = nf90_inquire_dimension (ncid, dimIDs(1), dimname, sat_nx)
  WRITE (*,*) " dimid: ",dimids(1), ", name: ", trim(dimname), ", len: ", sat_nx 
  istat = nf90_inquire_dimension (ncid, dimIDs(2), dimname, sat_ny)
  WRITE (*,*) " dimid: ",dimids(2), ", name: ", trim(dimname), ", len: ", sat_ny 
  istat = nf90_inquire_dimension (ncid, dimIDs(3), dimname, sat_nt)
  WRITE (*,*) " dimid: ",dimids(3), ", name: ", trim(dimname), ", len: ", sat_nt 
  istat = nf90_inquire_dimension (ncid, v2dimids(1), dimname, timestr_size)
  WRITE (*,*) " dimid: ",v2dimids(1), ", name: ", trim(dimname), ", len: ", &
              timestr_size 

  CALL my_check_stat (istat, "nf_inq_dim")
  IF (timestr_size < 16) THEN
    CALL my_check_stat (1,"TIME_STR too short")
  END IF
  IF ( sat_nx /= mm5_nx-1 ) THEN
    WRITE(*,*) " Sat. X dim: ", sat_nx
    WRITE(*,*) " MM5  X dim: ", mm5_nx
    CALL my_check_stat (1,"sat. X dimension /= MM5 X dimension")
  END IF
  IF ( sat_ny /= mm5_ny-1 ) THEN
    WRITE (*,*) " Sat. Y dim: ", sat_ny
    WRITE (*,*) " MM5  Y dim: ", mm5_ny
    CALL my_check_stat (1,"sat. Y dimension /= MM5 Y dimension")
  END IF

!-------------------------------------------------------------------------------
! Allocate memory for arrays.
!-------------------------------------------------------------------------------

  IF ( ALLOCATED (lcl_time_str) ) DEALLOCATE ( lcl_time_str ) ! local
  ALLOCATE ( lcl_time_str(sat_nt) ) ! local

  IF ( .NOT. ALLOCATED (sat_x)   ) ALLOCATE ( sat_x   (sat_nx)         )
  IF ( .NOT. ALLOCATED (sat_y)   ) ALLOCATE ( sat_y   (        sat_ny) )
  IF ( .NOT. ALLOCATED (sat_lon) ) ALLOCATE ( sat_lon (sat_nx, sat_ny) )
  IF ( .NOT. ALLOCATED (sat_lat) ) ALLOCATE ( sat_lat (sat_nx, sat_ny) )

  CALL allocate_sat_time    ( sat_nt )
  CALL allocate_sat_private ( sat_nx, sat_ny )

!-------------------------------------------------------------------------------
! Get variable IDs.
!-------------------------------------------------------------------------------

  istat = nf90_inq_varid (ncid, "TIME",           timeId)
  istat = nf90_inq_varid (ncid, "TIME_STR",       timestrId)
  istat = nf90_inq_varid (ncid, "X",              xId)
  istat = nf90_inq_varid (ncid, "Y",              yId)
  istat = nf90_inq_varid (ncid, "LON",            lonId)
  istat = nf90_inq_varid (ncid, "LAT",            latId)
  istat = nf90_inq_varid (ncid, "CLD_FRACTION",   cldfracId)
  istat = nf90_inq_varid (ncid, "CLD_ALB",        cldalbId)
  istat = nf90_inq_varid (ncid, "CLD_TR",         cldtrId)
  istat = nf90_inq_varid (ncid, "CLD_TOP_T",      cldtoptId)
  istat = nf90_inq_varid (ncid, "CLD_TOP_P",      cldtoppId)
  istat = nf90_inq_varid (ncid, "SFC_INSOLATION", sfcinsolId)
  istat = nf90_inq_varid (ncid, "SKIN_T",         skintId)
  istat = nf90_inq_varid (ncid, "SFC_ALB",        sfcalbId)
  
  CALL my_check_stat (istat, "nf_inq_varid in init_GOES_data_nc")
   
!-------------------------------------------------------------------------------
! Now read time-invariant variables.
!-------------------------------------------------------------------------------

  istat = nf90_get_var (ncid, timeID, sat_time)
  CALL my_check_stat (istat, "nf_get_var, getting sat_time")

  istat = nf90_get_var (ncid, timestrID, lcl_time_str)
  CALL my_check_stat (istat, "nf_get_var, getting lcl_time_str")

  istat = nf90_get_var (ncid, xID, sat_x)
  CALL my_check_stat (istat, "nf_get_var, getting sat_x")

  istat = nf90_get_var (ncid, yID, sat_y)
  CALL my_check_stat (istat, "nf_get_var, getting sat_y")

  istat = nf90_get_var (ncid, lonID, sat_lon)
  CALL my_check_stat (istat, "nf_get_var, getting sat_lon")

  istat = nf90_get_var (ncid, latID, sat_lat)
  CALL my_check_stat (istat, "nf_get_var, getting sat_lat")

!-------------------------------------------------------------------------------
! Build the time character string (yyyy-mm-dd_hh:mm:ss.ssss).
!-------------------------------------------------------------------------------

  DO i = 1, sat_nt
    sat_time_str(i) = CHAR(0)
    DO j = 1, 16
     sat_time_str(i)(j:j) = lcl_time_str(i)(j:j)
    END DO
    sat_time_str(i)(17:24) = ":00.0000"
!   write(*,*) "i, sat_time_str: ", i, sat_time_str(i)
  END DO 
    
  DEALLOCATE ( lcl_time_str )

  WRITE (*,*) "init_sat: ncid, sat_ncid = ", ncid, sat_ncid

END SUBROUTINE init_sat

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE readsat (sat_time_indx)
 
!-------------------------------------------------------------------------------
! Reads the satellite data for the time index sat_time_indx 
! corresponding to time mcip_now, and then copy the data
! into mcip_X grids.
!------------------------------------------------------------------------------- 

  USE m3utilio, ONLY: badval3, imiss3  ! badval3 = -9.999E36
  USE mcipparm, ONLY: nx => ncols_x, ny => nrows_x, &
                      x_offset => x0, y_offset => y0 
  USE xvars,    ONLY: xlonc, xlatc
  USE netcdf

  IMPLICIT NONE
  
  INTEGER, INTENT (IN)  :: sat_time_indx
  
  INTEGER               :: ncID  ! netCDF file ID

  INTEGER               :: istat = 0
  
  INTEGER               :: sx, ex, sy, ey
  INTEGER               :: i, j, L

  REAL                  :: diff
  REAL, PARAMETER       :: tol = 1.e-1

!-------------------------------------------------------------------------------
! Return if no sat. input is required.  LSAT is user input defined in mcipparm.
!-------------------------------------------------------------------------------

  IF ( lsat /= 1 ) THEN
    WRITE (*,*) "READSAT: lsat /= 1, I should not be here..."
    CALL graceful_stop ("READSAT")
  END IF

  ! Return if no sat. data for this time.

  IF ( sat_time_indx == 0 ) THEN
    WRITE (*,*) "READSAT: No sat. data for this time, returning ..."
    RETURN
  END IF

  WRITE (*,*) "in subroutine readsat ..."
  WRITE (*,*) "sat. time index = ",sat_time_indx
  WRITE (*,*) "ncID, sat_ncid, cldfracID = ", ncid, sat_ncid, cldfracID
  ncid = sat_ncid

!-------------------------------------------------------------------------------
! Read in time-varying satellite data
!-------------------------------------------------------------------------------

  CALL allocate_sat_private ( sat_nx, sat_ny )
  CALL init_sat_private
  
  L = sat_time_indx  ! time index for the current time
  WRITE (*,*) "reading satellite data for time: ", sat_time_str(L)

  istat = nf90_get_var (ncid, cldfracID, sat_cldfrac)
  CALL my_check_stat (istat, "nf_get_var_real, getting sat_cldfrac")

  istat = nf90_get_var (ncid, cldalbID, sat_cldalb)
  CALL my_check_stat (istat, "nf_get_var_real, getting sat_cldalb")

  istat = nf90_get_var (ncid, cldtrID, sat_cldtr)
  CALL my_check_stat (istat, "nf_get_var_real, getting sat_cldtr")

  istat = nf90_get_var (ncid, cldtoptID, sat_cldtopt)
  CALL my_check_stat (istat, "nf_get_var_real, getting sat_cldtopt")

  istat = nf90_get_var (ncid, cldtoppId, sat_cldtopp)
  CALL my_check_stat (istat, "nf_get_var_real, getting sat_cldtopp")

  istat = nf90_get_var (ncid, sfcinsolID, sat_sfcinsol)
  CALL my_check_stat (istat, "nf_get_var_real, getting sat_sfcinsol")

  istat = nf90_get_var (ncid, skintID, sat_skint)
  CALL my_check_stat (istat, "nf_get_var_real, getting sat_skint")

  istat = nf90_get_var (ncid, sfcalbID, sat_sfcalb)
  CALL my_check_stat (istat, "nf_get_var_real, getting sat_sfcalb")

  ! Set cloud flag.

  DO j = 1, sat_ny
    DO i = 1, sat_nx
      sat_cldflg(i,j) = 0
      IF ( sat_sfcinsol(i,j) < 0   ) sat_cldflg(i,j) = imiss3
      IF ( sat_cldfrac(i,j)  > 0.0 ) sat_cldflg(i,j) = 1
    END DO
  END DO

!-------------------------------------------------------------------------------
! Allocate memory for x-domain arrays.
! NCOLS_X and NROWS_X are cross point dimensions for X and Y.
! For MCIP X domain (ncols+2*nthik, nrows+2*nthik,
! where ncols and nrows are output dimensions) 
!-------------------------------------------------------------------------------

  CALL allocate_sat_public (ncols_x, nrows_x)

!-------------------------------------------------------------------------------
! Put time-variant cross-point arrays on MCIP_X grid.
!-------------------------------------------------------------------------------

  sx = x_offset
  ex = sx + ncols_x - 1
  sy = y_offset
  ey = sy + nrows_x - 1

  xsat_cldflg (:,:) = sat_cldflg (sx:ex,sy:ey)
  xsat_cldfrac(:,:) = sat_cldfrac(sx:ex,sy:ey)
  xsat_cldtr  (:,:) = sat_cldtr  (sx:ex,sy:ey)
  xsat_cldtopt(:,:) = sat_cldtopt(sx:ex,sy:ey)
  xsat_cldtopp(:,:) = sat_cldtopp(sx:ex,sy:ey)*100.0  ! convert mb to Pa

  xsat_lon    (:,:) = sat_lon    (sx:ex,sy:ey)
  xsat_lat    (:,:) = sat_lat    (sx:ex,sy:ey)

  i = ncols_x / 2
  j = nrows_x / 2
  diff = ABS(xsat_lon(i,j)-xlonc(i,j))
  IF (diff > tol) THEN
    WRITE (*,*) " MCIP (i,j,lon): ", i, j, xlonc(i,j)
    WRITE (*,*) " SAT  (i,j,lon): ", i, j, xsat_lon(i,j)
    CALL my_check_stat (1, "sat. lon does not match MM5 lon")
  END IF
  
  diff = ABS(xsat_lat(i,j)-xlatc(i,j))
  IF ( diff > tol ) THEN
    WRITE (*,*) " MCIP (i,j,LAT): ", i, j, xlatc(i,j)
    WRITE (*,*) " SAT  (i,j,LAT): ", i, j, xsat_lat(i,j)
    CALL my_check_stat (1, "sat. lat does not match MM5 lat")
  END IF

!-------------------------------------------------------------------------------
! Deallocate memory for the unused original arrays.
!-------------------------------------------------------------------------------
  
  CALL deallocate_sat_private

END SUBROUTINE readsat

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE satvars2ctm (mcip_now)

!-------------------------------------------------------------------------------
! This subroutine adds cloud transmissivity to MCIP when
! satellite data is available and corrects cloud top & bottom heights
! and cloud fraction based on satellite observation.
!-------------------------------------------------------------------------------

  USE m3utilio, ONLY: badval3  ! badval3 = -9.999E36
  USE mcipparm, ONLY: ncols_x, nrows_x, metlay
  USE xvars,    ONLY: xdx3htf, xtempm,  xpresm,  xpresf, xwvapor, &
                      xcldtop, xcldbot, xcfract 

  IMPLICIT NONE

  CHARACTER (LEN=24), INTENT(IN) :: mcip_now
  
  INTEGER                        :: sat_indx
  INTEGER                        :: c, r, k, l
  INTEGER                        :: k1000 = 0
  INTEGER                        :: ktop, kbase
  REAL                           :: ctop, cbase
  REAL                           :: grid_z(metlay)
  REAL                           :: tlcl  ! temp. at LCL (K)
  REAL                           :: zlcl  ! height of LCL (m)

  ! dalr: dry adiabatic lapse rate [ 9.7597226E-3 K/m ]
  REAL, PARAMETER                :: dalr  = 9.7597226e-3

  CHARACTER (LEN=16), PARAMETER  :: pname = 'SATVARS2CTM'

!-------------------------------------------------------------------------------
! Return if no sat. input is required.  LSAT is user input defined in mcipparm.
!-------------------------------------------------------------------------------

  IF ( lsat /= 1 ) THEN
    WRITE (*,*) "satvars2ctm: lsat /= 1, I should not be here..."
    RETURN
  END IF

!-------------------------------------------------------------------------------
! Allocate memory for variables if necessary.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED (xcldtr) ) then
    ALLOCATE (xcldtr(ncols_x, nrows_x))
  END IF

  CALL find_sat_indx (sat_time_str, mcip_now, sat_indx) 

  xcldtr = badval3
  IF ( sat_indx > 0 ) THEN  ! replace xvars with satvars

    !---------------------------------------------------------------------------
    ! Loop over grid points and replace cloud information.
    !---------------------------------------------------------------------------

    WRITE (*,*) " Replacing MCIP cloud info with satellite observation"
    WRITE (*,*) " MCIP time: ", mcip_now
    WRITE (*,*) " SAT  time: ", sat_time_str(sat_indx)
  
    L = sat_indx
    DO c = 1, ncols_x
      DO r = 1, nrows_x

      IF ( xsat_cldtr(c,r)   >= 0 ) THEN  ! good data continue

      IF ( xsat_cldfrac(c,r) > 0 .AND. &
           xsat_cldtopp(c,r) > 0 .AND. &
           xsat_cldtopp(c,r) < 1.0e5 ) THEN  ! grid is cloudy
          
      ! find sat. clod top, and base
      ! Look for cloud top and base layer up and down from level of max RH.


        ! Build array of layer heights

        grid_z(1) = xdx3htf(c,r,1)
        DO k = 2, metlay
          grid_z(k) = grid_z(k-1) + xdx3htf(c,r,k)
          IF ( grid_z(k) < 1000.0 ) k1000 = k
        END DO

        top: DO k = 2, metlay
          ktop = k
          IF (grid_z(k) >= 10000.) EXIT top
!         IF ( xtempm (c,r,k) <= xsat_cldtopt(c,r) ) EXIT top
!         IF ( xpresm (c,r,k) <= xsat_cldtopp(c,r) ) EXIT top
          IF ( xpresf (c,r,k) <= xsat_cldtopp(c,r) ) EXIT top
        ENDDO top

!       estimate lifting condensation level (LCL) by estimating
!       the condensation temperature (tlcl) and calculating
!       the corresponding height based on dry adiabatic lapse rate (K/m)

        tlcl = t_condensation( xwvapor(c,r,1), xtempm(c,r,1), &
                                               xpresm(c,r,1)  )
        zlcl = (xtempm(c,r,1)-tlcl)/dalr

        cbase = 0.0
        bottom: DO k = 1, metlay
          kbase = k
          IF ( zlcl < grid_Z(k) ) EXIT bottom
!         IF ( tlcl > xtempm(c,r,k) ) EXIT bottom
!         IF ( grid_Z(k) > 1000. ) EXIT bottom	! Cloud base set at 1-km
        ENDDO bottom

        IF (kbase < k1000) kbase = k1000
        IF (kbase >= ktop) kbase = ktop-1

        IF ( grid_Z(ktop) < 1000.0 ) THEN
          cbase = 0.0
          ctop  = 0.0
          xsat_cldfrac(c,r) = 0.0
          xsat_cldtr(c,r)   = 1.0
        ELSE
          cbase = grid_Z(kbase)
          ctop  = grid_Z(ktop)
        END IF

        xcldtop(c,r) = ctop
        xcldbot(c,r) = cbase
        xcfract(c,r) = xsat_cldfrac(c,r)
        xcldtr (c,r) = xsat_cldtr(c,r)

!       write(*,'(A, 3(f6.0,1x),e11.4)') 'cbase, ctop, cldfrac, cldtr: ',cbase, ctop, &
!             xsat_cldfrac(c,r), xsat_cldtr(c,r)           

      ELSE IF ( xsat_cldfrac(c,r) == 0 ) THEN  ! clear sky values

        xcldtop(c,r) = 0.0
        xcldbot(c,r) = 0.0
        xcfract(c,r) = 0.0
        xcldtr (c,r) = 1.0

      END IF    ! end of cloud fraction check

      END IF    ! end of valid sat. data check

      ENDDO
    ENDDO

  END IF

END SUBROUTINE satvars2ctm

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE my_check_stat(istat,msg)

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: istat
  CHARACTER (LEN=*)   :: msg

  IF ( istat /= 0 ) THEN
    WRITE (*,*) " ***********"
    WRITE (*,*) " error: ", trim(msg)
!     stop
    CALL graceful_stop ("sat2mcip")
  END IF

END SUBROUTINE my_check_stat

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

REAL FUNCTION t_condensation (wvapor, t0, p0)

  IMPLICIT NONE

  REAL, INTENT(IN)  :: wvapor  ! water vapor mixing ratio
  REAL, INTENT(IN)  :: t0      ! surface layer temperature (K)
  REAL, INTENT(IN)  :: p0      ! surface pressure (pa)

  ! Declare the constants.

  ! ratio of mol wt of water vapor to mol wt of air [ 0.62201513666 ]
  ! mvoma = mwwat /mwair , declared in const_mete
  ! gas constant over Cp [ 0.28571428571 ]
  ! rovcp = rdgas / cpd

  ! mean molecular weight for water vapor [ g/mol ]
  REAL, PARAMETER :: mwwat = 18.0153
  ! mean molecular weight for dry air [ g/mol ]
  ! -- 78.06% N2, 21% O2, and 0.943% A on a mole fraction basis
  !    (Source: Hobbs, 1995, pp. 69-70)
  REAL, PARAMETER :: mwair = 28.9628
  REAL, PARAMETER :: mvoma = mwwat /mwair  ! 0.622015

  ! universal gas constant [ J/mol-K ]
  REAL, PARAMETER :: rgasuniv = 8.314510
  ! dry-air gas constant [ 287.07548994 J/kg-K ]
  REAL, PARAMETER :: rdgas = 1.0e3 * rgasuniv / mwair
  ! specific heat of dry air at constant pressure [ 1004.7642148 J/kg-K ]
  REAL, PARAMETER :: cpd = 7.0 * rdgas / 2.0
  ! rovcp = rdgas / cpd [ K ]
  REAL, PARAMETER :: rovcp = rdgas / cpd

    
  REAL, PARAMETER :: A = 2.53E11           ! empirical constant (Pa)
  REAL, PARAMETER :: B = 5.42E03           ! empirical constant (K)
  REAL, PARAMETER :: cpovr = 1.0 / rovcp   ! 1/k

  REAL, PARAMETER :: tol = 1.0e-02    ! tolerance

  REAL    :: delta, tnew, told
  REAL    :: dum1, dum2
  INTEGER :: counter 

  ! Using the equation (2.33) from page 21
  ! A Short Course in Cloud Physics by R.R. Rogers & M.K. Yau,
  ! condensation temperature is calculated.

  ! We use fixed point iteration for simplicity.

  delta   = 1.0
  counter = 0
  told    = t0
  tnew    = 0.0

  DO WHILE ( delta > tol .AND. counter < 20 )
    counter = counter+1
    dum1    = (A*mvoma)/(wvapor*p0)
    dum2    = (t0/told)**cpovr
    tnew    = B / LOG(dum1*dum2)   
    delta   = ABS(tnew-told)
    told    = tnew
  END DO

  t_condensation = tnew

END FUNCTION t_condensation

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE allocate_sat_private (nx, ny)
  
  IMPLICIT NONE
  
  INTEGER, INTENT (IN) :: nx, ny

  IF ( ALLOCATED (sat_cldflg)   ) DEALLOCATE ( sat_cldflg   )
                                  ALLOCATE   ( sat_cldflg   (nx, ny) )
  IF ( ALLOCATED (sat_cldfrac)  ) DEALLOCATE ( sat_cldfrac  )
                                  ALLOCATE   ( sat_cldfrac  (nx, ny) )
  IF ( ALLOCATED (sat_cldalb)   ) DEALLOCATE ( sat_cldalb   )
                                  ALLOCATE   ( sat_cldalb   (nx, ny) )
  IF ( ALLOCATED (sat_cldtr)    ) DEALLOCATE ( sat_cldtr    )
                                  ALLOCATE   ( sat_cldtr    (nx, ny) )
  IF ( ALLOCATED (sat_cldtopt)  ) DEALLOCATE ( sat_cldtopt  )
                                  ALLOCATE   ( sat_cldtopt  (nx, ny) )
  IF ( ALLOCATED (sat_cldtopp)  ) DEALLOCATE ( sat_cldtopp  )
                                  ALLOCATE   ( sat_cldtopp  (nx, ny) )
  IF ( ALLOCATED (sat_sfcinsol) ) DEALLOCATE ( sat_sfcinsol )
                                  ALLOCATE   ( sat_sfcinsol (nx, ny) )
  IF ( ALLOCATED (sat_skint)    ) DEALLOCATE ( sat_skint    )
                                  ALLOCATE   ( sat_skint    (nx, ny) )
  IF ( ALLOCATED (sat_sfcalb)   ) DEALLOCATE ( sat_sfcalb   )
                                  ALLOCATE   ( sat_sfcalb   (nx, ny) )

END SUBROUTINE allocate_sat_private

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  
SUBROUTINE init_sat_private
  
  USE m3utilio, ONLY: badval3, imiss3  ! badval3 = -9.999E36, imiss3 = -9999

  IMPLICIT NONE
  
  ! Initialize private arrays.

  sat_cldflg   = imiss3
  sat_cldfrac  = badval3
  sat_cldalb   = badval3
  sat_cldtr    = badval3
  sat_cldtopt  = badval3
  sat_cldtopp  = badval3
  sat_sfcinsol = badval3
  sat_skint    = badval3
  sat_sfcalb   = badval3

END SUBROUTINE init_sat_private

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  
SUBROUTINE deallocate_sat_private
  
  IMPLICIT NONE

  IF ( ALLOCATED (sat_cldflg)   ) DEALLOCATE ( sat_cldflg )
  IF ( ALLOCATED (sat_cldfrac)  ) DEALLOCATE ( sat_cldfrac )
  IF ( ALLOCATED (sat_cldalb)   ) DEALLOCATE ( sat_cldalb )
  IF ( ALLOCATED (sat_cldtr)    ) DEALLOCATE ( sat_cldtr )
  IF ( ALLOCATED (sat_cldtopt)  ) DEALLOCATE ( sat_cldtopt )
  IF ( ALLOCATED (sat_cldtopp)  ) DEALLOCATE ( sat_cldtopp )
  IF ( ALLOCATED (sat_sfcinsol) ) DEALLOCATE ( sat_sfcinsol )
  IF ( ALLOCATED (sat_skint)    ) DEALLOCATE ( sat_skint )
  IF ( ALLOCATED (sat_sfcalb)   ) DEALLOCATE ( sat_sfcalb )

END SUBROUTINE deallocate_sat_private

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  
SUBROUTINE allocate_sat_public ( nx, ny )
  
  IMPLICIT NONE
  
  INTEGER, INTENT (IN) :: nx, ny

  IF ( ALLOCATED (xsat_lon)     ) DEALLOCATE ( xsat_lon     )
                                  ALLOCATE   ( xsat_lon     (nx, ny) )
  IF ( ALLOCATED (xsat_lat)     ) DEALLOCATE ( xsat_lat     )
                                  ALLOCATE   ( xsat_lat     (nx, ny) )
  IF ( ALLOCATED (xsat_cldflg)  ) DEALLOCATE ( xsat_cldflg  )
                                  ALLOCATE   ( xsat_cldflg  (nx, ny) )
  IF ( ALLOCATED (xsat_cldfrac) ) DEALLOCATE ( xsat_cldfrac )
                                  ALLOCATE   ( xsat_cldfrac (nx, ny) )
  IF ( ALLOCATED (xsat_cldtr)   ) DEALLOCATE ( xsat_cldtr   )
                                  ALLOCATE   ( xsat_cldtr   (nx, ny) )
  IF ( ALLOCATED (xsat_cldtopt) ) DEALLOCATE ( xsat_cldtopt )
                                  ALLOCATE   ( xsat_cldtopt (nx, ny) )
  IF ( ALLOCATED (xsat_cldtopp) ) DEALLOCATE ( xsat_cldtopp )
                                  ALLOCATE   ( xsat_cldtopp (nx, ny) )

END SUBROUTINE allocate_sat_public

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  
SUBROUTINE deallocate_sat_public
  
  IMPLICIT NONE
  
  IF ( ALLOCATED (xsat_lon)     ) DEALLOCATE ( xsat_lon     )
  IF ( ALLOCATED (sat_time)     ) DEALLOCATE ( xsat_lat     )
  IF ( ALLOCATED (xsat_cldflg)  ) DEALLOCATE ( xsat_cldflg  )
  IF ( ALLOCATED (xsat_cldfrac) ) DEALLOCATE ( xsat_cldfrac )
  IF ( ALLOCATED (xsat_cldtr)   ) DEALLOCATE ( xsat_cldtr   )
  IF ( ALLOCATED (xsat_cldtopt) ) DEALLOCATE ( xsat_cldtopt )
  IF ( ALLOCATED (xsat_cldtopp) ) DEALLOCATE ( xsat_cldtopp )

END SUBROUTINE deallocate_sat_public

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  
SUBROUTINE allocate_sat_time (sat_nt)
  
  IMPLICIT NONE
  
  INTEGER, INTENT (IN) :: sat_nt

  IF ( ALLOCATED (sat_time)     )  DEALLOCATE ( sat_time )
                                   ALLOCATE   ( sat_time (sat_nt) )
  IF ( ALLOCATED (sat_time_str) )  DEALLOCATE ( sat_time_str )
                                   ALLOCATE   ( sat_time_str (sat_nt) )

END SUBROUTINE allocate_sat_time

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE deallocate_sat_time
  
  IMPLICIT NONE

  DEALLOCATE ( sat_time     )
  DEALLOCATE ( sat_time_str )

END SUBROUTINE deallocate_sat_time

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

END MODULE sat2mcip
