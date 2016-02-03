MODULE sat2mcip

use mcipparm	! metcol, metrow, ncols_x, nrows_x, ncols, nrows, nlays
use xvars	! xlatc, xlonc, xcfract, xcldtop, xcldbot

!-------------------------------------------------------------------------------
! IMPORTANT GRID DEFINITIONS USED IN THE FOLLOWING:
! (in mcipparm)
! =================================================

!-------------------------------------------------------------------------------
! Dimensions of CTM domain.
!-------------------------------------------------------------------------------
! INTEGER            :: ncols         ! number of grid columns (X direction)
! INTEGER            :: nrows         ! number of grid rows (Y direction)
! INTEGER            :: nlays         ! number of vertical layers
!-------------------------------------------------------------------------------
! Horizontal dimensions of "X" domain (CTM + BNDARY area).
!-------------------------------------------------------------------------------
! INTEGER            :: nrows_x
! INTEGER            :: ncols_x
!-------------------------------------------------------------------------------
! Dimensions for MET input data.
!-------------------------------------------------------------------------------
! INTEGER            :: metrow      ! met. grid dimension for rows (N-S)
! INTEGER            :: metcol      ! met. grid dimension for columns (E-W)
! INTEGER            :: metlay      ! met. grid dimension for layers
!-------------------------------------------------------------------------------
!
!                    x_offset => x0, y_offset => y0 
!
! IF ( nbdrytrim >= 0 ) THEN
!   x0 = nbdrytrim + 1
!   y0 = nbdrytrim + 1
! ENDIF!
!
! metcol = met_nx
! metrow = met_ny
! metlay = met_nz
!
! IF ( nbdrytrim >= 0 ) THEN  ! not windowing...need to define NCOLS, NROWS
!   ncols = met_nx - (2 * nbdrytrim) - (2 * nthik) - 1
!   nrows = met_ny - (2 * nbdrytrim) - (2 * nthik) - 1
! ENDIF
!
! nrows_x = nrows + 2 * nthik
! ncols_x = ncols + 2 * nthik
!
! nbndy   = 2 * nthik  * (ncols + nrows + 2*nthik)
! nbndyd  = 2 * nthikd * (ncols + nrows + 2*nthikd)
!
!-------------------------------------------------------------------------------

IMPLICIT NONE
SAVE

!-------------------------------------------------------------------------------
! Run Options.
!-------------------------------------------------------------------------------
!
! Additional namelist variables for sat. photo adjustments
!
INTEGER,       PARAMETER        :: max_sat  = 100
CHARACTER (len=256), public     :: file_sat ( max_sat )

INTEGER, public                 :: lsat ! user input: 
                                        !   0 = no sat. photo adjustment
                                        !   1 =    sat. photo adjustment

!-------------------------------------------------------------------------------
!
! variables that will be used in MCIP
! These are a subset of available data from sat2mcip file
!
!-------------------------------------------------------------------------------

integer, public			:: sat_nx, sat_ny, sat_nt
real, public, allocatable	:: sat_time(:)	! secs from MM5 T0
character (len=24), public,     &
              allocatable	:: sat_time_str(:) ! time: yyyy-mm-dd_hh:mm:ss.ssss

real, public, allocatable	:: xsat_lon(:,:)
real, public, allocatable	:: xsat_lat(:,:)
integer, public, allocatable	:: xsat_cldflg(:,:)
real, public, allocatable	:: xsat_cldfrac(:,:)
real, public, allocatable	:: xsat_cldtr(:,:)
real, public, allocatable	:: xsat_cldtopt(:,:)
real, public, allocatable	:: xsat_cldtopp(:,:)

real, public			:: mm5_time_now	! mm5 time, min. from T0

!-------------------------------------------------------------------------------
! apb, Oct. 2007,
! Cloud Transmissivity Array to be used in metcro.F program (output)
!-------------------------------------------------------------------------------

  REAL, PUBLIC, ALLOCATABLE     :: xcldtr   ( : , : )     ! X   grid
  REAL, PUBLIC, ALLOCATABLE     :: cldtr_c  ( : , : )     ! CTM grid


!-------------------------------------------------------------------------------
!
! variables in sat2mcip file (private to sat2mcip)
! 
!-------------------------------------------------------------------------------

! dimensions KX, KY, and NT were defined in the public section
! TIME was defined in the public section
! TIME_STR was defined in the public section
real, private, allocatable, save:: sat_x(:)
real, private, allocatable, save:: sat_y(:)
real, private, allocatable, save:: sat_lon(:,:)
real, private, allocatable, save:: sat_lat(:,:)
real, private, allocatable	:: sat_cldfrac(:,:)
real, private, allocatable	:: sat_cldalb(:,:)
real, private, allocatable	:: sat_cldtr(:,:)
real, private, allocatable	:: sat_cldtopt(:,:)
real, private, allocatable	:: sat_cldtopp(:,:)
real, private, allocatable	:: sat_sfcinsol(:,:)
real, private, allocatable	:: sat_skint(:,:)
real, private, allocatable	:: sat_sfcalb(:,:)

! 
! variables used in this module
!
integer, private, allocatable	:: sat_cldflg(:,:)

integer, private, save          :: sat_ncId
integer, private, save          :: xID, yID, timeID, timestrID
integer, private, save          :: lonID, latID
integer, private, save          :: cldfracID, cldalbID, cldtrID
integer, private, save          :: cldtoptID, cldtoppID
integer, private, save          :: sfcinsolID, skintID, sfcalbID

!
!-------------------------

CONTAINS

!-------------------------------------------------------------------------------
! SUBROUTINES AND FUNCTIONS WILL FOLLOW
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

  subroutine getsat ( mcip_now )
!
! This subroutine reads in satellite data for time mcip_now
!
! CALLS:
!       find_sat_indx
!       init_sat  
!
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'

! input variables
  CHARACTER (LEN=*), INTENT (IN) :: mcip_now   ! YYYY-MO-DD-HH:MI:SS.SSSS

  
  logical, save	        :: first_time=.true.
  integer               :: istat=0
  
  integer, save         :: sat_file_indx = 0
  integer, save         :: sat_time_indx = 0
  
!-------------------------

 write(*,*) "in subroutine getsat ..."

!------------------------------------------------------------------------------
! return if no sat. input is required.
! lsat is user input defined in mcipparm
!------------------------------------------------------------------------------
  if (lsat /= 1) then
    write(*,*) " lsat /= 1, I should not be here..."
    CALL graceful_stop ("GETSAT")
  end if

!------------------------------------------------------------------------------
! get the satellite file that contains the data for this time.
!------------------------------------------------------------------------------

! find the correct satellite file, init time string, return time_index
  call get_sat_file ( mcip_now, sat_file_indx, sat_time_indx )

  write(*,*) "getsat: sat_ncid = ", sat_ncid

  if ( sat_file_indx == 0 ) then
    write(*,*) "GETSAT: no satellite data for this time"
    write(*,*) "        time: ", mcip_now(1:24)
    if ( LSAT > 0 ) then
      write(*,*) "GETSAT: ERROR -- LSAT not zero"
      write(*,*) "GETSAT:          LSAT set to zero"
      LSAT = 0
    end if
    RETURN
  end if

!------------------------------------------------------------------------------
! Read the satellite data for this time and
! copy it into X domain variables.
!------------------------------------------------------------------------------

  call readsat ( sat_time_indx )

! 
  end subroutine getsat  

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  subroutine get_sat_file ( mcip_now, sat_file_indx, sat_time_indx )
  
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
  

  ! declare calling parameters:
  CHARACTER (LEN=*), INTENT (IN)     :: mcip_now
  INTEGER, INTENT (INOUT)            :: sat_file_indx, sat_time_indx

  integer :: n
  logical :: file_was_found = .FALSE.

  write(*,*) "in subroutine get_sat_file ..."

!--------------------------------------------------------------------------
! check if sat file already contains the data for this time
!--------------------------------------------------------------------------
  IF ( ALLOCATED(sat_time_str) ) THEN
!   returns sat_time_indx=-999 if mcip_now is outside the time range
    call find_sat_indx (sat_time_str, mcip_now, sat_time_indx) 
    if (sat_time_indx >= 0) RETURN
  END IF    

!--------------------------------------------------------------------------
! data for mcip_now was not found,
! loop over sat files to find the right file
!--------------------------------------------------------------------------

  sat_file_indx = 0
  n = 0
  file_was_found = .FALSE.
  do while ( .NOT. file_was_found .AND. n <= SIZE(file_sat) )
    n = n+1
    call init_sat ( n )
    if ( LSAT /= 1 ) EXIT
    call find_sat_indx (sat_time_str, mcip_now, sat_time_indx) 
    
    if (sat_time_indx < 0) then
      file_was_found = .FALSE.
    else
      file_was_found = .TRUE.
    end if

  end do
  if ( file_was_found ) sat_file_indx = n

  write(*,*) "get_sat_file: sat_file_indx, sat_ncid = ", sat_file_indx, sat_ncid

  end subroutine get_sat_file
  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  subroutine find_sat_indx (time_array, curr_time, tindx)

  use date_pack
  
  IMPLICIT NONE

!
  CHARACTER (len=*), DIMENSION(:),  INTENT(IN)	:: time_array(:)
  CHARACTER (len=*),                INTENT(IN)	:: curr_time
  integer,                          INTENT(OUT)	:: tindx
!
! local variables
!
  integer	:: tol = 2700	! 45 min tolerance
  integer	:: arr_size	! size of time_array
  integer	:: tdiff	! time difference in seconds
  integer	:: tdiff1	! time difference in seconds
  integer	:: tdiff2	! time difference in seconds
  integer	:: i
  logical	:: found = .false.

!---------------------

  write(*,*) "in subroutine find_sat_indx ..."

  tindx=0
  arr_size = size(time_array)
  found = .false.
  i = 0
  find_loop: do while ( .not. found .and. i < arr_size)
    i=i+1
    call geth_idts (time_array(i), curr_time, tdiff2)
    if (tdiff2 < 0) CYCLE find_loop	! loop until after curr_time

    tdiff = tdiff2
    tindx = i
    found = .true.

    if (i > 1) then
      call geth_idts (time_array(i-1), curr_time, tdiff1)
      if (abs(tdiff1) < tdiff) then	! i-1 is closer to current time
        tdiff = abs(tdiff1)
        tindx = i-1
      end if
    end if    

    if (tdiff > tol) then
      tindx = 0
    end if 

  end do find_loop

!
! if curr_time is outside the time range of time_array, 
!    return a negative number
  if ( tindx == 0 .AND. i >=  arr_size) tindx = -999

  return
  end subroutine find_sat_indx
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

  subroutine init_sat ( sat_file_ID )

!------------------------------------------------------------------------------
!
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
!
!------------------------------------------------------------------------------
!

  use metinfo,  ONLY: mm5_nx => met_nx, mm5_ny => met_ny
  use mcipparm, ONLY: ncols_x, nrows_x

  IMPLICIT NONE
  INCLUDE 'netcdf.inc'

!
!
  integer, intent (in) 		:: sat_file_ID

  integer                       :: ncID
  integer			:: istat
!!!tlo  character (len=80)		:: path		! file name
  character (len=256)		:: path		! file name
  integer			:: varid	! variable id
  character (len=40)		:: dimname	! dimension name

  integer                       :: xDimID, yDimId, timeDimId
  integer                       :: timestrDimId
  integer, dimension(nf_max_var_dims) :: dimIDs

  integer			:: timestr_size
  character (len=24), allocatable :: lcl_time_str(:)
  integer			:: i, j, k, l
  integer			:: v1dimids(10), v2dimids(10)

  write(*,*) "in subroutine init_sat ..."

!--------------------------------------------------------------------------
! open NetCDF file
!--------------------------------------------------------------------------
! path="/rstor/biazar/data/sat_data/processed/sat2mcip.d3.0823_0902.2000.nc"

  IF (sat_NCID > 0) THEN
    istat = nf_close(sat_ncid)
    call my_check_stat (istat, "nf_close")
  END IF

  path=upcase(TRIM(file_sat(sat_file_ID)))
  if ( path == "NO_FILE" .OR. path == " " ) then
    WRITE (*,*) "INIT_SAT: NO SATELLITE FILE TO PROCESS"
    LSAT = 0
    RETURN
  end if

  path = TRIM(file_sat(sat_file_ID))
  
  write(*,*) path

  istat = nf_open(path, nf_nowrite, sat_ncid)
  call my_check_stat(istat,"nf_open")

  ncID = sat_ncID
  
!------------------------------------------------------------------------
!
! get dimensions from a 3D variable (CLD_TR) in the netCDF file
!
!------------------------------------------------------------------------
!_
  ! CLD_TR is a 3-dimensional variable (X,Y,TIME) in this file
  !
  istat = nf_inq_varid(ncid, "CLD_TR", VarId)
  istat = nf_inq_vardimid(ncid, VarId, v1dimIDs)
  call my_check_stat(istat,"failed to get dims for CLD_TR")

  ! TIME_STR is a 2-dimensional variable (TIME_STR,TIME) in this file
  ! where time_str is the length of the character string for time
  !
  istat = nf_inq_varid(ncid, "TIME_STR", VarId)
  istat = nf_inq_vardimid(ncid, VarId, v2dimIDs)
  call my_check_stat(istat,"failed to get dims for TIME_STR")

  dimIDs(1) = v1dimIDs(1)	! dim ID for X
  dimIDs(2) = v1dimIDs(2)	! dim ID for Y
  dimIDs(3) = v1dimIDs(3)	! dim ID for TIME
!             v2dimIDs(1)	! dim ID for TIME_STR (# of characters)
!             v2dimIDs(2)	! dim ID for TIME
  istat = nf_inq_dim(ncid, dimIDs(1), dimname, sat_nx)
  write(*,*) " dimid: ",dimids(1), ", name: ", trim(dimname), ", len: ", sat_nx 
  istat = nf_inq_dim(ncid, dimIDs(2), dimname, sat_ny)
  write(*,*) " dimid: ",dimids(2), ", name: ", trim(dimname), ", len: ", sat_ny 
  istat = nf_inq_dim(ncid, dimIDs(3), dimname, sat_nt)
  write(*,*) " dimid: ",dimids(3), ", name: ", trim(dimname), ", len: ", sat_nt 
  istat = nf_inq_dim(ncid, v2dimids(1), dimname, timestr_size)
  write(*,*) " dimid: ",v2dimids(1), ", name: ", trim(dimname), ", len: ", &
  timestr_size 

  call my_check_stat(istat,"nf_inq_dim")
  if (timestr_size < 16) then
    call my_check_stat(1,"TIME_STR too short")
  end if
  if (sat_nx /= mm5_nx-1) then
    write(*,*) " Sat. X dim: ", sat_nx
    write(*,*) " MM5  X dim: ", mm5_nx
    call my_check_stat(1,"sat. X dimension /= MM5 X dimension")
  end if
  if (sat_ny /= mm5_ny-1) then
    write(*,*) " Sat. Y dim: ", sat_ny
    write(*,*) " MM5  Y dim: ", mm5_ny
    call my_check_stat(1,"sat. Y dimension /= MM5 Y dimension")
  end if

!--------------------------------------------------------------------------
!
! allocate memory for arrays
!
!--------------------------------------------------------------------------
!
  if (ALLOCATED(lcl_time_str)) deallocate( lcl_time_str )	! local
  allocate( lcl_time_str(sat_nt) )	! local

  if ( .not. allocated(sat_x))   allocate( sat_x(sat_nx) )
  if ( .not. allocated(sat_y))   allocate( sat_y(sat_ny) )
  if ( .not. allocated(sat_lon)) allocate( sat_lon(sat_nx,sat_ny) )
  if ( .not. allocated(sat_lat)) allocate( sat_lat(sat_nx,sat_ny) )

  call allocate_sat_time    ( sat_nt )
  call allocate_sat_private ( sat_nx, sat_ny )

!---------------------------------------------------------------------------
!
! get variable IDs
!
!---------------------------------------------------------------------------

  istat = nf_inq_varid(ncid, "TIME",    timeId)
  istat = nf_inq_varid(ncid, "TIME_STR",    timestrId)
  istat = nf_inq_varid(ncid, "X",  xId)
  istat = nf_inq_varid(ncid, "Y",  yId)
  istat = nf_inq_varid(ncid, "LON",  lonId)
  istat = nf_inq_varid(ncid, "LAT",  latId)
  istat = nf_inq_varid(ncid, "CLD_FRACTION",  cldfracId)
  istat = nf_inq_varid(ncid, "CLD_ALB",  cldalbId)
  istat = nf_inq_varid(ncid, "CLD_TR",   cldtrId)
  istat = nf_inq_varid(ncid, "CLD_TOP_T", cldtoptId)
  istat = nf_inq_varid(ncid, "CLD_TOP_P", cldtoppId)
  istat = nf_inq_varid(ncid, "SFC_INSOLATION", sfcinsolId)
  istat = nf_inq_varid(ncid, "SKIN_T",  skintId)
  istat = nf_inq_varid(ncid, "SFC_ALB",  sfcalbId)
  
  call my_check_stat (istat, "nf_inq_varid in init_GOES_data_nc")
   
!---------------------------------------------------------------------------
!
! now read time-invariant variables
!
!---------------------------------------------------------------------------
  istat = nf_get_var_real(ncid, timeID, sat_time)
  call my_check_stat (istat, "nf_get_var, getting sat_time")

  istat = nf_get_var_text(ncid, timestrID, lcl_time_str)
  call my_check_stat (istat, "nf_get_var, getting lcl_time_str")

  istat = nf_get_var_real(ncid, xID, sat_x)
  call my_check_stat (istat, "nf_get_var, getting sat_x")

  istat = nf_get_var_real(ncid, yID, sat_y)
  call my_check_stat (istat, "nf_get_var, getting sat_y")

  istat = nf_get_var_real(ncid, lonID, sat_lon)
  call my_check_stat (istat, "nf_get_var, getting sat_lon")

  istat = nf_get_var_real(ncid, latID, sat_lat)
  call my_check_stat (istat, "nf_get_var, getting sat_lat")

!
! build the time character string (yyyy-mm-dd_hh:mm:ss.ssss)
!

  do i=1, sat_nt
    sat_time_str(i) = char(0)
    do j= 1, 16
     sat_time_str(i)(j:j) = lcl_time_str(i)(j:j)
    end do
    sat_time_str(i)(17:24) = ":00.0000"
!   write(*,*) "i, sat_time_str: ", i, sat_time_str(i)
  end do 
    
  deallocate( lcl_time_str )

  write(*,*) "init_sat: ncid, sat_ncid = ", ncid, sat_ncid

  end subroutine init_sat


  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  subroutine readsat ( sat_time_indx )
 
!-------------------------------------------------------------------------------
!
! Reads the satellite data for the time index sat_time_indx 
! corresponding to time mcip_now, and then copy the data
! into mcip_X grids
!
!------------------------------------------------------------------------------- 

  use parms3,   ONLY: badval3	! badval3 = -9.999E36
  use mcipparm, ONLY: nx => ncols_x, ny => nrows_x, &
                      x_offset => x0, y_offset => y0 
  use xvars,    ONLY: xlonc, xlatc

  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT (IN)  :: sat_time_indx
  
  integer               :: ncID		! netCDF file ID

  integer		:: istat=0
  
  integer		:: sx, ex, sy, ey
  integer		:: i, j, L

  real			:: diff
  real, PARAMETER	:: tol = 1.E-1

!----------------------------------------------------------------------------

! return if no sat. input is required.
! lsat is user input defined in mcipparm
!
  if (lsat /= 1) then
    write(*,*) "READSAT: lsat /= 1, I should not be here..."
    CALL graceful_stop ("READSAT")
  end if

! return if no sat. data for this time
!
  if (sat_time_indx == 0) then
    write(*,*) "READSAT: No sat. data for this time, returning ..."
    RETURN
  end if

  write(*,*) "in subroutine readsat ..."
  write(*,*) "sat. time index = ",sat_time_indx
  write(*,*) "ncID, sat_ncid, cldfracID = ", ncid, sat_ncid, cldfracID
  ncid = sat_ncid

  
!-------------------------------------------------------------------------------
! read in time-varying satellite data
!-------------------------------------------------------------------------------
!

  call allocate_sat_private ( sat_nx, sat_ny )
  call init_sat_private
  
  L = sat_time_indx		! time index for the current time
  write(*,*) "reading satellite data for time: ", sat_time_str(L)

  istat = nf_get_var_real(ncid, cldfracID, sat_cldfrac)
  call my_check_stat(istat,"nf_get_var_real, getting sat_cldfrac")

  istat = nf_get_var_real(ncid, cldalbID, sat_cldalb)
  call my_check_stat(istat,"nf_get_var_real, getting sat_cldalb")

  istat = nf_get_var_real(ncid, cldtrID, sat_cldtr)
  call my_check_stat(istat,"nf_get_var_real, getting sat_cldtr")

  istat = nf_get_var_real(ncid, cldtoptID, sat_cldtopt)
  call my_check_stat(istat,"nf_get_var_real, getting sat_cldtopt")

  istat = nf_get_var_real(ncid, cldtoppId, sat_cldtopp)
  call my_check_stat(istat,"nf_get_var_real, getting sat_cldtopp")

  istat = nf_get_var_real(ncid, sfcinsolID, sat_sfcinsol)
  call my_check_stat(istat,"nf_get_var_real, getting sat_sfcinsol")

  istat = nf_get_var_real(ncid, skintID, sat_skint)
  call my_check_stat(istat,"nf_get_var_real, getting sat_skint")

  istat = nf_get_var_real(ncid, sfcalbID, sat_sfcalb)
  call my_check_stat(istat,"nf_get_var_real, getting sat_sfcalb")

  !
  ! set cloud flag
  !
  do j=1, sat_ny
    do i=1, sat_nx
      sat_cldflg(i,j) = 0
!!!tlo      if (sat_sfcinsol(i,j) < 0.) sat_cldflg(i,j) = badval3
      if (sat_sfcinsol(i,j) < 0) sat_cldflg(i,j) = imiss3
      if (sat_cldfrac(i,j) > 0.)  sat_cldflg(i,j) = 1
    end do
  end do


!-------------------------------------------------------------------------------
!
! allocate memory for x-domain arrays
! ncols_x and nrows_x are cross pint dimensions for X and Y
! for MCIP X domain (ncols+2*nthik, nrows+2*nthik,
! where ncols and nrows are output dimensions) 
!
!-------------------------------------------------------------------------------

  call allocate_sat_public ( ncols_x, nrows_x )

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
  xsat_cldtopp(:,:) = sat_cldtopp(sx:ex,sy:ey)*100.	! convert mb to Pa

  xsat_lon    (:,:) = sat_lon    (sx:ex,sy:ey)
  xsat_lat    (:,:) = sat_lat    (sx:ex,sy:ey)

  i=ncols_x/2
  j=nrows_x/2
  diff = abs(xsat_lon(i,j)-xlonc(i,j))
  if (diff > tol) then
    write(*,*) " MCIP (i,j,lon): ", i, j, xlonc(i,j)
    write(*,*) " SAT  (i,j,lon): ", i, j, xsat_lon(i,j)
    call my_check_stat(1,"sat. lon does not match MM5 lon" )
  end if
  
  diff = abs(xsat_lat(i,j)-xlatc(i,j))
  if (diff > tol) then
    write(*,*) " MCIP (i,j,LAT): ", i, j, xlatc(i,j)
    write(*,*) " SAT  (i,j,LAT): ", i, j, xsat_lat(i,j)
    call my_check_stat(1,"sat. lat does not match MM5 lat")
  end if

!------------------------------------------------------------------------------ 
! Deallocate memory for the unused original arrays
!------------------------------------------------------------------------------
  
  call deallocate_sat_private
  
  end subroutine readsat
!  

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  subroutine satvars2ctm (mcip_now)

!-------------------------------------------------------------------------------
!
! This subroutine adds cloud transmissivity to MCIP when
! satellite data is available and corrects cloud top & bottom heights
! and cloud fraction based on satellite observation.
!
!-------------------------------------------------------------------------------

  use parms3,   ONLY: badval3	! badval3 = -9.999E36
  use mcipparm, ONLY: ncols_x, nrows_x, metlay
  use xvars,    ONLY: xdx3htf, xtempm,  xpresm,  xpresf, xwvapor, &
                      xcldtop, xcldbot, xcfract 


  IMPLICIT NONE

!
  CHARACTER*24,  INTENT(IN)    	:: mcip_now
  
  integer			:: sat_indx
  integer			:: c, r, k, l
  integer			:: k1000 = 0
  integer			:: ktop, kbase
  real				:: ctop, cbase
  real				:: grid_z(metlay)
  real                          :: tlcl	! temp. at lifting condensation level (K)
  real                          :: zlcl	! height of lifting condensation level (m)

! dalr: dry adiabatic lapse rate [ 9.7597226E-3 K/m ]
  real, parameter               :: dalr =  9.7597226E-3

  CHARACTER*16,  PARAMETER    	:: pname      = 'SATVARS2CTM'


!------------------------------------------------------------------------------
! return if no sat. input is required.
! lsat is user input defined in mcipparm
!------------------------------------------------------------------------------
  if (lsat /= 1) then
    write(*,*) "satvars2ctm: lsat /= 1, I should not be here..."
    RETURN
  end if

!
!-------------------------------------------------------------------------------
! allocate memory for variables if necessary
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED (xcldtr) ) then
    ALLOCATE (xcldtr(ncols_x, nrows_x))
  END IF

  call find_sat_indx (sat_time_str, mcip_now, sat_indx) 


  xcldtr = badval3
  if (sat_indx > 0) then	! replace xvars with satvars

    !-------------------------------------------------------------------------------
    ! Loop over grid points and replace cloud information.
    !-------------------------------------------------------------------------------

    write(*,*) " Replacing MCIP cloud info with satellite observation"
    write(*,*) " MCIP time: ", mcip_now
    write(*,*) " SAT  time: ", sat_time_str(sat_indx)
  
    L = sat_indx
    DO c = 1, ncols_x
      DO r = 1, nrows_x

      IF (xsat_cldtr(c,r)   >= 0 ) then     ! good data continue

      IF (xsat_cldfrac(c,r) > 0 .AND. &
          xsat_cldtopp(c,r) > 0 .AND. &
          xsat_cldtopp(c,r) < 1.E5 ) then   ! grid is cloudy
          
      ! find sat. clod top, and base
      ! Look for cloud top and base layer up and down from level of max RH.

        !
        ! Build array of layer heights
        !
        grid_z(1) = xdx3htf(c,r,1)
        DO k = 2, metlay
          grid_z(k) = grid_z(k-1) + xdx3htf(c,r,k)
          if (grid_z(k) < 1000.) k1000 = k
        END DO

        top: DO k = 2, metlay
          ktop = k
          if (grid_z(k) >= 10000.) EXIT top
!         IF ( xtempm (c,r,k) <= xsat_cldtopt(c,r) ) EXIT top
!         IF ( xpresm (c,r,k) <= xsat_cldtopp(c,r) ) EXIT top
          IF ( xpresf (c,r,k) <= xsat_cldtopp(c,r) ) EXIT top
        ENDDO top

!
!       estimate lifting condensation level (LCL) by estimating
!       the condensation temperature (tlcl) and calculating
!       the corresponding height based on dry adiabatic lapse rate (K/m)
!
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

        if (kbase < k1000) kbase = k1000
        if (kbase >= ktop) kbase = ktop-1

        if ( grid_Z(ktop) < 1000. ) then
          cbase = 0.
          ctop  = 0.
          xsat_cldfrac(c,r) = 0.
          xsat_cldtr(c,r)   = 1.0
        else
          cbase = grid_Z(kbase)
          ctop  = grid_Z(ktop)
        end if

        xcldtop(c,r) = ctop
        xcldbot(c,r) = cbase
        xcfract(c,r) = xsat_cldfrac(c,r)
        xcldtr (c,r) = xsat_cldtr(c,r)

!       write(*,'(A, 3(f6.0,1x),e11.4)') 'cbase, ctop, cldfrac, cldtr: ',cbase, ctop, &
!             xsat_cldfrac(c,r), xsat_cldtr(c,r)           

      ELSE IF (xsat_cldfrac(c,r) == 0) then	! clear sky values

        xcldtop(c,r) = 0.0
        xcldbot(c,r) = 0.0
        xcfract(c,r) = 0.0
        xcldtr (c,r) = 1.0

      END IF    ! end of cloud fraction check

      END IF    ! end of valid sat. data check

      ENDDO
    ENDDO

  end if

  end subroutine satvars2ctm


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine my_check_stat(istat,msg)
    IMPLICIT NONE
    integer, intent(in)	:: istat
    character (len=*)	:: msg

    if (istat /= 0) then
      write(*,*) " ***********"
      write(*,*) " error: ", trim(msg)
!     stop
      CALL graceful_stop ("sat2mcip")
    end if
!    if (istat /= nf90_noerr) call handle_err(istat)
  end subroutine my_check_stat
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!tlo  function t_condensation(wvapor,t0,p0)
  real function t_condensation(wvapor,t0,p0)

!   USE const, ONLY: mwwat, mwair, rgasuniv, rdgas, cpd	! contains const module, all constants needed

    IMPLICIT NONE
    real, intent(in)	:: wvapor	! water vapor mixing ratio
    real, intent(in)	:: t0		! surface layer temperature (K)
    real, intent(in)	:: p0		! surface pressure (pa)

!!!tlo    real, intent(out)	:: t_condensation	! condensation temp. (K)

    !
    !   declare the constants
    !
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

    
    REAL, PARAMETER :: A = 2.53E11	! empirical constant (pa)
    REAL, PARAMETER :: B = 5.42E03	! empirical constant (K)
    REAL, PARAMETER :: cpovr = 1./rovcp	! 1/k

    REAL, PARAMETER :: TOL = 1.E-02	! tolerance

    real :: delta, tnew, told
    real :: dum1, dum2
    integer :: counter 

!
! using the equation  (2.33) from page 21
! A Short Course in Cloud Physics by R.R. Rogers & M.K. Yau
! condensation temperature is calculated
!
! we use fixed point iteration for simplicity
!

    delta = 1.
    counter = 0
    told = t0
    tnew = 0.
    DO WHILE ( delta > tol .and. counter < 20 )
     counter = counter+1
     dum1 = (A*mvoma)/(wvapor*p0)
     dum2 = (t0/told)**cpovr
     tnew = B/log(dum1*dum2)   
     delta = abs(tnew-told)
     told = tnew
    END DO

    t_condensation = tnew

  end function t_condensation

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  function upcase ( string )
!
! This function changes string to uppercase
!
  IMPLICIT NONE
  
  ! declare calling parameters:
  CHARACTER (LEN=*), INTENT (IN)            :: string
!!!tlo  CHARACTER (LEN=LEN(string)), INTENT (OUT) :: UPCASE
  CHARACTER (LEN=LEN(string)) :: UPCASE

  ! declare local variables
  INTEGER :: i
  INTEGER :: length
  
  ! Get the length of the string
  length = LEN ( string )
  upcase = string
  
  ! now shift lowercase letters to uppercase.
  DO i = 1, length
    if ( LGE(upcase(i:i),'a') .AND. LLE(upcase(i:i),'z') ) then
      upcase(i:i) = ACHAR ( ICHAR ( upcase(i:i) ) - 32 )
    end if
  END DO

  end function upcase

  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  
  subroutine allocate_sat_private ( nx, ny )
  
  IMPLICIT NONE
  
  INTEGER, INTENT (IN) :: nx, ny

!
! allocate memory for arrays
!

  if (allocated(sat_cldflg))      deallocate(sat_cldflg)
  allocate( sat_cldflg(nx, ny) )

  if (allocated(sat_cldfrac))     deallocate(sat_cldfrac)
  allocate( sat_cldfrac(nx, ny) )

  if (allocated(sat_cldalb))      deallocate(sat_cldalb)
  allocate( sat_cldalb(nx, ny) )

  if (allocated(sat_cldtr))       deallocate(sat_cldtr)
  allocate( sat_cldtr(nx, ny) )

  if (allocated(sat_cldtopt))     deallocate(sat_cldtopt)
  allocate( sat_cldtopt(nx, ny) )

  if (allocated(sat_cldtopp))     deallocate(sat_cldtopp)
  allocate( sat_cldtopp(nx, ny) )

  if (allocated(sat_sfcinsol))    deallocate(sat_sfcinsol)
  allocate( sat_sfcinsol(nx, ny) )

  if (allocated(sat_skint))       deallocate(sat_skint)
  allocate( sat_skint(nx, ny) )

  if (allocated(sat_sfcalb))      deallocate(sat_sfcalb)
  allocate( sat_sfcalb(nx, ny) )

  end subroutine allocate_sat_private

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  
  subroutine init_sat_private
  
!!!tlo  use parms3,   ONLY: badval3	! badval3 = -9.999E36
  use parms3,   ONLY: badval3, imiss3	! badval3 = -9.999E36, imiss3 = -9999

  IMPLICIT NONE
  
!
! initialize private arrays
!

!!!tlo  sat_cldflg   = badval3
  sat_cldflg   = imiss3
  sat_cldfrac  = badval3
  sat_cldalb   = badval3
  sat_cldtr    = badval3
  sat_cldtopt  = badval3
  sat_cldtopp  = badval3
  sat_sfcinsol = badval3
  sat_skint    = badval3
  sat_sfcalb   = badval3

  end subroutine init_sat_private

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  
  subroutine deallocate_sat_private
  
  IMPLICIT NONE

!
! allocate memory for arrays
!
  if (ALLOCATED(sat_cldflg))   deallocate( sat_cldflg )
  if (ALLOCATED(sat_cldfrac))  deallocate( sat_cldfrac )
  if (ALLOCATED(sat_cldalb))   deallocate( sat_cldalb )
  if (ALLOCATED(sat_cldtr))    deallocate( sat_cldtr )
  if (ALLOCATED(sat_cldtopt))  deallocate( sat_cldtopt )
  if (ALLOCATED(sat_cldtopp))  deallocate( sat_cldtopp )
  if (ALLOCATED(sat_sfcinsol)) deallocate( sat_sfcinsol )
  if (ALLOCATED(sat_skint))    deallocate( sat_skint )
  if (ALLOCATED(sat_sfcalb))   deallocate( sat_sfcalb )

  end subroutine deallocate_sat_private

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  
  subroutine allocate_sat_public ( nx, ny )
  
  IMPLICIT NONE
  
  INTEGER, INTENT (IN) :: nx, ny

  !
  ! allocate memory for arrays
  !

  if (allocated(xsat_lon))                 deallocate(xsat_lon)
  allocate( xsat_lon(nx, ny) )
  if (allocated(xsat_lat))                 deallocate(xsat_lat)
  allocate( xsat_lat(nx, ny) )
  if (allocated(xsat_cldflg))              deallocate(xsat_cldflg)
  allocate( xsat_cldflg(nx, ny) )
  if (allocated(xsat_cldfrac))             deallocate(xsat_cldfrac)
  allocate( xsat_cldfrac(nx, ny) )
  if (allocated(xsat_cldtr))               deallocate(xsat_cldtr)
  allocate( xsat_cldtr(nx, ny) )
  if (allocated(xsat_cldtopt))             deallocate(xsat_cldtopt)
  allocate( xsat_cldtopt(nx, ny) )
  if (allocated(xsat_cldtopp))             deallocate(xsat_cldtopp)
  allocate( xsat_cldtopp(nx, ny) )

  end subroutine allocate_sat_public

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  
  subroutine deallocate_sat_public
  
  IMPLICIT NONE
  
!
! deallocate memory
!

  if (ALLOCATED(xsat_lon))      deallocate( xsat_lon )
  if (ALLOCATED(sat_time))      deallocate( xsat_lat )
  if (ALLOCATED(xsat_cldflg))   deallocate( xsat_cldflg )
  if (ALLOCATED(xsat_cldfrac))  deallocate( xsat_cldfrac )
  if (ALLOCATED(xsat_cldtr))    deallocate( xsat_cldtr )
  if (ALLOCATED(xsat_cldtopt))  deallocate( xsat_cldtopt )
  if (ALLOCATED(xsat_cldtopp))  deallocate( xsat_cldtopp )

  end subroutine deallocate_sat_public


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  
  subroutine allocate_sat_time ( nt )
  
  IMPLICIT NONE
  
  INTEGER, INTENT (IN) :: nt

  !
  ! allocate memory for arrays
  !

  if (allocated(sat_time))                 deallocate( sat_time )
                                           allocate  ( sat_time(sat_nt) )
  if (allocated(sat_time_str))             deallocate( sat_time_str )
                                           allocate  ( sat_time_str(sat_nt) )

  end subroutine allocate_sat_time


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  
  subroutine deallocate_sat_time
  
  IMPLICIT NONE
  
  !
  ! deallocate memory
  !

  deallocate(sat_time)
  deallocate(sat_time_str)

  end subroutine deallocate_sat_time


!-------------------------------------------------------------------------
!
!-------------------------------------------------------------------------
!
END MODULE sat2mcip
