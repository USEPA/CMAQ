
!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

MODULE wrf_netcdf

!-------------------------------------------------------------------------------
! Name:     WRF NetCDF
! Purpose:  Contains routines to read NetCDF output in the Weather Research and
!           Forecasting (WRF) Model I/O API format.
! Revised:  31 Aug 2004  Original version provided by U. Houston.  (S.-B. Kim)
!           21 Jul 2005  Updated to include error-handling on NetCDF functions
!                        and to conform to NetCDF standard routines for
!                        retrieving data (to eliminate type mismatches).
!                        Updated formatting.  (T. Otte)
!           02 Aug 2005  Changed order of variable declarations in some
!                        subroutines to avoid compile failure on some
!                        machines.  (T. Otte)
!           20 Jun 2006  Removed unused variables.  Changed local variables
!                        FILE to FILENAME and DATA to DATAOUT to avoid
!                        conflicts with F90 keywords.  (T. Otte)
!           19 Apr 2007  Added new routine GET_VAR_REAL2_CDF to read "real"
!                        scalars, as needed for WRFv2.2.  Added new routine
!                        GET_DIM_INT_CDF to retrieve netCDF dimensions.
!                        Changed internal error handling so that errors are
!                        passed back using a non-zero RCODE for dimension
!                        mismatches in addition to netCDF errors.  (T. Otte)
!           12 Feb 2010  Removed unused variable ID_TIME from subroutine
!                        GET_GL_ATT_INT_CDF.  (T. Otte)
!           19 Mar 2010  Removed routines GET_DIMS_CDF, GET_DIM_INT_CDF,
!                        GET_GL_ATT_INT_CDF, GET_GL_ATT_REAL_CDF,
!                        GET_GL_ATT_TEXT_CDF, and GET_DIM_ATT_INT_CDF.
!                        Removed file open and close functions from all
!                        remaining routines, and changed input argument
!                        from FILENAME to CDFID.  (T. Otte)
!-------------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_times_cdf (cdfid, times, n_times, max_times, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: cdfid
  INTEGER                      :: dimids     ( 10 )
  INTEGER                      :: i
  INTEGER                      :: id_time
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER,       INTENT(IN)    :: max_times  
  INTEGER,       INTENT(OUT)   :: n_times
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  CHARACTER*80                 :: time1
  CHARACTER*80,  INTENT(OUT)   :: times      ( max_times )
  CHARACTER*80                 :: varnam

  rcode = nf_inq_varid (cdfid, 'Times', id_time)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_inq_var (cdfid, id_time, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= nf_noerr ) RETURN

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= nf_noerr ) RETURN
  ENDDO

  ! Get the times.

  n_times = idims(2)
  DO i = 1, idims(2)
    istart(1) = 1  ;  iend(1) = idims(1)
    istart(2) = i  ;  iend(2) = 1
    rcode = nf_get_vara_text (cdfid, id_time, istart, iend, times(i))
    IF ( rcode /= nf_noerr ) RETURN
    time1 = times(i)
  ENDDO

END SUBROUTINE get_times_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_3d_real_cdf (cdfid, var, dataout, i1, i2, i3, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1
  INTEGER,       INTENT(IN)    :: i2
  INTEGER,       INTENT(IN)    :: i3

  INTEGER,       INTENT(IN)    :: cdfid
  REAL,          INTENT(OUT)   :: dataout    ( i1, i2, i3 )
  INTEGER                      :: dimids     ( 10 )
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  rcode = nf_inq_varid (cdfid, var, id_data)
  IF ( rcode /= nf_noerr ) RETURN
  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= nf_noerr ) RETURN

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= nf_noerr ) RETURN
  ENDDO

  ! Check the dimensions.

  IF ( ( i1   /= idims(1) ) .OR.  &
       ( i2   /= idims(2) ) .OR.  &
       ( i3   /= idims(3) ) .OR.  &
       ( time >  idims(4) ) )  THEN

    WRITE (6,*) 'error in 3d_var_real read, dimension problem'
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) i2, idims(2)
    WRITE (6,*) i3, idims(3)
    WRITE (6,*) time, idims(4)
    WRITE (6,*) ' error stop '
    rcode = -9999
    RETURN

  ENDIF

  ! Get the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = 1     ;  iend(2) = i2
  istart(3) = 1     ;  iend(3) = i3
  istart(4) = time  ;  iend(4) = 1

  rcode = nf_get_vara_real (cdfid, id_data, istart, iend, dataout)
  IF ( rcode /= nf_noerr ) RETURN

END SUBROUTINE get_var_3d_real_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_2d_real_cdf (cdfid, var, dataout, i1, i2, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1
  INTEGER,       INTENT(IN)    :: i2

  INTEGER,       INTENT(IN)    :: cdfid
  REAL,          INTENT(OUT)   :: dataout    ( i1, i2 )
  INTEGER                      :: dimids     ( 10 )
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  rcode = nf_inq_varid (cdfid, var, id_data)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= nf_noerr ) RETURN

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= nf_noerr ) RETURN
  ENDDO

  ! Check the dimensions.

  IF ( ( i1  /= idims(1) ) .OR.  &
       ( i2  /= idims(2) ) .OR.  &
       ( time > idims(3) ) ) THEN

    WRITE (6,*) 'error in 2d_var_real read, dimension problem'
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) i2, idims(2)
    WRITE (6,*) time, idims(4)
    WRITE (6,*) 'error stop'
    rcode = -9999
    RETURN

  ENDIF

  ! Get the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = 1     ;  iend(2) = i2
  istart(3) = time  ;  iend(3) = 1

  rcode = nf_get_vara_real (cdfid, id_data, istart, iend, dataout)
  IF ( rcode /= nf_noerr ) RETURN

END SUBROUTINE get_var_2d_real_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_2d_int_cdf (cdfid, var, dataout, i1, i2, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1
  INTEGER,       INTENT(IN)    :: i2

  INTEGER,       INTENT(IN)    :: cdfid
  INTEGER,       INTENT(OUT)   :: dataout    ( i1, i2 )
  INTEGER                      :: dimids     ( 10 )
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  rcode = nf_inq_varid (cdfid, var, id_data)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= nf_noerr ) RETURN

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= nf_noerr ) RETURN
  ENDDO

  ! Check the dimensions.

  IF ( ( i1   /= idims(1) ) .OR.  &
       ( i2   /= idims(2) ) .OR.  &
       ( time >  idims(3) ) )  THEN

    WRITE (6,*) 'error in 2d_var_real read, dimension problem'
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) i2, idims(2)
    WRITE (6,*) time, idims(4)
    WRITE (6,*) 'error stop'
    rcode = -9999
    RETURN

  ENDIF

  ! Get the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = 1     ;  iend(2) = i2
  istart(3) = time  ;  iend(3) = 1

  rcode = nf_get_vara_int (cdfid, id_data, istart, iend, dataout)
  IF ( rcode /= nf_noerr ) RETURN

END SUBROUTINE get_var_2d_int_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_1d_real_cdf (cdfid, var, dataout, i1, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1

  INTEGER,       INTENT(IN)    :: cdfid
  REAL,          INTENT(OUT)   :: dataout    ( i1 )
  INTEGER                      :: dimids     ( 10 )
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  rcode = nf_inq_varid (cdfid, var, id_data)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= nf_noerr ) RETURN

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= nf_noerr ) RETURN
  ENDDO

  ! Check the dimensions.

  IF ( ( i1  /= idims(1) ) .OR.  &
       ( time > idims(2) ) )  THEN

    WRITE (6,*) 'error in 1d_var_real read, dimension problem'
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) time, idims(2)
    WRITE (6,*) 'error stop'
    rcode = -9999
    RETURN

  ENDIF

  ! Get the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = time  ;  iend(2) = 1

  rcode = nf_get_vara_real (cdfid, id_data, istart, iend, dataout)
  IF ( rcode /= nf_noerr ) RETURN

END SUBROUTINE get_var_1d_real_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_real_cdf (cdfid, var, dataout, i1, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1

  INTEGER,       INTENT(IN)    :: cdfid
  REAL,          INTENT(OUT)   :: dataout
  REAL                         :: data2      ( i1 )
  INTEGER                      :: dimids     ( 10 )
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  rcode = nf_inq_varid (cdfid, var, id_data)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= nf_noerr ) RETURN

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= nf_noerr ) RETURN
  ENDDO

  ! Check the dimensions.

  IF ( ( i1   /= idims(1) ) .OR.  & 
       ( time >  idims(2) ) )  THEN

    WRITE (6,*) 'error in 1d_var_real read, dimension problem '
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) time, idims(2)
    WRITE (6,*) 'error stop'
    rcode = -9999
    RETURN

  ENDIF

  ! Get the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = time  ;  iend(2) = 1

  rcode = nf_get_vara_real (cdfid, id_data, istart, iend, data2)
  IF ( rcode /= nf_noerr ) RETURN

  dataout = data2(i1)

END SUBROUTINE get_var_real_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_real2_cdf (cdfid, var, dataout, i1, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1

  INTEGER,       INTENT(IN)    :: cdfid
  REAL,          INTENT(OUT)   :: dataout
  REAL                         :: data2      ( i1 )
  INTEGER                      :: dimids     ( 10 )
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  rcode = nf_inq_varid (cdfid, var, id_data)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= nf_noerr ) RETURN

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= nf_noerr ) RETURN
  ENDDO

  ! Check the dimensions.

  IF ( i1   /= idims(1) ) THEN

    WRITE (6,*) 'error in 1d_var_real read, dimension problem '
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) 'error stop'
    rcode = -9999
    RETURN

  ENDIF

  ! Get the data.

  istart(1) = 1
  iend(1)   = i1

  rcode = nf_get_vara_real (cdfid, id_data, istart, iend, data2)
  IF ( rcode /= nf_noerr ) RETURN

  dataout = data2(1)

END SUBROUTINE get_var_real2_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

END MODULE wrf_netcdf
