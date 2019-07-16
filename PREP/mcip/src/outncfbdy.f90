SUBROUTINE outncfbdy (mcip_now, sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Output netCDF File on Lateral Boundaries
! Purpose:  Create a netCDF file of MCIP output on lateral boundaries.
! Revised:  19 Dec 2018  Original version.  (T. Spero)
!-------------------------------------------------------------------------------

  USE files
  USE ctmvars
  USE mcipparm
  USE netcdf

  IMPLICIT NONE

  INTEGER                         :: dim_nperim
  INTEGER                         :: dim_nz
  INTEGER                         :: dim_time
  INTEGER                         :: dim_timestr
  LOGICAL,            SAVE        :: first      = .TRUE.
  CHARACTER(LEN=256)              :: fl
  INTEGER,  SAVE,     ALLOCATABLE :: id_fld     ( : )
  INTEGER,  SAVE                  :: id_time
  INTEGER,  SAVE                  :: it         = 0
  INTEGER,            PARAMETER   :: len_time   = 19
  CHARACTER(LEN=24),  INTENT(IN)  :: mcip_now
  INTEGER                         :: n
  INTEGER                         :: nn
  INTEGER                         :: ntot
  INTEGER                         :: nvars
  CHARACTER(LEN=16),  PARAMETER   :: pname      = 'OUTNCF'
  INTEGER                         :: rcode
  INTEGER,            INTENT(IN)  :: sdate
  INTEGER,            INTENT(IN)  :: stime
  CHARACTER(LEN=32)               :: var

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CREATING DIMENSION FOR ', a, &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR DEFINING VARIABLE ', a, &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CREATING ATTRIBUTE FOR', a, &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9350 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR ENDING DEFINITIONS ', &
    & /, 1x, '***   IN FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9400 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR WRITING VARIABLE ', a, &
    & /, 1x, '***   TO FILE ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9500 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CREATING NETCDF FILE', &
    & /, 1x, '***   FILE = ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9700 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CLOSING NETCDF FILE', &
    & /, 1x, '***   FILE = ', a, &
    & /, 1x, '***   ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  it = it + 1

  nvars = nfld2dxy + nfld3dxyzt + nfld3dxyzt_q

  IF ( .NOT. ALLOCATED ( id_fld ) ) ALLOCATE ( id_fld ( nvars ) )

!-------------------------------------------------------------------------------
! If first time calling this routine, set up the netCDF output file.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

  !-----------------------------------------------------------------------------
  ! Create netCDF file.
  !-----------------------------------------------------------------------------

    fl = TRIM(mcipbdyncf)

    rcode = nf90_create (fl, nf90_noclobber, cdfid_b)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9500) TRIM(pname), TRIM(fl), TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  !-----------------------------------------------------------------------------
  ! Set up dimensions.
  !-----------------------------------------------------------------------------

    var = "time"
    rcode = nf90_def_dim (cdfid_b, TRIM(var), nf90_unlimited, dim_time)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "timestr"
    rcode = nf90_def_dim (cdfid_b, TRIM(var), len_time, dim_timestr)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "nperim"
    rcode = nf90_def_dim (cdfid_b, TRIM(var), nperim, dim_nperim)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "nz"
    rcode = nf90_def_dim (cdfid_b, TRIM(var), nz, dim_nz)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  !-----------------------------------------------------------------------------
  ! Define variables that will populate the file.
  !-----------------------------------------------------------------------------

    var = "mtime"
    rcode = nf90_def_var (cdfid_b, TRIM(var), nf90_char,  &
                          (/ dim_timestr, dim_time /), id_time)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    DO n = 1, nfld2dxy
      var = TRIM(fld2dxy(n)%fldname)
      rcode = nf90_def_var (cdfid_b, TRIM(var), nf90_float,  &
                            (/ dim_nperim /), id_fld(n))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = nfld2dxy

    DO n = 1, nfld3dxyzt
      nn = ntot + n
      var = TRIM(fld3dxyzt(n)%fldname)
      rcode = nf90_def_var (cdfid_b, TRIM(var), nf90_float,  &
                            (/ dim_nperim, dim_nz, dim_time /), id_fld(nn))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld3dxyzt

    DO n = 1, nfld3dxyzt_q
      nn = ntot + n
      var = TRIM(fld3dxyzt_q(n)%fldname)
      rcode = nf90_def_var (cdfid_b, TRIM(var), nf90_float,  &
                            (/ dim_nperim, dim_nz, dim_time /), id_fld(nn))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld3dxyzt_q

  !-----------------------------------------------------------------------------
  ! Define global attributes.
  !-----------------------------------------------------------------------------

    CALL outncfglobal (cdfid_b, fl)

  !-----------------------------------------------------------------------------
  ! Define attributes for the variables.
  !-----------------------------------------------------------------------------

    DO n = 1, nfld2dxy
      var = TRIM(fld2dxy(n)%fldname)
      rcode = nf90_put_att (cdfid_b, id_fld(n), 'long_name',  &
                            TRIM(fld2dxy(n)%long_name))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_b, id_fld(n), 'units', TRIM(fld2dxy(n)%units))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = nfld2dxy

    DO n = 1, nfld3dxyzt
      nn = ntot + n
      var = TRIM(fld3dxyzt(n)%fldname)
      rcode = nf90_put_att (cdfid_b, id_fld(nn), 'long_name',  &
                            TRIM(fld3dxyzt(n)%long_name))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_b, id_fld(nn), 'units',  &
                            TRIM(fld3dxyzt(n)%units))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld3dxyzt

    DO n = 1, nfld3dxyzt_q
      nn = ntot + n
      var = TRIM(fld3dxyzt_q(n)%fldname)
      rcode = nf90_put_att (cdfid_b, id_fld(nn), 'long_name',  &
                            TRIM(fld3dxyzt_q(n)%long_name))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_b, id_fld(nn), 'units',  &
                            TRIM(fld3dxyzt_q(n)%units))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld3dxyzt_q

    rcode = nf90_enddef (cdfid_b)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9350) TRIM(pname), TRIM(fl), TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF  ! first = .TRUE.

!-------------------------------------------------------------------------------
! Write variables.
!-------------------------------------------------------------------------------

  var = "mtime"
  rcode = nf90_put_var (cdfid_b, id_time, mcip_now(1:len_time),  &
                        start = (/ 1, it /), count = (/ len_time, 1 /) )
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl), TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( first ) THEN  ! write time-independent fields

    DO n = 1, nfld2dxy
      var = TRIM(fld2dxy(n)%fldname)
      rcode = nf90_put_var (cdfid_b, id_fld(n), fld2dxy(n)%bdy)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO

  ENDIF  ! first

  ntot = nfld2dxy

  DO n = 1, nfld3dxyzt
    nn = ntot + n
    var = TRIM(fld3dxyzt(n)%fldname)
    rcode = nf90_put_var (cdfid_b, id_fld(nn), fld3dxyzt(n)%bdy,  &
                          start = (/ 1, 1, it /),  &
                          count = (/ fld3dxyzt(n)%iend_b(1), &
                                     fld3dxyzt(n)%iend_b(2), 1 /) )
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDDO
  ntot = ntot + nfld3dxyzt

  DO n = 1, nfld3dxyzt_q
    nn = ntot + n
    var = TRIM(fld3dxyzt_q(n)%fldname)
    rcode = nf90_put_var (cdfid_b, id_fld(nn), fld3dxyzt_q(n)%bdy,  &
                          start = (/ 1, 1, it /),  &
                          count = (/ fld3dxyzt_q(n)%iend_b(1), &
                                     fld3dxyzt_q(n)%iend_b(2), 1 /) )
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDDO
  ntot = ntot + nfld3dxyzt_q

  first = .FALSE.

END SUBROUTINE outncfbdy
