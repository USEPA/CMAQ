SUBROUTINE outncf (mcip_now, sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Output netCDF File
! Purpose:  Create a netCDF file of MCIP output.
! Revised:  19 Dec 2018  Original version.  (T. Spero)
!           18 Jun 2019  Added soil depths to output.  Created logic to support
!                        true Arakawa-C sizing of output arrays rather than
!                        false dot points for staggered fields.  (T. Spero)
!-------------------------------------------------------------------------------

  USE files
  USE ctmvars
  USE mcipparm
  USE metinfo, ONLY: met_hybrid
  USE metvars, ONLY: sigmah, sigmaf, c1f, c1h, c2f, c2h
  USE netcdf
  USE xvars,   ONLY: xzsoil

  IMPLICIT NONE

  INTEGER                         :: dim_nlucat
  INTEGER                         :: dim_nmos
  INTEGER                         :: dim_nsoillay
  INTEGER                         :: dim_nx
  INTEGER                         :: dim_nxp1
  INTEGER                         :: dim_ny
  INTEGER                         :: dim_nyp1
  INTEGER                         :: dim_nz
  INTEGER                         :: dim_nzp1
  INTEGER                         :: dim_time
  INTEGER                         :: dim_timestr
  LOGICAL,            SAVE        :: first      = .TRUE.
  CHARACTER(LEN=256)              :: fl
  INTEGER,  SAVE,     ALLOCATABLE :: id_fld     ( : )
  INTEGER,  SAVE                  :: id_time
  INTEGER,  SAVE                  :: it         = 0
  INTEGER,            PARAMETER   :: len_time   = 19
  CHARACTER(LEN=24),  INTENT(IN)  :: mcip_now
  INTEGER                         :: mydimx
  INTEGER                         :: mydimy
  INTEGER                         :: myendx
  INTEGER                         :: myendy
  INTEGER                         :: myendz
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

  nvars = nfld2dxy   + nfld2dxy_d + nfld3dxyl +   &
          nfld2dxyt  + nfld3dxyzt + nfld3dxyzt_q + nfld3dxyzt_d +  &
          nfld3dxyst + nfld3dxymt + 2  ! the extra 2 are full and half layers

  IF ( met_hybrid >= 0 ) THEN
    nvars = nvars + 4
  ENDIF

  IF ( ifsoil ) THEN
    nvars = nvars + 1
  ENDIF

  IF ( .NOT. ALLOCATED ( id_fld ) ) ALLOCATE ( id_fld ( nvars ) )

!-------------------------------------------------------------------------------
! If first time calling this routine, set up the netCDF output file.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

  !-----------------------------------------------------------------------------
  ! Create netCDF file.
  !-----------------------------------------------------------------------------

    fl = TRIM(mcipncf)

    rcode = nf90_create (fl, nf90_noclobber, cdfid_m)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9500) TRIM(pname), TRIM(fl), TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  !-----------------------------------------------------------------------------
  ! Set up dimensions.
  !-----------------------------------------------------------------------------

    var = "time"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nf90_unlimited, dim_time)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "timestr"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), len_time, dim_timestr)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "nx"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nx, dim_nx)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "ny"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), ny, dim_ny)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "nz"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nz, dim_nz)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "nxp1"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nxp1, dim_nxp1)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "nyp1"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nyp1, dim_nyp1)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    var = "nzp1"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nzp1, dim_nzp1)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    IF ( ifsoil ) THEN
      var = "nsoillay"
      rcode = nf90_def_dim (cdfid_m, TRIM(var), nsoi, dim_nsoillay)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDIF

    var = "nlucat"
    rcode = nf90_def_dim (cdfid_m, TRIM(var), nlucat, dim_nlucat)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    IF ( ifmosaic ) THEN
      var = "nmos"
      rcode = nf90_def_dim (cdfid_m, TRIM(var), nmos, dim_nmos)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9100) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDIF

  !-----------------------------------------------------------------------------
  ! Define variables that will populate the file.
  !-----------------------------------------------------------------------------

    var = "mtime"
    rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_char,  &
                          (/ dim_timestr, dim_time /), id_time)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    DO n = 1, nfld2dxy
      var = TRIM(fld2dxy(n)%fldname)
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ dim_nx, dim_ny /), id_fld(n))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = nfld2dxy

    DO n = 1, nfld2dxy_d  ! Some Arakawa-B (dots), some Arakawa-C (faces)
      nn = ntot + n
      var = TRIM(fld2dxy_d(n)%fldname)
      IF ( fld2dxy_d(n)%iend(1) == nx ) THEN
        mydimx = dim_nx
      ELSE IF ( fld2dxy_d(n)%iend(1) == nxp1 ) THEN
        mydimx = dim_nxp1
      ELSE
        mydimx = 0
      ENDIF
      IF ( fld2dxy_d(n)%iend(2) == ny ) THEN
        mydimy = dim_ny
      ELSE IF ( fld2dxy_d(n)%iend(2) == nyp1 ) THEN
        mydimy = dim_nyp1
      ELSE
        mydimy = 0
      ENDIF
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ mydimx, mydimy /), id_fld(nn))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld2dxy_d

    IF ( iflufrc ) THEN

      DO n = 1, nfld3dxyl
        nn = ntot + n
        var = TRIM(fld3dxyl(n)%fldname)
        rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                              (/ dim_nx, dim_ny, dim_nlucat /), id_fld(nn))
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                          TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
      ENDDO
      ntot = ntot + nfld3dxyl

    ENDIF

    DO n = 1, nfld2dxyt
      nn = ntot + n
      var = TRIM(fld2dxyt(n)%fldname)
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ dim_nx, dim_ny, dim_time /), id_fld(nn))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld2dxyt

    DO n = 1, nfld3dxyzt
      nn = ntot + n
      var = TRIM(fld3dxyzt(n)%fldname)
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ dim_nx, dim_ny, dim_nz, dim_time /), id_fld(nn))
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
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ dim_nx, dim_ny, dim_nz, dim_time /), id_fld(nn))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld3dxyzt_q

    DO n = 1, nfld3dxyzt_d  ! Some Arakawa-B (dots), some Arakawa-C (faces)
      nn = ntot + n
      var = TRIM(fld3dxyzt_d(n)%fldname)
      IF ( fld3dxyzt_d(n)%iend(1) == nx ) THEN
        mydimx = dim_nx
      ELSE IF ( fld3dxyzt_d(n)%iend(1) == nxp1 ) THEN
        mydimx = dim_nxp1
      ELSE
        mydimx = 0
      ENDIF
      IF ( fld3dxyzt_d(n)%iend(2) == ny ) THEN
        mydimy = dim_ny
      ELSE IF ( fld3dxyzt_d(n)%iend(2) == nyp1 ) THEN
        mydimy = dim_nyp1
      ELSE
        mydimy = 0
      ENDIF
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ mydimx, mydimy, dim_nz, dim_time /),  &
                            id_fld(nn))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld3dxyzt_d

    IF ( ifsoil ) THEN
      DO n = 1, nfld3dxyst
        nn = ntot + n
        var = TRIM(fld3dxyst(n)%fldname)
        rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                              (/ dim_nx, dim_ny, dim_nsoillay, dim_time /),  &
                              id_fld(nn))
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                          TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
      ENDDO
      ntot = ntot + nfld3dxyst
    ENDIF

    IF ( ifmosaic ) THEN
      DO n = 1, nfld3dxymt
        nn = ntot + n
        var = TRIM(fld3dxymt(n)%fldname)
        rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                              (/ dim_nx, dim_ny, dim_nmos, dim_time /),  &
                              id_fld(nn))
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                          TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
      ENDDO
      ntot = ntot + nfld3dxymt
    ENDIF

    nn = ntot + 1
    var = "ZNU"
    rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                          (/ dim_nz /), id_fld(nn))
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    nn = ntot + 2
    var = "ZNW"
    rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                          (/ dim_nzp1 /), id_fld(nn))
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
    ntot = ntot + 2

    IF ( met_hybrid >= 0 ) THEN

      nn = ntot + 1
      var = "C1H"
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ dim_nz /), id_fld(nn))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

      nn = ntot + 2
      var = "C2H"
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ dim_nz /), id_fld(nn))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

      nn = ntot + 3
      var = "C1F"
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ dim_nzp1 /), id_fld(nn))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

      nn = ntot + 4
      var = "C2F"
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ dim_nzp1 /), id_fld(nn))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

      ntot = ntot + 4

    ENDIF  ! met_hybrid >= 0

    IF ( ifsoil ) THEN
      nn = ntot + 1
      var = "ZSOIL"
      rcode = nf90_def_var (cdfid_m, TRIM(var), nf90_float,  &
                            (/ dim_nsoillay /), id_fld(nn))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9200) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      ntot = ntot + 1
    ENDIF  ! ifsoil = .TRUE.

  !-----------------------------------------------------------------------------
  ! Define global attributes.
  !-----------------------------------------------------------------------------

    CALL outncfglobal (cdfid_m, fl)

  !-----------------------------------------------------------------------------
  ! Define attributes for the variables.
  !-----------------------------------------------------------------------------

    DO n = 1, nfld2dxy
      var = TRIM(fld2dxy(n)%fldname)
      rcode = nf90_put_att (cdfid_m, id_fld(n), 'long_name',  &
                            TRIM(fld2dxy(n)%long_name))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_m, id_fld(n), 'units', TRIM(fld2dxy(n)%units))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = nfld2dxy

    DO n = 1, nfld2dxy_d
      nn = ntot + n
      var = TRIM(fld2dxy_d(n)%fldname)
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                            TRIM(fld2dxy_d(n)%long_name))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                            TRIM(fld2dxy_d(n)%units))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld2dxy_d

    IF ( iflufrc ) THEN
      DO n = 1, nfld3dxyl
        nn = ntot + n
        var = TRIM(fld3dxyl(n)%fldname)
        rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                              TRIM(fld3dxyl(n)%long_name))
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                          TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
        rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                              TRIM(fld3dxyl(n)%units))
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                          TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
      ENDDO
      ntot = ntot + nfld3dxyl
    ENDIF

    DO n = 1, nfld2dxyt
      nn = ntot + n
      var = TRIM(fld2dxyt(n)%fldname)
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                            TRIM(fld2dxyt(n)%long_name))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                            TRIM(fld2dxyt(n)%units))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld2dxyt

    DO n = 1, nfld3dxyzt
      nn = ntot + n
      var = TRIM(fld3dxyzt(n)%fldname)
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                            TRIM(fld3dxyzt(n)%long_name))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
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
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                            TRIM(fld3dxyzt_q(n)%long_name))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                            TRIM(fld3dxyzt_q(n)%units))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld3dxyzt_q

    DO n = 1, nfld3dxyzt_d
      nn = ntot + n
      var = TRIM(fld3dxyzt_d(n)%fldname)
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                            TRIM(fld3dxyzt_d(n)%long_name))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                            TRIM(fld3dxyzt_d(n)%units))
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld3dxyzt_d

    IF ( ifsoil ) THEN
      DO n = 1, nfld3dxyst
        nn = ntot + n
        var = TRIM(fld3dxyst(n)%fldname)
        rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                              TRIM(fld3dxyst(n)%long_name))
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                          TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
        rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                              TRIM(fld3dxyst(n)%units))
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                          TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
      ENDDO
      ntot = ntot + nfld3dxyst
    ENDIF

    IF ( ifmosaic ) THEN
      DO n = 1, nfld3dxymt
        nn = ntot + n
        var = TRIM(fld3dxymt(n)%fldname)
        rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                              TRIM(fld3dxymt(n)%long_name))
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                          TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
        rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                              TRIM(fld3dxymt(n)%units))
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                          TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
      ENDDO
      ntot = ntot + nfld3dxymt
    ENDIF

    nn = ntot + 1
    var = "ZNU"
    rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                          "eta values on half (mass) levels")
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
    rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                          "1")
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    nn = ntot + 2
    var = "ZNW"
    rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                          "eta values on full (w) levels")
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
    rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                          "1")
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    ntot = ntot + 2

    IF ( met_hybrid >= 0 ) THEN

      nn = ntot + 1
      var = "C1H"
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                            "half levels, c1h = d bf / d eta, using znw")
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                            "1")
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

      nn = ntot + 2
      var = "C2H"
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                            "half levels, c2h = (1-c1h)*(p0-pt)")
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                            "1")
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

      nn = ntot + 3
      var = "C1F"
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                            "full levels, c1f = d bf / d eta, using znu")
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                            "1")
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

      nn = ntot + 4
      var = "C2F"
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                            "full levels, c2f = (1-c1f)*(p0-pt)")
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                            "1")
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

      ntot = ntot + 4

    ENDIF  ! met_hybrid >= 0

    IF ( ifsoil ) THEN
      nn = ntot + 1
      var = "ZSOIL"
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'long_name',  &
                            "depth of bottom of soil layer")
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_put_att (cdfid_m, id_fld(nn), 'units',  &
                            "m")
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9300) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
      ntot = ntot + 1
    ENDIF  ! ifsoil = .TRUE.

    rcode = nf90_enddef (cdfid_m)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9350) TRIM(pname), TRIM(fl), TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF  ! first = .TRUE.

!-------------------------------------------------------------------------------
! Write variables.
!-------------------------------------------------------------------------------

  var = "mtime"
  rcode = nf90_put_var (cdfid_m, id_time, mcip_now(1:len_time),  &
                        start = (/ 1, it /), count = (/ len_time, 1 /) )
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl), TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( first ) THEN  ! write time-independent fields

    DO n = 1, nfld2dxy
      var = TRIM(fld2dxy(n)%fldname)
      rcode = nf90_put_var (cdfid_m, id_fld(n), fld2dxy(n)%fld)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO

    DO n = 1, nfld2dxy_d  ! Some Arakawa-B (dots), some Arakawa-C (faces)
      nn = nfld2dxy + n
      var = TRIM(fld2dxy_d(n)%fldname)
      myendx = fld2dxy_d(n)%iend(1)
      myendy = fld2dxy_d(n)%iend(2)
      rcode = nf90_put_var (cdfid_m, id_fld(nn),  &
                            fld2dxy_d(n)%fld(1:myendx,1:myendy),  &
                            start = (/ 1, 1 /),  &
                            count = (/ myendx, myendy /) )
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO

    IF ( iflufrc ) THEN
      DO n = 1, nfld3dxyl
        nn = nfld2dxy + nfld2dxy_d + n
        var = TRIM(fld3dxyl(n)%fldname)
        rcode = nf90_put_var (cdfid_m, id_fld(nn), fld3dxyl(n)%fld)
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                          TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
      ENDDO
    ENDIF

  ENDIF  ! first

  ntot = nfld2dxy + nfld2dxy_d + nfld3dxyl

  DO n = 1, nfld2dxyt
    nn = ntot + n
    var = TRIM(fld2dxyt(n)%fldname)
    rcode = nf90_put_var (cdfid_m, id_fld(nn), fld2dxyt(n)%fld,  &
                          start = (/ 1, 1, it /),  &
                          count = (/ fld2dxyt(n)%iend(1), &
                                     fld2dxyt(n)%iend(2), 1 /) )
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDDO
  ntot = ntot + nfld2dxyt

  DO n = 1, nfld3dxyzt
    nn = ntot + n
    var = TRIM(fld3dxyzt(n)%fldname)
    rcode = nf90_put_var (cdfid_m, id_fld(nn), fld3dxyzt(n)%fld,  &
                          start = (/ 1, 1, 1, it /),  &
                          count = (/ fld3dxyzt(n)%iend(1), &
                                     fld3dxyzt(n)%iend(2), &
                                     fld3dxyzt(n)%iend(3), 1 /) )
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
    rcode = nf90_put_var (cdfid_m, id_fld(nn), fld3dxyzt_q(n)%fld,  &
                          start = (/ 1, 1, 1, it /),  &
                          count = (/ fld3dxyzt_q(n)%iend(1), &
                                     fld3dxyzt_q(n)%iend(2), &
                                     fld3dxyzt_q(n)%iend(3), 1 /) )
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDDO
  ntot = ntot + nfld3dxyzt_q

  DO n = 1, nfld3dxyzt_d  ! Some Arakawa-B (dots), some Arakawa-C (faces)
    nn = ntot + n
    var = TRIM(fld3dxyzt_d(n)%fldname)
    myendx = fld3dxyzt_d(n)%iend(1)
    myendy = fld3dxyzt_d(n)%iend(2)
    myendz = fld3dxyzt_d(n)%iend(3)
    rcode = nf90_put_var (cdfid_m, id_fld(nn),  &
                          fld3dxyzt_d(n)%fld(1:myendx,1:myendy,1:myendz),  &
                          start = (/ 1, 1, 1, it /),  &
                          count = (/ myendx, myendy, myendz, 1 /) )
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDDO
  ntot = ntot + nfld3dxyzt_d

  IF ( ifsoil ) THEN
    DO n = 1, nfld3dxyst
      nn = ntot + n
      var = TRIM(fld3dxyst(n)%fldname)
      rcode = nf90_put_var (cdfid_m, id_fld(nn), fld3dxyst(n)%fld,  &
                            start = (/ 1, 1, 1, it /),  &
                            count = (/ fld3dxyst(n)%iend(1), &
                                       fld3dxyst(n)%iend(2), &
                                       fld3dxyst(n)%iend(3), 1 /) )
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld3dxyst
  ENDIF

  IF ( ifmosaic ) THEN
    DO n = 1, nfld3dxymt
      nn = ntot + n
      var = TRIM(fld3dxymt(n)%fldname)
      rcode = nf90_put_var (cdfid_m, id_fld(nn), fld3dxymt(n)%fld,  &
                            start = (/ 1, 1, 1, it /),  &
                            count = (/ fld3dxymt(n)%iend(1), &
                                       fld3dxymt(n)%iend(2), &
                                       fld3dxymt(n)%iend(3), 1 /) )
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
    ntot = ntot + nfld3dxymt
  ENDIF

  nn = ntot + 1
  var = "ZNU"
  rcode = nf90_put_var (cdfid_m, id_fld(nn), sigmah)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  nn = ntot + 2
  var = "ZNW"
  rcode = nf90_put_var (cdfid_m, id_fld(nn), sigmaf)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  ntot = ntot + 2

  IF ( met_hybrid >= 0 ) THEN

    nn = ntot + 1
    var = "C1H"
    rcode = nf90_put_var (cdfid_m, id_fld(nn), c1h)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    nn = ntot + 2
    var = "C2H"
    rcode = nf90_put_var (cdfid_m, id_fld(nn), c2h)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    nn = ntot + 3
    var = "C1F"
    rcode = nf90_put_var (cdfid_m, id_fld(nn), c1f)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    nn = ntot + 4
    var = "C2F"
    rcode = nf90_put_var (cdfid_m, id_fld(nn), c2f)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    ntot = ntot + 4

  ENDIF  ! met_hybrid >= 0

  IF ( ifsoil ) THEN
    nn = ntot + 1
    var = "ZSOIL"
    rcode = nf90_put_var (cdfid_m, id_fld(nn), xzsoil)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (6,f9400) TRIM(pname), TRIM(var), TRIM(fl),  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF  ! ifsoil = .TRUE.

  first = .FALSE.

END SUBROUTINE outncf
