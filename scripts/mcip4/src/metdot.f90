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

SUBROUTINE metdot (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Meteorology on Dot Points
! Purpose:  Compute and output MET_DOT_3D parameters.
! Revised:  05 Jan 1997  Created for MCIP and generalized CTM.  (D. Byun)
!           04 Feb 1998  LSM include for nonglobal variables changed.  (???)
!           30 Apr 1999  MAXK.  (D. Byun)
!           20 Sep 2001  Converted to free-form f90.  Added interface for
!                        COLLAPX and removed WORKC.  Added interface for
!                        SANITY and removed NCELLS.  Removed IFTYPE and
!                        dependence of TSTEP3D on this variable.  This routine
!                        always accesses time-dependent data.  Removed ISTAT
!                        from argument list.  Changed DUMARAY1 and JDENM to 
!                        allocatable.  Removed NLAYS from argument lists for
!                        COMHEADER and COLLAPX.  Changed vertical dimension
!                        on JDENM from MAXK to NLAYS.   (T. Otte)
!           20 Nov 2001  Removed "sanity" checks.  (T. Otte)
!           24 Jan 2002  Changed "stop" statements to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  Added logic
!                        to enable "missing" variables in output to have
!                        values of BADVAL3 < AMISS3.  (T. Otte)
!           18 Mar 2003  Corrected bugs in calculations in UHAT_S and VHAT_T:
!                        added calls to COLLAPX for XUHAT and XVHAT, and fixed
!                        algorithm to translate from dot points to square and
!                        triangle points.  Also changed COLLAPX calls for
!                        UHAT_S and VHAT_T to be on winds coupled with density
!                        and Jacobian for better mass conservation.  (J. Pleim
!                        and T. Otte)
!           08 Jul 2004  Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  (T. Otte)
!           04 Feb 2005  Changed calculations of JDENM, XUHAT_S, and XVHAT_T to
!                        reflect that XUU_S and XVV_T contain wind components
!                        on flux points, rather than dot points, and are no
!                        longer coupled with map-scale factors (as XUHAT and
!                        XVHAT).  Changed output file names by removing "_G1".
!                        (T. Otte)
!           19 Aug 2005  Changed internal variable INDEX to IDX to avoid
!                        confusion with F90 intrinsic function.  Modified call
!                        to COLLAPX to reflect that there is only one option
!                        for layer collapsing.  (T. Otte)
!           19 Jun 2006  Updated comment to reflect that there is only one
!                        way to collapse fields.  Made XORIGDOT and YORIGDOT
!                        double precision and updated calculated as such.
!                        Removed unused variables CP1, ILG, JDATE, L, LVL,
!                        and RP1.  (T. Otte)
!           29 Oct 2009  Added user option to output u- and v-component winds
!                        on C-staggered grid.  (T. Otte)
!           11 Aug 2011  Replaced modules FDESC3, IODECL3, and PARMS3 with
!                        I/O API module M3UTILIO.  (T. Otte)
!           29 Aug 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Improved error
!                        handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE coord
  USE m3utilio
  USE files
  USE mdoutcom
  USE vgrd

  IMPLICIT NONE

  INTEGER                           :: c
  INTEGER                           :: cm1
  INTEGER                           :: col
  REAL,    SAVE,      ALLOCATABLE   :: dumaray1    ( : , : , : , : )
  LOGICAL, SAVE                     :: first       = .TRUE.
  INTEGER                           :: idx
  CHARACTER(LEN=63)                 :: ifmt1
  INTEGER                           :: iuvcout
  REAL,    SAVE,      ALLOCATABLE   :: jdenm       ( : , : , : )
  INTEGER                           :: k
  INTEGER                           :: k1
  INTEGER                           :: k2
  INTEGER                           :: l
  INTEGER                           :: n
  CHARACTER(LEN=16),  PARAMETER     :: pname       = 'METDOT'
  INTEGER                           :: r
  INTEGER                           :: rm1
  INTEGER                           :: row
  INTEGER,           INTENT(IN)     :: sdate
  INTEGER,           INTENT(IN)     :: stime
  CHARACTER(LEN=2)                  :: str1
  CHARACTER(LEN=2)                  :: str2
  REAL(8)                           :: xorigdot
  REAL,   SAVE,      ALLOCATABLE    :: xuhat_s     ( : , : , : )
  REAL,   SAVE,      ALLOCATABLE    :: xvhat_t     ( : , : , : )
  REAL(8)                           :: yorigdot

  INTERFACE

    SUBROUTINE collapx (aa, vertin, vertout)
      IMPLICIT NONE
      REAL,               INTENT(INOUT) :: aa         ( : , : , : )
      REAL,               INTENT(IN)    :: vertin     ( : )
      REAL,               INTENT(IN)    :: vertout    ( : )
    END SUBROUTINE collapx

  END INTERFACE

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR OPENING FILE ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR WRITING TO FILE ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( jdenm   ) )  &
    ALLOCATE ( jdenm   (ncols_x+1, nrows_x+1, metlay) )
  IF ( .NOT. ALLOCATED ( xuhat_s ) )  &
    ALLOCATE ( xuhat_s (ncols_x+1, nrows_x+1, metlay) )
  IF ( .NOT. ALLOCATED ( xvhat_t ) )  &
    ALLOCATE ( xvhat_t (ncols_x+1, nrows_x+1, metlay) )

!-------------------------------------------------------------------------------
! If first time, build rest of header for MET_DOT_3D file.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    CALL comheader (sdate, stime)

    ! Compute XORIGDOT and YORIGDOT.
    ! Assume (XORIG_GD, YORIG_GD) are for cross cells without boundary.

    xorigdot = xorig_gd - 0.5d0 * xcell_gd
    yorigdot = yorig_gd - 0.5d0 * ycell_gd

    DO idx = 1, md3index
      vtype3d(idx) = m3real
      vname3d(idx) = md3vname(idx)
      units3d(idx) = md3units(idx)
      vdesc3d(idx) = md3vdesc(idx)
    ENDDO

    IF ( luvcout > 0 ) THEN
      iuvcout = 2
      idx = md3index + 1
      vtype3d(idx) = m3real
      vname3d(idx) = 'UWINDC'
      units3d(idx) = 'M/S'
      vdesc3d(idx) = 'U-comp. of true wind at W-E faces'
      idx = md3index + 2
      vtype3d(idx) = m3real
      vname3d(idx) = 'VWINDC'
      units3d(idx) = 'M/S'
      vdesc3d(idx) = 'V-comp. of true wind at S-N faces'
    ELSE
      iuvcout = 0
    ENDIF

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_DOT'

    xorig3d = xorigdot
    yorig3d = yorigdot
    ncols3d = ncols + 1
    nrows3d = nrows + 1

    ftype3d = grdded3
    nvars3d = md3index + iuvcout
    nlays3d = nlays
    tstep3d = grstep

    ! Write MET_DOT_3D header.

    IF ( .NOT. open3 (metdot3d, fsunkn3, pname) ) THEN
      WRITE (*,f9000) TRIM(pname), TRIM(metdot3d)
      CALL graceful_stop (pname)
    ENDIF

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Build common header for I/O API files.
!-------------------------------------------------------------------------------

  CALL comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Compute density*Jacobian for X-domain.  Persist outermost row and column
! of cross-point JDENM field into non-physical dot-point array.  These data
! will be used to fill non-physical extra row/column of square/triangle flux
! points.  These data are just available so that large "missing" values do not
! corrupt UHAT_S and VHAT_T fields for display purposes.  Note that JDENM is
! the portion of the UHAT_JD and VHAT_JD that includes (rho*J/m^2)*m.
! Also note that this is a subtle deviation from equations 12-120a and 12-120b
! in "Science Algorithms of the EPA Models-3/CMAQ Modeling System"
! (EPA/600/R-99/030), since contravariant wind components (equations 12-119a and
! 12-119b) are no longer computed in MCIP on dot points, and they are not stored
! locally.
!-------------------------------------------------------------------------------

  DO k = 1, metlay
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        jdenm(c,r,k) = xdensam(c,r,k) * x3jacobm(c,r,k) / xmapc(c,r)
     
      ENDDO
    ENDDO
  ENDDO

  jdenm(ncols_x+1,:,:) = jdenm(ncols_x,:,:)
  jdenm(:,nrows_x+1,:) = jdenm(:,nrows_x,:)

!-------------------------------------------------------------------------------
! Compute contra-u*density*Jacobian at flux (square) points,
! and compute contra-v*density*Jacobian at flux (triangle) points.
!-------------------------------------------------------------------------------

  DO k = 1, metlay
    DO r = 1, nrows_x+1
      rm1 = MAX( r-1, 1 )
      DO c = 1, ncols_x+1
        cm1 = MAX( c-1, 1 )

        xuhat_s(c,r,k) = 0.5 * ( jdenm(cm1,r,k) + jdenm(c,r,k) ) * xuu_s(c,r,k)
        xvhat_t(c,r,k) = 0.5 * ( jdenm(c,rm1,k) + jdenm(c,r,k) ) * xvv_t(c,r,k)
     
      ENDDO
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Collapse 3-D 'X' arrays.  (Original values are replaced temporarily.)
! For safe collpasing, store information first in DUMARRAY.
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

    IF ( .NOT. ALLOCATED ( dumaray1 ) )  &
      ALLOCATE ( dumaray1 ( ncols_x+1, nrows_x+1, 1:metlay, 4+iuvcout ) )

    DO k = 1, metlay
      DO r = 1, nrows_x+1
        DO c = 1, ncols_x+1
          dumaray1(c,r,k,1) = xuhat_s (c,r,k)
          dumaray1(c,r,k,2) = xvhat_t (c,r,k)
          dumaray1(c,r,k,3) = xuu_d   (c,r,k)
          dumaray1(c,r,k,4) = xvv_d   (c,r,k)
        ENDDO 
      ENDDO
    ENDDO

    CALL collapx (xuhat_s, xx3midl, x3midl)
    CALL collapx (xvhat_t, xx3midl, x3midl)
    CALL collapx (xuu_d,   xx3midl, x3midl)
    CALL collapx (xvv_d,   xx3midl, x3midl)

    IF ( luvcout > 0 ) THEN
      DO k = 1, metlay
        DO r = 1, nrows_x+1
          DO c = 1, ncols_x+1
            dumaray1(c,r,k,5) = xuu_s   (c,r,k)
            dumaray1(c,r,k,6) = xvv_t   (c,r,k)
          ENDDO 
        ENDDO
      ENDDO
      CALL collapx (xuu_s,   xx3midl, x3midl)
      CALL collapx (xvv_t,   xx3midl, x3midl)
    ENDIF
      
  ENDIF  

!-------------------------------------------------------------------------------
! Assign arrays in MET_DOT_3D (time dependent).
!-------------------------------------------------------------------------------

  DO k = 1, nlays
    DO row = 1, nrows+1
      r = row + nthik
      DO col = 1, ncols+1
        c = col + nthik

        uu_d   (col,row,k) = xuu_d   (c,r,k)
        vv_d   (col,row,k) = xvv_d   (c,r,k)
        uhat_s (col,row,k) = xuhat_s (c,r,k)
        vhat_t (col,row,k) = xvhat_t (c,r,k)

        IF ( luvcout > 0 ) THEN
          uu_s   (col,row,k) = xuu_s   (c,r,k)
          vv_t   (col,row,k) = xvv_t   (c,r,k)
        ENDIF

      ENDDO
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Write MET_DOT_3D data (time dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (metdot3d) ) THEN
    CALL m3err ('METDOT', sdate, stime,  &
                'Could not read DESC of ' // metdot3d // ' file', .TRUE.)
  ENDIF

  DO l = 1, md3index
    IF ( .NOT. write3 (metdot3d, vname3d(l), sdate, stime,  &
                       md3(1,1,1,l)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metdot3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

  IF ( luvcout > 0 ) THEN
    idx = md3index + 1
    IF ( .NOT. write3 (metdot3d, vname3d(idx), sdate, stime, uu_s) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metdot3d)
      CALL graceful_stop (pname)
    ENDIF
    idx = md3index + 2
    IF ( .NOT. write3 (metdot3d, vname3d(idx), sdate, stime, vv_t) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(metdot3d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! For safe collpasing, restore information from DUMARAY.
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

    DO k = 1, metlay
      DO r = 1, nrows_x+1
        DO c = 1, ncols_x+1
          xuhat_s (c,r,k) = dumaray1(c,r,k, 1)
          xvhat_t (c,r,k) = dumaray1(c,r,k, 2)
          xuu_d   (c,r,k) = dumaray1(c,r,k, 3)
          xvv_d   (c,r,k) = dumaray1(c,r,k, 4)
        ENDDO 
      ENDDO
    ENDDO

    IF ( luvcout > 0 ) THEN
      DO k = 1, metlay
        DO r = 1, nrows_x+1
          DO c = 1, ncols_x+1
            xuu_s   (c,r,k) = dumaray1(c,r,k, 5)
            xvv_t   (c,r,k) = dumaray1(c,r,k, 6)
          ENDDO 
        ENDDO
      ENDDO
    ENDIF

  ENDIF  

!-------------------------------------------------------------------------------
! Deallocate variables.
!-------------------------------------------------------------------------------

! DEALLOCATE ( dumaray1 )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( jdenm    )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( xuhat_s  )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( xvhat_t  )  ! commented out to avoid memory fragmentation

!-------------------------------------------------------------------------------
! Print sample output to log file.
!-------------------------------------------------------------------------------

  k1 = nlays / 5
  k2 = MOD(nlays, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a9,5(2x,f12.4)," // str1 // "(/,10x,5(2x,f12.4)),/,10x," &
        & // str2 // "(2x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a9,5(2x,f12.4)," // str1 // "(/,10x,5(2x,f12.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a9,5(2x,f12.4),/,10x," // str2 // "(2x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a9,5(2x,f12.4))"
    ENDIF
  ENDIF

  WRITE (*,'(/,a,/)') '- METDOT: Printing sample cells in output grid'

  DO n = 1, md3index
    WRITE (*,ifmt1) TRIM(md3vname(n)), md3(lprt_col,lprt_row,:,n)
  ENDDO

  IF ( luvcout > 0 ) THEN  ! Output u- and v-component winds on C grid
    WRITE (*,ifmt1) 'UWINDC',  uu_s(lprt_col,lprt_row,:)
    WRITE (*,ifmt1) 'VWINDC',  vv_t(lprt_col,lprt_row,:)
  ENDIF

END SUBROUTINE metdot
