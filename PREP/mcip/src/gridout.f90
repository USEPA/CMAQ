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

SUBROUTINE gridout (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Grid Out
! Purpose:  Fill arrays for GRID files and outputs.
!             GRID_DOT_2D     GRID_CRO_2D     GRID_CRO_3D
! Revised:  27 Jan 1997  Created for MCIP and generalized CTM.  (D. Byun)
!           04 Feb 1998  LSM method for nonglobal variables changed. (???)
!           30 Apr 1999  PSTAR0 is replaced with PRSFC0 and MAXK.  (D. Byun)
!           20 Sep 2001  Converted to free-form f90.  Added interface for
!                        COLLAPX and removed WORKC.  Added interface for
!                        SANITY and removed NCELLS.  Removed ISTAT from
!                        argument list.  Changed DUMARAY0 and DUMARAY1 to
!                        allocatable.  Removed NLAYS from argument lists
!                        for COMHEADER and COLLAPX.  Changed calling arguments
!                        for COLLAPX.  (T. Otte)
!           20 Nov 2001  Removed "sanity" checks.  (T. Otte)
!           23 Jan 2002  Changed "stop" statements to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  Added logic
!                        to enable "missing" variables in output to have
!                        values of BADVAL3 < AMISS3.  (T. Otte)
!           09 Jun 2003  Removed extraneous variables from output.  Also,
!                        removed unused files GRIDBDY2D and GRIDBDY3D from
!                        output.  (T. Otte)
!           08 Jul 2004  Restored GRIDBDY2D file to output.  Changed local
!                        array allocation to occur only on initial call to
!                        subroutine to avoid memory fragmentation.  (T. Otte)
!           29 Nov 2004  Added PURB.  (T. Otte)
!           25 Feb 2005  Changed names of output file variables by removing
!                        "_G1".  Eliminated GRIDCRO3D for hydrostatic runs.
!                        (T. Otte)
!           19 Aug 2005  Changed internal variable INDEX to IDX to avoid
!                        confusion with F90 intrinsic function.  Modified call
!                        to COLLAPX to reflect that there is one option for
!                        layer collapsing.  (T. Otte)
!           14 Jul 2006  Updated condition for GRID_CRO_3D files to reflect
!                        new vertical structure indicator for WRF.  Updated
!                        comment to reflect that there is only one way to
!                        collapse fields.  Removed dependency on module
!                        LRADMDAT.  Removed unused variable LBND.  Added new
!                        output fields LWMASK.  (T. Otte)
!           11 Apr 2007  Added XLUSE (fractional land use) to output for
!                        two-way modeling.  Refined description of DLUSE to
!                        allow for land use classification source to be
!                        appended.  (T. Otte)
!           18 Apr 2008  Added sample print statements for PURB and fractional
!                        land use when those fields are available.  Replaced
!                        variable NTHIKD with NTHIK and removed options for
!                        NTHIK=0 in GRIDBDY2D.  (T. Otte)
!           22 Sep 2009  Modified field descriptions of fractional land use
!                        categories to include category description.  Use new
!                        variable, XMAPC2, rather than explicitly squaring
!                        XMAPC for output.  Added latitude, longitude, and
!                        squared map-scale factors on U and V faces to
!                        GRIDDOT2D.  Allow output variable PURB to be created
!                        with urban model in WRF.  (T. Otte)
!           11 Aug 2011  Added space in output of DLUSE description to offset
!                        land-use classification from description.  Replaced
!                        modules FDESC3, IODECL3, and PARMS3 with I/O API
!                        module M3UTILIO.  (T. Otte)
!           29 Aug 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Improved error
!                        handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           17 Nov 2016  Changed IF/THEN/ELSE block for urban canopy array to
!                        avoid segmentation fault on some compilers where
!                        second conditional was based on an unallocated array.
!                        Initialized IFRCURB to 0.  (T. Spero)
!-------------------------------------------------------------------------------

  USE metinfo
  USE metvars, ONLY: frc_urb
  USE mcipparm
  USE xvars
  USE coord
  USE m3utilio
  USE files
  USE groutcom
  USE vgrd

  IMPLICIT NONE

  INTEGER                           :: c
  INTEGER                           :: col
  REAL,    SAVE,      ALLOCATABLE   :: dumaray0    ( : , : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dumaray1    ( : , : , : , : )
  INTEGER                           :: idx
  INTEGER                           :: ifrcurb     = 0
  INTEGER                           :: ilu
  CHARACTER(LEN=63)                 :: ifmt1
  INTEGER                           :: k
  INTEGER                           :: k1
  INTEGER                           :: k2
  INTEGER                           :: l
  CHARACTER(LEN=2)                  :: lucat
  INTEGER                           :: lvl
  INTEGER                           :: n
  CHARACTER(LEN=16),  PARAMETER     :: pname       = 'GRIDOUT'
  INTEGER                           :: r
  INTEGER                           :: row
  INTEGER,            INTENT(IN)    :: sdate
  INTEGER,            INTENT(IN)    :: stime
  CHARACTER(LEN=2)                  :: str1
  CHARACTER(LEN=2)                  :: str2
  REAL                              :: xmapmin
  REAL(8)                           :: xorigdot
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

  CHARACTER(LEN=256), PARAMETER :: f6000 = "(1x, a9, 2x, f12.4, 2x, a)"

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR OPENING FILE ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR WRITING TO FILE ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Build common header for I/O API files.
!-------------------------------------------------------------------------------

  CALL comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Compute XORIGDOT and YORIGDOT.  Assume (XORIG_GD, YORIG_GD) are
! for cross cells without boundary.
!-------------------------------------------------------------------------------

  xorigdot = xorig_gd - (0.5 * xcell3d)
  yorigdot = yorig_gd - (0.5 * ycell3d)

!-------------------------------------------------------------------------------
! Build rest of header for GRID_DOT_2D file.
!-------------------------------------------------------------------------------

  DO idx = 1, gd2index
    vtype3d(idx) = m3real
    vname3d(idx) = gd2vname(idx)
    units3d(idx) = gd2units(idx)
    vdesc3d(idx) = gd2vdesc(idx)
  ENDDO

  gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_DOT'

  xorig3d = xorigdot
  yorig3d = yorigdot
  ncols3d = ncols + 1
  nrows3d = nrows + 1
  nthik3d = nthik

  ftype3d = grdded3
  nvars3d = gd2index
  nlays3d = 1
  tstep3d = 0

  ! Write GRID_DOT_2D header.

  IF ( .NOT. open3 (griddot2d, fsunkn3, pname) ) THEN
    WRITE (*,f9000) TRIM(pname), TRIM(griddot2d)
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Build rest of header for GRID_CRO_2D file.
!-------------------------------------------------------------------------------

  DO idx = 1, gc2index
    vtype3d(idx) = m3real
    vname3d(idx) = gc2vname(idx)
    units3d(idx) = gc2units(idx)
    IF ( TRIM(vname3d(idx)) == 'DLUSE' ) THEN
      vdesc3d(idx) = TRIM(gc2vdesc(idx)) // ' ' // TRIM(xlusrc)
    ELSE
      vdesc3d(idx) = gc2vdesc(idx)
    ENDIF
  ENDDO

  gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

  xorig3d = xorig_gd
  yorig3d = yorig_gd
  ncols3d = ncols
  nrows3d = nrows

  ftype3d = grdded3
  nvars3d = gc2index
  nlays3d = 1
  tstep3d = 0

  IF ( iflufrc ) THEN
    idx     = gc2index + 1
    nvars3d = idx + nummetlu
    vtype3d(idx) = m3real
    vname3d(idx) = 'PURB'
    units3d(idx) = 'PERCENT'
    vdesc3d(idx) = 'urban percent of cell based on land'  
    DO ilu = 1, nummetlu
      idx = idx + 1
      vtype3d(idx) = m3real
      WRITE ( lucat, '(i2.2)' ) ilu
      vname3d(idx) = 'LUFRAC_' // lucat
      units3d(idx) = 'FRACTION'
      vdesc3d(idx) = TRIM(xludesc(ilu))
    ENDDO
  ELSE IF ( met_urban_phys >= 1 ) THEN
    IF ( MAXVAL(frc_urb) > 0.0 )  THEN
      ifrcurb = 1
      idx     = gc2index + 1
      nvars3d = idx + nummetlu
      vtype3d(idx) = m3real
      vname3d(idx) = 'PURB'
      units3d(idx) = 'PERCENT'
      vdesc3d(idx) = 'urban percent of cell based on land'  
    ENDIF
  ENDIF

  ! Write GRID_CRO_2D header.

  IF ( .NOT. open3 (gridcro2d, fsunkn3, pname) ) THEN
    WRITE (*,f9000) TRIM(pname), TRIM(gridcro2d)
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Build rest of header for GRID_BDY_2D file.  Header information is the
! same as GRID_CRO_2D except for file type.
!-------------------------------------------------------------------------------

  ftype3d = bndary3

  IF ( iflufrc ) THEN
    nvars3d = gb2index + 1 + nummetlu  ! include PURB and LUFRAC(nummetlu)
  ELSE IF ( ifrcurb == 1 ) THEN
    nvars3d = gb2index + 1             ! include PURB from urban canopy model
  ELSE
    nvars3d = gb2index
  ENDIF

  nlays3d = 1
  tstep3d = 0

  ! Write GRID_BDY_2D header.

  IF ( .NOT. open3 (gridbdy2d, fsunkn3, pname) ) THEN
    WRITE (*,f9000) TRIM(pname), TRIM(gridbdy2d)
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! If the file is required, build rest of header for GRID_CRO_3D file.
!-------------------------------------------------------------------------------

  IF ( ( vgtyp3d /= vgsgph3 ) .AND. ( vgtyp3d /= vgwrfem ) ) THEN  ! nonhydro

    DO idx = 1, gc3index
      vtype3d(idx) = m3real
      vname3d(idx) = gc3vname(idx)
      units3d(idx) = gc3units(idx)
      vdesc3d(idx) = gc3vdesc(idx)
    ENDDO

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

    xorig3d = xorig_gd
    yorig3d = yorig_gd
    ncols3d = ncols
    nrows3d = nrows

    ftype3d = grdded3
    nvars3d = gc3index
    nlays3d = nlays
    tstep3d = 0

    ! Write GRID_CRO_3D header.

    IF ( .NOT. open3 (gridcro3d, fsunkn3, pname) ) THEN
      WRITE (*,f9000) TRIM(pname), TRIM(gridcro3d)
      CALL graceful_stop (pname)
    ENDIF

  ENDIF

!-------------------------------------------------------------------------------
! Assign arrays in GRID_DOT_2D (time independent).
!-------------------------------------------------------------------------------

  xmapmin = MINVAL(xmapd)   ! XMAPMIN gets "recycled" below for XMAPC

  DO row = 1, nrows+1
    r = row + nthik
    DO col = 1, ncols+1
      c = col + nthik
      glat_d   (col,row) = xlatd (c,r)
      glon_d   (col,row) = xlond (c,r)

      IF ( xmapmin < amiss3 ) THEN  ! BADVAL3 < AMISS3
        gmsfsq_d (col,row) = badval3
      ELSE
        gmsfsq_d (col,row) = xmapd (c,r)**2
      ENDIF

      glatu_d  (col,row) = xlatu (c,r)
      glonu_d  (col,row) = xlonu (c,r)

      IF ( xmapmin < amiss3 ) THEN  ! BADVAL3 < AMISS3
        gmsfusq_d(col,row) = badval3
      ELSE
        gmsfusq_d(col,row) = xmapu (c,r)**2
      ENDIF

      glatv_d  (col,row) = xlatv (c,r)
      glonv_d  (col,row) = xlonv (c,r)

      IF ( xmapmin < amiss3 ) THEN  ! BADVAL3 < AMISS3
        gmsfvsq_d(col,row) = badval3
      ELSE
        gmsfvsq_d(col,row) = xmapv (c,r)**2
      ENDIF

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Assign arrays in GRID_CRO_2D (time independent).
!-------------------------------------------------------------------------------

  xmapmin = MINVAL(xmapc)  ! XMAPMIN previously defined for XMAPD

  DO row = 1, nrows
    r = row + nthik
    DO col = 1, ncols
      c = col + nthik
            
      glat_c   (col,row) = xlatc   (c,r)
      glon_c   (col,row) = xlonc   (c,r)

      IF ( xmapmin < amiss3 ) THEN  ! BADVAL3 < AMISS3
        gmsfsq_c (col,row) = badval3
      ELSE
        gmsfsq_c (col,row) = xmapc2 (c,r)  ! already squared
      ENDIF

      gtopo_c  (col,row) = xtopo   (c,r)
      gdluse_c (col,row) = xdluse  (c,r)
      glwmask_c(col,row) = xlwmask (c,r)

      IF ( iflufrc ) THEN
        gpurb_c  (col,row) = xpurb   (c,r)
        DO ilu = 1, nummetlu
          glufrac_c (col,row,ilu) = xluse (c,r,ilu)
        ENDDO
      ELSE IF ( ifrcurb == 1 ) THEN  ! include PURB from urban canopy model
        gpurb_c  (col,row) = xpurb   (c,r)
      ENDIF

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Assign arrays in GRID_BDY_2D (time independent).
!-------------------------------------------------------------------------------

  idx = 0

  ! Southern boundary moving west to east from column 1 (in output grid) to
  ! column NCOLS+NTHIK.

  DO r = 1, nthik
    DO c = 1 + nthik, ncols_x

      idx = idx + 1

      glat_b   (idx) = xlatc   (c,r)
      glon_b   (idx) = xlonc   (c,r)

      IF ( xmapmin < amiss3 ) THEN  ! BADVAL3 < AMISS3
        gmsfsq_b (idx) = badval3
      ELSE
        gmsfsq_b (idx) = xmapc2 (c,r)  ! already squared
      ENDIF

      gtopo_b  (idx) = xtopo   (c,r)
      gdluse_b (idx) = xdluse  (c,r)
      glwmask_b(idx) = xlwmask (c,r)

      IF ( iflufrc ) THEN
        gpurb_b  (idx) = xpurb   (c,r)
        DO ilu = 1, nummetlu
          glufrac_b(idx,ilu) = xluse(c,r,ilu)
        ENDDO
      ELSE IF ( ifrcurb == 1 ) THEN  ! include PURB from urban canopy model
        gpurb_b  (idx) = xpurb   (c,r)
      ENDIF

    ENDDO
  ENDDO

  ! Eastern boundary moving south to north from row 1 (in output grid) to
  ! row NROWS+NTHIK.

  DO r = 1+nthik, nrows_x
    DO l = 1, nthik

      c = ncols_x - nthik + l
      idx = idx + 1

      glat_b   (idx) = xlatc   (c,r)
      glon_b   (idx) = xlonc   (c,r)

      IF ( xmapmin < amiss3 ) THEN  ! BADVAL3 < AMISS3
        gmsfsq_b (idx) = badval3
      ELSE
        gmsfsq_b (idx) = xmapc2 (c,r)  ! already squared
      ENDIF

      gtopo_b  (idx) = xtopo   (c,r)
      gdluse_b (idx) = xdluse  (c,r)
      glwmask_b(idx) = xlwmask (c,r)

      IF ( iflufrc ) THEN
        gpurb_b  (idx) = xpurb   (c,r)
        DO ilu = 1, nummetlu
          glufrac_b(idx,ilu) = xluse(c,r,ilu)
        ENDDO
      ELSE IF ( ifrcurb == 1 ) THEN  ! include PURB from urban canopy model
        gpurb_b  (idx) = xpurb   (c,r)
      ENDIF

    ENDDO
  ENDDO

  ! Northern boundary moving west to east from column 1-NTHIK (in output grid)
  ! to column NCOLS.

  DO l = 1, nthik
    r = nrows_x - nthik + l
    DO c = 1, ncols_x - nthik
      idx = idx + 1

      glat_b   (idx) = xlatc   (c,r)
      glon_b   (idx) = xlonc   (c,r)

      IF ( xmapmin < amiss3 ) THEN  ! BADVAL3 < AMISS3
        gmsfsq_b (idx) = badval3
      ELSE
        gmsfsq_b (idx) = xmapc2 (c,r)
      ENDIF

      gtopo_b  (idx) = xtopo   (c,r)
      gdluse_b (idx) = xdluse  (c,r)
      glwmask_b(idx) = xlwmask (c,r)

      IF ( iflufrc ) THEN
        gpurb_b  (idx) = xpurb   (c,r)
        DO ilu = 1, nummetlu
          glufrac_b(idx,ilu) = xluse(c,r,ilu)
        ENDDO
      ELSE IF ( ifrcurb == 1 ) THEN  ! include PURB from urban canopy model
        gpurb_b  (idx) = xpurb   (c,r)
      ENDIF

    ENDDO
  ENDDO
      
  ! Western boundary moving south to north from row 1-NTHIK (in output grid)
  ! to row NROWS.

  DO r = 1, nrows_x - nthik
    DO c = 1, nthik

      idx = idx + 1

      glat_b   (idx) = xlatc   (c,r)
      glon_b   (idx) = xlonc   (c,r)

      IF ( xmapmin < amiss3 ) THEN  ! BADVAL3 < AMISS3
        gmsfsq_b (idx) = badval3
      ELSE
        gmsfsq_b (idx) = xmapc2 (c,r)  ! already squared
      ENDIF

      gtopo_b  (idx) = xtopo   (c,r)
      gdluse_b (idx) = xdluse  (c,r)
      glwmask_b(idx) = xlwmask (c,r)

      IF ( iflufrc ) THEN
        gpurb_b  (idx) = xpurb   (c,r)
        DO ilu = 1, nummetlu
          glufrac_b(idx,ilu) = xluse(c,r,ilu)
        ENDDO
      ELSE IF ( ifrcurb == 1 ) THEN  ! include PURB from urban canopy model
        gpurb_b  (idx) = xpurb   (c,r)
      ENDIF

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Collapse 3-D 'X' arrays.  (Original values are replaced temporarily.)
! For safe collpasing, store information first in DUMARRAY.
!-------------------------------------------------------------------------------

  IF ( ( vgtyp3d /= vgsgph3 ) .AND. ( vgtyp3d /= vgwrfem ) ) THEN  ! nonhydro

    IF ( metlay /= nlays ) THEN

      IF ( .NOT. ALLOCATED ( dumaray0 ) )  &
        ALLOCATE ( dumaray0 ( ncols_x, nrows_x, 0:metlay, 1 ) )
      IF ( .NOT. ALLOCATED ( dumaray1 ) )  &
        ALLOCATE ( dumaray1 ( ncols_x, nrows_x, 1:metlay, 1 ) )

      DO k = 1, metlay
        DO r = 1, nrows_x
          DO c = 1, ncols_x
            dumaray1(c,r,k,1) = x3htm(c,r,k)
          ENDDO 
        ENDDO
      ENDDO

      DO k = 0, metlay
        DO r = 1, nrows_x
          DO c = 1, ncols_x
            dumaray0(c,r,k,1) = x3htf(c,r,k)
          ENDDO 
        ENDDO
      ENDDO    

      CALL collapx (x3htm, xx3midl, x3midl)
      CALL collapx (x3htf, xx3face, x3face)

    ENDIF  

!-------------------------------------------------------------------------------
! If the file is required, assign arrays in GRID_CRO_3D (time independent).  
!-------------------------------------------------------------------------------

    DO row = 1, nrows
      r = row + nthik
      DO col = 1, ncols
        c = col + nthik
        DO lvl = 1, nlays
          k = lvl

          gx3htf_c (col,row,lvl) = x3htf (c,r,k)
          gx3htm_c (col,row,lvl) = x3htm (c,r,k)
 
        ENDDO
      ENDDO
    ENDDO

  ENDIF  ! NOT hydrostatic

!-------------------------------------------------------------------------------
! Write GRID_DOT_2D data (time independent data).
! Read FDESC3 from GRIDDOT2D to get header information.
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (griddot2d) ) THEN
    CALL m3err ('GRIDOUT', sdate, stime,  &
                'Could not read DESC of ' // griddot2d // ' file', .TRUE.)
  ENDIF

  IF ( .NOT. write3 (griddot2d, allvar3, sdate, stime, gd2) ) THEN
    WRITE (*,f9100) TRIM(pname), TRIM(griddot2d)
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Write GRID_CRO_2D data (time independent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (gridcro2d) ) THEN
    CALL m3err ('GRIDOUT', sdate, stime,  &
                'Could not read DESC of ' // gridcro2d // ' file', .TRUE.)
  ENDIF

  DO l = 1, gc2index
    IF ( .NOT. write3 (gridcro2d, vname3d(l), sdate, stime,  &
                       gc2(1,1,l)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(gridcro2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

  IF ( iflufrc ) THEN
    IF ( .NOT. write3 (gridcro2d, vname3d(gc2index+1), sdate, stime,  &
                       gc2(1,1,gc2index+1)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(gridcro2d)
      CALL graceful_stop (pname)
    ENDIF
    DO ilu = 1, nummetlu
      IF ( .NOT. write3 (gridcro2d, vname3d(gc2index+1+ilu), sdate, stime,  &
                         gc2(1,1,gc2index+1+ilu)) ) THEN
        WRITE (*,f9100) TRIM(pname), TRIM(gridcro2d)
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
  ELSE IF ( ifrcurb == 1 ) THEN  ! include PURB from urban canopy model
    IF ( .NOT. write3 (gridcro2d, vname3d(gc2index+1), sdate, stime,  &
                       gc2(1,1,gc2index+1)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(gridcro2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Write GRID_BDY_2D data (time independent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (gridbdy2d) ) THEN
    CALL m3err ('GRIDOUT', sdate, stime,  &
                'Could not read DESC of ' // gridbdy2d // ' file', .TRUE.)
  ENDIF

  DO l = 1, gb2index
    IF ( .NOT. write3 (gridbdy2d, vname3d(l), sdate, stime,  &
                       gb2(1,l)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(gridbdy2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDDO

  IF ( iflufrc ) THEN
    IF ( .NOT. write3 (gridbdy2d, vname3d(gb2index+1), sdate, stime,  &
                       gb2(1,gb2index+1)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(gridbdy2d)
      CALL graceful_stop (pname)
    ENDIF
    DO ilu = 1, nummetlu
      IF ( .NOT. write3 (gridbdy2d, vname3d(gb2index+1+ilu), sdate, stime,  &
                         gb2(1,gb2index+1+ilu)) ) THEN
        WRITE (*,f9100) TRIM(pname), TRIM(gridbdy2d)
        CALL graceful_stop (pname)
      ENDIF
    ENDDO
  ELSE IF ( ifrcurb == 1 ) THEN  ! include PURB from urban canopy model
    IF ( .NOT. write3 (gridbdy2d, vname3d(gb2index+1), sdate, stime,  &
                       gb2(1,gb2index+1)) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(gridbdy2d)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! If the file is required, write GRID_CRO_3D data (time independent data).
!-------------------------------------------------------------------------------

  IF ( ( vgtyp3d /= vgsgph3 ) .AND. ( vgtyp3d /= vgwrfem ) ) THEN  ! nonhydro

    IF ( .NOT. desc3 (gridcro3d) ) THEN
      CALL m3err ('GRIDOUT', sdate, stime,  &
                  'Could not read DESC of ' // gridcro3d // ' file', .TRUE.)
    ENDIF

    IF ( .NOT. write3 (gridcro3d, allvar3, sdate, stime, gc3) ) THEN
      WRITE (*,f9100) TRIM(pname), TRIM(gridcro3d)
      CALL graceful_stop (pname)
    ENDIF

!-------------------------------------------------------------------------------
! For safe collpasing, restore information from DUMARRAY.
!-------------------------------------------------------------------------------

    IF ( metlay /= nlays ) THEN

      DO k = 1, metlay
        DO r = 1, nrows_x
          DO c = 1, ncols_x
            x3htm(c,r,k) = dumaray1(c,r,k,1)
          ENDDO 
        ENDDO
      ENDDO

      DO k = 0, metlay
        DO r = 1, nrows_x
          DO c = 1, ncols_x
            x3htf(c,r,k) = dumaray0(c,r,k,1) 
          ENDDO 
        ENDDO
      ENDDO 

!     DEALLOCATE ( dumaray0 )  ! commented out to avoid memory fragmentation
!     DEALLOCATE ( dumaray1 )  ! commented out to avoid memory fragmentation

    ENDIF   

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
           &    // str2 // "(2x,f12.4))"
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

  ENDIF  ! NOT hydrostatic

  WRITE (*,'(/,a,/)') '- GRIDOUT: Printing sample cells in output grid'

  DO n = 1, gc2index
    WRITE (*,f6000) TRIM(gc2vname(n)), gc2(lprt_col,lprt_row,n), gc2units(n)
  ENDDO

  IF ( iflufrc ) THEN
    WRITE (*,f6000) TRIM(vname3d(gc2index+1)),  &
                    gc2(lprt_col,lprt_row,gc2index+1), units3d(gc2index+1)
    DO ilu = 1, nummetlu
      WRITE (*,f6000) TRIM(vname3d(gc2index+1+ilu)),          &
                      gc2(lprt_col,lprt_row,gc2index+1+ilu),  &
                      units3d(gc2index+1+ilu)
    ENDDO
  ELSE IF ( ifrcurb == 1 ) THEN  ! include PURB from urban canopy model
    WRITE (*,f6000) TRIM(vname3d(gc2index+1)),  &
                    gc2(lprt_col,lprt_row,gc2index+1), units3d(gc2index+1)
  ENDIF

  DO n = 1, gd2index
    WRITE (*,f6000) TRIM(gd2vname(n)), gd2(lprt_col,lprt_row,n), gd2units(n)
  ENDDO

  IF ( ( vgtyp3d /= vgsgph3 ) .AND. ( vgtyp3d /= vgwrfem ) ) THEN  ! nonhydro
    DO n = 1, gc3index
      WRITE (*,ifmt1) TRIM(gc3vname(n)), gc3(lprt_col,lprt_row,:,n)
    ENDDO
  ENDIF

END SUBROUTINE gridout
