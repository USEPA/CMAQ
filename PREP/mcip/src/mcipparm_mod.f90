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

MODULE mcipparm

!-------------------------------------------------------------------------------
! Name:     MCIP Parameters
! Purpose:  Contains MCIP parameters.
! Revised:  28 Jan 1997  Original version.  (D. Byun)
!           16 May 1997  Moved land use parameters into LANDPARM.EXT.  (???)
!           23 May 1997  Include MAXI, MAXJ, MAXK definitions.  (???)
!           01 May 2000  Added NTHIK=0 option.  (???)
!           20 Sep 2001  Converted to free-form f90 and changed name from
!                        MCIPPARM.EXT to module_mcipparm.F.  Removed
!                        explicit parameter definitions of grid dimensions.
!                        Added several variables formerly contained in
!                        MCIPCOM such that all user definitions (aside from
!                        those in the input meteorology) are contained in
!                        this module.  Added two new dry deposition species
!                        for atrazine.  (T. Otte)
!           03 Oct 2001  Added variable COORDNAM.  (T. Otte)
!           14 Jan 2002  Added new dry deposition species, methanol.
!                        (Y. Wu and T. Otte)
!           09 Jun 2003  Added new dry deposition species:  N2O5, NO3, and
!                        generic aldehyde.  Removed dry deposition species,
!                        ATRA and ATRAP, from output.  (D. Schwede and T. Otte)
!           11 Aug 2004  Modified code so that arrays are made available in
!                        output only if user options in MM5 generate those
!                        data, so NQSPECIES and NPXFIELDS are new variables.
!                        Added T2OUT.  Removed MAXVAR and LUTYPE.  Moved LWATER
!                        here from LRADMDAT.  (T. Otte)
!           29 Nov 2004  Added IFLUFRC to determine if fractional land use
!                        fields will be processed.  Added NUMMETLU to include
!                        number of land use categories in incoming meteorology
!                        data.  (T. Otte)
!           26 May 2005  Removed NDX and option to interpolate to finer scale
!                        meteorology.  Removed I0LUSE, J0LUSE, and BMAX, which
!                        are no longer used.  Removed NDEP and made its usage
!                        explicit in PBLPKG and RADMDRY.  Removed IWIND and
!                        made its usage explicit in VERTHYD.  Changed NCG_I and
!                        NCG_J to NCG_X and NCG_Y, changed I0 and J0 to X0 and
!                        Y0, and changed LPRT_METI and LPRT_METJ to LPRT_METX
!                        and LPRT_METY to make code more general.  Added
!                        new variables LDDGAS, LDDCL, and LDDHG to account for
!                        optional dry deposition of chlorine and mercury
!                        species.  Allowed LTOTG to be defined later from
!                        LDDGAS, LDDCL, and LDDHG.  Increased MAXLAYS from 50
!                        to 100.  Added logical variable NEEDLAYERS to allow
!                        users to use all input meteorology layers in MCIP
!                        without specifying in the namelist a priori.  (T. Otte)
!           19 Aug 2005  Removed unused variables NO_MAX and NO_MIN.  Updated
!                        comment on definition of ISESN.  (T. Otte)
!           01 Aug 2007  Added IMPLICIT NONE.  Removed user options for LPBL,
!                        LRAD, LCLD, and LHYDOUT.  Updated comment for LDDEP
!                        to reflect new option (0) and removed options (1-3).
!                        Removed RADMdry variable ISESN.  Removed T2OUT.
!                        Moved IFT2M and IFW10M from module METINFO.  Created
!                        new variables IFVEG, IFLAI, IFMOL, IFRESIST, IFWR, and
!                        IFSOIL.  Eliminated logical variable PX and NPXFIELDS.
!                        Eliminated two obsolete chlorine species.  Moved
!                        program and version descriptors from VSTAMP to here.
!                        Added array FDESC to capture metadata.  (T. Otte)
!           03 Nov 2008  Removed NTHIKD and NBNDYD.  Added IFQ2M, IFTKE, IFTKEF,
!                        and ERADM.  Increased number of dry deposition species
!                        from 26 to 31.  Updated release stamp.  (T. Otte)
!           24 Nov 2008  Updated release stamp.  (T. Otte)
!           23 Dec 2008  Added user-definable reference latitude for WRF
!                        Lambert conformal data sets.  Best used for
!                        consistency with existing MM5 data sets.  Updated
!                        release stamp.  (T. Otte)
!           29 Oct 2009  Added user-definable option to compute and output
!                        3D potential vorticity (LPV).  Added user-definable
!                        option to output meteorological model's vertical
!                        velocity (WWIND).  Added user-definable option to
!                        output u- and v-component winds on C-staggered grid.
!                        Added logical variable IFLUWRFOUT to determine if
!                        land use fractions are in WRF output or GEOGRID output.
!                        Added logical variable IFZNT to determine if roughness
!                        length is in input meteorological model file.  Updated
!                        release stamp.  (T. Otte)
!           19 Mar 2010  Updated release stamp.  (T. Otte)
!           20 Jan 2011  Removed option to compute dry deposition velocities
!                        in MCIP.  Added logical variable NEEDSEAICE to cue
!                        approximation of sea ice field if it is not available
!                        in input.  Moved LWATER to be a local variable in
!                        GETLUSE.  Updated release stamp.  (T. Otte)
!           01 Sep 2011  Replaced module FDESC3 with I/O API module M3UTILIO.
!                        Replaced F77 character declarations with F90 standard.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           09 Sep 2011  Updated release stamp.  (T. Otte)
!           11 May 2012  Updated release stamp.  (T. Otte)
!           21 Aug 2012  Updated release stamp.  (T. Otte)
!           11 Sep 2012  Updated release stamp.  (T. Otte)
!           30 Sep 2013  Updated release stamp.  (T. Otte)
!           26 Nov 2014  Updated release stamp.  (T. Spero)
!           27 Apr 2015  Added logical variable IFCLD3D to indicate if 3D cloud
!                        fraction field is available in incoming meteorological
!                        model output and represents resolved cloud fraction.
!                        Updated release stamp.  (T. Spero)
!           25 Aug 2015  Added variable IFMOLACM to control whether Monin-
!                        Obukhov length calculation will be updated with
!                        "corrector" portion of predictor-corrector from
!                        WRF/ACM2.  Updated release stamp.  (T. Spero)
!           08 Sep 2015  Updated release stamp.  (T. Spero)
!           17 Sep 2015  Changed IFMOLACM to IFMOLPX.  Updated release stamp.
!                        (T. Spero)
!           20 Jun 2017  Updated release stamp.  (T. Spero)
!-------------------------------------------------------------------------------

  USE m3utilio, ONLY: mxdesc3

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Dimensions of CTM domain.
!-------------------------------------------------------------------------------

  INTEGER            :: ncols         ! number of grid columns (X direction)
  INTEGER            :: nrows         ! number of grid rows (Y direction)
  INTEGER            :: nlays         ! number of vertical layers
  INTEGER, PARAMETER :: nthik = 1     ! boundary thickness (cells)
  INTEGER            :: nbndy         ! number of cells in one layer of boundary
  INTEGER            :: nbdrytrim     ! number of meteorology "boundary" points
                                      ! to remove (on each of four sides)
  INTEGER            :: ncg_x         ! coarse grid X
  INTEGER            :: ncg_y         ! coarse grid Y

  INTEGER, PARAMETER :: maxlays = 100 ! max allowed in NLAYS
  LOGICAL            :: needlayers    ! "TRUE" if using all input met layers
                                      ! without defining in namelist

!-------------------------------------------------------------------------------
! Horizontal dimensions of "X" domain (CTM + BNDARY area).
!-------------------------------------------------------------------------------

  INTEGER            :: nrows_x
  INTEGER            :: ncols_x

!-------------------------------------------------------------------------------
! Dimensions for MET input data.
!-------------------------------------------------------------------------------

  INTEGER            :: metrow      ! met. grid dimension for rows (N-S)
  INTEGER            :: metcol      ! met. grid dimension for columns (E-W)
  INTEGER            :: metlay      ! met. grid dimension for layers

!-------------------------------------------------------------------------------
! Other dimensional parameters.
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: maxluc = 100  ! max number of land use categories
  INTEGER            :: nqspecies     ! number of hydrometeor species in met
  INTEGER            :: nummetlu      ! number of met. land use categories
  REAL               :: eradm         ! earth radius [m]
  REAL               :: wrf_lc_ref_lat ! WRF Lambert conformal ref. latitude

!-------------------------------------------------------------------------------
! Flags to indicate whether or not fields are available in input meteorology.
!-------------------------------------------------------------------------------

  LOGICAL            :: ifcld3d       ! 3D resolved clouds in input file?
  LOGICAL            :: iflai         ! leaf area index in input file?
  LOGICAL            :: iflufrc       ! fractional land use available?
  LOGICAL            :: ifluwrfout    ! is fractional land use in WRF history?
  LOGICAL            :: ifmol         ! Monin-Obukhov length in input file?
  LOGICAL            :: ifmolpx       ! MOL to be updated from WRF/PX?
  LOGICAL            :: ifq2m         ! 2-m mixing ratio in input file?
  LOGICAL            :: ifresist      ! aero and stom resistances in input file?
  LOGICAL            :: ifsoil        ! soil mois, temp, and type in input file?
  LOGICAL            :: ift2m         ! 2-m temperature in input file?
  LOGICAL            :: iftke         ! turbulent kinetic energy in input file?
  LOGICAL            :: iftkef        ! TKE (if exists) on full levels?
  LOGICAL            :: ifveg         ! vegetation fraction in input file?
  LOGICAL            :: ifw10m        ! 10-m wind components in input file?
  LOGICAL            :: ifwr          ! canopy wetness in input file?
  LOGICAL            :: ifznt         ! roughness length in input file?
  LOGICAL            :: needseaice    ! sea ice in input file?

!-------------------------------------------------------------------------------
! Run Options.
!-------------------------------------------------------------------------------

  INTEGER :: lpv             ! user input: 0 = Do not compute or output PV
                             !             1 = Compute and output PV

  INTEGER :: lwout           ! user input: 0 = Do not output WWIND
                             !             1 = Output WWIND

  INTEGER :: luvcout         ! user input: 0 = Do not output UWINDC and VWINDC
                             !             1 = Output UWINDC and VWINDC

!-------------------------------------------------------------------------------
! Grid/Domain Related Parameters
!-------------------------------------------------------------------------------

  INTEGER :: x0              ! user input: lower-left corner of CTM on met grid
  INTEGER :: y0              ! user input: lower-left corner of CTM on met grid

!-------------------------------------------------------------------------------
! Run Time Specifications
!-------------------------------------------------------------------------------

  CHARACTER(LEN=24) :: mcip_start ! user input: YYYY-MO-DD-HH:MI:SS.SSSS
  CHARACTER(LEN=24) :: mcip_end   ! user input: YYYY-MO-DD-HH:MI:SS.SSSS

  INTEGER           :: intvl      ! user input: time interval for output [min]
  INTEGER           :: grstep     ! calculated from INTVL (HHMMSS)

  CHARACTER(LEN=16) :: coordnam   ! user input: Coordinate name
  CHARACTER(LEN=16) :: grdnam     ! user input: Grid name 

!-------------------------------------------------------------------------------
! Coordinates for diagnostic prints.
!-------------------------------------------------------------------------------

  INTEGER           :: lprt_metx  ! X-coordinate in MET domain
  INTEGER           :: lprt_mety  ! Y-coordinate in MET domain
  INTEGER           :: lprt_col   ! COL-coordinate in output domain
  INTEGER           :: lprt_row   ! ROW-coordinate in output domain
  INTEGER           :: lprt_xcol  ! COL-coordinate in X domain
  INTEGER           :: lprt_xrow  ! ROW-coordinate in X domain

!-------------------------------------------------------------------------------
! Program and version descriptors.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=80)                 :: fdesc      ( mxdesc3 )
  CHARACTER(LEN=16),  PARAMETER     :: progname   = 'MCIP'
  CHARACTER(LEN=10),  PARAMETER     :: vdate      = '06/23/2017'
  CHARACTER(LEN=8),   PARAMETER     :: ver        = 'V4.4'

END MODULE mcipparm
