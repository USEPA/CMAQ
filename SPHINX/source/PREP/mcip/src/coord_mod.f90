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

MODULE coord

!-------------------------------------------------------------------------------
! Name:     Coordinate and Domain Descriptions
! Purpose:  Contains coordinate and domain descriptions.
! Revised:  28 Jan 1998  Original version.  (D. Byun)
!           28 Jan 1998  Removed commented out old data.  (J. Young)
!           10 Sep 2001  Converted to free-form f90.  (T. Otte)
!           03 Oct 2001  Added variable COORDNAM_GD.  Changed declarations
!                        for real variables from REAL(8) to DOUBLE PRECISION.
!                        (T. Otte)
!           16 Aug 2005  Replaced DOUBLE PRECISION with REAL(8).  (T. Otte)
!           09 Apr 2007  Added IMPLICIT NONE.  (T. Otte)
!           30 Aug 2011  Changed F77 character declarations to F90 standard.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! The definitions and declarations in this include file follow those
! given in the I/O-API include file FDESC3C.EXT and replace similar
! definitions.  This include file is compatible with FDESC3C.EXT.
!
! All variable names end in string "_GD", which is a grid identifier for
! multiple grid applications. "_GD" should be "_G1" for the first grid,
! "_G2" for the second grid, etc.
!
! The horizontal grid definition information is REAL*8 in order 
! to achieve the required precision in geographic-to/from-grid
! coordinate conversions.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! GDTYP_GD:
! The map projection type:
!   1: LATGRD for lat-lon coordinates (unused)
!   2: LAMGRD for Lambert coordinates
!   3: MERGRD for Mercator coordinates
!   4: STEGRD for Stereographic coordinates
!   5: UTMGRD for UTM coordinates
!-------------------------------------------------------------------------------

  INTEGER                      :: gdtyp_gd

!-------------------------------------------------------------------------------
! The definitions of the map projection specification parameters:
!   P_ALP_GD  (PROJ_ALPHA),
!   P_BET_GD  (PROJ_BETA),
!   P_GAM_GD  (PROJ_GAMMA),
! depend upon the projection type, as follows:
! (Note: if P_ALP_GD < AMISS (=-9E36, from PARMS3.EXT), then the grid
!  description is missing or invalid.)
! 
! Lambert        P_ALP_GD <= P_BET_GD are the two latitudes that
!                determine the projection cone; P_GAM_GD is the
!                central meridian.
! 
! Mercator       P_ALP_GD and P_BET_GD are the latitude and longitude
!                of the coordinate origin (within the tangent circle);
!                P_GAM_GD is the angle between the cylinder axis
!                and the North polar axis.
! 
! Stereographic  P_ALP_GD and P_BET_GD are the latitude and longitude
!                of the point of tangency; P_GAM_GD is the angle from
!                true North to the Y-axis.
!    
! UTM:           P_ALP_GD is the UTM zone.
!                P_BET_GD and P_GAM_GD are unused.
!    
! lat-lon:       Currently not used.  Coordinate units are degrees, with
!                -180.0 < X <= 180.0,  -90.0 <= Y <= 90.0   
!                Western longitudes and southern latitudes are negative.
!-------------------------------------------------------------------------------

  REAL(8)                      :: p_alp_gd   ! degrees
  REAL(8)                      :: p_bet_gd   ! degrees
  REAL(8)                      :: p_gam_gd   ! degrees

!-------------------------------------------------------------------------------
! (XCENT_GD, YCENT_GD):
! For Lambert, Mercator, and Stereographic, these are the 
!     longitude, -180 < X <= 180, and the latitude, -90 <= Y <= 90, 
!     for the center of the grid's respective Cartesian coordinate system.
! For UTM:  ?
! For Lat-Lon:  unused
!-------------------------------------------------------------------------------
     
  REAL(8)                      :: xcent_gd   ! degrees longitude
  REAL(8)                      :: ycent_gd   ! degrees latitude

!-------------------------------------------------------------------------------
! (XORIG_GD, YORIG_GD):
! For Lambert, Mercator, Stereographic, and UTM these are the
!     location in map units (Km) of the origin cell (1,1) (lower left corner)
!     of the of the horizontal grid measured from (XCENT_GD, YCENT_GD).
! For Lat-Lon: units are degrees - unused
!-------------------------------------------------------------------------------
     
  REAL(8)                      :: xorig_gd   ! X-origin [m]
  REAL(8)                      :: yorig_gd   ! Y-origin [m]

!-------------------------------------------------------------------------------
! (XCELL_GD, YCELL_GD):
! The X-direction and Y-direction cell dimensions (m) for a regular grid
! If zero, the grid is assumed irregular and described by other means (e.g.
! a grid-geometry file).
!-------------------------------------------------------------------------------
     
  REAL(8)                      :: xcell_gd   ! X-cell dimension [m]
  REAL(8)                      :: ycell_gd   ! Y-cell dimension [m]

!-------------------------------------------------------------------------------
! VGTYP_GD:
! The vertical grid type:
!   1: VGSIGP for sigma-P coordinates
!   2: VGSGP0 for sigma-P0 coordinates
!   3: VGSIGZ for sigma-Z coordinates
!   4: VGETAP for eta-P coordinates
!   5: VGPRES for pressure coordinates
!   6: VGZVAL for Z (meters above ground)
!   7: VHZVAL for H (meters above mean sea level)
!   8: IMISS  for vertical coordinates not stored in VGLVSD
!             (e.g., temporally or spatially changing vertical coordinates)
!-------------------------------------------------------------------------------
     
  INTEGER                      :: vgtyp_gd

!-------------------------------------------------------------------------------
! VGTPUN_GD:
! The units of the vertical coordinate top.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=16)            :: vgtpun_gd

!-------------------------------------------------------------------------------
! VGTOP_GD:
! The value for the model top used in the definition of the sigma
! coordinate systems in the VGTPUN_GD units
! For sigma-P, the relationship between pressure levels P and sigma-P is
! given by the following formula:
!    sigma-P = ( P - VGTOP_GD ) / (P_srf - VGTOP_GD ),
! where P_srf is the surface pressure.
!-------------------------------------------------------------------------------

  REAL                         :: vgtop_gd

!-------------------------------------------------------------------------------
! VGLVUN_GD:
! The units of the vertical coordinate surface values
!-------------------------------------------------------------------------------

  CHARACTER(LEN=16)            :: vglvun_gd

!-------------------------------------------------------------------------------
! VGLVS_GD( 1...NLAYS+1 ):
! The list of vertical coordinate surface values in the VGLVUN_GD units
! Layer k extends from VGLVS3D( k ) to VGLVS3D( k+1 ).
!-------------------------------------------------------------------------------

  REAL,          ALLOCATABLE   :: vglvs_gd   ( : )

!-------------------------------------------------------------------------------
! X3FACE_GD( 0: NLAYS ):
! The list of vertical coordinate surface values in the VGLVUN_GD units 
! coverted to values monotonically increasing with altitude.  ( 1 - VGLVS_GD )
!-------------------------------------------------------------------------------

  REAL,          ALLOCATABLE   :: x3face_gd  ( : )

!-------------------------------------------------------------------------------
! COORDNAM_GD:
! The coordinate system name used for I/O-API description and GRIDDESC.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=16)            :: coordnam_gd

!-------------------------------------------------------------------------------
! GDNAME_GD:
! The grid name used for I/O-API description and GRIDDESC.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=16)            :: gdname_gd

END MODULE coord
