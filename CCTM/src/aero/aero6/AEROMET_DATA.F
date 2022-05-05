
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
C $Header: /project/yoj/arc/CCTM/src/aero/aero5/AEROMET_DATA.F,v 1.6 2012/01/19 13:10:03 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module aeromet_data

C Some parameters and meteorological variables required in aerosol
C processing.

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C
C SH  03/10/11 renamed file and module from MET_DATA to AEROMET_DATA
C----------------------------------------------------------------------

      Implicit None

C Fundamental constants
      Include SUBST_CONST    ! common constants

      Real( 8 ), Parameter :: f6dpi = 6.0D0 / dpi
      Real,      Parameter :: f6pi = 6.0 / pi
      Real( 8 ), Parameter :: dpi6 = dpi / 6.0D0
      Real,      Parameter :: pi6 = pi / 6.0
      Real( 8 ), Parameter :: boltzmann = drgasuniv / davo  ! [ J/K ]

C Concentration lower limit used for gas species
      Real,      Parameter :: min_gasconc = 1.0E-30        ! [ ug/m^3 ]
      Real,      Parameter :: inv_mwair   = 1.0E3 / mwair  ! [ 1/g ]

C Meteorological variables 
      Real :: airtemp      ! air temp [ K ] :from MET_CRO_3D File
      Real :: srftemp      ! 2-meter temp [ K ] :from MET_CRO_2D File
      Real :: airpres      ! air pressure [ Pa]
      Real :: airdens      ! air density [ kg/m^3]
      Real :: airRH        ! fractional relative humidity
      Real :: airQV        ! water vapor mass mixing ratio [ kg_H2O/kg_air ]
      Real :: H2OsatVP     ! saturation water vapor pressure [ Pa ]
      Real :: H2OVP        ! ambient water vapor pressure [ Pa ]
      Real :: cfrac        ! cloud fraction
      Real :: dz           ! grid cell height between layer faces
      Real :: zh           ! grid cell midpoint height (mass-weighted)
      Real :: pv           ! potential vorticity in grid cell

      End Module aeromet_data
