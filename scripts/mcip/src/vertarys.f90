
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

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/work/rep/MCIP2/src/mcip2/vertarys.F,v 1.2 2007/08/03 20:48:38 tlotte Exp $ 


SUBROUTINE vertarys (ctmlays)

!-------------------------------------------------------------------------------
! Name:     Vertical Arrays
! Purpose:  Define vertical structure arrays from input.
! Revised:  20 Sep 2001  Original version.  (T. Otte)
!           09 Apr 2007  Added IMPLICIT NONE.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE coord
  USE vgrd

  IMPLICIT NONE

  REAL,          INTENT(IN)    :: ctmlays      ( maxlays )
  CHARACTER*60                 :: ifmt1
  INTEGER                      :: k
  INTEGER                      :: k1
  INTEGER                      :: k2
  INTEGER                      :: lbnd
  INTEGER                      :: lbnd2
  INTEGER                      :: nfull
  CHARACTER*2                  :: str1
  CHARACTER*2                  :: str2

!-------------------------------------------------------------------------------
! VGLVS_GD( 1...NLAYS+1 ):
! The list of vertical coordinate surface values in the VGLVUN_GD units
! Layer k extends from VGLVS3D( k ) to VGLVS3D( k+1 ).
!-------------------------------------------------------------------------------

  vglvs_gd(1:nlays+1) = ctmlays(1:nlays+1)

!-------------------------------------------------------------------------------
! X3FACE_GD( 0: NLAYS ):
! The list of vertical coordinate surface values in the VGLVUN_GD units 
! coverted to values monotonically increasing with altitude.  ( 1 - VGLVS_GD )
!-------------------------------------------------------------------------------

  lbnd = LBOUND(x3face_gd,1)

  DO k = 0, nlays
    x3face_gd(lbnd+k) = 1.0 - vglvs_gd(k+1)
  ENDDO

!-------------------------------------------------------------------------------
! Echo user-specified grid description info to log file.
!-------------------------------------------------------------------------------

  nfull = nlays + 1

  k1 = nfull / 6
  k2 = MOD(nfull, 6)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,4x,a,6(2x,f7.5),/," // str1 // "(12x,6(2x,f7.5),/),12x,"   &
         &    // str2 // "(2x,f7.5),/)"
    ELSE
      ifmt1 = "(/,4x,a,6(2x,f7.5),/," // str1 // "(12x,6(2x,f7.5),/))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,4x,a,6(2x,f7.5),/,12x," // str2 // "(2x,f7.5),/)"
    ELSE
      ifmt1 = "(/,4x,a,6(2x,f7.5),/)"
    ENDIF
  ENDIF

  WRITE (*,ifmt1) 'VGLVS3D ', vglvs_gd

  WRITE (*, "(1x, 78('-'), /)")

!-------------------------------------------------------------------------------
! Layer definition for CTM coordinate (monotonic increase with height).
! From X3FACE values, compute X3MIDL (layer middle coord. definition)
! X3MIDL is defined in VGRD.
!-------------------------------------------------------------------------------

  lbnd2 = LBOUND(x3face,1)

  x3face(lbnd2:lbnd2+nlays) = x3face_gd(lbnd:lbnd+nlays)

  DO k = 1, nlays
    x3midl(k) = 0.5 * ( x3face(k-1) + x3face(k) )
  ENDDO

END SUBROUTINE vertarys
