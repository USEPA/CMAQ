
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

SUBROUTINE init_depv

!-------------------------------------------------------------------------------
! Name:     Initialize Dry Deposition Arrays
! Purpose:  Initializes dry deposition arrays.
! Revised:  11 Aug 2005  Initial version.  (T. Otte and W. Hutzell)
!           24 Jul 2007  Removed RADMdry arrays.  Changed code so that M3Dry
!                        with chlorine and mercury is the only option to
!                        compute dry deposition velocities in MCIP.  Removed
!                        dry deposition velocity calculations for obsolete
!                        chlorine species ICL1 and ICL2.  Added molecular
!                        weights for species.  (T. Otte)
!           24 Apr 2008  Added five new air toxic species.  (W. Hutzell and
!                        T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE depvvars

  IMPLICIT NONE
 
!-------------------------------------------------------------------------------
! For M3DRY (LDDEP=4).
!-------------------------------------------------------------------------------

  xdepspc( 1) = 'SO2'           ;  molwt( 1) =  64.0
  xdepspc( 2) = 'SULF'          ;  molwt( 2) =  96.0  ! SO4
  xdepspc( 3) = 'NO2'           ;  molwt( 3) =  46.0
  xdepspc( 4) = 'NO'            ;  molwt( 4) =  30.0
  xdepspc( 5) = 'O3'            ;  molwt( 5) =  48.0
  xdepspc( 6) = 'HNO3'          ;  molwt( 6) =  63.0
  xdepspc( 7) = 'H2O2'          ;  molwt( 7) =  34.0
  xdepspc( 8) = 'ALD'           ;  molwt( 8) =  44.0  ! CH3CHO, acetaldehyde
  xdepspc( 9) = 'HCHO'          ;  molwt( 9) =  30.0
  xdepspc(10) = 'OP'            ;  molwt(10) =  48.0  ! CH3OOH, methylhydroperoxide
  xdepspc(11) = 'PAA'           ;  molwt(11) =  76.0  ! CH3COOOH, peroxyacetic acid
  xdepspc(12) = 'ORA'           ;  molwt(12) =  60.0  ! CH3COOH, acetic acid
  xdepspc(13) = 'NH3'           ;  molwt(13) =  17.0
  xdepspc(14) = 'PAN'           ;  molwt(14) = 121.0  ! CH3COOONO2
  xdepspc(15) = 'HONO'          ;  molwt(15) =  47.0
  xdepspc(16) = 'CO'            ;  molwt(16) =  28.0
  xdepspc(17) = 'METHANOL'      ;  molwt(17) =  32.0  ! CH3OH
  xdepspc(18) = 'N2O5'          ;  molwt(18) = 108.0
  xdepspc(19) = 'NO3'           ;  molwt(19) =  62.0
  xdepspc(20) = 'GEN_ALD'       ;  molwt(20) =  58.0  ! CH3CH2COH (assume >ALD) @
  xdepspc(21) = 'CL2'           ;  molwt(21) =  70.0
  xdepspc(22) = 'HOCL'          ;  molwt(22) =  52.0
  xdepspc(23) = 'HCL'           ;  molwt(23) =  36.0
  xdepspc(24) = 'FMCL'          ;  molwt(24) =  64.0  ! ClCHO, formylchloride #
  xdepspc(25) = 'HG'            ;  molwt(25) = 200.59
  xdepspc(26) = 'HGIIGAS'       ;  molwt(26) = 270.59 ! Assume all HgCl2 $
  xdepspc(27) = 'HEXMETH_DIIS'  ;  molwt(27) = 168.2  ! hexamethylene diisocyanate ^
  xdepspc(28) = 'HYDRAZINE'     ;  molwt(28) =  32.0  ! hydrazine ^
  xdepspc(29) = 'MAL_ANHYDRIDE' ;  molwt(29) =  98.0  ! maleic anhydride ^
  xdepspc(30) = 'TOLUENE_DIIS'  ;  molwt(30) = 174.2  ! toluene diisocyanate ^
  xdepspc(31) = 'TRIETHYLAMINE' ;  molwt(31) = 101.2  ! triethylamine ^


  ! Footnotes:
  !   @  Based on discussion with D. Luecken (07/07)
  !   #  Based on discussion with G. Sarwar (07/07)
  !   $  Based on discussion with O. R. Bullock (07/07)
  !   ^  Air toxic species added by W. Hutzell (04/08)

END SUBROUTINE init_depv
