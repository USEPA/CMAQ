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

SUBROUTINE layht (xx3face, xx3midl, xx3jcbf, xx3jcbm, xx3htf, xx3htm)

!-------------------------------------------------------------------------------
! Name:     Layer Heights
! Purpose:  Compute height of vertical coordinates for given Jacobian.
!              XX3HT =  XX3JACOB * ( X3 (K+1) - X3 (K) )
!
! Input:   IMAX,JMAX:  total domain size in x (E-W) & y (N-S) direction
!          KMAX     :  number of layers (actually used)
!          XX3FACE  :  vertical coordinate values of layer interface
!          XX3MIDL  :  vertical coordinate values of layer middle
!          XX3JCBF  :  vertical Jacobian at layer face in meter
!          XX3JCBM  :  vertical Jacobian at layer middle in meter
!
! Output:  XX3HTF   :  height of layer interface in meter
!          XX3HTM   :  height of layer middle in meter
!     
! Revised:  05 Feb 1997  Created for MCIP and generalized CTM.  (D. Byun)
!           18 Sep 2001  Converted to free-form f90.  (T. Otte)
!           09 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           19 Jun 2006  Removed dependency on module CONST.  (T. Otte)
!           29 Aug 2011  Improved error handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER                           :: i
  INTEGER                           :: imax
  INTEGER                           :: j
  INTEGER                           :: jmax
  INTEGER                           :: k
  INTEGER                           :: lbndf
  INTEGER                           :: lbndm
  CHARACTER(LEN=16),  PARAMETER     :: pname      = 'LAYHT'
  INTEGER                           :: ubndf
  INTEGER                           :: ubndm
  REAL,               INTENT(IN)    :: xx3face    ( : )
  REAL,               INTENT(OUT)   :: xx3htf     ( : , : , : )
  REAL,               INTENT(OUT)   :: xx3htm     ( : , : , : )
  REAL,               INTENT(IN)    :: xx3jcbf    ( : , : , : )
  REAL,               INTENT(IN)    :: xx3jcbm    ( : , : , : )
  REAL,               INTENT(IN)    :: xx3midl    ( : )

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   PROBLEM WITH IMAX', &
    & /, 1x, '***   SIZES OF XX3HTF, XX3HTM = ', 2(2x, i4), &
    & /, 1x, '***   SIZES OF XX3JCBF, XX3JCBM = ', 2(2x, i4), &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   PROBLEM WITH JMAX', &
    & /, 1x, '***   SIZES OF XX3HTF, XX3HTM = ', 2(2x, i4), &
    & /, 1x, '***   SIZES OF XX3JCBF, XX3JCBM = ', 2(2x, i4), &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   PROBLEM WITH MIDDLE LEVEL DIMENSION', &
    & /, 1x, '***   SIZE OF XX3MIDL = ', 2x, i4, &
    & /, 1x, '***   SIZES OF XX3HTM, XX3JCBM = ', 2(2x, i4), &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   PROBLEM WITH FACE LEVEL DIMENSION', &
    & /, 1x, '***   SIZE OF XX3FACE = ', 2x, i4, &
    & /, 1x, '***   SIZES OF XX3HTF, XX3JCBF = ', 2(2x, i4), &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Since input arrays are assumed-shape, do simple QC on array sizes and
! extract dimension information.
!-------------------------------------------------------------------------------

  ! Check IMAX.

  imax = SIZE(xx3htf,1)
  IF ( ( SIZE(xx3htm,1)  /= imax ) .OR. ( SIZE(xx3jcbf,1) /= imax ) .OR.  &
       ( SIZE(xx3jcbm,1) /= imax ) ) THEN
    WRITE (*,f9000) TRIM(pname), imax, SIZE(xx3htm,1), SIZE(xx3jcbf,1),  &
                    SIZE(xx3jcbm,1)
    CALL graceful_stop (pname)
  ENDIF

  ! Check JMAX.

  jmax = SIZE(xx3htf,2)
  IF ( ( SIZE(xx3htm,2)  /= jmax ) .OR. ( SIZE(xx3jcbf,2) /= jmax ) .OR.  &
       ( SIZE(xx3jcbm,2) /= jmax ) ) THEN
    WRITE (*,f9100) TRIM(pname), jmax, SIZE(xx3htm,2), SIZE(xx3jcbf,2),  &
                    SIZE(xx3jcbm,2)
    CALL graceful_stop (pname)
  ENDIF

  ! Check vertical dimensions.

  IF ( ( SIZE(xx3midl,1) /= SIZE(xx3htm,3)  ) .OR.  &
       ( SIZE(xx3midl,1) /= SIZE(xx3jcbm,3) ) ) THEN
    WRITE (*,f9200) TRIM(pname), SIZE(xx3midl,1), SIZE(xx3htm,3),  &
                    SIZE(xx3jcbm,3)
    CALL graceful_stop (pname)
  ELSE
    lbndm = LBOUND(xx3midl,1)
    ubndm = UBOUND(xx3midl,1)
  ENDIF

  IF ( ( SIZE(xx3face,1) /= SIZE(xx3htf,3)  ) .OR.  &
       ( SIZE(xx3face,1) /= SIZE(xx3jcbf,3) ) ) THEN
    WRITE (*,f9300) TRIM(pname), SIZE(xx3face,1), SIZE(xx3htf,3),  &
                    SIZE(xx3jcbf,3)
    CALL graceful_stop (pname)
  ELSE
    lbndf = LBOUND(xx3face,1)
    ubndf = UBOUND(xx3face,1)
  ENDIF

!-------------------------------------------------------------------------------
! Build layer height fields using Jacobian.
!-------------------------------------------------------------------------------

  DO i = 1, imax
    DO j = 1, jmax

      ! Full levels

      xx3htf(i,j,lbndf) = 0.0        

      DO k = lbndf+1, ubndf
        xx3htf(i,j,k) = xx3htf(i,j,k-1) + ( xx3face(k) - xx3face(k-1) ) *  &
                                            xx3jcbm(i,j,lbndm+k-2)
      ENDDO                 

      ! Half levels

      xx3htm(i,j,lbndm) = ( xx3midl(lbndm) - xx3face(lbndf) )  &
                        * 0.5 * ( xx3jcbf(i,j,lbndf) + xx3jcbm(i,j,lbndm) )

      DO k = lbndm+1, ubndm
        xx3htm(i,j,k) = xx3htm(i,j,k-1) + ( xx3midl(k) - xx3midl(k-1) ) *  &
                                            xx3jcbf(i,j,lbndf+k-1)
      ENDDO

    ENDDO
  ENDDO   

END SUBROUTINE layht
