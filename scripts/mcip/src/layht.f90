
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
! $Header: /project/work/rep/MCIP2/src/mcip2/layht.F,v 1.2 2006/09/27 13:59:24 tlotte Exp $ 


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
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER                      :: i
  INTEGER                      :: imax
  INTEGER                      :: j
  INTEGER                      :: jmax
  INTEGER                      :: k
  INTEGER                      :: lbndf
  INTEGER                      :: lbndm
  CHARACTER*16,  PARAMETER     :: pname      = 'LAYHT'
  INTEGER                      :: ubndf
  INTEGER                      :: ubndm
  REAL,          INTENT(IN)    :: xx3face    ( : )
  REAL,          INTENT(OUT)   :: xx3htf     ( : , : , : )
  REAL,          INTENT(OUT)   :: xx3htm     ( : , : , : )
  REAL,          INTENT(IN)    :: xx3jcbf    ( : , : , : )
  REAL,          INTENT(IN)    :: xx3jcbm    ( : , : , : )
  REAL,          INTENT(IN)    :: xx3midl    ( : )

!-------------------------------------------------------------------------------
! Since input arrays are assumed-shape, do simple QC on array sizes and
! extract dimension information.
!-------------------------------------------------------------------------------

  ! Check IMAX.

  imax = SIZE(xx3htf,1)
  IF ( ( SIZE(xx3htm,1)  /= imax ) .OR. ( SIZE(xx3jcbf,1) /= imax ) .OR.  &
       ( SIZE(xx3jcbm,1) /= imax ) ) THEN
    WRITE (6,9000) imax, SIZE(xx3htm,1), SIZE(xx3jcbf,1), SIZE(xx3jcbm,1)
    GOTO 1001
  ENDIF

  ! Check JMAX.

  jmax = SIZE(xx3htf,2)
  IF ( ( SIZE(xx3htm,2)  /= jmax ) .OR. ( SIZE(xx3jcbf,2) /= jmax ) .OR.  &
       ( SIZE(xx3jcbm,2) /= jmax ) ) THEN
    WRITE (6,9100) jmax, SIZE(xx3htm,2), SIZE(xx3jcbf,2), SIZE(xx3jcbm,2)
    GOTO 1001
  ENDIF

  ! Check vertical dimensions.

  IF ( ( SIZE(xx3midl,1) /= SIZE(xx3htm,3)  ) .OR.  &
       ( SIZE(xx3midl,1) /= SIZE(xx3jcbm,3) ) ) THEN
    WRITE (6,9200) SIZE(xx3midl,1), SIZE(xx3htm,3), SIZE(xx3jcbm,3)
    GOTO 1001
  ELSE
    lbndm = LBOUND(xx3midl,1)
    ubndm = UBOUND(xx3midl,1)
  ENDIF

  IF ( ( SIZE(xx3face,1) /= SIZE(xx3htf,3)  ) .OR.  &
       ( SIZE(xx3face,1) /= SIZE(xx3jcbf,3) ) ) THEN
    WRITE (6,9300) SIZE(xx3face,1), SIZE(xx3htf,3), SIZE(xx3jcbf,3)
    GOTO 1001
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

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: LAYHT',                              &
              /, 1x, '***   PROBLEM WITH IMAX',                            &
              /, 1x, '***   SIZES OF XX3HTF, XX3HTM = ', 2(2x, i4),        &
              /, 1x, '***   SIZES OF XX3JCBF, XX3JCBM = ', 2(2x, i4),      &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: LAYHT',                              &
              /, 1x, '***   PROBLEM WITH JMAX',                            &
              /, 1x, '***   SIZES OF XX3HTF, XX3HTM = ', 2(2x, i4),        &
              /, 1x, '***   SIZES OF XX3JCBF, XX3JCBM = ', 2(2x, i4),      &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: LAYHT',                              &
              /, 1x, '***   PROBLEM WITH MIDDLE LEVEL DIMENSION',          &
              /, 1x, '***   SIZE OF XX3MIDL = ', 2x, i4,                   &
              /, 1x, '***   SIZES OF XX3HTM, XX3JCBM = ', 2(2x, i4),       &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: LAYHT',                              &
              /, 1x, '***   PROBLEM WITH FACE LEVEL DIMENSION',            &
              /, 1x, '***   SIZE OF XX3FACE = ', 2x, i4,                   &
              /, 1x, '***   SIZES OF XX3HTF, XX3JCBF = ', 2(2x, i4),       &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE layht
