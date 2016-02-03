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

SUBROUTINE getgist (hdr_int, hdr_real, hdr_int_desc, hdr_real_desc)

!-------------------------------------------------------------------------------
! Name:     Get Gist of MM5 Header
! Purpose:  Read MM5v2 or MM5v3 header information.
! Notes:    - Based on similar NCAR routine for MM5v2.
!           - For MM5v2, all four input arrays will be dimensioned (1000,20).
!           - For MM5v3, integer array and its description are (50,20) and
!             real array and its description are (20,20).
! Revised:  14 Sep 2001  Original version.  (T. Otte)
!           07 Jan 2002  Added OPEN and CLOSE statements for output file.
!                        (S. Howard and T. Otte)
!           09 Jan 2002  Changed "stop" statements to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           29 Aug 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Improved error
!                        handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE files

  IMPLICIT NONE

  INTEGER,            INTENT(IN)    :: hdr_int       ( : , : )
  CHARACTER(LEN=80),  INTENT(IN)    :: hdr_int_desc  ( : , : )
  REAL,               INTENT(IN)    :: hdr_real      ( : , : )
  CHARACTER(LEN=80),  INTENT(IN)    :: hdr_real_desc ( : , : )
  INTEGER                           :: i
  INTEGER                           :: j
  CHARACTER(LEN=3)                  :: name_hdr_int
  CHARACTER(LEN=3)                  :: name_hdr_real
  CHARACTER(LEN=16),  PARAMETER     :: pname     = 'GETGIST'

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f100 = &
    & "(a, '(', i3, ',', i2, ') = ', i10,   ' : ', a)"

  CHARACTER(LEN=256), PARAMETER :: f110 = &
    & "(a, '(', i3, ',', i2, ') = ', f10.2, ' : ', a)"

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ARRAY SIZE MISMATCH', &
    & /, 1x, '***   SIZES OF HDR_INT, HDR_INT_DESC = ', 2(2x, i4), &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ARRAY SIZE MISMATCH', &
    & /, 1x, '***   SIZES OF HDR_REAL, HDR_REAL_DESC = ', 2(2x, i4), &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ARRAY SIZE MISMATCH', &
    & /, 1x, '***   2nd DIM OF HDR_INT, HDR_REAL = ', 2(2x, i4), &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Since input arrays are assumed-shape, do some simple QC checks on
! array sizes.
!-------------------------------------------------------------------------------

  IF ( SIZE(hdr_int) /= SIZE(hdr_int_desc) ) THEN
    WRITE (*,f9000) TRIM(pname), SIZE(hdr_int), SIZE(hdr_int_desc)
    CALL graceful_stop (pname)
  ENDIF

  IF ( SIZE(hdr_real) /= SIZE(hdr_real_desc) ) THEN
    WRITE (*,f9100) TRIM(pname), SIZE(hdr_real), SIZE(hdr_real_desc)
    CALL graceful_stop (pname)
  ENDIF

  IF ( SIZE(hdr_int, 2) /= SIZE(hdr_real, 2) ) THEN
    WRITE (*,f9200) TRIM(pname), SIZE(hdr_int,2), SIZE(hdr_real,2)
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Based on sizes of header arrays, determine if output is MM5v2 or MM5v3.
! Assign appropriate names to the output arrays.
!-------------------------------------------------------------------------------

  IF ( SIZE(hdr_int, 1) == SIZE(hdr_real, 1) ) THEN  ! assume v1/v2
    name_hdr_int  = 'MIF'
    name_hdr_real = 'MRF'
  ELSE  ! assume v3
    name_hdr_int  = 'BHI'
    name_hdr_real = 'BHR'
  ENDIF

!-------------------------------------------------------------------------------
! Loop over the number of programs in this MM5 version's header.  Within
! that loop, loop over integer items then real items in this version of
! MM5, and print out the header variables that are "defined".
!-------------------------------------------------------------------------------

  OPEN (iuthdr, FILE=file_hdr)

  DO j = 1, SIZE(hdr_int, 2)   ! hdr_int & hdr_real dim 2 are same in v2 & v3

    DO i = 1, SIZE(hdr_int, 1)
      IF ( hdr_int(i,j) /= -999 )  &
        WRITE (iuthdr,f100) name_hdr_int, i, j, hdr_int(i,j), hdr_int_desc(i,j)
    ENDDO

    DO i = 1, SIZE(hdr_real, 1)
      IF ( NINT(hdr_real(i,j)) /= -999 )  &
        WRITE (iuthdr,f110) name_hdr_real, i, j, hdr_real(i,j), hdr_real_desc(i,j)
    ENDDO

  ENDDO

  CLOSE (iuthdr)

END SUBROUTINE getgist
