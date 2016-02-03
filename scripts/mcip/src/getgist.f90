
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
! $Header: /project/work/rep/MCIP2/src/mcip2/getgist.F,v 1.1.1.1 2002/03/09 14:48:00 yoj Exp $ 


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
!-------------------------------------------------------------------------------

  USE file

  IMPLICIT NONE

  INTEGER,       INTENT(IN)    :: hdr_int       ( : , : )
  CHARACTER*80,  INTENT(IN)    :: hdr_int_desc  ( : , : )
  REAL,          INTENT(IN)    :: hdr_real      ( : , : )
  CHARACTER*80,  INTENT(IN)    :: hdr_real_desc ( : , : )
  INTEGER                      :: i
  INTEGER                      :: j
  CHARACTER*3                  :: name_hdr_int
  CHARACTER*3                  :: name_hdr_real
  CHARACTER*16,  PARAMETER     :: pname     = 'GETGIST'

!-------------------------------------------------------------------------------
! Since input arrays are assumed-shape, do some simple QC checks on
! array sizes.
!-------------------------------------------------------------------------------

  IF ( SIZE(hdr_int) /= SIZE(hdr_int_desc) ) THEN
    WRITE (6,9000) SIZE(hdr_int), SIZE(hdr_int_desc)
    GOTO 1001
  ENDIF

  IF ( SIZE(hdr_real) /= SIZE(hdr_real_desc) ) THEN
    WRITE (6,9100) SIZE(hdr_real), SIZE(hdr_real_desc)
    GOTO 1001
  ENDIF

  IF ( SIZE(hdr_int, 2) /= SIZE(hdr_real, 2) ) THEN
    WRITE (6,9200) SIZE(hdr_int,2), SIZE(hdr_real,2)
    GOTO 1001
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
        WRITE (iuthdr,100) name_hdr_int, i, j, hdr_int(i,j), hdr_int_desc(i,j)
    ENDDO

    DO i = 1, SIZE(hdr_real, 1)
      IF ( NINT(hdr_real(i,j)) /= -999 )  &
        WRITE (iuthdr,110) name_hdr_real, i, j, hdr_real(i,j), hdr_real_desc(i,j)
    ENDDO

  ENDDO

  CLOSE (iuthdr)

  RETURN

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

  100 FORMAT ( a, '(', i3, ',', i2, ') = ', i10,   ' : ', a)
  110 FORMAT ( a, '(', i3, ',', i2, ') = ', f10.2, ' : ', a)

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                                &
              /, 1x, '*** SUBROUTINE: GETGIST',                              &
              /, 1x, '***   ARRAY SIZE MISMATCH',                            &
              /, 1x, '***   SIZES OF HDR_INT, HDR_INT_DESC = ', 2(2x, i4),   &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                                &
              /, 1x, '*** SUBROUTINE: GETGIST',                              &
              /, 1x, '***   ARRAY SIZE MISMATCH',                            &
              /, 1x, '***   SIZES OF HDR_REAL, HDR_REAL_DESC = ', 2(2x, i4), &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                                &
              /, 1x, '*** SUBROUTINE: GETGIST',                              &
              /, 1x, '***   ARRAY SIZE MISMATCH',                            &
              /, 1x, '***   2nd DIM OF HDR_INT, HDR_REAL = ', 2(2x, i4),     &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE getgist
