
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

SUBROUTINE readter

!-------------------------------------------------------------------------------
! Name:     Read MM5 Version 3 Terrain Output File
! Purpose:  Reads MM5 Version 3 Terrain output file to get fractional land use.
! Revised:  29 Nov 2004  Original version.  (T. Otte)
!           07 Jan 2005  Changed I and J variables to X and Y to make code
!                        more general.  (T. Otte)
!           11 Jul 2005  Clarified read statement so that false category 25 in
!                        USGS is not used.  (T. Otte)
!           19 Aug 2005  Eliminated use of F90 intrinsic function TRANSPOSE to
!                        avoid bounds checking issues on Linux Intel compilers.
!                        Corrected declaration of START_INDEX from CHARACTER*4
!                        to INTEGER.  Changed internal variable TIME to TIMEOUT
!                        to avoid confusion with F90 intrinsic function.
!                        Removed inadvertent TAB in format.  (T. Otte)
!           13 Apr 2006  Corrected logic to account for MM5 expanded grid in
!                        domain 1 TERRAIN file.  (T. Otte)
!           12 Feb 2010  Removed unused variable TERFILE.  (T. Otte)
!-------------------------------------------------------------------------------

  USE file
  USE metvars
  USE mcipparm

  IMPLICIT NONE

  INTEGER,       ALLOCATABLE   :: bhi       ( : , : )
  CHARACTER*80,  ALLOCATABLE   :: bhic      ( : , : )
  REAL,          ALLOCATABLE   :: bhr       ( : , : )
  CHARACTER*80,  ALLOCATABLE   :: bhrc      ( : , : )
  CHARACTER*24                 :: currentdate
  REAL,          ALLOCATABLE   :: data      ( : , : , : , : )
  CHARACTER*46                 :: description
  INTEGER                      :: end_index ( 4 )
  INTEGER                      :: expandi
  INTEGER                      :: expandj
  LOGICAL,       ALLOCATABLE   :: gotlu     ( : )
  INTEGER                      :: i
  INTEGER                      :: ii
  INTEGER                      :: icat
  INTEGER                      :: iflag
  INTEGER                      :: istat
  INTEGER                      :: j
  INTEGER                      :: jj
  CHARACTER*9                  :: name
  INTEGER                      :: ndim
  INTEGER,       PARAMETER     :: numprogs  = 20
  INTEGER,       PARAMETER     :: numvalsi  = 50
  INTEGER,       PARAMETER     :: numvalsr  = 20
  INTEGER                      :: offseti
  INTEGER                      :: offsetj
  CHARACTER*4                  :: ordering
  CHARACTER*16,  PARAMETER     :: pname     = 'READTER'
  CHARACTER*4                  :: staggering
  INTEGER                      :: start_index ( 4 )
  REAL                         :: timeout
  CHARACTER*25                 :: units
  CHARACTER*9                  :: var

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

  ALLOCATE ( bhi   (numvalsi, numprogs) )
  ALLOCATE ( bhic  (numvalsi, numprogs) )
  ALLOCATE ( bhr   (numvalsr, numprogs) )
  ALLOCATE ( bhrc  (numvalsr, numprogs) )

  ALLOCATE ( gotlu (nummetlu) )

!-------------------------------------------------------------------------------
! Loop through the TERRAIN output file.  Extract fractional land use.
!-------------------------------------------------------------------------------

  gotlu(:) = .FALSE.

  v3data: DO

    var = 'IFLAG    '
    READ (iutter, IOSTAT=istat, ERR=8000, END=999) iflag
  
    IF ( iflag == 0 ) THEN
      var = 'BIG HEADR'
      READ (iutter, IOSTAT=istat, ERR=8000, END=8100) bhi, bhr, bhic, bhrc

    ELSE IF ( iflag == 1 ) THEN

      var = 'SM HEADER'
      READ (iutter, IOSTAT=istat, ERR=8000, END=8200) ndim, start_index,  &
            end_index, timeout, staggering, ordering, currentdate, name,  &
            units, description

      IF ( ndim == 1 ) THEN
        ALLOCATE ( data(end_index(1), 1, 1, 1) )
      ELSE IF ( ndim == 2 ) THEN
        ALLOCATE ( data(end_index(1), end_index(2), 1, 1) )
      ELSE IF ( ndim == 3 ) THEN
        ALLOCATE ( data(end_index(1), end_index(2), end_index(3), 1) )
      ELSE IF ( ndim == 4 ) THEN
        ALLOCATE ( data(end_index(1), end_index(2), end_index(3), end_index(4)))
      ENDIF

      var = name
      READ (iutter, IOSTAT=istat, ERR=8000, END=8300) data

      IF ( name(1:6) == 'VEGCAT' ) THEN
        READ (name(7:8), '(i2)') icat
        IF ( icat <= nummetlu ) THEN
          IF ( .NOT. gotlu(icat) ) THEN
            IF ( ( SIZE(lufrac,1) == SIZE(data,2) ) .AND.  &
                 ( SIZE(lufrac,2) == SIZE(data,1) ) ) THEN
              ! fractional land use by category [fraction]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  lufrac(j,i,icat) = data(i,j,1,1) * 0.01
                ENDDO
              ENDDO
              gotlu(icat) = .TRUE.
            ELSE
              IF ( bhi(13,1) == 1 ) THEN  ! domain 1...expanded?
                expandi = bhi(9,1)
                expandj = bhi(10,1)
                offseti = bhi(11,1)
                offsetj = bhi(12,1)
                IF ( ( SIZE(data,1)               == expandi )      .AND. &
                     ( SIZE(data,2)               == expandj )      .AND. &
                     ( SIZE(lufrac,1) + 2*offsetj == SIZE(data,2) ) .AND. &
                     ( SIZE(lufrac,2) + 2*offseti == SIZE(data,1) ) ) THEN
                  ! fractional land use by category [fraction]
                  DO j = start_index(2)+offsetj, end_index(2)-offsetj
                    DO i = start_index(1)+offseti, end_index(1)-offseti
                      ii = i - offseti
                      jj = j - offsetj
                      lufrac(jj,ii,icat) = data(i,j,1,1) * 0.01
                    ENDDO
                  ENDDO
                  gotlu(icat) = .TRUE.
                ELSE
                  GOTO 8500
                ENDIF
              ELSE
                GOTO 8500
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      DEALLOCATE ( data )

    ELSE IF ( iflag == 2 ) THEN

      EXIT v3data

    ELSE

      WRITE (6,9400) iflag, iutter
      GOTO 1001

    ENDIF

    CYCLE v3data
 999 EXIT v3data

  ENDDO v3data

!-------------------------------------------------------------------------------
! Make sure we collected the arrays we need.
!-------------------------------------------------------------------------------

  DO icat = 1, nummetlu
    IF ( .NOT. gotlu(icat) ) THEN
      WRITE (6,9600) icat
      GOTO 1001
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Make minimum value equal to 0.0.
!-------------------------------------------------------------------------------

  lufrac(:,:,:) = MAX (lufrac(:,:,:), 0.0)

!-------------------------------------------------------------------------------
! Rewind model output file.
!-------------------------------------------------------------------------------

  REWIND (iutter)

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( bhi   )
  DEALLOCATE ( bhic  )
  DEALLOCATE ( bhr   )
  DEALLOCATE ( bhrc  )
  DEALLOCATE ( gotlu )

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) iutter, istat
      GOTO 1001

 8100 WRITE (6,9100) iutter, istat
      GOTO 1001

 8200 WRITE (6,9200) iutter, istat
      GOTO 1001

 8300 WRITE (6,9300) iutter, TRIM(var), istat
      GOTO 1001

 8500 WRITE (6,9500) TRIM(var)
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READTER',                            &
              /, 1x, '***   ERROR READING FILE ON UNIT = ', i3,            &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READTER',                            &
              /, 1x, '***   UNEXPECTED END-OF-FILE REACHED ON UNIT ', i3,  &
              /, 1x, '***   IOSTAT = ', i5,                                &
              /, 1x, '***   VERIFY THAT THE FILE EXISTS!!!'                &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READTER',                            &
              /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3,          &
              /, 1x, '***   VARIABLE = SMALL HEADER',                      &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READTER',                            &
              /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3,          &
              /, 1x, '***   VARIABLE = ', a,                               &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READTER',                            &
              /, 1x, '***   UNEXPECTED FLAG FOUND IN VERSION 3 HEADER',    &
              /, 1X, '***   IFLAG = ', i3,                                 &
              /, 1x, '***   UNIT  = ', i3,                                 &
              /, 1x, 70('*'))

 9500 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READTER',                            &
              /, 1x, '***   FOUND VARIABLE ', a,                           &
              /, 1x, '***   BUT ARRAY DIMENSIONS DO NOT MATCH',            &
              /, 1X, 70('*'))

 9600 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: READTER',                            &
              /, 1x, '***   DID NOT FIND FRACTIONAL LAND USE CAT ', i3,    &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE readter
