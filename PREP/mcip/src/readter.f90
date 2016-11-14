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
!           29 Aug 2011  Changed name of module FILE to FILES, DATA to DATA_IN,
!                        and NAME to VNAME to avoid conflicts with F90
!                        protected intrinsic.  Improved error handling.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE files
  USE metvars
  USE mcipparm

  IMPLICIT NONE

  INTEGER,            ALLOCATABLE   :: bhi       ( : , : )
  CHARACTER(LEN=80),  ALLOCATABLE   :: bhic      ( : , : )
  REAL,               ALLOCATABLE   :: bhr       ( : , : )
  CHARACTER(LEN=80),  ALLOCATABLE   :: bhrc      ( : , : )
  CHARACTER(LEN=24)                 :: currentdate
  REAL,               ALLOCATABLE   :: data_in   ( : , : , : , : )
  CHARACTER(LEN=46)                 :: description
  INTEGER                           :: end_index ( 4 )
  INTEGER                           :: expandi
  INTEGER                           :: expandj
  LOGICAL,            ALLOCATABLE   :: gotlu     ( : )
  INTEGER                           :: i
  INTEGER                           :: ii
  INTEGER                           :: icat
  INTEGER                           :: iflag
  INTEGER                           :: istat
  INTEGER                           :: j
  INTEGER                           :: jj
  INTEGER                           :: ndim
  INTEGER,            PARAMETER     :: numprogs  = 20
  INTEGER,            PARAMETER     :: numvalsi  = 50
  INTEGER,            PARAMETER     :: numvalsr  = 20
  INTEGER                           :: offseti
  INTEGER                           :: offsetj
  CHARACTER(LEN=4)                  :: ordering
  CHARACTER(LEN=16),  PARAMETER     :: pname     = 'READTER'
  CHARACTER(LEN=4)                  :: staggering
  INTEGER                           :: start_index ( 4 )
  REAL                              :: timeout
  CHARACTER(LEN=25)                 :: units
  CHARACTER(LEN=9)                  :: var
  CHARACTER(LEN=9)                  :: vname

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR READING FILE ON UNIT = ', i3, &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNEXPECTED END-OF-FILE REACHED ON UNIT ', i3, &
    & /, 1x, '***   IOSTAT = ', i5, &
    & /, 1x, '***   VERIFY THAT THE FILE EXISTS!!!', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3, &
    & /, 1x, '***   VARIABLE = SMALL HEADER', &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3, &
    & /, 1x, '***   VARIABLE = ', a, &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9400 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNEXPECTED FLAG FOUND IN VERSION 3 HEADER', &
    & /, 1X, '***   IFLAG = ', i3, &
    & /, 1x, '***   UNIT  = ', i3, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9500 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   FOUND VARIABLE ', a, &
    & /, 1x, '***   BUT ARRAY DIMENSIONS DO NOT MATCH', &
    & /, 1X, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9600 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   DID NOT FIND FRACTIONAL LAND USE CAT ', i3, &
    & /, 1x, 70('*'))"

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
    READ (iutter, IOSTAT=istat) iflag
  
    IF ( istat > 0 ) THEN  ! error on read
      WRITE (*,f9000) TRIM(pname), iutter, istat
      CALL graceful_stop (pname)
    ELSE IF ( istat < 0 ) THEN  ! end-of-file reached
      EXIT v3data
    ENDIF

    IF ( iflag == 0 ) THEN
      var = 'BIG HEADR'
      READ (iutter, IOSTAT=istat) bhi, bhr, bhic, bhrc

      IF ( istat > 0 ) THEN  ! error on read
        WRITE (*,f9000) TRIM(pname), iutter, istat
        CALL graceful_stop (pname)
      ELSE IF ( istat < 0 ) THEN  ! end-of_file reached
        WRITE (*,f9100) TRIM(pname), iutter, istat
        CALL graceful_stop (pname)
      ENDIF

    ELSE IF ( iflag == 1 ) THEN

      var = 'SM HEADER'
      READ (iutter, IOSTAT=istat) ndim, start_index, end_index, timeout,  &
            staggering, ordering, currentdate, vname, units, description

      IF ( istat > 0 ) THEN  ! error on read
        WRITE (*,f9000) TRIM(pname), iutter, istat
        CALL graceful_stop (pname)
      ELSE IF ( istat < 0 ) THEN  ! end-of_file reached
        WRITE (*,f9200) TRIM(pname), iutter, istat
        CALL graceful_stop (pname)
      ENDIF

      IF ( ndim == 1 ) THEN
        ALLOCATE ( data_in(end_index(1), 1, 1, 1) )
      ELSE IF ( ndim == 2 ) THEN
        ALLOCATE ( data_in(end_index(1), end_index(2), 1, 1) )
      ELSE IF ( ndim == 3 ) THEN
        ALLOCATE ( data_in(end_index(1), end_index(2), end_index(3), 1) )
      ELSE IF ( ndim == 4 ) THEN
        ALLOCATE ( data_in(end_index(1), end_index(2), end_index(3),  &
                           end_index(4)))
      ENDIF

      var = vname
      READ (iutter, IOSTAT=istat) data_in

      IF ( istat > 0 ) THEN  ! error on read
        WRITE (*,f9000) TRIM(pname), iutter, istat
        CALL graceful_stop (pname)
      ELSE IF ( istat < 0 ) THEN  ! end-of_file reached
        WRITE (*,f9300) TRIM(pname), iutter, TRIM(var), istat
        CALL graceful_stop (pname)
      ENDIF

      IF ( vname(1:6) == 'VEGCAT' ) THEN
        READ (vname(7:8), '(i2)') icat
        IF ( icat <= nummetlu ) THEN
          IF ( .NOT. gotlu(icat) ) THEN
            IF ( ( SIZE(lufrac,1) == SIZE(data_in,2) ) .AND.  &
                 ( SIZE(lufrac,2) == SIZE(data_in,1) ) ) THEN
              ! fractional land use by category [fraction]
              DO j = start_index(2), end_index(2)
                DO i = start_index(1), end_index(1)
                  lufrac(j,i,icat) = data_in(i,j,1,1) * 0.01
                ENDDO
              ENDDO
              gotlu(icat) = .TRUE.
            ELSE
              IF ( bhi(13,1) == 1 ) THEN  ! domain 1...expanded?
                expandi = bhi(9,1)
                expandj = bhi(10,1)
                offseti = bhi(11,1)
                offsetj = bhi(12,1)
                IF ( ( SIZE(data_in,1)            == expandi )      .AND. &
                     ( SIZE(data_in,2)            == expandj )      .AND. &
                     ( SIZE(lufrac,1) + 2*offsetj == SIZE(data_in,2) ) .AND. &
                     ( SIZE(lufrac,2) + 2*offseti == SIZE(data_in,1) ) ) THEN
                  ! fractional land use by category [fraction]
                  DO j = start_index(2)+offsetj, end_index(2)-offsetj
                    DO i = start_index(1)+offseti, end_index(1)-offseti
                      ii = i - offseti
                      jj = j - offsetj
                      lufrac(jj,ii,icat) = data_in(i,j,1,1) * 0.01
                    ENDDO
                  ENDDO
                  gotlu(icat) = .TRUE.
                ELSE
                  WRITE (*,f9500) TRIM(pname), TRIM(var)
                  CALL graceful_stop (pname)
                ENDIF
              ELSE
                WRITE (*,f9500) TRIM(pname), TRIM(var)
                CALL graceful_stop (pname)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      DEALLOCATE ( data_in )

    ELSE IF ( iflag == 2 ) THEN

      EXIT v3data

    ELSE

      WRITE (*,f9400) TRIM(pname), iflag, iutter
      CALL graceful_stop (pname)

    ENDIF

  ENDDO v3data

!-------------------------------------------------------------------------------
! Make sure we collected the arrays we need.
!-------------------------------------------------------------------------------

  DO icat = 1, nummetlu
    IF ( .NOT. gotlu(icat) ) THEN
      WRITE (*,f9600) TRIM(pname), icat
      CALL graceful_stop (pname)
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

END SUBROUTINE readter
