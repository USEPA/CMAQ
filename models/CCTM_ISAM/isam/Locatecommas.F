
      SUBROUTINE LOCATECOMMAS( TXTLINE, NCOMMAS, CMAPOS )  ! KRT 20130313

!20140428  locate the commas in the textline
!       Locatecommas.F is called by MAP_FRAC in SA_DEFN.f 
!

      IMPLICIT NONE

! Arguments
      CHARACTER( * ), INTENT(  IN ) :: TXTLINE
      INTEGER,         INTENT(  IN ) :: NCOMMAS
      INTEGER,         INTENT( OUT ) :: CMAPOS( NCOMMAS )

! Local
      INTEGER         JCHAR
      INTEGER         CNT

! External
      INTEGER         LEN_TRIM

!--------------------------------------------------------

      CNT = 0

      DO JCHAR = 1, LEN_TRIM(TXTLINE)
        IF ( TXTLINE(JCHAR:JCHAR) .EQ. ',' ) THEN
          CNT = CNT + 1
          CMAPOS( CNT ) = JCHAR
        ENDIF
      ENDDO

      END
