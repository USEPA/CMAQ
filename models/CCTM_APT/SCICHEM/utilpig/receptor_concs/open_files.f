      SUBROUTINE OPEN_FILES( CONC_IN,JDATE,JTIME,NHRS,NSPEC,SPNAME )

!     USE HOST_INC
      USE M3UTILIO

      IMPLICIT NONE

      CHARACTER( 16 ) :: CONC_IN

      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS
      INTEGER         NHRS      !  No. of hours of data
      INTEGER         NSPEC     !  Species required for processing

      CHARACTER( 16 ), DIMENSION( NSPEC ) :: SPNAME

      INTEGER         I,V        !  Variable loop indices

      LOGICAL, DIMENSION( NSPEC ) :: AVAILABLE

      CHARACTER( 16 ) :: PNAME = 'OPEN_FILES'
      CHARACTER( 120 ) :: XMSG = ' '

      write(6,*) 'Opening ',CONC_IN,' file'

      IF ( .NOT. OPEN3( CONC_IN, FSREAD3, PNAME ) ) THEN

         XMSG = 'Could not open ' // CONC_IN // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      IF ( .NOT. DESC3( CONC_IN ) ) THEN

         XMSG = 'Could not get ' // CONC_IN // ' file description'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF              !  error abort if DESC3 failed

!pk
!pk   write(*,*)'MXREC3D = ',MXREC3D
!pk   write(*,*)'SDATE3D, STIME3D: ',SDATE3D,STIME3D
!pk
      MXREC3D = NHRS

      SDATE3D = JDATE
      STIME3D = JTIME

!pk
!pk   write(*,*)'Saved species in CONC file:'
!pk   DO V = 1, NVARS3D
!pk      write(*,*)'Species: ',V,'; Name: ',VNAME3D(V)
!pk   END DO
!pk
! --- Check if required species are available in gridded file
      AVAILABLE = .FALSE.
      DO I = 1, NSPEC
         DO V = 1, NVARS3D
            IF (TRIM(SPNAME(I)) == TRIM(VNAME3D(V))) THEN
               AVAILABLE(I) = .TRUE.
!debug
!debug      write(*,*)'i,v: ',i,v
!debug
               EXIT
            END IF
         END DO
      END DO
      DO I = 1, NSPEC
         IF ( .NOT. AVAILABLE(I) ) THEN
            WRITE(*,*)'Warning: Species ',SPNAME(I),
     &                ' not available in CONC file'
            WRITE(*,*)'Zero will be assigned to grid value'
         END IF
      END DO

      RETURN
      END
