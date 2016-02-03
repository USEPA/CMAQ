      SUBROUTINE OPEN_FILES( CONC_IN,CONC_OUT,JDATE,JTIME,NHRS )

! Updated Feb 25, 2015 to include fix for W_VEL stored in base CONC file
! PKK, ENVIRON

!     USE HOST_INC
      USE M3UTILIO

      IMPLICIT NONE

      CHARACTER( 16 ) :: CONC_IN
      CHARACTER( 16 ) :: CONC_OUT

      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS
      INTEGER         NHRS      !  No. of hours of data
      INTEGER         V         !  Variable loop index

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
      write(*,*)'MXREC3D = ',MXREC3D
      write(*,*)'SDATE3D, STIME3D: ',SDATE3D,STIME3D
!pk
      MXREC3D = NHRS

      SDATE3D = JDATE
      STIME3D = JTIME

!pk
      DO V = 1, NVARS3D
         write(*,*)'Species: ',V,'; Name: ',VNAME3D(V)
      END DO
!pk

!FIX for W_VEL
      if (TRIM(vname3d(NVARS3D)) == 'W_VEL') NVARS3D = NVARS3D - 1
!FIX for W_VEL

      FDESC3D(1) = 'Merged SCICHEM/CTM output file ' // CONC_OUT

      IF ( .NOT. OPEN3( CONC_OUT, FSNEW3, PNAME ) ) THEN

         XMSG = 'Could not open ' // CONC_OUT // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      RETURN
      END
