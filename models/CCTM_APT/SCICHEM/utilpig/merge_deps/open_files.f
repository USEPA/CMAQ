      SUBROUTINE OPEN_FILES( DDEP_IN, WDEP_IN, DDEP_OUT, WDEP_OUT,
     &                       JDATE, JTIME, NHRS,NVARSDD, NVARSWD,
     &                       DNAME, WNAME )

!     USE HOST_INC
      USE M3UTILIO

      IMPLICIT NONE

      CHARACTER( 16 ) :: DDEP_IN, WDEP_IN, DDEP_OUT, WDEP_OUT

      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS
      INTEGER         NHRS      !  No. of hours of data
      INTEGER         V         !  Variable loop index
      INTEGER         SPC       !  Variable loop index

      INTEGER         NVARSDD, NVARSWD

      CHARACTER( 16 ), DIMENSION( * ) :: DNAME, WNAME
      CHARACTER( 16 ) :: SPNAME

      CHARACTER( 16 ) :: PNAME = 'OPEN_FILES'
      CHARACTER( 120 ) :: XMSG = ' '

      write(6,*) 'Opening ',DDEP_IN,' file'

      IF ( .NOT. OPEN3( DDEP_IN, FSREAD3, PNAME ) ) THEN

         XMSG = 'Could not open ' // DDEP_IN // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      IF ( .NOT. DESC3( DDEP_IN ) ) THEN

         XMSG = 'Could not get ' // DDEP_IN // ' file description'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF              !  error abort if DESC3 failed

!pk
      write(*,*)'Dry Dep file'
      write(*,*)'NVARS3D = ',NVARS3D
      write(*,*)'MXREC3D = ',MXREC3D
      write(*,*)'SDATE3D, STIME3D: ',SDATE3D,STIME3D
!pk

      NVARSDD = NVARS3D
      DO V = 1, NVARS3D
!pk
         write(*,*)'Species: ',V,'; Name: ',VNAME3D(V)
!pk
         DNAME( V ) = VNAME3D( V )
      END DO
!pk

      MXREC3D = NHRS

      SDATE3D = JDATE
      STIME3D = JTIME

      FDESC3D(1) = 'Merged SCICHEM/CTM dry deposition file ' // DDEP_OUT

      IF ( .NOT. OPEN3( DDEP_OUT, FSNEW3, PNAME ) ) THEN

         XMSG = 'Could not open ' // DDEP_OUT // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      write(6,*) 'Opening ',WDEP_IN,' file'

      IF ( .NOT. OPEN3( WDEP_IN, FSREAD3, PNAME ) ) THEN

         XMSG = 'Could not open ' // WDEP_IN // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      IF ( .NOT. DESC3( WDEP_IN ) ) THEN

         XMSG = 'Could not get ' // WDEP_IN // ' file description'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF              !  error abort if DESC3 failed

!pk
      write(*,*)'Wet Dep file'
      write(*,*)'NVARS3D = ',NVARS3D
      write(*,*)'MXREC3D = ',MXREC3D
      write(*,*)'SDATE3D, STIME3D: ',SDATE3D,STIME3D
!pk

      NVARSWD = NVARS3D
      DO V = 1, NVARS3D
!pk
         write(*,*)'Species: ',V,'; Name: ',VNAME3D(V)
!pk
         WNAME( V ) = VNAME3D( V )
      END DO

      MXREC3D = NHRS

      SDATE3D = JDATE
      STIME3D = JTIME

      FDESC3D(1) = 'Merged SCICHEM/CTM wet deposition file ' // WDEP_OUT

      IF ( .NOT. OPEN3( WDEP_OUT, FSNEW3, PNAME ) ) THEN

         XMSG = 'Could not open ' // WDEP_OUT // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      RETURN
      END
