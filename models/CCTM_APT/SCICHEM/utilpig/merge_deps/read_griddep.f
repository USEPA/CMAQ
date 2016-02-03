      SUBROUTINE READ_GRIDDEP( DEPFILE,DEP,NDEPS,DEPNAME,JDATE,JTIME )

!     USE HOST_INC
      USE ERROR_INC
      USE COMMON_MET
      USE GRID_CONF    ! horizontal & vertical grid definitions
      USE M3UTILIO

      IMPLICIT NONE

      CHARACTER( 16 ) :: DEPFILE
      REAL, DIMENSION( :,:,:)  :: DEP
      INTEGER         NDEPS

      CHARACTER(16), DIMENSION( NDEPS ) :: DEPNAME
      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS

      INTEGER  ROW                  ! Row index
      INTEGER  COL                  ! Column index
      INTEGER  LVL                  ! Layer index
      INTEGER  SPC                  ! species loop counters

      INTEGER :: IOS
      REAL, DIMENSION( :,: ), ALLOCATABLE :: DEPREAD

      CHARACTER( 16 ) :: PNAME = 'READ_GRIDDEP'
      CHARACTER( 120 ) :: XMSG = ' '

      write(6,*) 'Reading ',DEPFILE,' at ',JDATE,' and ',JTIME

      ALLOCATE(DEPREAD(MAXXB,MAXYB), STAT = IOS)
      IF (IOS /= 0) THEN
         NERROR   = SZ_ERROR
         EROUTINE = PNAME
         EMESSAGE = 'INSUFFICIENT MEMORY TO ALLOCATE DEPREAD ARRAY'
         WRITE(EINFORM,*) 'BYTES REQUESTED =',MAX2D*4
         STOP
      END IF

      DO SPC = 1, NDEPS
         write(6,*) DEPNAME( SPC )
C
         IF ( .NOT. READ3( DEPFILE, DEPNAME( SPC ), ALLAYS3,
!    &      JDATE, JTIME, DEP( 1, 1, SPC ) ) ) THEN
     &      JDATE, JTIME, DEPREAD ) ) THEN

            XMSG = 'Could not read ' // DEPNAME( SPC ) //
     &           ' from ' // DEPFILE
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF                    ! read failure
         DO  ROW = 1 , NROWS
            DO COL = 1 , NCOLS
               DEP( COL, ROW, SPC ) = DEPREAD( COL,ROW )
            END DO              !  end loop on COL
         END DO                 !  end loop on ROW

      END DO                    !  end loop on SPC

      DEALLOCATE(DEPREAD)

      write(6,*) 'End of read_griddep'
      RETURN
      END
