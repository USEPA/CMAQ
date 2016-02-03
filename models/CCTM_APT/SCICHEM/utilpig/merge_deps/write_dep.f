      SUBROUTINE WRITE_DEP( OUTFILE,DEP,NVARSD,DNAME,JDATE,JTIME )

!     USE HOST_INC
      USE ERROR_INC
      USE M3UTILIO
      USE COMMON_MET
      USE GRID_CONF    ! horizontal & vertical grid definitions
      USE MULTCOMP_INC

      IMPLICIT NONE

      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS

      REAL, DIMENSION( :,:,: ) :: DEP
      INTEGER :: NVARSD
      CHARACTER( 16 ), DIMENSION( MAX_MC ) :: DNAME

      INTEGER  ROW                  ! Row index
      INTEGER  COL                  ! Column index
      INTEGER  SPC                  ! species loop counters

      INTEGER :: IOS
      REAL, DIMENSION( :,: ), ALLOCATABLE :: DEPWRITE

      CHARACTER( 16 ) :: PNAME = 'WRITE_DEP'
      CHARACTER( 16 ) :: OUTFILE
      CHARACTER( 120 ) :: XMSG = ' '
      
      write(6,*) 'Writing deposition at ',JDATE,' and ',JTIME

      ALLOCATE(DEPWRITE(MAXXB,MAXYB), STAT = IOS)
      IF (IOS /= 0) THEN
         NERROR   = SZ_ERROR
         EROUTINE = PNAME
         EMESSAGE = 'INSUFFICIENT MEMORY TO ALLOCATE DEPWRITE ARRAY'
         WRITE(EINFORM,*) 'BYTES REQUESTED =',MAX2D*4
         STOP
      END IF

      DO SPC = 1 , NVARSD
         write(6,*) 'Writing ',DNAME( SPC )

         DO  ROW = 1 , NROWS
            DO COL = 1 , NCOLS
               DEPWRITE( COL,ROW ) = DEP( COL, ROW, SPC )
            END DO              !  end loop on COL
         END DO                 !  end loop on ROW
C
         IF ( .NOT. WRITE3( OUTFILE, DNAME(SPC), JDATE, JTIME,
     &                      DEPWRITE ) ) THEN

            XMSG =  'Could not WRITE species ' //  DNAME(SPC)
     &              // 'to file ' // OUTFILE
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF  ! if WRITE(3) failed

      END DO                    !  end loop on SPC

      DEALLOCATE(DEPWRITE)

      write(6,*) 'End of write_dep'
      call flush(6)
      RETURN
      END
