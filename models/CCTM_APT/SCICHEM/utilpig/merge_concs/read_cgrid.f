      SUBROUTINE READ_CGRID( CONCFILE,CGRID,JDATE,JTIME,ISSAVED )

      USE ERROR_INC
      USE CGRID_SPCS   ! CGRID species number and offsets
      USE GRID_CONF    ! horizontal & vertical grid definitions
!     USE HOST_INC
      USE M3UTILIO
!     USE HOST_CHEM_INC
      USE COMMON_MET
      USE MULTCOMP_INC

      IMPLICIT NONE

      CHARACTER( 16 ) :: CONCFILE
      REAL :: CGRID( :,:,:,: )              !3D ambient from host model
      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS
      LOGICAL :: ISSAVED( * ) ! Flag if species saved/not saved

C....... Local variables
      INTEGER         MDATE     !  date at mid-time step, coded YYYYDDD
      INTEGER         MTIME     !  time at mid-time step, coded HHMMSS

      INTEGER  ROW                  ! Row index
      INTEGER  COL                  ! Column index
      INTEGER  LVL                  ! Layer index
      INTEGER  SPC                  ! species loop counters
      integer strt, fini, v
      INTEGER :: IOS
      REAL, DIMENSION( :,:,: ), ALLOCATABLE :: CGOUT

      CHARACTER( 16 ) :: PNAME = 'READ_CGRID'

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      ALLOCATE(CGOUT(MAXXB,MAXYB,MAXZB), STAT = IOS)
      IF (IOS /= 0) THEN
         NERROR   = SZ_ERROR
         EROUTINE = 'READ_CGRID'
         EMESSAGE = 'INSUFFICIENT MEMORY TO ALLOCATE CGOUT ARRAY'
         WRITE(EINFORM,*) 'BYTES REQUESTED =',MAX2D*MAXZB*4
         STOP
      END IF
      
      MDATE = JDATE
      MTIME = JTIME

      write(6,*) 'Reading cgrid at ',MDATE,' and ',MTIME

      strt = GC_STRT
      fini = GC_STRT - 1 + N_GC_SPC
      spc = 0
      DO v = strt, fini

         spc = spc + 1
         write(6,*) ' Reading species: ',GC_SPC( SPC )
C
CPK         IF ( .NOT. INTERP3( CONCFILE, GC_SPC( SPC ) , PNAME,
CPK     &                       MDATE, MTIME, NCOLS*NROWS*NLAYS,
CPK     &                       CGOUT ) ) THEN
         IF ( .NOT. READ3( CONCFILE, GC_SPC( SPC ), ALLAYS3,
     &      MDATE, MTIME, CGOUT ) ) THEN
            IF ( FIRSTIME ) THEN
               WRITE(*,*)'Species: ',GC_SPC(SPC),' not in gridded file'
               DO  ROW = 1 , NROWS
                  DO  COL = 1 , NCOLS
                     DO  LVL = 1 , NLAYS

                        CGRID( COL, ROW, LVL, v ) = 0.

                     END DO           !  end loop on LVL
                  END DO              !  end loop on COL
               END DO                 !  end loop on ROW
               ISSAVED(v) = .FALSE.
            END IF
         ELSE
            DO  ROW = 1 , NROWS
               DO  COL = 1 , NCOLS
                  DO  LVL = 1 , NLAYS

                     CGRID( COL, ROW, LVL, v ) =
     &                       CGOUT( COL,ROW,LVL )

                  END DO           !  end loop on LVL
               END DO              !  end loop on COL
            END DO                 !  end loop on ROW
            IF ( FIRSTIME ) THEN
               ISSAVED(v) = .TRUE.
            END IF
         END IF                 !  if READ3() failed
      END DO                    !  end loop on GC_SPC

      strt = AE_STRT
      fini = AE_STRT - 1 + N_AE_SPC
      spc = 0
      DO v = strt, fini
         spc = spc + 1
         write(6,*) ' Reading species: ',AE_SPC( SPC )
C
         IF ( .NOT. READ3( CONCFILE, AE_SPC( SPC ), ALLAYS3,
     &      MDATE, MTIME, CGOUT ) ) THEN
            IF ( FIRSTIME ) THEN
               WRITE(*,*)'Species: ',AE_SPC(SPC),' not in gridded file'
               DO  ROW = 1 , NROWS
                  DO  COL = 1 , NCOLS
                     DO  LVL = 1 , NLAYS

                        CGRID( COL, ROW, LVL, v ) = 0.

                     END DO           !  end loop on LVL
                  END DO              !  end loop on COL
               END DO                 !  end loop on ROW
               ISSAVED(v) = .FALSE.
            END IF
         ELSE
            DO  ROW = 1 , NROWS
               DO  COL = 1 , NCOLS
                  DO  LVL = 1 , NLAYS

                     CGRID( COL, ROW, LVL, v ) =
     &                       CGOUT( COL,ROW,LVL )

                  END DO           !  end loop on LVL
               END DO              !  end loop on COL
            END DO                 !  end loop on ROW
            IF ( FIRSTIME ) THEN
               ISSAVED(v) = .TRUE.
            END IF
         END IF                 !  if READ3() failed
      END DO                    !  end loop on AE_SPC

      strt = NR_STRT
      fini = NR_STRT - 1 + N_NR_SPC
      spc = 0
      DO v = strt, fini
         spc = spc + 1
         write(6,*) ' Reading species: ',NR_SPC( SPC )
C
         IF ( .NOT. READ3( CONCFILE, NR_SPC( SPC ), ALLAYS3,
     &      MDATE, MTIME, CGOUT ) ) THEN
            IF ( FIRSTIME ) THEN
               WRITE(*,*)'Species: ',NR_SPC(SPC),' not in gridded file'
               DO  ROW = 1 , NROWS
                  DO  COL = 1 , NCOLS
                     DO  LVL = 1 , NLAYS

                        CGRID( COL, ROW, LVL, v ) = 0.

                     END DO           !  end loop on LVL
                  END DO              !  end loop on COL
               END DO                 !  end loop on ROW
               ISSAVED(v) = .FALSE.
            END IF
         ELSE
            DO  ROW = 1 , NROWS
               DO  COL = 1 , NCOLS
                  DO  LVL = 1 , NLAYS

                     CGRID( COL, ROW, LVL, v ) =
     &                       CGOUT( COL,ROW,LVL )

                  END DO           !  end loop on LVL
               END DO              !  end loop on COL
            END DO                 !  end loop on ROW
            IF ( FIRSTIME ) THEN
               ISSAVED(v) = .TRUE.
            END IF
         END IF                 !  if READ3() failed
      END DO                    !  end loop on NR_SPC

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
      END IF

      DEALLOCATE(CGOUT)

      write(6,*) 'End of read_cgrid'

      RETURN
      END
