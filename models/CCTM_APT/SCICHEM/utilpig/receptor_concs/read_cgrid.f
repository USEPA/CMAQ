      SUBROUTINE READ_CGRID( CONCFILE,CGRID,JDATE,JTIME )

      USE ERROR_INC
      USE CGRID_SPCS   ! CGRID species number and offsets
      USE GRID_CONF    ! horizontal & vertical grid definitions
      USE M3UTILIO
      USE COMMON_MET
      USE MULTCOMP_INC

      IMPLICIT NONE

      CHARACTER( 16 ) :: CONCFILE
      REAL :: CGRID( :,:,:,: )              !3D ambient from host model
      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS

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

!debug
!debug write(6,*)'CGRID Size: ',SIZE(CGRID)
!debug write(6,*)'maxxb,maxyb,maxzb: ',maxxb,maxyb,maxzb
!debug
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
!debug
!debug      write(6,*)'strt,fini: ',strt,fini
!debug
      DO v = strt, fini

         spc = spc + 1
         write(6,*) ' Reading species: ',GC_SPC( SPC )
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
               write(6,*) ' Initialized species: ',GC_SPC( SPC )
            END IF
         ELSE
!debug      write(6,*) ' Finished reading species: ',GC_SPC( SPC )
            DO  ROW = 1 , NROWS
               DO  COL = 1 , NCOLS
                  DO  LVL = 1 , NLAYS

                     CGRID( COL, ROW, LVL, v ) =
     &                       CGOUT( COL,ROW,LVL )

                  END DO           !  end loop on LVL
               END DO              !  end loop on COL
            END DO                 !  end loop on ROW
!debug      write(6,*) ' Finished assigning species: ',GC_SPC( SPC )
         END IF                 !  if READ3() failed
      END DO                    !  end loop on GC_SPC

      strt = AE_STRT
      fini = AE_STRT - 1 + N_AE_SPC
!debug
!debug      write(6,*)'strt,fini: ',strt,fini
!debug
      spc = 0
      DO v = strt, fini
         spc = spc + 1
         write(6,*) ' Reading species: ',AE_SPC( SPC )
!debug   call flush(6)
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
               write(6,*) ' Initialized species: ',AE_SPC( SPC )
!debug         call flush(6)
            END IF
         ELSE
!debug      write(6,*) ' Finished reading species: ',AE_SPC( SPC )
            DO  ROW = 1 , NROWS
               DO  COL = 1 , NCOLS
                  DO  LVL = 1 , NLAYS

                     CGRID( COL, ROW, LVL, v ) =
     &                       CGOUT( COL,ROW,LVL )

                  END DO           !  end loop on LVL
               END DO              !  end loop on COL
            END DO                 !  end loop on ROW
!debug      write(6,*) ' Finished assigning species: ',AE_SPC( SPC )
!debug      call flush(6)
         END IF                 !  if READ3() failed
      END DO                    !  end loop on AE_SPC

      strt = NR_STRT
      fini = NR_STRT - 1 + N_NR_SPC
!debug
!debug      write(6,*)'strt,fini: ',strt,fini
!debug
      spc = 0
      DO v = strt, fini
         spc = spc + 1
         write(6,*) ' Reading species: ',NR_SPC( SPC )
!debug   call flush(6)
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
               write(6,*) ' Initialized species: ',NR_SPC( SPC )
            END IF
         ELSE
!debug      write(6,*) ' Finished reading species: ',NR_SPC( SPC )
            DO  ROW = 1 , NROWS
               DO  COL = 1 , NCOLS
                  DO  LVL = 1 , NLAYS

                     CGRID( COL, ROW, LVL, v ) =
     &                       CGOUT( COL,ROW,LVL )

                  END DO           !  end loop on LVL
               END DO              !  end loop on COL
            END DO                 !  end loop on ROW
!debug      write(6,*) ' Finished assigning species: ',NR_SPC( SPC )
!debug      call flush(6)
         END IF                 !  if READ3() failed
      END DO                    !  end loop on NR_SPC

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
      END IF

      DEALLOCATE(CGOUT)

!debug      write(6,*) 'End of read_cgrid'
!debug      call flush(6)

      RETURN
      END
