      SUBROUTINE WRITE_CGRID( OUTFILE,CGRID,JDATE,JTIME,ISSAVED )

      USE ERROR_INC
      USE CGRID_SPCS   ! CGRID species number and offsets
      USE GRID_CONF    ! horizontal & vertical grid definitions
!     USE HOST_INC
      USE M3UTILIO
!     USE HOST_CHEM_INC
      USE COMMON_MET
      USE MULTCOMP_INC

      IMPLICIT NONE

      CHARACTER( 16 ) :: OUTFILE
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

      CHARACTER( 16 ) :: PNAME = 'WRITE_CGRID'

      CHARACTER( 120 ) :: XMSG = ' '
      
      MDATE = JDATE
      MTIME = JTIME

      ALLOCATE(CGOUT(MAXXB,MAXYB,MAXZB), STAT = IOS)
      IF (IOS /= 0) THEN
         NERROR   = SZ_ERROR
         EROUTINE = 'WRITE_CGRID'
         EMESSAGE = 'INSUFFICIENT MEMORY TO ALLOCATE CGOUT ARRAY'
         WRITE(EINFORM,*) 'BYTES REQUESTED =',MAX2D*MAXZB*4
         STOP
      END IF

      write(6,*) 'Writing cgrid at ',MDATE,' and ',MTIME
      strt = GC_STRT
      fini = GC_STRT - 1 + N_GC_SPC
      spc = 0
      DO v = strt, fini
         spc = spc + 1
         IF (ISSAVED(v)) THEN
            write(6,*) 'Writing ',GC_SPC( SPC )
C
            DO  ROW = 1 , NROWS
               DO  COL = 1 , NCOLS
                  DO  LVL = 1 , NLAYS
                     CGOUT( COL,ROW,LVL ) = 
     &                       CGRID( COL, ROW, LVL, v )

                  END DO           !  end loop on LVL
               END DO              !  end loop on COL
            END DO                 !  end loop on ROW
            IF ( .NOT. WRITE3( OUTFILE, GC_SPC(SPC), JDATE, JTIME,
     &                         CGOUT ) ) THEN
               XMSG =  'Could not WRITE species ' //  GC_SPC(SPC) //
     &                   'to file ' // OUTFILE
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF  ! if WRITE(3) failed

         END IF
      END DO                    !  end loop on GC_SPC

      strt = AE_STRT
      fini = AE_STRT - 1 + N_AE_SPC
      spc = 0
      DO v = strt, fini
         spc = spc + 1
         IF (ISSAVED(v)) THEN
            write(6,*) 'Writing ',AE_SPC( SPC )
C
            DO  ROW = 1 , NROWS
               DO  COL = 1 , NCOLS
                  DO  LVL = 1 , NLAYS
                     CGOUT( COL,ROW,LVL ) = 
     &                       CGRID( COL, ROW, LVL, v )

                  END DO           !  end loop on LVL
               END DO              !  end loop on COL
            END DO                 !  end loop on ROW
            IF ( .NOT. WRITE3( OUTFILE, AE_SPC(SPC), JDATE, JTIME,
     &                         CGOUT ) ) THEN
               XMSG =  'Could not WRITE species ' //  AE_SPC(SPC) //
     &                   'to file ' // OUTFILE
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF  ! if WRITE(3) failed

         END IF
      END DO                    !  end loop on AE_SPC

      strt = NR_STRT
      fini = NR_STRT - 1 + N_NR_SPC
      spc = 0
      DO v = strt, fini
         spc = spc + 1
         IF (ISSAVED(v)) THEN
            write(6,*) 'Writing ',NR_SPC( SPC )
C
            DO  ROW = 1 , NROWS
               DO  COL = 1 , NCOLS
                  DO  LVL = 1 , NLAYS
                     CGOUT( COL,ROW,LVL ) = 
     &                       CGRID( COL, ROW, LVL, v )

                  END DO           !  end loop on LVL
               END DO              !  end loop on COL
            END DO                 !  end loop on ROW
            IF ( .NOT. WRITE3( OUTFILE, NR_SPC(SPC), JDATE, JTIME,
     &                         CGOUT ) ) THEN
               XMSG =  'Could not WRITE species ' //  NR_SPC(SPC) //
     &                   'to file ' // OUTFILE
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF  ! if WRITE(3) failed

         END IF
      END DO                    !  end loop on NR_SPC

      DEALLOCATE(CGOUT)

      write(6,*) 'End of write_cgrid'
      RETURN
      END
