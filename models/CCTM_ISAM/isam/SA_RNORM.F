      SUBROUTINE SA_RNORM ( JDATE, JTIME, TSTEP, CGRID, ISAM )
      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c20140428  Provides ISAM tracer mass adjustments according to 
c          CGRID mass changes
c
c          Called by sciproc.F
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      USE GRID_CONF
      USE CGRID_SPCS            ! CGRID species number and offsets
      USE SA_DEFN               ! Mc06
      USE SUBST_MODULES         ! stenex
      USE UTILIO_DEFN
      
      IMPLICIT NONE
      
      INCLUDE SUBST_FILES_ID    ! I/O File name
      
      INTEGER, INTENT(IN) ::    JDATE          ! current model date, coded YYYYDDD
      INTEGER, INTENT(IN) ::    JTIME          ! current model time, coded HHMMSS
      INTEGER, INTENT(IN) ::    TSTEP(3)       ! format 10000, 0, 0

      REAL, POINTER :: CGRID( :,:,:,: )                 ! concentrations

      REAL, POINTER :: ISAM( :,:,:,:,: )
      
c     Local Variables

      LOGICAL, SAVE :: LBEFORE = .TRUE.  ! logical name to identify before/after

      INTEGER          C, R, L, S, K, JSPC                     ! LOOP INDEX

      
c     for all others
      REAL             CGTTL
      REAL             ISAMTTL
      REAL             DIFF

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER*16     TEMP
      REAL, ALLOCATABLE, SAVE    :: WR_BUFFER(:,:,:,:)  ! (c,r,l,s)
      REAL, ALLOCATABLE, SAVE    :: BUFF3(:,:,:)  ! (c,r,l)
      INTEGER          ALLOCSTAT
      CHARACTER*96 :: XMSG = ' '
      INTEGER          MDATE            ! middle of this time step
      INTEGER          MTIME            ! middle of this time step
      INTEGER          NDATE            ! middle of this time step
      INTEGER          NTIME            ! middle of this time step
      CHARACTER( 16 ), SAVE :: PNAME1 = 'SA_RNORM'


!20140416 edited
      INTEGER,  SAVE :: CNTRNORM
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: VRNORM( : )
      INTEGER,  ALLOCATABLE, SAVE :: MAPRNORM( : )
      INTEGER    JNORM
      INTEGER    TAILV

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      MDATE = JDATE
      MTIME = JTIME

      IF ( FIRSTIME .AND. LBEFORE ) THEN    

        FIRSTIME = .FALSE.

!20140416 Allocate array for map from tag species index to renorm index
        ALLOCATE ( MAPRNORM( NSPC_SA ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating MAPRNORM'
          CALL M3EXIT( PNAME1, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        MAPRNORM = 0

!20140416 Count number of bulk species that get renormalized
        CNTRNORM = 0
        DO JSPC = 1, NSPC_SA
          IF ( SPC_NAME( JSPC, OTHRTAG ) .EQ. 'O3V' ) THEN
            CNTRNORM = CNTRNORM + 1
          ELSEIF ( SPC_NAME( JSPC, OTHRTAG ) .NE. 'O3N' ) THEN
            CNTRNORM = CNTRNORM + 1
          END IF
          MAPRNORM( JSPC ) = CNTRNORM
        ENDDO ! jspc

!20140416 Allocate array for renorm output variable names
        ALLOCATE ( VRNORM( 2*CNTRNORM ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating VRNORM'
          CALL M3EXIT( PNAME1, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

!20140416 Assign species names
        DO JSPC = 1, NSPC_SA
          IF ( SPC_NAME( JSPC, OTHRTAG ) .EQ. 'O3V' ) THEN
            VRNORM(MAPRNORM(JSPC)) = 'BEF_O3'
            VRNORM(MAPRNORM(JSPC)+CNTRNORM) = 'AFT_O3'
          ELSEIF ( SPC_NAME( JSPC, OTHRTAG ) .NE. 'O3N' ) THEN
            VRNORM(MAPRNORM(JSPC)) = 'BEF_' // SPC_NAME(JSPC, OTHRTAG)
            VRNORM(MAPRNORM(JSPC)+CNTRNORM) = 'AFT_' // SPC_NAME(JSPC, OTHRTAG)
          ENDIF
        ENDDO ! jspc

!20140416 Allocate array for renorm difference
        ALLOCATE ( WR_BUFFER(MY_NCOLS, MY_NROWS, NLAYS, 2*CNTRNORM ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating WR_BUFFER'
          CALL M3EXIT( PNAME1, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        WR_BUFFER = 0.0

!20140417 Allocate 3D array 
        ALLOCATE ( BUFF3(MY_NCOLS, MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating BUFF3'
          CALL M3EXIT( PNAME1, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        BUFF3 = 0.0

        NVARS3D = CNTRNORM * 2  ! before / after
        SDATE3D = JDATE
        STIME3D = JTIME
        FTYPE3D = GRDDED3
        TSTEP3D = TSTEP(1)
        NCOLS3D = GL_NCOLS
        NROWS3D = GL_NROWS
        NLAYS3D = NLAYS
        NTHIK3D =     1
        GDTYP3D = GDTYP_GD
        P_ALP3D = P_ALP_GD
        P_BET3D = P_BET_GD
        P_GAM3D = P_GAM_GD
        XORIG3D = XORIG_GD
        YORIG3D = YORIG_GD
        XCENT3D = XCENT_GD
        YCENT3D = YCENT_GD
        XCELL3D = XCELL_GD
        YCELL3D = YCELL_GD
        VGTYP3D = VGTYP_GD
        VGTOP3D = VGTOP_GD
         
        DO L = 1, NLAYS3D + 1
          VGLVS3D( L ) = VGLVS_GD( L )
        END DO

        GDNAM3D = GRID_NAME  

        FDESC3D( 1 ) = 'SA RENORMAL MASS: CGRID - ISAM'
        DO L = 2, MXDESC3
          FDESC3D( L ) = ' '
        END DO

!20140416 Assign species names' attributes
        DO JNORM = 1, CNTRNORM
          VTYPE3D( JNORM ) = M3REAL
          VNAME3D( JNORM ) = VRNORM(JNORM)
          TAILV = LEN_TRIM(VRNORM(JNORM))
          IF ( VRNORM(JNORM)(5:5) .EQ. 'A' .AND.
     &         ( VRNORM(JNORM)(TAILV:TAILV) .EQ. 'I' .OR. 
     &           VRNORM(JNORM)(TAILV:TAILV) .EQ. 'J' ) ) THEN
            UNITS3D( JNORM ) = 'microgram/m**3'
          ELSE
            UNITS3D( JNORM ) = 'ppmV'
          END IF
          VDESC3D( JNORM ) = 'cgrid - isam before CHEM'

          VTYPE3D( JNORM + CNTRNORM ) = M3REAL
          VNAME3D( JNORM + CNTRNORM ) = VRNORM(JNORM+CNTRNORM)
          IF ( VRNORM(JNORM+CNTRNORM)(5:5) .EQ. 'A' .AND.
     &         ( VRNORM(JNORM+CNTRNORM)(TAILV:TAILV) .EQ. 'I' .OR.
     &           VRNORM(JNORM+CNTRNORM)(TAILV:TAILV) .EQ. 'J' ) ) THEN
            UNITS3D( JNORM + CNTRNORM ) = 'microgram/m**3'
          ELSE
            UNITS3D( JNORM + CNTRNORM ) = 'ppmV'
          END IF
          VDESC3D( JNORM + CNTRNORM ) = 'cgrid - isam after CHEM'
        END DO !jnorm

        CALL SUBST_BARRIER
        IF ( MYPE .EQ. 0 ) THEN
          IF ( .NOT. OPEN3( SA_RNORM_1, FSNEW3, PNAME1 ) ) THEN
            XMSG = 'Could not create '// SA_RNORM_1 // ' file'
            CALL M3EXIT ( PNAME1, SDATE3D, STIME3D, XMSG, XSTAT1 )
          END IF ! open3 new
        ENDIF !mype0

!20140418 Initial writeout    
        DO JNORM = 1, 2*CNTRNORM
          IF ( .NOT. WRITE3( SA_RNORM_1, VRNORM(JNORM), JDATE, JTIME, BUFF3 ) ) THEN
            XMSG = 'Could not write to ' // SA_RNORM_1
            CALL M3EXIT( PNAME1, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
        ENDDO ! jnorm
 
      END IF  ! firsttime


!20140416 Main loops

      DO C = 1, MY_NCOLS
        DO R = 1, MY_NROWS
          DO L = 1, NLAYS
                     
            DO JSPC = 1, NSPC_SA

              ISAMTTL = 1.E-30

              IF ( SPC_NAME(JSPC,OTHRTAG) .EQ. 'O3V' ) THEN

                CGTTL = CGRID(C,R,L,SPC_INDEX(JSPC,2))

                !20140416 isam total
                DO K = 1, NTAG_SA
                  ! O3V
                  IF (ISAM(C,R,L,JSPC,K) .LT. 0.0 ) THEN
                    ISAM(C,R,L,JSPC,K) = 0.0
                  ELSE
                    IF ( ISAM(C,R,L,JSPC,K) .NE. 0.0 ) THEN
                      ISAMTTL = ISAMTTL + ISAM(C,R,L,JSPC,K)
                    END IF ! isam not equal 0                         
                  END IF ! isam < 0
                  ! O3N
                  IF (ISAM(C,R,L,JSPC+1,K) .LT. 0.0 ) THEN
                    ISAM(C,R,L,JSPC+1,K) = 0.0
                  ELSE
                    IF ( ISAM(C,R,L,JSPC+1,K) .NE. 0.0 ) THEN
                      ISAMTTL = ISAMTTL + ISAM(C,R,L,JSPC+1,K)
                    END IF ! isam not equal 0                         
                  END IF ! isam < 0
                END DO ! k upto ntag_sa

                !20140416 renormalization
                IF ( CGTTL .LE. 0.0) THEN   !...bulk Conc is 0 so set all tags to zero
                  DO K = 1, NTAG_SA
                    ISAM(C,R,L,JSPC,K) = 0.0
                    ISAM(C,R,L,JSPC+1,K) = 0.0
                  END DO
                ELSE
                  DIFF = CGTTL - ISAMTTL   !...calculate error: term bulk conc - sum of tags            
                  IF ( ABS(DIFF) .NE. 0.0 ) THEN   !...need to increase mass of tags
                    DO K = 1, NTAG_SA
                      IF ( ISAMTTL .GT. 1.E-20 ) THEN
                        !O3V
                        ISAM(C,R,L,JSPC, K ) =
     &           ISAM(C,R,L,JSPC, K ) +
     &           DIFF * ISAM(C,R,L,JSPC,K ) / ISAMTTL
                        !O3N
                        ISAM(C,R,L,JSPC+1, K ) =
     &             ISAM(C,R,L,JSPC+1, K ) +
     &             DIFF * ISAM(C,R,L,JSPC+1,K ) / ISAMTTL
                      ELSE
                        !O3V
                        ISAM(C,R,L,JSPC, K ) = CGTTL / REAL( NTAG_SA * 2.0 )
                        !O3N
                        ISAM(C,R,L,JSPC+1, K ) = CGTTL /REAL( NTAG_SA * 2.0 )
                      ENDIF ! isamttl very small
                    END DO
                  END IF  !...ABS(DIFF)
                END IF  !...CGTTL .LE. 0.0            

                !20140416 write to output array
                IF ( LBEFORE ) THEN
                  WR_BUFFER(C, R, L, MAPRNORM(JSPC) ) = DIFF
                ELSE
                  WR_BUFFER(C, R, L, MAPRNORM(JSPC)+CNTRNORM ) = DIFF
                END IF ! lbefore true ?

              ELSEIF ( SPC_NAME(JSPC,OTHRTAG) .NE. 'O3N' ) THEN

                CGTTL = CGRID(C,R,L,SPC_INDEX(JSPC,2))
           
                !20140416 isam total
                DO K = 1, NTAG_SA
                  IF (ISAM(C,R,L,JSPC,K) .LT. 0.0 ) THEN
                    ISAM(C,R,L,JSPC,K) = 0.0
                  ELSE                            
                    IF ( ISAM(C,R,L,JSPC,K) .NE. 0.0 ) THEN
                      ISAMTTL = ISAMTTL + ISAM(C,R,L,JSPC,K)
                    END IF ! isam not equal 0                         
                  END IF ! isam < 0
                END DO ! k upto ntag_sa
          
                !20140416 renormalization
                IF ( CGTTL .LE. 0.0) THEN   !...bulk Conc is 0 so set all tags to zero
                  DO K = 1, NTAG_SA
                    ISAM(C,R,L,JSPC,K) = 0.0
                  END DO
                ELSE 
                  DIFF = CGTTL - ISAMTTL   !...calculate error: term bulk conc - sum of tags            
                  IF ( ABS(DIFF) .NE. 0.0 ) THEN   !...need to increase mass of tags
                    DO K = 1, NTAG_SA
                      IF ( ISAMTTL .GT. 1.E-20 ) THEN
                        ISAM(C,R,L,JSPC, K ) = 
     &           ISAM(C,R,L,JSPC, K ) +
     &           DIFF * ISAM(C,R,L,JSPC,K ) / ISAMTTL
                      ELSE
                        ISAM(C,R,L,JSPC, K ) = CGTTL / REAL( NTAG_SA )
                      ENDIF ! isamttl very small
                    END DO
                  END IF  !...ABS(DIFF)
                END IF  !...CGTTL .LE. 0.0            

                !20140416 write to output array
                IF ( LBEFORE ) THEN
                  WR_BUFFER(C, R, L, MAPRNORM(JSPC) ) = DIFF
                ELSE
                  WR_BUFFER(C, R, L, MAPRNORM(JSPC)+CNTRNORM ) = DIFF
                END IF ! lbefore true ?
          
              ENDIF ! spc_name o3v or anything but o3n

            END DO  ! jspc

          END DO ! L
        END DO ! R 
      END DO  ! C

cccccccccc
c check if the time is ready to write output
      IF (  LBEFORE ) THEN

        LBEFORE = .FALSE.

      ELSE

        LBEFORE = .TRUE.

        IF ( MOD( JTIME, TSTEP( 1 )  ) .EQ. 0 ) THEN
          DO JNORM = 1, 2*CNTRNORM
            DO L = 1, NLAYS
              DO R = 1, MY_NROWS
                DO C = 1, MY_NCOLS
                  BUFF3(C,R,L) = WR_BUFFER( C,R,L,JNORM )
                ENDDO !c
              ENDDO !r
            ENDDO !l
            IF ( .NOT. WRITE3( SA_RNORM_1, VRNORM(JNORM), JDATE, JTIME, BUFF3 ) ) THEN
              XMSG = 'Could not write to '
     &                // SA_RNORM_1
              CALL M3EXIT( PNAME1, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
          ENDDO ! jnorm
          WR_BUFFER = 0.0
        END IF ! MDATE

      END IF ! lbefore

      END
