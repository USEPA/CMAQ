
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CLDPROC ( CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C
C  FUNCTION:  RADM/ACM and Resolved cloud process driver
C
C  PRECONDITIONS REQUIRED:
C       Dates and times represented YYYYDDD:HHMMSS.
C
C  REVISION  HISTORY:
C      Date   Who             What
C    -------- ---             -----------------------------------------
C     11/21/00 J. Young       enable appending timesteps to wetdep1 and wetdep2
C     3/01/98 S.Roselle       modified to output 2 wet deposition files
C     8/12/97 S.Roselle       added conversion of cgrid units to mol/mol units
C     6/14/94 Dongming Hwang  configuration management prototype
C     Dec 00  J. Young        move CGRID_MAP into f90 module
C     Sep 01  J. Young        Dyn Alloc - Use HGRD_DEFN
C     Jan 05  J. Young        dyn alloc - establish both horizontal & vertical
C                             domain specifications in one module
C     May 05  J. Pleim        Replaced RADMcld with RADMacmcld
C     6/08/05 S.Roselle       added new cloud diagnostic variables
C     7/21/09 D. Wong         fixed parallel bug in opening WET_DEP_2
C     Oct 10  J.Young         convert for Namelist redesign
C     3/01/11 S.Roselle       replaced I/O API include files with UTILIO_DEFN
C     5/11/11 D.Wong:         incorporated twoway model implementation
C     02Aug12 S.Roselle:      instrumented to output transmissivity for 
C                             convective and resolved clouds
C     Aug 2015 D. Wong        Extracted section of code that deals with creating
C                             CTM_WET_DEP_1 and CTM_WET_DEP_2 and put it in opwdep.F
C                             Added a section of code to let non I/O processors
C                             open CTM_WET_DEP_1 and CTM_WET_DEP_2 for parallel I/O
C                             implementation
C     Feb 2018 D. Wong        Implemented centralized I/O approach, removed all MY_N
C                             clauses
C    Nov 2018 S.Napelenok     ISAM implementation
C    Apr 2019 F.Sidi & 
C             S.Roselle:      Fixed bug relating to incorrect unit conversions
C                             in GC, NR and TR. 
C    Dec 2019 S.Napelenok     DDM-3D implementation for v 5.3.1
C-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
      use CENTRALIZED_IO_MODULE, only : interpolate_var

#ifdef isam
      USE SA_DEFN, ONLY: ISAM, NSPC_SA, N_SPCTAG, TOT_SADEP, CONV_SADEP,
     &                   MAP_SAtoCGR, VNAM_SPCTAG, NTAG_SA, S_SPCTAG, 
     &                   ITAG, T_SPCTAG
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY: NP, NPMAX, SENGRID, SENNUM, 
     &                      S_TOTDEP, S_CONDEP, S_DEPWRT, SEN_PAR
#endif

#ifdef mpas
      use util_module, only : nextime, index1, time2sec
      use mio_module
      use coupler_module
#endif

      IMPLICIT NONE

C...........INCLUDES

      INCLUDE SUBST_CONST            ! constants
      INCLUDE SUBST_FILES_ID         ! file name parameters

      CHARACTER( 120 ) :: XMSG = ' ' ! exit message string

C...........PARAMETERS

C # of wet deposition species
      REAL, PARAMETER :: MWAIR_SI = MWAIR * 1.0E-3    ! kg mol-1
      REAL, PARAMETER :: CNV1 = MWAIR * 1.0E-9
      REAL, PARAMETER :: CNV1I = 1.0 / CNV1
      REAL, PARAMETER :: CNV2 = MWAIR_SI
      REAL, PARAMETER :: CNV2I = 1.0 / CNV2


C...........ARGUMENTS

!     REAL          CGRID( NCOLS, NROWS, NLAYS, * )  ! concentrations
!     REAL       :: CGRID( :,:,:,: )                 ! concentrations
      REAL, POINTER :: CGRID( :,:,:,: )                 ! concentrations
      INTEGER       JDATE            ! current model date, coded YYYYDDD
      INTEGER       JTIME            ! current model time, coded HHMMSS
      INTEGER       TSTEP( 3 )       ! model time step, coded HHMMSS

C...........Local Variables

      LOGICAL, SAVE :: FIRSTIME = .TRUE. ! flag for first pass thru

      CHARACTER( 16 ), SAVE :: PNAME = 'CLDPROC' ! driver program name
      CHARACTER( 16 ) :: VNAME            ! input variable name list

      INTEGER       COL              ! column subscript indices
      INTEGER       FINI             ! ending position
      INTEGER       LAY              ! layer subscript indices
      INTEGER       MDATE            ! middle of this time step
      INTEGER       MTIME            ! middle of this time step
      INTEGER       NDATE            ! middle of this time step
      INTEGER       NTIME            ! middle of this time step
      INTEGER, SAVE :: WSTEP  = 0    ! local write counter
      INTEGER, SAVE :: NNAE          ! number of #/m3 species
      INTEGER, SAVE :: NQAE          ! number of ug/m3 species
      INTEGER, SAVE :: NSAE          ! number of m2/m3 species
      INTEGER       ROW              ! row subscript indices
      INTEGER       SPC              ! species subscript indices
      INTEGER       STRT             ! starting position
      INTEGER       VAR              ! variable subscript indices
      INTEGER       ALLOCSTAT
      INTEGER       I

      REAL, ALLOCATABLE, SAVE :: CGRD   ( :,:,:,: )! concentrations
      REAL, ALLOCATABLE, SAVE :: DENS    ( :,:,: ) ! air density (kg/m3)
      REAL, ALLOCATABLE, SAVE :: FAC     ( :,:,: ) ! conversion factor
      REAL, ALLOCATABLE, SAVE :: CONV_DEP( :,:,: ) ! convective wdep only
      REAL, ALLOCATABLE, SAVE :: TOT_DEP ( :,:,: ) ! total wdep
      REAL, ALLOCATABLE, SAVE :: RESTRANS( :,: )   ! resolved cloud transmissivity
      REAL, ALLOCATABLE, SAVE :: SUBTRANS( :,:,: ) ! subgrid cloud transmissivity

      REAL CCMIN
#ifdef mpas
      CHARACTER (20) :: TIME_STAMP               ! this is for mpas
      integer :: year, month, day, hour, minute  ! this is for mpas

! this is for creating the output name list
      character (512) :: fname
      character (30), allocatable, save :: name_list(:)
      character (30) :: oname
      character (120) :: buf, buf2
      integer, save :: loc_n
      integer :: loc_nvars, stat, n, k, fnum
      logical :: found
      character (16) :: wdep_spc(300)
#endif

#ifdef isam
      REAL, ALLOCATABLE, SAVE :: BUFF2 ( :,: )
#endif

C...........EXTERNAL FUNCTIONS

      INTERFACE
         SUBROUTINE RESCLD ( CGRD, JDATE, JTIME, TSTEP,
     &                       DEP, RESTRANS )
            IMPLICIT NONE
            REAL, INTENT( IN )       :: CGRD( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE, JTIME, TSTEP( 3 )
            REAL,    INTENT( INOUT ) :: DEP( :,:,: )
            REAL,    INTENT( OUT )   :: RESTRANS( :,: )
         END SUBROUTINE RESCLD
         SUBROUTINE CONVCLD_ACM ( CGRD, JDATE, JTIME, TSTEP,
     &                            DEP, SUBTRANS )
            IMPLICIT NONE
            REAL,    INTENT( IN )    :: CGRD( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE, JTIME, TSTEP( 3 )
            REAL,    INTENT( INOUT ) :: DEP( :,:,: )
            REAL,    INTENT( OUT )   :: SUBTRANS( :,:,: )
         END SUBROUTINE CONVCLD_ACM
      END INTERFACE

C-----------------------------------------------------------------------
C   begin body of subroutine  CLDPROC

C...Initialization

      IF ( FIRSTIME ) THEN
        FIRSTIME = .FALSE.

C...first check to make sure that some species in CGRID were specified
C...for output in the wet deposition array, otherwise notify the user
C...and return
        IF ( N_SPC_WDEP .LE. 0 ) THEN

          XMSG = 'No species were specified for wet deposition ' //
     &           'tracking'
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )

          XMSG = 'ONLY CLOUD DIAGNOSTICS WILL BE WRITTEN TO THE ' //
     &           'WETDEP OUTPUT FILE!'
          CALL M3MESG ( XMSG )

        END IF

C...initialize the deposition array before processing clouds

        ALLOCATE ( TOT_DEP ( NCOLS,NROWS,N_SPC_WDEP+1 ),
     &             STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating TOT_DEP'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        TOT_DEP = 0.0   ! array assignment

        ALLOCATE ( CONV_DEP( NCOLS,NROWS,N_SPC_WDEP+8+4 ),
     &             STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating CONV_DEP'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        CONV_DEP = 0.0   ! array assignment

        ALLOCATE ( RESTRANS ( NCOLS,NROWS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating RESTRANS'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        RESTRANS = 1.0   ! array assignment

        ALLOCATE ( SUBTRANS ( NCOLS,NROWS,2 ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating SUBTRANS'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        SUBTRANS = 1.0   ! array assignment

        ALLOCATE ( DENS( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating DENS'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        
        ALLOCATE ( FAC( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating FAC conversion factor'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        
        ALLOCATE ( CGRD( NCOLS,NROWS,NLAYS,N_CGRID_SPC ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating CGRD'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

#ifdef isam
Ckrt deposition arrays for tagged species.....
        ALLOCATE ( TOT_SADEP ( NCOLS,NROWS,NSPC_SA,NTAG_SA ),
     &            CONV_SADEP ( NCOLS,NROWS,NSPC_SA,NTAG_SA ),
     &             STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating TOT_ or CONV_SADEP'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        TOT_SADEP = 0.0
        CONV_SADEP = 0.0
        ALLOCATE( BUFF2( NCOLS, NROWS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating BUFF2'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
#endif

#ifdef sens
         ALLOCATE ( S_TOTDEP ( NCOLS,NROWS,NPMAX,N_SPC_WDEP+1 ),
     &             STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating S_TOTDEP'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         S_TOTDEP = 0.0

         ALLOCATE ( S_CONDEP( NCOLS,NROWS,NPMAX,N_SPC_WDEP+1 ),
     &             STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating S_CONDEP'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         S_CONDEP = 0.0

         ALLOCATE ( S_DEPWRT ( NCOLS,NROWS, ( N_SPC_WDEP + 1 ) * NPMAX ),
     &             STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating S_DEPWRT'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         S_DEPWRT = 0.0
#endif

#ifdef parallel_io
         IF ( .NOT. IO_PE_INCLUSIVE ) THEN
            IF ( .NOT. OPEN3( CTM_WET_DEP_1, FSREAD3, PNAME ) ) THEN
               XMSG = 'Could not open ' // TRIM( CTM_WET_DEP_1 )
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF
         IF ( CLD_DIAG ) THEN
            IF ( .NOT. IO_PE_INCLUSIVE ) THEN
               IF ( .NOT. OPEN3( CTM_WET_DEP_2, FSREAD3, PNAME ) ) THEN
                  XMSG = 'Could not open ' // TRIM( CTM_WET_DEP_2 )
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            END IF
         END IF
#endif

#ifdef mpas
! this is for creating the output name list
         found = .false.
         fnum = 0
         do while ((.not. found) .and. (fnum < mio_outfile_def_info%num_of_file_definitions))
            fnum = fnum + 1
            if ('CTM_OUT' == mio_outfile_def_info%flist(fnum)%fname) then
               found = .true.
            end if
         end do
         if (.not. found) then
            write (logdev, *) ' Abort: file CTM_OUT not on the file_input.txt'
            stop
         end if

         loc_nvars = mio_outfile_def_info%flist(fnum)%nvars

         loc_n = 0
         if (found) then
            allocate (name_list(loc_nvars), stat=stat)
            do n = 1, loc_nvars
               buf = mio_outfile_def_info%flist(fnum)%vlist(n)
               found = .false.
               k = 0
               do while (.not. found)
                  k = k + 1
                  if (buf(k:k) == ' ') then
                     found = .true.
                  end if
               end do
               if (buf(1:3) == 'WD_') then
                  loc_n = loc_n + 1
                  name_list(loc_n) = buf(4:k-1)
               end if
            end do
         end if


         wdep_spc = ' '
         SPC = 0
         STRT = 1
         FINI = N_GC_WDEP
         DO VAR = STRT, FINI
            SPC = SPC + 1
            wdep_spc( VAR ) = GC_WDEP( SPC )
         END DO

         SPC = 0
         STRT = N_GC_WDEP + 1
         FINI = N_GC_WDEP + N_AE_WDEP
         DO VAR = STRT, FINI
            SPC = SPC + 1
            wdep_spc ( VAR ) = AE_WDEP( SPC )
         END DO

         SPC = 0
         STRT = N_GC_WDEP + N_AE_WDEP + 1
         FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP
         DO VAR = STRT, FINI
            SPC = SPC + 1
            wdep_spc ( VAR ) = NR_WDEP( SPC )
         END DO

         SPC = 0
         STRT = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + 1
         FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP
         DO VAR = STRT, FINI
            SPC = SPC + 1
            wdep_spc ( VAR ) = TR_WDEP( SPC )
         END DO
#endif
      END IF   ! FIRSTIME

      MDATE = JDATE
      MTIME = JTIME

      call interpolate_var ('DENS', mdate, mtime, DENS)  ! kg m-3
      CGRD = CGRID

C...convert units on cgrid and pass data to local cgrd array
      DO LAY = 1, NLAYS
        DO I = 1, N_CGRID_SPC

           ! Gas - Convert to molar mixing ratio
           IF ( CGRID_MASK_GAS( I ) .OR.
     &          CGRID_MASK_NR( I ) .OR.
     &          CGRID_MASK_TRAC( I ) ) THEN
             CGRD( :, :, LAY, I ) = MAX( CGRID( :, :, LAY, I ) * 1.0E-6,  1.0E-36 )
#ifdef sens
             DO ROW = 1,NROWS
               DO COL = 1,NCOLS
                 IF ( CGRD( COL,ROW, LAY, I ) .LE. 1.0E-30 ) THEN
                   SENGRID( COL,ROW,LAY,:,I ) = 0.0
                 ELSE
                   SENGRID( COL,ROW,LAY,:,I ) = SENGRID( COL,ROW,LAY,:,I ) * 1.0E-6
                 END IF
               END DO
             END DO
#endif
           END IF

           ! Aerosol Mass: ug m-3 -> mol / mol air
           IF ( CGRID_MASK_AERO( I ) .AND.
     &           .NOT. CGRID_MASK_NUM( I ) .AND.
     &           .NOT. CGRID_MASK_SRF( I )  ) THEN
             FAC( :,:,LAY ) = CNV1 / DENS( :,:,LAY) / CGRID_MW( I )
             CGRD( :,:,LAY,I ) = FAC(:,:,LAY) * MAX( CGRID( :,:,LAY,I ),1.0E-30/FAC(:,:,LAY) )
#ifdef sens
             DO ROW = 1,NROWS
               DO COL = 1,NCOLS
                 IF ( CGRD( COL,ROW,LAY,I ) .LE. 1.0E-30 ) THEN
                   SENGRID( COL,ROW,LAY,:,I ) = 0.0
                 ELSE
                   SENGRID( COL,ROW,LAY,:,I ) = FAC(COL,ROW,LAY) *
     &                                          SENGRID( COL,ROW,LAY,:,I )
                 ENDIF
               END DO
             END DO
#endif
           END IF

           ! Aerosol Number: N m-3 -> N / mol air
           ! Aerosol Surface Area: m2 m-3 -> m2 / mol air
           IF ( CGRID_MASK_NUM( I ) .OR.
     &          CGRID_MASK_SRF( I )  ) THEN
             FAC(:,:,LAY) = CNV2 / DENS(:,:,LAY)
             CGRD( :,:,LAY,I) = FAC(:,:,LAY) * MAX( CGRID( :,:,LAY, I ),1.0E-30/FAC(:,:,LAY) )
#ifdef sens
             DO ROW = 1,NROWS
               DO COL = 1,NCOLS
                 IF ( CGRD( COL,ROW,LAY,I ) .LE. 1.0E-30 ) THEN
                    SENGRID( COL,ROW,LAY,:,I ) = 0.0
                 ELSE
                    SENGRID( COL,ROW,LAY,:,I ) = FAC(COL,ROW,LAY) * 
     &                                            SENGRID( COL,ROW,LAY,:,I )
                 ENDIF
               END DO
             END DO
#endif
           END IF

        END DO
      END DO

#ifdef isam
c ISAM  unit conversion
      DO SPC = 1, NSPC_SA
        VAR = MAP_SAtoCGR(SPC)

        IF ( VAR .EQ. RHOJ_LOC  ) CYCLE    ! RHOJ 

        IF ( VAR .LE. N_GC_SPC .OR. VAR .GE. NR_STRT ) THEN ! this is not an aerosol
          DO ITAG = 1, NTAG_SA
            DO LAY = 1, NLAYS
              DO ROW = 1, NROWS
                DO COL = 1, NCOLS
                  ISAM( COL,ROW,LAY,SPC, ITAG ) = ISAM(COL,ROW,LAY,SPC,ITAG ) * 1.0E-6
                  ISAM( COL,ROW,LAY,SPC, ITAG ) = MAX ( ISAM( COL,ROW,LAY,SPC, ITAG ), 1.0E-30 )
c                 IF ( ISAM( COL,ROW,LAY,SPC, ITAG ) .LT. 1.0E-10 ) THEN
c                   ISAM( COL,ROW,LAY,SPC, ITAG ) = 0.0
c                 ENDIF
                END DO
              END DO
            END DO
          END DO
        ELSE ! this is an aerosol
          FAC(:,:,:) = CNV1 / DENS(:,:,:) / CGRID_MW( VAR ) 
          DO ITAG = 1, NTAG_SA
            DO LAY = 1, NLAYS
              DO ROW = 1, NROWS
                DO COL = 1, NCOLS
                  ISAM( COL,ROW,LAY,SPC, ITAG ) = ISAM( COL,ROW,LAY,SPC,ITAG ) * FAC(COL,ROW,LAY)
                  ISAM( COL,ROW,LAY,SPC, ITAG ) = MAX ( ISAM(COL,ROW,LAY,SPC, ITAG ), 1.0E-30 )
c                 IF ( ISAM( COL,ROW,LAY,SPC, ITAG ) .LT. 1.0E-10 ) THEN
c                   ISAM( COL,ROW,LAY,SPC, ITAG ) = 0.0
c                 ENDIF
                END DO
              END DO
            END DO
          END DO
        ENDIF
      END DO
#endif

C...compute cloud effects for the resolved clouds and resolved rainwater

      CALL RESCLD ( CGRD, JDATE, JTIME, TSTEP,
     &              TOT_DEP, RESTRANS )

C...compute cloud effects for convective (subgrid) clouds

      CALL CONVCLD_ACM ( CGRD, JDATE, JTIME, TSTEP,
     &                   CONV_DEP, SUBTRANS )

C...now check to see if it's time to write the deposition file
      WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
      IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN
        NDATE = JDATE
        NTIME = JTIME
        CALL NEXTIME( NDATE, NTIME, TSTEP( 2 ) )
        WSTEP = 0

        DO VAR = 1, N_SPC_WDEP + 1
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              TOT_DEP( COL, ROW, VAR ) = TOT_DEP ( COL, ROW, VAR )
     &                                 + CONV_DEP( COL, ROW, VAR )
#ifdef sens
              DO NP = 1, NPMAX
                 S_TOTDEP( COL, ROW, NP, VAR ) =
     &                 S_TOTDEP( COL, ROW, NP, VAR )
     &               + S_CONDEP( COL, ROW, NP, VAR )
              END DO
#endif
            END DO
          END DO
        END DO

        DO ROW = 1, NROWS
          DO COL = 1, NCOLS
            VAR = N_SPC_WDEP + 8 + 1
            CONV_DEP( COL, ROW, VAR ) = SUBTRANS( COL, ROW, 1 )
            VAR = VAR + 1
            CONV_DEP( COL, ROW, VAR ) = SUBTRANS( COL, ROW, 2 )
            VAR = VAR + 1
            CONV_DEP( COL, ROW, VAR ) = RESTRANS( COL, ROW )
            VAR = VAR + 1
            CONV_DEP( COL, ROW, VAR ) = SUBTRANS( COL, ROW, 1 )
     &                                * SUBTRANS( COL, ROW, 2 )
     &                                * RESTRANS( COL, ROW )
          END DO
        END DO

        RESTRANS = 1.0
        SUBTRANS = 1.0

#ifdef isam
Ckrt Sum up the wetdep from both resolved and convective clouds
Ckrt  for the tagged species as well....
        DO SPC = 1, N_SPCTAG
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              TOT_SADEP( COL,ROW,S_SPCTAG( SPC ),T_SPCTAG( SPC ) ) =
     &        TOT_SADEP( COL,ROW,S_SPCTAG( SPC ),T_SPCTAG( SPC ) ) +
     &       CONV_SADEP( COL,ROW,S_SPCTAG( SPC ),T_SPCTAG( SPC ) )
            END DO ! col
          END DO ! row
        END DO ! n_spctag
#endif

#ifdef mpas

         if ((loc_n > 0) .and. mpas_diag) then
            time_stamp = ctm_out_clock

            DO VAR = 1, N_SPC_WDEP
               k = index1( WDEP_SPC( VAR ), loc_n, name_list)
               if (k .gt. 0) then
                  oname = 'WD_' // WDEP_SPC( VAR )
                  call mio_fwrite ('CTM_OUT', oname, pname, TOT_DEP ( :,1, VAR ), TIME_STAMP)
               end if
            ENDDO
         end if

#else

C...write data to the normal wet deposition file

        IF ( .NOT. WRITE3( CTM_WET_DEP_1, ALLVAR3, NDATE,
     &                     NTIME, TOT_DEP ) ) THEN
          XMSG = 'Could not write ' // CTM_WET_DEP_1 // ' file'
          CALL M3EXIT ( PNAME, NDATE, NTIME, XMSG, XSTAT1 )
        END IF
#endif

        WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &         'Timestep written to', CTM_WET_DEP_1,
     &         'for date and time', NDATE, NTIME

#ifdef sens
C...write data to the sensitivity wet deposition file
c gas species
        DO NP = 1, NPMAX
          DO SPC = 1, ( N_SPC_WDEP + 1 ) 
            SENNUM = ( SPC - 1 ) * NPMAX + NP
            S_DEPWRT(:,:,SENNUM) = S_TOTDEP( :,:,NP,SPC )
          END DO
        END DO

        IF ( .NOT. WRITE3( CTM_SWETDEP_1, ALLVAR3, NDATE,
     &                     NTIME, S_DEPWRT ) ) THEN
          XMSG = 'Could not write ' // CTM_SWETDEP_1 // ' file'
          CALL M3EXIT ( PNAME, NDATE, NTIME, XMSG, XSTAT1 )
        END IF

        WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6)' )
     &         'Timestep written to', CTM_SWETDEP_1,
     &         'for date and time', NDATE, NTIME

#endif


C...write data to the diagnostic file if requested by the user

        IF ( CLD_DIAG ) THEN
#ifdef mpas
           if ((loc_n > 0) .and. mpas_diag) then
              call mio_setfile (CTM_WET_DEP_2)

              do spc = 1, mio_file_data(mio_cfile)%nvars
                 VNAME = mio_file_data(mio_cfile)%var_name(spc)
                 call mio_fwrite (CTM_WET_DEP_2, VNAME, pname, real(CONV_DEP(:,1,spc), 4), TIME_STAMP)
              end do
           end if
#else

          IF ( .NOT. WRITE3( CTM_WET_DEP_2, ALLVAR3, NDATE,
     &                       NTIME, CONV_DEP ) ) THEN
          XMSG = 'Could not write ' // CTM_WET_DEP_2 // ' file'
            CALL M3EXIT ( PNAME, NDATE, NTIME, XMSG, XSTAT1 )
          END IF
#endif

          WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &           'Timestep written to', CTM_WET_DEP_2,
     &           'for date and time', NDATE, NTIME

        END IF   ! CLD_DIAG

C...reinitialize deposition array

        TOT_DEP  = 0.0
        CONV_DEP = 0.0

#ifdef sens
        S_TOTDEP = 0.0
        S_CONDEP = 0.0
#endif

#ifdef isam
        BUFF2 = 0.0
        DO SPC = 1, N_SPCTAG
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              BUFF2( COL,ROW ) = TOT_SADEP( COL,ROW,S_SPCTAG( SPC ),T_SPCTAG( SPC ) )
            END DO ! col
          END DO ! row
          IF ( .NOT. WRITE3( SA_WD_1, VNAM_SPCTAG( SPC ),
     &                       NDATE, NTIME, BUFF2 ) ) THEN
            XMSG = 'Could not write ' // SA_WD_1 // ' file'
            CALL M3EXIT ( PNAME, NDATE, NTIME, XMSG, XSTAT1 )
          END IF
       END DO ! n_spctag

       TOT_SADEP  = 0.0
       CONV_SADEP = 0.0
#endif

      END IF   ! time to write

C...convert units on cgrid back to original units
C...  convert to ppmV gas

      DO LAY = 1, NLAYS
        DO I = 1,N_CGRID_SPC
           ! Gas - Convert to PPMV
           IF ( CGRID_MASK_GAS( I ) .OR.
     &          CGRID_MASK_NR( I ) .OR.
     &          CGRID_MASK_TRAC( I ) ) THEN
             CGRID( :, :, LAY, I ) = CGRD( :, :, LAY, I ) * 1.0E+6
#ifdef sens
             DO NP = 1, NPMAX
               SENGRID( :,:,LAY,NP,I ) = SENGRID( :,:,LAY,NP,I ) * 1.0E+6
             END DO
#endif
           END IF

           ! Aerosol Mass: mol / mol air -> ug m-3 
           IF ( CGRID_MASK_AERO( I ) .AND.
     &           .NOT. CGRID_MASK_NUM( I ) .AND.
     &           .NOT. CGRID_MASK_SRF( I )  ) THEN
            FAC(:,:,LAY) = CNV1I * DENS( :,:,LAY )
            CGRID( :,:,LAY,I ) = FAC(:,:,LAY) * CGRD( :,:,LAY,I ) * CGRID_MW(I)
#ifdef sens
             DO NP = 1, NPMAX
               SENGRID( :,:,LAY,NP,I ) = FAC(:,:,LAY) * SENGRID( :,:,LAY,NP,I ) * CGRID_MW(I)
             END DO
#endif
           END IF

           ! Aerosol Number: N / mol air -> N m-3
           ! Aerosol Surface Area: m2 / mol air -> m2 m-3
           IF ( CGRID_MASK_NUM( I ) .OR.
     &          CGRID_MASK_SRF( I )  ) THEN
             FAC(:,:,LAY) = CNV2I * DENS( :,:,LAY )
             CGRID( :,:,LAY,I ) = FAC(:,:,LAY) * CGRD( :,:,LAY,I )
#ifdef sens
             DO NP = 1, NPMAX
               SENGRID( :,:,LAY,NP,I ) = FAC(:,:,LAY) * SENGRID( :,:,LAY,NP,I )
             END DO
#endif
           END IF

        END DO
      END DO

#ifdef isam
c ISAM  unit conversion
      DO SPC = 1, NSPC_SA
        VAR = MAP_SAtoCGR(SPC)

        IF ( VAR .EQ. RHOJ_LOC ) CYCLE    ! RHOJ 

        IF ( .NOT. CGRID_MASK_AERO( VAR ) ) THEN ! this is not an aerosol
          DO ITAG = 1, NTAG_SA
            DO LAY = 1, NLAYS
              DO ROW = 1, NROWS
                DO COL = 1, NCOLS
                  ISAM( COL,ROW,LAY,SPC, ITAG ) = ISAM( COL,ROW,LAY,SPC,ITAG ) * 1.0E+6
                  IF ( ISAM( COL,ROW,LAY,SPC, ITAG ) .LT. 1.0E-18 ) THEN
                    ISAM( COL,ROW,LAY,SPC, ITAG ) = 0.0
                  END IF
                END DO
              END DO
            END DO
          END DO
        ELSE ! this is an aerosol
          DO ITAG = 1, NTAG_SA
            FAC(:,:,:) = CNV1I * DENS( :,:,: ) * CGRID_MW( VAR )
            DO LAY = 1, NLAYS
              DO ROW = 1, NROWS
                DO COL = 1, NCOLS
                  ISAM( COL,ROW,LAY,SPC,ITAG ) = ISAM( COL,ROW,LAY,SPC,ITAG ) * 
     &                                           FAC( COL,ROW,LAY )
                  IF ( ISAM( COL,ROW,LAY,SPC, ITAG ) .LT. 1.0E-18 ) THEN
                    ISAM( COL,ROW,LAY,SPC, ITAG ) = 0.0
                  END IF
                END DO
              END DO
            END DO
          END DO
        ENDIF
      END DO
#endif

      RETURN

      END
