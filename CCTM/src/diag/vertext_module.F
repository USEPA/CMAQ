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

       MODULE VERTEXT_MODULE

         USE STD_CONC, ONLY: N_C_GC_SPC, C_GC_SPC, N_C_AE_SPC, C_AE_SPC,  
     &                       N_C_NR_SPC, C_NR_SPC, N_C_TR_SPC, C_TR_SPC,
     &                       N_CSPCS, L_CONC_RH, L_CONC_TA,
     &                       L_CONC_PRES

         INTEGER              :: NVERTEXT
         INTEGER, ALLOCATABLE :: VERTEXTJ(:), VERTEXTI(:), VERT_SEQ(:)
 
         INTEGER                      :: NVERTEXT_SPCS = 0
         CHARACTER( 16 ), ALLOCATABLE :: VERTEXT_SPCS( : )

! for parallel implementation
         INTEGER              :: MY_NVERTEXT
         INTEGER, ALLOCATABLE :: MY_VERTEXTJ(:), MY_VERTEXTI(:), PE_NVERTEXT(:,:)

#ifndef mpas
         PRIVATE :: PWRVEC
#endif

         CONTAINS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE OPVEXT ( JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function:
C   Create the IO/API netCDF header and open the output VEXT file

C Revision history:
C   24 Nov 17 B.Henderson: copied from OPCONC.
C   09 Jan 18 D.Wong: put in parallel implementation
C   01 Feb 19 D.Wong: removed USE ALLOC_DATA_MODULE clause
C   09 Sep 19 F. Sidi: Replaced L_CONC_WVEL & L_ACONC_WVEL (depreciated) with W_VEL
C-----------------------------------------------------------------------

         USE GRID_CONF
         USE RUNTIME_VARS, ONLY: LOGDEV, LVEXT, N_CONC_VARS, W_VEL
         USE UTILIO_DEFN
#ifndef mpas
#ifdef parallel
         USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
         USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif

         IMPLICIT NONE
C        INTERFACE
C            SUBROUTINE GET_LONLAT(NVERT, VERTLAT, VERTLON)
C             REAL, ALLOCATABLE, INTENT(INOUT) :: VERTLAT(:), VERTLON(:)
C             INTEGER, INTENT(INOUT) :: NVERT
C            END SUBROUTINE GET_LONLAT
C        END INTERFACE

         INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

         INTEGER, INTENT(IN) :: JDATE        ! starting date (YYYYDDD)
         INTEGER, INTENT(IN) :: JTIME        ! starting time (HHMMSS)
         INTEGER, INTENT(IN) :: TSTEP        ! output timestep (HHMMSS)

C Local Variables:

         CHARACTER( 16 ) :: PNAME = 'OPVEXT'
         CHARACTER( 96 ) :: XMSG = ' '
         CHARACTER( 28 ) :: SSTR = ' species saved to VEXTfile:'

C environment variable for no. of layers from bottom to save on VEXT file
         CHARACTER( 16 ) :: NLAYS_VEXT = 'NLAYS_VEXT'

C  environment variable description
         CHARACTER( 80 ) :: VARDESC

         INTEGER :: K, KD, L, SPC, V, VAR   ! loop counters
         INTEGER :: STRT, FINI         ! loop counters
         INTEGER :: STATUS
         INTEGER :: GDTYP2 = 1
         REAL, ALLOCATABLE  :: VERTEXTLAT(:), VERTEXTLON(:)
         REAL, ALLOCATABLE :: VERTEXTJR(:), VERTEXTIR(:)

         LOGICAL, EXTERNAL :: FLUSH3
         CHARACTER( 16 ) :: DOVERTNAME = 'DOVERTEXT'
         CHARACTER( 16 ) :: VERTEXTLATNAME = 'VERTEXTLAT'
         CHARACTER( 16 ) :: VERTEXTLONNAME = 'VERTEXTLON'

         INTEGER :: PE
         LOGICAL :: FOUND, PRJSET

         CHARACTER (30) :: MYFMT

C-----------------------------------------------------------------------

#ifndef mpas
         IF (.not. LVEXT) RETURN
         CALL GET_LONLAT(NVERTEXT, VERTEXTLON, VERTEXTLAT)

         ALLOCATE ( 
     &              VERTEXTJR(NVERTEXT), 
     &              VERTEXTIR(NVERTEXT), 
     &              VERTEXTJ(NVERTEXT), 
     &              VERTEXTI(NVERTEXT), 
     &              MY_VERTEXTJ(NVERTEXT), 
     &              MY_VERTEXTI(NVERTEXT), 
     &              PE_NVERTEXT(0:NVERTEXT, 0:NPCOL*NPROW-1), 
     &              VERT_SEQ(NVERTEXT), 
     &              STAT = STATUS)

         WRITE(LOGDEV, *)'IN', NVERTEXT

C Set output file characteristics from GRID_CONF and open 

         FTYPE3D = GRDDED3
         SDATE3D = JDATE
         STIME3D = JTIME + TSTEP
         TSTEP3D = TSTEP
         NVARS3D = N_CONC_VARS
         NCOLS3D = 1
         NROWS3D = NVERTEXT
         NLAYS3D = NLAYS
         NTHIK3D = 1
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
         DO L = 1, NLAYS + 1
            VGLVS3D( L ) = VGLVS_GD( L )
         END DO
         GDNAM3D = GRID_NAME  ! from HGRD_DEFN

         FDESC3D( 1 ) = 'Concentration file output'
         FDESC3D( 2 ) = 'From CMAQ model dyn alloc version CTM'
         FDESC3D( 3 ) = 'Set of variables (possibly) reduced from CGRID'
         FDESC3D( 4 ) = 'For next scenario continuation runs,'
         FDESC3D( 5 ) = 'use the "one-step" CGRID file'
         KD = 5
         L = 0
         DO K = KD + 1, MIN ( NLAYS + KD, MXDESC3 )
            L = L + 1
            WRITE( FDESC3D( K ),'( "Layer", I3, " to", I3, " " )' )
     &      L - 1, L
         END DO
!        IF ( ( KD + 1 + L ) .LT. MXDESC3 ) THEN
!           DO K = KD + 1 + L, MXDESC3
!              FDESC3D( K ) = ' '
!           END DO
!        END IF
         WRITE (MYFMT, '(a10, i3, a6)') '(A16, 1X, ', NVERTEXT, 'f12.6)'
         WRITE (FDESC3D(KD + 2 + L), MYFMT) 'Latitude  list: ', VERTEXTLAT(1:NVERTEXT)
         WRITE (FDESC3D(KD + 3 + L), MYFMT) 'Longitude list: ', VERTEXTLON(1:NVERTEXT)
         CALL LONLAT2XY(NVERTEXT, VERTEXTLON, VERTEXTLAT,
     &                  VERTEXTIR, VERTEXTJR)
         WRITE (MYFMT, '(a10, i3, a6)') '(A16, 1X, ', NVERTEXT, 'f12.6)'
         WRITE (FDESC3D(KD + 4 + L), myfmt) 'Y list: ', VERTEXTJR(1:NVERTEXT)
         WRITE (FDESC3D(KD + 5 + L), myfmt) 'X list: ', VERTEXTIR(1:NVERTEXT)
         
         
         STATUS = 0
         MY_NVERTEXT = 0
         PE_NVERTEXT = 0
         VERT_SEQ = -1
         DO V = 1, NVERTEXT
            VERTEXTI(V) = INT((VERTEXTIR(V) - XORIG3D) / XCELL3D) + 1
            VERTEXTJ(V) = INT((VERTEXTJR(V) - YORIG3D) / YCELL3D) + 1
            IF ((VERTEXTI(V) .gt. GL_NCOLS) .or. (VERTEXTI(V) .lt. 1) .or.
     &          (VERTEXTJ(V) .gt. GL_NROWS) .or. (VERTEXTJ(V) .lt. 1)) THEN
               STATUS = STATUS + 1
               WRITE( LOGDEV, '( /5X, A, F10.4, F10.4, A, I8, I8)' )
     &              'Outside domain', VERTEXTLON(V), VERTEXTLAT(V),
     &              'COL/ROW', VERTEXTI(V), VERTEXTJ(V)
            ELSE 
               FOUND = .FALSE.
               PE = 0
               DO WHILE (.NOT. FOUND)
                  PE = PE + 1
                  IF ((VERTEXTI(V) .GE. COLSX_PE(1, PE)) .and. 
     &                (VERTEXTI(V) .LE. COLSX_PE(2, PE)) .and.
     &                (VERTEXTJ(V) .GE. ROWSX_PE(1, PE)) .and.
     &                (VERTEXTJ(V) .LE. ROWSX_PE(2, PE))) THEN
                      VERT_SEQ(V) = PE - 1
                      PE_NVERTEXT(0, PE-1) = PE_NVERTEXT(0, PE-1) + 1
                      K = PE_NVERTEXT(0, PE-1)
                      PE_NVERTEXT(K, PE-1) = V
                      FOUND = .TRUE.
                  END IF
               END DO
               IF (MYPE .EQ. VERT_SEQ(V)) THEN 
                  MY_NVERTEXT = MY_NVERTEXT + 1
                  MY_VERTEXTI(MY_NVERTEXT) = VERTEXTI(V) - COLSX_PE(1, MYPE+1) + 1
                  MY_VERTEXTJ(MY_NVERTEXT) = VERTEXTJ(V) - ROWSX_PE(1, MYPE+1) + 1

                  WRITE(LOGDEV, *)'IN', VERTEXTLON(V), VERTEXTLAT(V)
                  WRITE(LOGDEV, *) 'OUT', VERTEXTI(V), VERTEXTJ(V)
               END IF
            ENDIF
         END DO

         WRITE (MYFMT, '(a10, i3, a6)') '(A16, 1X, ', NVERTEXT, 'I6)'
         WRITE (FDESC3D(KD + 6 + L), MYFMT) 'J list: ', VERTEXTJ(1:NVERTEXT)
         WRITE (FDESC3D(KD + 7 + L), MYFMT) 'I list: ', VERTEXTI(1:NVERTEXT)
         WRITE( LOGDEV,* ) ' '
         WRITE( LOGDEV,* ) '      VEXT  File Header Description:'
         DO K = 1, KD + L
            WRITE( LOGDEV,* ) '    => ', TRIM( FDESC3D( K ) )
         END DO

         V = 0
         STRT = 1
         FINI = N_C_GC_SPC
         DO SPC = STRT, FINI
            V = V + 1
            VTYPE3D( SPC ) = M3REAL
            VNAME3D( SPC ) = C_GC_SPC( V )
            UNITS3D( SPC ) = 'ppmV'
            VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
         END DO

         V = 0
         STRT = FINI + 1
         FINI = FINI + N_C_AE_SPC
         DO SPC = STRT, FINI
            V = V + 1
            VTYPE3D( SPC ) = M3REAL
            VNAME3D( SPC ) = C_AE_SPC( V )
            IF ( VNAME3D( SPC )(1:3) .EQ. 'NUM' ) THEN
               UNITS3D( SPC ) = 'm-3'
            ELSE IF ( VNAME3D( SPC )(1:3) .EQ. 'SRF' ) THEN
               UNITS3D( SPC ) = 'm2 m-3'
            ELSE
               UNITS3D( SPC ) = 'ug m-3'
            END IF
            VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
         END DO

         V = 0
         STRT = FINI + 1
         FINI = FINI + N_C_NR_SPC
         DO SPC = STRT, FINI
            V = V + 1
            VTYPE3D( SPC ) = M3REAL
            VNAME3D( SPC ) = C_NR_SPC( V )
            UNITS3D( SPC ) = 'ppmV'
            VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
         END DO

         V = 0
         STRT = FINI + 1
         FINI = FINI + N_C_TR_SPC ! write all TR species
         DO SPC = STRT, FINI
            V = V + 1
            VTYPE3D( SPC ) = M3REAL
            VNAME3D( SPC ) = C_TR_SPC( V )
            UNITS3D( SPC ) = 'ppmV'
            VDESC3D( SPC ) = 'Molar Mixing Ratio for species ' // VNAME3D( SPC )
         END DO
 
         VAR = FINI
         IF ( W_VEL ) THEN   ! for W_YAMO
            VAR = VAR + 1
            VTYPE3D( VAR ) = M3REAL
            VNAME3D( VAR ) = 'W_VEL'
            UNITS3D( VAR ) = 'm s-1'
            VDESC3D( VAR ) = 'Derived vertical velocity component'
         END IF
      
         IF ( L_CONC_RH ) THEN   ! for Relative Humidity
            VAR = VAR + 1 
            VTYPE3D( VAR ) = M3REAL
            VNAME3D( VAR ) = 'RH'
            UNITS3D( VAR ) = '1'
            VDESC3D( VAR ) = 'Fractional Relative Humidity'
         END IF
       
         IF ( L_CONC_TA ) THEN   ! for Temperature
            VAR = VAR + 1 
            VTYPE3D( VAR ) = M3REAL
            VNAME3D( VAR ) = 'TA'
            UNITS3D( VAR ) = 'K'
            VDESC3D( VAR ) = 'Air Temperature'
         END IF
       
         IF ( L_CONC_PRES ) THEN   ! for Pressure
            VAR = VAR + 1 
            VTYPE3D( VAR ) = M3REAL
            VNAME3D( VAR ) = 'PRES'
            UNITS3D( VAR ) = 'Pa'
            VDESC3D( VAR ) = 'Air Pressure'
         END IF
 
         NVERTEXT_SPCS = NVARS3D
         ALLOCATE( VERTEXT_SPCS( NVERTEXT_SPCS ), STAT = STATUS )
         VERTEXT_SPCS( 1:NVERTEXT_SPCS ) = VNAME3D( 1:NVERTEXT_SPCS ) 

C create header

         IF ( IO_PE_INCLUSIVE ) THEN   ! open new

            IF ( .NOT. OPEN3( CTM_VEXT_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not open ' // CTM_VEXT_1
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            IF ( .NOT. FLUSH3 ( CTM_VEXT_1 ) ) THEN
               XMSG = 'Could not sync to disk ' // CTM_VEXT_1
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         END IF

         DEALLOCATE(VERTEXTLAT, VERTEXTLON, VERTEXTJR, VERTEXTIR)
#endif

         END SUBROUTINE OPVEXT

#ifndef mpas
#ifdef parallel
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         LOGICAL FUNCTION PWRVEC( FILNAME, VARNAME, DATE, TIME,
     &                            BUFFER, NLAYS,
     &                            NCOLS, NROWS, NP, 
     &                            NVERTEXT, VERTEXTII, VERTEXTJI)
C.....................................................................
  
C  PURPOSE:   Perform Models-3 file-write operation in a parallel
C             environment for a vector. Values of variable VARNAME on each processor
C             subdomain region are collected, via MPI calls, by the
C             primary I/O processor and the full grid of values are
C             written to file.

C  RETURN VALUE:  The function fails if M3IO routine WRITE3 fails. If
C       an MPI error occurs, the program is aborted with a call to
C       PM3EXIT. 
  
C  REVISION  HISTORY:
C       Copied 11/29/2017 by Barron Henderson
C          -- Copied from PWRGRDD
C   09 Jan 18 D.Wong: put in parallel implementation
  
C  ARGUMENT LIST DESCRIPTION:
C  M1 in PIOMAPS_MODULE
C  M2 in ALLOC_DATA_MODULE
C  E3 in PIOVARS.EXT
C  IN:
C    CHARACTER*(*)  FILNAME        ! Name of file containing variable VARNAME
C    CHARACTER*(*)  VARNAME        ! Name of file variable to write
C    INTEGER        DATE           ! Date, formatted YYYYDDD
C    INTEGER        TIME           ! Time, formatted HHMMSS
C    REAL           BUFFER(NCOLS, NROWS, NLAYS)
C                                  ! Buffer holding (local) array to be written
C    INTEGER  NLAYS                ! Layer dimension of file variables
C    INTEGER  NCOLS                ! Column dimension of local-processor arrays
C    INTEGER  NROWS                ! Row dimension of local-processor arrays
C    INTEGER  NP                   ! Number of processors
C    INTEGER  NVERTEXT             ! Number of Lat/Lon pairs
C    INTEGER  VERTEXTII            ! Vertice location
C    INTEGER  VERTEXTJI            ! Vertice location
 
C  OUT: none
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  CALLS: WRITE3, PM3WARN, PM3EXIT, MPI_SEND, MPI_RECV, MPI_BCAST
 
C  NOTES: (1) Only the primary I/O processor does the file writing. Input
C             arguments FILNAME, VARNAME, DATE, and TIME are meaningful
C             only to the I/O processor.
 
C         (2) This routine handles only gridded variables. The BUFFER is
C             assumed to be declared as BUFFER( NCOLS, NROWS, NLAYS ),
C             where NCOLS and NROWS are the local PE grid subdomain
C             dimensions and NLAYS is the file variable layer dimension.
C             BUFFER is assumed to be filled as BUFFER(1:C,1:R,1:NLAYS),
C             where C = WR_COLSX_PE(2,MY_PE+1), R = WR_ROWSX_PE(2,MY_PE+1).
C-----------------------------------------------------------------------

         USE PIOMAPS_MODULE
         USE M3UTILIO, ONLY : WRITE3              ! i/o api

         IMPLICIT NONE

C Include Files

         INCLUDE 'mpif.h'            ! MPI definitions and parameters
         INCLUDE 'PIOVARS.EXT'

C Arguments

         CHARACTER( * ), INTENT(IN) :: FILNAME   ! Name of file containing variable VARNAME
         CHARACTER( * ), INTENT(IN) :: VARNAME   ! Name of file variable to write
         INTEGER, INTENT(IN)        :: DATE      ! Date, formatted YYYYDDD
         INTEGER, INTENT(IN)        :: TIME      ! Time, formatted HHMMSS 
         INTEGER, INTENT(IN)        :: NLAYS     ! Layer dimension of file variables
         INTEGER, INTENT(IN)        :: NCOLS     ! Column dimension of local-processor arrays
         INTEGER, INTENT(IN)        :: NROWS     ! Row dimension of local-processor arrays
         INTEGER, INTENT(IN)        :: NP        ! Number of processors
         INTEGER, INTENT(IN)        :: NVERTEXT  ! Number of vertices
         INTEGER, INTENT(IN)        :: VERTEXTII(:),VERTEXTJI(:) ! vertex locations
         REAL, INTENT(IN)           :: BUFFER( :,:,: )           ! Buffer holding (local) array 

         REAL, ALLOCATABLE, SAVE :: WRITVBUF( :,:,: )
         REAL, ALLOCATABLE       :: RECVVBUF( :,: )
         REAL, ALLOCATABLE       :: SENDVBUF(:,:)     ! to be written

C External Functions:

         EXTERNAL PM3WARN                    ! Parallel M3IO library

C Local Variables:

         INTEGER         :: MSGSIZE   ! Message size of subgrid to receive
         INTEGER         :: IPE       ! For loop over processors
         INTEGER         :: IERROR    ! MPI error code
         LOGICAL         :: LERROR    ! LOCAL ERROR
         LOGICAL         :: RERROR    ! LOCAL MPI ALLREDUCE ERROR
         INTEGER         :: TC,TR     ! This cell position
         INTEGER         :: IG        ! Loop counter over grid layers
         CHARACTER( 16 ) :: FIL16     ! Scratch area for file-name
         CHARACTER( 16 ) :: VAR16     ! Scratch area for vble-name
         CHARACTER( 80 ) :: MSG       ! Message issued from PM3WARN routine
         INTEGER         :: STATUS( MPI_STATUS_SIZE )   ! MPI status code
         INTEGER         :: N, POS

         INTEGER, PARAMETER :: TAG1 = 901 ! MPI message tag for processor ID

C........................................................................

C Initialize return value and error code
         PWRVEC = .TRUE.
         LERROR = .FALSE.
         IERROR = 0
         
         IF ( MY_PE .EQ. IO_PE ) THEN    ! I/O processor collects and writes data

            IF (.NOT. ALLOCATED(WRITVBUF)) THEN
               ALLOCATE ( WRITVBUF  ( 1, NVERTEXT, NLAYS ), STAT = IERROR )
               IF ( IERROR .NE. 0 ) THEN
                  MSG = 'Failure allocating WRITVBUF '
                  CALL M3EXIT( 'PWRVEC', DATE, TIME, MSG, 1 )
               END IF
            END IF

C Gather the array and write it to file.

C I/O PE copies its own local array into output buffer

            DO N = 1, MY_NVERTEXT
               POS = PE_NVERTEXT(N, MY_PE)
               TC  = MY_VERTEXTI(N)
               TR  = MY_VERTEXTJ(N)

               WRITVBUF( 1, POS, : ) = BUFFER(TC, TR, :)
            END DO
 
C I/O PE receives array from all other processors and copies it to the output
C buffer. Arrays are received in a first-come-first-serve order.  

            DO IPE = 1, NP - 1

               IF (PE_NVERTEXT(0, IPE) .GT. 0) THEN
                  ALLOCATE ( RECVVBUF  ( PE_NVERTEXT(0, IPE), NLAYS ), STAT = IERROR )
                  IF ( IERROR .NE. 0 ) THEN
                     MSG = 'Failure allocating RECVVBUF '
                     CALL M3EXIT( 'PWRVEC', DATE, TIME, MSG, 1 )
                  END IF

                  MSGSIZE = PE_NVERTEXT(0, IPE) * NLAYS  

                  CALL MPI_RECV( RECVVBUF, MSGSIZE, MPI_REAL, IPE,
     &                           TAG1, MPI_COMM_WORLD, STATUS, IERROR )
                  IF ( IERROR .NE. 0 ) THEN
                     MSG = 'MPI error receiving data array RECVVBUF.'
                     CALL PM3WARN( 'PWRVEC', DATE, TIME, MSG )
                     LERROR = .TRUE.
                  END IF

                  DO N = 1, PE_NVERTEXT(0, IPE)
                     POS = PE_NVERTEXT(N, IPE)
                     TC  = MY_VERTEXTI(N)
                     TR  = MY_VERTEXTJ(N)

                     WRITVBUF( 1, POS, : ) = RECVVBUF(N,:)
                  END DO
                  DEALLOCATE (RECVVBUF)
               END IF
            END DO

C Write the accumulated array to file

            FIL16 = FILNAME
            VAR16 = VARNAME
            IF ( .NOT. WRITE3( FIL16, VAR16, DATE, TIME, WRITVBUF ) ) THEN
               MSG = 'Could not write '
     &               // TRIM( VARNAME ) //
     &               ' to file '// TRIM( FIL16 )

               CALL PM3WARN( 'PWRVEC', DATE, TIME, MSG )
               LERROR = .TRUE.
            END IF

         ELSE      ! Non-I/O processors send data

C Each processor, except for the I/O processor, sends its local array to
C the I/O processor.

            IF (MY_NVERTEXT .GT. 0) THEN
               IF (.NOT. ALLOCATED(SENDVBUF)) THEN
                  ALLOCATE ( SENDVBUF  ( MY_NVERTEXT, NLAYS   ), STAT = IERROR )
                  IF ( IERROR .NE. 0 ) THEN
                     MSG = 'Failure allocating SENDVBUF '
                     CALL M3EXIT( 'PWRVEC', DATE, TIME, MSG, 1 )
                  END IF
               END IF

               DO IG = 1, MY_NVERTEXT
                  TC = MY_VERTEXTI(IG)
                  TR = MY_VERTEXTJ(IG)

                  SENDVBUF( IG, : ) = BUFFER(TC, TR, :)
               END DO

               MSGSIZE = MY_NVERTEXT * NLAYS  

               CALL MPI_SEND( SENDVBUF, MSGSIZE, MPI_REAL, IO_PE, TAG1,
     &                        MPI_COMM_WORLD, IERROR )

               IF ( IERROR .NE. 0 ) THEN
                  MSG = 'MPI error sending data array WRITVBUF.'
                  CALL PM3WARN( 'PWRVEC', DATE, TIME, MSG )
                  LERROR = .TRUE.
               END IF
            END IF

         END IF     ! if( my_pe .eq. io_pe )

C Notify non-I/O processors of failure

         CALL MPI_ALLREDUCE( LERROR, RERROR, 1, MPI_LOGICAL, MPI_LAND, 
     &                       MPI_COMM_WORLD, IERROR )

         IF ( IERROR .NE. 0 ) THEN
            MSG = 'MPI Allreduce error.'
            CALL PM3WARN( 'PWRVEC', DATE, TIME, MSG )
            LERROR = .TRUE.
         END IF

         IF ( RERROR ) THEN
            MSG = 'Failed to write '
     &           // TRIM( VARNAME ) //
     &           ' from file '// TRIM( FILNAME )
            CALL PM3WARN( 'PWRVEC', DATE, TIME, MSG )
            PWRVEC = .FALSE.
         END IF

         END FUNCTION PWRVEC

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE WR_VEXT( CGRID, JDATE, JTIME, TSTEP )

C Revision History:
C   24 Nov 17 B.Henderson: copied from WR_CONC
C   24 Jul 18 C. Nolte: removed call to INIT_MET that was causing conflict.
C             Met should already be initialized by this point.
C   09 Sep 19 F. Sidi: Replaced L_CONC_WVEL & L_ACONC_WVEL (depreciated) with W_VEL
C-----------------------------------------------------------------------

         USE GRID_CONF             ! horizontal & vertical domain specifications
         USE RUNTIME_VARS
         USE STD_CONC              ! standard CONC
         USE PIOMAPS_MODULE
         USE UTILIO_DEFN
         USE ASX_DATA_MOD, Only: Met_Data
         USE WVEL_DEFN, Only : WVEL

         IMPLICIT NONE

C Include Files:

         INCLUDE SUBST_FILES_ID    ! I/O definitions and declarations

         REAL,    POINTER    :: CGRID( :,:,:,: )  ! Species concentrations

         INTEGER, INTENT(IN) :: JDATE             ! current model date, coded YYYYDDD
         INTEGER, INTENT(IN) :: JTIME             ! current model time, coded HHMMSS
         INTEGER, INTENT(IN) :: TSTEP             ! output timestep (HHMMSS)

C Local variables:

         CHARACTER( 16 ) :: PNAME = 'WR_VEXT'
         CHARACTER( 96 ) :: XMSG = ' '

         REAL, ALLOCATABLE :: DBUFF ( :,: )     ! input for W_VEL
         INTEGER           :: ALLOCSTAT

         LOGICAL, SAVE :: FIRSTIME = .TRUE.

C for INTERPX

         INTEGER      :: C, R, K, L, V,GI   ! loop induction variables
         INTEGER      :: C0, CN, R0, RN
         INTEGER      :: INDX, VAR
!        CHARACTER( 16 ), ALLOCATABLE, SAVE :: VNAME( : )
!        INTEGER, SAVE :: NVARS

C-----------------------------------------------------------------------
         IF ( FIRSTIME ) THEN

            FIRSTIME = .FALSE.

C open vext file for update

            IF ( .NOT. IO_PE_INCLUSIVE ) THEN
               IF ( .NOT. OPEN3( CTM_VEXT_1, FSREAD3, PNAME ) ) THEN
                  XMSG = 'Could not open ' // CTM_VEXT_1 //
     &                    ' file for update'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            END IF

!           IF ( .NOT. DESC3( CTM_VEXT_1 ) ) THEN
!              XMSG = 'Could not get file description from ' // CTM_VEXT_1
!              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!           END IF
!           NVARS = NVARS3D
!           ALLOCATE ( VNAME( NVARS ), STAT=ALLOCSTAT )
!           VNAME = VNAME3D( 1:NVARS )
!           WRITE(LOGDEV, *) 'WR_VEXT:NVARS', NVARS


            IF ( .NOT. ALLOCATED( DBUFF ) ) THEN
               ALLOCATE ( DBUFF( NVERTEXT, NLAYS ), STAT = ALLOCSTAT )
               IF ( ALLOCSTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating DBUFF'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            END IF

         END IF   ! firstime

         DO V = 1, N_CSPCS
            INDX = CONC_MAP( V )
            IF ( .NOT. PWRVEC(CTM_VEXT_1, CONC_FILE_SPCS( V ), JDATE, JTIME,
     &                        CGRID(:, :, :, INDX), NLAYS,
     &                        NCOLS, NROWS, NUMPROCS, 
     &                        NVERTEXT, VERTEXTI, VERTEXTJ     ) ) THEN
               XMSG = 'PWRGRDD failed writing variable '
     &              // TRIM( CONC_FILE_SPCS(V) ) //
     &              ' to file '// TRIM( CTM_VEXT_1 )
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            ELSE
               XMSG = 'PWRGRDD wrote variable '
     &              // TRIM( CONC_FILE_SPCS(V) ) //
     &              ' to file '// TRIM( CTM_VEXT_1 )
               WRITE( LOGDEV, '(A)') XMSG
            END IF
         END DO
 
         IF ( W_VEL ) THEN
         IF ( .NOT. PWRVEC(CTM_VEXT_1, 'W_VEL', JDATE, JTIME,
     &                     WVEL(:, :, :), NLAYS,
     &                     NCOLS, NROWS, NUMPROCS, 
     &                     NVERTEXT, VERTEXTI, VERTEXTJ     ) ) THEN
            XMSG = 'PWRGRDD failed writing variable WVEL'
     &          // ' to file '// TRIM( CTM_VEXT_1 )
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         END IF
         
         IF ( L_CONC_RH ) THEN
         IF ( .NOT. PWRVEC(CTM_VEXT_1, 'RH', JDATE, JTIME,
     &                     MET_DATA%RH(:, :, :), NLAYS,
     &                     NCOLS, NROWS, NUMPROCS, 
     &                     NVERTEXT, VERTEXTI, VERTEXTJ     ) ) THEN
            XMSG = 'PWRGRDD failed writing variable RH'
     &          // ' to file '// TRIM( CTM_VEXT_1 )
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         END IF
         
 
         IF ( L_CONC_PRES ) THEN
         IF ( .NOT. PWRVEC(CTM_VEXT_1, 'PRES', JDATE, JTIME,
     &                     Met_Data%PRES(:, :, :), NLAYS,
     &                     NCOLS, NROWS, NUMPROCS, 
     &                     NVERTEXT, VERTEXTI, VERTEXTJ     ) ) THEN
            XMSG = 'PWRGRDD failed writing variable PRES'
     &          // ' to file '// TRIM( CTM_VEXT_1 )
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         END IF
         
         IF ( L_CONC_TA ) THEN
         IF ( .NOT. PWRVEC(CTM_VEXT_1, 'TA', JDATE, JTIME,
     &                     Met_Data%TA(:, :, :), NLAYS,
     &                     NCOLS, NROWS, NUMPROCS, 
     &                     NVERTEXT, VERTEXTI, VERTEXTJ     ) ) THEN
            XMSG = 'PWRGRDD failed writing variable TA '
     &          // ' to file '// TRIM( CTM_VEXT_1 )
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         END IF

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &         'Timestep written to', CTM_VEXT_1,
     &         'for date and time', JDATE, JTIME

         END SUBROUTINE WR_VEXT
#endif
#endif
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE GET_LONLAT(NVERT, VERTLON, VERTLAT)
C-----------------------------------------------------------------------
C Function:
C   Get LON/LAT pairs from file
C Revision history:
C   09 Mar 18 B.Henderson: 
C-----------------------------------------------------------------------
         USE UTILIO_DEFN
         USE RUNTIME_VARS
         IMPLICIT NONE
         INTEGER :: STATUS, FUNIT, I
         INTEGER, INTENT(INOUT) :: NVERT
         REAL, ALLOCATABLE, INTENT(INOUT) :: VERTLAT(:), VERTLON(:)
        !  environment variable description
         CHARACTER( 80 ) :: VARDESC
         CHARACTER( 16 ) :: DEFTXT = ''
         CHARACTER( 16 ) :: PNAME = 'OPVEXT'
         CHARACTER( 96 ) :: XMSG = ' '

#ifndef mpas
         FUNIT = 100
! OPEN FORMATTED FILE
! EXPECTED FORMAT 
! REC1: N
! REC2-N+1: LON LAT
         OPEN(UNIT=FUNIT, FILE=VEXT_COORD_PATH, STATUS='OLD',
     &        FORM = 'FORMATTED', IOSTAT = STATUS)

         IF ( STATUS .NE. 0 ) THEN
            XMSG = TRIM(VEXT_COORD_PATH) //
     &             ' Could not be opened'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF
! READ NUMBER OF VERTICES
         READ(FUNIT, *, IOSTAT=STATUS) NVERT
         IF ( STATUS .NE. 0 ) THEN
            XMSG = TRIM(VEXT_COORD_PATH) //
     &             ' did not contain one integer on the first line'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF
! ALLOCATE OUTPUT VARIABLES
         ALLOCATE ( VERTLAT(NVERT), VERTLON(NVERT), STAT = STATUS )
! FOR EACH INPUT LINE READ LON,LAT
         DO I=1,NVERT
            READ(FUNIT, *, IOSTAT=STATUS) VERTLON(I), VERTLAT(I)
            IF ( STATUS .NE. 0 ) THEN
               WRITE(XMSG, '(I6)') I
               XMSG = TRIM(VEXT_COORD_PATH) //
     &                ' did not contain two reals on line: ' //
     &                TRIM(XMSG)
               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
            END IF
         ENDDO
! CLOSE FILE
         CLOSE(FUNIT)
#endif

         END SUBROUTINE GET_LONLAT

         SUBROUTINE LONLAT2XY(NVERTEXT, VERTEXTLON, VERTEXTLAT,
     &                        VERTEXTIR, VERTEXTJR)
         USE UTILIO_DEFN
         USE GRID_CONF             ! horizontal & vertical domain specifications
         IMPLICIT NONE
         INTEGER, INTENT(in) :: NVERTEXT
         REAL, INTENT(in) :: VERTEXTLON(NVERTEXT), VERTEXTLAT(NVERTEXT)
         REAL, INTENT(inout) :: VERTEXTIR(NVERTEXT), VERTEXTJR(NVERTEXT)
         CHARACTER( 16 ) :: PNAME = 'LONLAT2XY'
         CHARACTER( 96 ) :: XMSG = ' '
         LOGICAL :: PRJSET, LSTAT
         REAL :: JR, IR
         INTEGER :: V
!         IOAPI 3.2 has an explicit function, but many users are likely
!         using 3.1, so I basically rewrote LL2XY
!         CALL XY2XY( GDTYP_GD, P_ALP_GD, P_BET_GD, P_GAM_GD, XCENT_GD, YCENT_GD,
!     &               1      ,    0.d0,    0.d0,    0.d0,    0.d0,    0.d0,
!     &               NVERTEXT, VERTEXTLON, VERTEXTLAT, VERTEXTIR, VERTEXTJR )
#ifndef mpas
         IF ( GDTYP3D .EQ. LAMGRD3 ) THEN
             PRJSET = SETLAM( REAL(P_ALP_GD), REAL(P_BET_GD), REAL(P_GAM_GD),
     &                         REAL(XCENT_GD), REAL(YCENT_GD))
             DO V=1,NVERTEXT
                 LSTAT = LL2LAM( VERTEXTLON(V), VERTEXTLAT(V), IR, JR ) 
                 VERTEXTIR(V) = IR
                 VERTEXTJR(V) = JR
             ENDDO
         ELSEIF ( GDTYP_GD .EQ. POLGRD3 ) THEN
             PRJSET = SETPOL( REAL(P_ALP_GD), REAL(P_BET_GD), REAL(P_GAM_GD),
     &                         REAL(XCENT_GD), REAL(YCENT_GD))
             DO V=1,NVERTEXT
                 LSTAT = LL2POL( VERTEXTLON(V), VERTEXTLAT(V), IR, JR ) 
                 VERTEXTIR(V) = IR
                 VERTEXTJR(V) = JR
             ENDDO
         ELSEIF ( GDTYP_GD .EQ. EQMGRD3 ) THEN
             PRJSET = SETEQM( REAL(P_ALP_GD), REAL(P_BET_GD), REAL(P_GAM_GD),
     &                         REAL(XCENT_GD), REAL(YCENT_GD))
             DO V=1,NVERTEXT
                 LSTAT = LL2EQM( VERTEXTLON(V), VERTEXTLAT(V), IR, JR ) 
                 VERTEXTIR(V) = IR
                 VERTEXTJR(V) = JR
             ENDDO
         ELSEIF ( GDTYP_GD .EQ. TRMGRD3 ) THEN
             PRJSET = SETTRM( REAL(P_ALP_GD), REAL(P_BET_GD), REAL(P_GAM_GD),
     &                         REAL(XCENT_GD), REAL(YCENT_GD))
             DO V=1,NVERTEXT
                 LSTAT =  LL2TRM( VERTEXTLON(V), VERTEXTLAT(V), IR, JR )
                 VERTEXTIR(V) = IR
                 VERTEXTJR(V) = JR
             ENDDO
         ELSEIF ( GDTYP_GD .EQ. ALBGRD3 ) THEN
             PRJSET = SETALB( REAL(P_ALP_GD), REAL(P_BET_GD), REAL(P_GAM_GD),
     &                         REAL(XCENT_GD), REAL(YCENT_GD))
             DO V=1,NVERTEXT
                 LSTAT = LL2ALB( VERTEXTLON(V), VERTEXTLAT(V), IR, JR )
                 VERTEXTIR(V) = IR
                 VERTEXTJR(V) = JR
             ENDDO
         ELSE
             XMSG = 'Unsupported coordinate system type'
             CALL M3EXIT(PNAME, 0, 0, XMSG, XSTAT2)
         END IF
#endif

         END SUBROUTINE LONLAT2XY

       END MODULE VERTEXT_MODULE
