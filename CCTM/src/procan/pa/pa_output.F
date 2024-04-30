
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
       SUBROUTINE PA_OUTPUT( CGRID, JDATE, JTIME )

C-----------------------------------------------------------------------
C Function: Output the Process Analysis and/or Integrated Reaction Rate data           
 
C Preconditions: None
 
C Key Subroutines/Functions Called: M3EXIT
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  allow env var for file names Jeff, Dec, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Modified 1/19/99 by David Wong at LM:
C        -- add four include files because of new PA_CMN.EXT
C        -- add DATA_COPY function call to redistribute PA grid
C  Modified 2/26/99 by David Wong at LM:
C        -- remove SUBST_AE_SPC, SUBST_NR_SPC, SUBST_TR_SPC,
C           three .EXT files
C        -- replaced DATA_COPY function with dimension specific
C           DATA_COPY function and modified its argument list
C        -- used ifdef statement to distinguish parallel
C           implementation of IRR calculation which does not
C           start at the origin
C  Modified 22 Nov 00 by J.Young: Dave Wong's f90 stenex DATA_COPY
C  30 Mar 01 J.Young: dyn alloc - Use PAGRD_DEFN, which Uses
C                                 HGRD_DEFN; requires DBUFF for WRITE3
C  10 Oct 01 David Wong
C        -- use DBUFF to hold the output data for IRR data since
C           parallel WRITE3 can't output subsection of data
C        -- removed the usage of DBUFF for PA output
C        -- used a new formula to compute number of indexes in
C           each IPR and IRR output file rather than file header
C           to remove the call of DESC3 and use IPRFNAM and 
C           IRRNAME to determine variable name
C  10 Sep 02 J.Young: fix bug in IPRNAME, IRRNAME index
C  31 Jan 05 J.Young: dyn alloc - establish both horizontal &
C                     vertical domain specifications in one module (GRID_CONF)
C  23 Aug 05 David Wong
C        -- inserted PIO_INIT subroutine calls before and after
C           outputing PA data which is smaller than the model
C           domain size, to compute data indices correctly in each
C           processor for the entire PARIO library functions
C  15 Feb 06 J.Young: new pario PIO_RE_INIT from PIO_INIT to add flag
C                     to control printing decomposition map
C  10 Jul 10 J.Young: restructure so as to not keep reallocating DBUFF for IRR
C  16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C  26 Jan 16 J.Young: consolidated pio_init, flag for couple/decouple
C  16 Sep 16 J.Young: update for inline procan
C   7 Oct 16 J.Young: write domain re-decomp to log only on first call
C  17 Jan 24 D. Wong: fixed a bug that IRR/IPR process alllows a subset of
C                     level, a..b, where 1 <= a <= b <= NLAYS
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE PAGRD_DEFN            ! PA horiz domain specs
      USE UTILIO_DEFN           ! inherits PARUTILIO
#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE, SE_DATA_COPY_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE, NOOP_DATA_COPY_MODULE)
#endif
#endif
      USE PA_DEFN               ! Process Anaylsis control and data variables

      IMPLICIT NONE 

C Includes:
      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:
      REAL, POINTER :: CGRID( :,:,:,: )
      INTEGER, INTENT( IN ) :: JDATE    ! current model date, format YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME    ! current model time, format HHMMSS

C Parameters: None 

C External Functions: None 

C Local Variables: 
      CHARACTER( 16 ), SAVE :: PNAME = 'PA_OUTPUT'
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: IPRFNAM( : )  ! IPR output file names
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: IRRFNAM( : )  ! IRR output file names
      INTEGER, SAVE :: NIPRFLS          ! Number of IPR output files
      INTEGER, SAVE :: NIRRFLS          ! Number of IRR output files
      LOGICAL, SAVE :: LFIRST = .TRUE.  ! Flag for first call

      CHARACTER( 80 ) ::  XMSG = ' '    ! Error message buffer

      INTEGER C         ! Loop index for columns
      INTEGER ICG       ! Index for species in cgrid array
      INTEGER L         ! Loop index for layers
      INTEGER NIRR      ! Loop index for IRR output variables
      INTEGER NFL       ! Loop index for no. of files
      INTEGER NOUT      ! Loop index for PA output variables
      INTEGER R         ! Loop index for rows
#ifndef parallel
      INTEGER PC        ! index for window column
      INTEGER PR        ! index for window row
      INTEGER PL        ! index for window level
#endif
      INTEGER VARINDX   ! Index for output variable
      INTEGER NVARINDX  ! number of output variable
      INTEGER allocstat
     
#ifdef parallel
      CHARACTER( 2 ) :: COLROW = 'CR'  ! col/row arg list order for pio_re_init
      LOGICAL :: WFLG                  ! pio_init - print decomposition
#endif

C-----------------------------------------------------------------------

#ifndef mpas
      if (.not. allocated(IPRFNAM)) then
         allocate (IPRFNAM( MXFILE3 ),
     $             IRRFNAM( MXFILE3 ),
     $             stat=allocstat)
      end if

#ifdef parallel
      IF ( .NOT. LFIRST ) THEN
C Reset decomposition map
         IF ( .NOT. PIO_INIT( COLROW, PACOLS, PAROWS, PALEVS, NTHIK,
     &                        MY_PACOLS, MY_PAROWS, NPCOL, NPROW, NPCOL*NPROW,
     &                        MYPE ) ) THEN
            XMSG = 'Failed to re-initialize parallel I/O library.'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            RETURN
         END IF
      END IF
#endif

C On first call, get and save the output file names 
      IF ( LFIRST ) THEN

#ifdef parallel
C Reset decomposition map
         IF ( .NOT. PIO_INIT( COLROW, PACOLS, PAROWS, PALEVS, NTHIK,
     &                        MY_PACOLS, MY_PAROWS, NPCOL, NPROW, NPCOL*NPROW,
     &                        MYPE, WFLG = .TRUE. ) ) THEN
            XMSG = 'Failed to re-initialize parallel I/O library.'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            RETURN
         END IF
#endif

         IF ( LIPR .AND. NIPRVAR. GT. 0 ) THEN

            IF ( MOD( NIPRVAR, MXVARS3 ) .EQ. 0 ) THEN
               NIPRFLS = NIPRVAR / MXVARS3
            ELSE
               NIPRFLS = ( NIPRVAR / MXVARS3 ) + 1
            END IF

            DO NFL = 1, NIPRFLS

               IF ( NFL .EQ. 1 ) THEN
                  IPRFNAM( NFL ) = CTM_IPR_1
               ELSE IF ( NFL .EQ. 2 ) THEN
                  IPRFNAM( NFL ) = CTM_IPR_2
               ELSE IF ( NFL .EQ. 3 ) THEN
                  IPRFNAM( NFL ) = CTM_IPR_3
               ELSE IF ( NFL .GT. 3 ) THEN
                  XMSG = 'Maximum number of IPR output files exceeded'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF

               IF ( MYPE .EQ. 0 ) THEN
                  IF ( .NOT. OPEN3( IPRFNAM( NFL ), FSRDWR3, PNAME ) ) THEN
                     XMSG = 'Could not open Integrated Process Rate Output File: '
     &                    // IPRFNAM( NFL )
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               END IF

            END DO

         END IF

         IF ( LIRR .AND. NIRRVAR. GT. 0 ) THEN

            IF ( MOD( NIRRVAR, MXVARS3 ) .EQ. 0 ) THEN
               NIRRFLS = NIRRVAR / MXVARS3
            ELSE
               NIRRFLS = ( NIRRVAR / MXVARS3 ) + 1
            END IF

            DO NFL = 1, NIRRFLS
  
               IF ( NFL .EQ. 1 ) THEN
                  IRRFNAM( NFL ) = CTM_IRR_1
               ELSE IF ( NFL .EQ. 2 ) THEN
                  IRRFNAM( NFL ) = CTM_IRR_2
               ELSE IF ( NFL .EQ. 3 ) THEN
                  IRRFNAM( NFL ) = CTM_IRR_3
               ELSE IF ( NFL .GT. 3 ) THEN
                  XMSG = 'Maximum number of IRR output files exceeded'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF

               IF ( MYPE .EQ. 0 ) THEN
                  IF ( .NOT. OPEN3( IRRFNAM( NFL ), FSRDWR3, PNAME ) ) THEN
                     XMSG = 'Could not open Integrated Reaction Rate Output File: '
     &                    // IRRFNAM( NFL )
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               END IF

            END DO

         END IF

      END IF    ! LFIRST

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Output Process analysis data and re-initialize arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LIPR ) THEN

         DO NFL = 1, NIPRFLS

            NVARINDX = MIN ( MXVARS3, NIPRVAR - (NFL - 1) * MXVARS3 )

            DO NOUT = 1, NVARINDX
               VARINDX = ( NFL - 1 ) * MXVARS3 + NOUT

               IF ( .NOT. WRITE3( IPRFNAM( NFL ), IPRNAME( VARINDX ),
     &                 JDATE, JTIME, DELC( :,:,:,VARINDX ) ) ) THEN 
                  XMSG = 'Could not write ' // IPRFNAM( NFL )
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            END DO

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &            'Timestep written to', IPRFNAM( NFL ),
     &            'for date and time', JDATE, JTIME
         END DO

         DO NOUT = 1, NIPRVAR
            DO L = 1, PALEVS
               DO R = 1, MY_PAROWS
                  DO C = 1, MY_PACOLS
                     DELC( C,R,L,NOUT ) = 0.0
                  END DO
               END DO
            END DO
         END DO

      END IF   ! IPR

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Output the IRR data and re-initialize arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LIRR ) THEN

         DO NFL = 1, NIRRFLS

            NVARINDX = MIN (MXVARS3, NIRRVAR - (NFL - 1) * MXVARS3)

            DO NOUT = 1, NVARINDX
               VARINDX = ( NFL - 1 ) * MXVARS3 + NOUT

#ifdef parallel
!              write( logdev,* ) 'paout-nout,varindx,my_pacols,etc,sizes: ',
!    &         nout, varindx, my_pacols, my_parows, palevs,
!    &         size( irrout,1 ), size( irrout,2 ), size( irrout,3 ),
!    &         size( irrout,4 ), size( tirrout,1 ), size( tirrout,2 ),
!    &         size( tirrout,3 ), size( dbuff,1 ), size( dbuff,2 ),
!    &         size( dbuff,3 ) 
               CALL SUBST_DATA_COPY ( IRROUT, DBUFF, VARINDX, 1 )
#else
               DBUFF = IRROUT( 1:MY_PACOLS,1:MY_PAROWS,1:PALEVS,VARINDX )
#endif
               IF ( .NOT. WRITE3( IRRFNAM( NFL ), IRRNAME( VARINDX ),
     &              JDATE, JTIME, DBUFF ) ) THEN 
                  XMSG = 'Could not write ' // IRRFNAM( NFL )
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            END DO

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &            'Timestep written to', IRRFNAM( NFL ),
     &            'for date and time', JDATE, JTIME

         END DO

         DO NIRR = 1, NIRRVAR
         DO L = 1, PALEVS
#ifdef parallel
            DO R = MY_IRR_BEGROW, MY_IRR_ENDROW
               DO C = MY_IRR_BEGCOL, MY_IRR_ENDCOL
#else
            DO R = 1, MY_PAROWS
               DO C = 1, MY_PACOLS
#endif
                      IRROUT( C,R,L,NIRR ) = 0.0
                  END DO
               END DO
            END DO
         END DO

      END IF   ! IRR

#ifdef parallel
      IF ( .NOT. LFIRST ) THEN
C Reset decomposition map
         IF ( .NOT. PIO_INIT( COLROW, GL_NCOLS, GL_NROWS, NLAYS, NTHIK,
     &                        NCOLS, NROWS, NPCOL, NPROW, NPCOL*NPROW,
     &                        MYPE ) ) THEN
            XMSG = 'Failed to re-initialize parallel I/O library.'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            RETURN
         END IF
      ELSE
         IF ( .NOT. PIO_INIT( COLROW, GL_NCOLS, GL_NROWS, NLAYS, NTHIK,
     &                        NCOLS, NROWS, NPCOL, NPROW, NPCOL*NPROW,
     &                        MYPE, WFLG = .TRUE. ) ) THEN
            XMSG = 'Failed to re-initialize parallel I/O library.'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            RETURN
         END IF
      END IF
#endif
#endif

      IF ( LFIRST ) THEN
         LFIRST = .FALSE.
      END IF

      RETURN
      END
