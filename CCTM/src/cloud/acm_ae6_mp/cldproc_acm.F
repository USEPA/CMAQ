
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
C     April 2019 F.Sidi & 
C                S. Roselle:  Fixed bug relating to incorrect unit conversions
C                             for GC, NR and TR.  
C-----------------------------------------------------------------------

      USE GRID_CONF                  ! horizontal & vertical domain specifications
      USE CGRID_SPCS                 ! CGRID mechanism species
      USE BIDI_MOD,   ONLY: HGBIDI   ! Flag for bidirectional Hg flux 
      USE HGSIM,      ONLY: GET_WDEP ! bidirection hg exchange
      USE UTILIO_DEFN
      use CENTRALIZED_IO_MODULE, only : interpolate_var

      IMPLICIT NONE

C...........INCLUDES

      INCLUDE SUBST_CONST            ! constants
      INCLUDE SUBST_FILES_ID         ! file name parameters

      CHARACTER( 120 ) :: XMSG = ' ' ! exit message string

C...........PARAMETERS

C # of wet deposition species
      INTEGER, SAVE :: N_SPC_WDEP

      REAL, PARAMETER :: CNV1 = MWAIR * 1.0E-9
      REAL, PARAMETER :: CNV1I = 1.0 / CNV1
      REAL, PARAMETER :: CNV2 = MWAIR * 1.0E-3
      REAL, PARAMETER :: CNV2I = 1.0 / CNV2
!     REAL, PARAMETER :: CNV3 = MWAIR * 1.0E+3 / AVO  ! -> ppmV
!     REAL, PARAMETER :: CNV3 = CNV2 / AVO            ! -> mol/mol
      REAL, PARAMETER :: CNV3 = CNV2                  ! -> #/mol
      REAL, PARAMETER :: CNV3I = 1.0 / CNV3

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
      INTEGER       L                ! loop counter
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
      INTEGER, ALLOCATABLE, SAVE :: WDEP_MAP( : ) ! wet deposition map to CGRID
      INTEGER, ALLOCATABLE, SAVE :: QAE( : ) ! CGRID pointer to ug/m3 species
      INTEGER, ALLOCATABLE, SAVE :: NAE( : ) ! CGRID pointer to #/m3 species
      INTEGER, ALLOCATABLE, SAVE :: SAE( : ) ! CGRID pointer to m2/m3 species


!     REAL          DENS    ( NCOLS,NROWS,NLAYS ) ! air density (kg/m3)
!     REAL          CONV_DEP( NCOLS,NROWS,N_SPC_WDEP+8 ) ! convective wdep only
!     REAL          TOT_DEP ( NCOLS,NROWS,N_SPC_WDEP+1 ) ! total wdep
      REAL, ALLOCATABLE, SAVE :: DENS    ( :,:,: ) ! air density (kg/m3)
      REAL, ALLOCATABLE, SAVE :: CONV_DEP( :,:,: ) ! convective wdep only
      REAL, ALLOCATABLE, SAVE :: TOT_DEP ( :,:,: ) ! total wdep
      REAL, ALLOCATABLE, SAVE :: RESTRANS( :,: )   ! resolved cloud transmissivity
      REAL, ALLOCATABLE, SAVE :: SUBTRANS( :,:,: ) ! subgrid cloud transmissivity

      REAL ACCM_WDEP                 ! accumulated wet dep for Hg bidi

      REAL FAC                       ! temp conversion factor
      REAL CCMIN

C...........EXTERNAL FUNCTIONS

      INTERFACE
         SUBROUTINE RESCLD ( CGRID, JDATE, JTIME, TSTEP,
     &                       N_SPC_WDEP, WDEP_MAP, DEP, RESTRANS )
            REAL, POINTER            :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE
            INTEGER, INTENT( IN )    :: JTIME
            INTEGER, INTENT( IN )    :: TSTEP( 3 )
            INTEGER, INTENT( IN )    :: N_SPC_WDEP
            INTEGER, INTENT( IN )    :: WDEP_MAP( : )
            REAL,    INTENT( INOUT ) :: DEP( :,:,: )
            REAL,    INTENT( OUT )   :: RESTRANS( :,: )
         END SUBROUTINE RESCLD
         SUBROUTINE CONVCLD_ACM ( CGRID, JDATE, JTIME, TSTEP,
     &                            N_SPC_WDEP, WDEP_MAP, CONV_DEP, SUBTRANS )
            REAL, POINTER            :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE
            INTEGER, INTENT( IN )    :: JTIME
            INTEGER, INTENT( IN )    :: TSTEP( 3 )
            INTEGER, INTENT( IN )    :: N_SPC_WDEP
            INTEGER, INTENT( IN )    :: WDEP_MAP( : )
            REAL,    INTENT( INOUT ) :: CONV_DEP( :,:,: )
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

        N_SPC_WDEP = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP
        ALLOCATE ( WDEP_MAP( N_SPC_WDEP ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating WDEP_MAP'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        IF ( N_SPC_WDEP .LE. 0 ) THEN

          XMSG = 'No species were specified for wet deposition ' //
     &           'tracking'
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )

          XMSG = 'ONLY CLOUD DIAGNOSTICS WILL BE WRITTEN TO THE ' //
     &           'WETDEP OUTPUT FILE!'
          CALL M3MESG ( XMSG )

        END IF

C...check to see if user wants to output extra diagnostic files

        SPC = 0
        STRT = 1
        FINI = N_GC_WDEP
        DO VAR = STRT, FINI
          SPC = SPC + 1
          WDEP_MAP( VAR ) = GC_STRT - 1 + GC_WDEP_MAP( SPC )
        END DO

        SPC = 0
        STRT = N_GC_WDEP + 1
        FINI = N_GC_WDEP + N_AE_WDEP
        DO VAR = STRT, FINI
          SPC = SPC + 1
          WDEP_MAP( VAR ) = AE_STRT - 1 + AE_WDEP_MAP( SPC )
        END DO

        SPC = 0
        STRT = N_GC_WDEP + N_AE_WDEP + 1
        FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP
        DO VAR = STRT, FINI
          SPC = SPC + 1
          WDEP_MAP( VAR ) = NR_STRT - 1 + NR_WDEP_MAP( SPC )
        END DO

        SPC = 0
        STRT = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + 1
        FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP
        DO VAR = STRT, FINI
          SPC = SPC + 1
          WDEP_MAP( VAR ) = TR_STRT - 1 + TR_WDEP_MAP( SPC )
        END DO

        IF ( N_AE_SPC .GT. 0 ) THEN
C...create aerosol species pointers to distinguish micro-grams / m**3
C...  # / m**3 (number density), and m**2 / m**3 (surface area) units

          ALLOCATE ( QAE( N_AE_SPC ),
     &               NAE( N_AE_SPC ),
     &               SAE( N_AE_SPC ), STAT = ALLOCSTAT )
          IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating QAE, NAE, or SAE'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF

          NQAE = 0       ! number of ug/m3 species
          NNAE = 0       ! number of #/m3 species
          NSAE = 0       ! number of m2/m3 species

          DO VAR = 1, N_AE_SPC
            IF ( AE_SPC( VAR )( 1:3 ) .EQ. 'NUM' ) THEN
              NNAE = NNAE + 1
              NAE( NNAE ) = AE_STRT - 1 + VAR
            ELSE IF ( AE_SPC( VAR )( 1:3 ) .EQ. 'SRF' ) THEN
              NSAE = NSAE + 1
              SAE( NSAE ) = AE_STRT - 1 + VAR
            ELSE
              NQAE = NQAE + 1
              QAE( NQAE ) = AE_STRT - 1 + VAR
            END IF
          END DO

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

      END IF   ! FIRSTIME

      MDATE = JDATE
      MTIME = JTIME

      call interpolate_var ('DENS', mdate, mtime, DENS)

C...convert units on cgrid to molar mixing ratio (by volume)
C...  determine no. of moles of gas per moles of air by volume

      STRT = GC_STRT
      FINI = GC_STRT + N_GC_SPC - 1
      DO SPC = STRT, FINI
        DO LAY = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              CGRID( COL, ROW, LAY, SPC ) = MAX( CGRID( COL, ROW, LAY, SPC )
     &                                    * 1.0E-6,  1.0E-36 )
            END DO
          END DO
        END DO
      END DO

C...for aerosol mass concentration
C...  convert to moles of aerosol per mole of air by volume

      STRT = 1
      FINI = NQAE
      DO VAR = STRT, FINI
        SPC = QAE( VAR )
        DO LAY = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              FAC = CNV1 / DENS( COL, ROW, LAY ) / AE_MOLWT( SPC - AE_STRT + 1 )
              CCMIN = MAX( CGRID( COL, ROW, LAY, SPC ), 1.0E-30/FAC )
              CGRID( COL, ROW, LAY, SPC ) = FAC * CCMIN
            END DO
          END DO
        END DO
      END DO

C...for aerosol number concentration
C...    convert to # per mole of air by volume

      STRT = 1
      FINI = NNAE
      DO VAR = STRT, FINI
        SPC = NAE( VAR )
        DO LAY = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              FAC = CNV3 / DENS( COL, ROW, LAY )
              CCMIN = MAX( CGRID( COL, ROW, LAY, SPC ), 1.0E-30/FAC )
              CGRID( COL, ROW, LAY, SPC ) = FAC * CCMIN
            END DO
          END DO
        END DO
      END DO

C...for aerosol surface area
C...    convert to m2 per mole of air by volume

      STRT = 1
      FINI = NSAE
      DO VAR = STRT, FINI
        SPC = SAE( VAR )
        DO LAY = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              FAC = CNV2 / DENS( COL, ROW, LAY )
              CCMIN = MAX( CGRID( COL, ROW, LAY, SPC ), 1.0E-30/FAC )
              CGRID( COL, ROW, LAY, SPC ) = FAC * CCMIN
            END DO
          END DO
        END DO
      END DO

C...determine no. of moles of non-reactive gas per moles of air by volume

      STRT = NR_STRT
      FINI = NR_STRT + N_NR_SPC - 1
      DO SPC = STRT, FINI
        DO LAY = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              CGRID( COL, ROW, LAY, SPC ) = MAX( CGRID( COL, ROW, LAY, SPC )
     &                                    * 1.0E-6, 1.0E-36 )
            END DO
          END DO
        END DO
      END DO

C...determine no. of moles of tracer gas per moles of air by volume

      STRT = TR_STRT
      FINI = TR_STRT + N_TR_SPC - 1
      DO SPC = STRT, FINI
        DO LAY = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              CGRID( COL, ROW, LAY, SPC ) = MAX( CGRID( COL, ROW, LAY, SPC )
     &                                    * 1.0E-6, 1.0E-36 )
            END DO
          END DO
        END DO
      END DO

C...compute cloud effects for the resolved clouds and resolved rainwater

      CALL RESCLD ( CGRID, JDATE, JTIME, TSTEP,
     &              N_SPC_WDEP, WDEP_MAP, TOT_DEP, RESTRANS )

C...compute cloud effects for convective (subgrid) clouds

      CALL CONVCLD_ACM ( CGRID, JDATE, JTIME, TSTEP,
     &                   N_SPC_WDEP, WDEP_MAP, CONV_DEP, SUBTRANS )

C...share wet deposition with bidirectional surface exchange algorithms

      IF ( HGBIDI ) THEN
        DO VAR = 1, N_GC_WDEP
           IF ( GC_WDEP( VAR ) .EQ. 'HG' ) THEN
             DO ROW = 1, NROWS
                DO COL = 1, NCOLS
                   ACCM_WDEP = TOT_DEP( COL, ROW, VAR ) 
     &                       + CONV_DEP( COL, ROW, VAR )
                   CALL GET_WDEP ( 'HG      ', ACCM_WDEP, COL, ROW )
                END DO
             END DO
           END IF
           IF ( GC_WDEP( VAR ) .EQ. 'HGIIGAS' ) THEN
             DO ROW = 1, NROWS
                DO COL = 1, NCOLS
                   ACCM_WDEP = TOT_DEP( COL, ROW, VAR ) 
     &                       + CONV_DEP( COL, ROW, VAR )
                   CALL GET_WDEP ( 'HGIIGAS ', ACCM_WDEP, COL, ROW )
                END DO
             END DO
           END IF
        END DO
      END IF

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

C...write data to the normal wet deposition file

        IF ( .NOT. WRITE3( CTM_WET_DEP_1, ALLVAR3, NDATE,
     &                     NTIME, TOT_DEP ) ) THEN
          XMSG = 'Could not write ' // CTM_WET_DEP_1 // ' file'
          CALL M3EXIT ( PNAME, NDATE, NTIME, XMSG, XSTAT1 )
        END IF

        WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &         'Timestep written to', CTM_WET_DEP_1,
     &         'for date and time', NDATE, NTIME

C...write data to the diagnostic file if requested by the user

        IF ( CLD_DIAG ) THEN

          IF ( .NOT. WRITE3( CTM_WET_DEP_2, ALLVAR3, NDATE,
     &                       NTIME, CONV_DEP ) ) THEN
          XMSG = 'Could not write ' // CTM_WET_DEP_2 // ' file'
            CALL M3EXIT ( PNAME, NDATE, NTIME, XMSG, XSTAT1 )
          END IF

          WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &           'Timestep written to', CTM_WET_DEP_2,
     &           'for date and time', NDATE, NTIME

        END IF   ! CLD_DIAG

C...reinitialize deposition array

        TOT_DEP  = 0.0
        CONV_DEP = 0.0

      END IF   ! time to write

C...convert units on cgrid back to original units
C...  convert to ppmV gas

      STRT = GC_STRT
      FINI = GC_STRT + N_GC_SPC - 1
      DO SPC = STRT, FINI
        DO LAY = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              CGRID( COL, ROW, LAY, SPC ) = CGRID( COL, ROW, LAY, SPC )
     &                                    * 1.0E+6
            END DO
          END DO
        END DO
      END DO

C...convert to ug/m3 of aerosol mass

      STRT = 1
      FINI = NQAE
      DO VAR = STRT, FINI
        SPC = QAE( VAR )
        DO LAY = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              FAC = CNV1I * DENS( COL, ROW, LAY )
              CGRID( COL, ROW, LAY, SPC ) = FAC * CGRID( COL, ROW, LAY, SPC )
     &                                    * AE_MOLWT( SPC - AE_STRT + 1 )
            END DO
          END DO
        END DO
      END DO

C...convert to #/m3 of aerosol number

      STRT = 1
      FINI = NNAE
      DO VAR = STRT, FINI
        SPC = NAE( VAR )
        DO LAY = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              FAC = CNV3I * DENS( COL, ROW, LAY )
              CGRID( COL, ROW, LAY, SPC ) = FAC * CGRID( COL, ROW, LAY, SPC )
            END DO
          END DO
        END DO
      END DO

C...convert to m2/m3 of aerosol surface area

      STRT = 1
      FINI = NSAE
      DO VAR = STRT, FINI
        SPC = SAE( VAR )
        DO LAY = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              FAC = CNV2I * DENS( COL, ROW, LAY )
              CGRID( COL, ROW, LAY, SPC ) = FAC * CGRID( COL, ROW, LAY, SPC )
            END DO
          END DO
        END DO
      END DO

C...convert to ppmV non-reactive gas

      STRT = NR_STRT
      FINI = NR_STRT + N_NR_SPC - 1
      DO SPC = STRT, FINI
        DO LAY = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              CGRID( COL, ROW, LAY, SPC ) = CGRID( COL, ROW, LAY, SPC )
     &                                    * 1.0E+6
            END DO
          END DO
        END DO
      END DO

C...convert to ppmV tracer gas

      STRT = TR_STRT
      FINI = TR_STRT + N_TR_SPC - 1
      DO SPC = STRT, FINI
        DO LAY = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              CGRID( COL, ROW, LAY, SPC ) = CGRID( COL, ROW, LAY, SPC )
     &                                    * 1.0E+6
            END DO
          END DO
        END DO
      END DO

      RETURN

      END
