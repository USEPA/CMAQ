
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

      SUBROUTINE PA_WRTPADEFN

C-----------------------------------------------------------------------
 
C Function: To write the Models-3 Process Analysis Control PA_DEFN module 
 
C Preconditions: Requires mechanism to have been processed
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Modified Jan, 2006 by J. Young I couldn't resist the temptation to F90-ize the output
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI's M3UTILIO
 
C-----------------------------------------------------------------------
      USE M3UTILIO      ! IOAPI module
!     USE PA_GLOBAL     ! Mech and Grid data used 
      USE PA_VARS, Only: TDATOUT
      USE PA_IPRVARS, Only: MAXIPROUT
      USE PA_PARSE
!     USE PA_IPRDEF
!     USE PA_INCDATA
      USE PA_DEFN

      IMPLICIT NONE
      
C Includes: None
      
C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables:

!     CHARACTER(  1 ) :: DOT = '.'  ! Dot character
      CHARACTER(  1 ) :: DIT = '='  ! Dot character
      CHARACTER( 72 ) :: DELIM      ! Delimiter line output
      CHARACTER( 72 ) :: CEES       ! Line of 'c's
      CHARACTER( 80 ) :: LINOUT     ! Output line

      INTEGER IOUT            ! Unit number for include file
      INTEGER N               ! Loop index
         
      INTEGER MXCGRIDD        ! Dimension for number of familys for PA
      INTEGER MXNIRRRXNSD     ! Dimension parameter for MXNIRRRXNS
      INTEGER MXTEMPTRMSD     ! Dimension parameter for MXTEMPTRMS
      INTEGER NCSAVED         ! Dimension for NCSAVE
      INTEGER NIRRVARD        ! Dimension of IRR output variables
      INTEGER N_IPR_SPCD      ! Dimension statement for no. of PA species
      INTEGER NIPRVARD        ! Dimension of PA output variables
      INTEGER NUMTEMPSD       ! Dimension for number of temp IRRs
      INTEGER NUMOUTPOSD      ! Dimension parameter for NUMOUTPOS
      INTEGER NUMOUTNEGD      ! Dimension parameter for NUMOUTNEG
      INTEGER NUMOUTINDD      ! Dimension parameter for NUMOUTIND

!     LOGICAL LIRR            ! Flag for IRR computations
!     LOGICAL LIPR            ! Flag for process monitoring
         
      CHARACTER(  7 ) :: BUF6 ( MAXIPROUT )  ! 6-Character buffer
      CHARACTER( 13 ) :: BUF12( MAXIPROUT )  ! 12-Character buffer
      CHARACTER( 21 ) :: BUF20( MAXIPROUT )  ! 20-Character buffer

      INTEGER :: MAXCNT = 99  ! Maximum number of continuation lines
      INTEGER NHI             ! Highest index of loop
      INTEGER NIRR            ! IRR loop index
      INTEGER NLO             ! Lowest index of loop
      INTEGER NPA             ! IPR species loop index
      INTEGER NS              ! Loop index for sets of data statments
      INTEGER NSETS           ! Number of sets of data statements to write

C-----------------------------------------------------------------------

      DO N = 1, 72
         DELIM( N:N ) = DIT
         CEES( N:N ) = 'c'
      END DO     
      DELIM( 1:1 ) = 'C'

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open and Control Include file and write the header data
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IOUT = JUNIT(  )
      OPEN( UNIT = IOUT, FILE = 'PA_DEFN_REPORT' )
      WRITE( IOUT, 94000 ) DELIM( 1:72 )
      LINOUT = '      MODULE PA_DEFN'
      WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )
      WRITE( IOUT, 94000 ) LINOUT( 1:1 )
      LINOUT = 'C Contains: Control parameters for Process Analysis'
      WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )
      WRITE( IOUT, 94000 ) LINOUT( 1:1 )
      LINOUT = 'C Dependent On: Mechanism Input file'
      WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )
      WRITE( IOUT, 94000 ) LINOUT( 1:1 )
      LINOUT = 'C Created By: Process Analysis Control Program'
      WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )
      LINOUT = 'C           ' // TDATOUT
      WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )
      LINOUT = ''
      WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write the control parameters
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      WRITE( IOUT, 94000 ) CEES
      WRITE( IOUT, 94020 )
      WRITE( IOUT, 94040 )      
      WRITE( IOUT, 94000 ) CEES
      WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )

c..LIPR -- for integrated process rates
      IF ( N_IPR_SPC .GT. 0 ) THEN
         WRITE( IOUT, 94080 )
      ELSE
         WRITE( IOUT, 94100 )
      END IF

c..LIRR -- for integrated reaction rates     
      IF ( LFULLIRR .OR. LPARTIRR ) THEN
         WRITE( IOUT, 94140 )
      ELSE
         WRITE( IOUT, 94160 )
      END IF

!     IF ( LFULLIRR ) THEN
!        LIRR    = .TRUE.
!        NIRRVAR = NRXNS
!     ELSE IF ( LPARTIRR ) THEN
!        LIRR    = .TRUE.
!        NIRRVAR = NIRROUT
!     ELSE
!        LIRR = .FALSE.     
!     END IF

!     IF ( N_IPR_SPC .GT. 0 ) THEN
!        LIPR = .TRUE.
!     ELSE
!        LIPR = .FALSE.
!     END IF
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write messages about array dimensioning if needed
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. LIRR ) THEN
         WRITE( IOUT, 94000 ) CEES
         WRITE( IOUT, 95020 )
         WRITE( IOUT, 94000 ) CEES
         WRITE( IOUT, 94000 )
      END IF

      IF ( .NOT. LIPR ) THEN
         WRITE( IOUT, 94000 ) CEES
         WRITE( IOUT, 95040 )
         WRITE( IOUT, 94000 ) CEES
         WRITE( IOUT, 94000 ) 
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write output file variables 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( IOUT, 94000 ) CEES
      WRITE( IOUT, 95120 ) 
      WRITE( IOUT, 94000 ) CEES
      WRITE( IOUT, 95140 ) 
      WRITE( IOUT, 94000 ) CEES
      NIPRVARD = MAX( NIPRVAR, 1 )
      NIRRVARD = MAX( NIRRVAR, 1 )
      WRITE( IOUT, 95160 ) NIPRVAR, NIPRVARD
      WRITE( IOUT, 95180 ) NIRRVAR, NIRRVARD
      WRITE( IOUT, 95200 )
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write integrated process rate variables 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( IOUT, 94000 ) CEES
      WRITE( IOUT, 95260 ) 
      WRITE( IOUT, 94000 ) CEES
      WRITE( IOUT, 95280 ) 
      WRITE( IOUT, 94000 ) CEES

      N_IPR_SPCD  = MAX( N_IPR_SPC, 1 )
      NCSAVED     = MAX( NCSAVE, 1 )
      MXCGRIDD    = MAX( MXCGRID, 1 )

      WRITE( IOUT, 95300 ) NPRCS,
     &                     N_IPR_SPC, N_IPR_SPCD,
     &                     NCSAVE, NCSAVED,
     &                     MXCGRID, MXCGRIDD
      WRITE( IOUT, 95320 )
      LINOUT = ''
      WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write integrated reaction rate variables 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( IOUT, 94000 ) CEES
      WRITE( IOUT, 95360 ) 
      WRITE( IOUT, 94000 ) CEES
      WRITE( IOUT, 95380 ) 
      WRITE( IOUT, 94000 ) CEES

      IF ( LFULLIRR ) THEN
         WRITE( IOUT, 95420 )
      ELSE
         WRITE( IOUT, 95440 )
      END IF

      IF ( LPARTIRR ) THEN
         WRITE( IOUT, 95480 )
      ELSE
         WRITE( IOUT, 95500 )
      END IF

!     IF ( .NOT. LPARTIRR ) THEN
!        NUMTEMPS   = 0
!        NUMOUTPOS  = 0
!        NUMOUTNEG  = 0
!        NUMOUTIND  = 0
!        MXNIRRRXNS = 0
!        MXTEMPTRMS = 0
!     END IF

      NUMTEMPSD    = MAX( NUMTEMPS,   1 )
      NUMOUTPOSD   = MAX( NUMOUTPOS,  1 )
      NUMOUTNEGD   = MAX( NUMOUTNEG,  1 )
      NUMOUTINDD   = MAX( NUMOUTIND,  1 )
      MXNIRRRXNSD  = MAX( MXNIRRRXNS, 1 )
      MXTEMPTRMSD  = MAX( MXTEMPTRMS, 1 )

      WRITE( IOUT, 95520 ) NUMTEMPS, NUMTEMPSD,
     &                     NUMOUTPOS, NUMOUTPOSD,
     &                     NUMOUTNEG, NUMOUTNEGD,
     &                     NUMOUTIND, NUMOUTINDD,
     &                     MXNIRRRXNS, MXNIRRRXNSD,
     &                     MXTEMPTRMS, MXTEMPTRMSD

      IF ( NIRRVAR .NE. 0 ) THEN
         WRITE( IOUT, 95542 )
      ELSE 
         WRITE( IOUT, 95544 )
      END IF

      WRITE( IOUT, 95580 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write monocode extensions (declarations and common block) and end
c  line and return
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. LIPR .AND. .NOT. LIRR ) THEN
         WRITE( IOUT, 96110 )
         GO TO 999
      END IF         

C-----------------------------------------------------------------------
      LINOUT = ''
      WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )
      LINOUT = 'C Initialize variable arrays via DATA stmts.'
      WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write data statements for output file variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( IOUT, 94000 ) CEES
      WRITE( IOUT, 96120 ) 
      WRITE( IOUT, 94000 ) CEES
      WRITE( IOUT, 96200 ) 
 
      IF ( NIPRVAR .GT. 0 ) THEN
         NSETS = ( ( NIPRVAR - 1 ) / ( 3 * MAXCNT ) ) + 1
         write( *,* ) 'n IPR var nsets 1: ', nsets
         DO NS = 1, NSETS
            NLO = ( NS - 1 ) * ( 3 * MAXCNT ) + 1
            NHI = MIN( ( NS * 3 * MAXCNT ), NIPRVAR )
            write( *,* ) 'n IPR var nlo nhi: ', nlo, nhi
            WRITE( IOUT, 96220 ) NLO,  NHI
            DO NPA = NLO, NHI
               BUF20( NPA ) = ' "' // IPRNAME( NPA ) // '",'
            END DO
            BUF20( NHI )( 20:21 ) = ' /'
!           WRITE( IOUT, 96020 ) ( BUF20( NPA ), NPA = NLO, NHI )
            call listout( NLO, NHI, 3, IOUT, BUF20 )
         END DO

         NSETS = ( ( NIPRVAR - 1 ) / MAXCNT ) + 1
         write( *,* ) 'n IPR var nsets 2: ', nsets
         DO NS = 1, NSETS
            NLO = ( NS - 1 ) * ( MAXCNT ) + 1
            NHI = MIN( ( NS * MAXCNT ), NIPRVAR )         
            write( *,* ) 'n IPR var nlo nhi: ', nlo, nhi
            WRITE( IOUT, 96240 ) NLO,  NHI
            DO NPA = NLO, NHI
               LINOUT = '     &  "' // IPRDESC( NPA ) // '",'
               IF ( NPA .EQ. NHI ) LINOUT( 71:71 ) = '/'
               WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )
            END DO
         END DO
      END IF

      IF ( NIRRVAR .GT. 0 ) THEN
         NSETS = ( ( NIRRVAR - 1 ) / ( 3 * MAXCNT ) ) + 1
         write( *,* ) 'n IRR var nsets 1: ', nsets
         DO NS = 1, NSETS
            NLO = ( NS - 1 ) * ( 3 * MAXCNT ) + 1
            NHI = MIN( ( NS * 3 * MAXCNT ), NIRRVAR )
            write( *,* ) 'n IRR var nlo nhi: ', nlo, nhi
            WRITE( IOUT, 96260 ) NLO, NHI
            DO NIRR = NLO, NHI
               BUF20( NIRR ) = ' "' // IRRNAME( NIRR ) // '",'
            END DO
            BUF20( NHI )( 20:21 ) = ' /'
!           WRITE( IOUT, 96020 ) ( BUF20( NIRR ), NIRR = NLO, NHI )
            call listout( NLO, NHI, 3, IOUT, BUF20 )
         END DO

         NSETS = ( ( NIRRVAR - 1 ) / MAXCNT ) + 1
         write( *,* ) 'n IRR var nsets 2: ', nsets
         DO NS = 1, NSETS
            NLO = ( NS - 1 ) * ( MAXCNT ) + 1
            NHI = MIN( ( NS * MAXCNT ), NIRRVAR )         
            write( *,* ) 'n IRR var nlo nhi: ', nlo, nhi
            WRITE( IOUT, 96280 ) NLO,  NHI
            DO NIRR = NLO, NHI
               LINOUT = '     &  "' // IRRDESC( NIRR ) // '",'
               IF ( NIRR .EQ. NHI ) LINOUT( 71:71 ) = '/'
               WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )
            END DO
         END DO
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write data statements for integrated Process rate variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LIPR ) THEN
         WRITE( IOUT, 94000 ) ''
         WRITE( IOUT, 94000 ) CEES
         WRITE( IOUT, 96140 ) 
         WRITE( IOUT, 94000 ) CEES

         NSETS = ( ( NPRCS - 1 ) / ( 3 * MAXCNT ) ) + 1
         DO NS = 1,  NSETS
            NLO = ( NS - 1 ) * ( 3 * MAXCNT ) + 1
            NHI = MIN( ( NS * 3 * MAXCNT ),  NPRCS )         
            WRITE( IOUT, 96290 ) NLO, NHI
            DO NPA = NLO, NHI
               BUF20( NPA ) = ' "' // PROCNAME( NPA ) // '",' 
            END DO
            BUF20( NHI )( 20:21 ) = ' /'
!           WRITE( IOUT, 96020 ) ( BUF20( NPA ), NPA = NLO, NHI )
            call listout( NLO, NHI, 3, IOUT, BUF20 )
         END DO
      END IF

      IF ( N_IPR_SPC .GT. 0 ) THEN
         NSETS = ( ( N_IPR_SPC - 1 ) / ( 10 * MAXCNT ) ) + 1
         DO NS = 1, NSETS
            NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
            NHI = MIN( ( NS * 10 * MAXCNT ), N_IPR_SPC )         
            WRITE( IOUT, 96300 ) NLO, NHI
            DO NPA = NLO,  NHI
               WRITE( BUF6( NPA ), "( I5, ',' )" ) NCGRID( NPA )
            END DO
            BUF6( NHI )( 6:7 ) = ' /'
!           WRITE( IOUT, 96040 ) ( BUF6( NPA ), NPA = NLO, NHI )
            call listout( NLO, NHI, 10, IOUT, BUF6 )
         END DO
      END IF

      IF ( NCSAVE .GT. 0 ) THEN
         NSETS = ( ( NCSAVE - 1 ) / ( 10 * MAXCNT ) ) + 1
         DO NS = 1, NSETS
            NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
            NHI = MIN( ( NS * 10 * MAXCNT ), NCSAVE )         
            WRITE( IOUT, 96320 ) NLO, NHI
            DO NPA = NLO,  NHI
               WRITE( BUF6( NPA ), 96080 ) SV2GRD( NPA )
            END DO
            BUF6( NHI )( 6:7 ) = ' /'
!           WRITE( IOUT, 96040 ) ( BUF6( NPA ), NPA = NLO, NHI )
            call listout( NLO, NHI, 10, IOUT, BUF6 )
         END DO
      END IF

      IF ( N_IPR_SPC .GT. 0 ) THEN
         DO NPA = 1, N_IPR_SPC
            NSETS = ( ( NPRCS - 1 ) / ( 10 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
               NHI = MIN( ( NS * 10 * MAXCNT ), NPRCS )         
               WRITE( IOUT, 96340 ) NPA, NLO, NHI
               DO N = NLO,  NHI
                  WRITE( BUF6( N ), 96080 ) IPROUT( NPA, N )
               END DO
               BUF6( NHI )( 6:7 ) = ' /'
!              WRITE( IOUT, 96040 ) ( BUF6( N ), N = NLO, NHI )
               call listout( NLO, NHI, 10, IOUT, BUF6 )
            END DO 
         END DO
      END IF 

      IF ( N_IPR_SPC .GT. 0 ) THEN
         DO NPA = 1, N_IPR_SPC
            NSETS = ( ( NCGRID( NPA ) - 1 ) / ( 10 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
               NHI = MIN( ( NS * 10 * MAXCNT ),  NCGRID( NPA ) )         
               WRITE( IOUT, 96360 ) NPA, NLO, NHI
               DO N = NLO, NHI
                  WRITE( BUF6( N ), 96080 ) IPR2GRD( NPA, N )
               END DO
               BUF6( NHI )( 6:7 ) = ' /'
!              WRITE( IOUT, 96040 ) ( BUF6( N ), N = NLO, NHI )
               call listout( NLO, NHI, 10, IOUT, BUF6 )
            END DO 
         END DO
      END IF

      IF ( N_IPR_SPC .GT. 0 ) THEN
         DO NPA = 1, N_IPR_SPC
            NSETS = ( ( NCGRID( NPA ) - 1 ) / ( 10 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
               NHI = MIN( ( NS * 10 * MAXCNT ), NCGRID( NPA ) )         
               WRITE( IOUT, 96380 ) NPA,  NLO,  NHI
               DO N = NLO, NHI
                  WRITE( BUF6( N ), 96080 ) IPR2SAV( NPA, N )
               END DO
               BUF6( NHI )( 6:7 ) = ' /'
!              WRITE( IOUT, 96040 ) ( BUF6( N ), N = NLO, NHI )
               call listout( NLO, NHI, 10, IOUT, BUF6 )
            END DO 
         END DO
      END IF

      IF ( LIPR ) THEN
         NSETS = ( ( NPRCS - 1 ) / ( 5 * MAXCNT ) ) + 1
         DO NS = 1,  NSETS
            NLO = ( NS - 1 ) * ( 5 * MAXCNT ) + 1
            NHI = MIN( ( NS * 5 * MAXCNT ), NPRCS )         
            WRITE( IOUT, 96400 ) NLO,  NHI
            DO N = NLO, NHI
               IF ( LPROCOUT( N ) ) THEN
                  BUF12( N ) = '     .TRUE., '
               ELSE
                  BUF12( N ) = '    .FALSE., '
               END IF
            END DO
            BUF12( NHI )( 12:13 ) = ' /'  
!           WRITE( IOUT, 96060 ) ( BUF12( N ), N = NLO, NHI )
            call listout( NLO, NHI, 5, IOUT, BUF12 )
         END DO
      END IF

      IF ( N_IPR_SPC .GT. 0 ) THEN
         DO NPA = 1, N_IPR_SPC
            NSETS = ( ( NCGRID( NPA ) - 1 ) / ( 5 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 5 * MAXCNT ) + 1
               NHI = MIN( ( NS * 5 * MAXCNT ), NCGRID( NPA ) )         
               WRITE( IOUT, 96420 ) NPA, NLO, NHI
               DO N = 1,  NCGRID( NPA )
                  WRITE( BUF12( N ), 96100 ) SPCOEF( NPA,  N )
               END DO
               BUF12( NHI )( 12:13 ) = ' /'
               WRITE( IOUT, 96060 ) ( BUF12( N ), N = NLO, NHI )
            END DO 
         END DO
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write data statements for IRR variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LPARTIRR ) THEN
         WRITE( IOUT, 94000 )
         WRITE( IOUT, 94000 ) CEES
         WRITE( IOUT, 96160 ) 
         WRITE( IOUT, 94000 ) CEES

         IF ( NIRRVAR .GT. 0 ) THEN
            NSETS = ( ( NIRRVAR - 1 ) / ( 10 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
               NHI = MIN( ( NS * 10 * MAXCNT ), NIRRVAR )         
               WRITE( IOUT, 96440 ) NLO, NHI
               DO NIRR = NLO, NHI
                  WRITE( BUF6( NIRR ), 96080 ) NIRRRXNS( NIRR )
               END DO
               BUF6( NHI )( 6:7 ) = ' /'
!              WRITE( IOUT, 96040 ) ( BUF6( NIRR ), NIRR = NLO, NHI )
               call listout( NLO, NHI, 10, IOUT, BUF6 )
            END DO
         END IF     

         IF ( NUMTEMPS .GT. 0 ) THEN
            NSETS = ( ( NUMTEMPS - 1 ) / ( 10 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
               NHI = MIN( ( NS * 10 * MAXCNT ),  NUMTEMPS )         
               WRITE( IOUT, 96460 ) NLO, NHI
               DO NIRR = NLO, NHI
                  WRITE( BUF6( NIRR ), 96080 ) TEMPTERMS( NIRR )
               END DO
               BUF6( NHI )( 6:7 ) = ' /'
!              WRITE( IOUT, 96040 ) ( BUF6( NIRR ), NIRR = NLO, NHI )
               call listout( NLO, NHI, 10, IOUT, BUF6 )
            END DO
         END IF     
 
         IF ( NUMOUTPOS .GT. 0 ) THEN
            NSETS = ( ( NUMOUTPOS - 1 ) / ( 10 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
               NHI = MIN(  ( NS * 10 * MAXCNT ),  NUMOUTPOS )         
               WRITE( IOUT, 96480 ) NLO, NHI
               DO NIRR = NLO, NHI
                  WRITE( BUF6( NIRR ), 96080 ) TEMPOUTPOS( NIRR )
               END DO
               BUF6( NHI )( 6:7 ) = ' /'
!              WRITE( IOUT, 96040 ) ( BUF6( NIRR ), NIRR = NLO, NHI )
               call listout( NLO, NHI, 10, IOUT, BUF6 )
            END DO
         END IF 
             
         IF ( NUMOUTNEG .GT. 0 ) THEN
            NSETS = ( ( NUMOUTNEG - 1 ) / ( 10 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
               NHI = MIN( ( NS * 10 * MAXCNT ),  NUMOUTNEG )         
               WRITE( IOUT, 96500 ) NLO,  NHI
               DO NIRR = NLO, NHI
                  WRITE( BUF6( NIRR ), 96080 ) TEMPOUTNEG( NIRR )
               END DO
               BUF6( NHI )( 6:7 ) = ' /'
!              WRITE( IOUT, 96040 ) ( BUF6( NIRR ), NIRR = NLO, NHI )
               call listout( NLO, NHI, 10, IOUT, BUF6 )
            END DO
         END IF 

         IF ( NUMOUTIND .GT. 0 ) THEN
            NSETS = ( ( NUMOUTIND - 1 ) / ( 10 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
               NHI = MIN( ( NS * 10 * MAXCNT ), NUMOUTIND )         
               WRITE( IOUT, 96520 ) NLO, NHI
               DO NIRR = NLO, NHI
                  WRITE( BUF6( NIRR ), 96080 ) TEMPOUTIND( NIRR )
              END DO
               BUF6( NHI )( 6:7 ) = ' /'
!              WRITE( IOUT, 96040 ) ( BUF6( NIRR ), NIRR = NLO, NHI )
               call listout( NLO, NHI, 10, IOUT, BUF6 )
            END DO
         END IF 

         IF ( NUMOUTPOS .GT. 0 ) THEN
            NSETS = ( ( NUMOUTPOS - 1 ) / ( 10 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
               NHI = MIN( ( NS * 10 * MAXCNT ), NUMOUTPOS )         
               WRITE( IOUT, 96540 ) NLO, NHI
               DO NIRR = NLO, NHI
                  WRITE( BUF6( NIRR ), 96080 ) INDXOUTPOS( NIRR )
               END DO
               BUF6( NHI )( 6:7 ) = ' /'
!              WRITE( IOUT, 96040 ) ( BUF6( NIRR ), NIRR = NLO, NHI )
               call listout( NLO, NHI, 10, IOUT, BUF6 )
            END DO
         END IF 

         IF ( NUMOUTNEG .GT. 0 ) THEN
            NSETS = ( ( NUMOUTNEG - 1 ) / ( 10 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
               NHI = MIN( ( NS * 10 * MAXCNT ), NUMOUTNEG )       
               WRITE( IOUT, 96560 ) NLO, NHI
               DO NIRR = NLO, NHI
                  WRITE( BUF6( NIRR ), 96080 ) INDXOUTNEG( NIRR )
              END DO
               BUF6( NHI )( 6:7 ) = ' /'
!              WRITE( IOUT, 96040 ) ( BUF6( NIRR ), NIRR = NLO, NHI )
               call listout( NLO, NHI, 10, IOUT, BUF6 )
            END DO
         END IF 

         IF ( NUMOUTIND .GT. 0 ) THEN
            NSETS = ( ( NUMOUTIND - 1 ) / ( 10 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
               NHI = MIN( ( NS * 10 * MAXCNT ), NUMOUTIND )       
               WRITE( IOUT, 96580 ) NLO, NHI
               DO NIRR = NLO, NHI
                  WRITE( BUF6( NIRR ), 96080 ) INDXOUTIND( NIRR )
               END DO
               BUF6( NHI )( 6:7 ) = ' /'
!              WRITE( IOUT, 96040 ) ( BUF6( NIRR ), NIRR = NLO, NHI )
               call listout( NLO, NHI, 10, IOUT, BUF6 )
            END DO
         END IF

         IF ( NIRRVAR .GT. 0 ) THEN
            DO NIRR = 1, NIRRVAR
               IF ( NIRRRXNS( NIRR ) .GT. 0 ) THEN
                  NSETS = ( ( NIRRRXNS( NIRR ) - 1 ) / ( 10 * MAXCNT ) ) + 1
                  DO NS = 1,  NSETS
                     NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
                     NHI = MIN( ( NS * 10 * MAXCNT ), NIRRRXNS( NIRR ) )         
                     WRITE( IOUT, 96600 ) NIRR, NLO, NHI
                     DO N = NLO, NHI
                        WRITE( BUF6( N ), 96080 ) IRRRXN( NIRR, N )
                     END DO
                     BUF6( NHI )( 6:7 ) = ' /'
!                    WRITE( IOUT, 96040 ) ( BUF6( N ), N = NLO, NHI )
                     call listout( NLO, NHI, 10, IOUT, BUF6 )
                  END DO
               END IF 
            END DO
         END IF

         IF ( NUMTEMPS .GT. 0 ) THEN
            DO NIRR= 1, NUMTEMPS
               IF ( TEMPTERMS( NIRR ) .GT. 0 ) THEN
                  NSETS = ( ( TEMPTERMS( NIRR ) - 1 ) / ( 10 * MAXCNT ) ) + 1
                  DO NS = 1, NSETS
                     NLO = ( NS - 1 ) * ( 10 * MAXCNT ) + 1
                     NHI = MIN( ( NS * 10 * MAXCNT ), TEMPTERMS( NIRR ) )         
                     WRITE( IOUT, 96620 ) NIRR,  NLO,  NHI
                     DO N = NLO, NHI
                        WRITE( BUF6( N ), 96080 ) TEMPRXN( NIRR, N )
                     END DO
                     BUF6( NHI )( 6:7 ) = ' /'
!                    WRITE( IOUT, 96040 ) ( BUF6( N ), N = NLO,  NHI ) 
                     call listout( NLO, NHI, 10, IOUT, BUF6 )
                  END DO
               END IF
            END DO
         END IF

         IF ( NUMOUTPOS .GT. 0 ) THEN
            NSETS = ( ( NUMOUTPOS - 1 ) / ( 5 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 5 * MAXCNT ) + 1
               NHI = MIN( ( NS * 5 * MAXCNT ), NUMOUTPOS )         
               WRITE( IOUT, 96640 ) NLO, NHI
               DO NIRR = NLO, NHI
                  WRITE( BUF12( NIRR ), 96100 ) COEFOUTPOS( NIRR )
               END DO
               BUF12( NHI )( 12:13 ) = ' /'
!              WRITE( IOUT, 96060 ) ( BUF12( NIRR ), NIRR = NLO, NHI )
               call listout( NLO, NHI, 5, IOUT, BUF12 )
            END DO
         END IF

         IF ( NUMOUTNEG .GT. 0 ) THEN
            NSETS = ( ( NUMOUTNEG - 1 ) / ( 5 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 5 * MAXCNT ) + 1
               NHI = MIN( ( NS * 5 * MAXCNT ), NUMOUTNEG )         
               WRITE( IOUT, 96660 ) NLO,  NHI
               DO NIRR = NLO, NHI
                  WRITE( BUF12( NIRR ), 96100 ) COEFOUTNEG( NIRR )
               END DO
               BUF12( NHI )( 12:13 ) = ' /'
!              WRITE( IOUT, 96060 ) ( BUF12( NIRR ), NIRR = NLO, NHI )
               call listout( NLO, NHI, 5, IOUT, BUF12 )
            END DO
         END IF

         IF ( NUMOUTIND .GT. 0 ) THEN
            NSETS = ( ( NUMOUTIND - 1 ) / ( 5 * MAXCNT ) ) + 1
            DO NS = 1, NSETS
               NLO = ( NS - 1 ) * ( 5 * MAXCNT ) + 1
               NHI = MIN( ( NS * 5 * MAXCNT ), NUMOUTIND )         
               WRITE( IOUT, 96680 ) NLO,  NHI
               DO NIRR = NLO, NHI
                 WRITE( BUF12( NIRR ), 96100 ) COEFOUTIND( NIRR )
               END DO
               BUF12( NHI )( 12:13 ) = ' /'
!              WRITE( IOUT, 96060 ) ( BUF12( NIRR ), NIRR = NLO, NHI )
               call listout( NLO, NHI, 5, IOUT, BUF12 )
            END DO
         END IF

         IF ( NUMTEMPS .GT. 0 ) THEN
            DO NIRR= 1, NUMTEMPS
               IF ( TEMPTERMS( NIRR ) .GT. 0 ) THEN
                  NSETS = ( ( TEMPTERMS( NIRR ) - 1 ) / ( 5 * MAXCNT ) ) + 1
                  DO NS = 1, NSETS
                     NLO = ( NS - 1 ) * ( 5 * MAXCNT ) + 1
                     NHI = MIN(  ( NS * 5 * MAXCNT ), TEMPTERMS( NIRR ) )         
                     WRITE( IOUT, 96700 ) NIRR, NLO, NHI
                     DO N = NLO, NHI
                        WRITE( BUF12( N ), 96100 ) TEMPCOEF( NIRR, N )
                     END DO
                     BUF12( NHI )( 12:13 ) = ' /'
!                    WRITE( IOUT, 96060 ) ( BUF12( N ), N = NLO, NHI )
                     call listout( NLO, NHI, 5, IOUT, BUF12 )
                 END DO
               END IF 
            END DO
         END IF

         IF ( NIRRVAR .GT. 0 ) THEN
            DO NIRR = 1, NIRRVAR
               IF ( NIRRRXNS( NIRR ) .GT. 0 ) THEN
                  NSETS = ( ( NIRRRXNS( NIRR ) - 1 ) / ( 5 * MAXCNT ) ) + 1
                  DO NS = 1, NSETS
                     NLO = ( NS - 1 ) * ( 5 * MAXCNT ) + 1
                     NHI = MIN( ( NS * 5 * MAXCNT ), NIRRRXNS( NIRR ) )         
                     WRITE( IOUT, 96720 ) NIRR,  NLO,  NHI
                     DO N = NLO, NHI
                        WRITE( BUF12( N ), 96100 ) IRRCOEF( NIRR, N )
                     END DO
                     BUF12( NHI )( 12:13 ) = ' /'
!                    WRITE( IOUT, 96060 ) ( BUF12( N ), N = NLO, NHI )
                     call listout( NLO, NHI, 5, IOUT, BUF12 )
                  END DO
               END IF 
            END DO
         END IF
      END IF

999   CONTINUE

      LINOUT = ''
      WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )
      LINOUT = '      END MODULE PA_DEFN'
      WRITE( IOUT, 94000 ) LINOUT( 1:LEN_TRIM( LINOUT ) )
      CLOSE( IOUT )

      RETURN 
     
C----------------------- FORMAT Statements -----------------------------

94000 FORMAT( A )     
94020 FORMAT( 
     & 'c  The following variables are used to control calls to', 
     &     ' the Integrated' /
     & 'c  Process Rate and Integrated Reaction Rate routines:' )
94040 FORMAT( 
     & 'c     LIPR = Flag for calculating Integrated Process Rates' /
     & 'c     LIRR = Flag for calculating Integrated Reaction Rates' )
94080 FORMAT(   6X, 'LOGICAL, PARAMETER :: LIPR = .TRUE.' / )
94100 FORMAT(   6X, 'LOGICAL, PARAMETER :: LIPR = .FALSE.' / )
94140 FORMAT(   6X, 'LOGICAL, PARAMETER :: LIRR = .TRUE.' / )
94160 FORMAT(   6X, 'LOGICAL, PARAMETER :: LIRR = .FALSE.' / )

C----------------------- FORMAT Statements -----------------------------

95020 FORMAT( 
     & 'c  Integrated Reaction Rates are not being computed.  All',
     &     ' associated'/
     & 'c  arrays will be set to the smallest possible dimension.' )

95040 FORMAT( 
     & 'c  Integrated Process Rates are not being computed.  All',
     &     ' associated'/
     & 'c  arrays will be set to the smallest possible dimension.' )

95120 FORMAT( 
     &'c  The following variables are used in the output file ', 
     &      'descriptions' )
95140 FORMAT( 
     &'c     NIPRVAR   = Number of IPR output variables'/
     &'c     NIRRVAR   = Number of IRR output variables'/
     &'c     NIPRVARD  = Dimension for IPR output variables'/
     &'c     NIRRVARD  = Dimension for IRR output variables'/
     &'c     IPRNAME   = Name of IPR output variable'/
     &'c     IRRNAME   = Name of IRR output variable'/
     &'c     IPRDESC   = Extended name of IPR output variable'/
     &'c     IRRDESC   = Extended name of IRR output variable' )

95160 FORMAT( /
     & 6X, 'INTEGER, PARAMETER :: NIPRVAR =', I4, ',',
     & 2X,                       'NIPRVARD =', I4 )

95180 FORMAT( /
     & 6X, 'INTEGER, PARAMETER :: NIRRVAR =', I4, ',',
     & 2X,                       'NIRRVARD =', I4 / )

95200 FORMAT( 
     & 6X, 'CHARACTER( 16 ) :: IPRNAME( NIPRVARD )' /
     & 6X, 'CHARACTER( 60 ) :: IPRDESC( NIPRVARD )' /
     & 6X, 'CHARACTER( 16 ) :: IRRNAME( NIRRVARD )' /
     & 6X, 'CHARACTER( 60 ) :: IRRDESC( NIRRVARD )' / )

95260 FORMAT( 
     & 'c  The following variables are used in the integrated process', 
     &     ' rate '/'c  computations.' )

95280 FORMAT( 
     & 'c    NPRCS      = No. of science processes that can be monitored' /
     & 'c    N_IPR_SPC  = Number of families for IPR' /
     & 'c    NCSAVE     = Number of CTM species concs to save for IPR' /
     & 'c    MXCGRID    = Max number of CTM species in an IPR family' /
     & 'c    N_IPR_SPCD = Dimension for number of families for IPR' /
     & 'c    NCSAVED    = Dimension for NCSAVE' /
     & 'c    MXCGRIDD   = Dimension for MXCGRID' /
     & 'c    NCGRID     = Array of no. of CTM species in an IPR family' /
     & 'c    IPROUT     = Array of IPR output variable indices' /
     & 'c    IPR2GRD    = Array of CTM species indices for IPR families' /
     & 'c    IPR2SAV    = Array of saved species indices for IPR families' /
     & 'c    SV2GRD     = Array of CTM indicies for saved concs' /
     & 'c    SPCOEF     = Array of coefficients for IPR familys' ) 

95300 FORMAT( /
     & 6X, 'INTEGER, PARAMETER :: NPRCS      =', I4 /
     & 6X, 'INTEGER, PARAMETER :: N_IPR_SPC  =', I4, ',',
     & 2X,                       'N_IPR_SPCD =', I4 /
     & 6X, 'INTEGER, PARAMETER :: NCSAVE     =', I4, ',',
     & 2X,                       'NCSAVED    =', I4 /
     & 6X, 'INTEGER, PARAMETER :: MXCGRID    =', I4, ',',
     & 2X,                       'MXCGRIDD   =', I4 / )

95320 FORMAT( 
     & 6X, 'CHARACTER( 16 ) :: PROCNAME( NPRCS )' /
     & 6X, 'LOGICAL            LPROCOUT( NPRCS )' //
     & 6X, 'INTEGER NCGRID ( N_IPR_SPCD )' /
     & 6X, 'INTEGER SV2GRD ( NCSAVED )' /
     & 6X, 'INTEGER IPROUT ( N_IPR_SPCD,NPRCS )' /
     & 6X, 'INTEGER IPR2GRD( N_IPR_SPCD,MXCGRIDD )' /
     & 6X, 'INTEGER IPR2SAV( N_IPR_SPCD,MXCGRIDD )' //
     & 6X, 'REAL    SPCOEF ( N_IPR_SPCD,MXCGRIDD )' )

95322 FORMAT( / )

95360 FORMAT( 
     & 'c  The following variables are used in the integrated reaction', 
     &     ' rate' / 'c  computations.' )

95380 FORMAT( 
     & 'c    LFULLIRR     = Flag for computing IRRs for every reaction' /
     & 'c    LPARTIRR     = Flag for computing IRRs for selected', 
     &                    ' reactions' /
     & 'c    NUMTEMPS     = No. of temporary intermediate IRRs to', 
     &                    ' compute' /
     & 'c    NUMOUTPOS    = No. of temp. IRR terms to add iff the', 
     &                   ' IRR > 0' /
     & 'c    NUMOUTNEG    = No. of temp. IRR terms to add iff the', 
     &                   ' IRR < 0' /
     & 'c    NUMOUTIND    = No. of temp. IRR terms to add regardless', 
     &                   ' of sign' /
     & 'c    MXNIRRRXNS   = Max no. of reaction terms in an output IRR' /
     & 'c    MXTEMPTRMS   = Max no. of reaction terms in a temp. IRR' /
     & 'c    NUMTEMPSD    = Dimension for number of temp IRRs' /
     & 'c    NUMOUTPOSD   = Dimension parameter for NUMOUTPOS' /
     & 'c    NUMOUTNEGD   = Dimension parameter for NUMOUTNEG' /
     & 'c    NUMOUTINDD   = Dimension parameter for NUMOUTIND' /
     & 'c    MXNIRRRXNSD  = Dimension parameter for MXNIRRRXNS' /
     & 'c    MXTEMPTRMSD  = Dimension parameter for MXTEMPTRMS' /
     & 'c    NIRRRXNS     = Vector of no. of reaction terms in an', 
     &                    ' output IRR' /
     & 'c    TEMPTERMS    = Vector of no. of terms in a temporary IRR' /
     & 'c    TEMPOUTPOS   = Vector of no. of IRR terms to add if', 
     &                    ' IRR > 0' /
     & 'c    TEMPOUTNEG   = Vector of no. of IRR terms to add if', 
     &                    ' IRR < 0' /
     & 'c    TEMPOUTIND   = Vector of no. of IRR terms to add', 
     &                    ' regardless of sign' /
     & 'c    INDXOUTPOS   = Vector of indices pointing to IRR output', 
     &                    ' variable' /
     & 'c    INDXOUTNEG   = Vector of indices pointing to IRR output', 
     &                    ' variable' /
     & 'c    INDXOUTIND   = Vector of indices pointing to IRR output', 
     &                    ' variable' /
     & 'c    COEFOUTPOS   = Vector of temp IRR coefficients' /
     & 'c    COEFOUTNEG   = Vector of temp IRR coefficients' /
     & 'c    COEFOUTIND   = Vector of temp IRR coefficients' /
     & 'c    TEMPRXN      = Array of reaction nos. for a temp. IRR' /
     & 'c    IRRRXN       = Array of reaction nos. for an output IRR' /
     & 'c    TEMPCOEF     = Array of reaction coefficients for a', 
     &                    ' temp IRR' /
     & 'c    IRRCOEF      = Array of reaction coefficients for an', 
     &                   ' output IRR' /
     & 'c    IRRSTEP      = Array of output IRRs for one time step' /
     & 'c    IRRBLK       = Array of output IRRs for a block of cells' /
     & 'c    IRROUT       = Array of IRRs to output' ) 

95420 FORMAT( / 6X,  'LOGICAL, PARAMETER :: LFULLIRR = .TRUE.' )
95440 FORMAT( / 6X,  'LOGICAL, PARAMETER :: LFULLIRR = .FALSE.' )  
95480 FORMAT( / 6X,  'LOGICAL, PARAMETER :: LPARTIRR = .TRUE.' )
95500 FORMAT( / 6X,  'LOGICAL, PARAMETER :: LPARTIRR = .FALSE.' ) 

95520 FORMAT( /
     & 6X, 'INTEGER, PARAMETER :: NUMTEMPS   =', I4, ',',
     & 2X,                       'NUMTEMPSD   =', I4 /
     & 6X, 'INTEGER, PARAMETER :: NUMOUTPOS  =', I4, ',',
     & 2X,                       'NUMOUTPOSD  =', I4 /
     & 6X, 'INTEGER, PARAMETER :: NUMOUTNEG  =', I4, ',',
     & 2X,                       'NUMOUTNEGD  =', I4 /
     & 6X, 'INTEGER, PARAMETER :: NUMOUTIND  =', I4, ',',
     & 2X,                       'NUMOUTINDD  =', I4 /
     & 6X, 'INTEGER, PARAMETER :: MXNIRRRXNS =', I4, ',',
     & 2X,                       'MXNIRRRXNSD =', I4 /
     & 6X, 'INTEGER, PARAMETER :: MXTEMPTRMS =', I4, ',',
     & 2X,                       'MXTEMPTRMSD =', I4 )

95542 FORMAT( /
     & 6X, 'INTEGER, PARAMETER :: BLKSIZED = BLKSIZE' )

95544 FORMAT( /
     & 6X, 'INTEGER, PARAMETER :: BLKSIZED = 1' )

95580 FORMAT( /
     & 6X, 'INTEGER NIRRRXNS  ( NIRRVARD )' /
     & 6X, 'INTEGER TEMPTERMS ( NUMTEMPSD )' /
     & 6X, 'INTEGER TEMPOUTPOS( NUMOUTPOSD )' /
     & 6X, 'INTEGER INDXOUTPOS( NUMOUTPOSD )' /
     & 6X, 'INTEGER TEMPOUTNEG( NUMOUTNEGD )' /
     & 6X, 'INTEGER INDXOUTNEG( NUMOUTNEGD )' /
     & 6X, 'INTEGER TEMPOUTIND( NUMOUTINDD )' /
     & 6X, 'INTEGER INDXOUTIND( NUMOUTINDD )' /
     & 6X, 'INTEGER IRRRXN    ( NIRRVARD,MXNIRRRXNSD )' /
     & 6X, 'INTEGER TEMPRXN   ( NUMTEMPSD,MXTEMPTRMSD )' //
     & 6X, 'REAL    COEFOUTPOS( NUMOUTPOSD )' /
     & 6X, 'REAL    COEFOUTNEG( NUMOUTNEGD )' /
     & 6X, 'REAL    COEFOUTIND( NUMOUTINDD )' /
     & 6X, 'REAL    TEMPCOEF  ( NUMTEMPSD,MXTEMPTRMSD )' /
     & 6X, 'REAL    IRRCOEF   ( NIRRVARD,MXNIRRRXNSD )' /
     & 6X, 'REAL    IRRSTEP   ( BLKSIZED,NIRRVARD )' /
     & 6X, 'REAL    IRRBLK    ( BLKSIZED,NIRRVARD )' )

! Common blocks format labels not used
!   Warning: Label 95781 defined but not used
!   Warning: Label 95760 defined but not used
!   Warning: Label 95740 defined but not used
!   Warning: Label 95720 defined but not used
!   Warning: Label 95700 defined but not used
!   Warning: Label 95322 defined but not used

!95700 FORMAT( 'c  Common Blocks:' )

!95720 FORMAT( 
!    & 5X, ' COMMON /PACHAR/ IPRNAME,' /
!    & 5X, '&                IPRDESC,' /
!    & 5X, '&                IRRNAME,' /
!    & 5X, '&                IRRDESC,' /
!    & 5X, '&                PROCNAME' / )
     
!95740 FORMAT( 
!    & 5X, ' COMMON /PALGCL/ LPROCOUT' / )

!95760 FORMAT( 
!    & 5X, ' COMMON /PAINTG/ NCGRID,' /
!    & 5X, '&                SV2GRD,' /
!    & 5X, '&                IPROUT,' /
!    & 5X, '&                IPR2GRD,' /
!    & 5X, '&                IPR2SAV,' /
!    & 5X, '&                NIRRRXNS,' /
!    & 5X, '&                TEMPTERMS,' /
!    & 5X, '&                TEMPOUTPOS,' /
!    & 5X, '&                TEMPOUTNEG,' /
!    & 5X, '&                TEMPOUTIND,' /
!    & 5X, '&                INDXOUTPOS,' /
!    & 5X, '&                INDXOUTNEG,' /
!    & 5X, '&                INDXOUTIND,' /
!    & 5X, '&                TEMPRXN,' /
!    & 5X, '&                IRRRXN' / )

!95781 FORMAT( 
!    & 5X, ' COMMON /PAREAL/ SPCOEF,' /
!    & 5X, '&                COEFOUTPOS,' /
!    & 5X, '&                COEFOUTNEG,' /
!    & 5X, '&                COEFOUTIND,' /
!    & 5X, '&                TEMPCOEF,' /
!    & 5X, '&                IRRCOEF,' /
!    & 5X, '&                IRRSTEP,' /
!    & 5X, '&                IRRBLK' / )
 
C----------------------- FORMAT Statements -----------------------------

96020 FORMAT( 5X, '&  ', 3A21 )
96040 FORMAT( 5X, '&  ', 10A7 )
96060 FORMAT( 5X, '&  ', 5A13 )
96080 FORMAT( I5, ',' )
96100 FORMAT( F11.4, ',' )
96110 FORMAT( 'c  Neither IPR nor IRR calculations are being done.' /
     &        'c  Additional data are not required' )
96120 FORMAT( 'c  Output file descriptor data' )
96140 FORMAT( 'c  IPR data' )
96160 FORMAT( 'c  IRR data' )
96200 FORMAT( /6X, 'INTEGER TERM' )
96220 FORMAT( /6X, 'DATA ( IPRNAME( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96240 FORMAT( /6X, 'DATA ( IPRDESC( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96260 FORMAT( /6X, 'DATA ( IRRNAME( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96280 FORMAT( /6X, 'DATA ( IRRDESC( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96290 FORMAT( /6X, 'DATA ( PROCNAME( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96300 FORMAT( /6X, 'DATA ( NCGRID( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96320 FORMAT( /6X, 'DATA ( SV2GRD( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96340 FORMAT( /6X, 'DATA ( IPROUT( ', I3, ', TERM ), TERM =', I4, ', ', I4, ' ) /' )
96360 FORMAT( /6X, 'DATA ( IPR2GRD( ', I3, ', TERM ), TERM =', I4, ', ', I4, ' ) /' )
96380 FORMAT( /6X, 'DATA ( IPR2SAV( ', I3, ', TERM ), TERM =', I4, ', ', I4, ' ) /' )
96400 FORMAT( /6X, 'DATA ( LPROCOUT( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96420 FORMAT( /6X, 'DATA ( SPCOEF( ', I3, ', TERM ), TERM =', I4, ', ', I4, ' ) /' )
96440 FORMAT( /6X, 'DATA ( NIRRRXNS( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96460 FORMAT( /6X, 'DATA ( TEMPTERMS( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96480 FORMAT( /6X, 'DATA ( TEMPOUTPOS( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96500 FORMAT( /6X, 'DATA ( TEMPOUTNEG( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96520 FORMAT( /6X, 'DATA ( TEMPOUTIND( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96540 FORMAT( /6X, 'DATA ( INDXOUTPOS( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96560 FORMAT( /6X, 'DATA ( INDXOUTNEG( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96580 FORMAT( /6X, 'DATA ( INDXOUTIND( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96600 FORMAT( /6X, 'DATA ( IRRRXN( ', I3, ', TERM ), TERM =', I4, ', ', I4, ' ) /' )
96620 FORMAT( /6X, 'DATA ( TEMPRXN( ', I3, ', TERM ), TERM =', I4, ', ', I4, ' ) /' )
96640 FORMAT( /6X, 'DATA ( COEFOUTPOS( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96660 FORMAT( /6X, 'DATA ( COEFOUTNEG( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96680 FORMAT( /6X, 'DATA ( COEFOUTIND( TERM ), TERM =', I4, ', ', I4, ' ) /' )
96700 FORMAT( /6X, 'DATA ( TEMPCOEF( ', I3, ', TERM ), TERM =', I4, ', ', I4, ' ) /' )
96720 FORMAT( /6X, 'DATA ( IRRCOEF( ', I3, ', TERM ), TERM =', I4, ', ', I4, ' ) /' )

      contains
C=======================================================================
         subroutine listout ( lo, hi, n_form, iout, buf )

C    format a list and write to iout
C    n_form can be 3, 5, or 10

         integer, intent (in ) :: lo, hi, n_form, iout
         character( * ), intent( in ) :: buf( * )

         character( 80 ) :: linout
!        character(  4 ) :: fstr
         character( 19 ) :: fstr
         integer i, j, k, n, rng, last

         select case ( n_form )
            case(  3 )   ! associated w/ buf20
!              fstr = '2021'         
               fstr = "( 5x, '&  ', 3a21 )"
            case(  5 )   ! associated w/ buf12
!              fstr = '2041'         
               fstr = "( 5x, '&  ', 5a13 )"
            case( 10 )   ! associated w/ buf6
!              fstr = '2061'         
               fstr = "( 5x, '&  ', 10a7 )"
            case default
               write( *,* ) '*** INVALID format ', n_form
               stop
         end select

         rng = hi - lo + 1
         last = mod( rng, n_form )
         i = lo
         do k = 1, rng / n_form
            j = i + n_form - 1
            write( linout, fstr ) ( buf( n ), n = i, j )
            write( iout, '( a )' ) linout( 1:len_trim( linout ) )
            i = j + 1
         end do
         if ( last .gt. 0 ) then
         j = i + last - 1
         write( linout, fstr ) ( buf( n ), n = i, j )
         write( iout, '( a )' ) linout( 1:len_trim( linout ) )
         end if

2021     format( 5x, '&  ', 3a21 )
2041     format( 5x, '&  ', 5a13 )
2061     format( 5x, '&  ', 10a7 )

         end subroutine listout

      END SUBROUTINE PA_WRTPADEFN
