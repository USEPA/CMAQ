
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

      MODULE DEGRADE_SETUP_TOX
C**********************************************************************
C
C  FUNCTION:  Define arrays that identify species within CGRID used
C             based on input arrays
C
C  REVISION HISTORY: 07/29/05 : B.Hutzell - Initial version
C                    06 May 11: B.Hutzell: convert for Namelist redesign
C                    09 May 11: B.Hutzell: enabled a degraded species to
C                               be missing from namelists
C
C**********************************************************************

      USE GRID_CONF, ONLY: BLKSIZE ! vertical and horizontal domain specs
      USE CGRID_SPCS               ! CGRID species number and offsets
      USE UTILIO_DEFN              ! IOAPI declarations and definitions
      USE DEGRADE_PARAMETERS

      IMPLICIT NONE

C.....INCLUDES:

      INCLUDE SUBST_CONST         ! constants

      REAL(8)              :: EFFECTIVE_ZERO
      REAL(8)              :: LOG_EFFECTIVE_ZERO
      REAL(8)              :: INFINITY


C..arrays to store indices to CGRID

      INTEGER, ALLOCATABLE :: RXTANT_MAP   ( : )
      INTEGER, ALLOCATABLE :: DEGRADE_INDEX( : )
      INTEGER, ALLOCATABLE :: PROD_MAP( :,: )
      INTEGER, ALLOCATABLE :: RAD_MAP( :,: )
      INTEGER, ALLOCATABLE :: RAD2_MAP( :,:,: )
      INTEGER, ALLOCATABLE :: PHOTO_MAP( :,: )
      
      INTEGER :: DEGRADE_STEP
      INTEGER :: SA_DEGRADE_STEP = 0      

C..saved cell concentrations

      REAL( 8 ), ALLOCATABLE :: OLD_CONC( : )
      REAL( 8 ), ALLOCATABLE :: NEW_CONC( : )
#ifdef sens
      REAL,      ALLOCATABLE :: SENS_CONC( :,: )
#endif
      REAL( 8 )              :: TEMP_AIR            ! cell temperature [ K ]
      REAL( 8 )              :: CONC_AIR            ! cell air number density [ 1/CM^3 ]
      REAL( 8 )              :: CONC_N2             ! cell N2 number density [ 1/CM^3 ]
      REAL( 8 )              :: CONC_O2             ! cell O2 number density [ 1/CM^3 ]
      REAL( 8 )              :: CONC_CH4            ! cell CH4 number density [ 1/CM^3 ]
      REAL( 8 )              :: CONC_H2             ! cell H2 number density [ 1/CM^3 ]
      REAL( 8 )              :: CONC_H2O            ! cell H2O number density [ 1/CM^3 ]

C..saved blocked cells concentrations

      REAL( 8 ), ALLOCATABLE :: PREV_CONC( :,: )   
      REAL( 8 ), ALLOCATABLE :: CURR_CONC( :,: )
#ifdef sens
      REAL,      ALLOCATABLE :: SENS_BLK( :,:,: )
#endif
      REAL( 4 ), ALLOCATABLE :: AERO_BLK ( :,: )
      REAL( 8 ), ALLOCATABLE :: TEMP( : )           ! cell temperature [ K ]
      REAL( 8 ), ALLOCATABLE :: PRESS( : )          ! cell Pressure    [ Pa ]
      REAL( 8 ), ALLOCATABLE :: INV_TEMP( : )       ! cell inverse temperature [ 1/K ]
      REAL( 8 ), ALLOCATABLE :: NUMB_DENS( : )      ! cell air number density [ 1/CM^3 ]
      REAL( 8 ), ALLOCATABLE :: NUMB_H2O( : )       ! cell H2O number density [ 1/CM^3 ]
      REAL( 8 ), ALLOCATABLE :: CONV_FACT( : )      ! conversion factor from ppm to molecules/cm^3

      REAL(8), ALLOCATABLE :: CHANGE_CONC( : )    ! cell concentration changes predicted by degrade routine
      REAL(8), ALLOCATABLE :: DELT_CONC( :,: )    ! block concentration changes predicted by degrade routine

      REAL(8), ALLOCATABLE :: CELL_RKI( :,: )     ! cell rate constant for species
      REAL(8), ALLOCATABLE :: RATE_CONST( :,:,: ) ! block rate constants for species
      REAL(8), ALLOCATABLE :: RATE_YIELD( :,: )   ! product yield from reaction

#ifdef isam
      INTEGER                      :: ISAM_DEGRADED_SPC     ! number of ISAM species with degradation
      INTEGER,         ALLOCATABLE :: ISAM_DEGRADE_MAP( : ) ! index in ISAM species array to extract conc
      INTEGER,         ALLOCATABLE :: ISAM_TO_DEGRADED( : ) ! index in REACT concentation array
      INTEGER,         ALLOCATABLE :: ISAM_TO_REACTANT( : ) ! index in REACT data array to determine degradation
      REAL(8),         ALLOCATABLE :: CELL_ISAM( :,: )      ! concentrations apportioned to sources
      CHARACTER( 16 ), ALLOCATABLE :: ISAM_DEGRADED( : )    ! names of degraded isam species
#endif

      LOGICAL, ALLOCATABLE :: IS_AERO_ORGANIC( : )  ! is aerosol species as OA and not a tracer


      INTEGER :: NCELLS  = 0                        ! number of cells in block
C.. variables used to write cell results
      INTEGER :: DEG_LAY = 0
      INTEGER :: DEG_ROW = 0
      INTEGER :: DEG_COL = 0
C.. variable used to write a cell in a block
      INTEGER :: ICELL_WRITE = 1
      LOGICAL :: WRITE_BLOCK = .FALSE.
      LOGICAL, ALLOCATABLE :: WRITE_CELL( : )       ! write cell value used for debugging and QA


C**********************************************************************

      CONTAINS
      

         SUBROUTINE DEGRADE_MAP( JDATE, JTIME )
C**********************************************************************
C
C  Function:  Determine CGRID indices used in DEGRADE routine.
C             Check decay and degradation rates for negative values.
C
C  CALLED BY: INIT_DEGRADE
C
C**********************************************************************

         USE RXNS_DATA
#ifdef mpas
      use util_module, only : index1, upcase
#endif

         IMPLICIT NONE

C.....INCLUDES:


C.....ARGUMENTS:

         INTEGER, INTENT( IN ) :: JDATE        ! current model date , coded YYYYDDD
         INTEGER, INTENT( IN ) :: JTIME        ! current model time , coded HHMMSS

C.....PARAMETERS:

         REAL(8), PARAMETER :: TEMP_298K  = 298.15        ! K

C.....LOCAL VARIABLES:

         CHARACTER(  16 ) :: PNAME =  'DEGRADE_MAP    '     ! name of routine
         CHARACTER(  16 ) :: EMTPTY
         CHARACTER(  16 ) :: WNAME, XNAME                   ! SCRATCH variables
         CHARACTER(  16 ) :: VNAME( N_PROCESSES+1 )         ! SCRATCH variable
         CHARACTER( 128 ) :: XMSG = 'FATAL ERROR in DEGRADE_SETUP'

         INTEGER :: MARKER, N, M       ! indexes
         INTEGER :: I, J, K, L         ! loop counters
         INTEGER :: LEN_NAME           ! number of nonblank characters in species name
         INTEGER :: ICOUNT 
          
         REAL(8), PARAMETER :: INV_T298K = 1.0D0 / TEMP_298K   ! K^-1

         LOGICAL, SAVE :: INITIALIZED = .FALSE.

C.....EXTERNAL FUNCTIONS:

C**********************************************************************

         IF( INITIALIZED )RETURN
      
         INITIALIZED = .FALSE.
C..arrays to store indices to CGRID

         ALLOCATE( RXTANT_MAP   ( N_REACT ) )
         ALLOCATE( DEGRADE_INDEX( N_REACT ) )
         ALLOCATE( PROD_MAP  ( N_PROCESSES, N_REACT  ) )
         ALLOCATE( RAD_MAP   ( N_BI_LOSS + N_LANHIN_LOSS, N_REACT ) )
         ALLOCATE( RAD2_MAP  ( 2, N_TRI_LOSS, N_REACT ) )
         ALLOCATE( PHOTO_MAP ( N_PHOTO_LOSS, N_REACT ) )

         ALLOCATE( RATE_YIELD( N_PROCESSES, N_REACT ) )

C..Initialize maps

         RXTANT_MAP    = -1
         DEGRADE_INDEX = -1
         RAD_MAP     = -1
         RAD2_MAP    = -1
         PROD_MAP    = -1
         PHOTO_MAP   = -1

         RATE_YIELD = 1.0D0

C..save number of photolysis rates in mechanism

         N_PHOTO_TAB = NPHOTAB

C..Quality control on pairs of Reactant and Products

         WRITE( LOGDEV,* ) 'Comments on Species in degradation routines'

         N_REACT_FOUND = 0

         LOOP_REACT : DO I = 1, N_REACT

            VNAME( 1 ) = REACT( I )
            

            VNAME( UNI_START+1  :  UNI_STOP+1 )   = UNI_PROD  ( 1:N_UNI_LOSS, I )
            VNAME( BI_START+1   :   BI_STOP+1 )   = BI_PROD   ( 1:N_BI_LOSS, I  )
            VNAME( TRI_START+1  :  TRI_STOP+1 )   = TRI_PROD  ( 1:N_TRI_LOSS, I )
            VNAME( PHOTO_START+1:PHOTO_STOP+1 )   = PHOTO_PROD( 1:N_PHOTO_LOSS, I )
            VNAME( LANHIN_START+1:LANHIN_STOP+1 ) = LH_PROD   ( 1:N_LANHIN_LOSS, I )

            RATE_YIELD( UNI_START:UNI_STOP, I ) = UNI_YIELD( 1:N_UNI_LOSS, I )
            RATE_YIELD( BI_START:BI_STOP,   I ) = BI_YIELD ( 1:N_BI_LOSS, I  )
            RATE_YIELD( TRI_START:TRI_STOP, I ) = TRI_YIELD( 1:N_TRI_LOSS, I )
            RATE_YIELD( PHOTO_START:PHOTO_STOP, I ) = PHOTO_YIELD( 1:N_PHOTO_LOSS, I )
            RATE_YIELD( LANHIN_START:LANHIN_STOP, I ) = LH_YIELD( 1:N_LANHIN_LOSS, I )


            CALL UPCASE( VNAME( 1 ) )

            LEN_NAME = LEN_TRIM( VNAME( 1 ) )

            IF ( LEN_NAME < 1 ) THEN
               WRITE( LOGDEV,* ) 'A Reactant has no name.'
     &              // ' Check file degrade module'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            ENDIF
            
            DO K = 1, N_REACT

               WNAME = REACT( K )
               CALL UPCASE( WNAME )
               
               DO J = 2, N_PROCESSES+1
                  XNAME = VNAME( J ) 
                  IF( TRIM( WNAME ) == TRIM( XNAME ) )THEN
                     WRITE( XMSG,* ) 'ERROR: ',
     &                 TRIM( VNAME( 1 ) ), ' is a destroyed  and produced.'
     &                 // ' The property is not allowed because it'
     &                 // ' brakes linear assumptions used.'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
                  END IF
               END DO    
            
            END DO

            DO K = 1, N_BI_LOSS
               IF ( VNAME( 1 ) == BICAUSE( K, I ) ) THEN
                  WRITE( XMSG,* ) 'ERROR: ',
     &                 TRIM( VNAME( 1 ) ), ' has same name'
     &                 // ' as a species causing its bimolecular loss.'
     &                 // ' This breaks linear assumptions used.'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               ENDIF
            ENDDO

            DO K = 1, N_TRI_LOSS
               IF ( VNAME( 1 ) == TRICAUSE( 1, K, I ) .OR.
     &              VNAME( 1 ) == TRICAUSE( 2, K, I ) ) THEN
                  WRITE( XMSG,* ) 'ERROR: ',
     &                 TRIM( VNAME( 1 ) ), ' has same name as'
     &                 // ' a species causing its trimolecular loss.'
     &                 // ' This breaks linear assumptions used.'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               ENDIF
            ENDDO

            DO K = 1, N_LANHIN_LOSS
               IF ( VNAME( 1 ) == LHCAUSE( K, I ) ) THEN
                  WRITE( XMSG,* ) 'ERROR: ',
     &                 VNAME( 1 )( 1:LEN_NAME ), ' has'
     &                 // ' same name as a species causing its Langmuir'
     &                 // '-Hinshwood loss. This breaks linear assumptions used. '
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               ENDIF
            ENDDO



C..Set up indices that point to concentrations in CGRID.

            DO 20 J = 1, N_PROCESSES+1

               WNAME = VNAME( J )      ! note that reactant occupies VNAME(1)
               LEN_NAME = LEN_TRIM( WNAME )
               CALL UPCASE( WNAME )

               IF ( LEN_NAME > 0 ) THEN ! search gas species for index
                  N = INDEX1( WNAME, N_GC_SPC, GC_SPC )
                  MARKER = GC_STRT

                  IF ( N == 0 ) THEN  ! search aerosol them  non-reactive species for index

                     N = INDEX1( WNAME, N_AE_SPC, AE_SPC )
                     MARKER = AE_STRT
                     IF ( N == 0 ) THEN
                        N = INDEX1( WNAME, N_NR_SPC, NR_SPC )
                        MARKER = NR_STRT
                        IF ( N == 0 ) THEN
#ifdef verbose_gas
                           WRITE( LOGDEV,'(a)' ) TRIM( WNAME ), ' is not '
     &                          // 'in gas or nonreactive species table.'
     &                          // 'its loss processes not calculated '
#endif     
                           RXTANT_MAP( I ) = -1
                           CYCLE
                        ENDIF
                     ENDIF

                  ENDIF
         
                  N_REACT_FOUND   = N_REACT_FOUND + 1
               ELSE
                  VNAME( J ) = 'NONE'
                  CYCLE
               ENDIF

C..write degrade data table

               IF ( N_REACT_FOUND == 1 ) THEN
                  WRITE( LOGDEV,* ) 'TABLE on Degradation Simulated.'
                  WRITE( LOGDEV,* ) 'Note: Rates use units of cm, sec, and molecules.'
                  WRITE( LOGDEV,* )
                  WRITE( LOGDEV,1600 )
               ENDIF

C..set map values

               IF ( J < 2 ) THEN
                  DEGRADE_INDEX( N_REACT_FOUND ) = I
                  RXTANT_MAP ( I ) = N + MARKER - 1
               ELSE
                  PROD_MAP( J-1, I ) = N + MARKER - 1
               ENDIF

20          CONTINUE

C..cycle N_REACT LOOP

           IF( RXTANT_MAP( I ) .LT. 1 )CYCLE LOOP_REACT

C..check UNIMOLECULAR decay rates

            K = 0

            DO J = 1, N_UNI_LOSS

               IF ( UNIRATE( J, I ) < 0.0 ) THEN
                  WRITE( LOGDEV,* ) 'Species ', REACT( I ), ' has a'
     &                 // 'negative rate for unimolecular decay.'
     &                 // 'Check degrade module'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               ENDIF

               WRITE( LOGDEV,1100 ) VNAME( 1 ), RXTANT_MAP( I ),
     &              ' Unimolecular ',
     &              UNIRATE( J, I ) * TEMP_298K**UNI_TEXP( J, I )
     &              * EXP( -UNI_ACT( J, I ) * INV_T298K ),
     &              VNAME( J+1 ), PROD_MAP( J, I )

            ENDDO

            K = K + N_UNI_LOSS

C..locating degradation causes in CGRID

            DO 40 J = 1, N_BI_LOSS

C..checking degradation rates

               IF ( BIRATE( J, I ) < 0.0 ) THEN
                  WRITE( LOGDEV,* ) 'Species ', REACT( I ), 'has a negative'
     &                 // ' rate for degradation by ', WNAME( 1:LEN_NAME ), '.'
     &                 // ' Check degrade module.'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               ENDIF

               WNAME = BICAUSE( J, I )

               CALL UPCASE( WNAME )

               LEN_NAME = LEN_TRIM( WNAME )

               IF ( LEN_NAME < 1 ) CYCLE
               MARKER = 0

               IF ( WNAME == 'DENSITY' .OR. WNAME == 'M' ) THEN      ! special case rate proportion to air density
                  RAD_MAP( J, I ) = 9999
               ENDIF

               IF ( WNAME == 'N2' ) THEN      ! special case rate proportion to molecular nitrogen
                  RAD_MAP( J, I ) = 9998
               ENDIF

               IF ( WNAME == 'O2' ) THEN      ! special case rate proportion to molecular oxygen
                  RAD_MAP( J, I ) = 9997
               ENDIF

               IF ( WNAME == 'CH4' ) THEN      ! special case rate proportion to methane
                  RAD_MAP( J, I ) = 9996
               ENDIF

               IF ( WNAME == 'H2' ) THEN      ! special case rate proportion to hydrogen
                  RAD_MAP( J, I ) = 9995
               ENDIF

               IF ( WNAME == 'H2O' ) THEN      ! special case rate proportion to water vapor
                  RAD_MAP( J, I ) = 9994
               ENDIF

               IF ( RAD_MAP( J, I ) < 0 ) THEN ! search model species
                  N = INDEX1( WNAME, N_GC_SPC, GC_SPC )   ! gas species for index
                  IF ( N == 0 ) THEN                      ! non-reactive species
                     N = INDEX1( WNAME, N_NR_SPC, NR_SPC )
                     IF ( N > 0 ) THEN
                        MARKER = NR_STRT
                     END IF
                  ELSE   
                     MARKER = GC_STRT
                  ENDIF 
                  RAD_MAP( J, I ) = N + MARKER - 1
               END IF   

               IF ( RAD_MAP( J, I ) > 0 ) THEN
                  WRITE( LOGDEV,1200 ) VNAME( 1 ), RXTANT_MAP( I ),
     &                 ' Bimolecular ', WNAME, RAD_MAP( J, I ),
     &                 BIRATE( J, I ) * TEMP_298K**BI_TEXP( J, I )
     &                 * EXP( -BI_ACT( J, I ) * INV_T298K ),
     &                 VNAME( J+K+1 ), PROD_MAP( J+K, I )
               ELSE
                   WRITE( LOGDEV,1200 ) VNAME( 1 ), RXTANT_MAP( I ),
     &                 'NOT INCLUDED', WNAME, RAD_MAP( J, I ),
     &                  BIRATE( J,I ) * TEMP_298K**BI_TEXP( J, I )
     &                  * EXP( -BI_ACT( J, I ) * INV_T298K ),
     &                  VNAME( J+K+1 ), PROD_MAP( J+K, I )
               END IF

40          CONTINUE

            K = K + N_BI_LOSS

            DO 50 J = 1, N_TRI_LOSS

C..checking degradation rates

               IF ( TRIRATE( J, I ) < 0.0D0 ) THEN
                  WRITE( LOGDEV,* ) 'Species ', REACT( I ), 'has a negative'
     &                 // ' rate for trimolecular degradation.'
     &                 // ' Check degrade module.'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               ENDIF

               ICOUNT = 0
               
               DO 60 L = 1, 2

                  WNAME = TRICAUSE( L, J, I )

                  CALL UPCASE( WNAME )

                  LEN_NAME = LEN_TRIM( WNAME )
    
                  IF ( LEN_NAME < 1 ) CYCLE

                  ICOUNT  = ICOUNT + 1
                  MARKER  = 0

                  IF ( WNAME == 'DENSITY' .OR. WNAME == 'M' ) THEN      ! special case rate proportion to air density
                     RAD2_MAP( L, J, I ) = 9999
                  ENDIF
              
                  IF ( WNAME == 'N2' ) THEN      ! special case rate proportion to molecular nitrogen
                     RAD2_MAP( L, J, I  ) = 9998
                  ENDIF
              
                  IF ( WNAME == 'O2' ) THEN      ! special case rate proportion to molecular oxygen
                     RAD2_MAP( L, J, I  ) = 9997
                  ENDIF
              
                  IF ( WNAME == 'CH4' ) THEN      ! special case rate proportion to methane
                     RAD2_MAP( L, J, I  ) = 9996
                  ENDIF
              
                  IF ( WNAME == 'H2' ) THEN      ! special case rate proportion to hydrogen
                     RAD2_MAP( L, J, I  ) = 9995
                  ENDIF
              
                  IF ( WNAME == 'H2O' ) THEN      ! special case rate proportion to water vapor
                     RAD2_MAP( L, J, I  ) = 9994
                  ENDIF

                  IF ( RAD2_MAP( L, J, I  ) < 0 ) THEN ! search model species
                     N = INDEX1( WNAME, N_GC_SPC, GC_SPC )   ! gas species for index
                     IF ( N == 0 ) THEN                      ! non-reactive species
                        N = INDEX1( WNAME, N_NR_SPC, NR_SPC )
                        IF ( N > 0 ) THEN
                           MARKER = NR_STRT
                        END IF
                    ELSE   
                       MARKER = GC_STRT
                    ENDIF 
                    RAD2_MAP( L, J, I ) = N + MARKER - 1
                 END IF   

60             CONTINUE

               IF ( RAD2_MAP( 1, J, I ) > 0 .AND. RAD2_MAP( 2, J, I ) > 0 ) THEN
                  WRITE( LOGDEV,1300 ) VNAME( 1 ), RXTANT_MAP( I ),
     &                 ' Trimolecular ', TRICAUSE( 1, J, I ), RAD2_MAP( 1, J, I ),
     &                 TRICAUSE( 2, J, I ), RAD2_MAP( 2, J, I ),
     &                 TRIRATE( J, I ) * TEMP_298K**TRI_TEXP( J, I )
     &                 * EXP( -TRI_ACT( J, I ) * INV_T298K ),
     &                 VNAME( J+K+1 ), PROD_MAP( J+K, I )
               ELSE IF ( ICOUNT .GT. 0 ) THEN
                  WRITE( LOGDEV,1300 ) VNAME( 1 ), RXTANT_MAP( I ),
     &                 ' NOT INCLUDED ', TRICAUSE( 1, J, I ), RAD2_MAP( 1, J, I ),
     &                 TRICAUSE( 2, J, I ), RAD2_MAP( 2, J, I ),
     &                 TRIRATE( J, I ) * TEMP_298K**TRI_TEXP( J, I )
     &                 * EXP( -TRI_ACT( J, I ) * INV_T298K ),
     &                 VNAME( J+K+1 ), PROD_MAP( J+K, I )
               ENDIF

50          CONTINUE

            LEN_NAME = LEN_TRIM( REACT( I ) )

            K = K + N_TRI_LOSS

            DO 70 J = 1, N_PHOTO_LOSS

               WNAME = PHOTO_NAME( J, I )

               CALL UPCASE( WNAME )

               N = INDEX1( WNAME, NPHOTAB, PHOTAB )

               IF ( LEN_TRIM( WNAME ) < 2 ) CYCLE

               IF ( N < 1 ) THEN
                  WRITE( LOGDEV,* ) 'Photolysis rate, ', WNAME, ' for ',
     &                 REACT( I )( 1:LEN_NAME ),
     &                 'is not JTABLE and is not included. '
                  CYCLE
               ENDIF

               PHOTO_MAP( J, I ) = N

               WRITE( LOGDEV,1400 ) VNAME( 1 ), RXTANT_MAP( I ),
     &              ' Photolysis ', PHOTAB( N ), ' ', 'times', ' ',
     &              A_PHOTO( J, I ),
     &              VNAME( J+K+1 ), PROD_MAP( J+K, I )

70          CONTINUE

            K = K + N_PHOTO_LOSS

C..locating degradation causes in CGRID

            DO 80 J = 1, N_LANHIN_LOSS

C..checking degradation rates

               IF ( LHRATE( J, I ) < 0.0 ) THEN
                  WRITE( LOGDEV,* ) 'Species ', REACT( I ), 'has a negative'
     &                 // ' rate for degradation by ', WNAME( 1:LEN_NAME ), '.'
     &                 // ' Check degrade module.'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               ENDIF

               WNAME = LHCAUSE( J, I )

               CALL UPCASE( WNAME )

               LEN_NAME = LEN_TRIM( WNAME )

               IF ( LEN_NAME < 1 ) CYCLE

               MARKER = 0
               IF ( WNAME == 'DENSITY' .OR. WNAME == 'M' ) THEN      ! special case rate proportion to air density
                  RAD_MAP( J + N_BI_LOSS, I ) = 9999
               ENDIF

               IF ( WNAME == 'N2' ) THEN      ! special case rate proportion to molecular nitrogen
                  RAD_MAP( J + N_BI_LOSS, I ) = 9998
               ENDIF

               IF ( WNAME == 'O2' ) THEN      ! special case rate proportion to molecular oxygen
                  RAD_MAP( J + N_BI_LOSS, I ) = 9997
               ENDIF

               IF ( WNAME == 'CH4' ) THEN      ! special case rate proportion to methane
                  RAD_MAP( J + N_BI_LOSS, I ) = 9996
               ENDIF

               IF ( WNAME == 'H2' ) THEN      ! special case rate proportion to hydrogen
                  RAD_MAP( J + N_BI_LOSS, I ) = 9995
               ENDIF

               IF ( WNAME == 'H2O' ) THEN      ! special case rate proportion to water vapor
                  RAD_MAP( J + N_BI_LOSS, I ) = 9994
               ENDIF

               IF ( RAD_MAP( J + N_BI_LOSS, I ) < 0 ) THEN ! search model species
                  N = INDEX1( WNAME, N_GC_SPC, GC_SPC )   ! gas species for index
                  IF ( N == 0 ) THEN                      ! non-reactive species
                     N = INDEX1( WNAME, N_NR_SPC, NR_SPC )
                     IF ( N > 0 ) THEN
                        MARKER = NR_STRT
                     END IF
                  ELSE   
                     MARKER = GC_STRT
                  ENDIF 
                  RAD_MAP( J + N_BI_LOSS, I ) = N + MARKER - 1
               END IF   

               IF ( RAD_MAP( J + N_BI_LOSS, I ) > 0 ) THEN
                  WRITE( LOGDEV,1200 ) VNAME( 1 ), RXTANT_MAP( I ),
     &                 ' Langm-Hinsh ', WNAME, RAD_MAP( J + N_BI_LOSS, I  ),
     &                 LHRATE( J, I ) * LH_EQU( J, I ),
     &                 VNAME( J+K+1 ), PROD_MAP( J+K, I )
               ELSE
                  WRITE( LOGDEV,1200 ) VNAME( 1 ), RXTANT_MAP( I ),
     &                 'NOT INCLUDED', WNAME, RAD_MAP( J + N_BI_LOSS, I ),
     &                 LHRATE( J, I ) * LH_EQU( J, I ),
     &                 VNAME( J+K+1 ), PROD_MAP( J+K, I )
               ENDIF

80          CONTINUE

         END DO LOOP_REACT

         IF( N_REACT_FOUND .LT. 1 )RETURN

         WRITE( LOGDEV,'(A)' ) 'Note: If INDEX of CAUSE A OR B equals -1, the '
     &        // 'process is dropped from degradation '
     &        // 'calculation.'

         WRITE( LOGDEV,* ) BLANK

#ifdef isam         
         CALL SA_DEGRADE_INIT
#endif

1000     FORMAT(A20,1X,A5,1X,A20,1X,2(A20,1X,A5,1X),A12,1X,A20,1X,A5)
1100     FORMAT(A20,1X,I5,1X,A20,1X,2(21X,6X),ES12.4,1X,A20,1X,I5)
1200     FORMAT(A20,1X,I5,1X,A20,1X,A20,1X,I5,1X,21X,6X,ES12.4,1X,A20,1X,I5)
1300     FORMAT(A20,1X,I5,1X,A20,1X,2(A20,1X,I5,1X),ES12.4,1X,A20,1X,I5)
1400     FORMAT(A20,1X,I5,1X,A20,1X,2(A20,1X,A5,1X),ES12.4,1X,A20,1X,I5)
1600     FORMAT('       DEGRADED      ',' Index',
     &              '       Process      ','        Cause A      ',
     &              ' Index', '       Cause B      ',' Index',
     &              '    Rate at 298K    ', '       Product      ',
     &              ' Index')

         RETURN

         END SUBROUTINE DEGRADE_MAP

#ifdef isam

         SUBROUTINE SA_DEGRADE_INIT

           USE SA_DEFN
C Initialize arrays and maps that relate ISAM species to degaded species
C
C         Called by DEGRADE_MAP

           IMPLICIT NONE

C..Includes: None
 
           CHARACTER( 16 ), PARAMETER :: PNAME = 'SA_DEGRAGE_INIT'     ! Program name
 
           INTEGER :: I, J, RXN, IP, IL 
           INTEGER :: IOSTAT
           INTEGER :: C, L, R, S   ! Loop indices
           INTEGER :: SPC          ! array index
           INTEGER :: IOS


           CHARACTER( 132 ) :: MSG           ! Message text
! temporary arrays to set maps between isam to chemistry species
           INTEGER, ALLOCATABLE :: ISAM_SPC_IDX ( : )
           INTEGER, ALLOCATABLE :: ISAM_2_DEGRAD( : )
           INTEGER, ALLOCATABLE :: REACT_INDEX  ( : )
           LOGICAL, ALLOCATABLE :: NOT_DEGRADED ( : )

           CHARACTER(16), ALLOCATABLE :: FIND_IN_ISAM( : )

         
C=======================================================

                   
           ALLOCATE( ISAM_2_DEGRAD( NSPC_SA + 1 ) )
           ALLOCATE( ISAM_SPC_IDX( NSPC_SA + 1 ) )
           ALLOCATE( REACT_INDEX( NSPC_SA + 1 ) )
           ALLOCATE( NOT_DEGRADED( NSPC_SA + 1 ) )
           ALLOCATE( FIND_IN_ISAM( NSPC_SA + 1 ) )

! Identify species index in ISAM array
           ISAM_SPC_IDX  = 0
           ISAM_2_DEGRAD = 0
           REACT_INDEX   = 0
           NOT_DEGRADED = .TRUE.
           FIND_IN_ISAM = ' '
           
           DO S = 1, NSPC_SA
              FIND_IN_ISAM( S ) = ISAM_SPEC( S,OTHRTAG )
              ISAM_SPC_IDX( S ) = S
           END DO

! find tagged species in REACT array 
           SPC = NSPC_SA
           ISAM_DEGRADED_SPC = 0
           DO S = 1, NSPC_SA
              R  = INDEX1( TRIM(FIND_IN_ISAM( S )), N_REACT, REACT )
              IF ( R .LE. 0 ) THEN
                 MSG = 'ISAM SPECIES: ' 
     &              // TRIM( FIND_IN_ISAM( S ) ) 
     &              // ' not found in  REACT array  '
!                CALL M3WARN( PNAME, 0, 0, MSG )
                 CYCLE
              END IF
              IF( RXTANT_MAP( R ) .LT. 1 )CYCLE
              ISAM_DEGRADED_SPC  = ISAM_DEGRADED_SPC + 1           
              REACT_INDEX  ( S ) = R
              ISAM_2_DEGRAD( S ) = RXTANT_MAP( R )
              NOT_DEGRADED ( S ) = .FALSE.
           END DO
           
           IF( ANY(  .NOT. NOT_DEGRADED ) )THEN
C..Save pointer for isam species found in chemistry species
               ALLOCATE( CELL_ISAM( NTAG_SA,ISAM_DEGRADED_SPC ) )
               CELL_ISAM = 0.0D0
               ALLOCATE( ISAM_TO_DEGRADED( ISAM_DEGRADED_SPC ) )
               ALLOCATE( ISAM_DEGRADE_MAP( ISAM_DEGRADED_SPC ) )
               ALLOCATE( ISAM_TO_REACTANT( ISAM_DEGRADED_SPC ) )
               ALLOCATE( ISAM_DEGRADED   ( ISAM_DEGRADED_SPC ) )
               ISAM_TO_DEGRADED = -1
               ISAM_DEGRADE_MAP = -1
               ISAM_DEGRADED    = "XundefinedX"
               
               WRITE(LOGDEV,'(/A)')'Below isam species have a linear decay based on photochemistry '
               WRITE(LOGDEV,'("IDX, ISAM_SPC, IDX, DEGRADE_SPC  ")')
               L = 0 
               DO S = 1, NSPC_SA
                   IF ( .NOT. NOT_DEGRADED( S ) ) THEN
                       L = L + 1
                       C = ISAM_SPC_IDX ( S )
                       R = ISAM_2_DEGRAD( S )
                       SPC = REACT_INDEX( S )
                       ISAM_DEGRADE_MAP( L ) = ISAM_SPC_IDX( S )
                       ISAM_TO_DEGRADED( L ) = ISAM_2_DEGRAD( S )
                       ISAM_TO_REACTANT( L ) = REACT_INDEX( S )     
                       ISAM_DEGRADED   ( L ) = REACT( SPC )
                       WRITE(LOGDEV,'(I3,1X,A16,1x,I3,1X,A16)') 
     &                   C, FIND_IN_ISAM( S ), R, REACT( SPC )
                   END IF
               END DO
               IF( L .NE. ISAM_DEGRADED_SPC )THEN
                   MSG = 'ERROR mapping isam to chemistry species: inconsistent number found'
                   CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
               END IF
           ELSE
               MSG = 'NO ISAM species are degraded '
               CALL M3WARN ( PNAME, 0, 0, MSG )
           END IF
           L = 0
           DO S = 1, NSPC_SA
               IF( NOT_DEGRADED( S ) )THEN
                   C = ISAM_SPC_IDX( S )
                   IF( C .LE. 0 )CYCLE
                   L = L + 1
                   IF( L .LT. 2 )THEN
                       WRITE(LOGDEV,'(/A)')'Below isam species DO NOT have a linear decay based on photochemistry'
                       WRITE(LOGDEV,'("SPC     ISAM_SPC")')
                   END IF    
                       WRITE(LOGDEV,'(I3,1X,A16,1x,I3,A16)') C, FIND_IN_ISAM( S )
                   END IF
           END DO

           DEALLOCATE( ISAM_2_DEGRAD )
           DEALLOCATE( ISAM_SPC_IDX )
           DEALLOCATE( NOT_DEGRADED )
        
        END SUBROUTINE SA_DEGRADE_INIT
        SUBROUTINE SA_DEGRADE_EXTRACT( COL, ROW, LAY, DENS )
                
          USE SA_DEFN  

          IMPLICIT NONE

!..Arguments:
         INTEGER,   INTENT( IN ) ::  COL        ! cell column index
         INTEGER,   INTENT( IN ) ::  ROW        ! cell row index 
         INTEGER,   INTENT( IN ) ::  LAY        ! cell layer index      
         REAL,      INTENT( IN ) ::  DENS       ! air mass density, kg/m3

C..Includes: None

!..Local:
         CHARACTER( 32 ), PARAMETER :: PNAME = 'SA_DEGRAGE_EXTRACT'     ! Program name

         REAL( 8 ), PARAMETER  :: ONE       = 1.0D0
         REAL( 8 ), PARAMETER  :: ZERO      = 0.0D0

         REAL      :: FACTOR2
         REAL( 8 ) :: INV_DENS       ! one over air mass density, m3/kg

         INTEGER :: JSPC, KTAG
         INTEGER :: SPC, S          ! array index
         
          IF( ISAM_DEGRADED_SPC .LT. 1 )RETURN
          
          INV_DENS = REAL( ONE/DENS, 8 )
          
          DO JSPC = 1, ISAM_DEGRADED_SPC
             S       = ISAM_TO_DEGRADED( JSPC )
             SPC     = ISAM_DEGRADE_MAP( JSPC )
             LOAD_SOLD: DO KTAG = 1, NTAG_SA
                 FACTOR2 = ISAM( COL,ROW,LAY,SPC,KTAG )
                 CELL_ISAM( KTAG, JSPC ) = REAL( FACTOR2,8 )
             END DO LOAD_SOLD ! ktag loop
          END DO ! loop jspc

        END SUBROUTINE SA_DEGRADE_EXTRACT
        SUBROUTINE SA_DEGRADE_UPLOAD( COL, ROW, LAY, DENS )
                
            USE SA_DEFN  
            
            IMPLICIT NONE

!..Arguments:
            INTEGER,   INTENT( IN ) ::  COL        ! cell column index
            INTEGER,   INTENT( IN ) ::  ROW        ! cell row index 
            INTEGER,   INTENT( IN ) ::  LAY        ! cell layer index      
            REAL,      INTENT( IN ) ::  DENS       ! air mass density, kg/m3

C..Includes: None

!..Local:
            CHARACTER( 32 ), PARAMETER :: PNAME = 'SA_DEGRAGE_UPLOAD'     ! Program name
            
            REAL( 8 ), PARAMETER  :: ONE       = 1.0D0
            REAL( 8 ), PARAMETER  :: ZERO      = 0.0D0
            
            REAL      :: FACTOR2
            REAL( 8 ) :: TOTAL, FACTOR1, FACTOR3, FACTOR
!            REAL( 8 ) :: INV_DENS       ! one over air mass density, m3/kg
            
            INTEGER :: JSPC, KTAG, I_RAD, I_RXT
            INTEGER :: S, SPC          ! array index
            
            IF( ISAM_DEGRADED_SPC .LT. 1 )RETURN
             
#ifdef verbose_isam
      IF( DEG_LAY .EQ. 1 .AND. DEG_ROW .EQ. 1 .AND. DEG_COL .EQ. 1 )THEN
         WRITE(LOGDEV,'(6x,A)')'SA_DEGRADE_UPLOAD'
         WRITE(LOGDEV,'(//,6X,A16,(1X,A12),1X,A18,2(1X,A18))')'isam_degraded', 'factor', 'react',
     &   'INIT_CONC','FINAL_CONC'
      END IF
#endif
             DO JSPC = 1, ISAM_DEGRADED_SPC
                S       = ISAM_TO_DEGRADED( JSPC )
                SPC     = ISAM_DEGRADE_MAP( JSPC )
#ifdef verbose_isam
               IF( DEG_LAY .EQ. 1 .AND. DEG_ROW .EQ. 1 .AND. DEG_COL .EQ. 1)THEN 
                    I_RXT = ISAM_TO_DEGRADED( JSPC )
                    I_RAD = ISAM_TO_REACTANT( JSPC )
                    FACTOR2 = SUM(ISAM( COL,ROW,LAY,SPC,1:NTAG_SA ))
                    FACTOR3 = SUM(CELL_ISAM( 1:NTAG_SA,JSPC ))
                    FACTOR  = 1.0 + (FACTOR3-FACTOR2)/MAX(FACTOR2,1.0E-30)
                  WRITE(LOGDEV,'(6X,A16,1X,ES18.10,1X,A16,2(1X,ES18.10))')ISAM_DEGRADED( JSPC ), FACTOR, REACT( I_RAD ),
     &            FACTOR2, FACTOR3
               END IF
#endif               
                LOAD_ISAM: DO KTAG = 1, NTAG_SA
                    FACTOR1 =  CELL_ISAM( KTAG,JSPC )
                    ISAM( COL,ROW,LAY,SPC,KTAG ) = REAL( FACTOR1 )
                END DO LOAD_ISAM ! ktag loop
             END DO ! loop jspc

          END SUBROUTINE SA_DEGRADE_UPLOAD
#endif
                
      END MODULE DEGRADE_SETUP_TOX

