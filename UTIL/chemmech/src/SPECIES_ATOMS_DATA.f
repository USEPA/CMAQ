         MODULE SPECIES_ATOMS_DATA
         
           IMPLICIT NONE

            CHARACTER( 686 )              ::  EQNAME_ATOMS
            INTEGER                       ::  EXUNIT_ATOMS
            CHARACTER( 686 )              ::  EQNAME_ATOMS_REPORT
            INTEGER                       ::  EXUNIT_ATOMS_REPORT
            
            INTEGER                       ::  N_ATOM_SPECIES  = 0
            INTEGER                       ::  N_ATOMS         = 0
            CHARACTER( 16 ),  ALLOCATABLE ::  ATOMS          ( : )
            CHARACTER( 16 ),  ALLOCATABLE ::  ATOM_SPECIES   ( : )
            REAL,             ALLOCATABLE ::  ATOMS_SPECIES_MOLWT  ( : )
            CHARACTER( 100 ), ALLOCATABLE ::  ATOMS_SPECIES_REPRESENTATIVE( : )
            CHARACTER( 100 ), ALLOCATABLE ::  ATOMS_SPECIES_REPRESENTATION( : )
            CHARACTER( 100 ), ALLOCATABLE ::  ATOMS_SPECIES_DSSTOX_ID     ( : )
            CHARACTER( 100 ), ALLOCATABLE ::  ATOMS_SPECIES_SMILES        ( : )
            CHARACTER(   2 ), ALLOCATABLE ::  ATOMS_SPECIES_PHASE         ( : )
            REAL,             ALLOCATABLE ::  SPECIES_ATOMS( :,: )
            INTEGER,          ALLOCATABLE ::  ATOMS2MECH_MAP ( : )
            REAL,             ALLOCATABLE ::  MECH_SPECIES_ATOMS( :,: )
            REAL,             ALLOCATABLE ::  REACTION_DELTA( :,: )
            LOGICAL,          ALLOCATABLE ::  ATOM_FOUND( : )       
            LOGICAL                       ::  NONZERO_ATOMS = .FALSE.
            
            CHARACTER( 300 ), ALLOCATABLE ::  DELTA_ATOMS  ( : )
            LOGICAL,          ALLOCATABLE ::  NONZERO_DELTA( :,: )
     
            INTERFACE CONVERT_NUMBER                     ! convert number to text with real(4) with decimals            
                MODULE PROCEDURE CONVERT_REAL4,
     &                           CONVERT_REAL8,
     &                           CONVERT_INTEGER                
            END INTERFACE
            INTEGER, PARAMETER :: PRECISION_CONVERT = 4      ! number of decimal places CONVERT_NUMBER
            REAL(4), PARAMETER :: OFF_SET           = 5.0D-7 ! rounding parameter in CONVERT_NUMBER 

            
            LOGICAL, PARAMETER :: AE_NML_V53 = .FALSE.
            
         CONTAINS

            SUBROUTINE READ_MATRICES_ATOMS()
            
              USE GET_ENV_VARS
              USE MECHANISM_DATA
              USE CCTM_SPECIES           
              
              IMPLICIT NONE
            
! parameter to used to convert case in line read
              INTEGER, PARAMETER :: STRT   =  97
              INTEGER, PARAMETER :: FINI   = 122
              INTEGER, PARAMETER :: FACTOR = -32
              INTEGER, PARAMETER :: MAX_WORDS = 200 ! max number of words in Header
              
              
              CHARACTER( 16 ) :: ATOMS_FILE = 'ATOMS_FILE'
              INTEGER, EXTERNAL  :: JUNIT
              
              LOGICAL :: FILE_EXISTS  = .TRUE.
              LOGICAL :: FOUND_HEADER  = .FALSE.
              
              INTEGER :: NLINES_FILE
              INTEGER :: NLINE
              INTEGER :: IPOS, IC 
              INTEGER :: NAERO_COMPONENTS
              INTEGER :: POSITION_HEADER
              INTEGER :: ISPECIES, JSPECIES, IATOM
              INTEGER :: START_POSITION, STOP_POSITION
              INTEGER :: IO_STATUS 
              INTEGER :: LINES_IGNORED 
              
              CHARACTER(586)                         :: FILE_LINE, LINE_CONTENT 
              CHARACTER(100), ALLOCATABLE            :: LINE_WORDS(:)
              CHARACTER( 16), ALLOCATABLE            :: AERO_COMPONENTS(:)
              CHARACTER( 16 ), SAVE                  :: GC_MATRIX = 'gc_matrix_nml'           
              CHARACTER( 16 ), SAVE                  :: AE_MATRIX = 'ae_matrix_nml'           
              CHARACTER( 16 ), SAVE                  :: NR_MATRIX = 'nr_matrix_nml'           
              CHARACTER( 16 ), SAVE                  :: TR_MATRIX = 'tr_matrix_nml'           
              
              REAL              :: COMPONENT_WEIGHT
              REAL, ALLOCATABLE :: ATOM_COUNT( : )
              
              LOGICAL, SAVE     :: INITIALIZE = .TRUE. 
              LOGICAL           :: EFLAG
              
              INTEGER, PARAMETER :: N_MODES = 3
              CHARACTER( 16 )    :: AERO_NAMES( N_MODES )
              LOGICAL            :: MODE_FLAGS( N_MODES )
              

              
              IF ( INITIALIZE ) THEN
              
! get GC namelist name, open and count number of species
                 CALL VALUE_NAME( GC_MATRIX, EQNAME_ATOMS )                                                                                                 
                 WRITE( 6,'(A)' ) 'GC SPECIES NAMELIST: ' // TRIM( EQNAME_ATOMS )
                 
                 EXUNIT_ATOMS = JUNIT()
                 INQUIRE( FILE = TRIM( EQNAME_ATOMS ), EXIST = FILE_EXISTS )
                 IF( .NOT. FILE_EXISTS )THEN
                     WRITE(6,'(A,/,A)')'ERROR: CANNOT LOCATE the GC namelist: ', 
     &                                  TRIM(EQNAME_ATOMS)
                     STOP
                 END IF
                 OPEN( FILE=TRIM(EQNAME_ATOMS),UNIT=EXUNIT_ATOMS,STATUS='OLD',POSITION='REWIND' )                                      

                 N_ATOM_SPECIES = 1
! make rough count of GC species and allocate their name and molecular weight arrays 
                 IPOS           = 0
                 DO
                    READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
                    IF( IO_STATUS .NE. 0 )THEN
                        EXIT
                    END IF
                    FILE_LINE = ADJUSTL( FILE_LINE )
                    IF( FILE_LINE(1:1) .EQ. "'" )N_ATOM_SPECIES = N_ATOM_SPECIES + 1
                 END DO           
                 CLOSE( EXUNIT_ATOMS )
                 
                 IC = N_ATOM_SPECIES - IPOS
                 ALLOCATE( GC_SPC( IC+1 ), GC_MOLWT( IC+1 ) )
                 GC_SPC   = ''
                 GC_MOLWT = 0.0                 
                 
! get AE namelist name, open and count number of species
                 CALL VALUE_NAME( AE_MATRIX, EQNAME_ATOMS )                                                                                                 
                 WRITE( 6,'(A)' ) '    AE SPECIES NAMELIST: ', TRIM( EQNAME_ATOMS )
                 
                 EXUNIT_ATOMS = JUNIT()
                 INQUIRE( FILE = TRIM( EQNAME_ATOMS ), EXIST = FILE_EXISTS )
                 IF( .NOT. FILE_EXISTS )THEN
                     WRITE(6,'(A,/,A)')'ERROR: CANNOT LOCATE the AE namelist: ', 
     &                                  TRIM(EQNAME_ATOMS)
                     STOP
                 END IF
                 OPEN( FILE=TRIM(EQNAME_ATOMS),UNIT=EXUNIT_ATOMS,STATUS='OLD',POSITION='REWIND' )                                      
! make rough count of AE species and allocate their name and molecular weight arrays 
                 IPOS             = N_ATOM_SPECIES 
                 NAERO_COMPONENTS = 0
                 DO
                    READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
                    IF( IO_STATUS .NE. 0 )THEN
                        EXIT
                    END IF
                    FILE_LINE = ADJUSTL( FILE_LINE )
                    IF( FILE_LINE(1:1) .EQ. "'" )THEN
                      IF( AE_NML_V53 )THEN
                          N_ATOM_SPECIES = N_ATOM_SPECIES + 1
                      ELSE
                          N_ATOM_SPECIES   = N_ATOM_SPECIES + 3
                          NAERO_COMPONENTS = NAERO_COMPONENTS + 1
                      END IF
                    END IF
                 END DO           
                 CLOSE( EXUNIT_ATOMS )

                 IC = N_ATOM_SPECIES - IPOS
                 ALLOCATE( AE_SPC( IC+1 ), AE_MOLWT( IC+1 ) )
                 AE_SPC   = ''
                 AE_MOLWT = 0.0                 

                 IF( .NOT. AE_NML_V53 )THEN
                     ALLOCATE( AERO_COMPONENTS( NAERO_COMPONENTS + 1 ) )               
                     AERO_COMPONENTS = ''
                 END IF
                                  
! get NR namelist name, open and count number of species
                 CALL VALUE_NAME( NR_MATRIX, EQNAME_ATOMS )                                                                                                 
                 WRITE( 6,'(A)' ) '    NR SPECIES NAMELIST: ', TRIM( EQNAME_ATOMS )
                 
                 EXUNIT_ATOMS = JUNIT()
                 INQUIRE( FILE = TRIM( EQNAME_ATOMS ), EXIST = FILE_EXISTS )
                 IF( .NOT. FILE_EXISTS )THEN
                     WRITE(6,'(A,/,A)')'ERROR: CANNOT LOCATE the NR namelist: ', 
     &                                  TRIM(EQNAME_ATOMS)
                     WRITE(6,'(A)')'ERROR: SET BALANCE_ATOM option to "N" if file is not to used.'
                     STOP
                 END IF
                 OPEN( FILE=TRIM(EQNAME_ATOMS),UNIT=EXUNIT_ATOMS,STATUS='OLD',POSITION='REWIND' )                                      
                 NLINES_FILE    = 0

! make rough count of NR species and allocate their name and molecular weight arrays 
                 DO
                    READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
                    IF( IO_STATUS .NE. 0 )THEN
                        EXIT
                    END IF
                    FILE_LINE = ADJUSTL( FILE_LINE )
                    IF( FILE_LINE(1:1) .EQ. "'" )N_ATOM_SPECIES = N_ATOM_SPECIES + 1
                    NLINES_FILE = NLINES_FILE + 1   
                 END DO           
                 CLOSE( EXUNIT_ATOMS )                 
                 IC = N_ATOM_SPECIES - IPOS
                 ALLOCATE( NR_SPC( IC+1 ), NR_MOLWT( IC+1 ) )
                 NR_SPC   = ''
                 NR_MOLWT = 0.0                 
                 
! get TR namelist name, open and count number of species
                 CALL VALUE_NAME( TR_MATRIX, EQNAME_ATOMS )                                                                                                 
                 WRITE( 6,'(A)' ) '    TR SPECIES NAMELIST: ', TRIM( EQNAME_ATOMS )
                 
                 EXUNIT_ATOMS = JUNIT()
                 INQUIRE( FILE = TRIM( EQNAME_ATOMS ), EXIST = FILE_EXISTS )
                 IF( .NOT. FILE_EXISTS )THEN
                     WRITE(6,'(A,/,A)')'ERROR: CANNOT LOCATE the TR namelist: ', 
     &                                  TRIM(EQNAME_ATOMS)
                     STOP
                 END IF
                 OPEN( FILE=TRIM(EQNAME_ATOMS),UNIT=EXUNIT_ATOMS,STATUS='OLD',POSITION='REWIND' )                                      
! make rough count of TR species and allocate their name and molecular weight arrays 
                 IPOS           = N_ATOM_SPECIES 
                 DO
                    READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
                    IF( IO_STATUS .NE. 0 )THEN
                        EXIT
                    END IF
                    FILE_LINE = ADJUSTL( FILE_LINE )
                    IF( FILE_LINE(1:1) .EQ. "'" )N_ATOM_SPECIES = N_ATOM_SPECIES + 1
                 END DO
                 CLOSE( EXUNIT_ATOMS )
                 IC = N_ATOM_SPECIES - IPOS
                 ALLOCATE( TR_SPC( IC ), TR_MOLWT( IC ) )
                 TR_SPC   = ''
                 TR_MOLWT = 0.0                 
                 
                 ALLOCATE( ATOM_SPECIES  ( N_ATOM_SPECIES ),
     &                     ATOMS2MECH_MAP( N_ATOM_SPECIES ),
     &                     ATOMS_SPECIES_PHASE ( N_ATOM_SPECIES ),
     &                     ATOMS_SPECIES_MOLWT ( N_ATOM_SPECIES ) )

                 ATOM_SPECIES   = ''
                 ATOMS2MECH_MAP = -1
                 ATOMS_SPECIES_MOLWT = 0.0
                 ATOMS_SPECIES_PHASE = 'NA'
                 
                 IF( ATOMS_IN_NAMELISTS )THEN 
                 
                     CALL ALLOCATE_SMILES_ATOMS()

                     ALLOCATE( ATOM_COUNT (N_ATOMS) )
                     ATOM_COUNT      = 0.0
                                 
                 END IF   !  ATOMS_IN_NAMELIST

                 INITIALIZE = .FALSE.
                   
              END IF  ! INITIALIZE
              
              ALLOCATE( LINE_WORDS(MAX_WORDS) )
              ISPECIES = 0
                        
! get GC namelist name and open
              CALL VALUE_NAME( GC_MATRIX, EQNAME_ATOMS )                                                                                                 
              
              EXUNIT_ATOMS = JUNIT()
              INQUIRE( FILE = TRIM( EQNAME_ATOMS ), EXIST = FILE_EXISTS )
              IF( .NOT. FILE_EXISTS )THEN
                  WRITE(6,'(A,/,A)')'ERROR: CANNOT LOCATE the ATOMS_FILE: ', 
     &                               TRIM(EQNAME_ATOMS)
                  WRITE(6,'(A)')'ERROR: SET BALANCE_ATOM option to "N" if file is not to used.'
                  STOP
              END IF
              
              OPEN( FILE = TRIM( EQNAME_ATOMS ), UNIT = EXUNIT_ATOMS, STATUS = 'OLD', POSITION = 'REWIND' )                                      
              
              NLINES_FILE = 0
              DO
                 READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
                 IF( IO_STATUS .NE. 0 )THEN
                     EXIT
                 END IF
                 NLINES_FILE = NLINES_FILE + 1   
              END DO
              
              REWIND( EXUNIT_ATOMS )
              
              EFLAG = .FALSE.
              ISPECIES = 0
              DO NLINE = 1,NLINES_FILE
                 READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
! skip blank lines, comment lines or other lines without species information
                 IF( LEN_TRIM(FILE_LINE) .LE. 0 )THEN
                     CYCLE
                 END IF    
                 FILE_LINE = TRIM( ADJUSTL( FILE_LINE ) )
                 IF( FILE_LINE(1:1) .NE. "'" )THEN
                     CYCLE
                 END IF
                 ISPECIES = ISPECIES + 1
! get CMAQ CGRID GC species name, molecular weight, and phase              
                 CALL PARSE_STRING(FILE_LINE,IC,LINE_WORDS)
                 ATOM_SPECIES( ISPECIES ) = REPLACE_TEXT( LINE_WORDS(1),"'"," ")
                 READ( LINE_WORDS(2),*)ATOMS_SPECIES_MOLWT( ISPECIES )
                 ATOMS_SPECIES_PHASE( ISPECIES ) = 'GC'
                 N_GC_SPC = N_GC_SPC + 1
                 GC_SPC( N_GC_SPC ) = ATOM_SPECIES( ISPECIES )
                 GC_MOLWT( N_GC_SPC ) = ATOMS_SPECIES_MOLWT( ISPECIES )
                 IF( ATOMS_IN_NAMELISTS )THEN  ! subset line for end comments
                     
                     LINE_CONTENT = Tailing_Comment (FILE_LINE,"!")
                     IF( LEN_TRIM( LINE_CONTENT ) .LE. 1 )CYCLE
                ! parse comment for information
                     START_POSITION = 1
                     STOP_POSITION  = LEN_TRIM( LINE_CONTENT )
                     IC = 0
                     DO
                        IPOS = INDEX( LINE_CONTENT(START_POSITION:STOP_POSITION),"," )
                        IF ( IPOS .EQ. 0 )EXIT
                        IF ( IPOS .EQ. STOP_POSITION )EXIT
                        IC = IC + 1
                        LINE_WORDS( IC ) = TRIM( ADJUSTL(LINE_CONTENT(START_POSITION:IPOS-1)) )
                        IPOS = IPOS + 1
                        IF ( IPOS .GE. STOP_POSITION )EXIT
                        LINE_CONTENT     = LINE_CONTENT(IPOS:STOP_POSITION)
                     END DO
                     IF( LEN_TRIM(LINE_CONTENT) .GT. 0 )THEN
                        IC = IC + 1
                        LINE_WORDS( IC ) = TRIM( ADJUSTL(LINE_CONTENT) )
                     END IF   
                     IF( IC .NE. 4 )THEN
                        WRITE(6,'(A,1X,A,A)')'WARNING: ',ATOM_SPECIES( ISPECIES ),
     &                  ' does not have four item in trailing comment information. Will ignore information in items.'
                        EFLAG = .TRUE.
                        CYCLE
                     END IF
                     ATOMS_SPECIES_REPRESENTATIVE( ISPECIES ) = LINE_WORDS(1)
                     ATOMS_SPECIES_REPRESENTATION( ISPECIES ) = LINE_WORDS(2)
                     ATOMS_SPECIES_DSSTOX_ID     ( ISPECIES ) = LINE_WORDS(3)
                     IF( TRIM( LINE_WORDS(4) ) .NE. 'NA' 
     &                                          .AND. TRIM( LINE_WORDS(4) ) .NE. 'TBD' )THEN 
                        ATOMS_SPECIES_SMILES        ( ISPECIES ) = LINE_WORDS(4)
                        CALL COUNT_SMILES_ATOMS( ATOMS_SPECIES_SMILES(ISPECIES),
     &                                           ATOM_COUNT )
                        SPECIES_ATOMS(ISPECIES,1:N_ATOMS) = ATOM_COUNT(1:N_ATOMS)
                     END IF
                     
                 END IF ! atoms in namelist
              END DO          
              CLOSE( EXUNIT_ATOMS )
              
! get AE namelist name and open
              CALL VALUE_NAME( AE_MATRIX, EQNAME_ATOMS )                                                                                                 
!              WRITE( 6,'(A)' ) '    AE SPECIES NAMELIST: ', TRIM( EQNAME_ATOMS )
              
              EXUNIT_ATOMS = JUNIT()
              INQUIRE( FILE = TRIM( EQNAME_ATOMS ), EXIST = FILE_EXISTS )
              IF( .NOT. FILE_EXISTS )THEN
                  WRITE(6,'(A,/,A)')'ERROR: CANNOT LOCATE the ATOMS_FILE: ', 
     &                               TRIM(EQNAME_ATOMS)
                  STOP
              END IF
              
              OPEN( FILE = TRIM( EQNAME_ATOMS ), UNIT = EXUNIT_ATOMS, STATUS = 'OLD', POSITION = 'REWIND' )                                      
              
              NLINES_FILE = 0
              DO
                 READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
                 IF( IO_STATUS .NE. 0 )THEN
                     EXIT
                 END IF
                 NLINES_FILE = NLINES_FILE + 1   
              END DO
              
              REWIND( EXUNIT_ATOMS )
              
              NAERO_COMPONENTS = 0
              DO NLINE = 1,NLINES_FILE
                 READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
! skip blank lines, comment lines or other lines without species information
                 IF( LEN_TRIM(FILE_LINE) .LE. 0 )THEN
                     CYCLE
                 END IF    
                 FILE_LINE = TRIM( ADJUSTL( FILE_LINE ) )
                 IF( FILE_LINE(1:1) .NE. "'" )THEN
                     CYCLE
                 END IF
                 IF( AE_NML_V53 )THEN
                     ISPECIES = ISPECIES + 1
! get CMAQ CGRID AE species name, molecular weight, and phase              
                     CALL PARSE_STRING(FILE_LINE,IC,LINE_WORDS)
                     ATOM_SPECIES( ISPECIES ) = REPLACE_TEXT( LINE_WORDS(1),"'"," ")
                     READ( LINE_WORDS(2),*)ATOMS_SPECIES_MOLWT( ISPECIES )
                     ATOMS_SPECIES_PHASE( ISPECIES ) = 'AE'
                     N_AE_SPC = N_AE_SPC + 1
                     AE_SPC( N_AE_SPC )   = ATOM_SPECIES( ISPECIES )
                     AE_MOLWT( N_AE_SPC ) = ATOMS_SPECIES_MOLWT( ISPECIES )
                 ELSE   
                     JSPECIES = ISPECIES
                     CALL PARSE_STRING(FILE_LINE,IC,LINE_WORDS)
                     NAERO_COMPONENTS = NAERO_COMPONENTS + 1
                     AERO_COMPONENTS( NAERO_COMPONENTS ) = ADJUSTL(REPLACE_TEXT( LINE_WORDS(1),"'"," "))
                     READ( LINE_WORDS(2),*)COMPONENT_WEIGHT
                     DO IC = 3,3+(N_MODES-1)
                        IPOS = IC-N_MODES+1
                        LINE_WORDS(IC) = ADJUSTL( REPLACE_TEXT( LINE_WORDS(IC),"'"," ") )
                        CALL UCASE( LINE_WORDS(IC) )
                        IF( TRIM( LINE_WORDS(IC) ) .EQ. 'T' )THEN
                            MODE_FLAGS(IPOS) = .TRUE.
                        ELSE IF( TRIM( LINE_WORDS(IC) ) .EQ. 'F' )THEN 
                            MODE_FLAGS(IPOS) = .FALSE.
                        ELSE
                            EFLAG = .TRUE.
                            WRITE(6,'(A,6(A,1X))')'ERROR: ',TRIM( AERO_COMPONENTS( NAERO_COMPONENTS ) ),
     &                     ' has bad values for modal logical flag.', LINE_WORDS(IC)
                        END IF
                     END DO
!                    WRITE(6,'(A,A,3(1X,L2))')'SET: ',TRIM( AERO_COMPONENTS( NAERO_COMPONENTS ) ),MODE_FLAGS(:)
                     CALL SET_AERO_MODE_NAMES( AERO_COMPONENTS(NAERO_COMPONENTS),MODE_FLAGS,
     &                                         AERO_NAMES )
!                    WRITE(6,'(A,A,3(1X,L2),3(A16,1X))')'READ: ',TRIM( AERO_COMPONENTS( NAERO_COMPONENTS ) ),MODE_FLAGS(:),
!    &               (trim(LINE_WORDS(IC) ),ic=3,3+(N_MODES-1))
!                    WRITE(6,'(A,A,3(1X,L2),3(A16,1X))')'READ: ',TRIM( AERO_COMPONENTS( NAERO_COMPONENTS ) ),MODE_FLAGS(:),
!    &               (trim(AERO_NAMES(IC)),ic=1,N_MODES)
                     DO IC = 1,N_MODES
                        IF( MODE_FLAGS( IC ) )THEN
                            N_AE_SPC = N_AE_SPC + 1
                            AE_SPC( N_AE_SPC )   = AERO_NAMES(IC)
                            AE_MOLWT( N_AE_SPC ) = COMPONENT_WEIGHT
                            write(6,'(A,I3,1X,A,1X,f7.2)')"N_AE_SPC, AE_SPC, MOLWT= ",
     &                       N_AE_SPC,AE_SPC( N_AE_SPC ),AE_MOLWT( N_AE_SPC )
                            ISPECIES = ISPECIES + 1
                            ATOM_SPECIES( ISPECIES ) = AERO_NAMES(IC)
                            ATOMS_SPECIES_MOLWT( ISPECIES ) = COMPONENT_WEIGHT
                        ELSE 
!                            N_AE_SPC = MAX( 1,(N_AE_SPC-1) )
                            N_ATOM_SPECIES = MAX( 1,(N_ATOM_SPECIES-1) )
                        END IF
                     END DO 
                 END IF
                 IF( ATOMS_IN_NAMELISTS )THEN  ! subset line for end comments

                    LINE_CONTENT = Tailing_Comment (FILE_LINE,"!")
                    IF( LEN_TRIM( LINE_CONTENT ) .LE. 1 )CYCLE
             ! parse comment for information
                    START_POSITION = 1
                    STOP_POSITION  = LEN_TRIM( LINE_CONTENT )
                    IC = 0
                    DO
                       IPOS = INDEX( LINE_CONTENT(START_POSITION:STOP_POSITION),"," )
                       IF ( IPOS .EQ. 0 )EXIT
                       IF ( IPOS .EQ. STOP_POSITION )EXIT
                       IC = IC + 1
                       LINE_WORDS( IC ) = TRIM( ADJUSTL(LINE_CONTENT(START_POSITION:IPOS-1)) )
                       IPOS = IPOS + 1
                       IF ( IPOS .GE. STOP_POSITION )EXIT
                       LINE_CONTENT     = LINE_CONTENT(IPOS:STOP_POSITION)
                    END DO
                    IF( LEN_TRIM(LINE_CONTENT) .GT. 0 )THEN
                       IC = IC + 1
                       LINE_WORDS( IC ) = TRIM( ADJUSTL(LINE_CONTENT) )
                    END IF   
!                    IF( IC .GE. 1) write(6,'(12(a,1x))')ATOM_SPECIES( ISPECIES ),(TRIM(LINE_WORDS(IPOS)),IPOS=1,IC)
                    IF( IC .NE. 4 )THEN
                       WRITE(6,'(A,1X,A,A)')'WARNING: ',ATOM_SPECIES( ISPECIES ),
     &                 ' does not have four item in trailing comment information. Will ignore information in items.'
                       EFLAG = .TRUE.
                       CYCLE
                    END IF
                    IF( AE_NML_V53 )THEN

                        ATOMS_SPECIES_REPRESENTATIVE( ISPECIES ) = LINE_WORDS(1)
                        ATOMS_SPECIES_REPRESENTATION( ISPECIES ) = LINE_WORDS(2)
                        ATOMS_SPECIES_DSSTOX_ID     ( ISPECIES ) = LINE_WORDS(3)
                        IF( TRIM( LINE_WORDS(4) ) .NE. 'NA' 
     &                                             .AND. TRIM( LINE_WORDS(4) ) .NE. 'TBD' )THEN 
                           ATOMS_SPECIES_SMILES        ( ISPECIES ) = LINE_WORDS(4)
                           CALL COUNT_SMILES_ATOMS( ATOMS_SPECIES_SMILES(ISPECIES),
     &                                              ATOM_COUNT )
                           SPECIES_ATOMS(ISPECIES,1:N_ATOMS) = ATOM_COUNT(1:N_ATOMS)
                        END IF
                    ELSE    
                        IF( TRIM( LINE_WORDS(4) ) .NE. 'NA' 
     &                                            .AND. TRIM( LINE_WORDS(4) ) .NE. 'TBD' )THEN 
                             CALL COUNT_SMILES_ATOMS( LINE_WORDS(4),ATOM_COUNT )
                        END IF
                        DO IC = 1, N_MODES
                           IF( MODE_FLAGS( IC ) )THEN
                               JSPECIES = JSPECIES + 1
                               ATOMS_SPECIES_REPRESENTATIVE( JSPECIES ) = LINE_WORDS(1)
                               ATOMS_SPECIES_REPRESENTATION( JSPECIES ) = LINE_WORDS(2)
                               ATOMS_SPECIES_DSSTOX_ID     ( JSPECIES ) = LINE_WORDS(3)
                               ATOMS_SPECIES_SMILES        ( JSPECIES ) = LINE_WORDS(4)
                               SPECIES_ATOMS(JSPECIES,1:N_ATOMS) = ATOM_COUNT(1:N_ATOMS)
                           END IF
                        END DO
                    END IF
                    
                 END IF ! atoms in namelist
              END DO          
              CLOSE( EXUNIT_ATOMS )
              
              IF( EFLAG )THEN
                  STOP 'FATAL ERROR in AE namelist'
              END IF
              
! get NR namelist name and open
              CALL VALUE_NAME( NR_MATRIX, EQNAME_ATOMS )                                                                                                 
!              WRITE( 6,'(A)' ) '    NR SPECIES NAMELIST: ', TRIM( EQNAME_ATOMS )
              
              EXUNIT_ATOMS = JUNIT()
              INQUIRE( FILE = TRIM( EQNAME_ATOMS ), EXIST = FILE_EXISTS )
              IF( .NOT. FILE_EXISTS )THEN
                  WRITE(6,'(A,/,A)')'ERROR: CANNOT LOCATE the namelist: ', 
     &                               TRIM(EQNAME_ATOMS)
                  STOP
              END IF
              OPEN( FILE = TRIM( EQNAME_ATOMS ), UNIT = EXUNIT_ATOMS, STATUS = 'OLD', POSITION = 'REWIND' )                                      
              
              NLINES_FILE = 0
              DO
                 READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
                 IF( IO_STATUS .NE. 0 )THEN
                     EXIT
                 END IF
                 NLINES_FILE = NLINES_FILE + 1   
              END DO
              
              REWIND( EXUNIT_ATOMS )
              
              DO NLINE = 1,NLINES_FILE
                 READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
! skip blank lines, comment lines or other lines without species information
                 IF( LEN_TRIM(FILE_LINE) .LE. 0 )THEN
                     CYCLE
                 END IF    
                 FILE_LINE = TRIM( ADJUSTL( FILE_LINE ) )
                 IF( FILE_LINE(1:1) .NE. "'" )THEN
                     CYCLE
                 END IF
                 ISPECIES = ISPECIES + 1
! get CMAQ CGRID TR species name, molecular weight, and phase              
                 CALL PARSE_STRING(FILE_LINE,IC,LINE_WORDS)
                 ATOM_SPECIES( ISPECIES ) = REPLACE_TEXT( LINE_WORDS(1),"'"," ")
                 READ( LINE_WORDS(2),*)ATOMS_SPECIES_MOLWT( ISPECIES )
                 ATOMS_SPECIES_PHASE( ISPECIES ) = 'NR'
                 N_NR_SPC = N_NR_SPC + 1
                 NR_SPC( N_NR_SPC )   = ATOM_SPECIES( ISPECIES )
                 NR_MOLWT( N_NR_SPC ) = ATOMS_SPECIES_MOLWT( ISPECIES )
                 IF( ATOMS_IN_NAMELISTS )THEN  ! subset line for end comments

                     LINE_CONTENT = Tailing_Comment (FILE_LINE,"!")
                     IF( LEN_TRIM( LINE_CONTENT ) .LE. 1 )CYCLE
              ! parse comment for information
                     START_POSITION = 1
                     STOP_POSITION  = LEN_TRIM( LINE_CONTENT )
                     IC = 0
                     DO
                        IPOS = INDEX( LINE_CONTENT(START_POSITION:STOP_POSITION),"," )
                        IF ( IPOS .EQ. 0 )EXIT
                        IF ( IPOS .EQ. STOP_POSITION )EXIT
                        IC = IC + 1
                        LINE_WORDS( IC ) = TRIM( ADJUSTL(LINE_CONTENT(START_POSITION:IPOS-1)) )
                        IPOS = IPOS + 1
                        IF ( IPOS .GE. STOP_POSITION )EXIT
                        LINE_CONTENT     = LINE_CONTENT(IPOS:STOP_POSITION)
                     END DO
                     IF( LEN_TRIM(LINE_CONTENT) .GT. 0 )THEN
                        IC = IC + 1
                        LINE_WORDS( IC ) = TRIM( ADJUSTL(LINE_CONTENT) )
                     END IF   
                     IF( IC .GE. 1) write(6,'(12(a,1x))')ATOM_SPECIES( ISPECIES ),(TRIM(LINE_WORDS(IPOS)),IPOS=1,IC)
                     IF( IC .NE. 4 )THEN
                        WRITE(6,'(A,1X,A,A)')'WARNING: ',ATOM_SPECIES( ISPECIES ),
     &                  ' does not have four item in trailing comment information. Will ignore information in items.'
                        EFLAG = .TRUE.
                        CYCLE
                     END IF
!                     write(6,'(12(a,1x))')LINE_WORDS(1:IC)
                     ATOMS_SPECIES_REPRESENTATIVE( ISPECIES ) = LINE_WORDS(1)
                     ATOMS_SPECIES_REPRESENTATION( ISPECIES ) = LINE_WORDS(2)
                     ATOMS_SPECIES_DSSTOX_ID     ( ISPECIES ) = LINE_WORDS(3)
                     IF( TRIM( LINE_WORDS(4) ) .NE. 'NA' 
     &                                          .AND. TRIM( LINE_WORDS(4) ) .NE. 'TBD' )THEN 
                        ATOMS_SPECIES_SMILES        ( ISPECIES ) = LINE_WORDS(4)
                        CALL COUNT_SMILES_ATOMS( ATOMS_SPECIES_SMILES(ISPECIES),
     &                                       ATOM_COUNT )
                        SPECIES_ATOMS(ISPECIES,1:N_ATOMS) = ATOM_COUNT(1:N_ATOMS)
                     END IF
                 
                 END IF ! atoms in namelist

              END DO          
              CLOSE( EXUNIT_ATOMS )


! get TR namelist name and open
              CALL VALUE_NAME( TR_MATRIX, EQNAME_ATOMS )                                                                                                 
!              WRITE( 6,'(A)' ) '    TR SPECIES NAMELIST: ', TRIM( EQNAME_ATOMS )
              
              EXUNIT_ATOMS = JUNIT()
              INQUIRE( FILE = TRIM( EQNAME_ATOMS ), EXIST = FILE_EXISTS )
              IF( .NOT. FILE_EXISTS )THEN
                  WRITE(6,'(A,/,A)')'ERROR: CANNOT LOCATE the namelist: ', 
     &                               TRIM(EQNAME_ATOMS)
                  STOP
              END IF
              OPEN( FILE = TRIM( EQNAME_ATOMS ), UNIT = EXUNIT_ATOMS, STATUS = 'OLD', POSITION = 'REWIND' )                                      
              
              NLINES_FILE = 0
              DO
                 READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
                 IF( IO_STATUS .NE. 0 )THEN
                     EXIT
                 END IF
                 NLINES_FILE = NLINES_FILE + 1   
              END DO
              
              REWIND( EXUNIT_ATOMS )

              DO NLINE = 1,NLINES_FILE
                 READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
! skip blank lines, comment lines or other lines without species information
                 IF( LEN_TRIM(FILE_LINE) .LE. 0 )THEN
                     CYCLE
                 END IF    
                 FILE_LINE = TRIM( ADJUSTL( FILE_LINE ) )
                 IF( FILE_LINE(1:1) .NE. "'" )THEN
                     CYCLE
                 END IF
                 ISPECIES = ISPECIES + 1
! get CMAQ CGRID TR species name, molecular weight, and phase              
                 CALL PARSE_STRING(FILE_LINE,IC,LINE_WORDS)
                 ATOM_SPECIES( ISPECIES ) = REPLACE_TEXT( LINE_WORDS(1),"'"," ")
                 READ( LINE_WORDS(2),*)ATOMS_SPECIES_MOLWT( ISPECIES )
                 ATOMS_SPECIES_PHASE( ISPECIES ) = 'TR'
                 N_TR_SPC = N_TR_SPC + 1
                 TR_SPC( N_TR_SPC )   = ATOM_SPECIES( ISPECIES )
                 TR_MOLWT( N_TR_SPC ) = ATOMS_SPECIES_MOLWT( ISPECIES )
                 IF( ATOMS_IN_NAMELISTS )THEN  ! subset line for end comments

                    LINE_CONTENT = Tailing_Comment (FILE_LINE,"!")
                    IF( LEN_TRIM( LINE_CONTENT ) .LE. 1 )CYCLE
               ! parse comment for information
                    START_POSITION = 1
                    STOP_POSITION  = LEN_TRIM( LINE_CONTENT )
                    IC = 0
                    DO
                       IPOS = INDEX( LINE_CONTENT(START_POSITION:STOP_POSITION),"," )
                       IF ( IPOS .EQ. 0 )EXIT
                       IF ( IPOS .EQ. STOP_POSITION )EXIT
                       IC = IC + 1
                       LINE_WORDS( IC ) = TRIM( ADJUSTL(LINE_CONTENT(START_POSITION:IPOS-1)) )
                       IPOS = IPOS + 1
                       IF ( IPOS .GE. STOP_POSITION )EXIT
                       LINE_CONTENT     = LINE_CONTENT(IPOS:STOP_POSITION)
                    END DO
                    IF( LEN_TRIM(LINE_CONTENT) .GT. 0 )THEN
                       IC = IC + 1
                       LINE_WORDS( IC ) = TRIM( ADJUSTL(LINE_CONTENT) )
                    END IF   
                    IF( IC .GE. 1) write(6,'(12(a,1x))')ATOM_SPECIES( ISPECIES ),(TRIM(LINE_WORDS(IPOS)),IPOS=1,IC)
                    IF( IC .LT. 4 )THEN
                       WRITE(6,'(A,1X,A,A)')'WARNING: ',ATOM_SPECIES( ISPECIES ),
     &                 ' does not have four item in trailing comment information. Will ignore information in items.'
                       EFLAG = .TRUE.
                       CYCLE
                    END IF
!                    write(6,'(12(a,1x))')LINE_WORDS(1:IC)
                    ATOMS_SPECIES_REPRESENTATIVE( ISPECIES ) = LINE_WORDS(1)
                    ATOMS_SPECIES_REPRESENTATION( ISPECIES ) = LINE_WORDS(2)
                    ATOMS_SPECIES_DSSTOX_ID     ( ISPECIES ) = LINE_WORDS(3)
                    IF( TRIM( LINE_WORDS(4) ) .NE. 'NA' 
     &                                         .AND. TRIM( LINE_WORDS(4) ) .NE. 'TBD' )THEN 
                       ATOMS_SPECIES_SMILES        ( ISPECIES ) = LINE_WORDS(4)
                       CALL COUNT_SMILES_ATOMS( ATOMS_SPECIES_SMILES(ISPECIES),
     &                                          ATOM_COUNT )
                       SPECIES_ATOMS(ISPECIES,1:N_ATOMS) = ATOM_COUNT(1:N_ATOMS)
                    END IF
                 
                 END IF ! atoms in namelist

              END DO          
              CLOSE( EXUNIT_ATOMS )

              IF ( EFLAG ) THEN
                 WRITE(6,'(A)')"WARNING: ABOVE ERRROR OCCURRED reading species namelists."
              END IF
              
              START_POSITION = 1
              STOP_POSITION = ISPECIES

              IF( ATOMS_IN_NAMELISTS )THEN  ! report on captured information
! write report of species composition
                  EXUNIT_ATOMS_REPORT = JUNIT()
                  EQNAME_ATOMS_REPORT = TRIM( OUTDIR ) // '/atom_counts_' 
     &                                // TRIM( MECHNAME_LOWER_CASE ) // '_species.dat '
                  OPEN( FILE=TRIM(EQNAME_ATOMS_REPORT),UNIT=EXUNIT_ATOMS_REPORT,STATUS='UNKNOWN' )                                      
                  WRITE(EXUNIT_ATOMS_REPORT,99000)(TRIM(ATOMS(IPOS)) // '_Atoms',IPOS=N_ATOMS,1,-1)
99000             FORMAT("SPECIES,Representative_Compound,Explicit_or_Lumped,DSS_Toxics_ID,Phase,",40(A,","))              
                  
                  DO IC = START_POSITION, STOP_POSITION
                     WRITE(EXUNIT_ATOMS_REPORT,'(A,",",F8.3,",",5(1X,A,","),25(1X,F9.4,","))')
     &               TRIM(ATOM_SPECIES(IC)),
     &               ATOMS_SPECIES_MOLWT( IC ),
     &               TRIM(ATOMS_SPECIES_REPRESENTATIVE( IC )),
     &               TRIM(ATOMS_SPECIES_REPRESENTATION( IC )),
     &               TRIM(ATOMS_SPECIES_DSSTOX_ID     ( IC )),
     &               TRIM(ATOMS_SPECIES_SMILES        ( IC )),
     &               TRIM(ATOMS_SPECIES_PHASE         ( IC )),
     &               (SPECIES_ATOMS(IC,IPOS),IPOS=N_ATOMS,1,-1)
                  END DO
                  CLOSE(EXUNIT_ATOMS_REPORT)
                  
                  
                   DO IATOM = 1,N_ATOMS                    
                      IF( MAXVAL( SPECIES_ATOMS( :,IATOM ) ) .GT. 0.0 )THEN
                          WRITE(6,'(A)')ATOMS(IATOM) // ' is present among atom species.'
                          ATOM_FOUND( IATOM ) = .TRUE.
                      END IF
                   END DO
             ELSE 
                   N_ATOM_SPECIES = 0
                   DEALLOCATE( ATOM_SPECIES,
     &                         ATOMS2MECH_MAP,
     &                         ATOMS_SPECIES_PHASE,
     &                         ATOMS_SPECIES_MOLWT )
             
             END IF          
             CLOSE(EXUNIT_ATOMS)

            END SUBROUTINE READ_MATRICES_ATOMS
            SUBROUTINE ALLOCATE_SMILES_ATOMS()
            
              USE MECHANISM_DATA

              IMPLICIT NONE

              N_ATOMS = 14
              ALLOCATE( ATOMS(N_ATOMS) )
              ATOMS(1:N_ATOMS) = ( / 'CA', 'MN', 'CL', 'HG', 'BR', 'NA', 'SI', 'S ', 
     &                               'TI', 'FE', 'K ', 'I ', 'N ', 'C ' / )

              ALLOCATE( SPECIES_ATOMS ( N_ATOM_SPECIES,N_ATOMS),
     &                  ATOMS_SPECIES_REPRESENTATIVE( N_ATOM_SPECIES ),
     &                  ATOMS_SPECIES_REPRESENTATION( N_ATOM_SPECIES ),
     &                  ATOMS_SPECIES_DSSTOX_ID     ( N_ATOM_SPECIES ),
     &                  ATOMS_SPECIES_SMILES        ( N_ATOM_SPECIES ),
     &                  MECH_SPECIES_ATOMS( MAXSPEC,N_ATOMS))
                    
              
              ATOMS_SPECIES_REPRESENTATIVE = 'NA'
              ATOMS_SPECIES_REPRESENTATION = 'NA'
              ATOMS_SPECIES_DSSTOX_ID      = 'NA'
              ATOMS_SPECIES_SMILES         = 'NA'
              SPECIES_ATOMS                = 0.0
              MECH_SPECIES_ATOMS           = 0.0
              
              ALLOCATE( ATOM_FOUND(N_ATOMS) )
              
              ATOM_FOUND = .FALSE.
              
            END SUBROUTINE ALLOCATE_SMILES_ATOMS
            SUBROUTINE SET_ATOMS_MECHANISM_SPC( )

              USE MECHANISM_DATA
            
               IMPLICIT NONE
               
               
               INTEGER  :: ISPC, IATOM_SPC, IMAP
               LOGICAL  :: EFLAG 
               LOGICAL  :: ATOMS_FOUND
               
               EFLAG = .FALSE.
                  
               MECH_SPCS: DO ISPC = 1, NUMB_MECH_SPCS
                  ATOMS_FOUND = .FALSE.
                  ATOMS_SPC: DO IATOM_SPC = 1, N_ATOM_SPECIES 
                     IF( TRIM(MECHANISM_SPC( ISPC )) .EQ. TRIM(ATOM_SPECIES( IATOM_SPC )) )THEN
                         MECH_SPECIES_ATOMS( ISPC,1:N_ATOMS ) = SPECIES_ATOMS( IATOM_SPC,1:N_ATOMS )
!                         print*,iatom_spc,size(ATOMS2MECH_MAP)
                         ATOMS2MECH_MAP( IATOM_SPC ) = ISPC
                         ATOMS_FOUND = .TRUE.
                         EXIT
                     END IF
                  END DO ATOMS_SPC
                  IF( .NOT. ATOMS_FOUND )THEN
                     EFLAG = .TRUE.
                     WRITE(6,2002)TRIM( MECHANISM_SPC( ISPC ) )
                  END IF
               END DO MECH_SPCS
               
               IF( EFLAG )THEN
                 WRITE(6,*)'Above FATAL ERROR when mapping atoms species to mechanism species.'
                 STOP
               END IF

2002           FORMAT('mechanism species, ', A, ', not found in ATOM species.')
             
            END SUBROUTINE SET_ATOMS_MECHANISM_SPC         
            SUBROUTINE REACTION_DELTA_ATOMS( )
            
              USE MECHANISM_DATA

               IMPLICIT NONE
                              
               INTEGER  :: IRXN, IREACT, IPRODUCT, IATOM
               INTEGER  :: IPOS1, IPOS2
               
               CHARACTER( 16 ) :: COEFF_STRING
               
               LOGICAL       :: EFLAG 
               LOGICAL, SAVE :: FIRSTCALL = .TRUE.
               
               EFLAG = .FALSE.
               
                  
               IF( FIRSTCALL )THEN
               
                   ALLOCATE( REACTION_DELTA(NRXNS,N_ATOMS),
     &                       DELTA_ATOMS(NRXNS),
     &                       NONZERO_DELTA(NRXNS,N_ATOMS))   
                   
                   REACTION_DELTA  = 0.0
                   NONZERO_DELTA   = .FALSE.
                   DELTA_ATOMS     = ' '
               
               END IF

               DELTA_ATOMS     = ' '
               
               
               DO IRXN = 1, NRXNS                 
                  DO IATOM = 1, N_ATOMS
                     IF( FIRSTCALL )THEN
                        REACTION_DELTA(IRXN,IATOM) = 0.0
                        DO IREACT = 1,3
                           IF( IRR(IRXN,IREACT) .GT. 0 )THEN
                               REACTION_DELTA(IRXN,IATOM) = REACTION_DELTA(IRXN,IATOM)
     &                                                    + MECH_SPECIES_ATOMS( IRR(IRXN,IREACT),IATOM )
                           END IF 
                        END DO
                        DO IPRODUCT = 1,MAXPRODS
                           IF( IRR(IRXN,IPRODUCT+3) .GT. 0 )THEN
                               REACTION_DELTA(IRXN,IATOM) = REACTION_DELTA(IRXN,IATOM)
     &                                                    - SC(IRXN,IPRODUCT)*MECH_SPECIES_ATOMS( IRR(IRXN,IPRODUCT+3),IATOM )
                           END IF
                        END DO
                        IF( ABS( REACTION_DELTA(IRXN,IATOM) ) .LE. 9.99999E-8 )THEN
                            REACTION_DELTA(IRXN,IATOM) = 0.0
                        END IF
                     END IF  
                     IPOS1 = LEN_TRIM( DELTA_ATOMS(IRXN) ) + 1
                     IPOS2 = IPOS1 + 23
                     IF( REACTION_DELTA(IRXN,IATOM) .GT. 0.0 )THEN
!                        WRITE(DELTA_ATOMS(IRXN)(IPOS1:IPOS2),'(" + ",F7.4,"*DELTA_",A," ")')
!     &                  REACTION_DELTA(IRXN,IATOM),TRIM(ATOMS(IATOM))
                        COEFF_STRING = CONVERT_NUMBER(ABS(REACTION_DELTA(IRXN,IATOM)))
                        WRITE(DELTA_ATOMS(IRXN)(IPOS1:IPOS2),'(" + ",A,"*DELTA_",A," ")')
     &                  TRIM(COEFF_STRING),TRIM(ATOMS(IATOM))
                        NONZERO_DELTA(IRXN,IATOM) = .TRUE.
                     ELSE  IF( REACTION_DELTA(IRXN,IATOM) .LT. -1.0E-7 )THEN
!                        WRITE(DELTA_ATOMS(IRXN)(IPOS1:IPOS2),'(" - ",F10.7,"*DELTA_",A," ")')
!     &                  ABS( REACTION_DELTA(IRXN,IATOM) ),TRIM(ATOMS(IATOM))
                         COEFF_STRING = CONVERT_NUMBER(ABS(REACTION_DELTA(IRXN,IATOM)))
                        WRITE(DELTA_ATOMS(IRXN)(IPOS1:IPOS2),'(" - ",A,"*DELTA_",A," ")')
     &                  TRIM(COEFF_STRING),TRIM(ATOMS(IATOM))
                        NONZERO_DELTA(IRXN,IATOM) = .TRUE.
                     END IF
                  END DO
               END DO

               IF( FIRSTCALL )THEN
                  DO IATOM = 1,N_ATOMS
                     IF( MAXVAL( ABS( REACTION_DELTA( :,IATOM ) ) ) .GT. 0.0 )THEN
                         print*,ATOMS(IATOM),' is present among mechanism species: ',MAXVAL( ABS( REACTION_DELTA( :,IATOM ) ) )
                         ATOM_FOUND( IATOM ) = .TRUE.
                         NONZERO_ATOMS       = .TRUE.
                     ELSE
                         ATOM_FOUND(IATOM) = .FALSE.  
                     END IF
                  END DO
                  FIRSTCALL = .FALSE.
               END IF
             
            END SUBROUTINE REACTION_DELTA_ATOMS
         SUBROUTINE READ_SPECIES_ATOMS()

           USE GET_ENV_VARS
           USE MECHANISM_DATA

           IMPLICIT NONE

! parameter to used to convert case in line read
           INTEGER, PARAMETER :: STRT   =  97
           INTEGER, PARAMETER :: FINI   = 122
           INTEGER, PARAMETER :: FACTOR = -32
           INTEGER, PARAMETER :: MAX_WORDS = 200 ! max number of words in Header

           
           CHARACTER( 16 )     :: ATOMS_FILE = 'ATOMS_FILE'
           INTEGER, EXTERNAL  :: JUNIT
     
           LOGICAL :: FILE_EXISTS  = .TRUE.
           LOGICAL :: FOUND_HEADER = .FALSE.
           LOGICAL :: SMILES_FILE  = .FALSE.
           LOGICAL :: EFLAG        = .FALSE.
           
           INTEGER :: NLINES_FILE
           INTEGER :: NLINE
           INTEGER :: IPOS, IC
           INTEGER :: POSITION_HEADER
           INTEGER :: NWORDS
           INTEGER :: ISPECIES, IATOM, IWORD
           INTEGER :: START_POSITION, STOP_POSITION
           INTEGER :: IO_STATUS 
           INTEGER :: LINES_IGNORED 
           INTEGER :: ICOLUMN_SMILES = 0

           REAL, ALLOCATABLE  :: ATOM_COUNT( : )
           
           CHARACTER(586)              :: FILE_LINE, LINE_CONTENT 
           CHARACTER(100), ALLOCATABLE :: LINE_WORDS(:)
           
                     
           EXUNIT_ATOMS = JUNIT()
           CALL VALUE_NAME ( ATOMS_FILE, EQNAME_ATOMS )
           INQUIRE( FILE = TRIM( EQNAME_ATOMS ), EXIST = FILE_EXISTS )
           IF( .NOT. FILE_EXISTS )THEN
               WRITE(6,'(A,/,A)')'ERROR: CANNOT LOCATE the ATOMS_FILE: ', 
     &                            TRIM(EQNAME_ATOMS)
               WRITE(6,'(A)')'ERROR: SET BALANCE_ATOM option to "N" if file is not to used.'
               STOP
           END IF


           OPEN ( UNIT = EXUNIT_ATOMS, FILE = EQNAME_ATOMS, STATUS = 'UNKNOWN' )

           ALLOCATE( LINE_WORDS(MAX_WORDS) )

           NLINES_FILE   = 0
           LINES_IGNORED = 0
           
           DO
              READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
              IF( IO_STATUS .NE. 0 )THEN
                  EXIT
              END IF
              NLINES_FILE = NLINES_FILE + 1
              FILE_LINE = ADJUSTL(FILE_LINE)   
              IF ( INDEX( FILE_LINE,'SMILES' ) .GT. 0 ) THEN
                 SMILES_FILE     = .TRUE.
                 FOUND_HEADER    = .TRUE.
                 POSITION_HEADER = NLINES_FILE
                 LINES_IGNORED   = LINES_IGNORED + 1
                 CALL PARSE_STRING(FILE_LINE,NWORDS,LINE_WORDS)
! find column number giving the species SMILES               
                 DO IWORD = 1,NWORDS
                    IF( TRIM( ADJUSTL( LINE_WORDS(IWORD) ) ) .EQ. "SMILES" )THEN
                        ICOLUMN_SMILES = IWORD
                        EXIT
                    END IF
                 END DO
              END IF
           END DO
           
           REWIND( EXUNIT_ATOMS )
           
           DO NLINE = 1,NLINES_FILE
              READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
! skip blank lines and comment lines
              IF( LEN_TRIM(FILE_LINE) .LE. 0 )THEN
                  LINES_IGNORED = LINES_IGNORED + 1
                  CYCLE
              END IF         
              IF( LINE_CONTENT(1:1) .EQ. '!' )THEN
                  LINES_IGNORED = LINES_IGNORED + 1
                  CYCLE
              END IF  
              IF( SMILES_FILE )CYCLE  
! subset line to remove starting and ending white space
              STOP_POSITION = LEN_TRIM(FILE_LINE)
              FIND_START: DO IPOS = 1,STOP_POSITION
                 IF( FILE_LINE(IPOS:IPOS) .NE. ' ' )THEN
                     START_POSITION = IPOS
                     EXIT FIND_START
                 END IF
              END DO FIND_START
              LINE_CONTENT = FILE_LINE(START_POSITION:STOP_POSITION)
              FILE_LINE = LINE_CONTENT
! find header line that start with word, "SPECIES" to find atoms used
              LOOP_CASE: DO IPOS = 1,(STOP_POSITION-START_POSITION+1)
                 IC = ICHAR ( FILE_LINE ( IPOS:IPOS ) )
                 IF ( IC .GE. STRT  .AND.  IC .LE. FINI ) THEN
                    FILE_LINE ( IPOS:IPOS ) = CHAR ( IC + FACTOR )
                 END IF
              END DO LOOP_CASE
              write(6,'(i3,1x,a)')nline,TRIM(FILE_LINE)

              IF( FILE_LINE(1:7) .EQ. 'SPECIES' )THEN 

              write(6,'(i3,1x,a)')nline,TRIM(FILE_LINE)
                  
! found header line so string for names of atoms
                  IF( FOUND_HEADER )THEN
                     WRITE(6,'(A)')'ERROR: ATOMS_FILE has two header lines.'
                     STOP               
                  ELSE
                    FOUND_HEADER = .TRUE.
                  END IF
                  POSITION_HEADER = NLINE
                  LINES_IGNORED = LINES_IGNORED + 1
                  CALL PARSE_STRING(LINE_CONTENT,N_ATOMS,LINE_WORDS)
                  N_ATOMS = N_ATOMS - 1
                  ALLOCATE( ATOMS(N_ATOMS) )
                  ATOMS(1:N_ATOMS) = LINE_WORDS(2:N_ATOMS+1)
              END IF
           END DO


           IF( .NOT. FOUND_HEADER) THEN
              WRITE(6,'(A)')'ERROR: ATOMS_FILE is missing header line listing atoms'
              STOP               
           END IF
           REWIND(EXUNIT_ATOMS)           

           N_ATOM_SPECIES = NLINES_FILE - LINES_IGNORED

           IF( N_ATOM_SPECIES .LT. 1 )THEN
               WRITE(6,'(A)')'ERROR: ATOMS_FILE is missing header line listing atoms'
               STOP               
           END IF
                           
           IF( SMILES_FILE )THEN ! read species and their SMILES from atoms file

              ALLOCATE( ATOM_SPECIES  ( N_ATOM_SPECIES ),
     &                  ATOMS2MECH_MAP( N_ATOM_SPECIES ) )

              ATOM_SPECIES   = ''
              ATOMS2MECH_MAP = -1
                 
              CALL ALLOCATE_SMILES_ATOMS()
              
              ALLOCATE( ATOM_COUNT( N_ATOMS ) )
              ATOM_COUNT = 0.0
           
              ISPECIES = 0
              DO NLINE = 1,NLINES_FILE
                 READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
                 LINE_CONTENT = ADJUSTL( FILE_LINE )
! skip blank lines, comment lines, and header line
                 IF( LEN_TRIM(LINE_CONTENT) .LE. 0 )CYCLE
                 IF( LINE_CONTENT(1:1) .EQ. '!' )CYCLE
                 IF( NLINE .EQ. POSITION_HEADER )CYCLE
                 ISPECIES = ISPECIES + 1
                 START_POSITION = 1
                 STOP_POSITION  = LEN_TRIM( LINE_CONTENT )
                 IC = 0
                 DO
                    IPOS = INDEX( LINE_CONTENT(START_POSITION:STOP_POSITION),"," )
                    IF ( IPOS .EQ. 0 )EXIT
                    IF ( IPOS .EQ. STOP_POSITION )EXIT
                    IC = IC + 1
                    LINE_WORDS( IC ) = TRIM( ADJUSTL(LINE_CONTENT(START_POSITION:IPOS-1)) )
                    IPOS = IPOS + 1
                    IF ( IPOS .GE. STOP_POSITION )EXIT
                    LINE_CONTENT     = LINE_CONTENT(IPOS:STOP_POSITION)
                 END DO
                 IF( LEN_TRIM(LINE_CONTENT) .GT. 0 )THEN
                    IC = IC + 1
                    LINE_WORDS( IC ) = TRIM( ADJUSTL(LINE_CONTENT) )
                 END IF   
                 IF( IC .LT. 4 )THEN
                    WRITE(6,'(A,1X,A,A)')'WARNING: ',ATOM_SPECIES( ISPECIES ),
     &              ' does not have four items in trailing comment information. Will ignore information in items.'
                    EFLAG = .TRUE.
                    CYCLE
                 END IF
                 ATOM_SPECIES( ISPECIES )                 = LINE_WORDS(1)
                 ATOMS_SPECIES_REPRESENTATIVE( ISPECIES ) = LINE_WORDS(2)
                 ATOMS_SPECIES_REPRESENTATION( ISPECIES ) = LINE_WORDS(3)
                 ATOMS_SPECIES_DSSTOX_ID     ( ISPECIES ) = LINE_WORDS(4)
                 IF( TRIM( LINE_WORDS(5) ) .NE. 'NA' 
     &                                      .AND. TRIM( LINE_WORDS(5) ) .NE. 'TBD' )THEN 
                    ATOMS_SPECIES_SMILES        ( ISPECIES ) = LINE_WORDS(5)
                    CALL COUNT_SMILES_ATOMS( ATOMS_SPECIES_SMILES(ISPECIES),
     &                                       ATOM_COUNT )
                    SPECIES_ATOMS(ISPECIES,1:N_ATOMS) = ATOM_COUNT(1:N_ATOMS)
                 END IF
              END DO          

              IF ( EFLAG ) THEN
                 WRITE(6,'(A)')"WARNING: ABOVE ERRROR OCCURRED reading species namelists."
              END IF
              
              START_POSITION = 1
              STOP_POSITION  = ISPECIES

! write report of species composition
              EXUNIT_ATOMS_REPORT = JUNIT()
              EQNAME_ATOMS_REPORT = TRIM( OUTDIR ) // '/atom_counts_' 
     &                            // TRIM( MECHNAME_LOWER_CASE ) // '_species.dat '
              OPEN( FILE=TRIM(EQNAME_ATOMS_REPORT),UNIT=EXUNIT_ATOMS_REPORT,STATUS='UNKNOWN' )                                      
              WRITE(EXUNIT_ATOMS_REPORT,99000)(TRIM(ATOMS(IPOS)) // '_Atoms',IPOS=N_ATOMS,1,-1)
99000         FORMAT("SPECIES,Representative_Compound,Explicit_or_Lumped,DSS_Toxics_ID,",40(A,","))              

              DO IC = START_POSITION, STOP_POSITION
                 WRITE(EXUNIT_ATOMS_REPORT,'(A,",",4(1X,A,","),25(1X,F9.4,","))')
     &           TRIM(ATOM_SPECIES(IC)),
     &           TRIM(ATOMS_SPECIES_REPRESENTATIVE( IC )),
     &           TRIM(ATOMS_SPECIES_REPRESENTATION( IC )),
     &           TRIM(ATOMS_SPECIES_DSSTOX_ID     ( IC )),
     &           TRIM(ATOMS_SPECIES_SMILES        ( IC )),
     &           (SPECIES_ATOMS(IC,IPOS),IPOS=N_ATOMS,1,-1)
              END DO
              CLOSE(EXUNIT_ATOMS_REPORT)           


           ELSE ! read species and their counts per atom 



                ALLOCATE(    ATOM_SPECIES  ( N_ATOM_SPECIES ),
     &                       ATOMS2MECH_MAP( N_ATOM_SPECIES ),
     &                       SPECIES_ATOMS ( N_ATOM_SPECIES,N_ATOMS),
     &                       MECH_SPECIES_ATOMS( MAXSPEC,N_ATOMS))
                
                ALLOCATE( ATOM_FOUND(N_ATOMS) )
                
                ATOM_SPECIES   = ''
                ATOMS2MECH_MAP = -1
                SPECIES_ATOMS  = 0.0
                MECH_SPECIES_ATOMS = 0.0
                ATOM_FOUND = .FALSE.
                
                ISPECIES = 0
                DO NLINE = 1,NLINES_FILE
                    READ(EXUNIT_ATOMS,'(A)',IOSTAT=IO_STATUS)FILE_LINE
! skip blank lines, comment lines, and header line
                    IF( LEN_TRIM(FILE_LINE) .LE. 0 )CYCLE
                    IF( LINE_CONTENT(1:1) .EQ. '!' )CYCLE
                    IF( NLINE .EQ. POSITION_HEADER )CYCLE
! subset line to remove starting and ending white space
                    STOP_POSITION = LEN_TRIM(FILE_LINE)
                    LOCATE_START: DO IPOS = 1,STOP_POSITION
                        IF( FILE_LINE(IPOS:IPOS) .NE. ' ' )THEN
                            START_POSITION = IPOS
                            EXIT LOCATE_START
                        END IF
                    END DO LOCATE_START
                    LINE_CONTENT = FILE_LINE(START_POSITION:STOP_POSITION)
! read species composition
                    ISPECIES = ISPECIES + 1
                    CALL PARSE_STRING(LINE_CONTENT,IC,LINE_WORDS)
                    IF( IC .LT. N_ATOMS+1 )THEN
                        WRITE(6,'(A)')'ERROR IN ATOMS_FILE, below line missing information:'
                        WRITE(6,'(A)')TRIM(LINE_CONTENT)
                        STOP
                    END IF
!                      write(6,'(12(a,1x))')LINE_WORDS(1:IC)
                    ATOM_SPECIES( ISPECIES ) = LINE_WORDS(1)
!                      write(6,'(a)')ATOM_SPECIES( ISPECIES )
                    DO IATOM = 1, N_ATOMS
                        READ(LINE_WORDS(IATOM+1),*)SPECIES_ATOMS(ISPECIES,IATOM)
!                         write(6,'(5x,a,f8.3)')ATOMS(IATOM),SPECIES_ATOMS(ISPECIES,IATOM)
                    END DO
                END DO           

           END IF 
           
   
           DO IATOM = 1,N_ATOMS                    
              IF( MAXVAL( SPECIES_ATOMS( :,IATOM ) ) .GT. 0.0 )THEN
!                  print*,ATOMS(IATOM),' is present among atom species.'
                  ATOM_FOUND( IATOM ) = .TRUE.
              END IF
           END DO

           CLOSE(EXUNIT_ATOMS)
         END SUBROUTINE READ_SPECIES_ATOMS            
         SUBROUTINE ARE_THEY_ATOM_SPECIES( )

!=======================================================================
! Determines whether mechanism species found in species namelist
!=======================================================================
           USE MECHANISM_DATA
      
           IMPLICIT NONE

! Arguments: None

! Local:
           INTEGER :: ISPC, JSPC
           LOGICAL :: FOUND_ATOMS
           
           FOUND_ATOMS = .TRUE.
           MECH_SPECIES: DO JSPC = 1,NUMB_MECH_SPCS
              DO ISPC = 1, N_ATOM_SPECIES
                 IF ( TRIM( SPARSE_SPECIES( JSPC ) ) .EQ. TRIM( ATOM_SPECIES( ISPC ) ) ) THEN   ! found
                    MECH_SPECIES_ATOMS( JSPC,1:N_ATOMS ) = SPECIES_ATOMS( ISPC,1:N_ATOMS )
                    CYCLE MECH_SPECIES
                 END IF
              END DO
              FOUND_ATOMS = .FALSE.
              WRITE( 6,2002 ) TRIM( SPARSE_SPECIES( JSPC ) )
           END DO MECH_SPECIES
           
           IF ( .NOT. FOUND_ATOMS ) THEN
              WRITE( 6,'(/,A,/)') 'Check above error messages and make needed corrections.'
              STOP
           END IF         
           
           RETURN
2002       FORMAT( / 5X, '*** ERROR: ',
     &                   'MECH species, ', A, ', not found in namelist or atom species.',
     &             / 5X, 'Need to add to MECH species to a appropriate namelist or atoms file.' )
           
         END SUBROUTINE ARE_THEY_ATOM_SPECIES
            INTEGER FUNCTION COUNT_SPECIES( IUNIT,DELIMINATOR )
!    counts number species in open file. A species lines start with the DELIMINATOR
!    arguement.        
                IMPLICIT NONE
                
                INTEGER,       INTENT( IN ) :: IUNIT  ! file unit 
                CHARACTER*(*), INTENT( IN ) :: DELIMINATOR
                
                CHARACTER(586)              :: FILE_LINE 
                CHARACTER(LEN(DELIMINATOR)) :: DELIMINATE
                INTEGER                     :: NSPECIES
                INTEGER                     :: IO_STATUS
                
                COUNT_SPECIES = 0
                DELIMINATE     = TRIM( ADJUSTL( DELIMINATOR ) )
                DO
                  READ(IUNIT,'(A)',IOSTAT=IO_STATUS)FILE_LINE
                  IF( IO_STATUS .NE. 0 )THEN
                      EXIT
                  END IF
                  FILE_LINE = ADJUSTL( FILE_LINE )
                  IF( FILE_LINE(1:1) .EQ. TRIM(DELIMINATE) )COUNT_SPECIES = COUNT_SPECIES + 1
                END DO
                
           END FUNCTION COUNT_SPECIES
                   
           SUBROUTINE COUNT_SMILES_ATOMS( SPECIES_SMILES,ATOM_COUNT )
           
              USE MECHANISM_DATA
           
              IMPLICIT NONE
              
               CHARACTER*(*), INTENT( IN    )  :: SPECIES_SMILES
               REAL,          INTENT( INOUT ) :: ATOM_COUNT( : )
               
               CHARACTER(LEN(SPECIES_SMILES)) :: SMILES,CUT_UP
               INTEGER                        :: LEN_SMILES
               INTEGER                        :: LEN_ATOM
               INTEGER                        :: IATOM, IPOS
               INTEGER                        :: countsubstring
               
               SMILES = TRIM( ADJUSTL( SPECIES_SMILES ) )
               CALL UCASE( SMILES )
               LEN_SMILES = LEN_TRIM( SMILES )        
                 
               
               DO IATOM = 1,N_ATOMS
                  ATOM_COUNT(IATOM) = 0.0
                  ATOM_COUNT(IATOM) = Count_Text(SMILES, ATOMS(IATOM))
                  SMILES            = Replace_Text( SMILES,ATOMS(IATOM)," ")
!                  LEN_ATOM          = LEN_TRIM(ATOMS(IATOM))
!                  CUT_UP            = SMILES
!                  DO 
!                     LEN_SMILES = LEN_TRIM( CUT_UP )        
!                     IPOS =  INDEX( SMILES(1:LEN_SMILES),ATOMS(IATOM)(1:LEN_ATOM) ) 
!                     IF( IPOS .LT. 1  )EXIT
!                     IF( LEN_SMILES .EQ. LEN_ATOM )THEN
!                        SMILES = " "
!                     ELSE
!                        SMILES = SMILES(MIN(IPOS+1,LEN_SMILES):LEN_SMILES)
!                     END IF
!                     ATOM_COUNT(IATOM) = ATOM_COUNT(IATOM)+1
!!                     write(6,'((2A,1X),10(F8.2,1X))')ATOMS(IATOM),TRIM(SMILES),ATOM_COUNT(IATOM),REAL(IPOS)                  
!                  END DO
!                  write(6,'(3(A,1X),10(F8.2,1X))')ATOMS(IATOM),TRIM( ADJUSTL( SPECIES_SMILES ) ),
!     &            SMILES(1:LEN_SMILES),ATOM_COUNT(IATOM)
               END DO 
               
           END SUBROUTINE COUNT_SMILES_ATOMS   
            function countsubstring(s1, s2) result(c)
            
                IMPLICIT NONE
                character*(*), intent(in) :: s1, s2
                integer :: c, p, posn
                
                c = 0
                if(len(s2) == 0) return
                p = 1
                do 
                  posn = index(s1(p:), s2)
!                  print*,trim(s1(p:)),trim(s2)

                  if(posn == 0) return
                  c = c + 1
                  p = p + posn + len(s2) - 1
                end do
                
           end function countsubstring
           SUBROUTINE WRITE_REACTION (iunit,text,width)  
              Implicit None
   
              CHARACTER*(*), Intent( In ) :: text
              INTEGER,       Intent( In)  :: iunit
              INTEGER,       Intent( In)  :: width
   
              CHARACTER(LEN(text))        :: outs     ! provide outs with extra 100 char len
              CHARACTER(LEN(text))        :: pad
              CHARACTER(1)                :: rate_deliminator
              INTEGER                     :: i, no, nr, nt
              INTEGER                     :: icut
              INTEGER                     :: indent
              INTEGER                     :: start
              INTEGER                     :: iprecent
              LOGICAL                     :: FIRST_LINE
              LOGICAL                     :: FALLOUT_RATE
              
   
              outs   = TRIM( ADJUSTL(text) )
              pad(:) = ' '
              indent = max(1,index(outs,"=")-1)
              If( index(outs,"%") .gt. 0 )then
                 FALLOUT_RATE = .True.
              Else
                 FALLOUT_RATE = .False.
              End if
              FIRST_LINE    = .True.
              Paragraph: Do
                 nt = LEN_TRIM(outs)
                 If( .Not. FIRST_LINE )nt = nt + indent !  + 10
                 If( nt .le. width)then
                    If( FIRST_LINE )Then
                       write(iunit,'(a)')outs(1:nt)
                    Else
                       write(iunit,'(2a)')pad(1:indent),outs(1:nt)
                    End If
                    Exit Paragraph
                 Else
                    icut = width+1
                    If( FIRST_LINE )then
                        start = width
                    Else
                        start = max(width-indent,1)
                    End IF
                    Find_Brake: Do i = start,1,-1
!                       If( i .lt. start )
                       If( FALLOUT_RATE )then 
                         If( outs(i:i) .eq. '%' )then
                             icut = i-1
                             Exit Find_Brake
                         End If
                       Else 
                          If( outs(i:i) .eq. '#' )then
                             icut = i-1
                             Exit Find_Brake
                          End If
                       End IF
                       If( outs(i:i+2) .eq. ' + ' .or. outs(i:i+2) .eq. ' - ' )then
                         icut = i
                         Exit Find_Brake
                       End if
                       If( ichar(outs(i:i)) .eq. 38 )then ! cut at '&' 
                         icut = i-1
                         Exit Find_Brake
                       End If
                    End Do Find_Brake
                    icut = max(icut,1)
                    If( FIRST_LINE )Then
                       write(iunit,'(a)')outs(1:icut)
                    Else
                       write(iunit,'(2a)')pad(1:indent),outs(1:icut)
                    End IF
                    outs(1:) = outs(icut+1:)
                 End if
                 FIRST_LINE = .False.
             End do Paragraph
           END SUBROUTINE WRITE_REACTION
            SUBROUTINE WRITE_DELTA_MATH(IUNIT,IRXN)
               USE MECHANISM_DATA

               IMPLICIT NONE
! Arguments:
               INTEGER, INTENT( IN    ) ::  IUNIT  ! I/O unit #
               INTEGER, INTENT( IN    ) ::  IRXN   ! reaction index
! Local:
               INTEGER  :: IATOM               
               INTEGER  :: IREACT
               INTEGER  :: IPRODUCT
               REAL     :: REACTION_BALANCE
               
               LOGICAL, SAVE :: WRITTEN = .FALSE.
               
               IF( WRITTEN )RETURN
               
               IF( IRXN .GE. NRXNS ) WRITTEN = .TRUE.
               
               DO IATOM = N_ATOMS,1,-1
                  IF( ATOM_FOUND( IATOM ) )THEN
                     REACTION_BALANCE = 0.0
                     WRITE(IUNIT,'(A)',ADVANCE='NO')"! DELTA_" // TRIM(ATOMS(IATOM)) // ' = '                     
                     DO IREACT = 1,3
                        IF( IRR(IRXN,IREACT) .GT. 0 )THEN
                            REACTION_BALANCE = REACTION_BALANCE
     &                                       + MECH_SPECIES_ATOMS( IRR(IRXN,IREACT),IATOM )
                            WRITE(IUNIT,'(A,F6.3,"*",A)',ADVANCE='NO')' + ',MECH_SPECIES_ATOMS( IRR(IRXN,IREACT),IATOM ),
     &                      TRIM(ATOMS(IATOM) )                           
                        END IF 
                     END DO
                     DO IPRODUCT = 1,MAXPRODS
                        IF( IRR(IRXN,IPRODUCT+3) .GT. 0 )THEN
                            REACTION_BALANCE = REACTION_BALANCE
     &                                       - SC(IRXN,IPRODUCT)*MECH_SPECIES_ATOMS( IRR(IRXN,IPRODUCT+3),IATOM )
                            IF( SC(IRXN,IPRODUCT) .GT. 0.0 )THEN
                               WRITE(IUNIT,'(A,F6.3,"*",F10.7,"*",A)',ADVANCE='NO')' - ', 
     &                         MECH_SPECIES_ATOMS( IRR(IRXN,IPRODUCT+3),IATOM ),SC(IRXN,IPRODUCT),
     &                         TRIM(ATOMS(IATOM))
                            ELSE
                               WRITE(IUNIT,'(A,F6.3,"*",F10.7,"*",A)',ADVANCE='NO')' + ', 
     &                         MECH_SPECIES_ATOMS( IRR(IRXN,IPRODUCT+3),IATOM ),SC(IRXN,IPRODUCT),
     &                         TRIM(ATOMS(IATOM))
                            END IF
                        END IF                   
                     END DO
                     WRITE(IUNIT,'(" = ",F10.7)') REACTION_DELTA(IRXN,IATOM)
!                     WRITE(IUNIT,(" "))
                   END IF
                END DO
            END SUBROUTINE WRITE_DELTA_MATH
            FUNCTION CONVERT_REAL4( NUMBER ) RESULT( WORD )
            
               IMPLICIT NONE
               
               REAL(4), INTENT( IN ) :: NUMBER 
               
               CHARACTER( 16 ) :: WORD
               INTEGER         :: IPOS
               INTEGER         :: IPOS_DOT
               
               IF( PRECISION_CONVERT .LE. 6 )THEN
                   WRITE(WORD,'(F14.9)')ABS( NUMBER ) + OFF_SET
               END IF
               
               WORD = ADJUSTL( WORD )
               IPOS_DOT = INDEX(WORD,".",BACK=.TRUE.)+PRECISION_CONVERT
               WORD = WORD(1:IPOS_DOT)
               RETURN
!               DO
!                 IPOS = LEN_TRIM( WORD )
!                 IF( WORD(IPOS:IPOS) .NE. '0' )EXIT
!                 IPOS = IPOS-1
!                 IF( IPOS .LE. IPOS_DOT .OR. IPOS .GT. IPOS_DOT )EXIT                 
!                 WORD = WORD(1:IPOS)
!               END DO
                           
            END FUNCTION CONVERT_REAL4
            FUNCTION CONVERT_REAL8( NUMBER ) RESULT( WORD )
            
               IMPLICIT NONE
               
               REAL(8), INTENT( IN ) :: NUMBER 
               
               CHARACTER( 16 ) :: WORD
               INTEGER         :: IPOS
               INTEGER         :: IPOS_DOT
               
               IF( PRECISION_CONVERT .LE. 6 )THEN
                   WRITE(WORD,'(F14.9)')ABS( NUMBER ) + OFF_SET
               END IF

               WORD = ADJUSTL( WORD )
               IPOS_DOT = INDEX(WORD,".",BACK=.TRUE.)+PRECISION_CONVERT
               WORD = WORD(1:IPOS_DOT)
               RETURN
!               DO
!                 IPOS = LEN_TRIM( WORD )
!                 IF( WORD(IPOS:IPOS) .NE. '0' )EXIT
!                 IPOS = IPOS-1
!                 IF( IPOS .LE. IPOS_DOT .OR. IPOS .GT. IPOS_DOT )EXIT                 
!                 IPOS = INDEX(WORD,"0",BACK=.TRUE.)-1
!                 WORD = WORD(1:IPOS)
!               END DO
                           
            END FUNCTION CONVERT_REAL8
            FUNCTION CONVERT_INTEGER( NUMBER ) RESULT( WORD )
            
               IMPLICIT NONE
               
               INTEGER, INTENT( IN ) :: NUMBER 
               
               CHARACTER( 16 ) :: WORD
               INTEGER         :: IPOS
               INTEGER         :: IPOS_DOT

               IF( PRECISION_CONVERT .LE. 6 )THEN
                   WRITE(WORD,'(F14.9)')ABS( NUMBER ) + OFF_SET
               END IF

               WORD = ADJUSTL( WORD )
               IPOS_DOT = INDEX(WORD,".",BACK=.TRUE.)+PRECISION_CONVERT
               WORD = WORD(1:IPOS_DOT)
               RETURN
!               DO
!                 IPOS = LEN_TRIM( WORD )
!                 IF( WORD(IPOS:IPOS) .NE. '0' )EXIT
!                 IPOS = IPOS-1
!                 IF( IPOS .LE. IPOS_DOT .OR. IPOS .GT. IPOS_DOT+5 )EXIT                 
!                 WORD = WORD(1:IPOS)
!               END DO
                           
            END FUNCTION CONVERT_INTEGER

        END MODULE SPECIES_ATOMS_DATA
