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

      MODULE SPECIES_DEF

C*************************************************************************
C
C  FUNCTION:  To define a ozone species variable
C             
C  PRECONDITIONS: Species read from environment variables OZONE
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: GET_SPECS
C
C                   
C*************************************************************************

      TYPE SPEC_VAR

         CHARACTER(len=16) OBS_NAME
         CHARACTER(len=16) OBS_UNITS
         REAL              OBS_FACTOR

         INTEGER           NUMSPEC
         CHARACTER(len=80) MOD_EXPRESSION
         CHARACTER(len=16) MOD_UNITS
         CHARACTER(len=16) MOD_NAME(10)
         REAL              MOD_FACTOR(10)

      END TYPE SPEC_VAR

      TYPE ( SPEC_VAR ) SPECIES

      CONTAINS

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C   ROUTINE TO GET SPECIES DEFINITIONS FROM ENVIRONMENT VARIABLES
C
C   VARIABLES NAMES OZONE
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         SUBROUTINE GET_SPECS

         IMPLICIT NONE 
         
         !Extrenal functions
         INTEGER getParsedNumber
         REAL    ENVREAL

         ! local variables
         INTEGER            N
         INTEGER            STATUS        
         CHARACTER(LEN=256) RECORD
         CHARACTER(LEN=256) FIELD 

   
         SPECIES%OBS_NAME = 'OZONE'    
         SPECIES%OBS_UNITS = ' '

         ! GET FACTOR FOR OBSERVED VALUES
         SPECIES%OBS_FACTOR = ENVREAL('OBS_FACTOR','Multiplier factor for observed ozone',
     &              1.0, STATUS)

         ! GET DEFINITION RECORD for OZONE
         CALL ENVSTR( 'OZONE', 'OZONE Variable definition', '', RECORD, STATUS)
         IF( STATUS .NE. 0 ) then
           SPECIES%NUMSPEC = 1
           SPECIES%MOD_NAME(1) = 'O3'
           SPECIES%MOD_FACTOR(1) = 1.00
           SPECIES%MOD_EXPRESSION = 'O3'
           Return
           Endif

         !PARSE RECORD 
         if( getParsedNumber(record, ',') .eq. 0 ) Then
           CALL M3ERR( 'GET_SPECS', 0, 0,'Syntax error on variable definition [OZONE]', .TRUE. )
           Endif

         ! parse field 1 of record to model species
         Call getParsedField( record, ',', 1, field, .false. ) 
         SPECIES%MOD_EXPRESSION = field
         Call parseModelSpecies(field, status)
         if( status .ne. 0 ) Then
           CALL M3ERR( 'GET_SPECS', 0, 0,'Syntax error on variable definition [OZONE]', .TRUE. )
           Endif 
             
         ! parse field 4 of record to get model_units
         Call getParsedField( record, ',', 2, field, .false. )  
         if( LEN_TRIM(field) .gt. 0 ) then
           SPECIES%MOD_UNITS = field
          Else
           SPECIES%MOD_UNITS = ' '
           Endif

         END SUBROUTINE GET_SPECS

C*************************************************************************
C  routine to parse model species from character record
C*************************************************************************
         SUBROUTINE parseModelSpecies(record, status)

         IMPLICIT NONE 
         
         !Extrenal functions
         INTEGER getParsedNumber

         !arguments
         CHARACTER(LEN=256) RECORD
         INTEGER            STATUS 

         ! local variables
         INTEGER            N               
         CHARACTER(LEN=16)  VARNAME        
         CHARACTER(LEN=256) FIELD 
         CHARACTER(LEN=20)  factorFld
         CHARACTER(LEN=20)  speciesFld 

         ! set status flag to zero
         status = 0

         ! if record is blank, set NUMSPEC to zero and return
         if( record.eq.' ' ) Then
           SPECIES%NUMSPEC = 0
           return
           Endif

         ! find number of model species for variable NSPECVAR
         SPECIES%NUMSPEC = getParsedNumber(record, '+-')

         ! if no model species defined, return with error status
         if( SPECIES%NUMSPEC .eq. 0 ) goto 50
         
         Do N=1,SPECIES%NUMSPEC
          Call getParsedField( record, '+-', N, field, .true. ) 

          ! determine if spec field has a factor
          if( getParsedNumber(field, '*') .gt. 1 ) then
            Call getParsedField( field, '*', 1, factorFld, .true. ) 
            Call getParsedField( field, '*', 2, speciesFld, .false. ) 
           Else
            ! check first character for sign 
            if( Index('+-',field(1:1)) .gt.0 ) then
              factorFld = field(1:1) // '1.0'
              speciesFld = field(2:)
             Else
              factorFld = '1.0'
              speciesFld = field
              Endif
            Endif

          ! speciesFld and factorFld should be defined at this point

          Call leftTrim(speciesFld)
          SPECIES%MOD_NAME(N) = TRIM(speciesFld)
          Read(factorFld,'(BN,f16.0)', err=50) SPECIES%MOD_FACTOR(N)     

          EndDo

         Return

   50    status = 1
         return

         END SUBROUTINE parseModelSpecies

      END MODULE SPECIES_DEF
