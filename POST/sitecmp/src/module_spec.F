
!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /home/sjr/cvs2git/TOOLS/src/sitecmp/module_spec.F,v 1.4 2011/10/21 14:41:32 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C*************************************************************************
C
C  MODULE:  defines a set of species variables for output
C             
C*************************************************************************
      MODULE SPECIES_DEF

      TYPE SPEC_VAR

         CHARACTER(len=512) OBS_EXPRESSION
         CHARACTER(len=16) OBS_UNITS
         INTEGER           OBS_NUMSPEC
         CHARACTER(len=36) OBS_NAME(20)
         REAL              OBS_FACTOR(20)
         INTEGER           OBS_FIELD(20)
         LOGICAL           OBS_OPTNAL(20)

         CHARACTER(len=512) MOD_EXPRESSION
         CHARACTER(len=16) MOD_UNITS
         INTEGER           MOD_NUMSPEC
         CHARACTER(len=36) MOD_NAME(20)
         REAL              MOD_FACTOR(20)

         CHARACTER(len=2)  OP_CODE

      END TYPE SPEC_VAR

      TYPE ( SPEC_VAR ) SPECVARS(300)
      INTEGER NSPECVAR

      CONTAINS

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C   ROUTINE TO GET SPECIES DEFINITIONS FROM ENVIRONMENT VARIABLES
C
C   VARIABLE FORMAT obs_expression, obs_units, mod_expression, mod_units, [output name]
C
C   expressions are in the format (f1*s1 + f2*s2 +...+f10*s10)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         SUBROUTINE GET_SPECS

         IMPLICIT NONE 
         
         !Extrenal functions
         INTEGER getParsedNumber

         ! local variables
         INTEGER             N, I, K
         INTEGER             STATUS, split 
         CHARACTER(LEN=32)   VARNAME
         CHARACTER(LEN=1024) RECORD
         CHARACTER(LEN=1024) FIELD 
         CHARACTER(LEN=2)    operation(6)
         CHARACTER(LEN=6)    varType(6)


         ! OPERATION codes
         !  AV - average
         !  AC - accumulate
         !  WD - wet deposition
         !  WC - wet concentration
         DATA OPERATION/'CH','AV','AV','WD','WC','AC'/
         DATA varType/'CHAR','AERO','GAS','WETDEP','WETCON','PREC'/

         NSPECVAR = 0
         DO k=1,SIZE(varType)
           DO N=1,300

             !  build VARNAME
             IF( N.le.9 ) THEN
               WRITE(VARNAME,'(A,''_'',I1)') TRIM(varType(k)), N
             ELSE
               WRITE(VARNAME,'(A,''_'',I2)') TRIM(varType(k)), N
             ENDIF
    
             ! GET DEFINITION RECORD
             CALL ENVSTR( VARNAME, 'Species Variable definition', 'Not Used', RECORD, STATUS)
             IF( STATUS .NE. 0 ) EXIT 
             NSPECVAR = NSPECVAR + 1

             ! truncate record at ! character
             split = index(record, '!')
             if( split.gt.0 ) then
               record = record(1:split-1)
               endif

             !PARSE RECORD 
             if( getParsedNumber(record, ',') .eq. 0 ) Then
               CALL M3ERR( 'GET_SPECS', 0, 0,'Syntax error on variable definition '//
     &                    TRIM(varname), .TRUE. )
               Endif

             ! parse field 1 of record to get obs_name
             Call getParsedField( record, ',', 1, field, .false. ) 
             SPECVARS(NSPECVAR)%OBS_EXPRESSION = TRIM(field)

             ! check for Character species
             if( varType(k).eq.'CHAR' ) then
               SPECVARS(NSPECVAR)%OBS_UNITS = ' '   
               SPECVARS(NSPECVAR)%OBS_NUMSPEC = 1
               SPECVARS(NSPECVAR)%OBS_NAME(1) = field
               SPECVARS(NSPECVAR)%OBS_FACTOR(1) = 1.0 
               SPECVARS(NSPECVAR)%OBS_OPTNAL(1) = .FALSE.
               SPECVARS(NSPECVAR)%MOD_EXPRESSION = ' '
               SPECVARS(NSPECVAR)%MOD_UNITS = ' '
               SPECVARS(NSPECVAR)%OP_CODE = OPERATION(K)
               CYCLE
               endif 

             Call parseObsSpecies(SPECVARS(NSPECVAR), status)
             if( status .ne. 0 ) Then
               CALL M3ERR( 'GET_SPECS', 0, 0,'Syntax error on variable definition '//
     &                    TRIM(varname), .TRUE. )
               Endif 
         
             ! parse field 2 of record to get obs_units
             Call getParsedField( record, ',', 2, field, .false. )  
             if( LEN_TRIM(field) .gt. 0 ) then
               SPECVARS(NSPECVAR)%OBS_UNITS = field
              Else
               SPECVARS(NSPECVAR)%OBS_UNITS = ' '
               Endif

             ! parse field 3 of record to model species
             Call getParsedField( record, ',', 3, field, .false. ) 
             SPECVARS(NSPECVAR)%MOD_EXPRESSION = field
             Call parseModSpecies(SPECVARS(NSPECVAR), status)
             if( status .ne. 0 ) Then
               CALL M3ERR( 'GET_SPECS', 0, 0,'Syntax error on variable definition '//
     &                    TRIM(varname), .TRUE. )
               Endif 
             
             ! parse field 4 of record to get model_units
             Call getParsedField( record, ',', 4, field, .false. )  
             if( LEN_TRIM(field) .gt. 0 ) then
               SPECVARS(NSPECVAR)%MOD_UNITS = field
              Else
               SPECVARS(NSPECVAR)%MOD_UNITS = ' '
               Endif

             ! parse field 5 of record to get output name
             Call getParsedField( record, ',', 5, field, .false. )  
             if( LEN_TRIM(field) .gt. 0 ) then
               SPECVARS(NSPECVAR)%MOD_EXPRESSION = field
               SPECVARS(NSPECVAR)%OBS_EXPRESSION = field
               endif

             ! set operation code
             SPECVARS(NSPECVAR)%OP_CODE = OPERATION(K)

             ENDDO
           ENDDO

         END SUBROUTINE GET_SPECS


C*************************************************************************
C  routine to parse observed species from character record
C*************************************************************************
         SUBROUTINE parseObsSpecies(VAR, status)

         IMPLICIT NONE 
         
         !Extrenal functions
         INTEGER getParsedNumber

         !arguments
         TYPE (SPEC_VAR)    VAR
         INTEGER            STATUS 

         ! local variables
         INTEGER            N               
         INTEGER            I               
         CHARACTER(LEN=36)  VARNAME        
         CHARACTER(LEN=256) FIELD 
         CHARACTER(LEN=36)  factorFld
         CHARACTER(LEN=36)  speciesFld 

         ! set status flag to zero
         status = 0

         ! if record is blank, set NUMSPEC to zero and return
         if( VAR%OBS_EXPRESSION.eq.' ' ) Then
           VAR%OBS_NUMSPEC = 0
           return
           Endif

         ! find number of model species for variable NSPECVAR
         VAR%OBS_NUMSPEC = getParsedNumber(VAR%OBS_EXPRESSION, '+-')

         ! check if maximum number of species execeeded
         if( VAR%OBS_NUMSPEC .gt. SIZE(VAR%OBS_NAME) ) Then
           Write(*,'(/''**ERROR** The maximum number of species in an expression exceeded'')')
           Write(*,'(a)') TRIM(VAR%OBS_EXPRESSION)
           Stop
           endif

         ! if no model species defined, return with error status
         if( VAR%OBS_NUMSPEC .eq. 0 ) goto 50
         
         Do N=1,VAR%OBS_NUMSPEC
          Call getParsedField( VAR%OBS_EXPRESSION, '+-', N, field, .true. ) 

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


          ! check for Optional species ( in brackets [] )
          VAR%OBS_OPTNAL(N) = .FALSE.
          Call leftTrim(speciesFld)
          if( speciesFld(1:1) .eq. '[' ) then
            VAR%OBS_OPTNAL(N) = .TRUE.
            ! remove brackets
            do i=1,LEN_TRIM( speciesFld )
              if( speciesFld(i:i).eq.'[' .or. speciesFld(i:i).eq.']' ) speciesFld(i:i) = ' '
              enddo
            endif 

          ! speciesFld and factorFld should be defined at this point
          Call leftTrim(speciesFld)
          VAR%OBS_NAME(N) = TRIM(speciesFld)
          Read(factorFld,'(BN,f16.0)', err=50) VAR%OBS_FACTOR(N)     
          EndDo

         Return

   50    status = 1
         return

         END SUBROUTINE parseObsSpecies


C*************************************************************************
C  routine to parse model species from character record
C*************************************************************************
         SUBROUTINE parseModSpecies(VAR, status)

         IMPLICIT NONE 
         
         !Extrenal functions
         INTEGER getParsedNumber

         !arguments
         TYPE (SPEC_VAR)    VAR
         INTEGER            STATUS 

         ! local variables
         INTEGER            N               
         CHARACTER(LEN=36)  VARNAME        
         CHARACTER(LEN=256) FIELD 
         CHARACTER(LEN=36)  factorFld
         CHARACTER(LEN=36)  speciesFld 

         ! set status flag to zero
         status = 0

         ! if record is blank, set NUMSPEC to zero and return
         if( VAR%MOD_EXPRESSION.eq.' ' ) Then
           VAR%MOD_NUMSPEC = 0
           return
           Endif

         ! find number of model species for variable NSPECVAR
         VAR%MOD_NUMSPEC = getParsedNumber(VAR%MOD_EXPRESSION, '+-')

         ! if no model species defined, return with error status
         if( VAR%MOD_NUMSPEC .eq. 0 ) goto 50
        
         ! check if number of species exceed max
         if( VAR%MOD_NUMSPEC .gt. SIZE( VAR%MOD_NAME ) ) Then
           Write(*,'(/''**ERROR** The maximum number of species in an expression exceeded'')')
           Write(*,'(a)') TRIM(VAR%MOD_EXPRESSION)
           Stop
           endif
 
         Do N=1,VAR%MOD_NUMSPEC
          Call getParsedField( VAR%MOD_EXPRESSION, '+-', N, field, .true. ) 

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
          VAR%MOD_NAME(N) = TRIM(speciesFld)
          Read(factorFld,'(BN,f16.0)', err=50) VAR%MOD_FACTOR(N)     

          EndDo

         Return

   50    status = 1
         return

         END SUBROUTINE parseModSpecies

      END MODULE SPECIES_DEF
