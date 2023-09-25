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

C*************************************************************************
C
C  MODULE:  defines a set of species variables for daily output
C
C*************************************************************************
      MODULE SPECIES_DEF

      TYPE SPECIES
         CHARACTER(len=16) NAME
         CHARACTER(len=512) EXPRESSION
         CHARACTER(len=16) UNITS
         CHARACTER(len=16) DESCRIPTION
         CHARACTER(len=10)  OPERATION
      END TYPE SPECIES 
 
      TYPE ( SPECIES ), Allocatable :: SPECVARS(:)
      INTEGER NSPECVAR
 
      CONTAINS
 
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C   ROUTINE TO GET SPECIES DEFINITIONS FROM ENVIRONMENT VARIABLES
C
C   VARIABLE FORMAT name, expression, operation
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         SUBROUTINE LoadSpecies

         USE M3UTILIO
 
         IMPLICIT NONE

         !Extrenal functions
         Integer getNumberOfFields
 
         ! local variables
         Integer k, i
         Integer status
         Integer nflds, idx
         CHARACTER(LEN=16)  VARNAME
         CHARACTER(LEN=512) RECORD
         CHARACTER(LEN=5)   FIELD
         CHARACTER(LEN=10)   opcode(14)
         CHARACTER(LEN=16)  opname(14)

         data opcode/'SUM', 'AVG', 'MIN', 'MAX', 'HR@MIN', 'HR@MAX', '@MAXT',
     &               'MAXDIF', '8HRMAX', 'W126', '@8HRMAXO3', 'HR@8HRMAX',
     &               'SUM06', 'TZ'/ 
         data opname/'Daily-total', 'Daily-average', 'Min-hour', 'Max-hour',
     &               'Hour of Minimum', 'Hour of Maximum',
     &               'Value@Max-Temp', 'Max-hour-change',
     &               'Max-8-hour', 'W126', 'Value@8HrMax', 'Hour@8HrMax',
     &               'Sum-06', 'TIMEZONE'/ 

         !  allocate memory for SPECVARS array
         Allocate( SPECVARS( MXVARS3 ) )

         NSPECVAR = 0
         do k=1,MXVARS3
 
           !  build VARNAME
           write(field,'(i5)') k
           Call leftTrim(field)
           varname = 'SPECIES_' // TRIM(field)
 
           ! GET DEFINITION RECORD
           CALL ENVSTR( varname, 'Species Variable definition', 'Not Used', record, status)
           IF( status .ne. 0 ) EXIT
           NSPECVAR = NSPECVAR + 1

           nflds = getNumberOfFields( record, ',' )

           if( nflds.ne.4 ) then
             write(*,'(//,''**ERROR** Wrong number of fields in '',a,'' = '',a)') TRIM(varname),TRIM(record)
             write(*,'(''4 fields required for species definition'')')
             write(*,'(''Required fields are: "name, units, from_species, operation"'')')
             stop
             endif

           Call getField( record, ',', 1, SPECVARS(k)%NAME ) 
           Call getField( record, ',', 2, SPECVARS(k)%UNITS ) 
           Call getField( record, ',', 3, SPECVARS(k)%EXPRESSION )
           Call getField( record, ',', 4, SPECVARS(k)%OPERATION )
           Call UCASE( SPECVARS(k)%OPERATION ) 

           ! check for duplicate name
           do i=1,NSPECVAR-1
             if( SPECVARS(k)%NAME .eq. SPECVARS(i)%NAME ) then
               Write(*,'(''**ERROR** Duplicate Species name:'',a)') TRIM(SPECVARS(k)%NAME)
               stop
               endif
             enddo

           ! verify operation
           idx = INDEX1(SPECVARS(k)%OPERATION, SIZE(opcode), opcode)
           if( idx.gt.0 ) then
             SPECVARS(k)%DESCRIPTION = opname(idx)
            else
             Write(*,'(''INVALID Operation code:'',a)') TRIM(SPECVARS(k)%OPERATION) 
             stop 
             endif

           enddo
 
         end Subroutine LoadSpecies
 
      END MODULE SPECIES_DEF

