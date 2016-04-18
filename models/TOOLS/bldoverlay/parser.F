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

C****************************************************************************
C  routine to return the number of parsed fields in Character String
C****************************************************************************                    
      INTEGER FUNCTION getParsedNumber( record, delimiter ) result( nfields )

      CHARACTER*(*) record
      CHARACTER*(*) delimiter

      Integer i

      nfields = 1
      Do i=2, LEN(record)
       if( index(delimiter,record(i:i)) .gt. 0 ) nfields = nfields+1
      EndDo

      Return
      End

C****************************************************************************
C  routine to return the nth field from Character String
C****************************************************************************          
      Subroutine getParsedField( record, delimiter, nth, field, includeDel )

      CHARACTER*(*) record
      CHARACTER*(*) delimiter
      Integer nth
      CHARACTER*(*) field
      Logical includeDel 

      Integer nfields
      Integer i, pos1
  
      pos1 = 1
      nfields = 0
      field = ''
      Do i=2, LEN(record)
       if( index(delimiter,record(i:i)) .gt. 0 ) then
         nfields = nfields+1 
         if( nfields .eq. nth ) then
           field = record(pos1:i-1)
           return
           Endif

         ! define starting point of next field
         pos1 = i+1
         if( includeDel ) pos1 = i
         
         Endif
       Enddo

      nfields = nfields+1 

      ! check if last field
      if( nfields .eq. nth ) then
        field = record(pos1:)
        Endif

      Return
      End

C****************************************************************************
C  routine to remove leading blank spaces from Character String
C****************************************************************************
      Subroutine LeftTrim( STRING )

      CHARACTER*(*) STRING
      Integer I

      Do I=1,LEN(STRING)
        if(STRING(I:I) .ne. CHAR(32)) Then
          STRING = STRING(I:)
          RETURN
          EndIf 
         EndDo

      Return
      End Subroutine LeftTrim

C****************************************************************************         
C  routine to change character string to upper case
C****************************************************************************         
      Subroutine UPPER( STRING )                                                   
                                                                                      
      CHARACTER*(*) STRING                                                            
      Integer I, ic                                                                       

      Do I=1,LEN(STRING)
        ic = ichar(STRING(I:I))
        if(ic.ge.97 .and. ic.le.122) STRING(I:I) = CHAR(ic-32)
        EndDo

      Return
      End Subroutine Upper
