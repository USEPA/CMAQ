!-----------------------------------------------------------------------!
!  The Community Multisc.Le.Air Quality (CMAQ) system software is in    !
!  continuous development by various groups.And.is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract,.And.non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of,.And.distribute copies!
!  of their work in the CMAQ system to the public.And.to permit others  !
!  to Do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are .Eq.ested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!

!-------------------------------------------------------------------------------
!     routines for parsing a delimited text record
!     originally written in C by Steve Thorpe
!     rewritten in Fortran by Steve Howard
!     redone to meet CMAQ coding standards by Jeff Young
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!     Return the number of fields in record
!-------------------------------------------------------------------------------
      Function getNumberOfFields( record, delimiter ) result( nfields )

      Implicit None

      Character(*) :: record
      Character    :: delimiter
      Integer      :: nfields

      Integer getFieldCount
      Integer i

! if delimiter is space, use getFieldCount function
      If ( delimiter .Eq. ' ' ) Then
        nfields = getFieldCount( record, delimiter )
        Return
      End If

      nfields = 1
      Do i = 1, Len( record )
        If ( record( i:i ) .Eq. delimiter ) nfields = nfields+1
      End Do

      Return
      End Function getNumberOfFields

!-------------------------------------------------------------------------------
!  Return the number of parsed fields in record
!  this method considers duplicate delimiters as one
!-------------------------------------------------------------------------------
      Function getFieldCount( record, delimiter ) result( nfields )

      Implicit None

      Character(*) :: record
      Character    :: delimiter
      Integer      :: nfields

      Integer i
      Logical infield
      Logical isDel

      nfields = 0
      infield = .False.
      Do i = 1, Len( record )
        isDel = ( record( i:i ) .Eq. delimiter )

! check for start of field
        If ( .Not. infield .And. .Not. isDel ) Then
           nfields = nfields+1
           infield = .True.
           Cycle
        End If

! check for End of field
        If ( infield .And. isDel ) Then
          infield = .False.
          Cycle
        End If
      End Do 
       
      Return
      End Function getFieldCount

!-------------------------------------------------------------------------------
!  Return the number of parsed fields in record
!-------------------------------------------------------------------------------
      Function getParsedNumber( record, delimiter ) result( nfields )

      Implicit None

      Character(*) :: record
      Character    :: delimiter
      Integer      :: nfields

      Integer i

      nfields = 1
      Do i = 1, Len( record )
        If ( Index( delimiter, record( i:i ) ) .Gt. 0 ) nfields = nfields+1
      End Do

      Return
      End Function getParsedNumber

!-------------------------------------------------------------------------------
!  Return the nth field of record
!-------------------------------------------------------------------------------
      Subroutine getField( record, delimiter, nth, field )

      Implicit None

      Character(*) :: record
      Character    :: delimiter
      Integer      :: nth
      Character(*) :: field

      Integer nfields
      Integer i, pos1

! if delimiter is space, use method 2
      If ( delimiter .Eq. ' ' ) Then
        Call getField2( record, delimiter, nth, field )
        Call RightTrim( field )
        Return
      End If
  
      pos1 = 1
      nfields = 0
      field = ' '
      Do i = 1, Len( record )
        If ( record( i:i ) .Eq. delimiter ) Then
          nfields = nfields+1 
          If ( nfields .Eq. nth ) Then
            If ( pos1 .Lt. i ) field = record( pos1:i-1 )
            Call RightTrim( field )
            Return
          End if
          pos1 = i+1
        End If
      End Do

      nfields = nfields+1 

! check if last field
      If ( nfields .Eq. nth ) Then
        field = record( pos1: )
      End if

      Call RightTrim( field )
      Return
      End Subroutine getField
        
!-------------------------------------------------------------------------------
!  Return the nth field in record (method 2)
!  this method considers duplicate delimiters as one
!-------------------------------------------------------------------------------
      Subroutine getField2( record, delimiter, nth, field )

      Implicit None
 
      Character(*) :: record
      Character    :: delimiter
      Integer      :: nth
      Character(*) :: field
 
      Integer nfields
      Integer i, pos1
      Logical infield
      Logical isDel
 
      nfields = 0
      field = ' '
      infield = .False.
      Do i = 1, Len( record )
        isDel = ( record( i:i ) .Eq. delimiter )
 
! check for start of field
        If ( .Not. infield .And. .Not. isDel ) Then   
           nfields = nfields+1
           pos1 = i
           infield = .True.
        End If
 
! check for End of field
        If ( infield .And. isDel ) Then
          infield = .False.
        End If
 
! if End of nth field, Return
        If ( nfields .Eq. nth .And. .Not. infield ) Then
           If ( pos1 .Lt. i ) field = record( pos1:i-1 )
          Return
        End If
      End Do
 
! check for last field
      If ( nfields .Eq. nth ) field = record( pos1: )
 
      Return
      End Subroutine getField2

!-------------------------------------------------------------------------------
!  Return the nth field of record
!-------------------------------------------------------------------------------
      Subroutine getParsedField( record, delimiter, nth, field, includeDel )

      Implicit None

      Character(*) :: record
      Character    :: delimiter
      Integer      :: nth
      Character(*) :: field
      Logical includeDel 

      Integer nfields
      Integer i, pos1
  
      pos1 = 1
      nfields = 0
      field = ' '
      Do i = 1, Len( record )
        If ( Index( delimiter, record( i:i ) ) .Gt. 0 ) Then
          nfields = nfields+1 
          If ( nfields .Eq. nth ) Then
            If ( pos1 .Lt. i ) field = record( pos1:i-1 )
            Return
          End If

! define starting point of next field
          pos1 = i+1
          If ( includeDel ) pos1 = i
        End If
      End Do

      nfields = nfields+1 

! check if last field
      If ( nfields .Eq. nth ) Then
        field = record( pos1: )
      End If

      Return
      End Subroutine getParsedField
    
!-------------------------------------------------------------------------------
!     routine to remove leading blank spaces from Character String
!-------------------------------------------------------------------------------
      Subroutine LeftTrim( string )

      Implicit None

      Character*(*) :: string

      Integer i

      Do i = 1, Len(string)
      ! If ( string( i:i ) .Ne. Char( 32 ) ) Then
        If ( string( i:i ) .Ne. Char( 32 )  .And.   ! space
     &       string( i:i ) .Ne. Char( 9 )  ) Then   ! tab
          string = string( i: )
          Return
        End If 
      End Do

      Return
      End Subroutine LeftTrim

!-------------------------------------------------------------------------------
!     routine to remove trailing white spaces from Character String
!-------------------------------------------------------------------------------
      Subroutine RightTrim( string )

      Implicit None
 
      Character(*) :: string

      Integer i
 
      Do i = Len(string), 1, -1
         If ( string( i:i ) .Lt. Char( 32 ) ) string( i:i ) = Char( 32 )
         If ( string( i:i ) .Gt. Char( 32 ) ) Exit
      End Do

      Return
      End Subroutine RightTrim

!-------------------------------------------------------------------------------
!     routine to remove quotation marks from Character field
!-------------------------------------------------------------------------------
      Subroutine rmQuots( string )
 
      Implicit None
      
      Character(*) :: string
 
      Integer last, i
                    
      Call LeftTrim( string )
      last = Len_Trim( string )

! check for blank string
      If ( last .Le. 0 ) Return
 
! if no quote marks, Return
      If ( string( 1:1 ) .Ne. '"' .And. string( last:last ) .Ne. '"' ) Return
 
! replace last quote mark
      string( last:last ) = ' '
            
      Do i = 1, last-1
        string( i:i ) = string( i+1:i+1 )
      End Do      
                    
      Return                                                                           
      End Subroutine rmQuots 

!-------------------------------------------------------------------------------
!     routine to replace inchar with outchar between quot marks
!-------------------------------------------------------------------------------
      Subroutine replace( string, inchar, outchar, inQuots )

      Implicit none

      Character(*) :: string
      Character    :: inchar
      Character    :: outchar
      Logical      :: inQuots

      Integer last, i
      Logical infield

! if no quot marks, Return
      If ( inQuots .And. Index( string, '"' ) .Le. 0 ) Return

      Call LeftTrim( string )
      last = Len_Trim( string )

! check for blank string
      If ( last .Le. 0 ) Return

      infield = .False.

      Do i = 1, last
         If ( string( i:i ) .Eq. '"' ) infield = .Not. infield
         If ( .Not. inQuots ) infield = .True.      ! set to always true

         If ( infield .And. string( i:i ) .Eq. inchar ) string( i:i ) = outchar

      End Do

      Return
      End Subroutine replace 

!-------------------------------------------------------------------------------
!     Routine to change character string to upper characters
!-------------------------------------------------------------------------------
      Subroutine ucase ( str )

      Implicit None

      Character( * ) :: str

      Integer i
      Integer k

      Do i = 1, Len( str )
        k = Ichar( str( i:i ) )
        if ( ( k .Ge. 97 ) .And. ( k .Le. 122 ) ) str( i:i ) = Char( k - 32 )
      End Do

      Return
      End Subroutine ucase
