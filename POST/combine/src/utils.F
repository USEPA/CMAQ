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


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/TOOLS/src/combine/utils.F,v 1.1.1.1 2005/07/27 12:55:20 sjr Exp $


C***********************************************************************
C  Returns the nth field of record
C***********************************************************************
      Subroutine getFld( record, delimiter, nth, del, field, exception )

      IMPLICIT NONE

      CHARACTER*(*), Intent( In  ) :: record
      CHARACTER*(*), Intent( In  ) :: delimiter
      CHARACTER,     Intent( Out ) :: del
      Integer,       Intent( In  ) :: nth
      CHARACTER*(*), Intent( Out ) :: field
      CHARACTER*(*), Optional, Intent( In ) :: exception

      Integer nfields
      Integer i, j, pos1
      Integer nrec, nskip

      pos1 = 1
      nfields = 0
      del = delimiter(1:1)
      field = ''
      nrec  = LEN(record)

      If( Present( exception ) )Then
          nskip = Len_Trim( exception )
      Else
          nskip = 0
      End If

      Loop_record: Do i=1, nrec
       If( index( delimiter,record(i:i) ) .gt. 0 )Then
         If( i .gt. 1 .And. nskip .Gt. 0 )Then
           Do j = 1, nskip
             If(record(i-1:i-1) .Eq. exception(j:j))Cycle Loop_record
           End Do
         End If
         nfields = nfields+1 
         If( nfields .eq. nth )Then
           If(pos1.le.i) field = record(pos1:i-1)
           call LeftTrim(field)
           call RightTrim(field)
           return
         End If
         del = record(i:i)
         pos1 = i+1
       End If
      End Do Loop_record

      nfields = nfields+1 

      ! check If last field
      If( nfields .eq. nth ) Then
        field = record(pos1:)
      End If

      Call LeftTrim(field)
      Call RightTrim(field)
      Return
      End Subroutine getFld
        

 
C***********************************************************************
C  Returns the number of parsed fields in record
C***********************************************************************
      INTEGER FUNCTION getFldCount(record, delimiter, exception)
     &    result(nfields)

      IMPLICIT NONE
  
      CHARACTER*(*), Intent( In ) :: record
      CHARACTER*(*), Intent( In ) :: delimiter
      CHARACTER*(*), Optional, Intent( In ) :: exception

      Integer i, j
      Integer nskip, nrec
      Logical isDel

      nfields = 0

      If( Present( exception ) )Then
          nskip = Len_Trim( exception )
      Else
          nskip = 0
      End If

      nrec = LEN_TRIM(record)
      If( nrec.gt.0 ) nfields = 1

      Loop_record: Do i=1,nrec
        isDel = ( index(delimiter, record(i:i)) .gt. 0 ) 
        If( isDel ) Then
          If( i .gt. 1 .And. nskip .Gt. 0 )Then
            Do j = 1, nskip
             If( record(i-1:i-1) .Eq. exception(j:j))Then
               Cycle Loop_record 
             End If          
            End Do
          End If
          nfields = nfields+1
          cycle
        End If
      End Do Loop_record

      Return
      End FUNCTION getFldCount

C***********************************************************************
C  routine to remove leading blank spaces from Character String
C***********************************************************************
      Subroutine LeftTrim( STRING )

      IMPLICIT NONE

      CHARACTER*(*), INTENT( INOUT ) :: STRING
      Integer I

      Do I=1,LEN(STRING)
        If(STRING(I:I) .ne. CHAR(32)) Then
          STRING = STRING(I:)
          RETURN
          EndIf 
         EndDo

      Return
      End Subroutine LeftTrim


C***********************************************************************
C  routine to remove trailing white spaces from Character String
C***********************************************************************
      Subroutine RightTrim( STRING )
 
      IMPLICIT NONE
 
      CHARACTER*(*), INTENT( INOUT ) :: STRING
      Integer I
 
      Do I=LEN(STRING),1,-1
        If(STRING(I:I) .lt. CHAR(32)) STRING(I:I) = CHAR(32)
        If(STRING(I:I) .gt. CHAR(32)) Exit
        EndDo

      Return
      End Subroutine RightTrim
     

C***********************************************************************
C  Routine to change character string to upper characters
C***********************************************************************
      SUBROUTINE UCASE ( STR )

      IMPLICIT NONE

      CHARACTER, INTENT( INOUT ) :: STR*( * )
      INTEGER I
      INTEGER K

      DO I = 1, LEN(STR)
        K = ICHAR(STR(I:I))
        IF ( ( K .GE. 97 ) .AND. ( K .LE. 122 ) )
     &    STR( I:I ) = CHAR( K - 32 )
      END DO

      RETURN
      END SUBROUTINE UCASE

C****************************************************************************
C  routine to replace characters within []
C****************************************************************************
      Subroutine replace( string, old, new )

      Implicit none

      ! arguments
      Character*(*), Intent( InOut ) :: string
      Character*(1), Intent( In    ) :: old    
      Character*(1), Intent( In    ) :: new    

      ! local variables
      Integer last, i
      Logical infield

      ! If no bracket marks, return
      If( index(string, '[').le.0 ) return

      call LeftTrim(string)
      last = LEN_TRIM(string)

      ! check for blank string
      If( last.le.0 ) return

      infield = .false.

      Do i=1,last
        If( string(i:i).eq.'[' ) infield = .true.
        If( string(i:i).eq.']' ) infield = .false.

        If( infield .and. string(i:i).eq.old) string(i:i) = new

        End Do

      Return
      End Subroutine replace 
      SUBROUTINE Remove_WhiteSpaces (text)
         Implicit None

         CHARACTER*(*), Intent( InOut ) :: text
         CHARACTER(Len(text))        :: outs     ! provide outs with extra 100 char len
         INTEGER                     :: i, nt

         nt   = LEN_TRIM(text)
         If( text(1:1) .Eq. ' ' )Then
             outs = text(2:nt)
             nt = nt-1
         Else
             outs = text(1:nt)
         End If
         i = 2
         DO 
            nt = LEN_TRIM(outs)
            IF (i .Eq. nt ) EXIT
            IF( outs(i:i) .Eq. ' ' )THEN
               outs = outs(1:i-1) // outs(i+1:nt)
            ELSE
               i = i + 1
            END IF
         END DO
         text = outs
      END SUBROUTINE Remove_WhiteSpaces
