
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
! $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/combine/utils.F,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $


C***********************************************************************
C  Returns the nth field of record
C***********************************************************************
      Subroutine getFld( record, delimiter, nth, del, field )

      IMPLICIT NONE

      CHARACTER*(*) record
      CHARACTER*(*) delimiter
      CHARACTER del
      Integer nth
      CHARACTER*(*) field

      Integer nfields
      Integer i, pos1
  
      pos1 = 1
      nfields = 0
      del = delimiter(1:1)
      field = ''
      Do i=1, LEN(record)
       if( index( delimiter,record(i:i) ) .gt. 0 ) then
         nfields = nfields+1 
         if( nfields .eq. nth ) then
           if(pos1.le.i) field = record(pos1:i-1)
           call LeftTrim(field)
           call RightTrim(field)
           return
           Endif
         del = record(i:i)
         pos1 = i+1
         Endif
       Enddo

      nfields = nfields+1 

      ! check if last field
      if( nfields .eq. nth ) then
        field = record(pos1:)
        Endif

      Call LeftTrim(field)
      Call RightTrim(field)
      Return
      End
        

 
C***********************************************************************
C  Returns the number of parsed fields in record
C***********************************************************************
      INTEGER FUNCTION getFldCount(record, delimiter)
     &    result(nfields)

      IMPLICIT NONE
  
      CHARACTER*(*) record
      CHARACTER*(*) delimiter

      Integer i
      Logical isDel

      nfields = 0
      if( LEN_TRIM(record).gt.0 ) nfields = 1

      Do i=1,LEN(record)
        isDel = ( index(delimiter, record(i:i)) .gt. 0 ) 
        if( isDel ) then
          nfields = nfields+1
          cycle
          endif
        enddo

      Return
      end

C***********************************************************************
C  routine to remove leading blank spaces from Character String
C***********************************************************************
      Subroutine LeftTrim( STRING )

      IMPLICIT NONE

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


C***********************************************************************
C  routine to remove trailing white spaces from Character String
C***********************************************************************
      Subroutine RightTrim( STRING )
 
      IMPLICIT NONE
 
      CHARACTER*(*) STRING
      Integer I
 
      Do I=LEN(STRING),1,-1
        if(STRING(I:I) .lt. CHAR(32)) STRING(I:I) = CHAR(32)
        if(STRING(I:I) .gt. CHAR(32)) Exit
        EndDo

      Return
      End Subroutine RightTrim
     

C***********************************************************************
C  Routine to change character string to upper characters
C***********************************************************************
      SUBROUTINE UCASE ( STR )

      IMPLICIT NONE

      CHARACTER STR*( * )
      INTEGER I
      INTEGER K

      DO I = 1, LEN(STR)
        K = ICHAR(STR(I:I))
        IF ( ( K .GE. 97 ) .AND. ( K .LE. 122 ) )
     &    STR( I:I ) = CHAR( K - 32 )
      END DO

      RETURN
      END SUBROUTINE UCASE
