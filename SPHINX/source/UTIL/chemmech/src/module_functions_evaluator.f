!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to Do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/TOOLS/src/combine/module_evaluator.F,v 1.1.1.1 2005/07/27 12:55:20 sjr Exp $

C***********************************************************************
C
C  MODULE:  evaluates species expressions
C             
C***********************************************************************
      MODULE evaluator

      Public :: evaluate

      Private

      Integer, Parameter, Private   :: EXP_LEN     = 1024         
      Integer, Parameter, Private   :: isize       = 1
      Integer, Parameter, Private   :: Numb_Native = 14

      Character(5), Parameter, Private :: Native_Func( Numb_Native ) =
     &             (/ 'ABS  ', 'EXP  ', 'LOG10', 'LOG  ', 'SQRT ',
     &                'SINH ', 'COSH ', 'TANH ', 'SIN  ', 'COS  ',
     &                'TAN  ', 'ASIN ', 'ACOS ', 'ATAN ' /)
      
      Integer, Private :: NumVars

      Character(16), Allocatable, Private :: VarName( : )
      Character(EXP_LEN),         Private :: formula

      Real(8), Allocatable,  Private :: InputValue( : )
      Real(8), Allocatable , Private :: parseBuffer(:,:)
      
 
      Integer, Private :: idate   = 0
      Integer, Private :: itime   = 0
      Integer, Private :: ilayer  = 1

      Logical       :: eflag

      CONTAINS
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
!               print*,'Exception: ',record(i-1:i),' has ',exception(j:j)
               Cycle Loop_record 
             End If          
            End Do
          End If
          nfields = nfields+1
!         print*,'Pass: ',record(i-1:i),' No ',Trim(exception)
          cycle
        End If
      End Do Loop_record

!      If( Present( exception ) )print*,TRim(record),': has nfields = ',nfields

      Return
      End FUNCTION getFldCount


C  subroutine to evaluate species expression at date
C  returns buffer array values
      Subroutine evaluate(expression,jsize,variable,value,buffer)

      IMPLICIT NONE

      ! arguments
      Character*(*), Intent( In    ) :: expression
      Integer,       Intent( In    ) :: jsize
      Character*(*), Intent( In    ) :: Variable( jsize )
      Real(8),       Intent( In    ) :: Value(jsize)
      Real(8),       Intent( InOut ) :: buffer(isize)

      ! local variables
      Character(EXP_LEN) expresscp
      Character(EXP_LEN) express
      Character(16)      VarCheck     
      Integer nparen
      Integer depth, maxdepth
      Integer i, n, m, pos1, pos2
      Character*(5) nstring

       Logical, Save :: FirstCall = .True. 

      If( FirstCall )Then
          NumVars = jsize
          Allocate( VarName( jsize ) )
          Allocate( InputValue( jsize ) )
          FirstCall = .False.
      End If
      Varname = ''
      eflag = .False.
      Do i = 1, jsize
         pos1 = Len_Trim(Variable(i))
         VarName(i)(1:pos1) = Variable(i)(1:pos1)
         VarCheck           = VarName(i)
! check Varname equals an intrinsic function
         Call UCASE( VarCheck )
         Do n = 1, Numb_Native         
            If( Trim( VarCheck ) .Eq. Trim( Native_Func( n ) ) )Then
                If( .Not. eflag )Then
                   Write(6,'(a)')'Error in Formula: ' 
     &                         // Trim( expression )
                   Write(6,'(5x,a)')'Uses the below intrinsic function(s) '
     &                          //  'as a variable.'
                   eflag = .True.
                End If
                Write(6,'(10x,a)')Trim( Native_Func( n ) )                              
            End If
         End Do
         InputValue(i)      = Value(i)
      End Do
      If( eflag )Stop

      ! make copies of expression 
      formula   = ''
      pos1      = Len_Trim( expression )
      formula(1:pos1) = expression(1:pos1)


      ! defined expresscp after replace fortran power notation with evaluator notation
      expresscp = Replace_Text(expression,'**+','^')
      expresscp = Replace_Text(expression,'**-','^-')
      expresscp = Replace_Text(expression,'**','^')
      expresscp = Remove_WhiteSpaces(expresscp)
!      Call InsertBrackets(expresscp)
!      print*,TRIm(expresscp)

      ! check for scientIfic notation (E+,E-,e+,e-,D+,D-,d+,d-) and replace with 10.0^ or 10.0D0^
      call rmSciNot( expresscp )

      ! replace '+' characters inside [] brackets with '!' characters
      Call replace( expresscp, '+', '!' )

      ! replace '-' characters inside [] brackets with '#' characters
      Call replace( expresscp, '-', '#' )

      ! find number of parentheses and depth
      nparen = 0 
      depth = 0
      maxdepth = 0
      Do i=1,len_trim(expresscp)
        If( expresscp(i:i).eq.'(' ) Then
          nparen = nparen + 1
          depth = depth + 1
        End If

        If( expresscp(i:i).eq.')' ) Then
          depth = depth - 1
        End If
        
        If( depth.gt.maxdepth ) maxdepth = depth
      End Do

      !  check for unbalanced parentheses
      If( depth.ne.0 )Then
        write(*,'(/'' unbalanced parentheses in expression''/a)') trim(expresscp)
        stop
      End If

      ! allocate memory for parseBuffer If needed
      If( nparen.gt.0 ) Then
        If( Allocated(parseBuffer) .and. SIZE(parseBuffer,DIM=2).lt.nparen ) Then
          deAllocate(parseBuffer)
        End If

        If( .NOT.Allocated(parseBuffer) ) Then
           Allocate( parseBuffer(isize,nparen) )
        End If

        parseBuffer = 0.0D0
      End If

      ! find depth of parentheses
      depth = maxDepth 
      Do n=1,nparen

        ! build buffer number as string
        write(nstring, '(i5)') n
        Call leftTrim(nstring)

        ! try to find parentheses at depth
        Call findDepth( expresscp, depth, pos1, pos2 )
            
        If( pos1.eq.0 ) Then
          depth = depth - 1
          Call findDepth( expresscp, depth, pos1, pos2 )
      End If

        ! If parentheses found, evaluate sub expression
        If( pos1.gt.0 ) Then

          ! extract expression within parentheses and
          ! evaluate to parsebuffer(1:isize,n)
          express = expresscp(pos1+1:pos2-1)
          call eval1(express, parsebuffer(1:isize,n) )

          ! replace expression within parentheses with "buffer[n]"
          express = ''
          If( pos1.gt.1 ) express = expresscp(1:pos1-1)
          express = TRIM(express) // 'buffer[' // TRIM(nstring) //
     &              ']' // TRIM(expresscp(pos2+1:))
          expresscp = express 
      End If 
      End Do

      call eval1(expresscp, buffer)
      If( eflag )Write(6,'(a / a)')'Failed to evaluate formula: ',
     &           Trim(formula)

      End Subroutine evaluate
      FUNCTION Replace_Text (s,text,rep)  RESULT(outs)
         Implicit None

         CHARACTER(*), Intent( In ) :: s,text,rep
         CHARACTER(EXP_LEN)         :: outs     ! provide outs with extra 100 char len
         INTEGER                    :: i, nt, nr

         outs = s
         nt = LEN_TRIM(text)
         nr = LEN_TRIM(rep)
         DO
            i = INDEX(outs,text(:nt)) 
            IF (i .Eq. 0) EXIT
            outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
         END DO
      END FUNCTION Replace_Text
      Subroutine InsertBrackets(expression)
C insert characters, [0], after each VarName occurance in expression
        Implicit None

        Character*(*), Intent( InOut ) :: expression

        Integer            n, i, j, k, pos, pos1, pos2
        Character(EXP_LEN) expresscp
        Character(20)      text, string

        expresscp = Trim(expression) // ';'

        k = Len_Trim( expression )
        Do i = 1, NumVars
           j    = Len_Trim( VarName( i ) ) 
           pos  = 1
           pos1 = 1
           WRITE(string,'(i7)')i
           Call LeftTrim( string )
           expresscp = Replace_Text(expresscp,Varname(i),'{sashikomu}')
           text      = 'Variable[' // Trim( string ) // ']'
           expresscp = Replace_Text(expresscp,'{sashikomu}',text)
        End Do
        pos1 = Len_Trim( expresscp )-1
        expression = expresscp(1:pos1)

      End Subroutine InsertBrackets
C  subroutine to replace scientIfic notation strings
      Subroutine rmSciNot(expression)


      IMPLICIT NONE

      Character*(*), Intent( InOut ) :: expression

      Character*(2) estring(8)
      Character*(7) pstring(8)

      Integer n, i, pos, pos1, pos2

      Data estring/'E+','e+','E-','e-',
     &             'D+','d+','D-','d-' /
      Data pstring/'*(10.0^', '*(10.0^', '/(10.0^', '/(10.0^',
     &             '*(10.0^', '*(10.0^', '/(10.0^', '/(10.0^'/

      Do n=1,8
        Do while( index(expression, estring(n)) .gt. 0 )
          pos = index(expression, estring(n))

          ! search for start of number starting at pos-1 and working back
          pos1 = pos-1
          Do i=pos-1,1,-1
            If( index('0123456789.',expression(i:i)) .eq. 0 ) Then
              EXIT
            End If
            pos1 = i
            End Do

          ! search for end of number starting at pos+2
          Do i=pos+2,pos+12
            If( index('0123456789.',expression(i:i)) .eq. 0 ) Then
              pos2=i
              EXIT
            End If
            End Do

          If( pos1 .eq. 1 ) Then
            expression = '(' // expression(1:pos-1) // pstring(n) // expression(pos+2:pos2-1)
     &                // '))' // expression(pos2:)
          End If

          If( pos1 .gt. 1 ) Then
            expression = expression(1:pos1-1) // '(' // expression(pos1:pos-1) //
     &                 pstring(n) // expression(pos+2:pos2-1) // '))' // expression(pos2:)
          End If

          End Do
        End Do

      return
      end Subroutine rmSciNot



C  subroutine to find location of parentheses depth
      Subroutine findDepth(expression, depth, pos1, pos2)

      IMPLICIT NONE

      Character*(*), Intent( In )    ::  expression
      Integer,       Intent( InOut ) ::  depth, pos1, pos2

      Integer i, dep

      pos1 = 0
      pos2 = 0
      dep = 0

      ! try to find parentheses at depth
      Do i = 1, len_trim(expression)  
        If( expression(i:i).eq.'(' ) Then
            dep = dep+1
            If(dep.eq.depth) pos1 = i
          End If

          If( expression(i:i).eq.')' ) Then
            If(dep.eq.depth) Then
              pos2 = i
              return
            End If
            dep = dep-1
          End If           
      End Do

      return
      end Subroutine findDepth


C  subroutine to return buffer array value
      Subroutine getBuffer(field, buffer)
      IMPLICIT NONE

      Character*(*), Intent( InOut ) :: field
      Real(8),       Intent( InOut ) :: buffer(isize)


      Integer pos1, pos2, nbuf, status
      Character*(10) string
      Character*(10) func
      Logical KSWIT
      Logical SHUT3

      Call leftTrim(field)

      ! parse field to find buffer number
      pos1 = index(field, '[') 
      pos2 = index(field, ']',.true.) 

      If(pos1.le.0 .or. pos1.ge.pos2) Then
        write(*,'(/''**ERROR**  Invalid syntax in field: '',a)') trim(field)
         stop
      End If

      If(field(pos2+1:) .ne. ' ') Then
        write(*,'(/''**ERROR**  Invalid syntax in field: '',a)') trim(field)
        stop
      End If

      string = field(pos1+1:pos2-1)
      read(string,'(i10)',iostat=status) nbuf      
      If(status .ne. 0) Then
        write(*,'(/''**ERROR**  Invalid syntax in field: '',a)') trim(field)
        stop
      End If

      buffer = parsebuffer(1:isize,nbuf)      

      ! check for function
      pos1 = index(field, 'buffer[') 
      Call UCASE(field)

      If( pos1.gt.1 ) Then       
        func = field(1:pos1-1)

        If( func.eq.'LOG' ) Then
          buffer = LOG(buffer)
          return
        End If
        If( func.eq.'LOG10' ) Then
          buffer = LOG10(buffer)
          return
        End If
        If( func.eq.'EXP' ) Then
          buffer = EXP(buffer)
          return
        End If
        If( func.eq.'SIN' ) Then
          buffer = SIN(buffer)
          return
        End If
        If( func.eq.'COS' ) Then
          buffer = COS(buffer)
          return
        End If
        If( func.eq.'TAN' ) Then
          buffer = TAN(buffer)
          return
        End If
        If( func.eq.'ASIN' ) Then
          buffer = ASIN(buffer)
          return
        End If
        If( func.eq.'ACOS' ) Then
          buffer = ACOS(buffer)
          return
        End If
        If( func.eq.'ATAN' ) Then
          buffer = ATAN(buffer)
          return
        End If
        If( func.eq.'SINH' ) Then
          buffer = SINH(buffer)
          return
        End If
        If( func.eq.'COSH' ) Then
          buffer = COSH(buffer)
          return
        End If
        If( func.eq.'TANH' ) Then
          buffer = TANH(buffer)
          return
        End If
        If( func.eq.'SQRT' ) Then
          buffer = SQRT(buffer)
          return
        End If
        If( func.eq.'ABS' ) Then
          buffer = ABS(buffer)
          return
        End If

        write(*,'(/''**ERROR** Cannot evaluate function: '',a)') trim(func)
        eflag = .True.
      End If

      return
      end Subroutine getBuffer


C  subroutine to evaluate species expression (parses conditional statment If needed)
C   X = (y[1]>10) ? 10 : y[1]
C
      Subroutine eval1(expression, buffer)

      IMPLICIT NONE

      ! arguments
      Character*(*), Intent( In    ) :: expression
      Real(8),       Intent( InOut ) :: buffer(isize)

      ! functions
!      Integer getFldCount
 
      ! local variables
      Logical, Allocatable :: flags(:)
      Real(8), Allocatable :: value1(:)
      Real(8), Allocatable :: value2(:)
      Character*(EXP_LEN) field
      Character operator
      Integer nmajor
      Integer i
      Logical badopr


      ! parse major fields (?:)
      nmajor = getFldCount(expression, '?:')

      ! If conditional 
      If( nmajor.eq.3 ) Then 
        Allocate( flags(isize), value1(isize), value2(isize) )
        badopr = .false.

        call getFld( expression, '?:', 1, operator, field ) 
        If(operator.ne.'?') badopr = .true.
        call eval1b( field, flags)

        call getFld( expression, '?:', 2, operator, field ) 
        If(operator.ne.'?') badopr = .true.
        call eval2( field, value1)
        If( eflag )Return

        call getFld( expression, '?:', 3, operator, field ) 
        If(operator.ne.':') badopr = .true.
        call eval2( field, value2)
        If( eflag )Return

        If( badopr ) Then
          Write(*,'(/''**Error** Syntax error encountered in conditional expression: '',a)') trim(expression)
          stop
        End If

        ! set buffer values 
        Do i=1,isize
          If( flags(i) ) Then
            buffer(i) = value1(i)
          else
            buffer(i) = value2(i)
        End If 
        End Do 

        Deallocate (flags, value1, value2)
        return
      End If

      ! If no conditional
      If( nmajor.eq.1 )Then
!        print*,trim(expression),' ',trim(formula)
        call eval2( trim(expression), buffer )
        return
      End If

      ! syntax error
      Write(*,'(/''**Error** Syntax error encountered at: '',a)') trim(expression)
      stop   
      End Subroutine eval1


C  subroutine to evaluate condition expression (called from eval1) 
      Subroutine eval1b(expression, flags)

      IMPLICIT NONE

      ! arguments
      Character*(*), Intent( In    ) :: expression
      Logical,       Intent( InOut ) :: flags(isize)

 
      ! local variables
      Real(8), Allocatable :: value1(:)
      Real(8), Allocatable :: value2(:)
      Character*(EXP_LEN) field
      Character operator
      Integer nflds
      Integer i


      ! verIfy that expression contains a parse major fields (<=>)
      nflds = getFldCount(expression, '<=>')
      If( nflds.eq.0 ) Then
        Write(*,'(/''**Error** Syntax error encountered in conditional: '',a)') trim(expression)
        stop
      End If

      ! parse conditional expression
      Allocate( value1(isize), value2(isize) )

      ! determine conditional operator is <=
      If( index(expression,'<=').gt.0 ) Then
        call getFld( expression, '<=', 1, operator, field ) 
        call eval2( field, value1)
        If( eflag )Return
        call getFld( expression, '<=', 3, operator, field ) 
        call eval2( field, value2)
        If( eflag )Return
        flags = ( value1 .le. value2 )
        Deallocate (value1, value2)
        return
      End If

      ! determine conditional operator is >=
      If( index(expression,'>=').gt.0 ) Then
        call getFld( expression, '>=', 1, operator, field ) 
        call eval2( field, value1)
        If( eflag )Return
        call getFld( expression, '>=', 3, operator, field ) 
        call eval2( field, value2)
        If( eflag )Return
        flags = ( value1 .ge. value2 )
        Deallocate (value1, value2)
        return
      End If 

      ! determine conditional operator is >
      If( index(expression,'>').gt.0 ) Then
        call getFld( expression, '>', 1, operator, field ) 
        call eval2( field, value1)
        If( eflag )Return
        call getFld( expression, '>', 2, operator, field ) 
        call eval2( field, value2)
        If( eflag )Return
        flags = ( value1 .gt. value2 )
        Deallocate (value1, value2)
        return
      End If 

      ! determine conditional operator is <
      If( index(expression,'<').gt.0 ) Then
        call getFld( expression, '<', 1, operator, field ) 
        call eval2( field, value1)
        If( eflag )Return
        call getFld( expression, '<', 2, operator, field ) 
        call eval2( field, value2)
        If( eflag )Return
        flags = ( value1 .lt. value2 )
        Deallocate (value1, value2)
        return
      End If 

      ! determine conditional operator is =
      If( index(expression,'=').gt.0 ) Then
        call getFld( expression, '=', 1, operator, field ) 
        call eval2( field, value1)
        If( eflag )Return
        call getFld( expression, '=', 2, operator, field ) 
        call eval2( field, value2)
        If( eflag )Return
        flags = ( value1 .eq. value2 )
        Deallocate (value1, value2)
        return
      End If 

      ! syntax error
      Write(*,'(/''**Error** Syntax error encountered: '',a)') trim(expression)
      stop
    
      end Subroutine eval1b



C  subroutine to evaluate species expression (parses major fields (+-))
      Subroutine eval2(expression, buffer)

      IMPLICIT NONE

      ! arguments
      Character*(*), Intent( In )    :: expression
      Real(8),       Intent( InOut ) :: buffer(isize)

      ! local variables
      Real(8), Allocatable :: value(:)
      Character*(EXP_LEN)  :: field
      Character operator
      Integer nmajor
      Integer n

      buffer = 0.0D0
      Allocate ( value(isize) )

      ! parse major fields (+-)
      nmajor = getFldCount(expression, '+-', '*/^')

      ! loop thru and parse each major field and evaluate
      Do n=1,nmajor

        call getFld( expression, '+-', n, operator, field, '*/^' ) 
    
        If( field.eq.' ' ) Then
          value = 0.0D0
        else
!          print*,Trim(field)
          call eval3b( field, value)
          If( eflag )Return
        End If

        If( operator.eq.'+' ) Then
          buffer = buffer + value
        else
          buffer = buffer - value
        End If

        End Do

      Deallocate (value)
      return
      end Subroutine eval2


C  routine to compute a field of the expression (parses minor fields (*/^))
      Subroutine eval3(expression, value)
      
      IMPLICIT NONE

      ! arguments
      
      CHARACTER*(*), Intent( In    ) :: expression
      Real(8),       Intent( InOut ) :: value(isize)

      Logical SHUT3

      ! local variables
      Real(8), allocatable :: specValue(:)
      Character*(EXP_LEN) field
      Character      operator   
      Integer n, m, nflds, status
      Integer pos1, pos2, fnum
      Character*(16) funcName
      Character*(16) specName
      real(8) constant
      Logical KSWIT

      Allocate ( specValue(isize) )
      nflds = getFldCount(trim(expression), '*/^')
      value = 1.0
         
      Do n=1,nflds
        call getFld( trim(expression), '*/^', n, operator, field ) 

        print*,Trim(field)

        ! check for buffer array
        If( index(field,'buffer[') .gt.0 ) Then
          Call getBuffer(field, specValue)
          If( eflag )Then
             eflag = .True.
             value = -9.9999D-30
             Return
          End IF
          If( operator.eq.'*' ) value = value * specValue
          If( operator.eq.'/' ) value = value / specValue
          If( operator.eq.'^' ) value = value ** specValue
          cycle
        End If
        ! check for species argument (special functions)
        fnum = 0
        Do m = 1, NumVars
          If( Trim( Varname( m ) ) .Eq.  Trim(field) )Then
             fnum = m 
             Exit
         End If
        End Do

        If( fnum .Gt. 0 .And. fnum .Le. NumVars )Then
           SpecValue(isize) = InputValue( fnum )           
           If( operator.eq.'*' ) value = value * specValue
           If( operator.eq.'/' ) value = value / specValue
           If( operator.eq.'^' ) value = value ** specValue
           cycle
        End If
 
        ! check for species argument (special functions)
        If( index(field,'[') .gt.0 ) Then
  
          ! switch ! and # characters within [] brackets back to + and - characters
          Call replace(field, '!', '+')
          Call replace(field, '#', '-')

          ! parse field between [ ] and check If number or species name
          pos1 = index(field, '[')
          pos2 = index(field, ']',.true.)
          specName = field(pos1+1:pos2-1)
  
          read(specName,'(i16)',iostat=status) fnum

          If( status.eq.0 ) Then    !! number found
            Call readSpecies(field, specValue)
            If( operator.eq.'*' ) value = value * specValue
            If( operator.eq.'/' ) value = value / specValue
            If( operator.eq.'^' ) value = value ** specValue
            cycle
          End If    !! contains '['
        End If
      !try to read field as number
        read(field,'(f20.0)',iostat=status) constant

        If( status.eq.0 ) Then
             If( operator.eq.'*' ) value = value * constant
             If( operator.eq.'/' ) value = value / constant
             If( operator.eq.'^' ) value = value ** constant
        Else
             Write(*,'(''**Error** Invalid field encountered:'',a)') field
             stop 
        End If
      End Do

      Deallocate (specValue)
      return
      end Subroutine eval3

C  routine to compute a field of the expression (parses minor fields (*/^))
      Subroutine eval3b(expression, value)
      
      IMPLICIT NONE

      ! arguments
      
      CHARACTER*(*), Intent( In    ) :: expression
      Real(8),       Intent( InOut ) :: value(isize)

      Logical SHUT3

      ! local variables
      Real(8), allocatable :: specValue(:)
      Character*(EXP_LEN) field
      Character      operator   
      Integer n, nflds

      Allocate ( specValue(isize) )
!      nflds = getFldCount(trim(expression), '*/^')
      nflds = getFldCount(trim(expression), '*/')
      value = 1.0
         
      SpecValue = 0.0D0
      Do n=1,nflds
!        call getFld( trim(expression), '*/^', n, operator, field ) 
        call getFld( trim(expression), '*/', n, operator, field ) 
        call eval4(field, specValue)
!        call GetValue(field, specValue)
!        print*,operator,value,specValue

        If( eflag )Then
           eflag = .True.
           value = -9.9999D-30
           Return
        End IF
        If( operator.eq.'*' ) value = value * specValue
        If( operator.eq.'/' ) value = value / specValue
!        If( operator.eq.'^' ) value = value ** specValue

      End Do

      Deallocate (specValue)
      return
      end Subroutine eval3b
      Subroutine eval4(expression, value)
      
      IMPLICIT NONE

      ! arguments
      
      CHARACTER*(*), Intent( In    ) :: expression
      Real(8),       Intent( InOut ) :: value(:)

      Real(8), allocatable :: specValue(:)
      Real(8), allocatable :: specPower(:)
      Character*(EXP_LEN)  :: field
      Character(1)         :: operator   
      Integer              :: n, pos1, nflds
      Real(8) Factor

      
        

      nflds = getFldCount(trim(expression), '^')
!      value = 1.0
!...No exponents found
      Allocate ( specValue(isize) )
      If( nflds .Eq. 1 )Then
          field = expression
          call GetValue(field, specValue)
!          print*,field,value
           value = specValue
!          print*,Trim(field),specValue,value
          Deallocate (specValue)
          Return
      End If
!...check if correct number of exponents found      
!      If( mod(nflds,2) .Ne. 0 )Then
!          eflag = .True.
!          Write(6,*)'Incorrect number of exponents in Formula: ', Trim(formula)
!          Return
!      End If
!...compute fields with 
      Allocate ( specPower(isize) )
      Value = 1.0D0
      n = nflds 
      call getFld( trim(expression), '^', n, operator, field ) 
      n = n - 1
      call GetValue(field, specPower)
      Do 
         call getFld( trim(expression), '^', n, operator, field )
         n = n - 1
          If( field(1:1) .Eq. '-' )Then
            field  = field(2:)
            Factor = -1.0D0
          Else
            Factor = 1.0D0
          End If 
         call GetValue(field, specValue)
         Value = Factor * specValue**SpecPower
         If( n .Lt. 1)EXIT
         SpecPower = Value
      End Do

      Deallocate (specValue)
      Deallocate (specPower)

      end Subroutine eval4

C  routine to compute a field of the expression (parses minor fields (*/^))
      Subroutine GetValue(expression, value)
      
      IMPLICIT NONE

      ! arguments
      
      CHARACTER*(*), Intent( In    ) :: expression
      Real(8),       Intent( InOut ) :: value(isize)

      Logical SHUT3

      ! local variables
      Real(8), allocatable :: specValue(:)
      Character*(EXP_LEN) field
      Character      operator   
      Integer n, m, nflds, status
      Integer pos1, pos2, fnum
      Character*(16) funcName
      Character*(16) specName
      real(8) constant
      Real(8) Factor

      
        If( expression(1:1) .Eq. '-' )Then
            field    = expression(2:)
            Factor = -1.0D0
        Else If( expression .Eq. '+' )Then
            field    = expression(2:)
            Factor = 1.0D0
        Else            
            field = expression
            Factor = 1.0D0
        End If 
        
!...check for buffer array
        If( index(field,'buffer[') .gt.0 ) Then
          Call getBuffer(field, Value)
          If( eflag )Then
             eflag = .True.
             value = -9.9999D-30
          End IF
          Value = Factor * Value
          Return
        End If
!...check for species argument (special functions)
        fnum = 0
        Do m = 1, NumVars
          If( Trim( Varname( m ) ) .Eq.  Trim(field) )Then
             fnum = m 
             Exit
         End If
        End Do

        If( fnum .Gt. 0 .And. fnum .Le. NumVars )Then
           Value(isize) = Factor * InputValue( fnum )           
           Return
        End If
 
!...check for species argument (special functions)
        If( index(field,'[') .gt.0 ) Then
  
          ! switch ! and # characters within [] brackets back to + and - characters
          Call replace(field, '!', '+')
          Call replace(field, '#', '-')

          ! parse field between [ ] and check If number or species name
          pos1 = index(field, '[')
          pos2 = index(field, ']',.true.)
          specName = field(pos1+1:pos2-1)
  
          read(specName,'(i16)',iostat=status) fnum

          If( status.eq.0 ) Then    !! number found
            Call readSpecies(field, Value)
            Value = Factor * Value
            Return
          End If    !! contains '['
        End If
!...try to read field as number
        read(field,'(f20.0)',iostat=status) constant

        If( status.eq.0 ) Then
          value = Factor * constant
          Return
        Else
             Write(*,'(''**Error** Invalid field encountered:'',a)') field
             eflag = .True.
             value = -9.999E-30
        End If

      return
      end Subroutine GetValue

C  Routine to read species value array for given date and time
      Subroutine readSpecies( field, specValue)

      IMPLICIT NONE

      ! arguments
      Character*(*), Intent( In    ) :: field
      Real( 8 ),     Intent( Inout ) :: specValue(isize)

      ! local variables
      Integer pos1, pos2, status
      Character*(16) specName
      Character*(16) specIndex

      Integer fnum
      Integer m


      ! parse field into species name and file number
      pos1 = index(field, '[') 
      pos2 = index(field, ']',.true.) 
      specName  = field(1:pos1-1)
      specIndex = field(pos1+1:pos2-1)
      

      If(pos1.le.0 .and. pos1.ge.pos2) Then
        Write(*,'(''**ERROR** No Index for species '',a)') trim(specName)
        stop 
      End If

      read(SpecIndex,*,iostat=status) fnum
      if( status.ne.0 ) then
        Write(*,'(/''**ERROR** Cannot read array index for species: '',a)') trim(specName) 
        Write(*,'(''   character containing index:'',a)') trim(SpecIndex) 
        stop 
      endif

      !! call routine to read species values from file fnum
      If( fnum .lt. 1 .Or. fnum .Gt. NumVars )Then
         Write(*,95000)trim(specName), fnum, trim(formula)
         Write(*,'(''   index value:'',i7)')fnum
         Stop
      Else
         SpecValue(isize) = InputValue( fnum )          
         Return
      End If

      If( fnum .Eq. 0 )Then
         Write(*,'(/''**ERROR** Cannot find Function Name '',a,'' used in formula '',a)')
     &            trim(specName), trim(formula)
         Stop
      End If

      return
95000 Format(/'**ERROR** Species Index for ',a,' outside array bounds ',i7, ' in formula ', a)
      end Subroutine readSpecies  

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
      FUNCTION Remove_WhiteSpaces (text)  RESULT(outs)
         Implicit None

         CHARACTER(*), Intent( In ) :: text
         CHARACTER(EXP_LEN)         :: outs     ! provide outs with extra 100 char len
         INTEGER                    :: i, nt

         nt   = LEN_TRIM(text)
         outs = text(1:nt)
         i = 1
         DO 
            nt = LEN_TRIM(outs)
            IF (i .Eq. nt ) EXIT
            IF( outs(i:i) .Eq. ' ' )THEN
               outs = outs(1:i-1) // outs(i+1:nt)
            ELSE
               i = i + 1
            END IF
         END DO
      END FUNCTION Remove_WhiteSpaces
      END MODULE evaluator
