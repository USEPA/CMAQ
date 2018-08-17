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
C $Header: /project/work/rep/TOOLS/src/combine/module_evaluator.F,v 1.1.1.1 2005/07/27 12:55:20 sjr Exp $

C***********************************************************************
C
C  MODULE:  evaluates species expressions
C             
C***********************************************************************
      MODULE evaluator

      Real, Private, Allocatable :: parseBuffer(:,:)

      Integer, Private :: idate
      Integer, Private :: itime  
      Integer, Private :: ilayer
      Integer, Private :: isize 

      CONTAINS


C  subroutine to evaluate species expression at date
C  returns buffer array values
      Subroutine evaluate(expression,jdate,jtime,jlayer,jsize,buffer)

      IMPLICIT NONE

      ! arguments
      Character*(*) expression
      Integer jdate, jtime
      Integer jlayer
      Integer jsize
      Real    buffer(jsize)

      ! local variables
      Character*(512) expresscp
      Character*(512) express
      Integer nparen
      Integer depth, maxdepth
      Integer i, n, pos1, pos2
      Character*(5) nstring
      Logical KSWIT

      ! set module variables
      idate = jdate
      itime = jtime
      ilayer = jlayer
      isize = jsize
    
      ! find number of parentheses and depth
      nparen = 0 
      depth = 0
      maxdepth = 0
      Do i=1,len_trim(expression)
        if( expression(i:i).eq.'(' ) then
          nparen = nparen + 1
          depth = depth + 1
          endif

        if( expression(i:i).eq.')' ) then
          depth = depth - 1
          endif
        
        if( depth.gt.maxdepth ) maxdepth = depth
        enddo

      ! allocate memory for parseBuffer if needed
      if( nparen.gt.0 ) then
        if( Allocated(parseBuffer) .and.
     &      SIZE(parseBuffer,DIM=2).lt.nparen ) then
          deAllocate(parseBuffer)
          endif

        if( .NOT.Allocated(parseBuffer) ) then
          Allocate( parseBuffer(isize,nparen) )
          endif

        parseBuffer = 0.0
        endif

      ! make copy of expression to modify
      expresscp = expression

      depth = maxDepth 
      Do n=1,nparen

        ! build buffer number as string
        write(nstring, '(i5)') n
        Call leftTrim(nstring)

        ! try to find parentheses at depth
        Call findDepth( expresscp, depth, pos1, pos2 )
            
        if( pos1.eq.0 ) then
          depth = depth - 1
          Call findDepth( expresscp, depth, pos1, pos2 )
          endif

        ! if parentheses found, evaluate sub expression
        if( pos1.gt.0 ) then

          ! extract expression within parentheses and
          ! evaluate to parsebuffer(1:isize,n)
          express = expresscp(pos1+1:pos2-1)
          call eval1(express, parsebuffer(1:isize,n) )

          ! replace expression within parentheses with "buffer[n]"
          express = ''
          if( pos1.gt.1 ) express = expresscp(1:pos1-1)
          express = TRIM(express) // 'buffer[' // TRIM(nstring) //
     &              ']' // TRIM(expresscp(pos2+1:))
          expresscp = express 
          endif 
        enddo

      call eval1(expresscp, buffer)
      
      end Subroutine evaluate


C  subroutine to find location of parentheses depth
      Subroutine findDepth(expression, depth, pos1, pos2)

      IMPLICIT NONE

      Character*(*) expression
      Integer depth, pos1, pos2

      Integer i, dep

      pos1 = 0
      pos2 = 0
      dep = 0

      ! try to find parentheses at depth
      Do i = 1, len_trim(expression)  
        if( expression(i:i).eq.'(' ) then
            dep = dep+1
            if(dep.eq.depth) pos1 = i
            endif

          if( expression(i:i).eq.')' ) then
            if(dep.eq.depth) then
              pos2 = i
              return
              endif
            dep = dep-1
            endif           
         enddo

      return
      end Subroutine findDepth


C  subroutine to return buffer array value
      Subroutine getBuffer(field, buffer)
      IMPLICIT NONE

      Character*(*) field
      Real buffer(isize)
      Integer pos1, pos2, nbuf
      Character*(10) string
      Character*(10) func
      Logical KSWIT
      Logical SHUT3

      Call leftTrim(field)

      ! parse field to find buffer number
      pos1 = index(field, '[') 
      pos2 = index(field, ']') 

      string = field(pos1+1:pos2-1)
      read(string,'(i10)') nbuf      

      buffer = parsebuffer(1:isize,nbuf)      

      ! check for function
      pos1 = index(field, 'buffer[') 
      Call UCASE(field)

      if( pos1.gt.1 ) then       
        func = field(1:pos1-1)

        if( func.eq.'LOG' ) then
          buffer = LOG(buffer)
          return
          endif
        if( func.eq.'EXP' ) then
          buffer = EXP(buffer)
          return
          endif
        if( func.eq.'SQRT' ) then
          buffer = SQRT(buffer)
          return
          endif
        if( func.eq.'ABS' ) then
          buffer = ABS(buffer)
          return
          endif

        write(*,'(/''**ERROR** Invalid function name: '',a)') trim(func)
        KSWIT = SHUT3()
        stop
        endif

      return
      end Subroutine getBuffer


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  subroutine to evaluate species expression
C  parses conditional and three argument function statments if found 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine eval1(expression, buffer)

      IMPLICIT NONE

      ! arguments
      Character*(*) expression
      Real buffer(isize)

      ! functions
      Integer getFldCount
 
      ! local variables
      Logical, Allocatable :: flags(:)
      Real, Allocatable :: value1(:)
      Real, Allocatable :: value2(:)
      Real, Allocatable :: value3(:)
      Character*(512) field
      Character*(16)  func
      Character operator
      Integer nmajor
      Integer i
      Logical badopr


      ! parse major fields (?:)
      nmajor = getFldCount(expression, '?:')

      ! if function or conditional statement 
      if( nmajor.ge.3 ) then 
        Allocate( flags(isize), value1(isize), value2(isize), value3(isize) )

        ! if conditional statement
        if( nmajor.eq.3 .and. index(expression,'?') .gt. 0 ) then
          badopr = .false. 
          call getFld( expression, '?:', 1, operator, field ) 
          if(operator.ne.'?') badopr = .true.
          call eval1b( field, flags)

          call getFld( expression, '?:', 2, operator, field ) 
          if(operator.ne.'?') badopr = .true.
          call eval2( field, value1)

          call getFld( expression, '?:', 3, operator, field ) 
          if(operator.ne.':') badopr = .true.
          call eval2( field, value2)

          if( badopr ) then
            Write(*,'(/''**Error** Syntax error encountered at: '',a)') trim(expression)
            stop
            endif

          ! set buffer values by logical flags 
          do i=1,isize
            if( flags(i) ) then
              buffer(i) = value1(i)
            else
              buffer(i) = value2(i)
              endif 
            enddo
          endif

        ! if function statement  ( FunctionName:arg1:arg2:arg3 ) 
        if( nmajor.eq.4 .and. index(expression,'?') .le. 0 ) then
          badopr = .false.
          call getFld( expression, ':', 1, operator, func ) 
          call UCASE(func)
          if( func.ne.'CALCRH' .and. func.ne.'CALCHI' ) badopr = .true.

          call getFld( expression, ':', 2, operator, field ) 
          call eval2( field, value1)

          call getFld( expression, ':', 3, operator, field ) 
          call eval2( field, value2)

          call getFld( expression, ':', 4, operator, field ) 
          call eval2( field, value3)

          if( badopr ) then
            Write(*,'(/''**Error** Syntax error encountered at: '',a)') trim(expression)
            stop
            endif

          ! set buffer values by logical flags 
          do i=1,isize
            if( func.eq.'CALCRH' ) buffer(i) = CALCRH( value1(i), value2(i), value3(i) )
            if( func.eq.'CALCHI' ) buffer(i) = CALCHI( value1(i), value2(i), value3(i) )
            enddo

          endif

        Deallocate (flags, value1, value2, value3)
        return
        endif

      ! if no conditional
      if( nmajor.eq.1 ) then
        call eval2( trim(expression), buffer )
        return
        endif

      ! syntax error
      Write(*,'(/''**Error** Syntax error encountered at: '',a)') trim(expression)
      stop   
      end Subroutine eval1


C  subroutine to evaluate condition expression (called from eval1) 
      Subroutine eval1b(expression, flags)

      IMPLICIT NONE

      ! arguments
      Character*(*) expression
      Logical flags(isize)

      ! functions
      Integer getFldCount
 
      ! local variables
      Real, Allocatable :: value1(:)
      Real, Allocatable :: value2(:)
      Character*(512) field
      Character operator
      Integer nflds
      Integer i


      ! verify that expression contains a parse major fields (<=>)
      nflds = getFldCount(expression, '<=>')
      if( nflds.eq.0 ) then
        Write(*,'(/''**Error** Syntax error encountered at: '',a)') trim(expression)
        stop
        endif

      ! parse conditional expression
      Allocate( value1(isize), value2(isize) )

      ! determine conditional operator is <=
      if( index(expression,'<=').gt.0 ) then
        call getFld( expression, '<=', 1, operator, field ) 
        call eval2( field, value1)
        call getFld( expression, '<=', 3, operator, field ) 
        call eval2( field, value2)
        flags = ( value1 .le. value2 )
        Deallocate (value1, value2)
        return
        endif

      ! determine conditional operator is >=
      if( index(expression,'>=').gt.0 ) then
        call getFld( expression, '>=', 1, operator, field ) 
        call eval2( field, value1)
        call getFld( expression, '>=', 3, operator, field ) 
        call eval2( field, value2)
        flags = ( value1 .ge. value2 )
        Deallocate (value1, value2)
        return
        endif 

      ! determine conditional operator is >
      if( index(expression,'>').gt.0 ) then
        call getFld( expression, '>', 1, operator, field ) 
        call eval2( field, value1)
        call getFld( expression, '>', 2, operator, field ) 
        call eval2( field, value2)
        flags = ( value1 .gt. value2 )
        Deallocate (value1, value2)
        return
        endif 

      ! determine conditional operator is <
      if( index(expression,'<').gt.0 ) then
        call getFld( expression, '<', 1, operator, field ) 
        call eval2( field, value1)
        call getFld( expression, '<', 2, operator, field ) 
        call eval2( field, value2)
        flags = ( value1 .lt. value2 )
        Deallocate (value1, value2)
        return
        endif 

      ! determine conditional operator is =
      if( index(expression,'=').gt.0 ) then
        call getFld( expression, '=', 1, operator, field ) 
        call eval2( field, value1)
        call getFld( expression, '=', 2, operator, field ) 
        call eval2( field, value2)
        flags = ( value1 .eq. value2 )
        Deallocate (value1, value2)
        return
        endif 

      ! syntax error
      Write(*,'(/''**Error** Syntax error encountered at: '',a)') trim(expression)
      stop
    
      end Subroutine eval1b



C  subroutine to evaluate species expression (parses major fields (+-))
      Subroutine eval2(expression, buffer)

      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Character*(*) expression
      Real buffer(isize)

      ! functions
      Integer getFldCount

      ! local variables
      Real, Allocatable :: value(:)
      Character*(512) field
      Character operator
      Integer nmajor
      Integer n

      buffer = 0.0
      Allocate ( value(isize) )

      ! parse major fields (+-)
      nmajor = getFldCount(expression, '+-')

      ! loop thru and parse each major field and evaluate
      do n=1,nmajor

        call getFld( expression, '+-', n, operator, field ) 
        !write(*,'(''major field:'',a)') TRIM(field)
        call eval3( field, value)

        if( value(1).eq.AMISS3 ) then
          buffer = AMISS3
          EXIT 
          endif
        if( operator.eq.'+' ) then
          buffer = buffer + value
         else
          buffer = buffer - value
          endif

        enddo

      Deallocate (value)
      return
      end Subroutine eval2


C  routine to compute a field of the expression (parses minor fields (*/))
      Subroutine eval3(expression, value)
     
      USE M3UTILIO
 
      IMPLICIT NONE

      ! arguments
      CHARACTER*(*) expression
      Real value(isize)

      ! local variables
      Real, allocatable :: specValue(:)
      Integer getFldCount
      Character*(512) field
      Character      operator   
      Integer n, nflds, status
      real constant
      Logical KSWIT

      Allocate ( specValue(isize) )
      nflds = getFldCount(trim(expression), '*/')
      value = 1.0
         
      do n=1,nflds
        call getFld( trim(expression), '*/', n, operator, field ) 

        ! check for buffer array
        if( index(field,'buffer[') .gt.0 ) then
          Call getBuffer(field, specValue)
          if( operator.eq.'*' ) then
            value = value * specValue
           else
            value = value / specValue
            endif          
          cycle
          endif
  
        ! check for species variable
        if( INDEX1(TRIM(field), NVARS3D, VNAME3D) .gt.0 ) then
          Call readSpecies(field, specValue)
          if(specValue(1).eq.AMISS3) then
            value = specValue
            EXIT 
            endif
          if( operator.eq.'*' ) then
            value = value * specValue
           else
            value = value / specValue
            endif
          cycle
          endif

        !try to read field as number
        read(field,'(f20.0)',iostat=status) constant
        if( status.eq.0 ) then
          if( operator.eq.'*' ) then
            value = value * constant
           else
            value = value / constant
            endif
          else
           Write(*,'(''**Error** Invalid field encountered:'',a)') field
           KSWIT = SHUT3()
           stop 
           endif                         

        enddo

      Deallocate (specValue)
      return
      end Subroutine eval3


C  Routine to read species value array for given date and time
      Subroutine readSpecies( field, specValue)

      USE M3UTILIO
      USE M3FILES
      USE TIME_STEP

      IMPLICIT NONE

      ! arguments
      Character*(*) field
      Real specValue(isize)

      ! local variables
      Logical KSWIT
      INTEGER ISTEP
      
      ISTEP = FIND2( IDATE, ITIME, NSTEPS, STEP_DATE, STEP_TIME)

      KSWIT = READ3(M3_FLNAME(STEP_FILE(ISTEP)), field, ilayer, idate, itime, specValue)

      !! check read status
      if( .NOT.KSWIT ) then
        Write(*,'(/''**ERROR** Cannot read variable '',a,'' from input file.'')')
     &            trim(field)
        specValue = AMISS3
        !KSWIT = SHUT3()
        !stop 
      endif  

      return
      end Subroutine readSpecies  


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   function to compute relative humidity
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Real Function CALCRH(ta, psa, qva) result(rh)
      Implicit None

      ! arguments
      Real ta            ! surface temperature (K)
      Real psa           ! surface pressure (pascal)
      Real qva           ! water vapor mixing ratio (kg/kg)

      ! local variables
      Real es, w

      !  compute es
      if( ta .le. 273.15 ) es = 611.29 * EXP(22.514 - (6150.0 / ta))
      if( ta .gt. 273.15 ) es = 611.29 * EXP(17.67 * (ta-273.15) / (ta-29.65))

      !  compute rh
      w = 0.622 * es / (psa - es)
      rh = 100.0 * qva / w

      if( rh.gt.100.0 ) rh = 100.0

      return
      end Function CALCRH


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   function to compute heat index        
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Real Function CALCHI(ta, psa, qva) result(hi)
      Implicit None

      ! arguments
      Real ta            ! surface temperature (K)
      Real psa           ! surface pressure (pascal)
      Real qva           ! water vapor mixing ratio (kg/kg)

      ! local variables
      Real rh, tempF

      ! compute temperature to F
      tempF = 1.8*(ta-273.15) + 32.0

      ! compute RH
      rh = CALCRH(ta, psa, qva)

      ! check lower rh limit
      if( rh .lt. 40.0 ) then
        hi = tempF
        return
        endif 

      ! check lower tempF limit
      if( tempF .lt. 80.0 ) then
        hi = tempF
        return
        endif 

      ! compute HI
      hi = -42.379 + 2.04901523*tempF + 10.14333127*rh
      hi = hi - 0.22475541*tempF*rh - 6.83783E-3 * tempF**2
      hi = hi - 5.481717E-2 * rh**2 + 1.22874E-3 * tempF**2 * rh
      hi = hi + 8.5282E-4 * tempF * rh**2 - 1.99E-6 * tempF**2 * rh**2

      return
      end Function CALCHI

 
      END MODULE evaluator
