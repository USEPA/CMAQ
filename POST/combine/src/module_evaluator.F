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
      Integer, Parameter, Private :: EXP_LEN = 1024    

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
      Character*(EXP_LEN) expresscp
      Character*(EXP_LEN) express
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
    
      ! make copy of expression to modify
      expresscp = expression

      ! check for scientific notation (E+,E-,e+,e-) and replace with 10.0^
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
        if( expresscp(i:i).eq.'(' ) then
          nparen = nparen + 1
          depth = depth + 1
          endif

        if( expresscp(i:i).eq.')' ) then
          depth = depth - 1
          endif
        
        if( depth.gt.maxdepth ) maxdepth = depth
        enddo

      !  check for unbalanced parentheses
      if( depth.ne.0 ) then
        write(*,'(/'' unbalanced parentheses in expression''/a)') trim(expresscp)
        stop
        endif

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

      ! find depth of parentheses
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


C  subroutine to replace scientific notation strings
      Subroutine rmSciNot(expression)

      IMPLICIT NONE

      Character*(*) expression

      Character*(2) estring(4)
      Character*(7) pstring(4)

      Integer n, i, pos, pos1, pos2

      Data estring/'E+','e+','E-','e-'/
      Data pstring/'*(10.0^', '*(10.0^', '/(10.0^', '/(10.0^'/

      do n=1,4
        do while( index(expression, estring(n)) .gt. 0 )
          pos = index(expression, estring(n))

          ! search for start of number starting at pos-1 and working back
          pos1 = pos-1
          do i=pos-1,1,-1
            if( index('0123456789.',expression(i:i)) .eq. 0 ) then
              EXIT
              endif
            pos1 = i
            enddo

          ! search for end of number starting at pos+2
          do i=pos+2,pos+12
            if( index('0123456789.',expression(i:i)) .eq. 0 ) then
              pos2=i
              EXIT
              endif
            enddo

          if( pos1 .eq. 1 ) then
            expression = '(' // expression(1:pos-1) // pstring(n) // expression(pos+2:pos2-1)
     &                // '))' // expression(pos2:)
            endif

          if( pos1 .gt. 1 ) then
            expression = expression(1:pos1-1) // '(' // expression(pos1:pos-1) //
     &                 pstring(n) // expression(pos+2:pos2-1) // '))' // expression(pos2:)
            endif

          enddo
        enddo

      return
      end Subroutine rmSciNot



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
      Integer pos1, pos2, nbuf, status
      Character*(10) string
      Character*(10) func
      Logical KSWIT
      Logical SHUT3

      Call leftTrim(field)

      ! parse field to find buffer number
      pos1 = index(field, '[') 
      pos2 = index(field, ']',.true.) 

      if(pos1.le.0 .or. pos1.ge.pos2) then
        write(*,'(/''**ERROR**  Invalid syntax in field: '',a)') trim(field)
        KSWIT = SHUT3()
        stop
        endif

      if(field(pos2+1:) .ne. ' ') then
        write(*,'(/''**ERROR**  Invalid syntax in field: '',a)') trim(field)
        KSWIT = SHUT3()
        stop
        endif

      string = field(pos1+1:pos2-1)
      read(string,'(i10)',iostat=status) nbuf      
      if(status .ne. 0) then
        write(*,'(/''**ERROR**  Invalid syntax in field: '',a)') trim(field)
        KSWIT = SHUT3()
        stop
        endif

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


C  subroutine to evaluate species expression (parses conditional statment if needed)
C   X = (y[1]>10) ? 10 : y[1]
C
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
      Character*(EXP_LEN) field
      Character operator
      Integer nmajor
      Integer i
      Logical badopr


      ! parse major fields (?:)
      nmajor = getFldCount(expression, '?:')

      ! if conditional 
      if( nmajor.eq.3 ) then 
        Allocate( flags(isize), value1(isize), value2(isize) )
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
          Write(*,'(/''**Error** Syntax error encountered in conditional expression: '',a)') trim(expression)
          stop
          endif

        ! set buffer values 
        do i=1,isize
          if( flags(i) ) then
            buffer(i) = value1(i)
          else
            buffer(i) = value2(i)
            endif 
          enddo 

        Deallocate (flags, value1, value2)
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
      Character*(EXP_LEN) field
      Character operator
      Integer nflds
      Integer i


      ! verify that expression contains a parse major fields (<=>)
      nflds = getFldCount(expression, '<=>')
      if( nflds.eq.0 ) then
        Write(*,'(/''**Error** Syntax error encountered in conditional: '',a)') trim(expression)
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
      Write(*,'(/''**Error** Syntax error encountered: '',a)') trim(expression)
      stop
    
      end Subroutine eval1b



C  subroutine to evaluate species expression (parses major fields (+-))
      Subroutine eval2(expression, buffer)

      IMPLICIT NONE

      ! arguments
      Character*(*) expression
      Real buffer(isize)

      ! functions
      Integer getFldCount
 
      ! local variables
      Real, Allocatable :: value(:)
      Character*(EXP_LEN) field
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

        if( field.eq.' ' ) then
          value = 0
         else
          call eval3( field, value)
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


C  routine to compute a field of the expression (parses minor fields (*/^))
      Subroutine eval3(expression, value)
      
      IMPLICIT NONE

      ! arguments
      
      CHARACTER*(*) expression
      Real value(isize)

      Logical SHUT3

      ! local variables
      Real, allocatable :: specValue(:)
      Integer getFldCount
      Character*(EXP_LEN) field
      Character      operator   
      Integer n, nflds, status
      Integer pos1, pos2, fnum
      Character*(16) funcName
      Character*(16) specName
      real constant
      Logical KSWIT

      Allocate ( specValue(isize) )
      nflds = getFldCount(trim(expression), '*/^')
      value = 1.0
         
      do n=1,nflds
        call getFld( trim(expression), '*/^', n, operator, field ) 

        ! check for buffer array
        if( index(field,'buffer[') .gt.0 ) then
          Call getBuffer(field, specValue)
          if( operator.eq.'*' ) value = value * specValue
          if( operator.eq.'/' ) value = value / specValue
          if( operator.eq.'^' ) value = value ** specValue
          cycle
          endif
 
        ! check for species argument (special functions)
        if( index(field,'[') .gt.0 ) then
  
          ! switch ! and # characters within [] brackets back to + and - characters
          Call replace(field, '!', '+')
          Call replace(field, '#', '-')

          ! parse field between [ ] and check if number or species name
          pos1 = index(field, '[')
          pos2 = index(field, ']',.true.)
          specName = field(pos1+1:pos2-1)
  
          read(specName,'(i16)',iostat=status) fnum

          if( status.eq.0 ) then    !! number found
            Call readSpecies(field, specValue)
            if( operator.eq.'*' ) value = value * specValue
            if( operator.eq.'/' ) value = value / specValue
            if( operator.eq.'^' ) value = value ** specValue
            cycle
          else     !! species argument found
            funcName = field(1:pos1-1)
            Call UCASE(funcName)

            status = -1
            if( funcName .eq. 'AVG' )  Call avgSpecies( specName , specValue, status )
            if( funcName .eq. 'SDEV' ) Call sdevSpecies( specName , specValue, status )
            if( funcName .eq. 'MAX' )  Call maxSpecies( specName , specValue, status )
            if( funcName .eq. 'MIN' )  Call minSpecies( specName , specValue, status )

            if( status.eq.0 ) then
              if( operator.eq.'*' ) value = value * specValue
              if( operator.eq.'/' ) value = value / specValue
              if( operator.eq.'^' ) value = value ** specValue
              endif

            endif 

          cycle
          endif    !! contains '['

        !try to read field as number
        read(field,'(f20.0)',iostat=status) constant
        if( status.eq.0 ) then
          if( operator.eq.'*' ) value = value * constant
          if( operator.eq.'/' ) value = value / constant
          if( operator.eq.'^' ) value = value ** constant
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

      USE M3FILES
      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Character*(*) field
      Real specValue(isize)

      ! local variables
      Integer pos1, pos2, status
      Character*(16) specName
      Character*(16) fileName 
      Character*(10) numfld
      Integer fnum
      Integer kdate, ktime
      Integer edate, etime, m
      Logical KSWIT


      ! parse field into species name and file number
      pos1 = index(field, '[') 
      pos2 = index(field, ']',.true.) 
      specName = field(1:pos1-1)

      if(pos1.le.0 .and. pos1.ge.pos2) then
        Write(*,'(''**ERROR** Invalid file number for species '',a)') trim(specName)
        KSWIT = SHUT3()
        stop 
        endif

      ! parse file number
      numfld = field(pos1+1:pos2-1)

      ! read file number from numfld
      read(numfld,*,iostat=status) fnum
      if( status.ne.0 ) then
        Write(*,'(/''**ERROR** Invalid file number for species: '',a)') trim(specName) 
        Write(*,'(''   file number:'',a)') trim(numfld) 
        KSWIT = SHUT3()
        stop 
        endif

      kdate = idate
      ktime = itime

      !! Check numfld for +- sign, to read values of next or previous time step
      if( INDEX(numfld,'-').gt.0 .or. INDEX(numfld,'+').gt.0 ) then
        fnum = ABS( fnum )
        if( .NOT. getDESC( fnum ) ) then
          Write( *, '(''**Error** While running getDESC on file '',i5)') fnum
          stop  
          endif

        !! adjust date/time to read previous timestep
        if( INDEX(numfld,'-') .gt. 0 ) then
          if( SECSDIFF (kDate, ktime, SDATE3D, STIME3D) .lt. 0 ) then
            Call NEXTIME( kdate, ktime, -TSTEP )
            endif
          endif

        !! adjust date/time to read next timestep
        if( INDEX(numfld,'+') .gt. 0 ) then

          ! compute ending time of file
          edate = SDATE3D
          etime = STIME3D
          do m = 1, MXREC3D-1
            Call Nextime(edate, etime, TSTEP3D)
            enddo

          if( SECSDIFF (kDate, ktime, edate, etime) .gt. 0 ) then
            Call NEXTIME( kdate, ktime, TSTEP )
            endif
          endif
        endif   !! condition to adjust timestep for read

      !! call routine to read species values from file fnum
      status = 0
      !! write(*,'(''reading values for '',a,'' at '',2i8)') TRIM(specName), kdate,ktime
      Call ReadValues( fnum, specName, ilayer, kdate, ktime, isize,    
     &                   specValue, status)

      !! check read status
      if( status.ne.0 ) then
        if( fnum.eq.0 ) filename = 'OUTFILE'
        if( fnum.gt.0 ) filename = M3FILENAME(fnum)

        Write(*,'(/''**ERROR** Invalid syntax for field: '',a)') trim(field)     
        Write(*,'(/''**ERROR** Cannot read '',a,'' from '',a)')
     &            trim(specName), trim(fileName)
        KSWIT = SHUT3()
        stop 
        endif  

      return
      end Subroutine readSpecies  

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Routine to compute average species value at each cell from all input files
      Subroutine avgSpecies( specName, specValue, status)

      USE M3FILES
      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Character*(*) specname
      Real specValue(isize)
      Integer status

      ! local variables
      Integer n, i                 
      Real, Allocatable :: values(:,:)
      Logical KSWIT

      ! allocate arrays
      Allocate( values(isize, N_M3FILES) )

      ! read species values from all input files
      do n = 1, N_M3FILES

        status = 0  
        Call ReadValues( n, specName, ilayer, idate, itime, isize, 
     &                   values(:,n), status)

        !! check read status
        if( status.ne.0 ) then
          Write(*,'(/''**ERROR** Cannot read '',a,'' from '',a)')
     &              trim(specName), trim(M3FILENAME(n))
          KSWIT = SHUT3()
          stop
          endif

        enddo    !! read loop

      !! compute averages
      do i = 1, isize
        specValue(i) = 0.0
        do n = 1, N_M3FILES
          specValue(i) = specValue(i) + values(i,n)
          enddo
        specValue(i) = specValue(i) / N_M3FILES 
        enddo  

      ! deallocate arrays
      DeAllocate( values )

      status = 0
      return
      End Subroutine avgSpecies  


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Routine to find minimum species value at each cell from all input files
      Subroutine minSpecies( specName, specValue, status)

      USE M3FILES
      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Character*(*) specname
      Real specValue(isize)
      Integer status

      ! local variables
      Integer n, i                 
      Real, Allocatable :: values(:,:)
      Logical KSWIT

      ! allocate arrays
      Allocate( values(isize, N_M3FILES) )

      ! read species values from all input files
      do n = 1, N_M3FILES

        status = 0  
        Call ReadValues( n, specName, ilayer, idate, itime, isize, 
     &                   values(:,n), status)

        !! check read status
        if( status.ne.0 ) then
          Write(*,'(/''**ERROR** Cannot read '',a,'' from '',a)')
     &              trim(specName), trim(M3FILENAME(n))
          KSWIT = SHUT3()
          stop
          endif

        enddo    !! read loop

      !!  find minimums
      do i = 1, isize
        specValue(i) = values(i,1)
        do n = 2, N_M3FILES
          if(values(i,n) .lt. specValue(i)) specValue(i) = values(i,n)
          enddo
        enddo  

      ! deallocate arrays
      DeAllocate( values )

      status = 0
      return
      End Subroutine minSpecies  



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Routine to find maximum species value at each cell from all input files
      Subroutine maxSpecies( specName, specValue, status)

      USE M3FILES
      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Character*(*) specname
      Real specValue(isize)
      Integer status

      ! local variables
      Integer n, i                 
      Real, Allocatable :: values(:,:)
      Logical KSWIT

      ! allocate arrays
      Allocate( values(isize, N_M3FILES) )

      ! read species values from all input files
      do n = 1, N_M3FILES

        status = 0  
        Call ReadValues( n, specName, ilayer, idate, itime, isize, 
     &                   values(:,n), status)

        !! check read status
        if( status.ne.0 ) then
          Write(*,'(/''**ERROR** Cannot read '',a,'' from '',a)')
     &              trim(specName), trim(M3FILENAME(n))
          KSWIT = SHUT3()
          stop
          endif

        enddo    !! read loop

      !!  find minimums
      do i = 1, isize
        specValue(i) = values(i,1)
        do n = 2, N_M3FILES
          if(values(i,n) .gt. specValue(i)) specValue(i) = values(i,n)
          enddo
        enddo  

      ! deallocate arrays
      DeAllocate( values )

      status = 0
      return
      End Subroutine maxSpecies  


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Routine to compute the standard deviation value at each cell from all input files
      Subroutine sdevSpecies( specName, specValue, status)

      USE M3FILES
      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Character*(*) specname
      Real specValue(isize)
      Integer status

      ! local variables
      Integer n, i   
      Real xtotal, x2total , var
      Real, Allocatable :: values(:,:)
      Logical KSWIT

      !! if the number of files == 1, then set standard deviation values to zero
      if( N_M3FILES .le. 0 ) then
        specValue = 0.0
        status = 0
        return
        endif

      ! allocate arrays
      Allocate( values(isize, N_M3FILES) )

      ! read species values from all input files
      do n = 1, N_M3FILES

        status = 0  
        Call ReadValues( n, specName, ilayer, idate, itime, isize, 
     &                   values(:,n), status)

        !! check read status
        if( status.ne.0 ) then
          Write(*,'(/''**ERROR** Cannot read '',a,'' from '',a)')
     &              trim(specName), trim(M3FILENAME(n))
          KSWIT = SHUT3()
          stop
          endif

        enddo    !! read loop

      !!  find minimums
      do i = 1, isize
        xtotal = 0.0
        x2total = 0.0
        do n = 1, N_M3FILES
          xtotal = xtotal + values(i,n) 
          x2total = x2total + values(i,n)**2 
          enddo

        var = (N_M3FILES*x2total - xtotal**2) / (N_M3FILES * (N_M3FILES-1))
        specValue(i) = SQRT(var)

        enddo  

      ! deallocate arrays
      DeAllocate( values )

      status = 0
      return
      End Subroutine sdevSpecies  

      END MODULE evaluator
