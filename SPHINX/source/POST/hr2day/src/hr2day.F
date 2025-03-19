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
      PROGRAM HR2DAY
C****************************************************************************************
C     Converts hourly data values to daily
C    
C      Using 11 different operations
C   [sum, avg, min, max, hr@min, hr@max, @maxT, maxdif, 8hrmax, w126,
C    @8hrmaxO3, hr@8hrmax, sum06]
C
C          sum - sums the 24 hour values
C          avg - sums the 24 values and divides by 24
C          min - uses the minimum hourly value
C          max - uses the maximum hourly value
C          hr@min - hour at the minimum hourly value
C          hr@max - hour at the maximum hourly value
C          @maxT - uses the hourly value at maximum temperature
C          maxdif - uses the maximum hourly change
C          8hrmax - uses the maximum 8 hour period 
C          w126 - calculates daily w126 index values for use in secondary
C                 ozone standard calculations 
C          @8hrmaxO3 - averages the value within the 8-hr-max ozone period
C          hr@8hrmax - Starting hour of the 8-hr-max period
C          sum06 - sums all values>=0.06ppm between 8am & 8pm
C
C      A daily value is marked as missing when fewer then 18 hourly values
C      are valid.  For 8-hr-max calculations, each 8-hour period must have
C      six or more valid hourly values.
C 
C      USELOCAL using local time, else uses GMT
C
c      PROGRAM hr2day.exe
c
C*******************************************************************************
      USE M3UTILIO
      USE species_def
      USE evaluator
      USE ENV_VARS
      USE M3FILES
      USE GRID_DATA
      USE TIME_STEP

      IMPLICIT NONE

C External functions
      real getTZ

C local variables
      integer status
      logical rstatus
      integer logdev
      integer c, r, n, h, i 
      integer tzadj, hr1
      integer curdate, curtime, cdate, ctime
      integer first_date, first_time, last_date, last_time, runlen
      real x,y,longitude,latitude

      character*(256)  MSG
      character*(16)  PNAME
      DATA  PNAME       / 'hr2day'  /

C Array to store hourly input values
      real, allocatable :: hrValues (:, :, :)
      real, allocatable :: dayValues (:, :)
      integer, allocatable :: tzoffset(:,:)
      real, allocatable :: ftzoffset(:,:)
      integer, allocatable :: offset(:,:)


C... start program
      logdev = init3 ()

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the Models-3 file(s) to process and the other environment   
c  variables
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      CALL OPEN_M3FILES

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check the file(s) for consistency and make sure the requested   
c  species is on the file(s)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL CK_M3FLS()

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the grid definition and the tsteps from the M3 files
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL GET_M3GRID

      CALL GET_TSTEPS


C... load file description from first INFILE
      IF( .NOT. DESC3 ( M3_FLNAME( 1 ) ) ) THEN
         MSG = 'Could not read DESC of  ' // M3_FLNAME( 1 ) 
     &         // ' file'
         CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
      ENDIF

C... check for 1 hour time step
      if( TSTEP3D.ne.10000 ) then
        Call m3err(PNAME, 0, 0, 'Input file must have One hour time step', .TRUE.)
        endif

C... get environment variables
      call GET_ENVS 
c
c... set up the correct projection
c 
      if (GDTYP3D.eq.1) then !lat/lon, nothing to initialize
       continue
      elseif (GDTYP3D.eq.2) then !initialize Lambert projection
       if( .NOT. SETLAM( Real(P_ALP3D), Real(P_BET3D), Real(P_GAM3D),                            
     &                  Real(XCENT3D), Real(YCENT3D) )) then                                    
        Call m3exit (PNAME, 0, 0, 'Lambert projection setup error', xstat2)
       endif     
      elseif (GDTYP3D.eq.6) then !initialize polar stereographic projection
       if( .NOT. SETPOL( Real(P_ALP3D), Real(P_BET3D), Real(P_GAM3D),                            
     &                  Real(XCENT3D), Real(YCENT3D) )) then                                    
        Call m3exit (PNAME, 0, 0, 'Polar Stereographic projection setup error', xstat2)
       endif  
      elseif (GDTYP3D.eq.7) then !initialize equatorial mercator projection
       if( .NOT. SETEQM( Real(P_ALP3D), Real(P_BET3D), Real(P_GAM3D),                            
     &                  Real(XCENT3D), Real(YCENT3D) )) then                                    
        Call m3exit (PNAME, 0, 0, 'Equatorial mercator projection setup error', xstat2)
       endif  
      else
       Call m3exit (PNAME, 0, 0, 'Grid projection not supported', xstat2)   
      endif

C... store first and last day/time to process from module_tstep (across all input files)                    
      first_date = STEP_DATE(1)
      first_time = STEP_TIME(1)                                      
      last_date = STEP_DATE(NSTEPS)
      last_time = STEP_TIME(NSTEPS)                                      

c... adjust first and last date based on START_DATE and END_DATE envvars

      first_date = MAX( first_date, START_DATE)
      last_date  = MIN( last_date, END_DATE)
       
C... get species definitions from system variables
      Call loadSpecies()

C... create output file
      NLAYS3D = 1
      SDATE3D = first_date 
      STIME3D = 000000 
      TSTEP3D = 240000
      NVARS3D = NSPECVAR
      do n = 1, NSPECVAR
        VNAME3D(n) = SPECVARS(n)%NAME
        VDESC3D(n) = SPECVARS(n)%DESCRIPTION
        UNITS3D(n) = SPECVARS(n)%UNITS
        VTYPE3D(n) = M3REAL
      enddo

      if(.not. open3('OUTFILE',3,PNAME)) then
       if(.not. open3('OUTFILE',2,PNAME)) then
          Call m3err('average', 0, 0, 'Could not open OUTFILE file',.TRUE.)
       endif
      endif

C... Allocate memory for data arrays
      Allocate( tzoffset(NCOLS3D, NROWS3D) )
      Allocate( ftzoffset(NCOLS3D, NROWS3D) )
      Allocate( offset(NCOLS3D, NROWS3D) )
      Allocate( hrValues (-36:66, NCOLS3D, NROWS3D) )
      Allocate( dayValues (NCOLS3D, NROWS3D) )

      ! set tzoffset array values 
      ftzoffset = 0
      tzoffset = 0
      if( useLocal ) then
        write(*,'(/,''Computing timezone offsets for grid cells'',/)')
        do r=1,NROWS3D
          do c=1,NCOLS3D
            x = XORIG3D + (c-0.5) * XCELL3D
            y = YORIG3D + (r-0.5) * YCELL3D
            
            if (GDTYP3D.eq.1) then !lat/lon grid, x/y already lon/lat
             longitude=x
             latitude=y
            elseif (GDTYP3D.eq.2) then ! convert Lambert coordinates to lat/lon
             if( .NOT. LAM2LL(x, y, longitude, latitude) ) then
              Call m3exit (PNAME, 0, 0, 'Lat/Lon conversion error', xstat2)
             endif
            elseif (GDTYP3D.eq.6) then !convert polar stereographic coordinates to lat/lon
             if( .NOT. POL2LL(x, y, longitude, latitude) ) then
              Call m3exit (PNAME, 0, 0, 'Lat/Lon conversion error', xstat2)
             endif
            elseif (GDTYP3D.eq.7) then !convert equatorial mercator coordinates to lat/lon
             if( .NOT. EQM2LL(x, y, longitude, latitude) ) then
              Call m3exit (PNAME, 0, 0, 'Lat/Lon conversion error', xstat2)
             endif
            else
             Call m3exit (PNAME, 0, 0, 'Grid projection not supported', xstat2)   
            endif

            ftzoffset(c,r) = getTZ(longitude, latitude)
            tzoffset(c,r) = ftzoffset(c,r)
          enddo
        enddo
      endif

C... reload file description from INFILE
      IF( .NOT. DESC3 ( M3_FLNAME( 1 ) ) ) THEN
         MSG = 'Could not read DESC of  ' // M3_FLNAME( 1 ) 
     &         // ' file'
         CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
      ENDIF

C... start loop to read and process each variable
      do n = 1, NSPECVAR
        hr1 = -36
        curDate = first_date
        curTime = first_time
        hrValues = BADVAL3
        dayValues = BADVAL3

        ! start loops to read hourly values for each day 
        Do
          cDate = curDate
          cTime = hr1 * 10000
          offset = tzoffset
          if(useDST .and. isDSTime(cDate)) offset = offset - 1   ! adjustment for daylight savings time
          call NEXTIME(cDate,cTime,0) !reformat to make sure hours are between 0 and 23
          Do h=hr1,66
            rstatus = .false.
            hrValues(h,:,:) = BADVAL3
            if( (SECSDIFF(STEP_DATE(1), STEP_TIME(1), cdate, ctime) .ge. 0)
     &      .and. (SECSDIFF(cdate, ctime, STEP_DATE(NSTEPS), STEP_TIME(NSTEPS)) .ge. 0) ) then
              Call evaluate(SPECVARS(n)%EXPRESSION, cdate, ctime, 1, NCOLS3D*NROWS3D, hrValues(h,:,:))
              endif
            Call NEXTIME(cDate, cTime, 10000)
          enddo

          ! compute daily values
          if( SPECVARS(n)%OPERATION .eq. 'SUM' ) then
            Call sumValues(hrValues, dayValues, offset)
          endif

          if( SPECVARS(n)%OPERATION .eq. 'AVG' ) then
            Call avgValues(hrValues, dayValues, offset)
          endif

          if( SPECVARS(n)%OPERATION .eq. 'MIN' ) then
            Call minValues(hrValues, dayValues, offset)
          endif

          if( SPECVARS(n)%OPERATION .eq. 'MAX' ) then
            Call maxValues(hrValues, dayValues, offset)
          endif

          if( SPECVARS(n)%OPERATION .eq. 'HR@MIN' ) then
            Call minHrValues(hrValues, dayValues, offset)
          endif

          if( SPECVARS(n)%OPERATION .eq. 'HR@MAX' ) then
            Call maxHrValues(hrValues, dayValues, offset)
          endif

          if( SPECVARS(n)%OPERATION .eq. '@MAXT' ) then
            Call maxTValues(hrValues, dayValues, offset, curDate)
          endif

          if( SPECVARS(n)%OPERATION .eq. 'MAXDIF' ) then
            Call maxDifValues(hrValues, dayValues, offset)
          endif

          if( SPECVARS(n)%OPERATION .eq. '8HRMAX' ) then
            Call max8hr(hrValues, dayValues, offset)
          endif

          if( SPECVARS(n)%OPERATION .eq. 'W126' ) then
            Call w126_daily(hrValues, dayValues, offset, SPECVARS(n)%UNITS)
            endif

          if( SPECVARS(n)%OPERATION .eq. '@8HRMAXO3' ) then
            Call maxO3Values(hrValues, dayValues, offset, curDate)
          endif

          if( SPECVARS(n)%OPERATION .eq. 'HR@8HRMAX' ) then
            Call max8hrHour(hrValues, dayValues, offset)
          endif

          if( SPECVARS(n)%OPERATION .eq. 'SUM06' ) then
            Call sum06(hrValues, dayValues, offset, SPECVARS(n)%UNITS)
          endif

          if( SPECVARS(n)%OPERATION .eq. 'TZ' ) then
            dayValues(:,:) = -REAL(ftzoffset(:,:))
          endif

          ! write daily values to output
          if(.not.write3('OUTFILE',SPECVARS(n)%NAME,curDate,000000,dayValues)) then
          Call m3err (PNAME, curDate, 0, 'Write Error for ' // SPECVARS(n)%NAME, .TRUE.)
          endif

          ! copy current day's values to next day
          do h = -36, 42
            hrValues( h,:,:) = hrvalues(h+24,:,:) 
          enddo

          ! go to next time step
          Call NEXTIME(curdate, curtime, 240000) 

          ! if current date at noon is past last date and time, then exit loop
          if(SECSDIFF(curDate, 120000, last_date, last_time) .le. 0) EXIT

          hr1 = 43
          endDo  ! end (time-step-loop)

        enddo  ! var loop

        rstatus = SHUT3 ()
      stop
      END Program HR2DAY


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to sum hourly values at each cell
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine sumValues(hrValues, dayValues, tzoffset)  

      USE M3UTILIO
      USE ENV_VARS

      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)

C... local variables
      Integer c, r, h, count
      Real sum
 
C... start loops to find daily sum
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          dayValues(c,r) = BADVAL3
          sum = 0.0
          count = 0
          Do h = tzoffset(c,r)+startHr ,tzoffset(c,r)+endHr
            if( hrValues(h,c,r) .gt. BADVAL3 ) then
              count = count+1
              sum = sum + hrValues(h,c,r)
              endif
            endDo      ! end hour loop
          if(partDay .or. count.ge.18) dayValues(c,r) = sum
          endDo      ! end column loop
        endDo      ! end row loop

      return
      end Subroutine sumValues


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to average hourly values at each cell
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine avgValues(hrValues, dayValues, tzoffset)  

      USE M3UTILIO
      USE ENV_VARS

      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)

C... local variables
      Integer c, r, h, count
      Real sum
 
C... start loops to find daily sum
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          dayValues(c,r) = BADVAL3
          sum = 0.0
          count = 0
          Do h = tzoffset(c,r)+startHr ,tzoffset(c,r)+endHr
            if( hrValues(h,c,r) .gt. BADVAL3 ) then
              count = count+1
              sum = sum + hrValues(h,c,r)
              endif
            endDo      ! end hour loop
          if( count.ge.18 ) dayValues(c,r) = sum/count
          if(partDay .and. count.ge.1) dayValues(c,r) = sum/count
          endDo      ! end column loop
        endDo      ! end row loop

      return
      end Subroutine avgValues


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to find the minimum hourly value at each cell
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine minValues(hrValues, dayValues, tzoffset)

      USE M3UTILIO
      USE ENV_VARS

      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)

C... local variables
      Integer c, r, h, count
      Real minValue

C... start loops to find daily sum
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          count = 0
          dayValues(c,r) = BADVAL3
          minValue = 1.0E32 
          Do h = tzoffset(c,r)+startHr ,tzoffset(c,r)+endHr
            if( hrValues(h,c,r) .gt. BADVAL3 ) then
              count = count+1
              if( hrValues(h,c,r).lt.minValue ) minValue = hrValues(h,c,r)
              endif
            endDo      ! end hour loop
          if(partDay .or. count.ge.18) dayValues(c,r) = minValue
          endDo      ! end column loop
        endDo      ! end row loop

      return
      end Subroutine minValues
  

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to find the hour at the minimum hourly value at each cell
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine minHrValues(hrValues, dayValues, tzoffset)

      USE M3UTILIO
      USE ENV_VARS

      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)

C... local variables
      Integer c, r, h, count
      Real minValue
      Real minHour 

C... start loops to find daily sum
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          count = 0 
          dayValues(c,r) = BADVAL3
          minValue = 1.0E32
          minHour = BADVAL3
          Do h = tzoffset(c,r)+startHr ,tzoffset(c,r)+endHr
            if( hrValues(h,c,r) .gt. BADVAL3 ) then
              count = count+1
              if( hrValues(h,c,r).lt.minValue ) then
                minValue = hrValues(h,c,r)
                minHour = h - tzoffset(c,r)
                endif
              endif
            endDo      ! end hour loop
          if(partDay .or. count.ge.18) dayValues(c,r) = minHour 
          endDo      ! end column loop
        endDo      ! end row loop

      return
      end Subroutine minHrValues



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to find the maximum hourly value at each cell
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine maxValues(hrValues, dayValues, tzoffset)

      USE M3UTILIO
      USE ENV_VARS

      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)

C... local variables
      Integer c, r, h, count
      Real maxValue

C... start loops to find daily sum
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          maxValue = BADVAL3
          dayValues(c,r) = BADVAL3
          count = 0
          Do h = tzoffset(c,r)+startHr ,tzoffset(c,r)+endHr
            if( hrValues(h,c,r) .gt. BADVAL3 ) then
              count = count+1
              if( hrValues(h,c,r) .gt. maxValue ) maxValue = hrValues(h,c,r)
              endif
            endDo      ! end hour loop
          if(partDay .or. count.ge.18) dayValues(c,r) = maxValue
          endDo      ! end column loop
        endDo      ! end row loop

      return
      end Subroutine maxValues


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to find the hour at the maximum hourly value at each cell
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine maxHrValues(hrValues, dayValues, tzoffset)

      USE M3UTILIO
      USE ENV_VARS

      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)

C... local variables
      Integer c, r, h, count
      Real maxValue
      Real maxHour 

C... start loops to find daily sum
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          maxValue = BADVAL3
          maxHour = BADVAL3
          dayValues(c,r) = BADVAL3
          count = 0
          Do h = tzoffset(c,r)+startHr ,tzoffset(c,r)+endHr
            if( hrValues(h,c,r) .gt. BADVAL3 ) then
              count = count+1
              if( hrValues(h,c,r) .gt. maxValue ) then
                maxValue = hrValues(h,c,r)
                maxHour = h - tzoffset(c,r)
                endif
              endif
            endDo      ! end hour loop
          if(partDay .or. count.ge.18) dayValues(c,r) = maxHour 
          endDo      ! end column loop
        endDo      ! end row loop

      return
      end Subroutine maxHrValues

 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to find the value when at maximum temperature
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine maxTValues(hrValues, dayValues, tzoffset, curDate)

      USE M3UTILIO
      USE ENV_VARS
      USE M3FILES
      USE TIME_STEP

      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)
      Integer curDate

C... local variables
      Integer status
      Logical, save :: first=.true.  
      Integer, save :: first_date
      Integer, save :: first_time
      Integer, save :: last_date
      Integer, save :: last_time

      Real, save,allocatable :: tvalues(:,:,:)
      Integer runlen
      INTEGER ISTEP
      Integer c, r, h, count
      Integer cdate, ctime
      Real maxTemp, value


C... first pass
      if( first ) then
        first = .false.
        Allocate ( tvalues(-36:66, NCOLS3D, NROWS3D) )

C... store first and last day/time to process from module_tstep (across all input files)                    
        first_date = STEP_DATE(1)
        first_time = STEP_TIME(1)                                      
        last_date = STEP_DATE(NSTEPS)
        last_time = STEP_TIME(NSTEPS)                                      

c... adjust first and last date based on START_DATE and END_DATE envvars

        first_date = MAX( first_date, START_DATE)
        last_date  = MIN( last_date, END_DATE)
      endif   ! first pass
    
C... read temperature values and fill tvalues array
      cdate = curDate
      ctime = -36 * 10000
      call NEXTIME(cDate,cTime,0) !reformat to make sure hours are between 0 and 23
      tvalues = BADVAL3
      do h=-36,66

        if( (SECSDIFF(STEP_DATE(1), STEP_TIME(1), cdate, ctime) .ge. 0)
     &    .and. (SECSDIFF(cdate, ctime, STEP_DATE(NSTEPS), STEP_TIME(NSTEPS)) .ge. 0) ) then

          ISTEP = FIND2( CDATE, CTIME, NSTEPS, STEP_DATE, STEP_TIME)

          if(.not.READ3(M3_FLNAME(STEP_FILE(ISTEP)), tempvar, 1, cdate, ctime, tvalues(h,:,:)) ) then
            Write(*,'(''Cannot read temperature data for @MAXT operation'')')
            endif
          endif

        Call NEXTIME(cDate, cTime, 10000)
      enddo
         
C... start loops to find maximum temperature for each cell
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          dayValues(c,r) = BADVAL3
          maxTemp = BADVAL3
          value = BADVAL3
          count = 0
          Do h = tzoffset(c,r)+startHr ,tzoffset(c,r)+endHr
            if( hrValues(h,c,r) .gt. BADVAL3 ) then
              count = count+1
              if( tvalues(h,c,r) .gt. maxTemp ) then
                maxTemp = tvalues(h,c,r)
                value = hrValues(h,c,r)
                endif
              endif
            endDo      ! end hour loop
          if(partDay .or. count.ge.18) dayValues(c,r) = value
          endDo      ! end column loop
        endDo      ! end row loop

      return
      end Subroutine maxTValues


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to find the value in 8-hour-max ozone period
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine maxO3Values(hrValues, dayValues, tzoffset, curDate)

      USE M3UTILIO
      USE ENV_VARS
      USE M3FILES
      USE TIME_STEP

      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)
      Integer curDate
      INTEGER ISTEP

C... local variables
      Logical, save :: first=.true.  
      Integer, save :: first_date
      Integer, save :: first_time
      Integer, save :: last_date
      Integer, save :: last_time

c      Character*(16) ozonevar
      Real, save,allocatable :: O3values(:,:,:)
      Integer runlen
      Integer c, r, h, i
      Integer cdate, ctime
      Integer count, nperiods
      Real o3sum, sum, maxValue, sumValue, avg
      INTEGER STATUS                ! Status code
c      INTEGER HOURS_8HRMAX          ! number of 8hr values to compute 8hr max     
      CHARACTER*16 ENV_DESC         ! message string
      CHARACTER*16 PNAME            ! Program Name
      CHARACTER*80 MSG              ! Error message
      DATA  PNAME           / 'HR2DAY'        /



C... first pass
      if( first ) then
        first = .false.
        Allocate ( O3values(-36:66, NCOLS3D, NROWS3D) )
C... store first and last day/time to process from module_tstep (across all input files)                    
        first_date = STEP_DATE(1)
        first_time = STEP_TIME(1)                                      
        last_date = STEP_DATE(NSTEPS)
        last_time = STEP_TIME(NSTEPS)                                      

c... adjust first and last date based on START_DATE and END_DATE envvars

        first_date = MAX( first_date, START_DATE)
        last_date  = MIN( last_date, END_DATE)
      endif   ! first pass
    
C... read ozone values and fill tvalues array
      cdate = curDate
      ctime = -36 * 10000
      call NEXTIME(cDate,cTime,0) !reformat to make sure hours are between 0 and 23
      O3values = BADVAL3
      do h=-36,66
        if( (SECSDIFF(STEP_DATE(1), STEP_TIME(1), cdate, ctime) .ge. 0)
     &   .and. (SECSDIFF(cdate, ctime, STEP_DATE(NSTEPS), STEP_TIME(NSTEPS)) .ge. 0) ) then

          ISTEP = FIND2( CDATE, CTIME, NSTEPS, STEP_DATE, STEP_TIME)

          if(.not.READ3(M3_FLNAME(STEP_FILE(ISTEP)), ozonevar, 1, cdate, ctime, O3values(h,:,:)) ) then
            Write(*,'(''Cannot read Ozone data for @8HRMAXO3 operation'')')
           endif

        endif
        Call NEXTIME(cDate, cTime, 10000)
      enddo
        
C... start loops to find max 8-hr and save sum of hrValues
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          dayValues(c,r) = BADVAL3
          maxValue = BADVAL3
          sumValue = BADVAL3
          nperiods = 0
          
          if ( HOURS_8HRMAX .eq. 24 ) then ! use 24 8hr values
          
           Do h = tzoffset(c,r),tzoffset(c,r)+23
            o3sum = 0
            sum = 0
            count = 0
            Do i = 0,7
              if(( O3values(h+i,c,r) .gt. BADVAL3 ) .and.
     *           ( hrValues(h+i,c,r) .gt. BADVAL3 )) then
                o3sum = o3sum + O3values(h+i,c,r)
                sum = sum + hrValues(h+i,c,r)
                count = count + 1
                endif
              enddo   ! end 8 hour loop
            if(count.ge.6) then
               nperiods = nperiods + 1
               avg = o3sum / count
               if( avg.gt.maxValue) then
                 maxvalue = avg   
                 sumvalue = sum/count
                 endif
               endif
            endDo      ! end hour loop
           if(partDay .or. nperiods.ge.18) dayValues(c,r) = sumValue !require 18/24
           
          else !use only 17 8hr values, from 7 am to 11 pm LT

           Do h = tzoffset(c,r)+7,tzoffset(c,r)+23
            o3sum = 0
            sum = 0
            count = 0
            Do i = 0,7
              if(( O3values(h+i,c,r) .gt. BADVAL3 ) .and.
     *           ( hrValues(h+i,c,r) .gt. BADVAL3 )) then
                o3sum = o3sum + O3values(h+i,c,r)
                sum = sum + hrValues(h+i,c,r)
                count = count + 1
                endif
              enddo   ! end 8 hour loop
            if(count.ge.6) then
               nperiods = nperiods + 1
               avg = o3sum / count
               if( avg.gt.maxValue) then
                 maxvalue = avg   
                 sumvalue = sum/count
                 endif
               endif
            endDo      ! end hour loop
           if(partDay .or. nperiods.ge.13) dayValues(c,r) = sumValue !require 13/17

          endif           
           
          endDo      ! end column loop
        endDo      ! end row loop

      return
      end Subroutine maxO3Values


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to find the maximum hourly change
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine maxDifValues(hrValues, dayValues, tzoffset)

      USE M3UTILIO
      USE ENV_VARS

      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)

C... local variables
      Integer c, r, h, count
      Real diff    
      Real maxValue

C... start loops to find daily sum
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          count = 0
          dayValues(c,r) = BADVAL3
          maxValue = BADVAL3
          Do h = tzoffset(c,r)+startHr ,tzoffset(c,r)+endHr-1
            if( hrValues(h,c,r).gt.BADVAL3 .and. hrValues(h+1,c,r).gt.BADVAL3) then
              count = count + 1
              diff = ABS( hrValues(h+1,c,r) - hrValues(h,c,r) )
              if( diff .gt. maxValue ) then
                maxValue = diff
                endif
              endif
            endDo      ! end hour loop
          if(partDay .or. count.ge.18) dayValues(c,r) = maxValue
          endDo      ! end column loop
        endDo      ! end row loop

      return
      end Subroutine maxDifValues


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to find the 8-hour maximum value at each cell
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine max8hr(hrValues, dayValues, tzoffset)

      USE M3UTILIO
      USE ENV_VARS
      
      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)

C... local variables
      Integer c, r, h, i, count, nperiods
      Real sum, maxValue, avg
      INTEGER STATUS                ! Status code
c      INTEGER HOURS_8HRMAX          ! number of 8hr values to compute 8hr max     
      CHARACTER*16 ENV_DESC         ! message string
      CHARACTER*16 PNAME            ! Program Name
      CHARACTER*80 MSG              ! Error message
      DATA  PNAME           / 'HR2DAY'        /

C... Get the HOURS_8HRMAX environment variable (default is 24)                                                          
c       ENV_DESC = 'Number of 8hr values to use when computing DM8HR'                                               
c       HOURS_8HRMAX= ENVINT( 'HOURS_8HRMAX', ENV_DESC, 24, STATUS)  

c       if ( ( HOURS_8HRMAX .NE. 24) .AND. ( HOURS_8HRMAX .NE. 17) ) THEN                                

c        MSG = '**Error** Invalid value for HOURS_8HRMAX, use 24 or 17'
c        CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
c        Stop
c       Endif

C... start loops to find max 8-hr 
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          dayValues(c,r) = BADVAL3
          maxValue = BADVAL3
          nperiods = 0
          
          if ( HOURS_8HRMAX .eq. 24 ) then ! use 24 8hr values
          
           Do h = tzoffset(c,r),tzoffset(c,r)+23
            sum = 0
            count = 0
            Do i = 0,7
              if( hrValues(h+i,c,r) .gt. BADVAL3 ) then
                sum = sum + hrValues(h+i,c,r)
                count = count + 1
                endif
              enddo   ! end 8 hour loop
            if(count.ge.6) then
              nperiods = nperiods + 1
              avg = sum/count
              if( avg .gt. maxValue ) maxValue = avg
              endif
            endDo      ! end hour loop
            if(partDay .or. nperiods.ge.18) dayValues(c,r) = maxValue !require 18/24
           
          else !use only 17 8hr values, from 7 am to 11 pm LT

           Do h = tzoffset(c,r)+7,tzoffset(c,r)+23
            sum = 0
            count = 0
            Do i = 0,7
              if( hrValues(h+i,c,r) .gt. BADVAL3 ) then
                sum = sum + hrValues(h+i,c,r)
                count = count + 1
                endif
              enddo   ! end 8 hour loop
            if(count.ge.6) then
              nperiods = nperiods + 1
              avg = sum/count
              if( avg .gt. maxValue ) maxValue = avg
              endif
            endDo      ! end hour loop
            if(partDay .or. nperiods.ge.13) dayValues(c,r) = maxValue !require 13/17
            
          endif
           
          endDo      ! end column loop
        endDo      ! end row loop

      return
      end Subroutine max8hr
 


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to find the starting hour of the 8-hour maximum period
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine max8hrHour(hrValues, dayValues, tzoffset)

      USE M3UTILIO
      USE ENV_VARS

      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)

C... local variables
      Integer c, r, h, i, count, nperiods
      Real sum, maxValue, avg
      Integer maxHour
      INTEGER STATUS                ! Status code
c      INTEGER HOURS_8HRMAX          ! number of 8hr values to compute 8hr max     
      CHARACTER*16 ENV_DESC         ! message string
      CHARACTER*16 PNAME            ! Program Name
      CHARACTER*80 MSG              ! Error message
      DATA  PNAME           / 'HR2DAY'        /

C... Get the HOURS_8HRMAX environment variable (default is 24)                                                          
c       ENV_DESC = 'Number of 8hr values to use when computing DM8HR'                                               
c       HOURS_8HRMAX= ENVINT( 'HOURS_8HRMAX', ENV_DESC, 24, STATUS)  

c       if ( ( HOURS_8HRMAX .NE. 24) .AND. ( HOURS_8HRMAX .NE. 17) ) THEN                                

c        MSG = '**Error** Invalid value for HOURS_8HRMAX, use 24 or 17'
c        CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
c        Stop
c       Endif

C... start loops to find max 8-hr 
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          dayValues(c,r) = BADVAL3
          maxValue = BADVAL3
          maxHour = 0
          nperiods = 0

          if ( HOURS_8HRMAX .eq. 24 ) then ! use 24 8hr values

           Do h = tzoffset(c,r),tzoffset(c,r)+23
            sum = 0
            count = 0
            Do i = 0,7
              if( hrValues(h+i,c,r) .gt. BADVAL3 ) then
                sum = sum + hrValues(h+i,c,r)
                count = count + 1
                endif
              enddo   ! end 8 hour loop
            if(count.ge.6) then
              nperiods = nperiods + 1
              avg = sum/count
              if( avg .gt. maxValue ) then
                maxValue = avg
                maxHour = h - tzoffset(c,r)   !! maxHour is in local time (0-23)
                endif
              endif
            endDo      ! end hour loop

           if(partDay .or. nperiods.ge.18) dayValues(c,r) = 1.0 * maxHour !require 18/24

          else !use only 17 8hr values, from 7 am to 11 pm LT
          
           Do h = tzoffset(c,r)+7,tzoffset(c,r)+23
            sum = 0
            count = 0
            Do i = 0,7
              if( hrValues(h+i,c,r) .gt. BADVAL3 ) then
                sum = sum + hrValues(h+i,c,r)
                count = count + 1
                endif
              enddo   ! end 8 hour loop
            if(count.ge.6) then
              nperiods = nperiods + 1
              avg = sum/count
              if( avg .gt. maxValue ) then
                maxValue = avg
                maxHour = h - tzoffset(c,r)   !! maxHour is in local time (0-23)
                endif
              endif
            endDo      ! end hour loop

           if(partDay .or. nperiods.ge.13) dayValues(c,r) = 1.0 * maxHour !require 13/17
          
          endif

          endDo      ! end column loop
        endDo      ! end row loop

      return
      end Subroutine max8hrHour

 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to compute to daily W126 values, sums the weighted concentrations
c   in ppm between 8am & 8pm at each cell
c   These daily W126 index values can then be used in subsequent calculations
c   for the secondary ozone standard by summing the daily index values over 
c   consecutive 3 month periods and determining the maximum of these 3 months
c   sums.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine w126_daily(hrValues, dayValues, tzoffset, units)

      USE M3UTILIO
      USE ENV_VARS

      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)
      Character*(*) units

C... local variables
      Integer c, r, h, count
      Real ozone
      Real sum
      Real factor

C... set factor value to covert to ppm
      factor = 1.0
      if( INDEX(units,'ppb') .gt.0 ) factor = 0.001 ! to convert from ppb to ppm

C... start loops to find daily sum
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          sum = 0.0
          count = 0
          Do h = tzoffset(c,r)+8,tzoffset(c,r)+19     ! go from 8am to 7pm local time
            if( hrValues(h,c,r) .ge. 0.0 )  then
              count = count+1
              ozone = factor * hrValues(h,c,r)
              sum = sum + ozone / (1.0 + 4403.0 * EXP( -126.0 * ozone ))
              endif                                                           
            endDo      ! end hour loop                                        

          if( partDay .or. count.ge.9 ) then
            dayValues(c,r) = sum / factor
            endif
          endDo      ! end column loop
        endDo      ! end row loop
                                                                              
      return                                                                  
      end Subroutine w126_daily 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Routine to sum hourly values >= 0.06ppm between 8am & 8pm at each cell
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine sum06(hrValues, dayValues, tzoffset, units)

      USE M3UTILIO
      USE ENV_VARS

      IMPLICIT NONE

C... arguments
      Real hrValues(-36:66, NCOLS3D, NROWS3D)
      Real dayValues(NCOLS3D, NROWS3D)
      Integer tzoffset(NCOLS3D, NROWS3D)
      Character*(*) units

C... local variables
      Integer c, r, h, count
      Real sum
      Real minValue

C... set minimum cutoff value
      minValue = 0.06     ! default is 0.06 ppm
      if( INDEX(units,'ppb') .gt.0 ) minValue = 1000.0 * minValue
      
C... start loops to find daily sum
      Do r = 1, NROWS3D
        Do c = 1, NCOLS3D
          sum = 0.0
          count = 0
          Do h = tzoffset(c,r)+8,tzoffset(c,r)+20
            if( hrValues(h,c,r) .ge. minValue )  then
              count = count+1
              sum = sum + hrValues(h,c,r)
              endif
            endDo      ! end hour loop
          dayValues(c,r) = sum
          endDo      ! end column loop
        endDo      ! end row loop

      return     
      end Subroutine sum06

 
