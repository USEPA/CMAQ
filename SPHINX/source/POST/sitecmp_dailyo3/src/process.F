
      SUBROUTINE PROCESS

C*************************************************************************
C
C  code for comparing CASTNET data with model data
C 
C
C#############################################################
C#  Input files
C#############################################################
C
C   ioapi input files containing VNAMES (Max of 10 files)
C   set M3_FILE_1=example1.ioapi
C   set M3_FILE_2=example2.ioapi
C   set M3_FILE_3
C
C
C#############################################################
C#  Output files
C#############################################################
C
C   output table (delimited text file importable to Excel)
C   set OUT_TABLE=outTable.txt
C                   
C*************************************************************************


      USE M3FILES
      USE ENV_VARS
      USE GRID_DATA
      USE TIME_STEP
      USE SITE_DATA
      USE SPECIES_DEF
      USE M3UTILIO

      IMPLICIT NONE     

C..INCLUDE FILES:
C      INCLUDE SUBST_IOPARMS     ! IOAPI parameters
C      INCLUDE SUBST_IOFDESC     ! IOAPI file description
C      INCLUDE SUBST_IODECL      ! IOAPI declarations

C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS:
C      LOGICAL ISDSTIME
C      INTEGER TIME2SEC
C      INTEGER SECSDIFF
C      INTEGER JULIAN   
C      Character*10 HHMMSS
      Character*16 date2Str
      Character*16 date2Str_csv
      Character*16 real2Str
      Character*16 int2Str

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*16    PNAME        ! Program Name
      CHARACTER*80    MSG          ! Error message
      CHARACTER*256   RECORD       ! input buffer
      CHARACTER*256   OUTRECORD    ! output buffer
      CHARACTER*256   FIELD        ! field buffer  
      CHARACTER*16    CELLCOLUMN   ! field for cell column value
      CHARACTER*16    CELLROW      ! field for cell row value
      CHARACTER*1     delimiter    ! field delimiter  

      INTEGER n, i, j, index, in, out, siteNo
      CHARACTER*2560   HEADER1       ! first header line (variable names)
      CHARACTER*2560   HEADER2       ! second header line
      CHARACTER*2560   HEADER3       ! third header line (units)
      CHARACTER*2560   tpRecord      ! time period record

      Character*20    siteid, siteid2
      Character*2     cpoc
      Integer         poc, poc2
      REAL values(32), values2(24)

      Integer iyear, imonth, iday, status, julian1
      Integer iyear2, imonth2, iday2, status2, julian2
      Integer stepsec, periodsec, neededsteps, istep1, istep2
      Integer startDate, startTime, endDate, endTime, tz
      Integer recDate
      Real lat, lon, lambX, lambY

      Real valmax, varmax, val8max, var8max
      Real valSum,varSum
      Real valmin,varmin
      Real varmax9, var8max9
      Real w126_mod, w126_obs
      Integer valcount, varcount
      Real vardata(32,9)     ! species data (tsteps, cells)

      Integer hrMaxVal, hrMaxVar, hrMax9Var
      Integer hr8MaxVal, hr8MaxVar, hr8Max9Var

      Integer       valcnt, varcnt
      Integer nfields

      Character*16 :: fields(28)
      LOGICAL DATACHECK
  
C**********************************************************************
      DATA PNAME / 'PROCESS' /
      DATA delimiter / ',' /

C****************************************************************
C  set units for model species

      if( SPECIES%MOD_UNITS .eq. '' ) then
        Call get_units( SPECIES%MOD_NAME(1), SPECIES%MOD_UNITS )
        Endif
      varMin = 0.06
      if( INDEX(SPECIES%MOD_UNITS,'ppb') .gt. 0 ) varMin = 1000.0 * varMin

      SPECIES%OBS_UNITS = SPECIES%MOD_UNITS
      valMin = varMin

C****************************************************************
C  open input and output table files
C****************************************************************
      in = 20
      out = 30 
      open(unit=in, file=IN_TABLE, status='old', err=900)
      open(unit=out, file=OUT_TABLE, err=901)

C*********************************************************************
C*  build headers with variable names and units to headers and write to output
C*********************************************************************
      !  build time period record
      if( START_DATE.gt.0 .and. END_DATE.gt.0 ) then
        tpRecord = '  Output generated for period ' // TRIM(date2Str(START_DATE))
        tpRecord = TRIM(tpRecord) // ' ' // HHMMSS(START_TIME)
        tpRecord = TRIM(tpRecord) // ' thru ' // TRIM(date2Str(END_DATE))
        tpRecord = TRIM(tpRecord) // ' ' // HHMMSS(END_TIME)
       else
        tpRecord = '  Output generated for all observed readings'
        Endif
 
      header1 = 'SiteId,POCode,State,County,Elevation,Latitude,Longitude,'
      if( LAMBXY ) header1 = TRIM(header1) // 'Lambert-X,LAMBERT-Y,'
      header1 = TRIM(header1) // 'Column,Row,Time On,Time Off,'
      header1 = TRIM(header1) // 'SMM,SDD,SYYYY,EMM,EDD,EYYYY,'
      header1 = TRIM(header1) // 'O3_1hrmax_ob,O3_1hrmax_mod,'
      header1 = TRIM(header1) // 'O3_1hrmax_9cell_ob,O3_1hrmax_9cell_mod,'
      header1 = TRIM(header1) // 'O3_1hrmax_time_ob,O3_1hrmax_time_mod,'
      header1 = TRIM(header1) // 'O3_8hrmax_ob,O3_8hrmax_mod,'
      header1 = TRIM(header1) // 'O3_8hrmax_9cell_ob,O3_8hrmax_9cell_mod,'
      header1 = TRIM(header1) // 'O3_8hrmax_time_ob,O3_8hrmax_time_mod,'
      header1 = TRIM(header1) // 'W126_ob,W126_mod,SUM06_ob,SUM06_mod'

      header2 = ',,,,(m),(deg),(deg),,,mm/dd/yyyy hh:mm,mm/dd/yyyy hh:mm'  
      if( LAMBXY ) header2 = ',,,,(m),(deg),(deg),(meters),(meters),,,mm/dd/yyyy hh:mm,mm/dd/yyyy hh:mm'  
      header2 = TRIM(header2) // ',MM,DD,YYYY,MM,DD,YYYY'
      header2 = TRIM(header2) // ',' // TRIM(SPECIES%OBS_UNITS) 
      header2 = TRIM(header2) // ',' // SPECIES%MOD_UNITS 
      header2 = TRIM(header2) // ',' // TRIM(SPECIES%OBS_UNITS) 
      header2 = TRIM(header2) // ',' // SPECIES%MOD_UNITS 
      header2 = TRIM(header2) // ',hour,hour' 
      header2 = TRIM(header2) // ',' // TRIM(SPECIES%OBS_UNITS)
      header2 = TRIM(header2) // ',' // SPECIES%MOD_UNITS
      header2 = TRIM(header2) // ',' // TRIM(SPECIES%OBS_UNITS)
      header2 = TRIM(header2) // ',' // SPECIES%MOD_UNITS
      header2 = TRIM(header2) // ',hour,hour' 
      header2 = TRIM(header2) // ',ppm-hours,ppm-hours'
      header2 = TRIM(header2) // ',ppm-hours,ppm-hours'


      header3 = 'id,,,,,,,,,,,,,,,,,observed,modeled,observed,modeled,observed,'//
     &  'modeled,observed,modeled,'//
     &  'observed,modeled,observed,modeled,observed,modeled,observed,modeled' 
     
      if( LAMBXY ) header3 =
     &  'id,,,,,,,,,,,,,,,,,,,observed,modeled,observed,modeled,observed,'//
     &  'modeled,observed,modeled,'//
     &  'observed,modeled,observed,modeled,observed,modeled,observed,modeled' 



c      Write(out,'(''Maximum Observed and Modeled Ozone Values'',/)')
c      Write(out,'(a,/)') TRIM(tpRecord)
c      Write(out,'(''Maximum Observed and Modeled Ozone Values'')')
c      Write(out,'(a)') TRIM(tpRecord)
      Write(out,'(''Modeled values read from file:'',a)') TRIM(M3FILE)
c      Write(out,'(''Observed values read from file:'',a,/)') TRIM(IN_TABLE)
      Write(out,'(''Observed values read from file:'',a)') TRIM(IN_TABLE)
      Write(out,'(a)') TRIM(tpRecord)
c      Write(out,'(a)') TRIM(header1)
      Write(out,'(a)') TRIM(header2)
      Write(out,'(a)') TRIM(header3)
      Write(out,'(a)') TRIM(header1)


C**********************************************************************
C*   read each record, get site and time period
C**********************************************************************
      !  read values for day 1 
      Call readInput(in, siteid, poc, iyear, imonth, iday, values, status)
      if(status.lt.0) goto 200           
      julian1 = JULIAN(iyear, imonth, iday)

      do i=1,24
       if (values(i).ge.0) then
        values(i) = SPECIES%OBS_FACTOR * values (i)
       endif
      enddo !i
      

      Do While(.true.)
        DATACHECK = (status.eq.0)
  
        !  read next day and append 8 hours to day 1
        Call readInput(in, siteid2, poc2, iyear2, imonth2, iday2, values2, status2)
        julian2 = JULIAN(iyear2, imonth2, iday2)
        do i=1,24
         if (values2(i).ge.0) then
          values2(i) = SPECIES%OBS_FACTOR * values2 (i)
         endif
        enddo !i
        if(status2.eq.0 .and. siteid2.eq.siteid .and. poc2.eq.poc .and. 
     *     julian1+1.eq.julian2 )then
          do i=1,8
            values(24+i) = values2(i)
            enddo
         else
          do i=1,8
            values(24+i) = -999     
            enddo
         endif

         valcnt = 0
         do i=1,24
          if (values(i).ge.0) then
           valcnt = valcnt + 1
          endif
        enddo !i
      
        if (valcnt .eq. 0) DATACHECK = .FALSE. 

        siteno = getSiteNumber( siteid )
        
       ! check if site is located in grid domain
        if(getColumn(siteno).eq.0 .OR. getRow(siteno).eq.0) DATACHECK = .FALSE. 
     
        if( DATACHECK .and. siteno.gt.0 ) then
          lat = getLatitude( siteno )
          lon = getLongitude( siteno ) 
          lambX = getLambertX( siteno )
          lambY = getLambertY( siteno )
          tz =  getTimeZone( siteno )

          Write(cellColumn,'(i5)') getColumn(siteno)
          Call LeftTrim(cellColumn) 
          Write(cellRow,'(i5)') getRow(siteno)
          Call LeftTrim(cellRow) 
    
          ! compute date of input record
          recDate = 1000*iyear + JULIAN( iyear, imonth, iday )
 
          ! find start and end dates and times
          startDate = recDate
          startTime = 0
    
          ! adjust timezone for day light saving only if APPLYDLS is true
          if( APPLYDLS .AND. ISDSTIME( startDate ) ) tz = tz-1

          ! adjust starting date and time for timezone
          CALL NEXTIME( startDate, startTime, 10000*tz )

          endDate = startDate
          endTime = startTime
          CALL NEXTIME( endDate, endTime, 235959 )

          ! check if dates are within time window
          if( START_DATE .gt. 0 ) Then
            if( startDate .lt. START_DATE )  DATACHECK = .FALSE.     
            if( startDate .eq. START_DATE .and. startTime .lt. START_TIME) DATACHECK = .FALSE. 
            Endif
            
          if( END_DATE .gt. 0 ) Then
            if( startDate .gt. END_DATE )  DATACHECK = .FALSE.         
            Endif

          if( DATACHECK ) then 
            Call startEndSteps(startDate, startTime, endDate, endTime, istep1, istep2) 
            if( istep1.lt.0 .or.istep2.lt.0 ) DATACHECK = .FALSE.
            Endif

          if( DATACHECK ) then
            stepsec = TIME2SEC( TIMESTEP )
            periodsec = SECSDIFF( startDate, startTime, endDate, endTime )
            neededSteps = periodsec / stepsec
            if( neededSteps .gt. istep2-istep1+1 ) then
              Write(*,'(''**Warning** not all values found for day '',i8)') startDate
              endif
            EndIF
 
          ! Read values for SPECIES
          if( DATACHECK ) then
             vardata = 0
             Call getSpeciesValue(siteNo, Species, istep1, istep2+8, vardata )

             ! find max value over 24 hour period
             valmax = -99.0
             varmax = -99.0
             hrMaxVal = -99
             hrMaxVar = -99
             valcnt = 0
             varcnt = 0

             Do i=1,24
              if( values(i) .ge. 0 ) valcnt = valcnt+1
              if( values(i) .gt. valmax ) then
               valmax = values(i)
               hrMaxVal = i - 1
              endif
              if( vardata(i,5) .ge. 0 ) varcnt = varcnt+1
              if( vardata(i,5) .gt. varmax ) then
               varmax = VARDATA(i,5)
               hrMaxVar = i - 1
              endif            
              Enddo

             !  check for incomplete days
             if( MISS_CHECK ) then
               if( valcnt.lt.18 ) valmax = -99.0
               if( valcnt.lt.18 ) hrMaxVal = -99
               if( varcnt.lt.18 ) varmax = -99.0
               if( varcnt.lt.18 ) hrMaxVar = -99
               endif

             Call get8hourMax(values, val8max, hr8MaxVal, MISS_CHECK)
             Call get8hourMax(vardata(:,5), var8max, hr8MaxVar, MISS_CHECK)
             Call getMax9(vardata, varmax9, hrMax9var)
             Call get8hrMax9(vardata, var8max9, hr8Max9var, MISS_CHECK)
             Call getW126(values, SPECIES%OBS_UNITS, w126_obs)
             Call getW126(vardata(:,5), SPECIES%MOD_UNITS, w126_mod)

             ! find sum06 value, sum over values (8am-8pm) 
             valSum = 0.0
             varSum = 0.0
             valCnt = 0
             varCnt = 0

             Do i=8,20
              if( values(i) .ge. 0.0 ) valCnt = valCnt + 1
              if( vardata(i,5) .ge. 0.0 ) varCnt = varCnt + 1            
              if( values(i) .ge. valMin ) valSum = valSum + values(i)
              if( vardata(i,5) .ge. varMin ) varSum = varSum + vardata(i,5)            
              Enddo

             ! check for missing values
             if( valCnt .lt. 8 ) valSum = -99.0
             if( varCnt .lt. 8 ) varSum = -99.0
             
             if( INDEX(SPECIES%MOD_UNITS,'ppb') .gt. 0 ) then !convert sum06 back to ppm-hours for output
              if (valSum.ge.0.) valSum = valSum / 1000.
              if (varSum.ge.0.) varSum = varSum / 1000.
             endif

             !build output fields
          if( LAMBXY ) then
            nfields = 26
            fields(1) = real2Str(lat, .false.)
            fields(2) = real2Str(lon, .false.)
            fields(3) = real2Str(lambX, .false.)
            fields(4) = real2Str(lambY, .false.)
            fields(5) = cellColumn
            fields(6) = cellRow
            write (fields(7),'(a,a)')  Trim(date2Str(recDate)),' 00:00'
            write (fields(8),'(a,a)')  Trim(date2Str(recDate)),' 23:59'
            write (fields(9),'(a)')  Trim(date2Str_csv(recDate))
            write (fields(10),'(a)')  Trim(date2Str_csv(recDate))
            fields(11) = real2Str(valmax, .true.)
            fields(12) = real2Str(varmax, .true.)
            fields(13) = real2Str(valmax, .true.)
            fields(14) = real2Str(varmax9, .true.)
            fields(15) = int2Str(hrMaxVal, .true.)
            fields(16) = int2Str(hrMaxVar, .true.)
            fields(17) = real2Str(val8max, .true.)
            fields(18) = real2Str(var8max, .true.)
            fields(19) = real2Str(val8max, .true.)
            fields(20) = real2Str(var8max9, .true.)
            fields(21) = int2Str(hr8MaxVal, .true.)
            fields(22) = int2Str(hr8MaxVar, .true.)
            fields(23) = real2Str(w126_obs, .true.)
            fields(24) = real2Str(w126_mod, .true.)
            fields(25) = real2Str(valSum, .true.)
            fields(26) = real2Str(varSum, .true.)
           else
            nfields = 24
            fields(1) = real2Str(lat, .false.)
            fields(2) = real2Str(lon, .false.)
            fields(3) = cellColumn
            fields(4) = cellRow
            write (fields(5),'(a,a)')  Trim(date2Str(recDate)),' 00:00'
            write (fields(6),'(a,a)')  Trim(date2Str(recDate)),' 23:59'
            write (fields(7),'(a)')  Trim(date2Str_csv(recDate))
            write (fields(8),'(a)')  Trim(date2Str_csv(recDate))
            fields(9) = real2Str(valmax, .true.)
            fields(10) = real2Str(varmax, .true.)
            fields(11) = real2Str(valmax, .true.)
            fields(12) = real2Str(varmax9, .true.)
            fields(13) = int2Str(hrMaxVal, .true.)
            fields(14) = int2Str(hrMaxVar, .true.)
            fields(15) = real2Str(val8max, .true.)
            fields(16) = real2Str(var8max, .true.)
            fields(17) = real2Str(val8max, .true.)
            fields(18) = real2Str(var8max9, .true.)
            fields(19) = int2Str(hr8MaxVal, .true.)
            fields(20) = int2Str(hr8MaxVar, .true.)
            fields(21) = real2Str(w126_obs, .true.)
            fields(22) = real2Str(w126_mod, .true.)
            fields(23) = real2Str(valSum, .true.)
            fields(24) = real2Str(varSum, .true.)
           endif

           write(cpoc,'(i0)') poc
             ! write output record to out table
             Write(out,'(70a)') Trim(siteid),delimiter,Trim(cpoc),
     &           delimiter,trim(SITES(siteno)%state),
     &           delimiter,trim(SITES(siteno)%county),
     &           delimiter,trim(real2Str(SITES(siteno)%elev, .true.)),
     &           (delimiter,Trim(fields(i)), i=1,nfields)

             Write(*,'(''values for site '',a,'' written for '',a)')       
     &            Trim(siteid), Trim(fields(5))                    
 
             Endif
          
        Else
          If(siteno .le. 0 ) then
           Write(*,'(''Site not found in SITE_FILE: '',a)') Trim(siteid)
          endif
         EndIf 

      ! exit loop at eof             
      if( status2.lt.0 ) go to 200

      ! copy values from day2 to day1
      siteid = siteid2
      poc = poc2
      iyear = iyear2
      imonth = imonth2
      iday = iday2
      julian1 = julian2
      status = status2
      do i=1,24
        values(i) = values2(i)
        enddo
      
      EndDo
  200 continue

      RETURN

  900 write(*,'('' Cannot open input table file ['',a,'']'')') TRIM(IN_TABLE)
      return

  901 write(*,'('' Cannot open output table file ['',a,'']'')') TRIM(OUT_TABLE)
      return
      END


C****************************************************************************
C  routine to find the starting and ending time steps
C****************************************************************************
      Subroutine startEndSteps(startDate, startTime, endDate, endTime, istep1, istep2)

      USE TIME_STEP

      INTEGER startDate, startTime, endDate, endTime, istep1, istep2

      INTEGER I

      istep1 = -1
      istep2 = -1
   
      ! find starting step
      Do I=1,NSTEPS
        if( STEP_DATE(I).gt.startDate ) istep1=I  
        if( STEP_DATE(I).eq.startDate .AND. STEP_TIME(I).ge.startTime ) istep1=I
        if(istep1.gt.0) EXIT
      EndDo

      ! if starting point not found return -1's
      if(istep1.lt.0) return 

      istep2 = NSTEPS

      ! find ending step
      Do I=istep1,NSTEPS
        if( STEP_DATE(I).gt.endDate ) EXIT
        if( STEP_DATE(I).eq.endDate .AND. STEP_TIME(I).gt.endTime ) EXIT
        istep2=I
      EndDo

      Return
      End Subroutine startEndSteps


C****************************************************************************
C  routine to get values for species from ioapi files for time period
C****************************************************************************
      Subroutine getSpeciesValue(siteNo, Spec, istep1, istep2, values)
 
      USE ENV_VARS
      USE SITE_DATA
      USE SPECIES_DEF
      USE TIME_STEP
 
      IMPLICIT NONE
 
C..ARGUMENTS:
      INTEGER siteNo
      TYPE ( SPEC_VAR ) Spec
      INTEGER istep1, istep2
      Real values(32,9)    
 
C..Local variables
      Real  specValues(32,9)
      Real*8 specValue
      Integer nvalues, nv
      Integer n, i, c
      Integer lastStep
 
C..  allocate array to store values read from ioapi files
      nvalues = istep2 - istep1 + 1
     
      lastStep = istep2
 
C.. check if last step is past end of data
      if(istep2.gt.NSTEPS) then

        ! initialize values past end of data to -99
        nv = NSTEPS - istep1 + 1
        if(nv.lt.0) nv=0 
        Do i=nv+1,nvalues
          values(i,:) = -99.0
          enddo

        !  set the number of values to read
        lastStep = NSTEPS
        nvalues = nv
        endif

C.. initialize specValue to zero
      specValue = 0.0

C.. read each model species from file and update values
      Do n=1,spec%NUMSPEC
        Call getValues(siteNo, spec%MOD_NAME(n), istep1, lastStep, specValues) 
      
        ! update each cell 1-9
        do c = 1,9
          ! update values array
          Do i=1,nvalues
 
            ! adjust value by factor
            if ( (values(i,c) .ge. 0.) .and. (specValues(i,c) .ge. 0.) )then
             values(i,c) = values(i,c) + specValues(i,c) * spec%MOD_FACTOR(n)
            endif
            Enddo 
          EndDo
        EndDo
 
      Return
      End Subroutine getSpeciesValue
 

C****************************************************************************
C  routine to read values from files at site for variable for time period
C****************************************************************************
      SUBROUTINE getValues(siteNo, VARNAME, istep1, istep2, VALUES)

      USE SITE_DATA
      USE M3FILES
      USE ENV_VARS
      USE GRID_DATA
      USE TIME_STEP
      USE M3UTILIO
      

      IMPLICIT NONE     

C..INCLUDE FILES:
C      INCLUDE SUBST_IOPARMS     ! IOAPI parameters
C      INCLUDE SUBST_IOFDESC     ! IOAPI file description
C      INCLUDE SUBST_IODECL      ! IOAPI declarations

C..ARGUMENTS:
      INTEGER siteNo
      Character*(*) VARNAME
      INTEGER istep1, istep2
      REAL VALUES(32,9)

C..SCRATCH LOCAL VARIABLES:
      INTEGER   N, I, J, C, R, C1, R1, S
      INTEGER  row, col
      CHARACTER*16    PNAME        ! Program Name
      CHARACTER*80    MSG          ! Error message
      REAL, Allocatable, Save :: GRIDVAL(:,:)
      REAL, Allocatable, Save :: SITEVAL(:,:,:)
      LOGICAL, Save :: LFIRST 

      DATA LFIRST / .true. /
      DATA PNAME / 'SITE_EXTRACT' /


      ! on first time called, allocate memory for GRIDVAL array
      if( LFIRST ) then
        Allocate ( GRIDVAL( M3GRID % NCOLS, M3GRID % NROWS ) )
        Allocate ( SITEVAL( size(SITES), NSTEPS , 9 ) )
        SITEVAL = -999.
        LFIRST = .false.
        
        do N = 1, NSTEPS !number of time steps across all model files from module_tstep

         IF( .NOT. READ3( M3_FLNAME(STEP_FILE(N)), VARNAME, 1, STEP_DATE(N), 
     &                   STEP_TIME(N), GRIDVAL ) ) THEN
     
          MSG = 'Could not read input Models-3 file ' // M3_FLNAME(STEP_FILE(N))        
          CALL M3WARN( PNAME, STEP_DATE(N), STEP_TIME(N), MSG)
          
         ELSE
         
          do S = 1, size(SITES)

           row = getRow(S)
           col = getColumn(S)

           if ( ( row .ne. 0 ) .and. ( col .ne. 0 ) ) then

            j = 0
            Do c = col-1,col+1
             Do r = row-1, row+1
              j = j+1

              ! check for edge of grid
              c1 = c
              r1 = r
              if(c1.le.0 .or. c1.gt.NCOLS3D) c1 = col
              if(r1.le.0 .or. r1.gt.NROWS3D) r1 = row
              SITEVAL(S,N,J) = GRIDVAL( c1, r1 )
             endDo   ! row loop
            enddo     ! column loop
           
           endif ! site within domain

          enddo !S
         
         ENDIF
        
        enddo !N
      EndIf !first time



      ! loop to read each value in time period
      I = 0
      Do N=istep1, istep2
        I = I + 1
     
        do J = 1, 9
         VALUES(I,J) = SITEVAL(siteno,n,j)
        endDo   ! J
      enddo     ! N

 
      Return
      End SUBROUTINE getValues 


C****************************************************************************
C  routine to compute the 8 hour max from array of hourly values           
C****************************************************************************
      Subroutine get8hourMax(values,sumMax,hourMax,missChk) 
      
      USE ENV_VARS

      Implicit None

      ! arguments
      Real values(*)
      Real sumMax
      Integer hourMax
      Logical missChk

      Integer i,j,count,tcount
      Real sum

      tcount = 0
      summax = -99.0
      hourMax = -99
      
      if ( HOURS_8HRMAX .eq. 24 ) then ! use 24 8hr values

       do i=1,24
        sum = 0
        count = 0
        do j=1,8
          if( values(i+j-1).ge.0.0 ) then
            count = count + 1
            sum = sum + values(i+j-1)
            endif
          enddo

        if( count .ge. 6 ) then
          tcount = tcount + 1
          sum = sum / count
          if( sum .gt. summax ) then
            summax = sum
            hourMax = i - 1
            endif
          Endif
        enddo

       if( missChk .and. tcount.lt.18 ) then !require 18/24
        summax = -99.0
        hourMax = -99
        endif

      else !use only 17 8hr values, from 7 am to 11 pm

       do i=8,24
        sum = 0
        count = 0
        do j=1,8
          if( values(i+j-1).ge.0.0 ) then
            count = count + 1
            sum = sum + values(i+j-1)
            endif
          enddo

        if( count .ge. 6 ) then
          tcount = tcount + 1
          sum = sum / count
          if( sum .gt. summax ) then
            summax = sum
            hourMax = i - 1
            endif
          Endif
        enddo

       if( missChk .and. tcount.lt.13 ) then !require 13/17
        summax = -99.0
        hourMax = -99
        endif
      
      
      endif

      return
      End Subroutine get8hourMax
        
 
C****************************************************************************
C  routine to compute the W126 value from array of hourly values           
C****************************************************************************
      Subroutine getW126(values, units, w126) 

      Implicit None

      ! arguments
      Real values(*)
      Character*(*) units
      Real w126   

      Integer i,count
      Real ozone
      Real sum
      Real factor
      Character*(10) lunits

      count = 0
      sum = 0.0
      w126 = -999.0
      factor = 1.0     ! units = ppm

      ! if units contain ppb, then set factor to 0.001
      lunits = TRIM(units)
      Call UCASE(lunits)
      if( INDEX(lunits,'PPB') .gt. 0 ) factor = 0.001

      do i=8,19    ! go from 8am to 7pm local time
        if( values(i).ge.0.0 ) then
          ozone = factor * values(i)
          count = count + 1
          sum = sum + ozone / (1.0 + 4403.0 * EXP( -126*ozone ))
          endif
        enddo

      if( count .ge. 9 ) then
        w126 = sum
        endif 

      return
      End Subroutine getW126
        

C****************************************************************************

C****************************************************************************
C  routine to compute the max hourly of all 9 cells
C****************************************************************************
      Subroutine getMax9(values, max9, hourMax)

      Implicit None

      ! arguments 
      Real values(32,9)
      Real max9
      Integer hourMax

      ! local variables
      Integer i,j
 
      max9 = 0.0
      hourMax = 0
      do i=1,24
        do j=1,9
          if( values(i,j).ge.max9 ) then
            max9 = values(i,j)
            hourMax = i - 1
            endif
          enddo                                                                                      
        enddo

      if( max9.le.0 ) then
        max9 = -999.0
        hourMax = -99
        endif
                                                                                                     
      return 

      End Subroutine getMax9


C****************************************************************************
C  routine to compute the 8 hour max for each 9 cells
C****************************************************************************
      Subroutine get8hrMax9(values, max9, hourMax, missChk)

      Implicit None

      ! arguments
      Real values(32,9)
      Real max9
      Integer hourMax
      Logical missChk

      ! local variables
      Integer i, hr
      Real cellMax

      max9 = -99.0
      do i=1,9
        Call get8hourMax(values(:,i), cellMax, hr, missChk)
        if( cellMax.ge.max9 ) then
          max9 = cellMax
          hourMax = hr
          endif
        enddo

      return
      End Subroutine get8hrMax9
 

C****************************************************************************
C  routine to convert date to string as "mm/dd/yyyy"
C****************************************************************************
      Character*16 Function date2Str( idate ) result(dateStr)

      Integer idate
      Integer status

C..  local variables
      Integer month, day, year

      call DayMon( idate, month, day )
      year = idate/1000

      write(dateStr,'(i2.2,''/'',i2.2,''/'',i4.4)',iostat=status) month,day,year
      return
      End Function date2Str

C****************************************************************************
C  routine to convert date and time to string as "MM,DD,YYYY"
C****************************************************************************

      Character*16 Function date2Str_csv( idate ) result(dateStr)

      Integer idate
      Integer status

C..  local variables
      Integer month, day, year

      call DayMon( idate, month, day )
      year = idate/1000

      write(dateStr,'(i2.2,'','',i2.2,'','',i4.4)',iostat=status) month,day,year
      return
      End Function date2Str_csv


C****************************************************************************
C  routine to convert real to string 
C****************************************************************************
      Character*16 Function real2Str( value, chk4miss ) result(realStr)
 
      USE ENV_VARS

      Real value   
      Logical chk4miss 
      Character*80 record
      Integer status

      if( chk4miss .and. value.lt.0.0 ) then 
        realStr = MISSING_VALUE
        Call LeftTrim(realStr)
        return
        endif

      Write(record,'(G14.5)',iostat=status) value
      Call LeftTrim(record)
      realStr = record
      return
      End Function real2Str
 
         
C****************************************************************************
C  routine to convert integer to string 
C****************************************************************************
      Character*16 Function int2Str( value, chk4miss ) result(intStr)

      USE ENV_VARS

      Implicit None

      Integer value   
      Logical chk4miss 
      Character*80 record
      Integer status

      if( chk4miss .and. value.lt.0 ) then 
        intStr = MISSING_VALUE
        Call LeftTrim(intStr)
        return
        endif

      Write(record,'(I15)',iostat=status) value
      Call LeftTrim(record)
      intStr = record
      return
      End Function int2Str
 
        
