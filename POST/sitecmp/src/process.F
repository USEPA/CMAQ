
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
! $Header: /home/sjr/cvs2git/TOOLS/src/sitecmp/process.F,v 1.5 2011/10/21 14:41:33 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C*************************************************************************
C
C  code for running the site compare process
C
C*************************************************************************
      SUBROUTINE PROCESS


      USE M3FILES
      USE ENV_VARS
      USE GRID_DATA
      USE TIME_STEP
      USE SITE_DATA
      USE SPECIES_DEF

      USE M3UTILIO

      IMPLICIT NONE     

C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS:
      INTEGER getNumberOfFields
      INTEGER str2Date
      INTEGER str2Time
      LOGICAL ISLOCAL
      Character*16 date2Str
      Character*16 date2Str_csv
      Real getSpeciesValue
      Character*16 real2Str

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*16    PNAME        ! Program Name
      CHARACTER*80    MSG          ! Error message
      CHARACTER*3200  RECORD       ! input buffer
      CHARACTER*3200  OUTRECORD    ! output buffer
      CHARACTER*3200  FIELD        ! field buffer  
      CHARACTER*3200  FIELD1       ! field buffer  
      CHARACTER*3200  FIELD2       ! field buffer  
      CHARACTER*16    FLAG         ! field code    
      CHARACTER*1     delimiter    ! field delimiter    


      INTEGER n, i, j, k, index, in, out, siteNo, status
      CHARACTER*16    UNITS         ! variable units
      CHARACTER*20    DATESTR       ! date string 
      CHARACTER*16    dfield, tfield
      CHARACTER*36    obsName        

      INTEGER startDate
      INTEGER startTime
      INTEGER endDate
      INTEGER endTime
      INTEGER siteField, pocField, sDateField, eDateField
      INTEGER istep1, istep2, TIMEZONE
      INTEGER stepsec, periodsec, neededSteps, nfields
      INTEGER SubPptFld
      Real    fieldValue
      Real    ppValue
      LOGICAL zeroPrecip
      LOGICAL WET_DEP

      REAL, ALLOCATABLE  :: VARDATA( : )

      LOGICAL DATACHECK
      LOGICAL DATECHECK
  
C**********************************************************************
      DATA PNAME / 'PROCESS' /

C****************************************************************
C  open input and output table files
C****************************************************************
      in = 10
      out = 11 
      open(unit=in, file=IN_TABLE, status='old', err=900)
      open(unit=out, file=OUT_TABLE, err=901)

C****************************************************************
C  read input table file and search for header line
C****************************************************************
      delimiter = ','
      ISLOCAL = .TRUE. 
      siteField = -1
      pocField  = -1
      sDateField = -1
      eDateField = -1

      ! read until record contains "site_id"
      Do 
        Read(in,'(a)', iostat=status) record
        call rmCommas(record)
        call ucase(record)
        if(status.ne.0) then
          Write(*,'(''Total data file read without finding a matching site_id'')')
          CALL M3ERR( PNAME, 0, 0,'No matching site records found in data file:'//IN_TABLE, .TRUE. ) 
          endif

        nfields = getNumberOfFields(record, delimiter)

        if( TABLE_TYPE .eq. 'CASTNET' ) then
          if( INDEX(record,'SITE_ID').gt.0 .AND. nfields.gt.3 ) then
            Do i=1,nfields
             call getField(record, delimiter, i, field)
             call rmQuots( field )
             call LeftTrim(field)
             if( Trim(field) .eq. 'SITE_ID') siteField = i
             if( Trim(field) .eq. 'POCODE') pocField = i
             if( Trim(field) .eq. 'DATEON') sdateField = i
             if( Trim(field) .eq. 'DATEOFF') edateField = i
             EndDo 
            EXIT
            EndIf

        else if( TABLE_TYPE .eq. 'SEARCH' ) then
          if( INDEX(record,'SITE_ID').gt.0 .AND. nfields.gt.3 ) then
            Do i=1,nfields
             call getField(record, delimiter, i, field)
             call rmQuots( field )
             call LeftTrim(field)
             if( Trim(field) .eq. 'SITE_ID') siteField = i
             if( Trim(field) .eq. 'POCODE') pocField = i
             if( Trim(field) .eq. 'DATEON') sdateField = i
             if( Trim(field) .eq. 'DATEOFF') edateField = i
             EndDo 
            EXIT
            EndIf

        else if( TABLE_TYPE .eq. 'IMPROVE' ) then
          if( INDEX(record,'SITE_CODE').gt.0 .AND. nfields.gt.2 ) then
            Do i=1,nfields
             call getField(record, delimiter, i, field)
             call rmQuots( field )
             call LeftTrim(field)
             if( Trim(field) .eq. 'SITE_CODE') siteField = i
             if( Trim(field) .eq. 'POCODE') pocField = i
             if( Trim(field) .eq. 'OBS_DATE') sdateField = i
             EndDo 
            Exit
            EndIf


        else if( TABLE_TYPE .eq. 'NADP' ) then
          if( INDEX(record,'SITEID').gt.0 .AND. nfields.gt.2 ) then
            Do i=1,nfields
             call getField(record, delimiter, i, field)
             call rmQuots( field )
             call LeftTrim(field)
             if( Trim(field) .eq. 'SITEID') siteField = i
             if( Trim(field) .eq. 'POCODE') pocField = i
             if( Trim(field).eq.'DATEON' .or. Trim(field).eq.'DATE ON') sdateField = i
             if( Trim(field).eq.'DATEOFF' .or. Trim(field).eq.'DATE OFF') edateField = i
             EndDo                                        
            Exit                                         
            EndIf    

        else if( TABLE_TYPE .eq. 'AIRMON' ) then
          if( INDEX(record,'SITE').gt.0 .AND. nfields.gt.4 ) then
            Do i=1,nfields
             call getField(record, delimiter, i, field)
             call rmQuots( field )
             call LeftTrim(field)
             if( Trim(field) .eq. 'SITE') siteField = i
             if( Trim(field) .eq. 'POCODE') pocField = i
             if( Trim(field) .eq. 'DATE/TIME ON') sdateField = i
             if( Trim(field) .eq. 'DATE/TIME OFF') edateField = i
             EndDo
            Exit
            EndIf

        else if( TABLE_TYPE .eq. 'STN' ) then
          if( INDEX(record,'SITE').gt.0 .AND. nfields.gt.2 ) then
            Do i=1,nfields                                                
             call getField(record, delimiter, i, field)                        
             call rmQuots( field )
             call LeftTrim(field)
             if( Trim(field) .eq. 'SITE') siteField = i       
             if( Trim(field) .eq. 'POCODE') pocField = i
             if( Trim(field) .eq. 'DATE') sdateField = i           
             EndDo                                                    
            Exit                                                     
            EndIf    

        else if( TABLE_TYPE .eq. 'MDN' ) then           
          if( INDEX(record,'SITEID').gt.0 ) then
            delimiter = ','
            ISLOCAL = .FALSE.
            Do i=1,nfields                                                
             call getField(record, delimiter, i, field)                        
             call rmQuots( field )
             call LeftTrim(field)
             if( Trim(field) .eq. 'SITEID') siteField = i       
             if( Trim(field) .eq. 'POCODE') pocField = i
             if( Trim(field) .eq. 'DATEON') sdateField = i           
             if( Trim(field) .eq. 'DATEOFF') edateField = i
             EndDo                                                    
            Exit                                                      
            EndIf                                                     

        else if( TABLE_TYPE .eq. 'MET' ) then
          if( INDEX(record,'SITE_ID').gt.0 ) then
            delimiter = ','
            ISLOCAL = .TRUE.
            Do i=1,nfields
             call getField(record, delimiter, i, field)
             call rmQuots( field )
             call LeftTrim(field)
             if( Trim(field) .eq. 'SITE_ID') siteField = i
             if( Trim(field) .eq. 'POCODE') pocField = i
             if( Trim(field) .eq. 'DATE_TIME') sdateField = i
             EndDo
            Exit
            EndIf
 
        else if( TABLE_TYPE .eq. 'DEARS' ) then
          if( INDEX(record,'PID').gt.0 ) then
            delimiter = ','
            ISLOCAL = .TRUE.
            Do i=1,nfields
             call getField(record, delimiter, i, field)
             call rmQuots( field )
             call LeftTrim(field)
             if( Trim(field) .eq. 'PID') siteField = i
             if( Trim(field) .eq. 'POCODE') pocField = i
             if( Trim(field) .eq. 'STARTDATE') sdateField = i
             EndDo
            Exit
            EndIf
 
        else if( TABLE_TYPE .eq. 'OUTPUT' ) then           
          if( INDEX(record,'SITEID').gt.0 .AND. nfields.gt.3 ) then
            Do i=1,nfields
             call getField(record, delimiter, i, field)
             call rmQuots( field )
             call LeftTrim(field)
             if( Trim(field) .eq. 'SITEID') siteField = i
             if( Trim(field) .eq. 'POCODE') pocField = i
             if( Trim(field) .eq. 'TIME ON') sdateField = i
             if( Trim(field) .eq. 'TIME OFF') edateField = i
             EndDo 
            EXIT
            EndIf
 
        else
          CALL M3ERR( PNAME, 0, 0,'Invalid Table type', .TRUE. )        
          Exit
        EndIf 
      EndDo
      

      ! check for missing site and date fields
      if( siteField.le.0 ) then
        CALL M3ERR( PNAME, 0, 0,'Cannot find Site field in header record', .TRUE. )
        endif
     
      if( sdateField.le.0 ) then
        CALL M3ERR( PNAME, 0, 0,'Cannot find Date fields in header record', .TRUE. )
        endif
     

C*********************************************************************
C*  determine field numbers for species variables from header record
C*********************************************************************    
      SubPptFld = 0
      WET_DEP = .FALSE.

      Do i=1,NSPECVAR

        ! use units of first model species for model units 
        if( SPECVARS(i)%MOD_UNITS .eq. '' ) then
          Call get_units( SPECVARS(i)%MOD_NAME(1), SPECVARS(i)%MOD_UNITS )
          Endif

        ! check for wet deposition operation
        if( SPECVARS(i)%OP_CODE .eq. 'WD' ) WET_DEP = .TRUE.

        !  check for null obs_name
        If(SPECVARS(i)%OBS_NAME(1).eq.'null' .or. SPECVARS(i)%OBS_NAME(1).eq.' ') then
          SPECVARS(i)%OBS_NUMSPEC = 0
          cycle
          Endif

        !  match OBS_NAME with header field
        Do j=nfields,1,-1
          call getField(record, delimiter, j, field)
          call rmQuots( field )
          call LeftTrim(field)
          Do k=1, SPECVARS(i)%OBS_NUMSPEC
            obsName = SPECVARS(i)%OBS_NAME(k)
            Call ucase( obsName )
            if( Trim(field) .eq. Trim(obsname) ) then
              SPECVARS(i)%OBS_FIELD(k) = j
              endif
            enddo 
            
          ! if field = "Sub Ppt", set SubPptFld value
          if( Trim(field) .eq. 'SUB PPT' ) SubPptFld = j
          Enddo 

        !  verify that all observered names exists 
        Do k=1, SPECVARS(i)%OBS_NUMSPEC
          if( SPECVARS(i)%OBS_FIELD(k) .eq. 0) then
            Write(*,'('' Observed Species ['',a,''] not found in header ['',a,'']'')')
     &        Trim(SPECVARS(i)%OBS_NAME(k)), Trim(record)
            CALL M3ERR( PNAME, 0, 0, 'Observed Speices ['//Trim(SPECVARS(i)%OBS_NAME(k))//'] not found', .TRUE. ) 
            Endif
          EndDo      
        EndDo

        if(WET_DEP .and. SubPptFld.eq.0) Then
          Write(*,'(''Error, WET_DEP requires precip field "Sub Ppt" which was not found'')')
          CALL M3ERR( PNAME, 0, 0, 'Observed Speices [Sub Ppt] not found', .TRUE. ) 
          Endif


C*********************************************************************
C*  build headers with variable names and units and write to output
C*********************************************************************
      if( TABLE_TYPE .eq. 'OUTPUT' ) then
        Call bldOPheader(in,out,record)
       else
        Call bldRegHeader(out)
       endif


C**********************************************************************
C*   Allocate memory to store data values for Variables
C**********************************************************************
      Allocate( VARDATA( NSPECVAR ) )

C**********************************************************************
C*   read each record, get site and time period
C**********************************************************************
      Do While(.true.)
        Read(in,'(a)', end=200) record
        if(LEN_TRIM(record).eq.0) CYCLE
        call rmCommas(record)
        call getField(record, delimiter, siteField, field1)
        if ( pocField .gt. 0 ) then
         call getField(record, delimiter, pocField, field2)
        else
         field2 = '1' !use 1 as default parameter occurrence code
        endif
        call rmQuots( field1 )
        call rmQuots( field2 )


        outRecord = trim(field1)//","//trim(field2)
        siteNo = getSiteNumber( field1, field2 )
        if( siteNo .gt. 0 ) Then

          if( TABLE_TYPE .eq. 'IMPROVE' ) then
            call getField(record, delimiter, sDateField, DateStr)
            call rmQuots( DateStr )
            startDate = str2Date( DateStr )
            startTime = 0
            endDate = startDate
            endTime = 235959
          else if( TABLE_TYPE .eq. 'CASTNET' ) then
            call getField(record, delimiter, sDateField, DateStr)
            call rmQuots( DateStr )
            call getField(DateStr, ' ', 1, dfield)
            call getField(DateStr, ' ', 2, tfield)
            startDate = str2Date(dfield)
            startTime = str2Time(tfield)
            call getField(record, delimiter, eDateField, DateStr)
            call rmQuots( DateStr )
            call getField(DateStr, ' ', 1, dfield)
            call getField(DateStr, ' ', 2, tfield)
            endDate = str2Date(dfield)
            endTime = str2Time(tfield)
          else if( TABLE_TYPE .eq. 'SEARCH' ) then
            call getField(record, delimiter, sDateField, DateStr)
            call rmQuots( DateStr )
            call getField(DateStr, ' ', 1, dfield)
            call getField(DateStr, ' ', 2, tfield)
            startDate = str2Date(dfield)
            startTime = str2Time(tfield)
            call getField(record, delimiter, eDateField, DateStr)
            call rmQuots( DateStr )
            call getField(DateStr, ' ', 1, dfield)
            call getField(DateStr, ' ', 2, tfield)
            endDate = str2Date(dfield)
            endTime = str2Time(tfield)
          else if( TABLE_TYPE .eq. 'STN' ) then
            call getField(record, delimiter, sDateField, DateStr)
            call rmQuots( DateStr )
            startDate = str2Date( DateStr ) 
            startTime = 000000     ! changed from 090000 on 02/24/2006
            endDate = startDate
            endTime = 235959   
          else if( TABLE_TYPE .eq. 'NADP' ) then
            call getField(record, delimiter, sDateField, DateStr)
            call rmQuots( DateStr )
            if(index(DateStr, ' ').gt.1 .and. index(DateStr, ':').gt.1) then
              call getField(DateStr, ' ', 1, dfield)
              call getField(DateStr, ' ', 2, tfield)
              startDate = str2Date(dfield)
              startTime = str2Time(tfield)
             else
              startDate = str2Date( DateStr )
              startTime = 90001
             endif

            call getField(record, delimiter, eDateField, DateStr)
            call rmQuots( DateStr )
            if(index(DateStr, ' ').gt.1 .and. index(DateStr, ':').gt.1) then
              call getField(DateStr, ' ', 1, dfield)
              call getField(DateStr, ' ', 2, tfield)
              endDate = str2Date(dfield)
              endTime = str2Time(tfield)
             else
              endDate = str2Date( DateStr )
              endTime = 90000
             endif
          else if( TABLE_TYPE .eq. 'AIRMON' ) then
            call getField(record, delimiter, sDateField, DateStr)
            call rmQuots( DateStr )
            if(index(DateStr, ' ').gt.1 .and. index(DateStr, ':').gt.1) then
              call getField(DateStr, ' ', 1, dfield)
              call getField(DateStr, ' ', 2, tfield)
              startDate = str2Date(dfield)
              startTime = str2Time(tfield)
             else
              startDate = str2Date( DateStr )
              startTime = 90000
             endif
 
            call getField(record, delimiter, eDateField, DateStr)
            call rmQuots( DateStr )
            if(index(DateStr, ' ').gt.1 .and. index(DateStr, ':').gt.1) then
              call getField(DateStr, ' ', 1, dfield)
              call getField(DateStr, ' ', 2, tfield)
              endDate = str2Date(dfield)
              endTime = str2Time(tfield)
             else
              endDate = str2Date( DateStr )
              endTime = 90000
             endif
            Call Nextime( endDate, endTime, -1 )
          else if( TABLE_TYPE .eq. 'MDN' ) then                   
            call getField(record, delimiter, sDateField, DateStr)     
            call rmQuots( DateStr )
            call getField(DateStr, ' ', 1, dfield)
            call getField(DateStr, ' ', 2, tfield)
            startDate = str2Date(dfield)
            startTime = str2Time(tfield)
            call getField(record, delimiter, eDateField, DateStr)     
            call rmQuots( DateStr )
            call getField(DateStr, ' ', 1, dfield)
            call getField(DateStr, ' ', 2, tfield)
            endDate = str2Date(dfield)
            endTime = str2Time(tfield)
          else if( TABLE_TYPE .eq. 'MET' ) then
            call getField(record, delimiter, sDateField, DateStr)
            call rmQuots( DateStr )
            call getField(DateStr, ' ', 1, dfield)
            call getField(DateStr, ' ', 2, tfield)
            startDate = str2Date(dfield)
            startTime = str2Time(tfield)
            endDate = startDate
            endTime = startTime
            CALL NEXTIME( endDate, endTime, 5900 )
          else if( TABLE_TYPE .eq. 'DEARS' ) then
            call getField(record, delimiter, sDateField, DateStr)
            call rmQuots( DateStr )
            startDate = str2Date(DateStr)
            startTime = 090000
            endDate = startDate
            endTime = startTime
            CALL NEXTIME( endDate, endTime, 235900 )
          else if( TABLE_TYPE .eq. 'OUTPUT' ) then
            call getField(record, delimiter, sDateField, DateStr)
            call rmQuots( DateStr )
            call getField(DateStr, ' ', 1, dfield)
            call getField(DateStr, ' ', 2, tfield)
            startDate = str2Date(dfield)
            startTime = str2Time(tfield)
 
            call getField(record, delimiter, eDateField, DateStr)
            call rmQuots( DateStr )
            call getField(DateStr, ' ', 1, dfield)                      
            call getField(DateStr, ' ', 2, tfield)                      
            endDate = str2Date(dfield)
            endTime = str2Time(tfield)   
          else
            Write(*,'(''Error, Invalid table type:'', a)') TRIM(TABLE_TYPE)
            Stop
          Endif

          !  add state to outRecord
          field = STATE(siteno)
          Call LeftTrim(field)
          outRecord = Trim(outRecord) // ',"' // trim(field) // '"'

          !  add county to outRecord
          field = COUNTY(siteno)
          Call LeftTrim(field)
          outRecord = Trim(outRecord) // ',"' // trim(field) // '"'

          !  add elevation to outRecord
          Write(field,'(f7.0)') ELEV(siteno)
          Call LeftTrim(field)
          outRecord = Trim(outRecord) // ',' // trim(field)

          !  add lat and long fields to outRecord
          Write(field,'(f12.4)') getLatitude(siteno)
          Call LeftTrim(field)
          outRecord = Trim(outRecord) // ',' // trim(field)

          Write(field,'(f12.4)') getLongitude(siteno)                                                       
          Call LeftTrim(field) 
          outRecord = Trim(outRecord) // ',' // trim(field)

          !  add cell column and row to outRecord
          Write(field,'(i5)') COL(siteno)
          Call LeftTrim(field)
          outRecord = Trim(outRecord) // ',' // trim(field)

          Write(field,'(i5)') ROW(siteno)
          Call LeftTrim(field)
          outRecord = Trim(outRecord) // ',' // trim(field)


          !  add date/time fields to outRecord
          outRecord = Trim(outRecord) // ',' // date2Str(startDate,startTime)
          outRecord = Trim(outRecord) // ',' // date2Str(endDate,endTime)

          !  add date/time fields to outRecord in MM,DD,YYYY,HH,SS format
          outRecord = Trim(outRecord) // ',' // date2Str_csv(startDate,startTime)
          outRecord = Trim(outRecord) // ',' // date2Str_csv(endDate,endTime)

          !  adjust time to GMT if ISLOCAL
          if( ISLOCAL ) then
            ! adjust timezone for day light saving only if APPLYDLS is true
            TIMEZONE = TZ(siteNo)
            if( APPLYDLS .AND. ISDSTIME( startDate ) ) TIMEZONE = TIMEZONE-1

            ! adjust time by TIME_SHIFT (default is no shift)
            TIMEZONE = TIMEZONE + TIME_SHIFT

            ! adjust starting date and time for timezone
            CALL NEXTIME( startDate, startTime, 10000*TIMEZONE )
            CALL NEXTIME( endDate, endTime, 10000*TIMEZONE )
            endif

          DATACHECK = .TRUE.

          ! check if site is outside of grid domain
          if( COL(siteno).eq.0 .or. ROW(siteno).eq.0 ) DATACHECK = .FALSE.

          ! check if dates are within time window
          if( START_DATE .gt. 0 ) Then
            if(SECSDIFF(START_DATE, START_TIME, startDate, startTime).lt.0)
     &          DATACHECK = .FALSE.
            Endif

          if( END_DATE .gt. 0 ) Then
            if(SECSDIFF(END_DATE, END_TIME, startDate, startTime).gt.0)
     &          DATACHECK = .FALSE.
            Endif

          if( DATACHECK ) then 
            Call startEndSteps(startDate, startTime, endDate, endTime, istep1, istep2) 
            DATECHECK = .TRUE.
            if( istep1.lt.0 .or.istep2.lt.0 ) DATECHECK = .FALSE.
            Endif

          if( DATACHECK .and. DATECHECK ) then
            stepsec = TIME2SEC( TIMESTEP )
            periodsec = SECSDIFF( startDate, startTime, endDate, endTime )
            neededSteps = periodsec / stepsec
            if( neededSteps .gt. istep2-istep1+1 ) DATACHECK = .FALSE.
            EndIF
 
          ! Read values for each variable and compute it's period value
          if( DATACHECK ) then
            VARDATA = -999.0   ! init to missing
            if( DATECHECK ) then 
              Do i=1,NSPECVAR
                VARDATA(i) = getSpeciesValue(siteNo, SPECVARS(i), istep1, istep2)
                EndDo          
              EndIf
            Endif

           ! build output record and write to out Table
           if( DATACHECK ) then
             
             ! set flag for zero precip
             zeroPrecip = .false.
             if( SubPptFld.gt.0 ) then
               call getField(record, delimiter, SubPptFld, field)
               call rmQuots( field )
               zeroPrecip = ( Trim(field) .eq. '0' ) 
               Endif
             
             Do i=1,NSPECVAR
               if( SPECVARS(i)%OBS_NUMSPEC .gt. 0 ) Then

                 if( SPECVARS(i)%OP_CODE.eq.'CH' ) Then
                   Call getField(record, delimiter, SPECVARS(i)%OBS_FIELD(1), field)
                   Call rmQuots( field )
                   Endif

                 if( SPECVARS(i)%OP_CODE.ne.'CH' ) Then
                   Call getObsField(record, delimiter, TABLE_TYPE, SPECVARS(i), fieldvalue)
                   if(fieldvalue.le.-999.0) then
                     field = MISSING_VALUE
                    else
                     field = Real2Str(fieldValue) 
                     Endif
                   Endif 

                 ! check for WET_DEP operation
                 if(SPECVARS(i)%OP_CODE.eq.'WD') then
                   if( fieldvalue.lt.0.0 ) field = MISSING_VALUE
                   if( fieldvalue.ge.0.0 ) then
                     Call getField(record, delimiter, SubPptFld, field)
                     read(field,'(g16.0)',iostat=status) ppvalue
                     if( status.ne.0 .or. ppvalue.lt.0 ) ppvalue = 0.0
                     fieldvalue = fieldvalue * ppvalue / 100.0
                     field = Real2Str(fieldValue)
                     Endif
                   Endif 

                 outRecord = Trim(outRecord) // ',' // Trim(field)
                 Endif

               if( SPECVARS(i)%Mod_NUMSPEC .gt. 0 ) Then 
                 field = Real2Str(VARDATA(i))
                 outRecord = Trim(outRecord) // ',' // Trim(field)
                 Endif
               EndDo

             ! print status line to screen
             Write(*,'(''Writing values for site '',a,'' starting at '',a,'' GMT'')') 
     &                 TRIM(SITE(siteNo)), DT2STR(startDate, startTime)

             ! write output record to out table
             Write(out,'(a)') TRIM(outRecord)
           EndIf            
        Else
          Write(*,'(''No site found for site = '',a)') TRIM(field1)
          !!Write(*,'(''  field'',i3,'' in record = '',a)') siteField,TRIM(record)
        EndIf 

      EndDo
  200 continue

      RETURN

  900 write(*,'('' Cannot open input table file ['',a,'']'')') TRIM(IN_TABLE)
      return

  901 write(*,'('' Cannot open output table file ['',a,'']'')') TRIM(OUT_TABLE)
      return
      END




C****************************************************************************
C  routine to build and write header lines for all TABLE_TYPEs != OUTPUT
C****************************************************************************
      Subroutine bldRegheader(out)

      USE M3FILES
      USE SPECIES_DEF 
      USE ENV_VARS

      ! arguments
      Integer out

      ! local variables
      CHARACTER*(80)   obsTitle
      CHARACTER*(80)   modTitle
      Character*(3200) header1
      Character*(3200) header2
      Character*(3200) header3

      header1 = 'SiteId,POCode,State,County,Elevation,Latitude,Longitude'
      header1 = Trim(header1) // ',Column,Row,Time On,Time Off'
      header1 = Trim(header1) // ',SMM,SDD,SYYYY,Shh,Smm,EMM,EDD,EYYYY,Ehh,Emm'
      header2 = ',,,,(m),(deg),(deg)'
      header2 = Trim(header2) // ',,,(MM/DD/YYYY hh:mm),(MM/DD/YYYY hh:mm)'
      header2 = Trim(header2) // ',MM,DD,YYYY,hh,mm,MM,DD,YYYY,hh,mm'
      header3 = ',,,,,,'
      header3 = Trim(header3) // ',,,,'
      header3 = Trim(header3) // ',,,,,,,,,,'
 
      Do i=1,NSPECVAR
        if( SPECVARS(i)%OBS_NUMSPEC .gt. 0 ) Then
          obsTitle = SPECVARS(i)%OBS_Expression
          header1 = Trim(header1) // ',' // TRIM(obsTitle) // '_ob'
          header2 = Trim(header2) // ',' // TRIM(SPECVARS(i)%OBS_UNITS)
          header3 = Trim(header3) // ',' // 'Observed'
          Endif
       
        if( SPECVARS(i)%Mod_NUMSPEC .gt. 0 ) Then
          modTitle = SPECVARS(i)%MOD_EXPRESSION
          header1 = Trim(header1) // ',' // TRIM(modTitle) // '_mod'
          header2 = Trim(header2) // ',' // TRIM(SPECVARS(i)%MOD_UNITS)
          header3 = Trim(header3) // ',' // 'Modeled'
          Endif
 
        EndDo
 
      Write(out,'(''Observed values read from file:'',a)') TRIM(IN_TABLE)
      Write(out,'(''Modeled values read from file:'',a,/)') TRIM(M3FILE)
      Write(out,'(a)') TRIM(header2)
      Write(out,'(a)') TRIM(header3)
      Write(out,'(a)') TRIM(header1)
 

      return
      end

                                                                    
C****************************************************************************
C  routine to build and write header lines for TABLE_TYPE OUTPUT
C****************************************************************************         
      Subroutine bldOPheader(in,out,record)                               
                                                                        
      USE M3FILES
      USE SPECIES_DEF                                                   
      USE ENV_VARS
                                                                        
      ! arguments                                                       
      Integer in, out                                                       
      Character*(*) record
                      
      ! local variables
      CHARACTER*(80)   obsTitle
      CHARACTER*(80)   modTitle
      Character*(3200) header1
      Character*(3200) header2
      Character*(3200) header3
      Character*(3200) unitsRec 
      Character*(3200) sourceRec 
      Character*(80)   field

      TYPE ( SPEC_VAR ) newSpecs(50)
      INTEGER nspecs   


      header1 = 'SiteId,POCode,Latitude,Longitude,Column,Row,Time On,Time Off' 
      header1 = Trim(header1) // ',SMM,SDD,SYYYY,Shh,Smm,EMM,EDD,EYYYY,Ehh,Emm'
      header2 = ',,(deg),(deg),,,(MM/DD/YYYY hh:mm),(MM/DD/YYYY hh:mm)'  
      header2 = Trim(header2) // ',MM,DD,YYYY,hh,mm,MM,DD,YYYY,hh,mm'
      header3 = ',,,,,,,'                                                
      header3 = Trim(header3) // ',,,,,,,,,,'
                 
      !  read units header line
      read(in,'(a)') unitsRec

      !  read observed, modeled line
      read(in,'(a)') sourceRec


      ! build new set of species variables
      nspecs = 0
      Do i=1,NSPECVAR
        nspecs = nspecs + 1
        newSpecs(nspecs) = SPECVARS(i)

        ! check for no old modeled field
        call getField(sourceRec, ',', SPECVARS(i)%OBS_FIELD(1)+1, field)
        if( field.ne.'Modeled' ) CYCLE

        ! remove mod field from spec and add new spec using old modeled and new modeled
        newSpecs(nspecs)%MOD_NUMSPEC = 0

        nspecs = nspecs + 1
        call getField(record, ',', SPECVARS(i)%OBS_FIELD(1)+1, field)
        newSpecs(nspecs) = SPECVARS(i)
        newSpecs(nspecs)%OBS_NUMSPEC = 1
        newSpecs(nspecs)%OBS_EXPRESSION = field
        newSpecs(nspecs)%OBS_NAME(1) = field
        newSpecs(nspecs)%OBS_FIELD(1) = SPECVARS(i)%OBS_FIELD(1)+1
        newSpecs(nspecs)%OBS_FACTOR = 1.0
        call getField(unitsRec, ',', SPECVARS(i)%OBS_FIELD(1)+1, field)
        newSpecs(nspecs)%OBS_UNITS = field                 

        enddo

      ! replace SPECVARS with new specs
      NSPECVAR = nspecs
      Do i=1,nspecs
        SPECVARS(i) = newSpecs(i)
        enddo

      !  build header records with new species
      Do i=1,NSPECVAR                                                   
        if( SPECVARS(i)%OBS_NUMSPEC .gt. 0 ) Then
          obsTitle = SPECVARS(i)%OBS_Expression
          call getField(sourceRec, ',', SPECVARS(i)%OBS_FIELD(1), field)
          if( field.eq.'Modeled' ) field = trim(field) // '(A)'
          header1 = Trim(header1) // ',' // TRIM(obsTitle) // '_ob'
          header2 = Trim(header2) // ',' // TRIM(SPECVARS(i)%OBS_UNITS)
          header3 = Trim(header3) // ',' // trim(field)
          Endif
                                                                        
        if( SPECVARS(i)%Mod_NUMSPEC .gt. 0 ) Then                       
          modTitle = SPECVARS(i)%MOD_EXPRESSION                         
          header1 = Trim(header1) // ',' // TRIM(modTitle) // '_mod'              
          header2 = Trim(header2) // ',' // TRIM(SPECVARS(i)%MOD_UNITS) 
          header3 = Trim(header3) // ',' // 'Modeled(B)'
          Endif
 
        EndDo
 
      Write(out,'(''Observed values read from file:'',a)') TRIM(IN_TABLE)
      Write(out,'(''Modeled values read from file:'',a,/)') TRIM(M3FILE)
      Write(out,'(a)') TRIM(header2)
      Write(out,'(a)') TRIM(header3)                                    
      Write(out,'(a)') TRIM(header1)
 
      return                                                            
      end                                                               
                                                                        
        
C****************************************************************************
C  routine to read and compute observed values from input record
C****************************************************************************
      Subroutine getObsField(record, delimiter, TABLE_TYPE, VAR, resultValue)

      USE SPECIES_DEF 

      ! arguments
      Character*(*) record
      Character*(*) delimiter
      Character*(*) TABLE_TYPE
      TYPE (SPEC_VAR) VAR
      Real resultValue

      ! local variables
      Integer n, status 
      Real fieldValue
      Character*(32) field
      Character*(32) flag


      resultValue = 0

      Do n=1,VAR%OBS_NUMSPEC
        
        Call getField(record, delimiter, VAR%OBS_FIELD(n), field)
        Call rmQuots( field )

        ! check for a code flag field following data field 
        if(TABLE_TYPE .eq. 'CASTNET' .or. TABLE_TYPE .eq. 'SEARCH') then
          Call getField(record, delimiter, VAR%OBS_FIELD(n)+1, flag)
          Call rmQuots( flag )
          if( INDEX('#IJKLMNBCDFP', flag(1:1)) .gt. 0 ) field = ' '
          endif

        ! check for a code flag field following data field
        if(TABLE_TYPE .eq. 'MET') then
          Call getField(record, delimiter, VAR%OBS_FIELD(n)+1, flag)
          Call rmQuots( flag )
          if( LEN_TRIM(flag).eq.1 .and. INDEX('BCDFIMP',flag(1:1)).gt.0 ) field = ' '
          endif

        !  read fieldValue and check if missing
        read(field,'(g16.0)',iostat=status) fieldValue
        if(status.ne.0.or.TRIM(field).eq.' '.or.TRIM(field).eq.'.'.or.fieldvalue.le.-999.0) then
          if( .NOT.VAR%OBS_OPTNAL(n) ) then
            resultValue = -999.0
            Return
            endif   
          fieldValue = 0.0    ! if missing and optional, set to zero
          Endif               ! missing condition

        ! update resultValue 
        resultValue = resultValue + VAR%OBS_FACTOR(n) * fieldValue
        Enddo

      return
      End Subroutine getObsField



C****************************************************************************
C  routine to convert date string "yyyy-mm-dd" to an Integer value yyyyDDD
C****************************************************************************
      Integer Function str2Date( dString ) result(IDATE)

      Character*(*)  dString

C..EXTERNAL FUNCTIONS:
      INTEGER JULIAN

      Character*10  monStr
      Character*10  dayStr
      Character*10  yrStr

      Integer mo, dy, yr, jday
      Integer i,nchar

      ! remove quote marks
      Call rmQuots( dString )

      if( index(dString,'-') .gt. 0 ) then           ! yyyy-mm-dd
        Call getField(dString,'-',1,yrStr)
        Call getField(dString,'-',2,monStr)
        Call getField(dString,'-',3,dayStr)
      elseif( index(dString,'/') .gt. 0 ) then           ! mm/dd/yyyy
        Call getField(dString,'/',3,yrStr)
        Call getField(dString,'/',1,monStr)
        Call getField(dString,'/',2,dayStr)
      else                                           !yyyymmdd
        yrStr = dString(1:4)
        monStr = dString(5:6)
        dayStr = dString(7:8)
        Endif

      Read(monStr,'(i10)',err=500) mo
      Read(dayStr,'(i10)',err=500) dy
      Read(yrStr,'(i10)',err=500) yr

      ! check for 2 digit year
      if( yr.gt.0 .and. yr.lt.100 ) Then
        if( yr.ge.50 ) yr = yr + 1900
        if( yr.lt.50 ) yr = yr + 2000
        Endif

      jday = JULIAN( yr, mo, dy )

      IDATE = 1000*yr + jday
      Return

  500 Write(*,'(''Error converting date string '',a)') TRIM(dString)
      IDATE = 9999999
      Return
      End Function str2Date


C****************************************************************************
C  routine to convert time string "HH:MM" or "HH:MM:SS" to an Integer value HHMMSS
C****************************************************************************
      Integer Function str2Time( timeStr ) result(ITIME)

      Character*(*) timeStr
      Character*10  tString

      Character*10  hhStr
      Character*10  mmStr
      Character*10  ssStr

      Integer hh, mm, ss
      Integer i,nchar

      nchar = min(LEN(timeStr), 10 )
      
      Do i=1,nchar
        tString(i:i) = timeStr(i:i)
      EndDo
           
      Call getField(tString,':',1,hhStr)
      Call getField(tString,':',2,mmStr)
      Call getField(tString,':',3,ssStr)

      Read(hhStr,'(i10)',err=500) hh
      if( hh.gt.24 ) go to 500

      Read(mmStr,'(i2)',err=501) mm
      if( mm.lt.0 .or. mm.gt.59 ) go to 501

      Read(ssStr,'(i2)',err=502) ss
      if( ss.lt.0 .or. ss.gt.59 ) go to 502

      ITIME = 10000 * hh + 100 * mm + ss
      Return

  500 Write(*,'(''Error converting hour string '',a)') hhStr
      ITIME = 0
      Return
  501 Write(*,'(''Error converting minute string '',a)') mmStr
      ITIME = 0
      Return
  502 Write(*,'(''Error converting seconds string '',a)') ssStr
      ITIME = 0
      Return

      End Function str2Time


C****************************************************************************
C  routine to convert date and time to string as "mm/dd/yyyy HH:MM"
C****************************************************************************
      Character*16 Function date2Str( date, time ) result(dateStr)

      Integer date, time

C..  local variables
      Integer month, day, year, hour, minutes

      call DayMon( date, month, day )
      year = date/1000
 
      hour = time/10000
      minutes = (time - 10000*hour)/100

      write(dateStr,'(i2.2,''/'',i2.2,''/'',i4.4,'' '',i2.2,'':'',i2.2)') 
     &      month,day,year,hour,minutes
      return
      End Function date2Str

C****************************************************************************
C  routine to convert date and time to string as "MM,DD,YYYY,HH,MM"
C****************************************************************************
      Character*16 Function date2Str_csv( date, time ) result(dateStr)

      Integer date, time

C..  local variables
      Integer month, day, year, hour, minutes

      call DayMon( date, month, day )
      year = date/1000
 
      hour = time/10000
      minutes = (time - 10000*hour)/100

      write(dateStr,'(i2.2,'','',i2.2,'','',i4.4,'','',i2.2,'','',i2.2)')
     &      month,day,year,hour,minutes
      return
      End Function date2Str_csv


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
      Real Function getSpeciesValue(siteNo, Species, istep1, istep2)
     &    result(value)

      USE ENV_VARS
      USE SITE_DATA
      USE SPECIES_DEF

      IMPLICIT NONE     

C..ARGUMENTS:
      INTEGER siteNo
      TYPE ( SPEC_VAR ) Species
      INTEGER istep1, istep2

C..Local variables
      Real, Allocatable  :: specValues(:)
      Real, Allocatable  :: RTValues(:)
      Real*8             :: RTtotal
      Real*8 specValue1
      Real*8 specValue2
      Character*16       :: PrecipUnits
      Integer nvalues
      Integer n, i, nm, ns

C..  allocate array to store values read from ioapi files
      nvalues = istep2 - istep1 + 1
      Allocate( specValues(nvalues) )

C.. if OP_CODE.eq.'WC', then read RT values for volumn-weighted calculations
      if( Species%OP_CODE.eq.'WC' ) then
        Allocate( RTValues(nvalues) )
        Call getValues(siteNo, PRECIP_FIELD, istep1, istep2, RTValues)

        RTtotal = 0.0
        Do i=1,nvalues 
         if (RTValues(i) .ge. 0.) then
          RTtotal = RTtotal + RTValues(i)
         endif
        EndDo 

        ! if units of Precip == mm, adjust total to cm
        Call get_units( PRECIP_FIELD, PrecipUnits )
        if( PrecipUnits .eq. 'mm' ) then
          RTtotal = RTtotal /10.0
          EndIf
        Endif


C.. initialize specValue1 (sum over time) and specValue2 (sum over specs) to zero
      specValue1 = 0.0
      specValue2 = 0.0
      ns = 0

C.. read each model species from file and update values
      Do n=1,species%Mod_NUMSPEC
        Call getValues(siteNo, species%MOD_NAME(n), istep1, istep2, specValues)
        
        nm = 0
        specValue1 = 0.0

        ! update values array
        Do i=1,nvalues 
        
          if ( specValues(i) .ne. -999. ) then
          
           nm = nm + 1
          
           ! adjust value by factor
           specValues(i) = specValues(i) * species%MOD_FACTOR(n)

           ! perform operation defined by Species%OP_CODE 
           if( Species%OP_CODE .eq. 'AV' ) then
             specValues(i) = specValues(i)

           ElseIf( Species%OP_CODE .eq. 'AC' ) then
             specValues(i) = specValues(i)

           ElseIf( Species%OP_CODE .eq. 'WD' ) then
             specValues(i) = specValues(i)

           ElseIf( Species%OP_CODE .eq. 'WC' ) then
            if( RTtotal.gt.0.001 ) Then
              specValues(i) = specValues(i) * 10.0 / RTtotal
            Else
              specValues(i) = 0.0
            Endif

           Endif

           specValue1 = specValue1 + specValues(i) 
           
          endif !specValues(i) not missing
          
        EndDo !loop over time steps
        
        if ( nm .ge. 1 ) then
        
           if( Species%OP_CODE .eq. 'AV' ) then
             specValue1 = specValue1 / float(nm)
           endif        
           
        else
        
          specValue1 = -999.
        
        endif
        
        if (specValue1 .ne. -999.) then
        
         specValue2 = specValue2 + specValue1
         ns = ns + 1
         
        endif
      
      EndDo !loop over species

      if (ns .eq. 0 ) specValue2 = -999. !no non-missing values found for any species
      
      value = specValue2
      
      Return
      End Function getSpeciesValue 



C****************************************************************************
C  routine to read values from files at site for variable for time period
C****************************************************************************
      SUBROUTINE getValues(siteNo, VARNAME, istep1, istep2, VALUES)

      USE M3FILES
      USE ENV_VARS
      USE GRID_DATA
      USE TIME_STEP
      USE SITE_DATA
      USE SPECIES_DEF

      USE M3UTILIO

      IMPLICIT NONE     

C..ARGUMENTS:
      INTEGER siteNo
      Character*(*) VARNAME
      INTEGER istep1, istep2
      REAL VALUES(*)

C..SCRATCH LOCAL VARIABLES:
      INTEGER   N, I, K, KK, S, ALLOCSTAT
      CHARACTER*16    PNAME        ! Program Name
      CHARACTER*80    MSG          ! Error message
      REAL, Allocatable, Save :: GRIDVAL(:,:)
      REAL, Allocatable, Save :: SITESPECVAL(:,:,:)
      INTEGER, Save :: NMODSPEC_ALL
      CHARACTER(len=16), Save :: MODSPEC_ALL(100)
      LOGICAL, Save :: LFIRST 
      LOGICAL, Save :: LFAST 

      DATA LFIRST / .true. /
      DATA PNAME / 'SITE_EXTRACT' /


      ! on first time called, allocate memory for GRIDVAL array
      if ( LFIRST ) then

        Allocate ( GRIDVAL( M3GRID % NCOLS, M3GRID % NROWS ) )
        LFIRST = .false.
      
        ! first, need to build a masterlist of all needed model species
        ! the master list is stored in MODSPEC_ALL
        ! these species are then read in for each site and time step
        ! and stored in SITESPECVAL
      
        NMODSPEC_ALL = 0
        do n = 1, 100 !initialize the array of model species to blank
         MODSPEC_ALL(n) = '                '
        enddo !n
        
        do n = 1, NSPECVAR !total number of species expressions from module SPECIES_DEF
         do k = 1, SPECVARS(n)%MOD_NUMSPEC !number of model species for this expression
         
          if (NMODSPEC_ALL .eq. 0) then !no elements in master list yet
          
           NMODSPEC_ALL = NMODSPEC_ALL + 1
           MODSPEC_ALL(NMODSPEC_ALL) = SPECVARS(n)%MOD_NAME(k)(1:16)
           
          else
          
           if (INDEX1 (SPECVARS(n)%MOD_NAME(k)(1:16), NMODSPEC_ALL,
     &                 MODSPEC_ALL) .eq. 0) then !species not found, add to master list
     


            NMODSPEC_ALL = NMODSPEC_ALL + 1
            MODSPEC_ALL(NMODSPEC_ALL) = SPECVARS(n)%MOD_NAME(k)(1:16)
           
           endif
                     
          endif 
          
         enddo !k
        
        enddo !n
        
c       try to add the precipitation variable to the list if available
c       in file and not already part of the master list

        IF ( INDEX1 (PRECIP_FIELD(1:16), NMODSPEC_ALL,
     &                 MODSPEC_ALL) .eq. 0) THEN !precip variable not found, check in file        

         IF ( DESC3( M3_FLNAME( 1 ) ) ) THEN

          IF (INDEX1 (PRECIP_FIELD(1:16), NVARS3D,
     &                 VNAME3D) .ne. 0) THEN !precip variable found, add to master list
           NMODSPEC_ALL = NMODSPEC_ALL + 1
           MODSPEC_ALL(NMODSPEC_ALL) = PRECIP_FIELD(1:16)
                      
          ENDIF
         
         ENDIF
         
        ENDIF
        
        Write(*,'(''master list of required model species:'')')
        do k = 1, NMODSPEC_ALL
         Write(*,'(i0,1x,a)') k,MODSPEC_ALL(k)
        enddo !k
      
        Allocate ( SITESPECVAL( NSITES, NSTEPS , NMODSPEC_ALL ), 
     *             STAT = ALLOCSTAT)
        
        if ( ALLOCSTAT .eq. 0 ) then !enough memory for fast approach
        
         lfast = .true.
         Write(*,'(''enough memory, using data prefetch approach'')')
         Write(*,'(''reading model values for all sites and time steps'')')
        
         SITESPECVAL = -999.        

         do N = 1, NSTEPS !number of time steps across all model files from module_tstep

          Write(*,'(''Reading model data for '',i0,1x,i0)') 
     &                 STEP_DATE(N), STEP_TIME(N)

          do K = 1, NMODSPEC_ALL
          
           
         
           IF( .NOT. READ3( M3_FLNAME(STEP_FILE(N)), MODSPEC_ALL (k), 
     &                   1, STEP_DATE(N), 
     &                   STEP_TIME(N), GRIDVAL ) ) THEN
     
            MSG = 'Could not read input Models-3 file ' // M3_FLNAME(STEP_FILE(N))        
            CALL M3ERR( PNAME, STEP_DATE(N), STEP_TIME(N), MSG, .FALSE. )
          
           ELSE
         
            do S = 1, NSITES

             if ( ( ROW(S) .ne. 0 ) .and. ( COL(S) .ne. 0 ) ) then
           
              SITESPECVAL(S,N,K) = GRIDVAL( COL(S), ROW(S) )
            
             endif ! site within domain

            enddo !S
         
           ENDIF
          
          enddo !K
        
         enddo !N
         
         Write(*,'(''finished reading model values'')')

        else ! not enough memory for fast approach
        
         lfast = .false.
         Write(*,'(''not enough memory, not using data prefetch approach'')')
         
        endif

      EndIf !Lfirst

c     return the values for argument VARNAME at site siteno
c     between istep1 and istep2
c     use fast or slow approach depending on memory

      if (lfast) then !enough memory for fast approach

       k = INDEX1 (VARNAME(1:16), NMODSPEC_ALL, MODSPEC_ALL)

       ! loop to read each value in time period
       I = 0
       Do N=istep1, istep2
        I = I + 1
     
         VALUES(I) = SITESPECVAL(siteno,n,k)

       enddo     ! N

      else !not enough memory for fast approach, use old approach
      
       ! loop to read each value in time period
       I = 0
       Do N=istep1, istep2
        I = I + 1
     
        IF( .NOT. READ3( M3_FLNAME(STEP_FILE(N)), VARNAME, 1, STEP_DATE(N), 
     &                   STEP_TIME(N), GRIDVAL ) ) THEN
          MSG = 'Could not read input Models-3 file ' // M3_FLNAME(STEP_FILE(N))        
          CALL M3ERR( PNAME, STEP_DATE(N), STEP_TIME(N), MSG, .FALSE. )
          GRIDVAL = -999.   
          ENDIF
        
        VALUES(I) = GRIDVAL( COL(siteNo), ROW(siteNo) )        

       EndDo
      
      
      endif

      Return
      End SUBROUTINE getValues 


 
C****************************************************************************
C  routine to convert real to string
C****************************************************************************
      Character*16 Function real2Str( value ) result(realStr)
 
      IMPLICIT NONE
 
      ! argument
      Real value
 
      ! local variables
      Character*16 MISSING_VALUE
      Character*80 record
      Integer status
 
      MISSING_VALUE = '-999'
 
      if( value.gt.-999.001 .AND. value.lt.-998.999 ) then
        realStr = MISSING_VALUE
        Call LeftTrim(realStr)
        return
        endif
 
      Write(record,'(E16.5)',iostat=status) value
      Call LeftTrim(record)
      realStr = record
      return
      End Function real2Str
 
       
