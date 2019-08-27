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

      PROGRAM O3TOIOAPI
      
C**********************************************************************
C
C  This program generates a site IOAPI file from AIRS or CASTNET OZONE
C
C
C***********************************************************************

c  output data description
C
C    hourly values are in GMT time
C    daily averages are in local time
C    daily 1 hour maximums are in local time
C    daily 8 hour maximums are in local time
C
C
C
      USE M3UTILIO
      USE SITE_DATA

      IMPLICIT NONE

C...define local variables
      Character*(256)  INFILE
      Character*(24)   DATFILE
      Character*(10)   FILETYPE
      Character*(10)   OLAYTYPE
      Integer status, logdev, lfn
      Integer SDATE, EDATE
      Integer i, n, iDate, iHr, curDate, curHr  
      integer id  

      Character*(10) :: otypes(4)
      Integer :: tsteps(4)
      Integer :: otype

      Character*(16) vdesc
      Integer numvar

      Integer, Allocatable :: station(:)
      Real, Allocatable :: longitude(:)
      Real, Allocatable :: latitude(:)
      Real, Allocatable :: dataValue(:,:)
      Real, Allocatable :: iValues(:)

      LOGICAL KSWIT   
   
      Data lfn /15/
      Data otypes/ 'HOURLY','DAILY','1HRMAX','8HRMAX' /
      Data tsteps/ 10000, 240000, 240000, 240000 /

C...start IOAPI
      LOGDEV = INIT3()

C...get name of input file
      CALL ENVSTR( 'INFILE', 'Input filename', 'castnet.txt', INFILE, STATUS)

C...get input file type
      CALL ENVSTR( 'FILETYPE', 'input file type', 'OBS', FILETYPE, STATUS)

C...get overlay type (hourly, daily, 1hrmax, 8hrmax)
      otype = 1
      if(FILETYPE.eq.'AIRS' .or. FILETYPE.eq.'CASTNET' .or. FILETYPE.eq.'OBS') then
        CALL ENVSTR( 'OLAYTYPE', 'overlay type', 'HOURLY', OLAYTYPE, STATUS)
        Call Upper(OLAYTYPE)
        otype = INDEX1(OLAYTYPE, SIZE(otypes), otypes)
        if( otype.le.0 ) then
          Write(*,'(''Invalid overlay type, type must be one of the following'')')
          Write(*,'(3x,a)') otypes
          Write(*,'(/''** Program Aborted**'')')
          Stop
          endif
        endif

C...get starting and ending dates
      SDATE = ENVINT('SDATE','Starting Date', 1995001, STATUS) 
      EDATE = ENVINT('EDATE','End Date', 2012366, STATUS) 

C... set data file name
      DATFILE = TRIM(FILETYPE) // '.sorted'

C... read and process input file for given type
      if(FILETYPE .eq. 'OBS') then 
        Call readOBS(OTYPE, INFILE, DATFILE, sDate, eDate, status)
      else if(FILETYPE .eq. 'SITES') then 
        Call readSites(OTYPE, INFILE, DATFILE, sDate, eDate, status)
      else
        Write(*,'(''**ERROR** Invalid file type defined'')')
        Stop
        endif
     
      if( status.ne.0 ) then
        Write(*,'(''--Program aborted while reading input--'')')
        Stop
        endif

C... allocate data arrays
      Allocate( station(NSITES) )
      Allocate( latitude(NSITES) )
      Allocate( longitude(NSITES) )
      Allocate( dataValue(NVARS3D-3,NSITES) )
      Allocate( iValues(NVARS3D-3) )

C... build station, latitude, and longitude arrays
      Do n = 1, NSITES
        station(n) = sites(n)%siteId
        latitude(n) = sites(n)%latitude
        longitude(n) = sites(n)%longitude
        enddo
      dataValue = BADVAL3
      iValues = BADVAL3

C... open sorted DATFILE and process a time step at a time
      Open(unit=lfn, file=datfile, iostat=status)
      IF (status.ne.0) Then
        WRITE( *, '('' Error opening file:'',A)' ) TRIM(datfile)
        KSWIT = SHUT3()
        WRITE(*,'('' API Shut down'')')
        END IF       

C...read first record of data file to get starting date                          
      Read(lfn,'(i7,i3)',iostat=status) curDate, curHr                           
      if(status.ne.0) then
        Write(*,'(''**ERROR** No data found for time period'')')                 
        KSWIT = SHUT3()                                                          
        Stop                                                                     
        endif                                                                    
      rewind(lfn)                               

      if( curDate.lt.sDate ) then
        curDate = sDate
        curHr = 0
        endif                                 

C... define IOAPI file parameters
      FTYPE3D = -1
      SDATE3D = curDate
      STIME3D = 10000 * curHr
      TSTEP3D = tsteps(otype)
      NTHIK3D = 1
      NCOLS3D = NSITES
      NROWS3D = 1
      NLAYS3D = 1
      GDTYP3D = 2
      P_ALP3D = 30.
      P_BET3D = 60.
      P_GAM3D = -100.
      XCENT3D = -100.
      YCENT3D = 40.      
      XORIG3D = -292500.
      YORIG3D = -1687500.
      XCELL3D = 45000.
      YCELL3D = 45000.
      VGTYP3D = -9999
      VGTOP3D = -9.9980008e+36

      !  check for time independent data
      if( SDATE.eq.0 ) then
        TSTEP3D = 0
        SDATE3D = 0
        STIME3D = 0
        endif


      numvar = NVARS3D
      vdesc = VDESC3D(4)

C...try to create new file, if error open file as old
      IF ( OPEN3( 'OUTFILE', 3, 'BLDOVERLAY' ) ) THEN
          Write(*,'(''OUTFILE created'')')
        else
         if (.NOT.OPEN3( 'OUTFILE', 2, 'BLDOVERLAY' )) Then
           WRITE( *, '(''Error: cannot open OUTFILE'')' )
           Stop
           endif
        endif

C...get file description and verify
      IF( .NOT.DESC3('OUTFILE') ) then
        WRITE( *, '(''Error: cannot read file description of OUTFILE'')' )
        Stop
        endif

      ! verify time step
      IF( TSTEP3D.ne.tsteps(otype) .and. SDATE.gt.0 ) then 
        WRITE( *, '(''Error: time step of output file does not equal'',i8)') tsteps(otype)
        Stop
        endif

      ! verify NVARS3D and VDESC3D(4)   
      IF( NVARS3D.ne.numvar .or. VDESC3D(4).ne.vdesc ) then 
        WRITE( *, '(''Error: Output file description does not match'')')
        Stop
        endif

      if( TSTEP3D.gt.0 ) then
        write(*,'(/,8x,''Generating '',a,'' values'',/)') trim(otypes(otype))
       else
        write(*,'(/,8x,''Generating time independent values'',/)')
        endif

C...start loop to read values and write to OUTFILE
      Do
        read(lfn,'(i7,i3,1x,i10,10F16.0)',iostat=status) iDate, iHr, id, iValues

        ! skip if iDate is outside of date window
        if( status.eq.0 .and. (iDate.lt.sdate .or. iDate.gt.eDate) ) CYCLE

        ! check to output data for hour
        if(status.ne.0 .or. iDate.ne.curDate .or. iHr.ne.curHr) then

          if(.NOT. write3('OUTFILE', VNAME3D(1), curDate, 10000*curHr, station)) then
            Call M3EXIT('BLDOVERLAY', curDate, 10000*curHr,
     &                   '**Error** Cannot write IOAPI record', status)
            endif
 
          if(.NOT. write3('OUTFILE', VNAME3D(2), curDate, 10000*curHr, Latitude)) then
            Call M3EXIT('BLDOVERLAY', curDate, 10000*curHr,
     &                   '**Error** Cannot write Latitude record', status)
            endif
        
          if(.NOT. write3('OUTFILE', VNAME3D(3), curDate, 10000*curHr, Longitude)) then
            Call M3EXIT('BLDOVERLAY', curDate, 10000*curHr,
     &                   '**Error** Cannot write Longitude record', status)
            endif
 
          Do i=1,SIZE(iValues)
            if(.NOT. write3('OUTFILE', VNAME3D(3+i), curDate, 10000*curHr, dataValue(i,:))) then
              Call M3EXIT('BLDOVERLAY', curDate, 10000*curHr,
     &                   '**Error** Cannot write IOAPI record', status)
              endif
            enddo

          ! check for eof
          if(status.ne.0) EXIT

          ! check for time independent data (1 step)
          if( TSTEP3D==0 ) EXIT

          ! update date for next record
          curDate = iDate
          curHr = iHr  
          dataValue = BADVAL3
          endif

        !  find site number       
        n = getSiteNumber( id )
        if( n.le. 0 ) then
          Write(*,'(''Invalid site id:'',i10)') id
          KSWIT = SHUT3()
          Stop
          endif

        !  update dataValue array from iValues
        do i=1,SIZE(iValues)
          if( iValues(i).gt.-98.0 ) then   ! values < -98, mark as missing 
            datavalue(i,n) = iValues(i)
           else
            datavalue(i,n) = BADVAL3
            endif
          enddo

        enddo

      ! delete sorted file
      close(unit=lfn,status='delete')   ! delete sorted file

      KSWIT = SHUT3()
      WRITE(*,'('' API Shut down'')')
      STOP
      END
  
  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc           
ccc  subroutine to read hourly data that has been windowed and sorted by date
ccc
ccc  the datafile should contain the following data in csv format
ccc
ccc  yyyyjjj, hour, siteId, longitude, latitude, (values)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc           
      Subroutine ReadOBS(oType, dataFile, sortFile, sDate, eDate, status)                       
      USE M3UTILIO
      USE SITE_DATA                                                                       
      IMPLICIT NONE                                                                       
      !  arguments                                                                        
      Integer       oType
      Character*(*) dataFile                                                              
      Character*(*) sortFile                                                              
      Integer sDate                                                                       
      Integer eDate                                                                       
      Integer status                                                                      
                 
      ! external functions
      Integer getParsedNumber
      Integer system

      !  local variables                                                                  
      Integer stat
      INTEGER HOURS_8HRMAX          ! number of 8hr values to compute 8hr max     
      Character*(256) record
      Character*(32) field
      Character*(16) varDesc(4)
      Character*(32) tmpFile
      CHARACTER*16 ENV_DESC         ! message string
      CHARACTER*16 PNAME            ! Program Name
      CHARACTER*80 MSG              ! Error message
      Integer       siteid, curid, previd
      Integer hour, lnblnk
      Real latitude, longitude
      Integer hdate, htime, i, id, tzoffset, getTZ
      Integer curDate, h, n, prevDate, hrmaxValues
      Logical hasValues, miss_check
      Real hrTotal, max8hr

      !  species variables
      Integer nSpecies
      Character*(16), Allocatable :: vNames(:)
      Character*(10), Allocatable :: vUnits(:)
      Real, Allocatable :: values(:)
      Real, Allocatable :: values32(:)
      Real, Allocatable :: vals(:,:)
      Real, Allocatable :: prevVals(:,:)
      Real, Allocatable :: maxValues(:)

      DATA  PNAME           / 'HR2DAY'        /


      Data varDesc /'Hourly Value','Daily Avg','1-Hour Max','8-Hour Max'/

      ! lfns
      Integer in, out
      Data in /10/
      Data out /11/
     
      tmpFile = 'OBS.tmp'


C...get species list from environment variable SPECIES
      CALL ENVSTR( 'SPECIES', 'List of species', ' ', record, STATUS)
      if(status.ne.0 .or. record.eq.' ') then
        Write(*,'(''**Error** Species not defined'')')
        return
        endif

C...find number of species and allocate arrays 
      nSpecies = getParsedNumber( record, ',' )
      Allocate( vNames(nSpecies), vUnits(nSpecies), values(nSpecies) )
      Allocate( vals(24,nSpecies), prevVals(24,nSpecies) )
      Allocate( values32(32) )
      Allocate( maxValues(nSpecies) )

C... parse species record
      Do i=1,nSpecies
        Call getParsedField( record, ',', i, field, .false. )
        if( field.eq.' ' ) then
          Write(*,'(''**Error** Invalid Species name:'',a)') TRIM(field)
          status = -1
          return
          endif
        vNames(i) = field 
        enddo

C...get species units list from environment variable UNITS
      vUnits = 'na'
      CALL ENVSTR( 'UNITS', 'List of species units', '', record, STATUS)
      if(status.eq.0) then
        Do i=1,nSpecies
          Call getParsedField( record, ',', i, field, .false. )
          vUnits(i) = field
          enddo
        endif 

C... Get the HOURS_8HRMAX environment variable (default is 24)                                                          
       ENV_DESC = 'Number of 8hr values to use when computing DM8HR'                                               
       HOURS_8HRMAX= ENVINT( 'HOURS_8HRMAX', ENV_DESC, 24, STATUS)  
	 
       if ( ( HOURS_8HRMAX .NE. 24) .AND. ( HOURS_8HRMAX .NE. 17) ) THEN                                

        MSG = '**Error** Invalid value for HOURS_8HRMAX, use 24 or 17'
        CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
        Stop
       Endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the switch for setting incomplete days to missing
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
       MISS_CHECK = ENVYN('MISS_CHECK', 'Set incomplete days to missing', .TRUE., STATUS)


      ! sort input file in station order 
      status = system( 'sort -t "," -k3,3n  -k1,1n -k2,2n '
     &  // TRIM(dataFile) // ' > ' // TRIM(sortFile) )


C... open data file as input
      open(unit=in, file=sortFile,status='OLD', iostat=status)     
      if( status.ne.0 ) then
        Write(*,'(''**Error** Cannot open input file:'',a)') TRIM(sortFile)
        return
        endif

C... open temp file as output
      open(unit=out, file=tmpFile, iostat=status)     
      if( status.ne.0 ) then
        Write(*,'(''**Error** Cannot open temp file:'',a)') TRIM(tmpFile)
        return
        endif

      previd = -99
      curid = -99
      vals = -999.0
      prevVals = -999.0
      curDate = 0
      prevDate = 0

C... start loop to read and write each record
      Do 
        read(in,'(a)',iostat=stat) record
        if( stat.ne.0 ) EXIT    ! exit read loop
       
        !parse fields
        Call getParsedField(record,',',1,field,.false.)
        read(field,'(i10)') hDate 
 
        Call getParsedField(record,',',2,field,.false.)
        read(field,'(i10)') hour  
        hTime = 10000 * hour
 
        Call getParsedField(record,',',4,field,.false.)
        read(field,'(f16.0)') longitude
 
        Call getParsedField(record,',',5,field,.false.)
        read(field,'(f16.0)') latitude

        Call getParsedField(record,',',3,field,.false.)
          read(field,'(i10)',err=10) siteId !try to read as integer
	  goto 20
10          siteid = 100 * latitude !could not read as integer, use lat
20	continue
 
        Do i=1,nSpecies
          Call getParsedField(record,',',5+i,field,.false.)
          read(field,'(f16.0)',iostat=stat) values(i)
          if(stat.ne.0 .or. field.eq.' ') values(i) = -999.0
          enddo

        if( oType.eq.1 ) then  !hourly, shift from local to GMT
          htime = 10000 * hour
	  tzoffset = 10000 * getTZ(longitude, latitude)
          Call NEXTIME( hdate, htime, tzoffset)
          write(11,'(i7,i3.2,1x,i10.9,10g16.5)') hdate, htime/10000, siteid, values
          endif !hourly

        if( oType.eq.2 ) then  !daily 
          write(11,'(i7,i3.2,1x,i10.9,10g16.5)') hdate, 0, siteid, values
          endif !daily

        if( oType.eq.3 ) then  !1 hour max
          if( hdate.ne.curDate .or. curid.ne.siteid ) then ! new day and/or site
            maxValues = -999.0
            hasValues = .false.
            do n=1,nspecies
              do h=1,24
                if( vals(h,n).gt.maxValues(n) ) maxValues(n) = vals(h,n)
                enddo
              if(maxValues(n).gt.-998) hasValues = .true.
              enddo
   
            if( hasValues .and. curDate.gt.1900 ) then                           
              write(11,'(i7,i3.2,1x,i10.9,10g16.5)') curDate, 0, curid, maxValues
              endif

            vals = -999.0
            curDate = hdate
            endif ! new day and/or site

          h = hTime/10000 + 1
          vals(h,:) = values
          endif !1 hour max

        if( oType.eq.4 ) then  !8 hour max     
          if( hdate.ne.curDate .or. curid.ne.siteid ) then   !new day and/or site          

            do n=1,nspecies  
	    
	     do h = 1, 24
	     
	      values32(h) = prevVals(h,n)
	      
	     enddo !h 

	     do h = 1, 8
	     
	      if ( (curid .eq. previd) .and. (curDate .eq. prevDate + 1) ) then
	      
	       values32(24+h) = vals(h,n)
	       
	      else
	      
	       values32(24+h) = -999.9
	       
	      endif
	      
	     enddo !h 
	    
	     Call get8hourMax(values32, max8hr, hrmaxValues, miss_check, hours_8hrmax)
	     
	     maxValues(n) = max8hr
	     
	    enddo !n
	                                                                                                                       
            if( prevDate.gt.1900 ) then                           
              write(11,'(i7,i3.2,1x,i10.9,10g16.5)') prevDate, 0, previd, maxValues      
              endif           
                      
            prevVals = vals                                                  
            vals = -999.0
	    prevDate = curDate
	    previd = curid                                               
            curDate = hdate                                             
          endif ! new day and/or site

          h = hTime/10000 + 1                                         
          vals(h,:) = values                                          
          endif ! 8 hour max
 
        ! check for new siteid
        if(siteid .ne. curid) then
    
          ! check if siteid is new       
          if( getSiteNumber( siteid ) .le.0 ) then
            Call addSite(siteid,longitude,latitude)
            endif

          curid = siteid
          endif

        enddo  ! end of read loop


      ! close files before sorting                                      
      close(unit=10)   ! input file                                     
      close(unit=11)   ! temp file to sort                              
                     
      ! sort output by date                                                   
      status = system( 'sort ' // TRIM(tmpFile) // ' > ' // TRIM(sortFile) )
                                                                        
      ! delete tmp file                                                 
      open(unit=11, file=tmpFile)                                       
      close(unit=11,status='delete')   ! delete temp file after sorting 
  
      ! setup IOAPI variables in file header
      NVARS3D = 3 + nSpecies
      VNAME3D(1) = 'STNID' 
      VDESC3D(1) = 'AIRS site id'
      UNITS3D(1) = ''
      VTYPE3D(1) = M3INT 
 
      VNAME3D(2) = 'LAT'  
      VDESC3D(2) = 'Latitude at site'     
      UNITS3D(2) = 'degrees'
      VTYPE3D(2) = M3REAL

      VNAME3D(3) = 'LON'  
      VDESC3D(3) = 'Longitude at site'     
      UNITS3D(3) = 'degrees'
      VTYPE3D(3) = M3REAL

      Do i=1, nspecies
        VNAME3D(3+i) = vNames(i)
        VDESC3D(3+i) = varDesc(oType)
        UNITS3D(3+i) = vUnits(i)
        VTYPE3D(3+i) = M3REAL
        enddo

      return
      end Subroutine ReadOBS 

  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc           
ccc  subroutine to read site data and write a record for each site
ccc
ccc  the site file should contain the following data in tab delimitered format
ccc
ccc  siteId, longitude, latitude
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc           
      Subroutine ReadSites(oType, dataFile, sortFile, sDate, eDate, status)                       

      USE M3UTILIO                                                                                          
      USE SITE_DATA                                                                       
                                                                                          
      IMPLICIT NONE                                                                       
                                                                                          
                                                                                          
      !  arguments                                                                        
      Integer       oType
      Character*(*) dataFile                                                              
      Character*(*) sortFile                                                              
      Integer sDate                                                                       
      Integer eDate                                                                       
      Integer status                                                                      
                 
      ! external functions
      Integer getParsedNumber

      !  local variables                                                                  
      Integer stat
      Character*(256) record
      Character*(32) field
      Integer id, ksites
      Real latitude, longitude, value
      Integer curDate, curTime

      ! lfns
      Integer in, out
      Data in /10/
      Data out /11/
     
      value = ENVREAL('VALUE','value at sites', 1.0, STATUS) 

C... open data file as input
      open(unit=in, file=dataFile,status='OLD', iostat=status)     
      if( status.ne.0 ) then
        Write(*,'(''**Error** Cannot open input file:'',a)') TRIM(dataFile)
        return
        endif

C... open sort file as output
      open(unit=out, file=sortFile, iostat=status)     
      if( status.ne.0 ) then
        Write(*,'(''**Error** Cannot open file:'',a)') TRIM(sortFile)
        return
        endif

C... read input file and save sites
      ksites = 0
      Do 
        read(in,'(a)',iostat=stat) record
        if( stat.ne.0 ) EXIT    ! exit read loop
       
        !parse fields
        Call getParsedField(record,'\t',2,field,.false.)
        read(field,'(f16.0)',iostat=status) longitude
        if(status.ne.0) then
          write(*,'(''**ERROR** Read error of site record:'',a)') TRIM(record)
          stop
          endif
 
        Call getParsedField(record,'\t',3,field,.false.)
        read(field,'(f16.0)',iostat=status) latitude
        if(status.ne.0) then
          write(*,'(''**ERROR** Read error of site record:'',a)') TRIM(record)
          stop
          endif
 
        ! add site n
        ksites = ksites+1
        Call addSite(ksites,longitude,latitude)
        enddo

      curDate = sdate
      curTime = 0

C... start time loop 
      Do
        Do id=1,nsites
          write(11,'(i7,i3.2,1x,i10.9,g16.5)') curdate, curtime/10000, id, value
          enddo  ! end of write loop

        Call NEXTIME( curdate, curtime, 10000)
        if(curDate.gt.eDate) EXIT
        enddo  ! end time loop


      ! close files before sorting                                      
      close(unit=10)   ! input file                                     
      close(unit=11)   ! sort file
                     
  
      ! setup IOAPI variables in file header
      NVARS3D = 4
      VNAME3D(1) = 'STNID' 
      VDESC3D(1) = 'site id'
      UNITS3D(1) = ''
      VTYPE3D(1) = M3INT 
 
      VNAME3D(2) = 'LAT'  
      VDESC3D(2) = 'Latitude at site'     
      UNITS3D(2) = 'degrees'
      VTYPE3D(2) = M3REAL

      VNAME3D(3) = 'LON'  
      VDESC3D(3) = 'Longitude at site'     
      UNITS3D(3) = 'degrees'
      VTYPE3D(3) = M3REAL

      VNAME3D(4) = 'VALUE'  
      VDESC3D(4) = 'value at site'     
      UNITS3D(4) = 'na'
      VTYPE3D(4) = M3REAL

      return
      end Subroutine ReadSites



C****************************************************************************
C  routine to compute the 8 hour max from array of hourly values           
C****************************************************************************
      Subroutine get8hourMax(values,sumMax,hourMax,missChk,hours_8hrmax) 

C    (uses hours 1-24 of current day and 1-7 of next day if HOURS_8HRMAX
C     is set to 24 and 8-24 of current day and 1-7 of next day if 
C     HOURS_8HRMAX is set to 17)
      
      USE M3UTILIO
      Implicit None

      ! arguments
      Real values(*)
      Real sumMax
      Integer hourMax, status
      Logical missChk

      Integer i,j,count,tcount
      Real sum

      INTEGER HOURS_8HRMAX          ! number of 8hr values to compute 8hr max     

      tcount = 0
      summax = -999.0
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
        summax = -999.0
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
        summax = -999.0
        hourMax = -99
        endif
      
      
      endif

      return
      End Subroutine get8hourMax
