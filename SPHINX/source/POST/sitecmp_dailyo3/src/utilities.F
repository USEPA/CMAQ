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


C******************************************************************************
C   subroutine to read values for day from observed hourly hourly file
C   Assumptions: labels for species and optional QA flag label /strings are
C                defined by environment variables OBS_SPECIES, QA_FLAG_HEADER,
C                QA_FLAG_VALUES, and QA_FLAG_CHECK
C******************************************************************************
      Subroutine readInput(in, site, poc, iyear, imonth, iday, values, status)

      USE ENV_VARS

      IMPLICIT NONE     

      !  arguments
      Integer in, iyear, imonth, iday, status
      Character*(*) site
      Integer poc
      Real values(24)

      ! functions
      Integer getNumberOfFields

      ! saved
      Logical, save :: firstime=.TRUE.
      Integer, save :: ozoneFld
      Integer, save :: flagFld
      Integer, save :: pocFld
      Integer, save :: siteFld
      Integer, save :: sdateFld

      ! local
      Integer i, ihour, stat
      Character*256 record
      Character*256 field  
      Character*256 field2  
      Integer poc2
      Character*16  flag   
      Integer iyr, imon, idy, ihr
      Integer f, nflds


      status = 0
      values = -99.0

      !! first pass
      if( firstime ) then
        firstime = .false.
        
        Read(in,'(a)',iostat=status) record
        if(status.ne.0) then                                                              
          status = -1                                                                     
          return                                                                          
          endif 

        nflds = getNumberOfFields(record,',')
        siteFld = -1
        pocFld = -1
        sdateFld = -1
        ozoneFld = -1
        flagFld = -1
        Do f=1,nflds
          Call getField(record, ',', f, field)
          Call UCASE(field)
          Call LeftTrim(field)
          Call UCASE(OBS_SPECIES_NAME)
          Call UCASE(QA_FLAG_NAME)
          if ( TRIM(field) .eq. 'SITE_ID' ) siteFld = f
          if (( TRIM(field) .eq. 'DATEON' ) .or.  
     *        ( TRIM(field) .eq. 'DATE_TIME' )) sdateFld = f
          if( TRIM(field) .eq. 'POCODE' ) pocFld = f
          if( TRIM(field) .eq. 'POCODE' ) pocFld = f
          if( TRIM(field) .eq. TRIM(OBS_SPECIES_NAME) ) ozoneFld = f
          if (QAFLAG_CHECK) then 
           if( TRIM(field) .eq. TRIM(QA_FLAG_NAME) ) flagFld = f 
          endif
          enddo

        ! check if ozone field was found
        if( ozoneFld.lt.1 ) then
          write(*,'(''**ERROR** Cannot locate observed ozone field '',a)') OBS_SPECIES_NAME
          Stop
          endif

        endif

      !!  read first record to get site and date
      Read(in,'(a)',iostat=status) record
      if(status.ne.0) then
        status = -1
        return
        endif

      Call getField(record, ',', siteFld, field) 
      Call LeftTrim(field)       
      Call rmQuots(field)
      site = field
      Call getField(record, ',', sdateFld, field)
      Call LeftTrim(field)
      Call rmQuots(field)
      Call str2Date(field, iyear, imonth, iday, ihour)
      if(iyear.le.0) then
        status = 1
        return
        endif
	
      if (pocFld .gt. 0) then

       Call getField(record, ',', pocFld, field) 
       Call LeftTrim(field)       
       Call rmQuots(field)
       read(field,*) poc

      else
      
       poc = 1 !default POC
       
      endif

      Call getField(record, ',', ozoneFld, field)
      Call LeftTrim(field)
      Call rmQuots(field)
      read(field,'(f16.0)',iostat=stat) values(ihour)
      flag = ' '
      if( flagFld.gt.0 ) Call getField(record, ',', flagFld, flag)
      Call LeftTrim(flag)
      Call rmQuots(flag)
      Call UCASE(QA_FLAG_STRING)

      if(stat.ne.0 .or. values(ihour).le.0 .or. INDEX(TRIM(QA_FLAG_STRING),flag(1:1)).gt.0) values(ihour)=-99.0

      ! read next 23 records to complete day
      Do i=1,23
        Read(in,'(a)',iostat=status) record
        if(status.ne.0) then
          status = -1
          return
          endif

        ! check for new year
        Call getField(record, ',', sdateFld, field)
        Call LeftTrim(field)
        Call rmQuots(field)
        Call str2Date(field, iyr, imon, idy, ihr) 
        if(iyear.le.0) then
          status = 1
          return
          endif

        ! read site field
        Call getField(record, ',', siteFld, field) 
        Call LeftTrim(field)
        Call rmQuots(field)

        ! attempt to read POC field
        if (pocFld .gt. 0) then

         Call getField(record, ',', pocFld, field2) 
         Call LeftTrim(field2)       
         Call rmQuots(field2)
	 read(field2,*) poc2
	 
        else
      
         poc2 = 1 !default POC
       
        endif

        ! check for new site or date
        if(iyr.ne.iyear .or. imon.ne.imonth .or.
     &     idy.ne.iday .or. field.ne.site .or.
     &     poc2.ne.poc ) Then
          backspace in
          endif

        Call getField(record, ',', ozoneFld, field) 
        Call LeftTrim(field)
        Call rmQuots(field)
        read(field,'(f16.0)',iostat=stat) values(ihr)
        flag = ' '
        if(flagFld.gt.0) Call getField(record, ',', flagFld, flag)  
        Call LeftTrim(flag)
        Call rmQuots(flag)
        Call UCASE(QA_FLAG_STRING)
        if(stat.ne.0 .or. values(ihr).le.0 .or. INDEX(TRIM(QA_FLAG_STRING),flag(1:1)).gt.0) values(ihr)=-99.0
        endDo

      return
      End 
     

****************************************************************************
C  routine to convert date string "yyyy-mm-dd" to an Integer value yyyyDDD
C****************************************************************************
      Subroutine str2Date( dateStr, yr, mo, dy, hr )
 
      Character*(*) dateStr
      Integer yr, mo, dy, hr
      Character*22  cString
      Character*10  dString
      Character*10  tString
 
      Character*10  monStr
      Character*10  dayStr
      Character*10  yrStr
      Character*10  hhStr
 
      Integer i,j,nchar
 
      ! adjust for quote mark
      j = 0
      if(dateStr(1:1).eq.'"') j=1
 
      nchar = min( LEN(dateStr), 20+j )
         
      Do i=1,nchar
        cString(i:i) = dateStr(i+j:i+j)
      EndDo

      Call getField(cString,' ',1,dstring) 
      Call getField(cString,' ',2,tstring) 
 
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

      Call getField(tString,':',1,hhStr)
      Read(hhStr,'(i10)',err=501) hr
      hr = hr+1
      return
 
  500 Write(*,'(''Error converting date string '',a)') TRIM(dString)
      yr = -99
      Return

  501 Write(*,'(''Error converting hour string '',a)') Trim(tString)
      yr = -99
      Return
      End Subroutine str2Date
 

