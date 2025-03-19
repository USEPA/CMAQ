C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C*************************************************************************
C
C  MODULE: defines site data
C             
C*************************************************************************
      MODULE SITE_DATA

      INTEGER  NSITES

      CHARACTER*256   SITE_FNAME  

      CHARACTER*20, ALLOCATABLE :: SITE( : )
      CHARACTER*25, ALLOCATABLE :: STATE( : )
      CHARACTER*25, ALLOCATABLE :: COUNTY( : )
      INTEGER,  ALLOCATABLE     :: POC( : )

      INTEGER, ALLOCATABLE :: TZ( : ) 
      INTEGER, ALLOCATABLE :: COL( : )
      INTEGER, ALLOCATABLE :: ROW( : )

      REAL, ALLOCATABLE :: LAT( : )   
      REAL, ALLOCATABLE :: LON( : ) 
      REAL, ALLOCATABLE :: SX( : )  
      REAL, ALLOCATABLE :: SY( : )  
      REAL, ALLOCATABLE :: ELEV( : )   


      CONTAINS


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Subroutine to get data from file type 1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         SUBROUTINE FL_TYP1
C*************************************************************************
C
C  FUNCTION:  To provide site data
C             
C*************************************************************************
         USE ENV_VARS
         USE M3UTILIO

         IMPLICIT NONE 

      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS:
         INTEGER getNumberOfFields

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
         CHARACTER*300    RECORD       ! input record buffer
         CHARACTER*80    FIELD        ! input field
         CHARACTER*80    FIELD1       
         CHARACTER*80    FIELD2       
         CHARACTER*16    ENV_DFLT     ! Environment variable default value
         CHARACTER*16    PNAME        ! Program Name
         CHARACTER*80    ENV_DESC     ! Environment variable description
         CHARACTER*80    MSG          ! Error message
         CHARACTER*256   RET_VAL      ! Returned value of environment variable

         INTEGER   NFIELDS
         INTEGER   LFN   
         INTEGER   N, J   
         INTEGER   NN   
         INTEGER   STATUS  
         INTEGER   TZONE  

         Integer   numsites
         Character*(20)              ::    prevSite
         Integer                     ::    prevPOC
         Character*(20), allocatable ::    idfld(:)
         Character*(25), allocatable ::    statefld(:)
         Character*(25), allocatable ::    countyfld(:)
         Integer      ,  allocatable ::    pocfld(:,:)
         Real, allocatable           ::    lonfld(:)
         Real, allocatable           ::    latfld(:)
         Real, allocatable           ::    elevfld(:)
         Integer, allocatable        ::    npoc(:)
         Integer, allocatable        ::    tzfld(:)
         Logical, allocatable        ::    active(:) 
         LOGICAL IS_CSV  

         Integer                     ::    siteField
         Integer                     ::    latField
         Integer                     ::    lonField
         Integer                     ::    stateField
         Integer                     ::    countyField
         Integer                     ::    elevField
         Integer                     ::    tzField
         Integer                     ::    pocField
         Integer                     ::    iPOC
         Integer                     ::    t
         Character*(10)              ::    ftypes(10)
         Character*(10)              ::    sitehdrs(10)

         Data ftypes /'CASTNET', 'SEARCH', 'IMPROVE', 'NADP', 'AIRMON', 'STN', 
     &                'MDN', 'MET', 'DEARS', 'OUTPUT'/

         Data sitehdrs /'SITE_ID', 'SITE_ID', 'SITE_CODE', 'SITEID', 'SITE', 'SITE',
     &                  'SITEID', 'SITE_ID', 'PID', 'SITEID'/

C**********************************************************************
         DATA  PNAME       / 'FL_TYP1'  /
         LFN = JUNIT()
         SITE_FNAME = SITE_FILE

         OPEN(unit=lfn, file=SITE_FNAME, status='old', iostat=status)
         if(status.ne.0) then
           MSG = 'Cannot open site file:' // trim(SITE_FNAME)
           CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
           endif

         !  read the first line to determine whether this site file
         !  is using tab-delimited (no header line) or csv format
         
         READ( LFN, '(A)', iostat=status ) RECORD
         if(status.ne.0) then
           MSG = 'Cannot read first line of site file:' // trim(SITE_FNAME)
           CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
           endif
         nfields = getNumberOfFields(record, ",")
         if (nfields .le. 1) then
          IS_CSV = .FALSE. !assume tab delimited site file, no comma in first line
         else
          IS_CSV = .TRUE.  !assume csv site file, commas in first line.
                             !find columns for stat_id, lat, lon, 
                             !timezone, elevation, state, county
          
         endif
         REWIND( LFN )


         !  read file to determine number of sites and metadata
         
         IF (.NOT.IS_CSV) THEN 
          write(*,'(''ASSUMING TAB-DELIMITED SITE FILE: '',a)') trim(SITE_FNAME)
          numsites = 0
          DO
           READ( LFN, '(A)', iostat=status ) RECORD
           if(status.ne.0) EXIT
           Call getField(record, char(9), 1, field)
           nfields = getNumberOfFields(record, char(9))
           IF(nfields.ge.3 .and. LEN_TRIM(field).ge.2) numsites = numsites + 1
           ENDDO
      
          if( numsites.eq.0 ) then
           write(*,'(''**ERROR**, No sites found in site file:'',a)') trim(SITE_FNAME)
           Stop
           endif

 
          ALLOCATE( idfld( numsites ), lonfld( numsites ), latfld( numsites ) )
          ALLOCATE( tzfld( numsites ), active( numsites), pocfld( numsites , 99) )
          ALLOCATE( npoc( numsites ) , elevfld (numsites), statefld (numsites) )
          ALLOCATE( countyfld( numsites ) )
                  
          npoc = 0
          active = .false.
          pocfld = 1 !default POC is 1
          elevfld = -999. !default elevation is missing
          statefld = "NotAvailable" !default state name
          countyfld = "NotAvailable" !default state name

          REWIND( LFN )
          ! read all site data from LFN
          N = 0
          DO
            READ( LFN, '(A)', iostat=status ) RECORD
            if( status.ne.0 ) EXIT

            nfields = getNumberOfFields(record, char(9))
            Call getField(record, char(9), 1, field)
            IF(nfields.ge.3 .and. LEN_TRIM(field).ge.2) THEN
               N = N + 1
               idfld( N ) = field
               Call getField(record, char(9), 2, field)
               read(field,'(f16.0)',iostat=status) lonfld(n)
               if( status.ne.0 ) then
                 write(*,'(''**WARNING**  Invalid site record:'',a)') TRIM(record)
                 CYCLE
                 endif

               Call getField(record, char(9), 3, field)
               read(field,'(f16.0)',iostat=status) latfld(n)
               if( status.ne.0 ) then
                 write(*,'(''**WARNING**  Invalid site record:'',a)') TRIM(record)
                 CYCLE
                 endif

               ! compute Time zone offset from longitude
               tzfld(n) = -(lonfld(n)+7.5) / 15

               ! try to read Time zone offset from field 4
               if(nfields.eq.4) then
                 Call getField(record, char(9), 4, field)
                 READ(field, '(BN,i10)', iostat=status) tzone
                 if(status.eq.0 .and. field.ne.' ') tzfld(n) = tzone
                 endif

               endif
            enddo

          close(unit=LFN)
        
         ELSE !csv-formatted site file
         
          write(*,'(''ASSUMING CSV SITE FILE: '',a)') trim(SITE_FNAME)
          
          numsites = 0
          
          ! read header line first to find the position of the 
          ! lat, lon, time zone, state,county, and elevation fields
          

          siteField = 0
          stateField  = 0
          countyField  = 0
          latField  = 0
          lonField  = 0
          elevField  = 0
          tzField  = 0

          read(lfn,'(a)',iostat=status) record
          if( status.ne.0 ) then
           write(*,'(''**ERROR** Invalid table header in table file'')')
           Stop
          endif

          call UCASE( record )
          call rmCommas(record)
          nfields = getNumberOfFields(record, ',')
          
          Do n=1,nfields
           call getField(record, ',', n, field)
           call rmQuots( field )
           call LeftTrim(field)
           if( INDEX(field,'STAT_ID').gt.0 ) then
            siteField = n
            exit
           endif
          EndDo

          Do n=1,nfields
           call getField(record, ',', n, field)
           call rmQuots( field )
           call LeftTrim(field)
           if( INDEX(field,'LAT').gt.0 )  then
            latField = n
            exit
           endif
          EndDo

          Do n=1,nfields
           call getField(record, ',', n, field)
           call rmQuots( field )
           call LeftTrim(field)
           if( INDEX(field,'LON').gt.0 )  then
            lonField = n
            exit
           endif
          EndDo


          Do n=1,nfields
           call getField(record, ',', n, field)
           call rmQuots( field )
           call LeftTrim(field)
           if( INDEX(field,'ELEVATION').gt.0 )  then
            elevField = n
            exit
           endif
          EndDo


          Do n=1,nfields
           call getField(record, ',', n, field)
           call rmQuots( field )
           call LeftTrim(field)
           if( INDEX(field,'STATE').gt.0 )  then
            stateField = n
            exit
           endif
          EndDo


          Do n=1,nfields
           call getField(record, ',', n, field)
           call rmQuots( field )
           call LeftTrim(field)
           if( INDEX(field,'COUNTY').gt.0 )  then
            countyField = n
            exit
           endif
          EndDo


          Do n=1,nfields
           call getField(record, ',', n, field)
           call rmQuots( field )
           call LeftTrim(field)
           if( INDEX(field,'GMT_OFFSET').gt.0 )  then
            tzField = n
            exit
           endif
          EndDo

          if( ( siteField .eq. 0 ) .or. ( latField .eq. 0 ) .or. 
     *        ( lonField .eq. 0 ) ) then
           write(*,'(''**ERROR**, stat_id, lat, and/or lon column ''//''
     *       missing in:'',a)') trim(SITE_FNAME)
           Stop
          endif
          
          ! done reading and processing header line, now read station data
          
          DO
           READ( LFN, '(A)', iostat=status ) RECORD
           if(status.ne.0) EXIT
           Call getField(record, ',' , siteField, field)
           nfields = getNumberOfFields(record, ',' )
           IF(nfields.ge.3 .and. LEN_TRIM(field).ge.2) numsites = numsites + 1
          ENDDO
      
          if( numsites.eq.0 ) then
           write(*,'(''**ERROR**, No sites found in site file:'',a)') trim(SITE_FNAME)
           Stop
          endif

 
          ALLOCATE( idfld( numsites ), lonfld( numsites ), latfld( numsites ) )
          ALLOCATE( tzfld( numsites ), active( numsites), pocfld( numsites , 99) )
          ALLOCATE( npoc( numsites ) , elevfld (numsites), statefld (numsites) )
          ALLOCATE( countyfld( numsites ) )
                  
          npoc = 0
          active = .false.
          pocfld = 1 !default POC is 1
          elevfld = -999. !default elevation is missing
          statefld = "NotAvailable" !default state name
          countyfld = "NotAvailable" !default state name

          REWIND( LFN )
          
          ! read header line
          read(lfn,'(a)',iostat=status) record
          if( status.ne.0 ) then
           write(*,'(''**ERROR** Invalid table header in table file'')')
           Stop
          endif

          ! read all site data from LFN
          N = 0
          DO
            READ( LFN, '(A)', iostat=status ) RECORD
            if( status.ne.0 ) EXIT
            
            Call getField(record, ',', siteField, field)
            call rmQuots( field )
                        
            IF(LEN_TRIM(field).ge.2) THEN
               N = N + 1
               idfld( N ) = field

               Call getField(record, ',', lonField, field)
               call rmQuots( field )
               read(field,'(f16.0)',iostat=status) lonfld(n)
               if( status.ne.0 ) then
                 write(*,'(''**WARNING**  Invalid site record for lon:'',a)') TRIM(record)
                 write(*,'(''**WARNING**  field = '',a)') TRIM(field)
                 CYCLE
                 endif

               Call getField(record, ',', latField, field)
               call rmQuots( field )
               read(field,'(f16.0)',iostat=status) latfld(n)
               if( status.ne.0 ) then
                 write(*,'(''**WARNING**  Invalid site record for lat:'',a)') TRIM(record)
                 write(*,'(''**WARNING**  field = '',a)') TRIM(field)
                 CYCLE
                 endif

               if (stateField .gt. 0) then
                Call getField(record, ',', stateField, field)
                call rmQuots( field )
                statefld( N ) = field
               endif
               
               if (countyField .gt. 0) then
                Call getField(record, ',', countyField, field)
                call rmQuots( field )
                countyfld( N ) = field
               endif
               

               if (elevField .gt. 0) then
                Call getField(record, ',', elevField, field)
                call rmQuots( field )
                read(field,'(f16.0)',iostat=status) elevfld(n)
                if( status.ne.0 ) then
                 write(*,'(''**WARNING**  Invalid site record for elev:'',a)') TRIM(record)
                 write(*,'(''**WARNING**  field = '',a)') TRIM(field)
                 CYCLE
                endif
               endif

               ! compute Time zone offset from longitude
               tzfld(n) = -(lonfld(n)+7.5) / 15

               ! try to read Time zone offset from field 4
               if (tzField .gt. 0) then
                 Call getField(record, ',', tzField, field)
                 call rmQuots( field )
                 READ(field, '(BN,i10)', iostat=status) tzone
                 if(status.eq.0 .and. field.ne.' ') tzfld(n) = tzone
                 endif

             endif
          enddo

          close(unit=LFN)
         
         ENDIF !tab-delimited or csv site file
                           
         write(*,'(i6,'' total sites read'')') numsites

         OPEN(unit=lfn, file=IN_TABLE, status='old', iostat=status)
         if(status.ne.0) then
           MSG = 'Cannot open IN_TABLE:' // trim(IN_TABLE)
           CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
           endif

         ! find table type index t
         do n=1,SIZE(ftypes)
           if( TABLE_TYPE.eq.ftypes(n) ) t = n
           enddo

         ! determine location of site field in table file
         siteField = 0
         pocField  = 0

         do
           read(lfn,'(a)',iostat=status) record
           if( status.ne.0 ) then
             write(*,'(''**ERROR** Invalid table header in table file'')')
             Stop
             endif

           call UCASE( record )
           call rmCommas(record)
           nfields = getNumberOfFields(record, ',')

           if( nfields.lt.3 ) CYCLE

           if( INDEX(record,TRIM(sitehdrs(t))).gt.0 ) then
             Do n=1,nfields
               call getField(record, ',', n, field)
               call rmQuots( field )
               call LeftTrim(field)
               if( INDEX(field,TRIM(sitehdrs(t))).gt.0 ) siteField = n
               if( INDEX(field,'POCODE').gt.0 ) pocField = n
               EndDo
             EndIf
           if( siteField.gt.0 ) EXIT
           enddo
        
         ! read table file and check for active sites
         prevSite = ' '
         prevPOC  = 0
         do
           read(lfn,'(a)',iostat=status) record
           if( status.ne.0 ) EXIT
           call rmCommas(record)

           Call getField(record, ',', siteField, field1 )
           if ( pocField .gt. 0 ) then
            Call getField(record, ',', pocField, field2 )
           else
            field2 = '1' !use 1 as default parameter occurrence code
           endif
           
           call rmQuots( field1 )
           call rmQuots( field2 )
           read(field2,*) iPOC
           
           if (( TRIM(field1) .eq. TRIM(prevSite) ) .and. ( iPOC .eq. prevPOC )) CYCLE

           do n=1,numsites
             if( TRIM(field1) .eq. TRIM(idfld(n)) ) then
               active(n) = .true.
               npoc(n) = npoc(n) + 1
               pocfld(n,npoc(n)) = iPOC
               EXIT
               endif
             enddo
           prevSite = field1
           prevPOC  = iPOC
           enddo

         !  count number of active sites
         nsites = 0
         do n=1,numsites
           if( active(n) ) then
            do j = 1, npoc(n)
             nsites = nsites + 1
            enddo !nn
           endif
         enddo


         ! build list of active sites
         ALLOCATE( SITE( nsites ), POC( nsites ), TZ( nsites ), LON( nsites ), LAT( nsites ) )
         ALLOCATE( STATE( nsites ), COUNTY( nsites ), ELEV( nsites ) )

         nn = 0
         do n=1,numsites
           if( active(n) ) then
            do j = 1, npoc(n)
             nn = nn + 1
             SITE(nn) = idfld(n)
             STATE(nn) = statefld(n)
             COUNTY(nn) = countyfld(n)
             POC(nn) = pocfld(n,j)
             TZ(nn) = tzfld(n)
             LON(nn) = lonfld(n)
             LAT(nn) = latfld(n)
             ELEV(nn) = elevfld(n)
            enddo !j
           endif
         enddo !n


         write(*,'(i6,'' active sites loaded'')') NSITES

         close(lfn)
         RETURN

         END SUBROUTINE FL_TYP1


         SUBROUTINE SET_SITE_LOC

C*************************************************************************
C
C  FUNCTION: Finds the col and row location of each site
C             
C*************************************************************************
         USE ENV_VARS
         USE M3FILES
         USE M3UTILIO
         USE GRID_DATA
         IMPLICIT NONE     

C..ARGUMENTS: None

C..PARAMETERS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
         CHARACTER*24  CRDATE      ! Create date
         CHARACTER*80  MSG         ! Log message
         CHARACTER*16  PNAME       ! Program Name
         CHARACTER*256   RET_VAL   ! Returned value of environment variable

         INTEGER   C, R, N         ! Loop indices
         INTEGER   IOUT            ! Output file unit number
         INTEGER   JDATE           ! Create date YYYYDDD
         INTEGER   JTIME           ! Create timeHHMMSS

         REAL   LATIN           ! Input lat
         REAL   LONIN           ! Input lon
         REAL   X               ! x-coordinate for lambert projection
         REAL   Y               ! y-coordinate for lambert projection
         REAL   XW, XE          ! X-coordinates of grid cell edges  
         REAL   YS, YN          ! Y-coordinates of grid cell edges  
         LOGICAL PROJ           ! projection is supported
   
C**********************************************************************
         DATA PNAME / 'SET_SITE_LOC' /
         DATA PROJ  / .FALSE. /

         ALLOCATE( COL( NSITES ), ROW( NSITES ), SX( NSITES ), SY( NSITES ) )

         Call SETPROJ( GDTYP3D, Real(M3GRID%P_ALP),Real(M3GRID%P_BET),
     &                 Real(M3GRID%P_GAM),Real(M3GRID%XCENT),Real(M3GRID%YCENT) )

C  process each site
         DO N = 1, NSITES

c..for now make sure longitude is negative
            LONIN = LON( N )
            LATIN = LAT( N )
         
c..get the x,y coordinates 
            Call ToProj(GDTYP3D, LONIN, LATIN, X, Y)

c..save x,y cooridinates
            SX( N ) = X
            SY( N ) = Y

c..find the column location 
            COL( N )  = 0
            DO C = 1, NCOLS3D
               XW = M3GRID % XORIG + FLOAT( C - 1 ) * M3GRID % XCELL 
               XE = XW + M3GRID % XCELL
               IF( X .GE. XW .AND. X .LT. XE ) COL( N ) = C
            ENDDO

c..find the row location 
            ROW( N ) = 0
            DO R = 1, NROWS3D
               YS = M3GRID % YORIG + FLOAT( R - 1 ) * M3GRID % YCELL 
               YN = YS + M3GRID % YCELL
               IF( Y .GE. YS .AND. Y .LT. YN ) ROW( N ) = R
            ENDDO

           ! Write(*,'(a,2f10.4,2i5)') SITE(N),LON(N),LAT(N), COL(N), ROW(N)
         ENDDO

         RETURN

92000 FORMAT( '! INPUT M3 FILE ', I2, ': ', A )
92020 FORMAT( '! INPUT SITE FILE: ', A )
92040 FORMAT( '! SITEID STATE REGION LON LAT COL ROW X Y ' )
92060 FORMAT( A9, 1X, A2, 1X, A2, F8.3, 1X, F8.3, 1X, I3, 1X, I3, 2F15.2 )

         END SUBROUTINE SET_SITE_LOC

C  function to find the index number of a site
      Integer Function getSiteNumber( id1, id2 ) result (siteNo)

      CHARACTER*(*) :: id1, id2
      Integer :: i

      !  local variables
      Character*(20), save :: prevId1
      Integer, save        :: prevId2
      Integer, save        :: prevNo
      Integer              :: iPOC

      siteNo = -1
      read(id2,*) iPOC  
      
      !  check for repeat
      if (( id1 .eq. prevId1 ) .and. ( iPOC .eq. prevId2 )) then
        siteNo = prevNo
        return
        endif

      ! search list for match 
      do i=1,NSITES
       if (( TRIM(id1) .eq. TRIM(SITE(I)) ) .and. 
     *     ( iPOC .eq. POC(I) )) then
         siteNo = i

         ! save for next lookup
         prevId1 = id1
         prevId2 = iPOC
         prevNo = siteNo
         return
         endif
       Enddo 
      
      !Write(*,'('' cannot find match for site '',a)') id1
      Return
      End Function getSiteNumber


C  function to return latitude value for site n
      Real Function getLatitude( id ) result (latitude)

      if(id .le. size(LAT)) Then
        latitude = lat(id)
      else
        latitude = -999
        endif

      Return
      End Function getLatitude
      

C  function to return longitude value for site n
      Real Function getLongitude( id ) result (longitude)

      if(id .le. size(LON)) Then
        longitude = lon(id)
      else
        longitude = -999
        endif

      Return
      End Function getLongitude


      !****************************************************************************
      !  routine to set map projection
      !****************************************************************************
      Subroutine SetProj(gdtype, alpha, beta, gamma, xcent, ycent)

      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Integer gdtype
      Real alpha, beta, gamma, xcent, ycent

      !  check for LAT/LON projection
      if( gdtype .eq. 1 ) then
        return
        endif

      !  check for lambert projection
      if( gdtype .eq. 2 ) then
        if( .NOT. SETLAM( alpha, beta, gamma, xcent, ycent) ) then
          Call m3exit ('sitecmp', 0, 0, 'Lambert projection setup error', xstat2)
          endif
        return
        endif

      !  check for Polar projection
      if( gdtype .eq. 6 ) then
        if( .NOT. SETPOL( alpha, beta, gamma, xcent, ycent) ) then
          Call m3exit ('sitecmp', 0, 0, 'Polar projection setup error', xstat2)
          endif
        return
        endif

      !  check for equatorial mercator projection
      if( gdtype .eq. 7 ) then
        if( .NOT. SETEQM( alpha, beta, gamma, xcent, ycent) ) then
          Call m3exit ('sitecmp', 0, 0, 'Equatorial mercator projection setup error', xstat2)
          endif
        return
        endif


      Call m3exit ('sitecmp', 0, 0, 'Unsupported map projection', xstat2)

      end Subroutine SetProj


      !C****************************************************************************
      !C  routine to compute map projection from LAT/LON
      !C****************************************************************************
      Subroutine ToProj(gdtype, longitude, latitude, x, y)

      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Integer gdtype
      Real longitude, latitude, x, y

      !  check for LAT/LON projection
      if( gdtype .eq. 1 ) then
        x = longitude
        y = latitude
        return
        endif

      !  check for lambert projection
      if( gdtype .eq. 2 ) then
        if(.NOT.LL2LAM(longitude, latitude, x, y) ) then
          Call m3exit('sitecmp', 0, 0, 'Lat/Lon to Lambert error', xstat2)
          endif
        return
        endif

      !  check for polar projection
      if( gdtype .eq. 6 ) then
        if(.NOT.LL2POL(longitude, latitude, x, y) ) then
          Call m3exit('sitecmp', 0, 0, 'Lat/Lon to Polar error', xstat2)
          endif
        return
        endif

      !  check for polar projection
      if( gdtype .eq. 7 ) then
        if(.NOT.LL2EQM(longitude, latitude, x, y) ) then
          Call m3exit('sitecmp', 0, 0, 'Lat/Lon to equatorial mercator error', xstat2)
          endif
        return
        endif

      Call m3exit ('sitecmp', 0, 0, 'Unsupported map projection', xstat2)

      end Subroutine ToProj


      !C****************************************************************************
      !C  routine to compute LAT/LON from map projection
      !C****************************************************************************
      Subroutine ToLL(gdtype, x, y, longitude, latitude)

      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Integer gdtype
      Real longitude, latitude, x, y

      !  check for LAT/LON projection
      if( gdtype .eq. 1 ) then
        longitude = x
        latitude = y
        return
        endif

      !  check for lambert projection
      if( gdtype .eq. 2 ) then
        if(.NOT.LAM2LL(x, y, longitude, latitude) ) then
          Call m3exit('sitecmp', 0, 0, 'Lat/Lon to Lambert error', xstat2)
          endif
        return
        endif

      !  check for polar projection
      if( gdtype .eq. 6 ) then
        if(.NOT.POL2LL(x, y, longitude, latitude) ) then
          Call m3exit('sitecmp', 0, 0, 'Lat/Lon to Polar error', xstat2)
          endif
        return
        endif

      !  check for equatorial mercator projection
      if( gdtype .eq. 7 ) then
        if(.NOT.EQM2LL(x, y, longitude, latitude) ) then
          Call m3exit('sitecmp', 0, 0, 'Lat/Lon to equatorial mercator error', xstat2)
          endif
        return
        endif

      Call m3exit ('sitecmp', 0, 0, 'Unsupported map projection', xstat2)

      end Subroutine ToLL

      END MODULE SITE_DATA
