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

      MODULE SITE_DATA

      TYPE SITE
         character*20 ::  site_id
         CHARACTER*25 ::  STATE
         CHARACTER*25 ::  COUNTY
         REAL         ::  LAT
         REAL         ::  LON
         REAL         ::  LAMBX
         REAL         ::  LAMBY
         REAL         ::  ELEV
         Integer      ::  TZ
         Integer      ::  row
         Integer      ::  column 
      END TYPE SITE

c      TYPE ( SITE ), PRIVATE, ALLOCATABLE :: SITES(:)
      TYPE ( SITE ), ALLOCATABLE :: SITES(:)

      CONTAINS


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Subroutine to get data from file type 1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         SUBROUTINE LOAD_SITES
         
         USE M3UTILIO

         IMPLICIT NONE 

C..INCLUDES:  
C         INCLUDE SUBST_IOPARMS     ! IOAPI parameters
C         INCLUDE SUBST_IOFDESC     ! IOAPI file description
C         INCLUDE SUBST_IODECL      ! IOAPI declarations

      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS:
C         INTEGER JUNIT    ! Get a Fortran unit number
         INTEGER getNumberOfFields

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
         CHARACTER*256   SITE_FNAME  
         CHARACTER*300   RECORD       ! input record buffer
         CHARACTER*80    FIELD        ! input field
         CHARACTER*16    ENV_DFLT     ! Environment variable default value
         CHARACTER*16    PNAME        ! Program Name
         CHARACTER*16    SITEFILE     ! Env var for site file name
         CHARACTER*80    ENV_DESC     ! Environment variable description
         CHARACTER*80    MSG          ! Error message
         CHARACTER*256   RET_VAL      ! Returned value of environment variable

         INTEGER   nsites             ! number of sites
         INTEGER   NFIELDS            ! number of fields in record
         INTEGER   lfn                ! Unit no. of input site file
         INTEGER   tzone              ! time zone offset number read from field 4
         INTEGER   N                  ! Loop index
         INTEGER   STATUS             ! Status code

         Integer                     ::    siteField
         Integer                     ::    latField
         Integer                     ::    lonField
         Integer                     ::    stateField
         Integer                     ::    countyField
         Integer                     ::    elevField
         Integer                     ::    tzField

         LOGICAL IS_CSV  

!        CHARACTER*16 site,lonfld,latfld,tzfld

C**********************************************************************
         DATA  PNAME       / 'LOAD_SITES'/
         DATA  SITEFILE    / 'SITE_FILE' /

         ENV_DFLT = ' '
         ENV_DESC = 'Input site file'        
         CALL ENVSTR( SITEFILE, ENV_DESC, ENV_DFLT, SITE_FNAME, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'No input site file assigned to SITEFILE'
            CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
         ENDIF

 
         !  open file
         lfn = 10
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


         !  read file to determine number of sites

         IF (.NOT.IS_CSV) THEN 
          write(*,'(''ASSUMING TAB-DELIMITED SITE FILE: '',a)') trim(SITE_FNAME)

          NSITES = 0
          DO
           read(lfn, '(a)', iostat=status ) RECORD
           if(status.ne.0) EXIT
           Call getField(record, char(9), 1, field)
           nfields = getNumberOfFields(record, char(9))
           IF(nfields.ge.3 .and. LEN_TRIM(field).ge.4) NSITES = NSITES + 1
          ENDDO
      
          if( NSITES.eq.0 ) then
           write(*,'(''**ERROR**, No sites found in site file:'',a)') trim(SITE_FNAME)
           Stop
          endif
 
       
          ALLOCATE( SITES( nsites ) )

          SITES%ELEV = -999. !default elevation is missing
          SITES%STATE = "NotAvailable" !default state name
          SITES%COUNTY = "NotAvailable" !default state name

          REWIND( lfn )

          ! read in site data
          N = 0
          DO 
            READ( LFN, '(A)', iostat=status ) RECORD
            if( status.ne.0 ) EXIT
 
            nfields = getNumberOfFields(record, char(9))
            Call getField(record, char(9), 1, field)
            IF(nfields.ge.3 ) THEN                           
               N = N + 1                                                               
               SITES(N)%site_id = field                                                       
                                                                                       
               Call getField(record, char(9), 2, field)                                
               READ(field, '(BN,f15.0)', iostat=status) SITES(N)%LON
               if( status.ne.0 ) then                                                  
                 write(*,'(''**ERROR** Cannot read longitude from record:'',a)') trim(record)        
                 stop                                                                  
               endif                                                                 
                                                                                                                                                                              
               Call getField(record, char(9), 3, field)                                
               READ(field, '(BN,f15.0)', iostat=status) SITES(N)%LAT
               if( status.ne.0 ) then                                                  
                 write(*,'(''**ERROR** Cannot read latitude from record:'',a)') trim(record)
                 stop                                                                  
               endif                                                                 
                                                                                       
               ! compute Time zone offset from longitude                               
               SITES(N)%TZ = -(SITES(N)%LON) / 15                                                  
                                                                                       
               ! try to read Time zone offset from field 4                             
               if(nfields.eq.4) then                                                   
                 Call getField(record, char(9), 4, field)                              
                 READ(field, '(BN,i10)', iostat=status) tzone                          
                 if(status.eq.0 .and. tzone.gt.0) SITES(N)%TZ = tzone                        
               endif                                                                 
                                                                                       
            ENDIF
          ENDDO
        
          close(lfn)
          
         ELSE !csv-formatted site file
         
          write(*,'(''ASSUMING CSV SITE FILE: '',a)') trim(SITE_FNAME)
          
          nsites = 0
          
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
           IF(nfields.ge.3 .and. LEN_TRIM(field).ge.3) nsites = nsites + 1
          ENDDO
      
          if( nsites.eq.0 ) then
           write(*,'(''**ERROR**, No sites found in site file:'',a)') trim(SITE_FNAME)
           Stop
          endif

          ALLOCATE( SITES( nsites ) )
                  
          SITES%ELEV = -999. !default elevation is missing
          SITES%STATE = "NotAvailable" !default state name
          SITES%COUNTY = "NotAvailable" !default state name

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
                        
            IF(LEN_TRIM(field).ge.3) THEN
               N = N + 1
               SITES( N )%site_id = field

               Call getField(record, ',', lonField, field)
               call rmQuots( field )
               read(field,'(f16.0)',iostat=status) SITES( N )%LON
               if( status.ne.0 ) then
                 write(*,'(''**WARNING**  Invalid site record for lon:'',a)') TRIM(record)
                 write(*,'(''**WARNING**  field = '',a)') TRIM(field)
                 CYCLE
                 endif

               Call getField(record, ',', latField, field)
               call rmQuots( field )
               read(field,'(f16.0)',iostat=status) SITES( N )%LAT
               if( status.ne.0 ) then
                 write(*,'(''**WARNING**  Invalid site record for lat:'',a)') TRIM(record)
                 write(*,'(''**WARNING**  field = '',a)') TRIM(field)
                 CYCLE
                 endif

               if (stateField .gt. 0) then
                Call getField(record, ',', stateField, field)
                call rmQuots( field )
                SITES( N )%STATE = field
               endif
               
               if (countyField .gt. 0) then
                Call getField(record, ',', countyField, field)
                call rmQuots( field )
                SITES( N )%COUNTY = field
               endif
               

               if (elevField .gt. 0) then
                Call getField(record, ',', elevField, field)
                call rmQuots( field )
                read(field,'(f16.0)',iostat=status) SITES( N )%ELEV
                if( status.ne.0 ) then
                 write(*,'(''**WARNING**  Invalid site record for elev:'',a)') TRIM(record)
                 write(*,'(''**WARNING**  field = '',a)') TRIM(field)
                 CYCLE
                endif
               endif

               ! compute Time zone offset from longitude
               SITES( N )%TZ = -( SITES( N )%LON + 7.5 ) / 15

               ! try to read Time zone offset from field 4
               if (tzField .gt. 0) then
                 Call getField(record, ',', tzField, field)
                 call rmQuots( field )
                 READ(field, '(BN,i10)', iostat=status) tzone
                 if(status.eq.0 .and. field.ne.' ') SITES( N )%TZ = tzone
                 endif

             endif
          enddo

          close(unit=LFN)
         
         ENDIF !tab-delimited or csv site file
          
          
         RETURN

         END SUBROUTINE LOAD_SITES



C*************************************************************************
         SUBROUTINE SET_SITE_LOC
C*************************************************************************
         USE ENV_VARS
         USE M3FILES
         USE GRID_DATA
         USE M3UTILIO
         IMPLICIT NONE     

C..INCLUDE FILES:
C         INCLUDE SUBST_IOPARMS     ! IOAPI parameters
C         INCLUDE SUBST_IOFDESC     ! IOAPI file description
C         INCLUDE SUBST_IODECL      ! IOAPI declarations

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

   
C**********************************************************************
         DATA PNAME / 'SET_SITE_LOC' /

         DO N = 1, size(SITES)

c..for now make sure longitude is negative
c            LONIN = MIN( SITES(N)%LON, -1.0 * SITES(N)%LON )
            LONIN = SITES(N)%LON
            LATIN = SITES(N)%LAT
         
          IF ( M3GRID % GDTYP . eq . 2 ) THEN ! Lambert


            
            IF( .NOT. SETLAM( SNGL( M3GRID % P_ALP ),     !  first, initialize
     &                  SNGL( M3GRID % P_BET ),        !  for LL2LAM()
     &                  SNGL( M3GRID % P_GAM ),
     &                  SNGL( M3GRID % XCENT ), 
     &                  SNGL( M3GRID % YCENT ) ) ) THEN
              MSG = 'Lambert projection setup error'
              CALL M3EXIT( PNAME, 0, 0, MSG, 2 )
            ENDIF
         
c..get the x,y coordinates using lat/lon to lambert conversion
            IF( .NOT. LL2LAM( LONIN, LATIN, X, Y ) ) THEN
               MSG = 'Lat/Lon to Lambert to conversion error'
               CALL M3EXIT( PNAME, 0, 0, MSG, 2 )
            ENDIF         

            SITES(N)%LAMBX = X
            SITES(N)%LAMBY = Y

c..find the column location 
            SITES(N)%column  = 0
            DO C = 1, NCOLS3D
               XW = M3GRID % XORIG + FLOAT( C - 1 ) * M3GRID % XCELL 
               XE = XW + M3GRID % XCELL
               IF( X .GE. XW .AND. X .LT. XE ) SITES(N)%column = C
            ENDDO

c..find the row location 
            SITES(N)%row = 0
            DO R = 1, NROWS3D
               YS = M3GRID % YORIG + FLOAT( R - 1 ) * M3GRID % YCELL 
               YN = YS + M3GRID % YCELL
               IF( Y .GE. YS .AND. Y .LT. YN ) SITES(N)%row = R
            ENDDO
            
          ELSEIF ( M3GRID % GDTYP . eq . 6 ) THEN ! Polar Stereographic


            
            IF( .NOT. SETPOL( SNGL( M3GRID % P_ALP ),     !  first, initialize
     &                  SNGL( M3GRID % P_BET ),        !  for LL2POL()
     &                  SNGL( M3GRID % P_GAM ),
     &                  SNGL( M3GRID % XCENT ), 
     &                  SNGL( M3GRID % YCENT ) ) ) THEN
              MSG = 'Polar stereographic projection setup error'
              CALL M3EXIT( PNAME, 0, 0, MSG, 2 )
            ENDIF
         
c..get the x,y coordinates using lat/lon to lambert conversion
            IF( .NOT. LL2POL( LONIN, LATIN, X, Y ) ) THEN
               MSG = 'Lat/Lon to polar stereographic to conversion error'
               CALL M3EXIT( PNAME, 0, 0, MSG, 2 )
            ENDIF         

            SITES(N)%LAMBX = X
            SITES(N)%LAMBY = Y

c..find the column location 
            SITES(N)%column  = 0
            DO C = 1, NCOLS3D
               XW = M3GRID % XORIG + FLOAT( C - 1 ) * M3GRID % XCELL 
               XE = XW + M3GRID % XCELL
               IF( X .GE. XW .AND. X .LT. XE ) SITES(N)%column = C
            ENDDO

c..find the row location 
            SITES(N)%row = 0
            DO R = 1, NROWS3D
               YS = M3GRID % YORIG + FLOAT( R - 1 ) * M3GRID % YCELL 
               YN = YS + M3GRID % YCELL
               IF( Y .GE. YS .AND. Y .LT. YN ) SITES(N)%row = R
            ENDDO
            
          ELSEIF ( M3GRID % GDTYP . eq . 7 ) THEN ! Equatorial mercator


            
            IF( .NOT. SETEQM( SNGL( M3GRID % P_ALP ),     !  first, initialize
     &                  SNGL( M3GRID % P_BET ),        !  for LL2EQM()
     &                  SNGL( M3GRID % P_GAM ),
     &                  SNGL( M3GRID % XCENT ), 
     &                  SNGL( M3GRID % YCENT ) ) ) THEN
              MSG = 'Equatorial mercator projection setup error'
              CALL M3EXIT( PNAME, 0, 0, MSG, 2 )
            ENDIF
         
c..get the x,y coordinates using lat/lon to equatorial mercator conversion
            IF( .NOT. LL2EQM( LONIN, LATIN, X, Y ) ) THEN
               MSG = 'Lat/Lon to equatorial mercator to conversion error'
               CALL M3EXIT( PNAME, 0, 0, MSG, 2 )
            ENDIF         

            SITES(N)%LAMBX = X
            SITES(N)%LAMBY = Y

c..find the column location 
            SITES(N)%column  = 0
            DO C = 1, NCOLS3D
               XW = M3GRID % XORIG + FLOAT( C - 1 ) * M3GRID % XCELL 
               XE = XW + M3GRID % XCELL
               IF( X .GE. XW .AND. X .LT. XE ) SITES(N)%column = C
            ENDDO

c..find the row location 
            SITES(N)%row = 0
            DO R = 1, NROWS3D
               YS = M3GRID % YORIG + FLOAT( R - 1 ) * M3GRID % YCELL 
               YN = YS + M3GRID % YCELL
               IF( Y .GE. YS .AND. Y .LT. YN ) SITES(N)%row = R
            ENDDO
            
         ELSEIF ( M3GRID % GDTYP . eq . 1 ) THEN ! LAT/LON GRID
         
            X = LONIN
            Y = LATIN
            
            SITES(N)%LAMBX = X
            SITES(N)%LAMBY = Y

c..find the column location 
            SITES(N)%column  = 0
            DO C = 1, NCOLS3D
               XW = M3GRID % XORIG + FLOAT( C - 1 ) * M3GRID % XCELL 
               XE = XW + M3GRID % XCELL
               IF( X .GE. XW .AND. X .LT. XE ) SITES(N)%column = C
            ENDDO

c..find the row location 
            SITES(N)%row = 0
            DO R = 1, NROWS3D
               YS = M3GRID % YORIG + FLOAT( R - 1 ) * M3GRID % YCELL 
               YN = YS + M3GRID % YCELL
               IF( Y .GE. YS .AND. Y .LT. YN ) SITES(N)%row = R
            ENDDO
         
         ELSE

               MSG = 'Unknown grid type'
               CALL M3EXIT( PNAME, 0, 0, MSG, 2 )
           
         ENDIF

         ENDDO

         RETURN

92000 FORMAT( '! INPUT M3 FILE ', I2, ': ', A )
92020 FORMAT( '! INPUT SITE FILE: ', A )
92040 FORMAT( '! SITEID STATE REGION LON LAT COL ROW X Y ' )
92060 FORMAT( A9, 1X, A2, 1X, A2, F8.3, 1X, F8.3, 1X, I3, 1X, I3, 2F15.2 )

         END SUBROUTINE SET_SITE_LOC

C  function to find the index number of a site
      Integer Function getSiteNumber( site ) result (siteNo)

      CHARACTER*(*) :: site

      Integer :: i

      siteNo = -1
 
      do i=1,size(SITES)
       if( TRIM(site) .eq. TRIM(SITES(I)%site_id) ) Then
         siteNo = i
         return
         endif
       Enddo 
      
      Write(*,'('' cannot find match for site '',a)') site
      Return
      End Function getSiteNumber


C  function to return the site id from it's index number
      Character*(20) Function getSiteId( siteNo ) result (siteId)

      Integer siteNo

      siteId = '-999'

      if(siteNo.gt.0 .and. siteNo .le. size(SITES)) Then
        siteId = TRIM(SITES(siteNo)%site_id) 
        endif

      Return
      End Function getSiteId



C  function to return latitude value for site n
      Real Function getLatitude( id ) result (latitude)

      Integer id

      if(id .le. size(SITES)) Then
        latitude = SITES(id)%LAT
      else
        latitude = -99
        endif

      Return
      End Function getLatitude
      

C  function to return longitude value for site n
      Real Function getLongitude( id ) result (longitude)

      Integer id
 
      if(id .le. size(SITES)) Then
        longitude = SITES(id)%LON
      else
        longitude = -99
        endif

      Return
      End Function getLongitude

C  function to return Lambert X value for site n
      Real Function getLambertX( id ) result (X)

      Integer id

      if(id .le. size(SITES)) Then
        X = SITES(id)%LAMBX
      else
        X = -99
        endif

      Return
      End Function getLambertX 

C  function to return Lambert Y value for site n
      Real Function getLambertY( id ) result (Y)

      Integer id

      if(id .le. size(SITES)) Then
        Y = SITES(id)%LAMBY
      else
        Y = -99
        endif

      Return
      End Function getLambertY 




C  function to return time zone value for site n
      Integer Function getTimeZone( id ) result (tz)

      Integer id
 
      if(id .le. size(SITES)) Then
        tz = SITES(id)%TZ
      else
        tz = 0
        endif

      Return
      End Function getTimeZone


C  function to return the grid column for site n
      Integer Function getColumn( id ) result (column)

      Integer id
 
      if(id.gt.0 .and. id.le.size(SITES)) Then
        column = SITES(id)%column
      else
        column = 0
        endif

      Return
      End Function getColumn

C  function to return the grid row for site n
      Integer Function getRow( id ) result (row)

      Integer id
 
      if(id.gt.0 .and. id.le.size(SITES)) Then
        row = SITES(id)%row
      else
        row = 0
        endif

      Return
      End Function getRow

C  function to return the number of sites    
      Integer Function getNumSites() result (nsites)

      nsites = 0
      if( Allocated(SITES) ) nsites = size(SITES)

      Return
      End Function getNumSites

      END MODULE SITE_DATA
