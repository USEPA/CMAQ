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


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  function to compute time zone offset from lat/lon 
C
C  The routine requires file "tz.csv" for timezone data
C  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Integer Function getTZ(longitude, latitude) result(tzoffset)

      Implicit None

      ! defined type for line curve
      TYPE LINE
         Integer npts
         Character*(20) name
         Real offset
         Real xmin, xmax, ymin, ymax
         Real, Pointer :: x(:) 
         Real, Pointer :: y(:)
      End TYPE LINE   

      ! defined type for array points used for different size arrays
      TYPE POINTS
         Real, Pointer :: x(:,:)
         Real, Pointer :: y(:,:)
      End TYPE POINTS


      ! arguments
      Real latitude, longitude

      ! function
      Real getValue
      Logical inArea

      ! default Timezone data file
      Character*(256), Parameter :: defaultTZ = '/home/shq/src/writesite/tz.csv'      

      !  saved variables
      Logical, save :: firstime=.true.
      Integer, save :: nlines
      TYPE (LINE),allocatable,save :: lines(:)
      TYPE (POINTS),allocatable,save :: pts(:)  !pointer used for differsize arrays

      ! create different size arrays for storing line points
      Real,target,allocatable,save :: lon1(:,:)
      Real,target,allocatable,save :: lat1(:,:)

      Real,target,allocatable,save :: lon2(:,:)
      Real,target,allocatable,save :: lat2(:,:)

      Real,target,allocatable,save :: lon3(:,:)
      Real,target,allocatable,save :: lat3(:,:)

      Real,target,allocatable,save :: lon4(:,:)
      Real,target,allocatable,save :: lat4(:,:)

      Real,target,allocatable,save :: lon5(:,:)
      Real,target,allocatable,save :: lat5(:,:)

      Real,target,allocatable,save :: lon6(:,:)
      Real,target,allocatable,save :: lat6(:,:)

      Real,target,allocatable,save :: lon7(:,:)
      Real,target,allocatable,save :: lat7(:,:)

      Real,target,allocatable,save :: lon8(:,:)
      Real,target,allocatable,save :: lat8(:,:)

      Real,target,allocatable,save :: lon9(:,:)
      Real,target,allocatable,save :: lat9(:,:)


      ! local variables
      Character*(256) tzfile
      Character*(120) record
      Character*(32) field
      Integer npts
      Integer i, j, status
      Real lat(15000), lon(15000), long  
      Integer count(9), sizes(9)
      Integer nx, nfound, nsort
      Real xsec(500), ysec(500), temp

      Integer :: lfn=15

      Data sizes/50,100,200,500,1000,2000,5000,10000,15000/

      if( firstime ) then

        firstime = .false.

        ! get tz file name
        CALL ENVSTR('TZFILE','Time zone data file',defaultTZ,tzFile,status)

        ! open tz boundary file
        open(unit=lfn,file=tzFile,status='OLD',iostat=status)
        if(status.ne.0) then
          write(*,'(//''**ERROR** Cannot open time zone data file:'',a,//)') TRIM(tzFile)
          Stop
          endif

        count = 0
        nlines = 0
      
        ! read tz data file and count number of lines needed
        do 
          read(lfn,'(a)',iostat=status) record
          if(status.ne.0) exit

          Call getField( record, ',', 1, field ) 
          read(field,'(i16)') npts   
  
          nlines = nlines+1

          ! update count
          do i=1,SIZE(count)
            if(npts.le.sizes(i)) then
              count(i) = count(i) + 1
              EXIT
              endif 
            enddo

          ! read point records
          do i=1,npts
            read(lfn,'(a)',iostat=status) record
            if(status.ne.0) then
              write(*,'(''Read error on record:'',a)') trim(record)
              stop
              endif 
            enddo
          enddo
        rewind(lfn)

        ! allocate lines and pointers
        Allocate( lines(nlines) )
        Allocate( pts(SIZE(count)) )

        ! asign pointers to size arrays
        pts(1)%x => lon1
        pts(1)%y => lat1
        pts(2)%x => lon2
        pts(2)%y => lat2
        pts(3)%x => lon3
        pts(3)%y => lat3
        pts(4)%x => lon4
        pts(4)%y => lat4
        pts(5)%x => lon5
        pts(5)%y => lat5
        pts(6)%x => lon6
        pts(6)%y => lat6
        pts(7)%x => lon7
        pts(7)%y => lat7
        pts(8)%x => lon8
        pts(8)%y => lat8
        pts(9)%x => lon9
        pts(9)%y => lat9

        ! allocate point arrays for each size using pointers
        do i=1,SIZE(count)
          Allocate( pts(i)%x(sizes(i),count(i)) )
          Allocate( pts(i)%y(sizes(i),count(i)) )
          enddo

        ! read each line and set pointers
        nlines = 0 
        count = 0
        do  
          read(lfn,'(a)',iostat=status) record
          if(status.ne.0) exit
          nlines = nlines+1

          Call getField( record, ',', 1, field ) 
          read(field,'(i16)') lines(nlines)%npts   
          Call getField( record, ',', 2, field ) 
          read(field,'(f16.0)') lines(nlines)%offset  
          Call getField( record, ',', 3, field ) 
          lines(nlines)%name = field
              
          ! read points into lat and lon arrays 
          do i=1,lines(nlines)%npts
            read(lfn,'(a)',iostat=status) record
            if(status.ne.0) then
              write(*,'(''Read error on record:'',a)') trim(record)
              stop
              endif 
            Call getField( record, ',', 1, field ) 
            read(field,'(f32.0)') lon(i)
            Call getField( record, ',', 2, field ) 
            read(field,'(f32.0)') lat(i)
            enddo

          ! compute min and max for each line
          lines(nlines)%xmin = lon(1)
          lines(nlines)%xmax = lon(1) 
          lines(nlines)%ymin = lat(1)
          lines(nlines)%ymax = lat(1) 
          do i=1,lines(nlines)%npts
            if(lon(i) .lt. lines(nlines)%xmin) lines(nlines)%xmin = lon(i)
            if(lon(i) .gt. lines(nlines)%xmax) lines(nlines)%xmax = lon(i)
            if(lat(i) .lt. lines(nlines)%ymin) lines(nlines)%ymin = lat(i)
            if(lat(i) .gt. lines(nlines)%ymax) lines(nlines)%ymax = lat(i)
            enddo

          ! copy arrays to correct size array using pointers
          do i=1,SIZE(count)
            if(lines(nlines)%npts.le.sizes(i)) then
              count(i) = count(i) + 1
              pts(i)%x(:,count(i)) = lon(1:lines(nlines)%npts)
              pts(i)%y(:,count(i)) = lat(1:lines(nlines)%npts)
              lines(nlines)%x => pts(i)%x(:,count(i))
              lines(nlines)%y => pts(i)%y(:,count(i))
              EXIT
              endif
            enddo

          enddo
        close(lfn)
        endif  ! firstime
       

       ! find all intersecting points at longitude
       nx = 0
       do i = 1, nlines
         if(lines(i)%xmin.le.longitude .and. longitude.le.lines(i)%xmax) then
           Call getValues(longitude, lines(i)%npts, lines(i)%x, lines(i)%y, nfound, lat) 

           ! check if point lies in line area
           if( nfound.ge.2 .and. lines(i)%ymin.le.latitude .and. latitude.le.lines(i)%ymax ) then
             if( inArea( latitude, nfound, lat) ) then
               !write(*,'(''point lies in area'',i5)') i
               tzoffset = -lines(i)%offset
               return              
               endif
             endif

           ! add lat values to array
           do j=1,nfound
             nx = nx+1
             xsec(nx) = lat(j)  
             ysec(nx) = lines(i)%offset
             enddo        
           endif
         enddo 

       ! if more than 1 intersecting point found, sort them
       if( nx.gt.1 ) then
         nsort = nx
         do
           nfound=0
           nsort = nsort-1
           do i=1,nsort
             if( xsec(i).gt.xsec(i+1) ) then
               temp = xsec(i)
               xsec(i) = xsec(i+1)
               xsec(i+1) = temp
               temp = ysec(i)
               ysec(i) = ysec(i+1)
               ysec(i+1) = temp
               nfound=1
               endif
             enddo
             if(nfound.eq.0) EXIT
           enddo  

         ! find offset for latitude
         do i=1,nx-1
           if(latitude.ge.xsec(i) .and. latitude.le.xsec(i+1)) then 
             if( ysec(i).eq.ysec(i+1) .and. xsec(i+1)-xsec(i).lt.4.0  ) then
               tzoffset = -ysec(i)
               !write(*,'(''point lies between areas'')')
               return
               endif
             endif
           enddo    

         endif
          
       ! compute tzoffset from longitude
       long = abs(longitude)
       tzoffset = (long+7.5) / 15
       if(longitude.gt.0) tzoffset = -tzoffset
       !write(*,'(''offset computed by longitude'')')
       return
       end Function getTZ   
 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Subroutine to get intersecting values from array
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       Subroutine getValues(xx, npts, x, y, nfound, yy)
       
       Implicit None
       
       ! arguments
       Real xx
       Integer npts
       Real x(*)
       Real y(*)
       Integer nfound
       Real yy(*)

       Real slope
       Integer i
       nfound = 0

       do i=1,npts-1

         if( (xx.ge.x(i) .and. xx.lt.x(i+1)) .or.
     &       (xx.le.x(i) .and. xx.gt.x(i+1)) ) then   
           nfound = nfound+1
           slope = 1.0
           if( x(i).ne.x(i+1) ) slope = (y(i)-y(i+1)) / (x(i)-x(i+1))
           yy(nfound) = y(i) + slope * (xx - x(i))
           
           endif
         enddo

        return
        end Subroutine getValues

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  function to get value from array table
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       Real Function getValue(xx, npts, x, y) result(yy)
       
       Implicit None
       
       Real xx
       Integer npts
       Real x(*)
       Real y(*)

       Real slope

       Integer i

       do i=1,npts-1

         if( (xx.ge.x(i) .and. xx.lt.x(i+1)) .or.
     &       (xx.le.x(i) .and. xx.gt.x(i+1)) ) then   

           slope = 1.0
           if( x(i).ne.x(i+1) ) slope = (y(i)-y(i+1)) / (x(i)-x(i+1))
           yy = y(i) + slope * (xx - x(i))
           return
           endif
         enddo

        yy = 0.0
        return
        end Function getValue
 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  function to check if latitude is in line area
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       Logical Function inArea( x, nval, values) result(result)
       
       Implicit None
       
       ! arguments
       Real x
       Integer nval
       Real values(*)

       ! local variables
       Real nsort
       Integer i
       logical sorted
       Real temp

       result = .false.
 
       ! sort the values
       nsort = nval
       do
         sorted = .true.
         nsort = nsort-1
         do i=1,nsort
           if( values(i).gt.values(i+1) ) then
             temp = values(i)
             values(i) = values(i+1)
             values(i+1) = temp
             sorted=.false.
             endif
           enddo
           if(sorted) EXIT
         enddo  

       ! check if x is between values
       do i=1,nval-1,2
         if( x.ge.values(i) .and. x.le.values(i+1) ) result = .true.
         enddo

        return
        end Function inArea
