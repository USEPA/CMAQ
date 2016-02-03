
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
! $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/rd_airs/module_sites.F,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Module sitedata

      TYPE SITE
         character*2 :: state_id
         character*3 :: county_id
         character*4 :: site_id
         INTEGER     :: land_use
         REAL        ::  LAT
         REAL        ::  LON
      END TYPE SITE

      TYPE ( SITE ), PRIVATE, ALLOCATABLE :: SITES(:)


      Type AIRS_ID
        Character*2   state_code
        Character*3   county_code
        Character*4   site_code
        Character*5   parameter
        Character*1   poc
        Character*1   duration 
        Integer       units
        Integer       method
        Integer       year
        Integer       month
        Integer       day
       End Type AIRS_ID

      CONTAINS

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc  subroutine to load SITES array with site file data cccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         SUBROUTINE LOADSITES( filename )

         IMPLICIT NONE       

         ! argument
         Character*(*) filename
    
         ! local variables
         integer lfn
         integer i 
         integer status
         integer noSites
         Character*132 record
         Character*80 field

C... external routines
         integer getParsedNumber

         Write(*,'('' reading sites from '',a)') TRIM(filename)

         !  open file
         lfn = 10
         OPEN(unit=lfn, file=filename, status='old', err=500)


         ! read site file to count number of sites
         nosites = 0
         Do 
           read(lfn,'(a)',end=100) record 
           if(record(1:2).eq.'AA') nosites = nosites+1
           Enddo

  100    rewind lfn

         !  allocate memory for SITES array
         Allocate( SITES(nosites) )

         ! fill array
         I = 0
         Do 
           read(lfn,'(a)', end=200) record

           if(record(1:2).eq.'AA') Then
             I = I+1
             Call getParsedField(record,'|', 3, SITES(I)%state_id, .false. )
             Call getParsedField(record,'|', 4, SITES(I)%county_id, .false. )
             Call getParsedField(record,'|', 5, SITES(I)%site_id, .false. )
             Call getParsedField(record,'|', 6, field, .false. )
              Read(field,'(f15.0)',iostat=status) SITES(I)%lat
             Call getParsedField(record,'|', 7, field, .false. )
              Read(field,'(f15.0)',iostat=status) SITES(I)%lon
             Call getParsedField(record,'|', 23, field, .false. )
               SITES(I)%land_use = 0
               if(field(1:1).eq.'U') SITES(I)%land_use = 1
               if(field(1:1).eq.'S') SITES(I)%land_use = 2
               if(field(1:1).eq.'R') SITES(I)%land_use = 3
             EndIf
           EndDo

  200    Write(*,'(i5,'' sites read from site file: '',a)') 
     &                                  nosites, TRIM(filename)
         Return

  500    Write(*,'(''**ERROR** Cannot open or read site file:'',a)') 
     &         TRIM(filename)
         Stop
         End SUBROUTINE LOADSITES
         

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc  subroutine to return lat, long given site id       cccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         SUBROUTINE GETLL( stateID, countyID, siteID, luse, lat, lon )

         IMPLICIT NONE       

         ! arguments
         character *2 stateID
         character *3 countyID
         character *4 siteID
         Integer luse
         Real lat, lon
    
         ! local variables
         integer i 

         Do I=1, SIZE(SITES)
           if( SITES(I)%state_id .ne. stateID  ) CYCLE
           if( SITES(I)%county_id .ne. countyID  ) CYCLE
           if( SITES(I)%site_id .ne. siteID  ) CYCLE

           lat = SITES(I)%lat
           lon = SITES(I)%lon
           luse = SITES(I)%land_use
           Return  
           ENDDO

         ! no matching site found, return lat,lon = -99
         lat = -99.0
         lon = -99.0
         luse = -99 
         Write(*,'(''No site found for '',a,1x,a,1x,a)') stateID, countyID, siteID
         Return

         End SUBROUTINE GETLL


      END MODULE sitedata
