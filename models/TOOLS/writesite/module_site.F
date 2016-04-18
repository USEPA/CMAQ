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

C***********************************************************************
C
C  MODULE:  site ids and location        
C             
C***********************************************************************
      MODULE SITES     

      Logical ALLCELLS
      Integer nsites
      Character*(10) siteid(2000)
      Real   longitude(2000)
      Real   latitude(2000)
      Integer siteCol(2000)
      Integer siteRow(2000)

      contains

C  subroutine to open site file and load arrays     
      Subroutine loadSites()

      USE M3UTILIO

      IMPLICIT NONE

      ! functions
      Integer getNumberOfFields

      ! local variables
      Integer status
      Integer lfn
      Integer nflds
      Character*(512) sitefile
      Character*(512) record   
      Character*(16) field    
      Character*(1)  delimiter
      Logical        useColRow
      Real value
      Real x, y 

      lfn = 20
      ALLCELLS = .false.

      CALL ENVSTR( 'SITE_FILE', 'Name of site definition file', 'ALL', sitefile, status)
      if( sitefile.eq.'ALL') then
        ALLCELLS = .true.
        return
        endif

      CALL ENVSTR( 'DELIMITER', 'Delimiter character in site file', char(9), delimiter, status)

      useColRow = ENVYN( 'USECOLROW', 'Column/row in site file', .false., status)

      OPEN(unit=lfn, file=sitefile, status='old', iostat=status)
      IF( status .ne. 0 ) then
        write(*,'(''**ERROR** Cannot open SITE_FILE:'',a)') TRIM(sitefile)
        Stop
        endif

      ! read until site record found
      do
        read(lfn,'(a)',iostat=status) record
        if(status.ne.0) then
          write(*,'(''**ERROR** Cannot find site records in SITE_FILE:'',a)') TRIM(sitefile) 
          Stop 
          endif 

        nflds = getNumberOfFields( record, delimiter ) 
        if( nflds.lt.3 ) CYCLE

        Call getField( record, delimiter, 2, field ) 
        read(field,'(f16.0)',iostat=status) value
        if( status.ne.0 ) CYCLE
    
        Call getField( record, delimiter, 3, field ) 
        read(field,'(f16.0)',iostat=status) value
        if( status.ne.0 ) CYCLE

        !! site record found, backspace and exit loop   
        Backspace(lfn)
        EXIT 

        enddo

      ! read and load sites
      nsites = 0
      do 
        read(lfn,'(a)',iostat=status) record                                    
        if(status.ne.0) EXIT 

        nflds = getNumberOfFields( record, delimiter ) 
        if( nflds.lt.3 ) EXIT

        nsites = nsites + 1
        if( nsites.gt.SIZE(siteid) ) then
          write(*,'(''**ERROR** The number of sites exceed maximum'')')
          Stop                                                                  
          endif

        Call getField( record, delimiter, 1, siteid(nsites) ) 

        if( useColRow ) then    ! read col/row and compute lon/lat

          Call getField( record, delimiter, 2, field ) 
          read(field,'(i16)') siteCol(nsites)

          Call getField( record, delimiter, 3, field ) 
          read(field,'(i16)') siteRow(nsites)

          ! compute longitude and latitude for site
          x = XORIG3D + (siteCol(nsites)-0.5) * XCELL3D
          y = YORIG3D + (siteRow(nsites)-0.5) * YCELL3D

          Call toLL( GDTYP3D, x, y, longitude(nsites), latitude(nsites) )

        else   ! read lon/lat and compute col/row

          Call getField( record, delimiter, 2, field ) 
          read(field,'(f16.0)') longitude(nsites)
 
          Call getField( record, delimiter, 3, field ) 
          read(field,'(f16.0)') latitude(nsites)

          ! compute row and column for site
          Call toProj( GDTYP3D, longitude(nsites), latitude(nsites), x, y )

          siteCol(nsites) = INT( (x-XORIG3D)/XCELL3D ) + 1
          siteRow(nsites) = INT( (y-YORIG3D)/YCELL3D ) + 1
          endif

        enddo

      write(*,'(i5,'' sites loaded'')') nsites
      close(unit=lfn)

      return
      end Subroutine loadSites

      END MODULE SITES     
