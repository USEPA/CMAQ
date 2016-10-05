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


C*************************************************************************
C
C  MODULE: defines site data
C             
C*************************************************************************
      MODULE SITE_DATA

      Type METSITE
        Integer        :: siteid
        Real           :: latitude
        Real           :: longitude
        End Type METSITE


      Integer         :: NSITES
      Type (METSITE)  :: sites(5000)

      CONTAINS


C*************************************************************************                
C   routine to add a new site to the sites list
C*************************************************************************                
      Subroutine addSite2( id, column, row )
         
         USE M3UTILIO
                                                                                 
         IMPLICIT NONE                                                                    

         Integer id, column, row

         REAL   X               ! x-coordinate for lambert projection
         REAL   Y               ! y-coordinate for lambert projection
         REAL   LONGITUDE       
         REAL   LATITUDE       

 
         !  set lambert projection
         IF( .NOT. SETLAM( SNGL(P_ALP3D), SNGL(P_BET3D), SNGL(P_GAM3D),
     &                    SNGL(XCENT3D), SNGL(YCENT3D) ) ) THEN
           CALL M3EXIT( 'O3TOIOAPI', 0, 0, 'Lambert projection setup error', 2 )
           ENDIF


         X = XORIG3D + (column-0.5) * XCELL3D
         Y = YORIG3D + (row-0.5) * YCELL3D

         !..get the lat/lon coordinates using lambert conversion
         IF( .NOT. LAM2LL( X, Y, longitude, latitude) ) THEN
           CALL M3EXIT( 'O3TOIOAPI', 0, 0,
     &                      'Lat/Lon to Lambert to conversion error', 2 )
           endif           

         Call addSite(id, longitude, latitude)
        
         RETURN
         END SUBROUTINE addSite2     

C*************************************************************************
C   routine to find the index number of a site 
C*************************************************************************
      Integer Function getSiteNumber( id ) result (siteNo)

         IMPLICIT NONE     

         Integer :: id
         Integer :: i

         siteNo = -1
 
         do i=1,NSITES
         if( id .eq. sites(i)%siteid ) then
           siteNo = i
           return
           endif
           Enddo 
      
         Return
         End Function getSiteNumber

C*************************************************************************                
C   routine to add a new site to the sites list
C*************************************************************************                
      Subroutine addSite( id, lon, lat )
                                                                                          
         IMPLICIT NONE                                                                    
                                                                                          
         Integer       :: id           
         Real          :: lon                                                   
         Real          :: lat                                                   

         if( NSITES .lt. SIZE(sites) ) then
           NSITES = NSITES + 1
           sites(NSITES)%siteid = id 
           sites(NSITES)%longitude = lon
           sites(NSITES)%latitude = lat
         else
           write(*,'(''**ERROR** site '',i10,
     &           '' Cannot be added, Maximum size exceeded'')') id
           endif

         return
         End Subroutine addSite
   
      END MODULE SITE_DATA
