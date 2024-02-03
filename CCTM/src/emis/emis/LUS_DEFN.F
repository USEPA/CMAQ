
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

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      module lus_defn
C Land Use/Cover Schema

C Revision History:
C   10 Nov 2011: J.Young - Created
C   10 Dec 2011: J.Young - fix for two-way application (Tanya Spero`s help)
C    2 Feb 2019:  D. Wong - Implemented centralized I/O approach, removed all 
C                           MY_N clauses, with new re-structure of LUS_DEFN, 
C                           most of the data declaration has been moved to 
C                           lus_data_module (model_data_module.f)
C    3 Mar 2022: J. Willison - Remove ctm_wbdust_beld option

      implicit none

      public lus_init
      private

      contains

#ifdef mpas
         function lus_init( mminlu_in, landusef_in ) result ( success )
#else
         function lus_init( jdate, jtime ) result ( success )
#endif

         use hgrd_defn           ! horizontal domain specifications
#ifdef twoway
         use twoway_data_module, only : mminlu, num_land_cat
#endif
         use utilio_defn
         use lus_data_module

         implicit none

#ifdef mpas
         character (*), intent( in ) :: mminlu_in
         real, intent( in ) :: landusef_in(:,:)

         integer, allocatable :: lu_ind(:)
         integer :: j
         logical :: found

         character (16) :: mminlu
         integer :: num_land_cat
#else
         integer, intent( in ) :: jdate      ! current model date, coded YYYYDDD
         integer, intent( in ) :: jtime      ! current model time, coded HHMMSS
#endif

C Includes:
         include SUBST_FILES_ID  ! file name parameters

         logical :: success

         integer :: gxoff, gyoff             ! global origin offset from file
         integer, save :: strtcol_01, endcol_01, strtrow_01, endrow_01
         integer, save :: strtcol_02, endcol_02, strtrow_02, endrow_02

         character( 16 ) :: pname = 'LUS_INIT'
         character( 128 ) :: vardesc = ' '
         character( 128 ) :: xmsg = ' '
         character( 24 ) :: strg = 'incorrect num_land_cat, '
         real( 8 ) :: tland
         integer i, c, r, err

#ifdef mpas
         mminlu = mminlu_in
         if (mminlu .eq. 'USGS') then
            num_land_cat = 24
         else if (mminlu .eq. 'NLCD40') then
            num_land_cat = size(landusef_in,1)
         end if
         dust_land_scheme = mminlu_in
#endif

         success = .true.

            ladut = 100.0 * ladut   ! array assignment
            lut   = 100.0 * lut     ! array assignment

C Merge fractional landuse categories into 4 groups for dust transport factors
         select case( dust_land_scheme )

            case( 'USGS24' )
            do r = 1, nrows
            do c = 1, ncols
C           water & barren surrounding
               uland( c,r,1 ) = lut( c,r,8 )   ! Shrubland
     &                        + lut( c,r,16 )  ! Water
     &                        + lut( c,r,19 )  ! Barren or Sparsely Vegetated
     &                        + lut( c,r,22 )  ! Mixed Tundra
     &                        + lut( c,r,23 )  ! Bare Ground Tundra
     &                        + lut( c,r,24 )  ! Snow and Ice

C           grass & crop surrounding
               uland( c,r,2 ) = lut( c,r,2 )   ! Dryland Cropland and Pasture
     &                        + lut( c,r,3 )   ! Irrigated Cropland and Pasture
     &                        + lut( c,r,4 )   ! Mixed Dryland and Irrigated Cropland and Pasture
     &                        + lut( c,r,5 )   ! Cropland-Grassland Mosaic
     &                        + lut( c,r,6 )   ! Cropland-Woodland Mosaic
     &                        + lut( c,r,7 )   ! Grassland
     &                        + lut( c,r,9 )   ! Mixed Shrubland-Grassland
     &                        + lut( c,r,10 )  ! Savannah
     &                        + lut( c,r,17 )  ! Herbaceous Wetland
     &                        + lut( c,r,20 )  ! Herbaceous Tundra

C           building surrounding
               uland( c,r,3 ) = lut( c,r,1 )   ! Urban Land

C           forest surrounding
               uland( c,r,4 ) = lut( c,r,11 )  ! Deciduous Broadleaf Forest
     &                        + lut( c,r,12 )  ! Deciduous Needleleaf Forest
     &                        + lut( c,r,13 )  ! Evergreen Broadleaf Forest
     &                        + lut( c,r,14 )  ! Evergreen Needleleaf Forest
     &                        + lut( c,r,15 )  ! Mixed Forest
     &                        + lut( c,r,18 )  ! Wooded Wetland
     &                        + lut( c,r,21 )  ! Wooded Tundra
            end do
            end do

!           case( 'MODIS' )
!           do r = 1, nrows
!           do c = 1, ncols
!              uland( c,r,1 ) = lut( c,r,> )
!           end do
!           end do

            case( 'NLCD40' )
            do r = 1, nrows
            do c = 1, ncols
C           water & barren surrounding
               uland( c,r,1 ) = lut( c,r,21 )   ! Open Water
     &                        + lut( c,r,22 )   ! Perennial Ice-Snow
     &                        + lut( c,r,27 )   ! Barren Land (Rock-Sand-Clay)
     &                        + lut( c,r,31 )   ! Dwarf Scrub
     &                        + lut( c,r,32 )   ! Shrub-Scrub
     &                        + lut( c,r,15 )   ! Permanent Snow and Ice
     &                        + lut( c,r,16 )   ! Barren or Sparsely Vegetated
     &                        + lut( c,r,17 )   ! IGBP Water

C           grass & crop surrounding
               uland( c,r,2 ) = lut( c,r,33 )  ! Grassland-Herbaceous
     &                        + lut( c,r,34 )  ! Sedge-Herbaceous
     &                        + lut( c,r,35 )  ! Lichens
     &                        + lut( c,r,36 )  ! Moss
     &                        + lut( c,r,37 )  ! Pasture-Hay
     &                        + lut( c,r,38 )  ! Cultivated Crops
     &                        + lut( c,r,6  )  ! Closed Shrublands
     &                        + lut( c,r,7  )  ! Open Shrublands
     &                        + lut( c,r,9  )  ! Savannas
     &                        + lut( c,r,10 )  ! Grasslands
     &                        + lut( c,r,12 )  ! Croplands
     &                        + lut( c,r,14 )  ! Cropland-Natural Vegetation Mosaic

C           building surrounding
               uland( c,r,3 ) = lut( c,r,24 )   ! Developed Low Intensity
     &                        + lut( c,r,25 )   ! Developed Medium Intensity
     &                        + lut( c,r,26 )   ! Developed High Intensity
     &                        + lut( c,r,13 )   ! Urban and Built Up

C           forest surrounding
               uland( c,r,4 ) = lut( c,r,28 )  ! Deciduous Forest
     &                        + lut( c,r,29 )  ! Evergreen Forest
     &                        + lut( c,r,30 )  ! Mixed Forest
     &                        + lut( c,r,39 )  ! Woody Wetlands
     &                        + lut( c,r,40 )  ! Emergent Herbaceous Wetlands
     &                        + lut( c,r,1  )  ! Evergreen Needleleaf Forest
     &                        + lut( c,r,2  )  ! Evergreen Broadleaf Forest
     &                        + lut( c,r,3  )  ! Deciduous Needleleaf Forest
     &                        + lut( c,r,4  )  ! Deciduous Broadleaf Forest
     &                        + lut( c,r,5  )  ! Mixed Forests
     &                        + lut( c,r,8  )  ! Woody Savannas
     &                        + lut( c,r,11 )  ! Permanent Wetlands
            end do
            end do

            case( 'NLCD-MODIS', 'NLCD50' )
            do r = 1, nrows
            do c = 1, ncols
C           water & barren surrounding
               uland( c,r,1 ) = lut( c,r,1 )   ! Open Water
     &                        + lut( c,r,2 )   ! Perennial Ice-Snow
     &                        + lut( c,r,7 )   ! Barren Land (Rock-Sand-Clay)
     &                        + lut( c,r,12 )  ! Dwarf Scrub
     &                        + lut( c,r,13 )  ! Shrub-Scrub
     &                        + lut( c,r,18 )  ! Tundra
     &                        + lut( c,r,31 )  ! Water
     &                        + lut( c,r,46 )  ! Permanent Snow and Ice
     &                        + lut( c,r,47 )  ! Barren or Sparsely Vegetated
     &                        + lut( c,r,48 )  ! IGBP Water

C           grass & crop surrounding
               uland( c,r,2 ) = lut( c,r,14 )  ! Grassland-Herbaceous
     &                        + lut( c,r,15 )  ! Sedge-Herbaceous
     &                        + lut( c,r,16 )  ! Lichens
     &                        + lut( c,r,17 )  ! Moss
     &                        + lut( c,r,19 )  ! Pasture-Hay
     &                        + lut( c,r,20 )  ! Cultivated Crops
     &                        + lut( c,r,37 )  ! Closed Shrublands
     &                        + lut( c,r,38 )  ! Open Shrublands
     &                        + lut( c,r,40 )  ! Savannas
     &                        + lut( c,r,41 )  ! Grasslands
     &                        + lut( c,r,43 )  ! Croplands
     &                        + lut( c,r,45 )  ! Cropland-Natural Vegetation Mosaic

C           building surrounding
               uland( c,r,3 ) = lut( c,r,4 )   ! Developed Low Intensity
     &                        + lut( c,r,5 )   ! Developed Medium Intensity
     &                        + lut( c,r,6 )   ! Developed High Intensity
     &                        + lut( c,r,44 )  ! Urban and Built Up

C           forest surrounding
               uland( c,r,4 ) = lut( c,r,9 )   ! Deciduous Forest
     &                        + lut( c,r,10 )  ! Evergreen Forest
     &                        + lut( c,r,11 )  ! Mixed Forest
     &                        + lut( c,r,21 )  ! Woody Wetlands
     &                        + lut( c,r,22 )  ! Palustrine Forested Wetland
     &                        + lut( c,r,24 )  ! Estuarine Forested Wetland
     &                        + lut( c,r,32 )  ! Evergreen Needleleaf Forest
     &                        + lut( c,r,33 )  ! Evergreen Broadleaf Forest
     &                        + lut( c,r,34 )  ! Deciduous Needleleaf Forest
     &                        + lut( c,r,35 )  ! Deciduous Broadleaf Forest
     &                        + lut( c,r,36 )  ! Mixed Forests
     &                        + lut( c,r,39 )  ! Woody Savannas
     &                        + lut( c,r,42 )  ! Permanent Wetlands
            end do
            end do

            case( 'MODIS_NOAH' )
            do r = 1, nrows
            do c = 1, ncols
C           water & barren surrounding
               uland( c,r,1 ) = lut( c,r,6 )   ! Closed Shrublands
     &                        + lut( c,r,7 )   ! Open Shrublands
     &                        + lut( c,r,15 )  ! Snow and Ice
     &                        + lut( c,r,16 )  ! Barren or Sparsely Vegetated
     &                        + lut( c,r,17 )  ! Water
     &                        + lut( c,r,19 )  ! Mixed Tundra
     &                        + lut( c,r,20 )  ! Barren Tundra

C           grass & crop surrounding
               uland( c,r,2 ) = lut( c,r,8 )   ! Woody Savannas
     &                        + lut( c,r,9 )   ! Savannas
     &                        + lut( c,r,10 )  ! Grasslands
     &                        + lut( c,r,12 )  ! Croplands
     &                        + lut( c,r,14 )  ! Cropland-Natural Vegetation Mosaic
     &                        + lut( c,r,18 )  ! Wooded Tundra

C           building surrounding
               uland( c,r,3 ) = lut( c,r,13 )  ! Urban and Built-Up

C           forest surrounding
               uland( c,r,4 ) = lut( c,r,1 )   ! Evergreen Needleleaf Forest
     &                        + lut( c,r,2 )   ! Evergreen Broadleaf Forest
     &                        + lut( c,r,3 )   ! Deciduous Needleleaf Forest
     &                        + lut( c,r,4 )   ! Deciduous Broadleaf Forest
     &                        + lut( c,r,5 )   ! Mixed Forests
     &                        + lut( c,r,11 )  ! Permanent Wetlands
            end do
            end do

         end select

         do r = 1, nrows
         do c = 1, ncols

            tland = 0.0d0
            do i = 1, 4
               uland( c,r,i ) = max( 0.0, min( 100.0, uland( c,r,i ) ) )
               tland = tland + real( uland( c,r,i ), 8 )
            end do

            if ( tland .gt. 100.0d0 ) then
#ifdef dbg_wbdust
               write( logdev,* ) ' >>> tland:', c, r, tland
#endif
               do i = 1, 4
                  uland( c,r,i ) = uland( c,r,i ) * real( 100.0d0 / tland, 4 )
               end do
            end if

         end do
         end do

         deallocate( lut )

         return
         end function lus_init

      end module lus_defn
