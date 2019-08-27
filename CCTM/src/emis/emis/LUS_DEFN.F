
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

      implicit none

      public lus_init
      private

      contains

         function lus_init( jdate, jtime ) result ( success )

         use hgrd_defn           ! horizontal domain specifications
#ifdef twoway
         use twoway_data_module, only : mminlu, num_land_cat
#endif
         use utilio_defn
         use lus_data_module

         implicit none

C Includes:
         include SUBST_FILES_ID  ! file name parameters

         integer, intent( in ) :: jdate      ! current model date, coded YYYYDDD
         integer, intent( in ) :: jtime      ! current model time, coded HHMMSS
         logical :: success

         integer :: gxoff, gyoff             ! global origin offset from file
         integer, save :: strtcol_01, endcol_01, strtrow_01, endrow_01
         integer, save :: strtcol_02, endcol_02, strtrow_02, endrow_02

         character( 16 ) :: ctm_wbdust_beld = 'CTM_WBDUST_BELD'
         character( 16 ) :: pname = 'LUS_INIT'
         character( 128 ) :: vardesc = ' '
         character( 128 ) :: xmsg = ' '
         character( 24 ) :: strg = 'incorrect num_land_cat, '
         real( 8 ) :: tland
         integer i, c, r, err

         success = .true.

         if ( .not. isbeld ) then   ! transform ladut & lut to %
            ladut = 100.0 * ladut   ! array assignment
            lut   = 100.0 * lut     ! array assignment
         end if

C Merge fractional landuse categories into 4 groups for dust transport factors
         select case( dust_land_scheme )

            case( 'BELD3' )
            do r = 1, nrows
            do c = 1, ncols
C           water & barren surrounding
               uland( c,r,1 ) = lut( c,r,7 )   ! USGS_shrubland
     &                        + lut( c,r,14 )  ! USGS_water
     &                        + lut( c,r,16 )  ! USGS_sprsbarren
     &                        + lut( c,r,18 )  ! USGS_mxtundra
     &                        + lut( c,r,19 )  ! USGS_snowice
C           grass & crop surrounding
               uland( c,r,2 ) = lut( c,r,6 )   ! USGS_grassland
     &                        + lut( c,r,8 )   ! USGS_shrubgrass
     &                        + lut( c,r,9 )   ! USGS_savanna
     &                        + lut( c,r,17 )  ! USGS_woodtundr
     &                        + lut( c,r,2 )   ! USGS_drycrop
     &                        + lut( c,r,3 )   ! USGS_irrcrop
     &                        + lut( c,r,4 )   ! USGS_cropgrass
     &                        + lut( c,r,5 )   ! USGS_cropwdlnd
C           building surrounding
               uland( c,r,3 ) = lut( c,r,1 )   ! USGS_urban
C           forest surrounding
               uland( c,r,4 ) = lut( c,r,10 )  ! USGS_decidforest
     &                        + lut( c,r,11 )  ! USGS_evbrdleaf
     &                        + lut( c,r,12 )  ! USGS_coniferfor
     &                        + lut( c,r,13 )  ! USGS_mxforest
     &                        + lut( c,r,15 )  ! USGS_wetwoods
     &                        + lut( c,r,20 )  ! FOREST  (dust_lu_2)
            end do
            end do

            case( 'BELD4' )   ! NLCD is national, MODIS is outside U.S.
            do r = 1, nrows
            do c = 1, ncols
C           water & barren surrounding
               uland( c,r,1 ) = lut( c,r,1 )   ! NLCD_11 Open Water
     &                        + lut( c,r,2 )   ! NLCD_12 Perennial Ice/Snow
     &                        + lut( c,r,7 )   ! NLCD_31 Barren Land (Rock/Sand/Clay)
     &                        + lut( c,r,12 )  ! NLCD_51 Dwarf Scrub
     &                        + lut( c,r,13 )  ! NLCD_52 Shrub/Scrub
     &                        + lut( c,r,18 )  ! NLCD_75 Tundra
     &                        + lut( c,r,31 )  ! MODIS_0 water
     &                        + lut( c,r,46 )  ! MODIS_15 permanent snow and ice
     &                        + lut( c,r,47 )  ! MODIS_16 barren or sparsely vegetated
     &                        + lut( c,r,48 )  ! MODIS_17 IGBP water

C           grass & crop surrounding
               uland( c,r,2 ) = lut( c,r,14 )  ! NLCD_71 Grassland/Herbaceous
     &                        + lut( c,r,15 )  ! NLCD_72 Sedge/Herbaceous
     &                        + lut( c,r,16 )  ! NLCD_73 Lichens
     &                        + lut( c,r,17 )  ! NLCD_74 Moss
     &                        + lut( c,r,19 )  ! NLCD_81 Pasture/Hay
     &                        + lut( c,r,20 )  ! NLCD_82 Cultivated Crops
     &                        + lut( c,r,37 )  ! MODIS_6 closed shrublands
     &                        + lut( c,r,38 )  ! MODIS_7 open shrublands
     &                        + lut( c,r,40 )  ! MODIS_9 savannas
     &                        + lut( c,r,41 )  ! MODIS_10 grasslands
     &                        + lut( c,r,43 )  ! MODIS_12 croplands
     &                        + lut( c,r,45 )  ! MODIS_14 cropland / natural vegetation mosaic

C           building surrounding
               uland( c,r,3 ) = lut( c,r,4 )   ! NLCD_22 Developed Low Intensity
     &                        + lut( c,r,5 )   ! NLCD_23 Developed Medium Intensity
     &                        + lut( c,r,6 )   ! NLCD_24 Developed High Intensity
     &                        + lut( c,r,44 )  ! MODIS_13 urban and built up

C           forest surrounding
               uland( c,r,4 ) = lut( c,r,9 )   ! NLCD_41 Deciduous Forest
     &                        + lut( c,r,10 )  ! NLCD_42 Evergreen Forest
     &                        + lut( c,r,11 )  ! NLCD_43 Mixed Forest
     &                        + lut( c,r,21 )  ! NLCD_90 Woody Wetlands
     &                        + lut( c,r,22 )  ! NLCD_91 Palustrine Forested Wetland
     &                        + lut( c,r,24 )  ! NLCD_93 Estuarine Forested Wetland
     &                        + lut( c,r,32 )  ! MODIS_1 evergreen needleleaf forest
     &                        + lut( c,r,33 )  ! MODIS_2 evergreen broadleaf forest
     &                        + lut( c,r,34 )  ! MODIS_3 deciduous needleleaf forest
     &                        + lut( c,r,35 )  ! MODIS_4 deciduous broadleaf forest
     &                        + lut( c,r,36 )  ! MODIS_5 mixed forests
     &                        + lut( c,r,39 )  ! MODIS_8 woody savannas
     &                        + lut( c,r,42 )  ! MODIS_11 permanent wetlands

C Not used:
C               3  NLCD_21 Developed Open Space
C               8  NLCD_32 Unconsolidated Shore
C              23  NLCD_92 Palustrine Scrub/Shrub Wetland
C              25  NLCD_94 Estuarine Scrub/Shrub Wetland
C              26  NLCD_95 Emergent Herbaceous Wetlands
C              27  NLCD_96 Palustrine Emergent Wetland
C              28  NLCD_97 Estuarine Emergent Wetland
C              29  NLCD_98 Palustrine Aquatic Bed
C              30  NLCD_99 Estuarine Aquatic Bed
C              49  MODIS_254 unclassified
C              50  MODIS_255 fill value (normally ocean water)

            end do
            end do

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
