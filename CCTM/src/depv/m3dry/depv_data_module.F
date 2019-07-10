
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

!------------------------------------------------------------------------!
! This module contains key component from ABFLUX_MOD.F to facilitate 
! centralized I/O implementation by avoiding cyclic data dependence

! Revision History:
!  01 Feb 2019 D.Wong initial implementation, 
!------------------------------------------------------------------------!

          module depv_data_module

            implicit none

            integer, parameter :: e2c_cats = 42   ! number of crop catigories

! Some compilers do not pad with blank characters in data initialization
!              '12345678901234567890'
            character( 20 ), parameter :: Beld_Names( e2c_cats ) =
     &        (/ 'Hay                 ',  !  1
     &           'Hay_ir              ',  !  2
     &           'Alfalfa             ',  !  3
     &           'Alfalfa_ir          ',  !  4
     &           'Other_Grass         ',  !  5
     &           'Other_Grass_ir      ',  !  6
     &           'Barley              ',  !  7
     &           'Barley_ir           ',  !  8
     &           'BeansEdible         ',  !  9
     &           'BeansEdible_ir      ',  ! 10
     &           'CornGrain           ',  ! 11
     &           'CornGrain_ir        ',  ! 12
     &           'CornSilage          ',  ! 13
     &           'CornSilage_ir       ',  ! 14
     &           'Cotton              ',  ! 15
     &           'Cotton_ir           ',  ! 16
     &           'Oats                ',  ! 17
     &           'Oats_ir             ',  ! 18
     &           'Peanuts             ',  ! 19
     &           'Peanuts_ir          ',  ! 20
     &           'Potatoes            ',  ! 21
     &           'Potatoes_ir         ',  ! 22
     &           'Rice                ',  ! 23
     &           'Rice_ir             ',  ! 24
     &           'Rye                 ',  ! 25
     &           'Rye_ir              ',  ! 26
     &           'SorghumGrain        ',  ! 27
     &           'SorghumGrain_ir     ',  ! 28
     &           'SorghumSilage       ',  ! 29
     &           'SorghumSilage_ir    ',  ! 30
     &           'Soybeans            ',  ! 31
     &           'Soybeans_ir         ',  ! 32
     &           'Wheat_Spring        ',  ! 33
     &           'Wheat_Spring_ir     ',  ! 34
     &           'Wheat_Winter        ',  ! 35
     &           'Wheat_Winter_ir     ',  ! 36
     &           'Other_Crop          ',  ! 37
     &           'Other_Crop_ir       ',  ! 38
     &           'Canola              ',  ! 39
     &           'Canola_ir           ',  ! 40
     &           'Beans               ',  ! 41
     &           'Beans_ir            '   ! 42
     &           /)

            real, allocatable :: Beld_ag ( :,:,: )
!   for E2C_SOIL file
            real, allocatable :: pHs1    ( :,:,: )
            real, allocatable :: pHs2    ( :,:,: )
            real, allocatable :: por1    ( :,:,: )
            real, allocatable :: por2    ( :,:,: )
            real, allocatable :: wp1     ( :,:,: )
            real, allocatable :: wp2     ( :,:,: )
            real, allocatable :: cec1    ( :,:,: )
            real, allocatable :: cec2    ( :,:,: )
!   for E2C_CHEM file
            real, Allocatable :: NH4ps1  ( :,:,: ) ! Total Soil NH4 from EPIC
            real, Allocatable :: NH4ps2  ( :,:,: )
            real, Allocatable :: wep1    ( :,:,: )
            real, Allocatable :: wep2    ( :,:,: )
            real, Allocatable :: dep2    ( :,:,: )

          end module depv_data_module
