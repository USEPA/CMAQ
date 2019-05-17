
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
! This module contains key component from NH3_BIDI_MOD.F to facilitate 
! centralized I/O implementation by avoiding cyclic data dependence

! Revision History:
!  01 Feb 2019 D.Wong initial implementation, 
!------------------------------------------------------------------------!

          module depv_data_module

            use Bidi_Mod, Only: gamma1, gamma2

            implicit none

            Integer, Parameter :: e2c_cats = 42   ! number of crop catigories

            Real, Allocatable :: Beld_ag ( :,:,: )
            Real, Allocatable :: pHs1    ( :,:,: )
            Real, Allocatable :: pHs2    ( :,:,: )
            Real, Allocatable :: NH4ps1  ( :,:,: ) ! Initial Soil NH4
            Real, Allocatable :: NH4ps2  ( :,:,: ) ! Initial Soil NH4

            Real, Allocatable :: Nit1    ( :,:,: ) ! Epic Nitrification layer 1
            Real, Allocatable :: Nit2    ( :,:,: ) ! Epic Nitrification layer 2
            Real, Allocatable :: L1_ON   ( :,:,: ) ! Epic Layer 1 organic N
            Real, Allocatable :: L2_ON   ( :,:,: ) ! Epic Layer 2 organic N

            Real, Allocatable :: GMN     ( :,:,: ) ! Epic Layer 1 and 2 N mineralized in Fest-C 1.4

            Real, Allocatable :: BDc1    ( :,:,: ) ! crop soil bulk density kg/ha
            Real, Allocatable :: BDc2    ( :,:,: ) ! crop soil bulk density kg/ha   

            Real, Allocatable :: F1_NH4  ( :,:,: ) ! fertlizer application to layer 1 
            Real, Allocatable :: F2_NH4  ( :,:,: ) ! fertlizer application to layer 2

            Real, Allocatable :: L1_ON_Yest ( :,:,: ) ! Epic Layer 1 organic N
            Real, Allocatable :: L2_ON_Yest ( :,:,: ) ! Epic Layer 2 organic N
            Real, Allocatable :: F1_ON      ( :,:,: ) ! Epic Layer 1 organic N Fertilization 
            Real, Allocatable :: F2_ON      ( :,:,: ) ! Epic Layer 2 organic N Fertilization 

            Character( 20 )            :: Beld_Names( e2c_cats )

C Some compilers do not pad with blank characters in data initialization
C                          '12345678901234567890'
         Data Beld_Names / 'Hay                 ',  !  1
     &                     'Hay_ir              ',  !  2
     &                     'Alfalfa             ',  !  3
     &                     'Alfalfa_ir          ',  !  4
     &                     'Other_Grass         ',  !  5
     &                     'Other_Grass_ir      ',  !  6
     &                     'Barley              ',  !  7
     &                     'Barley_ir           ',  !  8
     &                     'BeansEdible         ',  !  9
     &                     'BeansEdible_ir      ',  ! 10
     &                     'CornGrain           ',  ! 11
     &                     'CornGrain_ir        ',  ! 12
     &                     'CornSilage          ',  ! 13
     &                     'CornSilage_ir       ',  ! 14
     &                     'Cotton              ',  ! 15
     &                     'Cotton_ir           ',  ! 16
     &                     'Oats                ',  ! 17
     &                     'Oats_ir             ',  ! 18
     &                     'Peanuts             ',  ! 19
     &                     'Peanuts_ir          ',  ! 20
     &                     'Potatoes            ',  ! 21
     &                     'Potatoes_ir         ',  ! 22
     &                     'Rice                ',  ! 23
     &                     'Rice_ir             ',  ! 24
     &                     'Rye                 ',  ! 25
     &                     'Rye_ir              ',  ! 26
     &                     'SorghumGrain        ',  ! 27
     &                     'SorghumGrain_ir     ',  ! 28
     &                     'SorghumSilage       ',  ! 29
     &                     'SorghumSilage_ir    ',  ! 30
     &                     'Soybeans            ',  ! 31
     &                     'Soybeans_ir         ',  ! 32
     &                     'Wheat_Spring        ',  ! 33
     &                     'Wheat_Spring_ir     ',  ! 34
     &                     'Wheat_Winter        ',  ! 35
     &                     'Wheat_Winter_ir     ',  ! 36
     &                     'Other_Crop          ',  ! 37
     &                     'Other_Crop_ir       ',  ! 38
     &                     'Canola              ',  ! 39
     &                     'Canola_ir           ',  ! 40
     &                     'Beans               ',  ! 41
     &                     'Beans_ir            ' / ! 42

          end module depv_data_module
