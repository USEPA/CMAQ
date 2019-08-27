
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
! This module contains key component from cropcal.F to facilitate 
! centralized I/O implementation by avoiding cyclic data dependence

! Revision History:
!  01 Feb 2019 D.Wong initial implementation, 
!------------------------------------------------------------------------!

          module crop_data_module

            implicit none

            integer, parameter :: nlcrp = 17
            character( 16 ), parameter :: crop_vnmld( nlcrp ) =  ! beld01
     &         ( / 'Alfalfa  ', 'Barley   ', 'Corn     ',
     &             'Cotton   ', 'Grass    ', 'Hay      ',
     &             'Misc_crop', 'Oats     ', 'Pasture  ',
     &             'Peanuts  ', 'Potatoes ', 'Rice     ',
     &             'Rye      ', 'Sorghum  ', 'Soybeans ',
     &             'Tobacco  ', 'Wheat    ' / )

C Conventional tillage (0-15% residue) fraction for 17 crops as VNMLD list
            real, parameter :: ctilf( nlcrp ) =
     &         ( / 0.0,  0.37, 0.40, 0.70,  ! Alfalfa, Barley, Corn, Cotton,
     &             0.0,  0.0,  0.70, 0.47,  ! Grass, Hay, Misc_crop, Oats,
     &             0.36, 0.80, 0.74, 0.79,  ! Pasture, Peanuts, Potatoes, Rice,
     &             0.52, 0.46, 0.24, 0.96,  ! Rye, Sorghum, Soybeans, Tobacco, 
     &             0.40 / )                 ! Wheat

C No tillage fraction for 17 crops as VNMLD list
            real, parameter :: ntilf( nlcrp ) =
     &         ( / 0.80, 0.10, 0.20, 0.14,  ! Alfalfa, Barley, Corn, Cotton,
     &             0.80, 0.80, 0.05, 0.08,  ! Grass, Hay, Misc_crop, Oats,
     &             0.47, 0.09, 0.0,  0.05,  ! Pasture, Peanuts, Potatoes, Rice,
     &             0.11, 0.14, 0.33, 0.01,  ! Rye, Sorghum, Soybeans, Tobacco, 
     &             0.13 / )                 ! Wheat

            real, allocatable :: crop_ladut ( :,:,: )   ! cropland fraction from BELD1
            real, allocatable :: crplnd( :,:,: )        ! each erodible crop land fraction

            integer, parameter :: ncrop = 18
            character( 16 ), parameter :: vcrop( ncrop ) =  ! crop calendar )CROPCAL)
     &         ( / 'Alfalfa     ', 'BarleySpring', 'BarleyFall  ',
     &             'Corn        ', 'Cotton      ', 'Hay         ',
     &             'OatsSpring  ', 'OatsFall    ', 'Peanuts     ',
     &             'Potatoes    ', 'Rice        ', 'Rye         ',
     &             'Sorghum     ', 'Soybeans    ', 'Sugerbeets  ',
     &             'Tobacco     ', 'WheatSpring ', 'WheatWinter ' / )

            character( 16 ), parameter :: crname( 3 ) = ! crop calendar NetCDF file name
     &         ( / 'CROPMAP01',        ! begin planting
     &             'CROPMAP04',        ! end planting
     &             'CROPMAP08' / )     ! end harvesting

            real, allocatable :: cropdt( :,:,:,: ) ! cropland calendar for species and 3 activities

          end module crop_data_module
