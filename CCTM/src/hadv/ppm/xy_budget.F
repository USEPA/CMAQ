
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

!-----------------------------------------------------------------------
      Module XY_BUDGET

!  Collects Advection Process Rates
!
!-----------------------------------------------------------------------

      Implicit None

      SAVE

      ! These Arrays are sorted in the same manner as CGRID so they may
      ! be directly applied in modules like Process Analysis and ISAM
      REAL, ALLOCATABLE :: DELC_XADV  ( :,:,:,: ) ! Change in Species due to X Advection
      REAL, ALLOCATABLE :: DELC_YADV  ( :,:,:,: )   ! Change in Species due to Y Advection

      ! Total burden change at boundaries due to advection. The units are
      ! moles for gases, grams for particle mass, 1 for particle number, 
      ! and m2 for particle surface area. The conversion total burden
      ! must be done at this level because of variability in vertical
      ! grid cell height over the model output time step.
      REAL, ALLOCATABLE :: F_WEST_IN  ( :,: ) ! Mass entering the western boundary
      REAL, ALLOCATABLE :: F_WEST_OUT ( :,: ) ! Mass leaving the western boundary
      REAL, ALLOCATABLE :: F_EAST_IN  ( :,: ) ! Mass entering the eastern boundary
      REAL, ALLOCATABLE :: F_EAST_OUT ( :,: ) ! Mass leaving the eastern boundary
      REAL, ALLOCATABLE :: F_SOUTH_IN ( :,: ) ! Mass entering the southern boundary 
      REAL, ALLOCATABLE :: F_SOUTH_OUT( :,: ) ! Mass leaving the southern boundary  
      REAL, ALLOCATABLE :: F_NORTH_IN ( :,: ) ! Mass entering the northern boundary 
      REAL, ALLOCATABLE :: F_NORTH_OUT( :,: ) ! Mass leaving the northern boundary  

C-----------------------------------------------------------------------

      End Module XY_BUDGET
