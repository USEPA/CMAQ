!===============================================================================
! Purpose:  Define the mapping between CMAQ species and aerosol information, water
!           soluable, water insoluable, elementary carbon, sea salt, and water,
!           that will affect the radiation calculation
!
! Revised:  11 Aug 2011  Original version.  David Wong
!           21 Oct 2015  Updated water insoluble species list
!           22 Nov 2016  Constructed water soluble and insoluble list dynamically
!                        based on a given chemical mechanism and AE scheme
!           01 Aug 2019  -- renamed ASEACATK to ASEACAT
!                        -- added a H2O species, AORGH2OJ
!===============================================================================

  module twoway_cgrid_aerosol_spc_map_module

    use aero_data

    ! water soluble
    integer :: num_ws_spc(3)

    integer, allocatable :: ws_spc_index(:,:)

    ! water insoluble
    integer :: num_wi_spc(3)

    integer, allocatable :: wi_spc_index(:,:)

    ! elmental carbon
    integer, parameter :: num_ec_spc = 2

    integer :: ec_spc_index(num_ec_spc)

    character (len = 16), parameter :: ec_spc(num_ec_spc) = &
      (/ 'AECI            ', 'AECJ            '             &
      /)

! ANAK = ANAK 

! NUMATKN = VAT0
! NUMACC  = VAC0
! NUMCOR  = VCO0

    ! sea salt
    integer, parameter :: num_ss_spc = 5

    integer :: ss_spc_index(num_ss_spc)

    character (len = 16), parameter :: ss_spc(num_ss_spc) = &
      (/ 'ANAJ            ', 'ACLJ            ',            &
         'ACLK            ', 'ASO4K           ',            &
         'ASEACAT         '                                 &
      /)

    ! water
    integer, parameter :: num_h2o_spc = 4

    integer :: h2o_spc_index(num_h2o_spc)

    character (len = 16), parameter :: h2o_spc(num_h2o_spc) = &
      (/ 'AH2OI           ', 'AH2OJ           ',              &
         'AH2OK           ', 'AORGH2OJ        '               &
      /)

    INTEGER, PARAMETER :: num_twoway_ae_cmaq_spc = 44

    INTEGER, PARAMETER :: num_twoway_ae_cmaq_spc_other = 12

    INTEGER, PARAMETER :: n_feedback_var  = 22 + num_twoway_ae_cmaq_spc + 3

! for feedback

    CHARACTER (LEN = 16), PARAMETER :: feedback_vlist(n_feedback_var) = &
      (/ 'WS_1            ', 'WS_2            ', 'WS_3            ',  &
         'IS_1            ', 'IS_2            ', 'IS_3            ',  &
         'EC_1            ', 'EC_2            ', 'EC_3            ',  &
         'SEASALT_1       ', 'SEASALT_2       ', 'SEASALT_3       ',  &
         'WATER_1         ', 'WATER_2         ', 'WATER_3         ',  &
         'DIAMETERS_1     ', 'DIAMETERS_2     ', 'DIAMETERS_3     ',  &
         'SD_1            ', 'SD_2            ', 'SD_3            ',  &
         'O3              ',                                          &
         'ASO4I           ', 'ASO4J           ', 'ASO4K           ', 'ANO3I           ', 'ANO3J           ', &
         'ANO3K           ', 'ANH4I           ', 'ANH4J           ', 'ANH4K           ', 'AALK1J          ', &
         'AALK2J          ',                                                                                 &
         'AXYL1J          ', 'AXYL2J          ', 'AXYL3J          ', 'ATOL1J          ', 'ATOL2J          ', &
         'ATOL3J          ', 'ABNZ1J          ', 'ABNZ2J          ', 'ABNZ3J          ', 'ATRP1J          ', &
         'ATRP2J          ', 'AISO1J          ', 'AISO2J          ', 'ASQTJ           ', 'AISO3J          ', &
         'AOLGAJ          ', 'AOLGBJ          ', 'AORGCJ          ', 'AORGPAI         ', 'AORGPAJ         ', &
         'AECI            ', 'AECJ            ', 'AOTHRI          ', 'AOTHRJ          ', 'ANAI            ', &
         'ANAJ            ', 'ANAK            ', 'ACLI            ', 'ACLJ            ', 'ACLK            ', &
         'ACORSK          ', 'ASOILJ          ', 'ASOIL           ',                                         &
         'PMASSAT         ', 'PMASSAC         ', 'PMASSCO         '                                          &
      /)

! this is for aerosol indirect effect to map cgrid species to wrf 

    character (len = 16), parameter :: twoway_ae_cmaq_spc_name (num_twoway_ae_cmaq_spc) =                    &
      (/ 'ASO4I           ', 'ASO4J           ', 'ASO4K           ', 'ANO3I           ', 'ANO3J           ', &
         'ANO3K           ', 'ANH4I           ', 'ANH4J           ', 'ANH4K           ', 'AALK1J          ', &
         'AALK2J          ',                                                                                 &
         'AXYL1J          ', 'AXYL2J          ', 'AXYL3J          ', 'ATOL1J          ', 'ATOL2J          ', &
         'ATOL3J          ', 'ABNZ1J          ', 'ABNZ2J          ', 'ABNZ3J          ', 'ATRP1J          ', &
         'ATRP2J          ', 'AISO1J          ', 'AISO2J          ', 'ASQTJ           ', 'AISO3J          ', &
         'AOLGAJ          ', 'AOLGBJ          ', 'AORGCJ          ', 'AORGPAI         ', 'AORGPAJ         ', &
         'AECI            ', 'AECJ            ', 'AOTHRI          ', 'AOTHRJ          ', 'ANAI            ', &
         'ANAJ            ', 'ANAK            ', 'ACLI            ', 'ACLJ            ', 'ACLK            ', &
         'ACORSK          ', 'ASOILJ          ', 'ASOILK          '                                          &
      /)

    integer :: twoway_ae_cmaq_spc_name_index (num_twoway_ae_cmaq_spc)

! this is for aerosol indirect effect to map cgrid species to wrf 

    character (len = 16), parameter :: twoway_ae_cmaq_spc_name_other (num_twoway_ae_cmaq_spc_other) =        &
      (/ 'APOCI           ', 'APNCOMI         ', 'APOCJ           ', 'APNCOMJ         ', 'ASEACAT         ', &
         'ASOILK          ', 'ACORSK          ', 'AALJ            ', 'ASIJ            ', 'ACAJ            ', &
         'AFEJ            ', 'ATIJ            '  &
      /)

    integer :: twoway_ae_cmaq_spc_name_other_index (num_twoway_ae_cmaq_spc_other)

    contains

    integer function find_index (vname) result (index)

    implicit none

    character (len = 16), intent(in) :: vname

    logical :: found
    integer :: s, m

    found = .false.
    s = 0
    do while ((s < n_aerospc) .and. (.not. found))
       s = s + 1
       m = 0
       do while ((m < n_mode) .and. (.not. found))
          m = m + 1
          if (aerospc(s)%name(m) == vname) then
             found = .true.
             index = aerospc_map(s, m)
          end if
       end do
    end do

    if (.not. found) then
       print *, ' Error: variable ', trim(vname), ' not found '
       stop
    end if

    end function find_index

  end module twoway_cgrid_aerosol_spc_map_module
