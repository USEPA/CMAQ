	module data_module

        use type_def_module

        implicit none

! for wrf
        integer :: wrf_nlays

! general information variables
        integer :: ndim, nvar, n_global_att, unlimited

! dimenstion information variables
        character (len = nf90_max_name), allocatable :: dim_name(:)
        integer, allocatable                         :: dim_len(:,:)

! variable information variables
        character (len = nf90_max_name), allocatable :: var_name(:)
        integer, allocatable                         :: var_type(:),    &
                                                        var_ndims(:),   &
                                                        var_dimids(:,:)

! attribute information variables
        type(att_value_record), allocatable :: global_att(:)
        integer                             :: att_type, att_len
        type(var_att_record), allocatable   :: var_att(:)

        integer, allocatable :: supplement_data_int (:, :)
        real, allocatable    :: supplement_data_real (:, :)

	end module data_module
