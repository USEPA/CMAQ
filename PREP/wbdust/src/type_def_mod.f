	module type_def_module

        use netcdf

        implicit none

        integer, parameter :: max_num_att = 20

        type att_value_record
          character (len = nf90_max_name) :: att_name
          integer                         :: att_type
          integer                         :: att_len
          integer                         :: att_value_int
          integer                         :: att_value_int_link
          real                            :: att_value_float
          integer                         :: att_value_float_link
          double precision                :: att_value_double
          integer                         :: att_value_double_link
          character (len = 5000)          :: att_value_char
          integer                         :: att_value_char_link
        end type att_value_record

        type var_att_record
          integer :: num_var_att
          type (att_value_record) :: var_att_record_array(max_num_att)
        end type var_att_record

        type file_record
          integer               :: fileid
          character (len = 200) :: filename
          character (len = 1)   :: io_type
        end type file_record

        integer, parameter :: max_len = 256
        integer, parameter :: ioapi_3_str_len = 16

	end module type_def_module
