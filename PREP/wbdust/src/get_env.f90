        module get_env_module

        implicit none

        integer, parameter :: max_str_len = 10000

        character (len = max_str_len) :: loc_str

        interface get_env
          module procedure get_env_int,      &
                           get_env_int8,     &
                           get_env_float,    &
                           get_env_double,   &
                           get_env_char,     &
                           get_env_logical
        end interface

        contains

! --------------------------------------------------------------------------------
        subroutine get_env_int (env_value, env_var, default_env_value)

        integer, intent(out)            :: env_value
        character (len = *), intent(in) :: env_var
        integer, intent(in)             :: default_env_value

        call getenv (env_var, loc_str)

        if (len(trim(loc_str)) == 0) then
           env_value = default_env_value
           print *, ' Note: Default value ', default_env_value, &
                    ' is used for environment variable ', trim(env_var)
        else
           read (loc_str, *) env_value
        end if

        end subroutine get_env_int

! --------------------------------------------------------------------------------
        subroutine get_env_int8 (env_value, env_var, default_env_value)

        integer*8, intent(out)            :: env_value
        character (len = *), intent(in)   :: env_var
        integer, intent(in)               :: default_env_value

        call getenv (env_var, loc_str)

        if (len(trim(loc_str)) == 0) then
           env_value = default_env_value
           print *, ' Note: Default value ', default_env_value, &
                    ' is used for environment variable ', trim(env_var)
        else
           read (loc_str, *) env_value
        end if

        end subroutine get_env_int8

! --------------------------------------------------------------------------------
        subroutine get_env_float (env_value, env_var, default_env_value)

        real, intent(out)               :: env_value
        character (len = *), intent(in) :: env_var
        real, intent(in)                :: default_env_value

        call getenv (env_var, loc_str)

        if (len(trim(loc_str)) == 0) then
           env_value = default_env_value
           print *, ' Note: Default value ', default_env_value, &
                    ' is used for environment variable ', trim(env_var)
        else
           read (loc_str, *) env_value
        end if

        end subroutine get_env_float

! --------------------------------------------------------------------------------
        subroutine get_env_double (env_value, env_var, default_env_value)

        real (8), intent(out)           :: env_value
        character (len = *), intent(in) :: env_var
        real, intent(in)                :: default_env_value

        call getenv (env_var, loc_str)

        if (len(trim(loc_str)) == 0) then
           env_value = default_env_value
           print *, ' Note: Default value ', default_env_value, &
                    ' is used for environment variable ', trim(env_var)
        else
           read (loc_str, *) env_value
        end if

        end subroutine get_env_double

! --------------------------------------------------------------------------------
        subroutine get_env_char (env_value, env_var, default_env_value)

        character (len = *), intent(inout) :: env_value
        character (len = *), intent(in)  :: env_var
        character (len = *), intent(in)  :: default_env_value

        call getenv (env_var, loc_str)

        if (len(trim(loc_str)) == 0) then
           env_value = default_env_value
           print *, ' Note: Default value ', trim(default_env_value), &
                    ' is used for environment variable ', trim(env_var)
        else
           env_value = loc_str
        end if

        end subroutine get_env_char

! --------------------------------------------------------------------------------
        subroutine get_env_logical (env_value, env_var, default_env_value)

        logical, intent(out)             :: env_value
        character (len = *), intent(in)  :: env_var
        logical, intent(in)              :: default_env_value

        integer :: length

        call getenv (env_var, loc_str)

        length = len(trim(loc_str))

        if (length == 0) then
           env_value = default_env_value
           print *, ' Note: Default value ', default_env_value, &
                    ' is used for environment variable ', trim(env_var)
        else
           if ((length == 1) .and. ((loc_str(1:1) .eq. 'Y') .or.            &
                                    (loc_str(1:1) .eq. 'y') .or.            &
                                    (loc_str(1:1) .eq. 'T') .or.            &
                                    (loc_str(1:1) .eq. 't'))) then
              env_value = .true.
           else if ((length == 1) .and. ((loc_str(1:1) .eq. 'N') .or.       &
                                         (loc_str(1:1) .eq. 'n') .or.       &
                                         (loc_str(1:1) .eq. 'F') .or.       &
                                         (loc_str(1:1) .eq. 'f'))) then
              env_value = .false.
           else if ((trim(loc_str) == '.TRUE.') .or.                        &
                    (trim(loc_str) == '.true.') .or.                        &
                    (trim(loc_str) == '.True.')) then
              env_value = .true.
           else if ((trim(loc_str) == '.FALSE.') .or.                       &
                    (trim(loc_str) == '.false.') .or.                       &
                    (trim(loc_str) == '.False.')) then
              env_value = .false.
           end if
        end if

        end subroutine get_env_logical

        end module get_env_module
