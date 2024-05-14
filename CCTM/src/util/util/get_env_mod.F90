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

      module get_env_module

! Function: get environment variables

! Revision History:
!        2010 D.Wong: initial implementation
!  2 Feb 2010 D.Wong: provided an optional outputing device option,
!                     absorbed get_envlist function

        implicit none

        integer, parameter :: max_str_len = 10000

        character (max_str_len) :: loc_str

        interface get_env
          module procedure get_env_int,      &
                           get_env_float,    &
                           get_env_double,   &
                           get_env_char,     &
                           get_env_logical
        end interface

        contains

! --------------------------------------------------------------------------------
        subroutine get_env_int (env_value, env_var, default_env_value, logdev)

          integer, intent(out)      :: env_value
          character (*), intent(in) :: env_var
          integer, intent(in)       :: default_env_value
          integer, intent(in), optional :: logdev

          integer :: loc_logdev
          logical :: default, regular

          call getenv (env_var, loc_str)

          if (present(logdev)) then
             loc_logdev = logdev
          else
             loc_logdev = 6
          end if

          regular = .false.
          default = .false.

          if (len(trim(loc_str)) == 0) then
             env_value = default_env_value
             default = .true.
          else
             read (loc_str, *) env_value
             regular = .true.
          end if

          if ( loc_logdev .gt. 0 ) then
             if (default) then
                write( loc_logdev, '(A21,2x,A,2x,i10, 1x, a9)' ), env_var,'|', env_value, '(default)'
             else if (regular) then
                write( loc_logdev, '(A21,2x,A,2x,i10)' ), env_var,'|', env_value
             end if
          end if

        end subroutine get_env_int

! --------------------------------------------------------------------------------
        subroutine get_env_float (env_value, env_var, default_env_value, logdev)

          real, intent(out)         :: env_value
          character (*), intent(in) :: env_var
          real, intent(in)          :: default_env_value
          integer, intent(in), optional :: logdev

          integer :: loc_logdev
          logical :: default, regular

          call getenv (env_var, loc_str)

          if (present(logdev)) then
             loc_logdev = logdev
          else
             loc_logdev = 6
          end if

          regular = .false.
          default = .false.

          if (len(trim(loc_str)) == 0) then
             env_value = default_env_value
             default = .true.
          else
             read (loc_str, *) env_value
             regular = .true.
          end if

          if ( loc_logdev .gt. 0 ) then
             if (default) then
                write( loc_logdev, '(A21,2x,A,2x,e10.3, 1x, a9)' ), env_var,'|', env_value, '(default)'
             else if (regular) then
                write( loc_logdev, '(A21,2x,A,2x,e10.3)' ), env_var,'|', env_value
             end if
          end if

        end subroutine get_env_float

! --------------------------------------------------------------------------------
        subroutine get_env_double (env_value, env_var, default_env_value, logdev)

          real (8), intent(out)     :: env_value
          character (*), intent(in) :: env_var
          real (8), intent(in)          :: default_env_value
          integer, intent(in), optional :: logdev

          integer :: loc_logdev
          logical :: default, regular

          call getenv (env_var, loc_str)

          if (present(logdev)) then
             loc_logdev = logdev
          else
             loc_logdev = 6
          end if

          regular = .false.
          default = .false.

          if (len(trim(loc_str)) == 0) then
             env_value = default_env_value
             default = .true.
          else
             read (loc_str, *) env_value
             regular = .true.
          end if

          if ( loc_logdev .gt. 0 ) then
             if (default) then
                write( loc_logdev, '(A21,2x,A,2x,e10.3, 1x, a9)' ), env_var,'|', env_value, '(default)' 
             else if (regular) then
                write( loc_logdev, '(A21,2x,A,2x,e10.3)' ), env_var,'|', env_value
             end if
          end if

        end subroutine get_env_double

! --------------------------------------------------------------------------------
        subroutine get_env_char (env_value, env_var, default_env_value, logdev)

          character (*), intent(out) :: env_value
          character (*), intent(in)  :: env_var
          character (*), intent(in)  :: default_env_value
          integer, intent(in), optional :: logdev

          integer :: loc_logdev, length
          logical :: default, regular
          character (50) :: myfmt

          call getenv (env_var, loc_str)

          if (present(logdev)) then
             loc_logdev = logdev
          else
             loc_logdev = 6
          end if

          regular = .false.
          default = .false.

          if (len(trim(loc_str)) == 0) then
             env_value = default_env_value
             default = .true.
          else
             env_value = loc_str
             regular = .true.
          end if

          if ( loc_logdev .gt. 0 ) then
             length = len_trim(env_value)
             if (default) then
                if (length .eq. 0) then
                   write( loc_logdev, '(A21, 2x, A, 13x, a9)') env_var, '|', '(default)'
                else
                   write (myfmt, '(a18, i3.3, a9)') '(A21, 2x, A, 2x, A', length, ', 1x, a9)'
                   write( loc_logdev, myfmt) env_var, '|', env_value, '(default)'
                end if
             else if (regular) then
                write (myfmt, '(a18, i3.3, a1)') '(A21, 2x, A, 2x, A', length, ')'
                write( loc_logdev, myfmt) env_var,'|', env_value
             end if
          end if

        end subroutine get_env_char

! --------------------------------------------------------------------------------
        subroutine get_env_logical (env_value, env_var, default_env_value, logdev)

          logical, intent(out)      :: env_value
          character (*), intent(in) :: env_var
          logical, intent(in)       :: default_env_value
          integer, intent(in), optional :: logdev

          integer :: length
          integer :: loc_logdev
          logical :: default, regular

          call getenv (env_var, loc_str)

          if (present(logdev)) then
             loc_logdev = logdev
          else
             loc_logdev = 6
          end if

          length = len(trim(loc_str))
          regular = .false.
          default = .false.

          if (length <= 0) then
             env_value = default_env_value
             default = .true.
          else if ((length == 1) .and. ((loc_str(1:1) .eq. 'Y') .or.       &
                                        (loc_str(1:1) .eq. 'y') .or.       &
                                        (loc_str(1:1) .eq. 'T') .or.       &
                                        (loc_str(1:1) .eq. 't'))) then
             env_value = .true.
             regular = .true.
          else if ((length == 1) .and. ((loc_str(1:1) .eq. 'N') .or.       &
                                        (loc_str(1:1) .eq. 'n') .or.       &
                                        (loc_str(1:1) .eq. 'F') .or.       &
                                        (loc_str(1:1) .eq. 'f'))) then
             env_value = .false.
             regular = .true.
          else if ((trim(loc_str) == '.TRUE.') .or.                        &
                   (trim(loc_str) == '.true.') .or.                        &
                   (trim(loc_str) == '.True.') .or.                        &
                   (trim(loc_str) == 'TRUE') .or.                          &
                   (trim(loc_str) == 'true') .or.                          &
                   (trim(loc_str) == 'True') .or.                          &
                   (trim(loc_str) == 'YES') .or.                           &
                   (trim(loc_str) == 'yes') .or.                           &
                   (trim(loc_str) == 'Yes')) then
             env_value = .true.
             regular = .true.
          else if ((trim(loc_str) == '.FALSE.') .or.                       &
                   (trim(loc_str) == '.false.') .or.                       &
                   (trim(loc_str) == '.False.') .or.                       &
                   (trim(loc_str) == 'FALSE') .or.                         &
                   (trim(loc_str) == 'false') .or.                         &
                   (trim(loc_str) == 'False') .or.                         &
                   (trim(loc_str) == 'NO') .or.                            &
                   (trim(loc_str) == 'no') .or.                            &
                   (trim(loc_str) == 'No')) then
             env_value = .false.
             regular = .true.
          else
             write (loc_logdev, *) ' Note: Variable ', trim(env_var), ' improperly formatted'
             env_value = default_env_value
             default = .true.
          end if

          if ( loc_logdev .gt. 0 ) then
             if (default) then
                write( loc_logdev, '(A21,2x,A,10x,L, 1x, a9)' ), env_var,'|', env_value, '(default)'
             else if (regular) then
                write( loc_logdev, '(A21,2x,A,10x,L)' ), env_var,'|', env_value
             end if
          end if

        end subroutine get_env_logical

! --------------------------------------------------------------------------------
        subroutine get_envlist ( env_var, nvars, val_list, in_logdev )

! get a list env var (quoted string of items delimited by white space,
! commas or semi-colons) and parse out the items into variables. Two data
! types: character strings and integers (still represented as strings in
! the env var vaules).
! Examples:
! 1)   setenv AVG_CONC_VARS "O3 NO NO2"
! 2)   setenv AVG_CONC_LAYS "2 5"          < start at two, end at 5
! 3)   setenv NPCOLSXNPROWS "4 3"
! 4)   setenv BCOL_ECOL "3 8"
! 5)   setenv BROW_EROW "2 10"
! 6)   setenv BLAY_ELAY "1 5"

! In example (1), not only parse out the named items "O3", "NO" and "NO2",
! but also obtain the count on the number of items (=3).

! Revision: 2013/02/11 David Wong: increased the max env var length from 256 to 1000
! 13 Dec 2013 J.Young: 1000 breaks BUFLEN in IOAPI's envgets.c. Change to 512.
! 17 Jun 2016 J.Young: IOAPI's envgets.c BUFLEN has been increased to 10000.
! 20 Jun 2016 J.Young: Forget IOAPI's envgets.c: use Fortran GETENV

#ifndef mpas
          use utilio_defn
#endif

          character( * ),  intent ( in )  :: env_var
          integer,         intent ( out ) :: nvars
          character( 16 ), intent ( out ) :: val_list( : )
          integer, intent(in), optional :: in_logdev

          integer             :: max_len
          character( 16 )     :: pname = 'GET_ENVLIST'
          character( 16*size( val_list ) ) :: e_val
          character(  1 )     :: chr
          character( 96 )     :: xmsg

          integer :: jp( 16*size( val_list ) ), kp( 16*size( val_list ) ), status
          integer ip, v

          integer :: loc_logdev
 
          if (present(in_logdev)) then
             loc_logdev = in_logdev
          else
             loc_logdev = 6
          end if

           max_len = 16 * size( val_list )

          call get_env( e_val, env_var, ' ', loc_logdev )

          if ( e_val .eq. " " ) then
             xmsg = 'Environment variable ' // env_var // ' not set'
#ifndef mpas
             call m3warn( pname, 0, 0, xmsg )
#endif
             nvars = 0
             val_list = ''
             return
          end if

          nvars = 1

          ip = 0

101   continue
          ip = ip + 1
          if ( ip .gt. max_len ) go to 301
          chr = e_val( ip:ip )
          if ( chr .eq. ' ' .or. ichar ( chr ) .eq. 09 ) go to 101
          jp( nvars ) = ip   ! 1st char

201   continue
          ip = ip + 1
          if ( ip .gt. max_len ) then
             xmsg = 'Environment variable value too long'
#ifndef mpas
             call m3exit( pname, 0, 0, xmsg, 2 )
#endif
          end if
          chr = e_val( ip:ip )
          if ( chr .ne. ' ' .and.    &
               chr .ne. ',' .and.    &
               chr .ne. ';' .or.     &
               ichar ( chr ) .eq. 09 ) then  ! 09 = horizontal tab
             go to 201
          else
             kp( nvars ) = ip - 1 ! last char in this item
             nvars = nvars + 1
          end if

          go to 101

301       continue
          nvars = nvars - 1

          do v = 1, nvars
             val_list( v ) = e_val( jp( v ):kp( v ) )
          end do

        end subroutine get_envlist
        function get_free_iounit() result ( iounit )

! function finds and return a free IO unit
! adapted from D.Wong's mio library

           implicit none

           integer :: iounit
           logical :: found, opened

           iounit = 99
           found = .false.
           do while ((.not. found) .and. (iounit .le. 100000))
              inquire (unit=iounit, opened=opened)
              if (.not. opened) then
                 found = .true.
              else
                 iounit = iounit + 1
              end if
           end do


        end function get_free_iounit


      end module get_env_module
