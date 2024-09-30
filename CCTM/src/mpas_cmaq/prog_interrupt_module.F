        module prog_interrupt_module

          use universal_data_module, only : logdev

          implicit none

          interface prog_interrupt
            module procedure prog_interrupt_exit,
     &                       prog_interrupt_warning,
     &                       prog_interrupt_msg,
     &                       prog_interrupt_msgs,
     &                       prog_interrupt_err
          end interface

          contains

! ------------------------------------------------------------------------------
          subroutine prog_interrupt_exit ( caller, date, time, msg, flag )

            character(*), intent(in) :: caller, msg
            integer,      intent(in) :: date, time
            integer,      intent(in) :: flag

            character(500) :: loc_msg(4)
            character(14)  :: time_str
            integer        :: i

            write (time_str, '(2i7)') date, time

            write (logdev, *) ' '
            write (logdev, *) 'ABORT in subroutine ', trim(caller), ' at time ', trim(time_str)
            write (logdev, *) trim(msg)
            write (logdev, *) ' '

!           do i = 1, 4
!              write (logdev, '(5x, a)') trim(loc_msg(i))
!           end do

!           print *, ' ==d== ABORT p ', logdev, trim(caller), ' ', trim(msg)
!           write (logdev, *) ' ==d== ABORT w', logdev, trim(caller), ' ', trim(msg)

            stop

          end subroutine prog_interrupt_exit

! ------------------------------------------------------------------------------
          subroutine prog_interrupt_warning ( caller, date, time, msg )

            character(*), intent(in) :: caller, msg
            integer,      intent(in) :: date, time

            character(500) :: loc_msg(4)
            character(14)  :: time_str
            integer        :: i

            write (time_str, '(2i7)') date, time

            loc_msg(1) = ' '
            loc_msg(2) = 'WARNING in subroutine ' // trim(caller) // ' at time ' // trim(time_str)
            loc_msg(3) = msg
            loc_msg(4) = ' '

            do i = 1, 4
               write (logdev, '(5x, a)') trim(loc_msg(i))
            end do

          end subroutine prog_interrupt_warning

! ------------------------------------------------------------------------------
          subroutine prog_interrupt_msg ( msg )

            character(*), intent(in) :: msg

            character(500) :: loc_msg(3)
            integer        :: i

            loc_msg(1) = ' '
            loc_msg(2) = msg
            loc_msg(3) = ' '

            do i = 1, 3
               write (logdev, '(5x, a)') trim(loc_msg(i))
            end do

          end subroutine prog_interrupt_msg

! ------------------------------------------------------------------------------
          subroutine prog_interrupt_msgs ( msg )

            character(*), intent(in) :: msg(:)

            character(2), parameter :: loc_msg = ' '
            integer        :: i

            write (logdev, '(5x, a)') trim(loc_msg)
            do i = 1, size(msg)
               write (logdev, '(5x, a)') trim(msg(i))
            end do
            write (logdev, '(5x, a)') trim(loc_msg)

          end subroutine prog_interrupt_msgs

! ------------------------------------------------------------------------------
          subroutine prog_interrupt_nmsgs ( n, msg )

            integer, intent(in) :: n
            character(*), intent(in) :: msg(:)

            character(2), parameter :: loc_msg = ' '
            integer        :: i

            write (logdev, '(5x, a)') trim(loc_msg)
            do i = 1, n
               write (logdev, '(5x, a)') trim(msg(i))
            end do
            write (logdev, '(5x, a)') trim(loc_msg)

          end subroutine prog_interrupt_nmsgs

! ------------------------------------------------------------------------------
          subroutine prog_interrupt_err ( caller, date, time, msg, fatal )

            character(*), intent(in) :: caller, msg
            integer,      intent(in) :: date, time
            logical,      intent(in) :: fatal

            if (fatal) then
               call prog_interrupt_exit ( caller, date, time, msg, 2 )
            else
               call prog_interrupt_warning ( caller, date, time, msg )
            end if

          end subroutine prog_interrupt_err

        end module prog_interrupt_module
