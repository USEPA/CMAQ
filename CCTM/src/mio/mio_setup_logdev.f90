! Purpose: Establish ancillary run time log files for each parallel processor
!          Effectively no operation, if serial

! Revision History:
!   brorrowed from CMAQ setup_logdev.F

      function mio_setup_logdev() result ( logdev )

        use mio_global_data_module, only: mio_mype, mio_parallelism, mio_serial
        use mio_get_env_module

        implicit none

        character( 48 ) :: eqname
        character(  8 ) :: prestr = 'MIO_LOG_'
        character(  3 ) :: cmype
        character( 96 ) :: iologeq
        character(  8 ) :: appl = 'MIO_APPL'

        integer :: LOGDEV, IOST, i
        logical :: found, opened

        if (mio_parallelism .eq. mio_serial) then
           logdev = 6
        else
           call mio_get_env ( eqname, appl, ' ' )
           write ( cmype, '(I3.3)' ) mio_mype
           if ( ' ' .NE. eqname(1:8 ) ) then
              iologeq = prestr // cmype // '.' // trim( eqname )
           else
              iologeq = prestr // cmype
           end if

           i = 99
           found = .false.
           do while ((.not. found) .and. (i .le. 150))
              inquire (unit=i, opened=opened)
              if (.not. opened) then
                 found = .true.
              else
                 i = i + 1
              end if 
           end do 

           logdev = i

           open ( unit    =  logdev,        &
                  iostat  =  iost,          &
                  file    =  iologeq,       &
                  status  =  'new',         &
                  access  =  'sequential')

        end if

      end function mio_setup_logdev
