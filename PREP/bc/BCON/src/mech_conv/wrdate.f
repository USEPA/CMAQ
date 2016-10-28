      subroutine wrdate ( date )
      implicit none

      character( 32 ) :: dcmd = 'date -u +%F > fort.99'
      character( 32 ) :: rcmd = '/bin/rm fort.99'
      character( * ) date

      call system( dcmd )
      read( 99,* ) date
      write( *,'(3x, a/)' ) date
      call system( rcmd )

      return
      end subroutine wrdate

