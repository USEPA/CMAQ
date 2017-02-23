
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/ICON/src/mech_conv/wrdate.f,v 1.1 2011/09/12 16:31:34 yoj Exp $

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

