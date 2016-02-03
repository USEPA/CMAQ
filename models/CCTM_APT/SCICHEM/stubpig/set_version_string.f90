!----------------------------------------------------------------------------

      subroutine set_version_string(iver,cver)

      implicit none

      integer iver
      character*(*) cver

      real      xversion
      integer nch

      xversion = 0.001*float(iver)
      call c_format(xversion,nch,cver)
      do while (cver(nch:nch) == '0' .and. nch > 1)
         nch = nch - 1
      end do
      cver(nch+1:nch+3) = 'sgi'
      cver(nch+4:) = ' '

      return
      end
