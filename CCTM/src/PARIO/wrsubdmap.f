
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

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine wrsubdmap ( nprocs, ncols_pe, nrows_pe, colsx_pe, rowsx_pe )

      implicit none

!> layout parallel decomp subdomains given processor decomp and grid dimensions

      integer, intent( in ) :: nprocs   ! npcol*nprow
      integer, intent( in ) :: ncols_pe( nprocs ) ! No. of 1st dimension elements per PE
      integer, intent( in ) :: nrows_pe( nprocs ) ! No. of 2nd dimension elements per PE
      integer, intent( in ) :: colsx_pe( 2,nprocs ) ! 1st dimension index range per PE
      integer, intent( in ) :: rowsx_pe( 2,nprocs ) ! 2nd dimension index range per PE

      character( 49 ) :: colrow = '  PE    #Cols    Col_Range     #Rows    Row_Range'
      character( 49 ) :: title

      integer i               ! loop counter.

      title = colrow

      write( *,* )
      write( *,* ) '         -=-  MPP Processor-to-Subdomain Map  -=-'
      write( *,'(A,I3)' ) '                 Number of Processors = ',nprocs
      write( *,* ) '   ____________________________________________________'
      write( *,* ) '   |                                                  |'
      write( *,* ) '   |' // title // ' |'
      write( *,* ) '   |__________________________________________________|'
      write( *,* ) '   |                                                  |'
      do i = 1, nprocs
         write( *,1003 ) i-1, ncols_pe(i), colsx_pe(1,i), colsx_pe(2,i),
     &                        nrows_pe(i), rowsx_pe(1,i), rowsx_pe(2,i)
      end do
      write( *,* ) '   |__________________________________________________|'
      write( *,* )

1003  format('    |', i3, 5x, i4, 3x, i4, ':', i4,
     &                    7x, i4, 3x, i4, ':', i4, '   |')

      end subroutine wrsubdmap
