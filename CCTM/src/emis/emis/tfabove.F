
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


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/arc/CCTM/src/emis/emis/tfabove.F,v 1.3 2011/10/21 16:10:48 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine tfabove ( tfa )

C-----------------------------------------------------------------------
C Description:
C   Calculate transport fraction considering removal by flow above canopy
 
C Subroutines and Functions Called: None

C Revison History:
C  Shan He at RTP 2003
C  Jun 2009 D. Tong
C  Jan 2011 J. Young: mods for inline wind-blown dust module
C  Jun 2011 J. Young: add reshape to vd initialization for pgf90 compiler
C  Oct 2018 D. Wong: With new re-structure of LUS_DEFN, most of the data
C                    declaration has been moved to lus_data_module
C  Feb 2019 David Wong: removed all MY_N clauses
C-----------------------------------------------------------------------

      use hgrd_defn                    ! horizontal domain specifications
      use asx_data_mod                 ! meteorology data
      use lus_data_module, only: uland

      implicit none
 
C Includes:

C Arguments:

      real, intent( out ) :: tfa  ( :,: )   ! above canopy transport factor

C Parameters:

C Local Variables:

      integer c, r, i   ! loop indicies
      integer indx      ! windspeed index

C Based on Slinn 1982, fugitive dust in coarse mode, Vd average from
C PM2~PM10; 3 wind velocities: 10, 5, 1 m/s; 4 canopy characteristics:
C water and lamda = 1.0, 3.5, 5.0

      real, parameter :: vd( 4,3 ) = reshape (  ! deposition velocity [m/s]
     &            (/ 0.0051,    ! natural water Vd(1,1), WindSpeed = 10 m/s
     &               0.0152,    ! lamda = 2.0   Vd(2,1), WindSpeed = 10 m/s
     &               0.0268,    ! lamda = 3.5   Vd(3,1), WindSpeed = 10 m/s 
     &               0.0382,    ! lamda = 5.0   Vd(4,1), WindSpeed = 10 m/s
     &               0.0018,    ! natural water Vd(1,2), WindSpeed =  5 m/s
     &               0.0056,    ! lamda = 2.0   Vd(2,2), WindSpeed =  5 m/s
     &               0.0099,    ! lamda = 3.5   Vd(3,2), WindSpeed =  5 m/s
     &               0.0141,    ! lamda = 5.0   Vd(4,2), WindSpeed =  5 m/s
     &               0.0018,    ! natural water Vd(1,3), WindSpeed =  1 m/s
     &               0.0020,    ! lamda = 2.0   Vd(2,3), WindSpeed =  1 m/s
     &               0.0021,    ! lamda = 3.5   Vd(3,3), WindSpeed =  1 m/s
     &               0.0030 /), ! lamda = 5.0   Vd(4,3), WindSpeed =  1 m/s
     &           (/ 4,3 /), order = (/ 1,2 /) )

      character( 16 ) :: pname = 'tfabove'
 
      real   :: ul( 4 )
      real   :: ku              ! k=0.08u*
      real   :: omeg            ! transport factor variable

C ----------------------------------------------------------------------
      do r = 1, nrows
      do c = 1, ncols
         tfa( c,r ) = 0.0
         if ( Met_Data%wspd10( c,r ) .le. 2.0 ) then        ! surface wind [0-2] range
            indx = 3
         else if ( Met_Data%wspd10( c,r ) .lt. 7.0 ) then   ! surface wind (2-7) range
            indx = 2
         else                                     ! surface wind [7-^) range
            indx = 1
         end if
         ul = uland( c,r,: )   ! array asignment
         ku = 0.08 * Met_Data%ustar( c,r )
         do i = 1, 4                              ! landuse loop
            omeg = 0.0
            if ( ul( i ) .gt. 0.0 ) then          ! non-zero landuse
               omeg = ku / ( vd( i,indx ) + ku )  ! ratio dqup/dqrd    
               tfa( c,r ) = tfa( c,r ) + 0.01 * omeg * ul( i )
            end if
         end do
      end do
      end do

      return
      end

C SWIND     0..1..2..3..4..5..6..7..8..9..10.....
C           |  .  .  .  .  .  .  .  .  .  |
C INDX      [ -3- ]     -2-      )         -1->
C WindSpeed |--1--|------5-------|----------10---> m/s

