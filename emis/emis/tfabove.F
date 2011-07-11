
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/emis/emis/tfabove.F,v 1.2 2011/07/11 13:48:29 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine tfabove ( ustar, swind, uland, tfa )

C-----------------------------------------------------------------------
C Description:
C   Calculate transport fraction considering removal by flow above canopy
 
C Subroutines and Functions Called: None

C Revison History:
C  Shan He at RTP 2003
C  Jun 2009 D. Tong
C  Jan 2011 J. Young: mods for inline wind-blown dust module
C  Jun 2011 J. Young: add reshape to vd initialization for pgf90 compiler
C-----------------------------------------------------------------------

      use hgrd_defn             ! horizontal domain specifications

      implicit none
 
C Includes:

C Arguments:

      real, intent( in )  :: ustar( :,: )   ! friction velocity [m/s]
      real, intent( in )  :: swind( :,: )   ! surface wind [m/s] SWIND
      real, intent( in )  :: uland( :,:,: ) ! fraction of 4 landtypes
                                            ! for dust removal 0~100
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
      do r = 1, my_nrows
      do c = 1, my_ncols
         tfa( c,r ) = 0.0
         if ( swind( c,r ) .le. 2.0 ) then        ! surface wind [0-2] range
            indx = 3
         else if ( swind( c,r ) .lt. 7.0 ) then   ! surface wind (2-7) range
            indx = 2
         else                                     ! surface wind [7-^) range
            indx = 1
         end if
         ul = uland( c,r,: )   ! array asignment
         ku = 0.08 * ustar( c,r )
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

