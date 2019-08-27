
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/cropcal.F,v 1.4 2012/01/19 15:23:37 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine cropcal ( jdate, jtime, agland )

C-----------------------------------------------------------------------
C Description:
C   Read cropland fraction from BELD and 3 gridded crop calendar files
C   and calculate erodible agriculture land fraction
 
C Revison History:
C      2003 Shan He at RTP: initial
C      2003 Daniel Tong:
C  Jan 2011 Jeff Young: mods for inline wind-blown dust module
C  Apr 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C  Jul 2011 J.Young: pad vnmld and vcrop for uniform string lengths
C  Jan 2012 J.Young: pad vnmld and vcrop for 16 char string lengths
C  Jun 2012 J.Young: remove full character blank padding put in for GNU Fortran (GCC) 4.1.2
C  Feb 2019 D.Wong: Implemented centralized I/O approach, and created
C                   a new module crop_data_module (model_data_module.F) 
C                   to hold some of information originally stored here 
C                   to avoid cyclic dependence
 
C Subroutines and Functions Called:
C   open3, xtract3, m3exit
C-----------------------------------------------------------------------

      use hgrd_defn             ! horizontal domain specifications
      use utilio_defn
      use crop_data_module

      implicit none

C Includes:
!     include 'DUST_CONST.EXT'

C Arguments:
      integer, intent(  in ) :: jdate   ! current date, coded YYYYDDD
      integer, intent(  in ) :: jtime   ! current model time, coded HHMMSS
      real,    intent( out ) :: agland( :,: ) ! total erodible crop land fraction

C Local Variables:
      character(  16 ) :: pname = 'CROPCAL         '
      character(  16 ) :: dust_lu_1 = 'DUST_LU_1'
      character( 120 ) :: xmsg = ' '

      real    jday                 ! Julian day of the year converted to REAL
      real    ttmp
      real    twos                 ! two season factor (1.0 or 0.5)
      real    pctf, potf           ! ?? ?? Tillage Factor ????????
      integer i, m, c, r
      
      real :: crpdt1, crpdt2, crpdt3        ! cropland calendar subexpression

      integer allocstat

C-----------------------------------------------------------------------

C Get day number in year
      jday = float( mod( jdate,1000 ) )

C Calculate erodible ag land fraction
      do r = 1, nrows
      do c = 1, ncols
         agland( c,r ) = 0.0
         ttmp = 0.0
         pctf = 0.0
         potf = 0.0
         do i = 1, nlcrp
            crplnd( c,r,i ) = 0.0
            if ( crop_ladut( c,r,i ) .gt. 0.0 ) then  ! non zero crop cover begin if1
               if ( i .eq.  1 ) m = 1      ! Alfalfa
               if ( i .eq.  2 ) m = 2      ! Barley spring fall
               if ( i .eq.  3 ) m = 4      ! Corn
               if ( i .eq.  4 ) m = 5      ! Cotton
               if ( i .eq.  5 ) m = 6      ! Grass as hay
               if ( i .eq.  6 ) m = 6      ! Hay
               if ( i .eq.  7 ) m = 15     ! Misc_crop as sugerbeets
               if ( i .eq.  8 ) m = 7      ! Oats spring fall
               if ( i .eq.  9 ) m = 6      ! Pasture
               if ( i .eq. 10 ) m = 9      ! Peanuts
               if ( i .eq. 11 ) m = 10     ! Potatoes
               if ( i .eq. 12 ) m = 11     ! Rice
               if ( i .eq. 13 ) m = 12     ! Rye
               if ( i .eq. 14 ) m = 13     ! Sorghum
               if ( i .eq. 15 ) m = 14     ! Soybeans
               if ( i .eq. 16 ) m = 16     ! Tobacco
               if ( i .eq. 17 ) m = 17     ! Wheat spring winter

               crpdt1 = cropdt( c,r,m,1 )
               if ( crpdt1 .eq. 0.0 ) go to 7744

3344           continue

               crpdt2 = cropdt( c,r,m,2 )
               crpdt3 = cropdt( c,r,m,3 )

               ! extend planting end day if needed
               if ( ( crpdt1 .lt. crpdt2 ) .and.
     &                crpdt2 .lt. ( crpdt1 + 14.0 ) ) then
                  crpdt2 = crpdt2 + 14.0
                  if ( crpdt2 .gt. 365.0 ) crpdt2 = 365.0 - crpdt2
               end if

               if ( ( crpdt1 + 14.0 ) .gt. 365.0 ) then
                  xmsg = '*** Error in cropland erodibility'
                  call m3exit ( pname, jdate, jtime, xmsg, xstat1 )
               end if

               ! before planting after harvest BEGIN IF2
               if ( ( ( crpdt1 .lt. crpdt3 ) .and.
     &                ( jday .le. crpdt1 .or.
     &                  jday .ge. crpdt3 ) ) .or.
     &                ( ( crpdt1 .gt. crpdt3 ) .and.
     &                ( jday .ge. crpdt1 .and.
     &                  jday .le. crpdt3 ) ) ) then
                  pctf = 1.0
                  potf = 0.0
                  ! other than cotton, peanut, potato, tobacco IF2
                  if ( ( i .ne. 4 .and. i .ne. 10 .and. i .ne. 11 .and. i .ne. 16 ) .and.
     &                 ( ( crpdt1 .lt. crpdt3 .and.
     &                   ( jday .le. ( crpdt1 - 7.0 ) .or.
     &                     jday .ge. ( crpdt3 + 14.0 ) ) ) .or.
     &                   ( crpdt1 .gt. crpdt3 .and.
     &                   ( jday .le. ( crpdt1 - 7.0 ) .and.
     &                     jday .ge. ( crpdt3 + 14.0 ) ) ) ) )
     &               pctf = 0.0
               else if ( jday .ge. crpdt1 .and.
     &                   jday .le. ( crpdt1 + 14.0 ) ) then   ! in 2 weeks after planting if2
                  ttmp = ( jday - crpdt1 ) / 14.0
                  pctf = 1.0
                  potf = 0.6 * ttmp
               else if ( crpdt1 .lt. crpdt2 ) then  ! planting end same year if2
                  if ( jday .ge. ( crpdt1 + 14.0 ) .and.
     &                 jday .le. crpdt2 ) then   ! after 2 weeks planting to the end planting begin if3
                     ttmp = (   jday - ( crpdt1 + 14.0 ) )
     &                    / ( crpdt2 - ( crpdt1 + 14.0 ) )
                     pctf = ttmp
                     potf = 0.6 * ttmp
                  else     ! in harvesting if3
                     pctf = 0.0
                     potf = 0.0
                  end if   ! if3
               else                            ! planting start this year end next if2
                  if ( jday .ge. crpdt2 ) then      ! in harvesting begin if4
                     pctf = 0.0
                     potf = 0.0
                  else                              ! in planting if4
                     if ( jday .ge. ( crpdt1 + 14.0 ) ) then  ! in planting at year end begin if5
                        ttmp = (               jday - ( crpdt1 + 14.0 ) )
     &                       / ( ( crpdt2 + 365.0 ) - ( crpdt1 + 14.0 ) )
                     else                                   ! in planting at year begin if5
                        ttmp = (   ( jday + 365.0 ) - ( crpdt1 + 14.0 ) )
     &                       / ( ( crpdt2 + 365.0 ) - ( crpdt1 + 14.0 ) )
                     end if   ! if5
                     pctf = ttmp
                     potf = 0.6 * ttmp
                  end if   ! if4
               end if   ! if2

               ! possible 2 season for barley, oats or wheat
               twos = 1.0
               if ( ( i .eq.  2 .and. cropdt( c,r, 2,1 ) .ne. 0.0 .and.
     &                                cropdt( c,r, 3,1 ) .ne. 0.0 ) .or.
     &              ( i .eq.  8 .and. cropdt( c,r, 7,1 ) .ne. 0.0 .and.
     &                                cropdt( c,r, 8,1 ) .ne. 0.0 ) .or.
     &              ( i .eq. 17 .and. cropdt( c,r,17,1 ) .ne. 0.0 .and.
     &                                cropdt( c,r,18,1 ) .ne. 0.0 ) )
     &            twos = 0.5   ! if 2 season crop then 50-50 each

               crplnd( c,r,i ) =     twos * crop_ladut( c,r,i )
     &                           * ( pctf * ctilf( i )
     &                           +   potf * ( 1.0 - ntilf( i ) - ctilf( i ) ) ) 

               agland( c,r ) = agland( c,r ) + crplnd( c,r,i )

               if ( i .eq. 2 .and. m .eq. 2 ) then          ! Barley: spring -> fall
                  m = 3
                  go to 3344
               else if ( i .eq. 8 .and. m .eq. 7 ) then     ! Oats: spring -> fall
                  m = 8
                  go to 3344
               else if ( i .eq. 17 .and. m .eq. 17 ) then   ! Wheat: spring -> winter
                  m = 18
                  go to 3344
               end if

7744           continue

            end if   ! if1
         end do   ! i
      end do   ! c
      end do   ! r

      return
      end subroutine cropcal

