
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/tfbelow.F,v 1.2 2011/07/11 13:45:23 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine tfbelow ( jdate, jtime, uland, tfb )

C-----------------------------------------------------------------------
C Description:
C   Extracts selected landuse types from BELD01 and BELD03 and merges
C   the selections into a dust-related landuse array (ULAND).
C   Optionally, reads 3 gridded crop calendar file and calculates an
C   erodible agriculture land fraction.
C   Applies a predetermined removal fraction in and below canopy to
C   ULAND and determines a transport factor (TFB) for this regime.
 
C Subroutines and Functions Called:
C      OPEN3, INTERPX, XTRACT3, M3EXIT

C Revison History:
C  Jun 2009 D. Tong
C  Jan 2011 J. Young: mods for inline wind-blown dust module
C  Apr 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C  Jul 2011 J.Young: pad vnmld for uniform string lengths
C-----------------------------------------------------------------------

      use hgrd_defn           ! horizontal domain specifications
      use utilio_defn

      implicit none

C Includes:
      include SUBST_FILES_ID  ! file name parameters
      include SUBST_CONST     ! physical and mathematical constants

C Arguments:
      integer, intent( in )  :: jdate      ! current model date, coded YYYYDDD
      integer, intent( in )  :: jtime      ! current model time, coded HHMMSS
      real,    intent( out ) :: uland( :,:,: )  ! 4 landtypes fraction 
      real,    intent( out ) :: tfb  ( :,: ) ! dust transport factor from
                                             ! flow into canopy
 
      character(  16 ) :: pname = 'TFBELOW'
!     character(  16 ) :: hname = 'RAINMAP'
      character( 128 ) :: xmsg = ' '

!     integer, parameter :: nlutyp = 21   ! number of landuse types
!!!!!!! 12 km missing 'AGRICULTURE'
      integer, parameter :: nlutyp = 20   ! number of landuse types

      character( 16 ) :: vnmld( nlutyp ) =
     &         (/ 'USGS_urban      ', 'USGS_drycrop    ', 'USGS_irrcrop    ',
     &            'USGS_cropgrass  ', 'USGS_cropwdlnd  ', 'USGS_grassland  ', 
     &            'USGS_shrubland  ', 'USGS_shrubgrass ', 'USGS_savanna    ', 
     &            'USGS_decidforest', 'USGS_evbrdleaf  ', 'USGS_coniferfor ',
     &            'USGS_mxforest   ', 'USGS_water      ', 'USGS_wetwoods   ',
     &            'USGS_sprsbarren ', 'USGS_woodtundr  ', 'USGS_mxtundra   ',
!    &            'USGS_snowice    ', 'AGRICULTURE     ', 'FOREST          ' /)
     &            'USGS_snowice    ', 'FOREST          ' /)

      real   :: lut( ncols,nrows,nlutyp ) ! landuse type fraction

C uland merging:
C                                        water & barren surrounding
C        uland( c,r,1 ) = lut( c,r,7 )   'USGS_shrubland'
c                       + lut( c,r,14 )  'USGS_water'
c                       + lut( c,r,16 )  'USGS_sprsbarren'
c                       + lut( c,r,18 )  'USGS_mxtundra'
c                       + lut( c,r,19 )  'USGS_snowice'
c                                        grass surrounding
c        uland( c,r,2 ) = lut( c,r,6 )   'USGS_grassland'
c                       + lut( c,r,8 )   'USGS_shrubgrass'
c                       + lut( c,r,9 )   'USGS_savanna'
c                       + lut( c,r,17 )  'USGS_woodtundr'
c                                        crop surrounding
c                       + lut( c,r,2 )   'USGS_drycrop'
c                       + lut( c,r,3 )   'USGS_irrcrop'
c                       + lut( c,r,4 )   'USGS_cropgrass'
c                       + lut( c,r,5 )   'USGS_cropwdlnd'
c                       + lut( c,r,20 )  'agriculture' (beld3)
c                                        building surrounding
c        uland( c,r,3 ) = lut( c,r,1 )   'USGS_urban'
c                                        forest surrounding
c        uland( c,r,4 ) = lut( c,r,10 )  'USGS_decidforest'
c                       + lut( c,r,11 )  'USGS_evbrdleaf'
c                       + lut( c,r,12 )  'USGS_coniferfor'
c                       + lut( c,r,13 )  'USGS_mxforest'
c                       + lut( c,r,15 )  'USGS_wetwoods'
c                       + lut( c,r,21 )  'forest' (beld3)

!     real, allocatable :: rainf( :,: )         ! annual rainfall in inchs
      real              :: ufc  ! total weighted removal fraction for each cell
      real              :: tland

      integer i, c, r

      real   :: fw( 4 ) = ! removal fraction in and below canopy
     &         (/ 0.0,    ! water
     &            0.10,   ! grass&crop
     &            0.50,   ! building
     &            0.95 /) ! forest

!     real    :: fc( 4,2 ) = ! removal fraction in and below canopy
!    &      (/ 0.0, 0.10, 0.15, 0.20,   ! agriculture source dust
!    &         0.0, 0.30, 0.75, 0.90 /) ! road and construction source dust

      integer  gxoff, gyoff             ! global origin offset from file
      integer, save :: strtcol_01, endcol_01, strtrow_01, endrow_01
      integer, save :: strtcol_02, endcol_02, strtrow_02, endrow_02

!     integer  allocstat
c-----------------------------------------------------------------------

!dqt: We have disabled the rain impact on FW(2) 
!dqt  if ( .not. open3( hname, fsread3, pname ) ) then
!dqt     xmsg = 'could not open ' // hname // ' file'
!dqt     call m3exit( pname, jdate, jtime, xmsg, xstat1 )
!dqt  end if

!dqt  call subhfile ( hname, gxoff, gyoff,
!dqt &    strtcol, endcol, strtrow, endrow )

!dqt  allocate ( rainf( ncols,nrows ), stat = allocstat )
!dqt  if ( allocstat .ne. 0 ) then
!dqt     xmsg = 'Failure allocating rainf'
!dqt     call m3exit( pname, jdate, jtime, xmsg, xstat1 )
!dqt  end if

!dqt  if ( .not. interpx ( hname, 'rain', pname,
!dqt &                     strtcol,endcol,strtrow,endrow, 1,1,
!dqt &                     jdate, jtime, rainf ) ) then
!dqt     xmsg = 'Could not read rainfall from ' // hname
!dqt     call m3exit ( pname, jdate, jtime, xmsg, xstat1 )
!dqt  end if

C Open the BELD1 file
      if ( .not. open3( dust_lu_1, fsread3, pname ) ) then
         xmsg = 'Could not open ' // dust_lu_1 // ' file'
         call m3exit( pname, 0, 0, xmsg, xstat1 )
      end if

C Open the BELD3 file
      if ( .not. open3( dust_lu_2, fsread3, pname ) ) then
         xmsg = 'Could not open ' // dust_lu_2 // ' file'
         call m3exit( pname, 0, 0, xmsg, xstat1 )
      end if

C Get domain window info
      call subhfile ( dust_lu_1, gxoff, gyoff,
     &                strtcol_01, endcol_01, strtrow_01, endrow_01 )

      call subhfile ( dust_lu_2, gxoff, gyoff,
     &                strtcol_02, endcol_02, strtrow_02, endrow_02 )

C Get landuse (fraction) data
!     do i = 1, nlutyp-2   ! dust_lu_1
      do i = 1, nlutyp-1   ! dust_lu_1
         if ( .not. xtract3( dust_lu_1, vnmld( i ), 1,1,
     &                       strtrow_01,endrow_01, strtcol_01,endcol_01,
     &                       0, 0, lut( 1,1,i ) ) ) then
            xmsg = 'Could not read ' // trim( vnmld( i ) ) // ' from DUST_LU_1'
            call m3exit ( pname, 0, 0, xmsg, xstat1 )
         end if
      end do

!     do i = nlutyp-1, nlutyp   ! dust_lu_2
      do i = nlutyp, nlutyp   ! dust_lu_2
         if ( .not. xtract3( dust_lu_2, vnmld( i ), 1,1,
     &                       strtrow_02,endrow_02, strtcol_02,endcol_02,
     &                       0, 0, lut( 1,1,i ) ) ) then
           xmsg = 'Could not read ' // trim( vnmld( i ) ) // ' from DUST_LU_2'
           call m3exit ( pname, 0, 0, xmsg, xstat1 )
         end if
      end do

C Merge fractional BELD landuse types into 4 categories for dust removal
      do r = 1, my_nrows
      do c = 1, my_ncols
         uland( c,r,1 ) = lut( c,r,7 )  + lut( c,r,14 )  ! water & barren surrounding
     &                  + lut( c,r,16 ) + lut( c,r,18 )
     &                  + lut( c,r,19 )
         uland( c,r,2 ) = lut( c,r,6 )  + lut( c,r,8 )   ! grass surrounding
     &                  + lut( c,r,9 )  + lut( c,r,17 )
     &                  + lut( c,r,2 )  + lut( c,r,3 )   ! crop surrounding
     &                  + lut( c,r,4 )  + lut( c,r,5 )
!    &                  + lut( c,r,20 )
         uland( c,r,3 ) = lut( c,r,1 )                   ! building surrounding
         uland( c,r,4 ) = lut( c,r,10 ) + lut( c,r,11 )  ! forest surrounding
     &                  + lut( c,r,12 ) + lut( c,r,13 )
!    &                  + lut( c,r,15 ) + lut( c,r,21 )
     &                  + lut( c,r,15 ) + lut( c,r,20 )

         tland = 0.0
         do i = 1, 4
            uland( c,r,i ) = max( 0.0, min( 100.0, uland( c,r,i ) ) )
            tland = tland + uland( c,r,i )
         end do

         if ( tland .gt. 100.0 ) then
            do i = 1, 4
               uland( c,r,i ) = uland( c,r,i ) * 100.0 / tland
            end do
         end if

!dqt Windbreak effect on grass&crop
!dqt     if( uland( c,r,4 ) .gt. 5 .or. rainf( c,r ) .gt. 15 ) fw( 2 ) = 0.20

         ufc = 0.0
         do i = 1, 4
!           ufc = ufc + fw( i ) * uland( c,r,i ) * 0.01
            ufc = ufc + fw( i ) * uland( c,r,i )
         end do
!        tfb( c,r ) = 1.0 - ufc
         tfb( c,r ) = 1.0 - ufc * 0.01

         if ( tfb( c,r ) .gt. 1.0 .or. tfb( c,r ) .lt. 0.0 ) then
            write( xmsg,'( a, e10.3, a, 2i4 )' ) 'Transport factor error: ',
     &            tfb( c,r ), ' At col, row: ', c, r 
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

      end do   ! my_ncols
      end do   ! my_nrows

      return
      end

