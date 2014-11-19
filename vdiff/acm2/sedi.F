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
C $Header: /project/yoj/arc/CCTM/src/vdiff/acm2/vdiffacm2.F,v 1.13 2012/01/19 14:37:47 yoj Exp $
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C Revision History:
C 07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine sedi ( dtsec, sedvel, cgrid, cngrd )

      use cgrid_spcs          ! CGRID mechanism species
      use grid_conf
      use asx_data_mod
      use vdiff_map
      use utilio_defn
      use vdiff_diag, dtccr => dtccr_mean
      implicit none

!      include SUBST_CONST     ! constants: mwair, avo

C Arguments:
      real, intent( in )    :: dtsec                ! model time step in seconds
C grav settling velocity applies only to coarse mode aerosols (J-,K-mode), but the VGS
C array is filled for *all* CGRID diffused species. VGS is set to zero for all the non-
C coarse mode aerosols.
      real, intent( out )   :: sedvel( :,:,:,: ) ! grav settling velocity
      real, intent( inout ) :: cngrd  ( :,:,:,: ) ! cgrid replacement
      real, pointer :: cgrid( :,:,:,: )

c Parameters:
      real, parameter :: alpha = 1.1
!     real, parameter :: alpha = 2.0
      real, parameter :: gpkg = 1.0e+03        ! g/Kg
      real, parameter :: maogpkg = mwair / gpkg
      real, parameter :: gpkgoma = 1.0 / maogpkg
      real, parameter :: maoavo1000 = 1.0e+03 * mwair / avo
      real, parameter :: avooma_001 = 1.0 / maoavo1000

c External Functions:

C Local Variables:

      character( 120 ) :: xmsg = ' '
      character( 16 ), save :: pname = 'SEDI'
      logical, save :: firstime = .TRUE.
      integer, save :: logdev

      real,    allocatable, save :: cgrd( :,: )    ! CGRID subset
      real,    allocatable, save :: conc( :,: )    ! secondary CGRID expression
      real,    allocatable, save :: vsed( :,: )    ! settling vel.
      real,    allocatable, save :: vsed_ae( :,: ) ! settling vel.
      real,    allocatable, save :: dtsvsed( :,: ) ! settling vel. factor
      real,    allocatable, save :: sumvsed( :,: ) ! for diagnostics
      integer, allocatable, save :: sedi_map( : )
      integer, allocatable, save :: conc_map( : )
      real       adts, dts, rdts
      real       rdl, fs
      integer    off, dtc
      integer, save :: n_sedi_map
      integer    c, r, l, v, n
      integer    astat
      integer    mdate, mtime

      interface
         subroutine aero_sedv ( col, row, cgrd, vsed_ae )
         use grid_conf
         use cgrid_spcs
         use utilio_defn
         use asx_data_mod
         use aero_data
         use soa_defn
         use aeromet_data
         implicit none
         integer, intent( in )  :: col, row
         real,    intent( in )  :: cgrd( :,: )
         real,    intent( out ) :: vsed_ae( :,: )
         end subroutine aero_sedv
      end interface

C-----------------------------------------------------------------------

      if ( firstime ) then

         firstime = .false.
         logdev = init3()

         mdate = 0; mtime = 0

c sedi_map - from ae_trns to ae_spc
c conc_map - from cgrid to ae_trns species
         allocate ( sedi_map( n_ae_spc ),
     &              conc_map( n_ae_spc ),  stat = astat )
         if ( astat .ne. 0 ) then
            xmsg = 'Failure allocating VSED_MAP or CONC_MAP'
            call m3exit( pname, mdate, mtime, xmsg, xstat1 )
         end if

         off = n_gc_trns   ! 48
         n_sedi_map = 0
         do v = 1, n_ae_spc
            n = index1( ae_spc( v ), n_ae_trns, ae_trns )
            if ( n .gt. 0 ) then
               n_sedi_map = n_sedi_map + 1
               sedi_map( n_sedi_map ) = n
               conc_map( n_sedi_map ) = diff_map( n + off )
            end if
         end do
         write( logdev,'( 19x, "SEDI_MAP", 4x, "CONC_MAP" )' )
         do v = 1, n_sedi_map
            write( logdev,* ) v, sedi_map( v ), conc_map( v )
         end do

         allocate ( vsed_ae( n_ae_spc,nlays ), stat = astat )
         if ( astat .ne. 0 ) then
            xmsg = 'Failure allocating VSED_AE'
            call m3exit( pname, mdate, mtime, xmsg, xstat1 )
         end if
         vsed_ae = 0.0  ! array assignment

         allocate ( cgrd( nlays,size( cgrid,4 ) ), stat = astat )
         if ( astat .ne. 0 ) then
            xmsg = 'Failure allocating CGRD'
            call m3exit( pname, mdate, mtime, xmsg, xstat1 )
         end if

         allocate ( conc( n_sedi_map,nlays ),
     &              vsed( n_sedi_map,nlays ),
     &              dtsvsed( n_sedi_map,nlays ), stat = astat )
         if ( astat .ne. 0 ) then
            xmsg = 'Failure allocating CONC, VSED,  or DTSVSED'
            call m3exit( pname, mdate, mtime, xmsg, xstat1 )
         end if
         conc = 0.0  ! array assignment
         vsed = 0.0  ! array assignment

         if ( vdiffdiag ) then
            allocate ( sumvsed( n_ae_spc,nlays ), stat = astat )
            if ( astat .ne. 0 ) then
               xmsg = 'Failure allocating SUMVSED'
               call m3exit( pname, mdate, mtime, xmsg, xstat1 )
            end if
         end if

      end if   !  if firstime

      dtccr = 0   ! array assignment
      do 345 r = 1, my_nrows
      do 344 c = 1, my_ncols

!        do v = 1, n_sedi_map
C subset all the layers and species in cgrid for this grid cell
         do v = 1, size( cgrid,4 )
            do l = 1, nlays
!              cgrd( l,v ) = cgrid( c,r,l, conc_map( v ) )
               cgrd( l,v ) = cgrid( c,r,l,v )
            end do
         end do

         if ( vdiffdiag ) then
            sumvsed = 0.0   ! array assignment
         end if
         adts = 0.0; dtc = 0
         do while ( adts .lt. dtsec )

            call aero_sedv( c, r, cgrd, vsed_ae )
            if ( vdiffdiag ) then
               do l = 1, nlays
                  do v = 1, n_ae_spc
                     sumvsed( v,l ) = sumvsed( v,l ) + vsed_ae( v,l )
                  end do
               end do
            end if

            dtsvsed = 0.0   ! array assignment
            do l = 1, nlays
               do v = 1, n_sedi_map
                  conc( v,l ) = cngrd( conc_map( v ),l,c,r )
                  vsed( v,l ) = vsed_ae( sedi_map( v ),l )
               end do
            end do

            rdts = 1.0 / dtsec
            do l = 1, nlays
               rdl = alpha / Met_Data%DZF( c,r,l )
               do v = 1, n_sedi_map
                  fs = rdl * vsed( v,l )
                  if ( rdts .lt. fs ) rdts = fs
               end do
            end do
            dts = 1.0 / rdts
            adts = adts + dts
            dtc = dtc + 1

            do l = 1, nlays
               do v = 1, n_sedi_map
                  dtsvsed( v,l ) = dts * Grid_Data%rdx3f( l ) * vsed( v,l ) * Met_Data%rjacf( c,r,l )
               end do
            end do

            l = 1
            do v = 1, n_sedi_map
               conc( v,l ) = conc( v,l )
     &                     + dtsvsed( v,l+1 ) * conc( v,l+1 )
            end do

            do l = 2, nlays-1
               do v = 1, n_sedi_map
                  conc( v,l ) = conc( v,l )
     &                        + dtsvsed( v,l+1 ) * conc( v,l+1 )
     &                        - dtsvsed( v,l )   * conc( v,l )
               end do
            end do
            l = nlays
            do v = 1, n_sedi_map
               conc( v,l ) = conc( v,l )
     &                     - dtsvsed( v,l ) * conc( v,l )
            end do

         end do   ! while

         do l = 1, nlays
            do v = 1, n_sedi_map
               cngrd( conc_map( v ),l,c,r ) = conc( v,l )
            end do
         end do

         if ( vdiffdiag ) then
            dtccr( c,r ) = real( dtc )
            do l = 1, nlays
               do v = 1, n_ae_spc
                  sedvel( v,l,c,r ) = sumvsed( v,l ) / real( dtc )
               end do
            end do
         end if

344   continue         !  end loop on col C
345   continue         !  end loop on row R

      return
      end

