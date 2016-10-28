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
      module sedimentation

C Revision History:
C 03 Jul 15 J.Young: inital

      implicit none

      public sedi
      private

      include SUBST_CONST     ! constants
      include SUBST_FILES_ID  ! file name parameters

      real,    parameter :: gpkg = 1.0e+03        ! g/Kg
      real,    parameter :: maogpkg = mwair / gpkg
      real,    parameter :: gpkgoma = 1.0 / maogpkg
      real,    parameter :: maoavo1000 = 1.0e+03 * mwair / avo
      real,    parameter :: avooma_001 = 1.0 / maoavo1000

      real,    allocatable, save :: cgrd( :,: )   ! density units
      real,    allocatable, save :: conc( :,: )   ! mixing ratio units

      real,    allocatable, save :: dens( :,:,: ) ! air density
      real,    allocatable, save :: ldens( : )    ! time-local

      integer, save :: nqae              ! number of micro-grams/m**3 species
      integer, save :: nnae              ! number of #/m**3 species
      integer, save :: nsae              ! number of m**2/m**3 species
      integer, save :: cg_off            ! cngrd offset to aero species
      integer, allocatable, save :: qae( : ) ! cgrd pointer to micro-grams/m**3 species
      integer, allocatable, save :: nae( : ) ! cgrd pointer to #/m**3 species
      integer, allocatable, save :: sae( : ) ! cgrd pointer to m**2/m**3 species
      real,    allocatable, save :: molwt( : ) ! only for "qae" species
!     integer, save :: odate, otime
      real    conv, fac            ! temp var
      integer ios
      integer, save :: logdev

      integer gxoff, gyoff         ! global origin offset from file
C for interpx
      integer, save :: strtcolmc3, endcolmc3, strtrowmc3, endrowmc3

      contains

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine sedi ( jdate, jtime, dtsec, sedvel, cgrid, cngrd )

C Revision History:
C           J.Young, J.Pleim: inital
C 07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C 05 Mar 15 J.Pleim: correct layer thickness index off by 1
C 15 Jul 15 J.Young: correct sub timestep calculation and apply Martin Otte`s
C                    correction for the first-order upstream sedimentation
C-----------------------------------------------------------------------

      use cgrid_spcs          ! cgrd mechanism species
      use grid_conf
      use asx_data_mod
      use vdiff_map
      use utilio_defn
      use vdiff_diag, dtccr => dtccr_mean
      implicit none

C Arguments:
      integer, intent( in )    :: jdate, jtime       !
      real,    intent( in )    :: dtsec              ! model time step in seconds
C grav settling velocity applies only to coarse mode aerosols (J-,K-mode), but the VGS
C array is filled for *all* cgrd diffused species. VGS is set to zero for all the non-
C coarse mode aerosols.
      real,    intent( out )   :: sedvel( :,:,:,: )  ! grav settling vel. for diagnostic
      real,    pointer         :: cgrid ( :,:,:,: )
      real,    intent( inout ) :: cngrd ( :,:,:,: )  ! cgrid replacement

c Parameters:
      real, parameter :: alpha = 1.1
!     real, parameter :: alpha = 2.0

c External Functions:

C Local Variables:

      character( 120 ) :: xmsg = ' '
      character( 16 ), save :: pname = 'SEDI'
      logical, save :: firstime = .TRUE.

      real,    allocatable, save :: vsed    ( :,: ) ! settling vel.
      real,    allocatable, save :: vsed_ae ( :,: ) ! settling vel.
      real,    allocatable, save :: dtsvsed ( :,: ) ! settling vel. factor
      real,    allocatable, save :: sumvsed ( :,: ) ! for diagnostics
      real,    allocatable, save :: rdl     ( : )   ! subloop var
      real,    allocatable, save :: rrhodz  ( : )   ! reciprocal rho * deltaZ
      integer, allocatable, save :: sedi_map( : )
      integer, allocatable, save :: conc_map( : )
      real       adts, dts, rdts
      real       ldt, fs
      integer    off, dtc
      integer, save :: n_sedi_map   ! aero species, all modes
      integer    c, r, l, n, s, v
      integer    astat
      integer    mdate, mtime

!     logical    chk
!     logical :: wrt1 = .true.
!     character( 43 ), save :: dbgstr = "@@c    l   Dzf           vsed          jacf"
!     character( 75 ), save :: dbgstr1 =
!    &  "@@1    l    v   srdl          rdl           svsed         vsed         rdts"
!     real       srdl, svsed

      interface   ! for external procedures
         subroutine aero_sedv ( col, row, cgrd, vsed_ae )
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

c sedi_map - from ae_trns to ae_spc (currently, all ae spc`s are transported)
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

         allocate ( rdl( nlays ),
     &              rrhodz( nlays ),
     &              ldens( nlays ), stat = astat )
         if ( astat .ne. 0 ) then
            xmsg = 'Failure allocating RDL, RRHODZ, or LDENS'
            call m3exit( pname, mdate, mtime, xmsg, xstat1 )
         end if

         if ( vdiffdiag ) then
            allocate ( sumvsed( n_ae_spc,nlays ), stat = astat )
            if ( astat .ne. 0 ) then
               xmsg = 'Failure allocating SUMVSED'
               call m3exit( pname, mdate, mtime, xmsg, xstat1 )
            end if
         end if

C set up the convert arrays
c create aerosol species pointers to distinguish micro-grams/m**3,
C #/m**3 (number density), and m**2/m**3 (surface area) species

         allocate ( qae( n_ae_spc ),
     &              nae( n_ae_spc ),
     &              sae( n_ae_spc ),
     &              molwt( n_ae_spc ), stat = ios )
         if ( ios .ne. 0 ) then
            xmsg = 'Failure allocating qae, nae, sae, or molwt'
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if
         nqae = 0       ! no. of micro-grams/m**3 species
         nnae = 0       ! no. of  #/m**3 species
         nsae = 0       ! no. of  m**2/m**3 species
         cg_off = ae_strt - 1
         off = 0        ! aero species offset to local conc array
         do s = 1, n_ae_spc
            if ( ae_spc( s )( 1:3 ) .eq. 'NUM' ) then
               nnae = nnae + 1
               nae( nnae ) = off + s
            else if ( ae_spc( s )( 1:3 ) .eq. 'SRF' ) then
               nsae = nsae + 1
               sae( nsae ) = off + s
            else
               nqae = nqae + 1
               qae( nqae ) = off + s
               molwt( nqae ) = ae_molwt( s )
            end if
         end do

C=======================================================================
!        write( logdev,'( 2(a, i5) )' ), ' off: ', off, ' cg_off: ', cg_off
!        write( logdev,'( a, i5 )' ), ' nae: ', nnae
!        do s = 1, nnae
!           write( logdev,'( 2i5 )' ), s, nae( s )
!        end do
!        write( logdev,'( a, i5 )' ), ' sae: ', nsae
!        do s = 1, nsae
!           write( logdev,'( 2i5 )' ), s, sae( s )
!        end do
!        write( logdev,'( a, i5 )' ), ' qae: ', nqae
!        do s = 1, nqae
!           write( logdev,'( 2i5,f10.4 )' ), s, qae( s ), ae_molwt( qae( s ) )
!        end do
C=======================================================================

      end if   !  if firstime

      do 345 r = 1, my_nrows
      do 344 c = 1, my_ncols

C subset all the layers and species in cgrid for this grid cell
         do v = 1, size( cgrid,4 )
            do l = 1, nlays
               cgrd( l,v ) = cgrid( c,r,l,v )
            end do
         end do

         do l = 1, nlays
            rdl( l ) = alpha * Met_Data%rjacf( c,r,l ) * Grid_Data%rdx3f( l )
            rrhodz( l ) = Met_Data%rrhoj( c,r,l ) * Grid_Data%rdx3f( l )
            ldens( l ) = Met_Data%dens( c,r,l )
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

 ! cgrd -> conc: from density units to mixing ratio units
            call conv_cgrd( )

            do l = 1, nlays
               do v = 1, n_sedi_map
                  vsed( v,l ) = vsed_ae( sedi_map( v ),l )
               end do
            end do

            rdts = 1.0 / dtsec
            do l = 1, nlays
               do v = 1, n_sedi_map
                  fs = rdl( l ) * vsed( v,l )
                  if ( rdts .lt. fs ) rdts = fs
               end do
            end do
            dts = 1.0 / rdts
            adts = adts + dts
            if ( adts .gt. dtsec ) then   ! don`t overshoot the timestep
               dts = adts - dtsec
               adts = dtsec
            end if
            dtc = dtc + 1

            do l = 1, nlays
               ldt = dts * ldens( l )
               do v = 1, n_sedi_map
                  dtsvsed( v,l ) = ldt * vsed( v,l )
               end do
            end do

            l = 1
            do v = 1, n_sedi_map
               conc( v,l ) = conc( v,l )
     &                     + dtsvsed( v,l+1 ) * conc( v,l+1 ) * rrhodz( l )
            end do

            do l = 2, nlays-1
               do v = 1, n_sedi_map
                  conc( v,l ) = conc( v,l )
     &                        + ( dtsvsed( v,l+1 ) * conc( v,l+1 )
     &                        -   dtsvsed( v,l )   * conc( v,l ) ) * rrhodz( l )
               end do
            end do
            l = nlays
            do v = 1, n_sedi_map
               conc( v,l ) = conc( v,l )
     &                     - dtsvsed( v,l ) * conc( v,l ) * rrhodz( l )
            end do

 ! conc -> cgrd: from mixing ratio units to density units
            call conv_conc( )

         end do   ! while

         if ( vdiffdiag ) then
            dtccr( c,r ) = real( dtc )
            do l = 1, nlays
               do v = 1, n_ae_spc
                  sedvel( v,l,c,r ) = sumvsed( v,l ) / real( dtc )
               end do
            end do
         end if

         do l = 1, nlays
            do v = 1, n_sedi_map
               cngrd( conc_map( v ),l,c,r ) = conc( v,l )
            end do
         end do

344   continue         !  end loop on col C
345   continue         !  end loop on row R

      return
      end subroutine sedi

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      subroutine conv_cgrd( )
         use grid_conf        ! horizontal & vertical domain specifications
         use utilio_defn
         use asx_data_mod
         implicit none

         integer   l, v            ! loop induction variables

C-----------------------------------------------------------------------

C micro-grams/m**3 aerosol -> ppmv
C (Don`t divide by MGPG, then multiply by 1.0E+6: 1/MGPG = 1.0E-6 cancels out ppm = 1.0E6)

         do l = 1, nlays
            fac = maogpkg / ldens( l )
            do v = 1, nqae
               conv = fac / molwt( v )
               conc( qae( v ),l ) = conv * cgrd( l,cg_off+qae( v ) )
            end do
         end do

C number/m**3 aerosol -> ppmv
C (Don`t divide by MGPG, etc. See note above)

         do l = 1, nlays
            conv = maoavo1000 / ldens( l )
            do v = 1, nnae
               conc( nae( v ),l ) = conv * cgrd( l,cg_off+nae( v ) )
            end do
         end do

C m**2/m**3 aerosol -> m**2/mol air

         do l = 1, nlays
            conv = maogpkg / ldens( l )
            do v = 1, nsae
               conc( sae( v ),l ) = conv * cgrd( l,cg_off+sae( v ) )
            end do
         end do

      return

      end subroutine conv_cgrd

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      subroutine conv_conc ( )
         use grid_conf        ! horizontal & vertical domain specifications
         implicit none

         integer   l, v            ! loop induction variables

C-----------------------------------------------------------------------

C m**2/m**3 aerosol -> m**2/mol air
C aerosol ppmv -> micro-grams/m**3
C (Don`t multiply by MGPG, then divide by 1.0E+6: 1/MGPG = 10**-6 cancels out
C ppm = 10**6)

         do v = 1, nqae
            fac = gpkgoma * molwt( v )
            do l = 1, nlays
               conv = fac * ldens( l )
               cgrd( l,cg_off+qae( v ) ) = conv * conc( qae( v ),l )
            end do
         end do

C aerosol ppmv -> number/m**3
C (Don`t multiply by MGPG, etc. See note above)

         do v = 1, nnae
            do l = 1, nlays
               conv = avooma_001 * ldens( l )
               cgrd( l,cg_off+nae( v ) ) = conv * conc( nae( v ),l )
            end do
         end do

C m**2/m**3 aerosol -> m**2/mol air

         do v = 1, nsae
            do l = 1, nlays
               conv = gpkgoma * ldens( l )
               cgrd( l,cg_off+sae( v ) ) = conv * conc( sae( v ),l )
            end do
         end do

      return

      end subroutine conv_conc

      end module sedimentation
