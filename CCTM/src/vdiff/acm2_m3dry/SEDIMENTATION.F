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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      module sedimentation

C Revision History:
C 03 Jul 15 J.Young: inital

      implicit none

      public sedi

      private

      include SUBST_CONST     ! constants
      include SUBST_FILES_ID  ! file name parameters

      real,    allocatable, save :: cgrd( :,: )   ! density units
      real,    allocatable, save :: conc( :,: )   ! mixing ratio units
#ifdef isam
      real(8), allocatable, save :: conc_isam( :,:,: )      ! extracted isam mixing ratio units
#endif

      real,    allocatable, save :: ldens( : )    ! local density kg m-3

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

      integer gxoff, gyoff         ! global origin offset from file
C for interpx
      integer, save :: strtcolmc3, endcolmc3, strtrowmc3, endrowmc3
      integer, allocatable, save :: map_sedi2cgrid( : )
      integer, save :: n_sedi_map   ! aero species, all modes
#ifdef isam
      integer,              save :: n_isam_sedi_map  ! isam aero species, all modes
      integer, allocatable, save :: isam_sedi_map( : )
#endif

      contains

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine sedi ( jdate, jtime, dtsec, sedvel, cgrid, cngrd )
C Revision History:
C           J.Young, J.Pleim: inital
C 07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C 05 Mar 15 J.Pleim: correct layer thickness index off by 1
C 15 Jul 15 J.Young: correct sub timestep calculation and apply Martin Otte`s
C                    correction for the first-order upstream sedimentation
C  1 Feb 19 David Wong: removed all MY_N clauses
C-----------------------------------------------------------------------

      use cgrid_spcs          ! cgrd mechanism species
      use grid_conf
      use asx_data_mod
      use vdiff_map
      use utilio_defn
      use vdiff_diag, dtccr => dtccr_mean
#ifdef isam
      use sa_defn
#endif

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
#ifdef isam
      integer, allocatable, save :: map_sedi_to_isam( : )
      real,    allocatable, save :: isam_dtsvsed ( : ) ! isam settling vel. factor
#endif
      real       adts, dts, rdts
      real       ldt, fs
      integer    off, dtc, iter 
      integer    c, r, l, n, s, v
      integer    astat
      integer    mdate, mtime

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

         mdate = 0; mtime = 0

c sedi_map - from ae_trns to ae_spc (currently, all ae spc`s are transported)
c conc_map - from cgrid to ae_trns species
         allocate ( sedi_map( n_ae_spc ),
     &              map_sedi2cgrid( n_cgrid_spc ),
     &              conc_map( n_ae_spc ),  stat = astat )
         if ( astat .ne. 0 ) then
            xmsg = 'Failure allocating VSED_MAP or CONC_MAP'
            call m3exit( pname, mdate, mtime, xmsg, xstat1 )
         end if
#ifdef isam
         allocate ( isam_sedi_map( nspc_sa_ae ),
     &              map_sedi_to_isam( nspc_sa_ae ),  stat = astat )
         if ( astat .ne. 0 ) then
            xmsg = 'Failure allocating map_sedi_to_isam or isam_sedi_map'
            call m3exit( pname, mdate, mtime, xmsg, xstat1 )
         end if
         isam_sedi_map = 0
         map_sedi_to_isam = 0
         n_isam_sedi_map = 0
#endif
         map_sedi2cgrid = 0
         off = n_gc_trns   
         cg_off = ae_strt - 1   
         n_sedi_map = 0
         do v = 1, n_ae_spc
            n = index1( ae_spc( v ), n_ae_trns, ae_trns )
            if ( n .gt. 0 ) then
               n_sedi_map = n_sedi_map + 1
               sedi_map( n_sedi_map ) = n
               map_sedi2cgrid( n_sedi_map ) = n+cg_off
               conc_map( n_sedi_map ) = diff_map( n + off )
#ifdef isam
               do s = 1, nspc_sa_ae
                  if(  isam_to_aero( s ) .eq. sedi_map( n_sedi_map ) ) then
                     n_isam_sedi_map =  n_isam_sedi_map + 1
                     isam_sedi_map(  n_isam_sedi_map ) = sedi_map( n_sedi_map )   
                     map_sedi_to_isam(  n_isam_sedi_map ) = sa_aero_indx( s )   
                  end if
               end do
#endif
            end if
         end do
         write( logdev,'( 19x, "SEDI_MAP", 4x, "CONC_MAP" )' )
         do v = 1, n_sedi_map
            write( logdev,* ) v, sedi_map( v ), conc_map( v )
         end do
#if defined(verbose_isam) && defined(isam)
         write(logdev,'("ISAM Aerosol Sedimentation Mapping")')
         write( logdev,'( A4,1X,2(A16,1X),3(A8,1x) )' )"  N ","   Bulk AERO   ","    ISAM AERO   ",
     &   "SEDI_MAP", "CONC_MAP","MOLE_WGT"
         do v = 1, n_isam_sedi_map
            write( logdev,'( I4,1X,2(A16,1X),2(I8,1x),F8.2 )' ) v, ae_spc( isam_sedi_map( v ) ),
     &      isam_spec( map_sedi_to_isam( v ),1 ), isam_sedi_map( v ), map_sedi_to_isam( v ),
     &      CGRID_MW( MAP_SEDI2CGRID(isam_sedi_map( v )) )
         end do
#endif

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
#ifdef isam
         allocate ( conc_isam( n_isam_sedi_map,nlays,ntag_sa ), 
     &              isam_dtsvsed( nlays ), stat = astat )
         if ( astat .ne. 0 ) then
            xmsg = 'Failure allocating CONC0,CONC_ISAM'
            call m3exit( pname, mdate, mtime, xmsg, xstat1 )
         end if
#endif

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

      end if   !  if firstime

      do 345 r = 1, nrows
      do 344 c = 1, ncols

C subset all the layers and species in cgrid for this grid cell
         do v = 1, size( cgrid,4 )
            do l = 1, nlays
               cgrd( l,v ) = cgrid( c,r,l,v )
            end do
         end do

#ifdef isam
         do v = 1,  n_isam_sedi_map
            n = map_sedi_to_isam( v )
            do l = 1, nlays
               do itag = 1,ntag_sa
! used upper case to not conflict with CCP flag
                  conc_isam( v,l,itag ) = ISAM( c,r,l,n,itag )
               end do
            end do
         end do
#endif
         do l = 1, nlays
#ifdef mpas
            rdl( l ) = alpha * Met_Data%rjacf( c,r,l ) * inv_cell_thickness(c, r, l)
            rrhodz( l ) = Met_Data%rrhoj( c,r,l ) * inv_cell_thickness(c, r, l)
#else
            rdl( l ) = alpha * Met_Data%rjacf( c,r,l ) * Grid_Data%rdx3f( l )
            rrhodz( l ) = Met_Data%rrhoj( c,r,l ) * Grid_Data%rdx3f( l )
#endif

            ldens( l ) = Met_Data%dens( c,r,l )
         end do

         if ( vdiffdiag ) then
            sumvsed = 0.0   ! array assignment
         end if
         adts = 0.0; dtc = 0; iter = 0

         do while ( adts .lt. dtsec .and. iter .le. 10 )
            iter = iter + 1

            call aero_sedv( c, r, cgrd, vsed_ae )
            if ( vdiffdiag ) sumvsed = sumvsed + vsed_ae

            ! cgrd -> conc: from density units to mixing ratio units
            call conv_cgrd( )

            do v = 1, n_sedi_map
               vsed( v,: ) = vsed_ae( sedi_map( v ),: )
            end do

            ! Assess reciprocal of the time step and increase it if
            ! one of the surrogates shows a very fast sedimentation 
            ! velocity
            rdts = 1.0 / dtsec
            do l = 1, nlays
               do v = 1, n_sedi_map
                  fs = rdl( l ) * vsed( v,l )
                  if ( rdts .lt. fs ) rdts = fs
               end do
            end do
            dts = 1.0 / rdts
            
            ! Add the new time step increment
            adts = adts + dts
            if ( adts .gt. dtsec ) then   ! don`t overshoot the timestep
               dts = dtsec - (adts - dts)
               adts = dtsec
            end if

            dtc = dtc + 1

            ! Integrate sedimentation velocity change in time
            do l = 1, nlays
               ldt = dts * ldens( l )
               do v = 1, n_sedi_map
                  dtsvsed( v,l ) = ldt * vsed( v,l )
               end do
            end do

            ! Important Issue: Are these rrhodz values being applied
            ! correctly or should they be matched to each layer?

            ! Apply sedimentation from layer 2 to layer 1, only
            ! production
            l = 1
            do v = 1, n_sedi_map
               conc( v,l ) = conc( v,l )
     &                     + dtsvsed( v,l+1 ) * conc( v,l+1 ) * rrhodz( l )
            end do
#ifdef isam
            do v = 1, n_isam_sedi_map
               s = isam_sedi_map( v )
               do itag = 1,ntag_sa
                  conc_isam( v,l,itag ) = conc_isam( v,l,itag )
     &                                  + dtsvsed( s,l+1 ) * conc_isam( v,l+1,itag ) * rrhodz( l )
               end do
            end do
#endif

            ! Apply production and loss to layers 2 through NZ-1
            do l = 2, nlays-1
               do v = 1, n_sedi_map
                  conc( v,l ) = conc( v,l )
     &                        + ( dtsvsed( v,l+1 ) * conc( v,l+1 )
     &                        -   dtsvsed( v,l )   * conc( v,l ) ) * rrhodz( l )
               end do
#ifdef isam
                do v = 1, n_isam_sedi_map
                   s = isam_sedi_map( v )
                   do itag = 1,ntag_sa
                      conc_isam( v,l,itag ) = conc_isam( v,l,itag )
     &                                      + dtsvsed( s,l+1 ) * conc_isam( v,l+1,itag ) * rrhodz( l )
     &                                      - dtsvsed( s,l )   * conc_isam( v,l,itag   ) * rrhodz( l )

                   end do
                end do
#endif
            end do

            ! Apply only loss to layer NZ
            l = nlays
            do v = 1, n_sedi_map
               conc( v,l ) = conc( v,l )
     &                     - dtsvsed( v,l ) * conc( v,l ) * rrhodz( l )
            end do
#ifdef isam
             do v = 1, n_isam_sedi_map
                s = isam_sedi_map( v )
                do itag = 1,ntag_sa
                   conc_isam( v,l,itag ) = conc_isam( v,l,itag )
     &                                   - dtsvsed( s,l ) * conc_isam( v,l,itag ) * rrhodz( l )
                end do 
             end do
#endif

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
#ifdef isam
         do v = 1,  n_isam_sedi_map
            n = map_sedi_to_isam( v )
            do l = 1, nlays
               do itag = 1,ntag_sa
! used upper case to not conflict with CCP flag
                  ISAM( c,r,l,n,itag ) = conc_isam( v,l,itag )
               end do
            end do
         end do
#endif
#if defined(isam) && defined(verbose_isam)
         if( c .eq. 1 .and. r .eq. 1 )then
            write( logdev,'("ISAM Aerosol Sedimentation Results for col and row equal 1")')
            write( logdev,'( A4,1X,2(A16,1X),2(A12,1x) )' )"  N ","   Bulk AERO ","    ISAM AERO   ",
     &      "SUM_BULK ", "SUM_ISAM "
            do v = 1, n_isam_sedi_map
               write( logdev,'( I4,1X,2(A16,1X),2(ES12.4,1x) )' ) v, ae_spc( isam_sedi_map( v ) ),
     &         isam_spec( map_sedi_to_isam( v ),1 ), sum( conc( isam_sedi_map( v ),1:nlays ) ), 
     &         sum(conc_isam( v,1:nlays,1:ntag_sa ))
            end do
         end if
#endif


344   continue         !  end loop on col C
345   continue         !  end loop on row R

      return
      end subroutine sedi

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      subroutine conv_cgrd( )
         use cgrid_spcs          ! cgrd mechanism species
         use grid_conf        ! horizontal & vertical domain specifications
         use utilio_defn
         use asx_data_mod
         use vdiff_map

         implicit none

         integer   i, l, v            ! loop induction variables
         real FAC1( nlays )
         real, parameter :: mwair_si = mwair * 1.0e-3

C-----------------------------------------------------------------------

      FAC1 = MWAIR_SI / LDENS  ! kg m-3 -> m3 mol-1

      DO I = 1,N_SEDI_MAP
          ! Aerosol Mass: ug/m3 -> ppmv
          IF ( CGRID_MASK_AERO( MAP_SEDI2CGRID(I) ) .AND.
     &         .NOT. CGRID_MASK_NUM( MAP_SEDI2CGRID(I) ) .AND.
     &         .NOT. CGRID_MASK_SRF( MAP_SEDI2CGRID(I) )  )
     &       CONC( I,: ) = CGRD( :,MAP_SEDI2CGRID(I) ) 
     &                    *FAC1( : ) / CGRID_MW( MAP_SEDI2CGRID(I) )

          ! Aerosol Number: N m-3 -> N mol-1
          ! Aerosol Surface Area: m2/m3 -> m2 / mol air
          IF ( CGRID_MASK_NUM( MAP_SEDI2CGRID(I) ) .OR.
     &         CGRID_MASK_SRF( MAP_SEDI2CGRID(I) )      )
     &       CONC( I,: ) = CGRD( :,MAP_SEDI2CGRID(I) ) * FAC1( : )

      END DO

      RETURN

      end subroutine conv_cgrd

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      subroutine conv_conc ( )
         use cgrid_spcs          ! cgrd mechanism species
         use grid_conf        ! horizontal & vertical domain specifications
         use vdiff_map


         implicit none

         integer   i, l, v            ! loop induction variables
         real, parameter :: mwair_si = mwair * 1.0e-3
         real fac1( nlays )

C-----------------------------------------------------------------------
      FAC1 = LDENS / MWAIR_SI  ! kg m-3 -> mol m-3

      DO I = 1,N_SEDI_MAP
          ! Aerosol Mass: ug/m3 -> ppmv
          IF ( CGRID_MASK_AERO( MAP_SEDI2CGRID(I) ) .AND.
     &         .NOT. CGRID_MASK_NUM( MAP_SEDI2CGRID(I) ) .AND.
     &         .NOT. CGRID_MASK_SRF( MAP_SEDI2CGRID(I) )  )
     &      CGRD( :,MAP_SEDI2CGRID(I) ) = CONC( I,: ) * FAC1( : ) 
     &                                  * CGRID_MW( MAP_SEDI2CGRID(I) )

          ! Aerosol Number: N m-3 -> N mol-1
          ! Aerosol Surface Area: m2/m3 -> m2 / mol air
          IF ( CGRID_MASK_NUM( MAP_SEDI2CGRID(I) ) .OR.
     &         CGRID_MASK_SRF( MAP_SEDI2CGRID(I) )      ) 
     &      CGRD( :,MAP_SEDI2CGRID(I) ) = CONC( I,: ) * FAC1( : )

      END DO
 
      RETURN

      end subroutine conv_conc

      end module sedimentation
