
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
      module vdiff_diag
C diagnostic output of acm2 parameters and sedimentation velocities
      use runtime_vars
      use utilio_defn
      use grid_conf
      use cgrid_spcs

      implicit none

      character( 16 ), save :: vdiff_diag_file = 'VDIFF_DIAG_FILE'

      integer, save :: n_vdiff
      integer, save :: n_vsed
      character( 16 ), allocatable, save :: vsed_name( : )
      integer, allocatable, save :: vsed_map( : )

      integer, save :: ntics = 0           ! no. of substeps within an output time step
C vdiff sub-timestep counter
      real, allocatable :: nlpcr_mean( :,: )   ! over the output time step TSTEP(1)
      real, allocatable :: nlpcr_max( :,: )    ! over the output time step TSTEP(1)
      real, allocatable :: nlpcr_min( :,: )    ! over the output time step TSTEP(1)
      real, allocatable :: nlpcr_sum( :,: )    ! accumulator
C sedi sub-timestep counter
      real, allocatable :: dtccr_mean( :,: )   ! over the output time step TSTEP(1)
      real, allocatable :: dtccr_max( :,: )    ! over the output time step TSTEP(1)
      real, allocatable :: dtccr_min( :,: )    ! over the output time step TSTEP(1)
      real, allocatable :: dtccr_sum( :,: )    ! accumulator
      real, allocatable :: cnvct( :,: )
      real, allocatable :: vsed_buf( :,:,:,: )

      contains

         function vdiff_diag_init( jdate, jtime, tstep1, grvsetl ) result ( success )

! Revision History.
!     Aug 12,2015 D. Wong: Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O implementation
!     Mar  2,2016 D. Wong: Modified the code to handle any combination of two environment
!                          variables: CTM_GRAV_SETL and VDIFF_DIAG_FILE, in a robust way for
!                          parallel I/O implementation

#ifndef mpas
#ifdef parallel
         use se_modules            ! stenex (using SE_UTIL_MODULE)
#else
         use noop_modules          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif

         implicit none
         include SUBST_FILES_ID  ! file name parameters
         logical success
         integer jdate, jtime, tstep1
         logical grvsetl
         character( 96 ) :: xmsg = ' '
         character( 16 ) :: pname = 'vdiff_diag_init'  
         logical ok, add, skip
         integer i, j, k, v, ios
         integer, allocatable :: found( : )

#ifndef mpas
         success = .true.

         if ( grvsetl ) then
            n_vdiff = 8   ! nlp's, sedi dtc's, convct, lpbl
            n_vsed = 6    ! aero sedv
            allocate( vsed_name( n_vsed ),
     &                vsed_map( n_vsed ),
     &                found( n_vsed ), stat = ios )
            if ( ios .ne. 0 ) then
               xmsg = "ERROR allocating nlp_'s or cnvct"
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
            vsed_name = ( / 'VNUMACC', 'VNUMCOR', 'VSRFACC',
     &                      'VSRFCOR', 'VMASSJ ', 'VMASSC ' / )
         else
            n_vdiff = 5
            n_vsed = 0
         end if

         if ( grvsetl ) then
C Create the vsed_map (find the 1st vsed_ae species for each of the 6 surrogate classes)
            do i = 1, n_vsed
               found( i ) = -9
            end do

            k = 0
            do v = 1, n_ae_depv

               skip = .true.
               do i = 1, n_vsed   ! search the entire list
                  if ( ae_depv( v ) .eq. vsed_name( i ) ) skip = .false.
               end do
               if ( skip ) cycle

               j = index1( ae_depv( v ), n_vsed, vsed_name )
               add = .true.
               do i = 1, n_vsed   ! search the entire list for j
                  if ( j .eq. found( i ) ) then
                     add = .false.
                  end if
               end do
               if ( add ) then    ! this j is not in the list; add it
                  k = k + 1
                  found( k ) = j
                  vsed_map( k ) = v
               end if

            end do
    
         end if

         if ( io_pe_inclusive ) then

C get CONC file header description
            ok = open3( ctm_conc_1, fsread3, pname )
            ok = desc3( ctm_conc_1 )
            if ( .not. ok ) then
               xmsg = 'could not read '// trim( ctm_conc_1 )
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if

            sdate3d = jdate
            stime3d = jtime
            call nextime ( sdate3d, stime3d, tstep1 ) 

            if ( grvsetl ) then

               nvars3d = n_vsed
               fdesc3d = ' '   ! array
               fdesc3d( 1 ) = 'representative coarse aerosol gravitational settling velocities'
               do v = 1, n_vsed
                  vtype3d( v ) = m3real
                  units3d( v ) = 'm s-1'
                  vname3d( v ) = vsed_name( v )
                  vdesc3d( v ) = 'gravitational settling velocity'
               end do

               ok = open3( ctm_vsed_diag, fsnew3, pname )
               if ( .not. ok ) then
                  xmsg = 'could not create '// trim( ctm_vsed_diag ) // ' file'
                  call m3warn( 'vdiff_diag', jdate, jtime, xmsg )
                  success = .false.; return
               end if

            end if

            nvars3d = n_vdiff
            nlays3d = 1
            fdesc3d = ' '   ! array
            fdesc3d( 1 ) = 'vdiff diagnostic variables'
            v = 1
            vtype3d( v ) = m3real
            units3d( v ) = ''
            vname3d( v ) = 'NLP_MEAN'
            vdesc3d( v ) = 'mean sub-timestep iteration count'
            v = v + 1
            vtype3d( v ) = m3real
            units3d( v ) = ''
            vname3d( v ) = 'NLP_MAX'
            vdesc3d( v ) = 'max sub-timestep iteration count'
            v = v + 1
            vtype3d( v ) = m3real
            units3d( v ) = ''
            vname3d( v ) = 'NLP_MIN'
            vdesc3d( v ) = 'min sub-timestep iteration count'

            if ( grvsetl ) then

               v = v + 1
               vtype3d( v ) = m3real
               units3d( v ) = ''
               vname3d( v ) = 'SEDI_DTC_MEAN'
               vdesc3d( v ) = 'mean grav. settling velocity sub-timestep iteration count'
               v = v + 1
               vtype3d( v ) = m3real
               units3d( v ) = ''
               vname3d( v ) = 'SEDI_DTC_MAX'
               vdesc3d( v ) = 'max grav. settling velocity sub-timestep iteration count'
               v = v + 1
               vtype3d( v ) = m3real
               units3d( v ) = ''
               vname3d( v ) = 'SEDI_DTC_MIN'
               vdesc3d( v ) = 'min grav. settling velocity sub-timestep iteration count'
            end if

            v = v + 1
            vtype3d( v ) = m3real
            units3d( v ) = ''
            vname3d( v ) = 'CONVCT'
            vdesc3d( v ) = 'Convective cell (for ACM2)'
            v = v + 1
            vtype3d( v ) = m3real
            units3d( v ) = ''
            vname3d( v ) = 'LPBL'
            vdesc3d( v ) = 'PBL top sigma layer'

            ok = open3( ctm_vdiff_diag, fsnew3, pname )
            if ( .not. ok ) then
               xmsg = 'could not create '// trim( ctm_vdiff_diag ) // ' file'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if

         end if  ! io_pe_inclusive

         allocate ( nlpcr_max( ncols,nrows ),
     &              nlpcr_min( ncols,nrows ),
     &              nlpcr_mean( ncols,nrows ),
     &              nlpcr_sum( ncols,nrows ),
     &              cnvct( ncols,nrows ), stat = ios )
         if ( ios .ne. 0 ) then
            xmsg = "ERROR allocating nlp_'s or cnvct"
            call m3warn( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if
         nlpcr_max = 0.0      ! array assignment
         nlpcr_min = 9.9E30   ! array assignment
         nlpcr_sum = 0.0      ! array assignment

         if ( grvsetl ) then

            allocate ( vsed_buf( ncols,nrows,nlays,n_vsed ), stat = ios )
            if ( ios .ne. 0 ) then
               xmsg = 'ERROR allocating vsed_buf'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if

            allocate ( dtccr_max( ncols,nrows ), 
     &                 dtccr_min( ncols,nrows ), 
     &                 dtccr_mean( ncols,nrows ),
     &                 dtccr_sum( ncols,nrows ), stat = ios )
            if ( ios .ne. 0 ) then
               xmsg = "ERROR allocating , sedi dtc's"
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
            dtccr_max = 0.0      ! array assignment
            dtccr_min = 9.9E30   ! array assignment
            dtccr_sum = 0.0      ! array assignment

         end if

#ifdef parallel_io
         call se_barrier
         if ( .not. io_pe_inclusive ) then
            if ( grvsetl ) then
               if ( .not. open3 ( ctm_vsed_diag, fsread3, pname ) ) then
                  xmsg = 'Could not open ' // trim( ctm_vsed_diag )
                  call m3exit ( pname, jdate, jtime, xmsg, xstat2 )
               end if
            end if
            if ( .not. open3 ( ctm_vdiff_diag, fsread3, pname ) ) then
               xmsg = 'Could not open ' // trim( ctm_vdiff_diag )
               call m3exit ( pname, jdate, jtime, xmsg, xstat2 )
            end if
         end if
#endif
#endif

         end function vdiff_diag_init

      end module vdiff_diag

