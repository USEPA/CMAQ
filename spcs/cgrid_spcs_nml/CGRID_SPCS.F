
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
C $Header: /project/yoj/arc/CCTM/src/spcs/cgrid_spcs_nml/CGRID_SPCS.F,v 1.8 2012/03/19 15:44:24 yoj Exp $

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      module cgrid_spcs

c CGRID gas chem, aerosol, non-reactive, and tracer species definitions
c based on namelist specifications
c Revision History:
c Jeff Young 31 Aug 09: created
c Bill Hutzell 8 Oct 10: added Gas Chemistry consistency check
C Shawn Roselle 16 Feb 11: replaced I/O API include files with UTILIO_DEFN
C 07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C 21 Apr 16 D.Luecken:  increased spc_dim to 1000
c-----------------------------------------------------------------------

      implicit none

c main CGRID table
      integer,                      save :: n_gc_spc = 0, n_ae_spc = 0, n_nr_spc = 0, n_tr_spc = 0
      character( 16 ), allocatable, save :: gc_spc( : ),  ae_spc( : ),  nr_spc( : ),  tr_spc( : )
      real, allocatable,            save :: gc_molwt( : ), ae_molwt( : ), nr_molwt( : ), tr_molwt( : )

      integer,                      save :: n_gc_spcd = 0

c Starting and ending index of gas chemistry species in CGRID
      integer,                      save :: gc_strt = 1
      integer,                      save :: gc_fini = 0

c Starting and ending index of aerosol species in CGRID
      integer,                      save :: ae_strt = 0
      integer,                      save :: ae_fini = 0

c Starting and ending index of non-reactive species in CGRID
      integer,                      save :: nr_strt = 0
      integer,                      save :: nr_fini = 0

c Starting and ending index of tracer species in CGRID
      integer,                      save :: tr_strt = 0
      integer,                      save :: tr_fini = 0

c number of species in CGRID
      integer,                      save :: nspcsd = 1

c surrogate species
      integer,                      save :: n_gc_emis = 0, n_ae_emis = 0, n_nr_emis = 0, n_tr_emis = 0,
     &                                      n_gc_icbc = 0, n_ae_icbc = 0, n_nr_icbc = 0, n_tr_icbc = 0,
     &                                      n_gc_depv = 0, n_ae_depv = 0, n_nr_depv = 0, n_tr_depv = 0,
     &                                      n_gc_scav = 0, n_ae_scav = 0, n_nr_scav = 0, n_tr_scav = 0,
     &                                      n_gc_g2ae = 0,                n_nr_n2ae = 0, n_tr_t2ae = 0,
     &                                      n_gc_g2aq = 0, n_ae_a2aq = 0, n_nr_n2aq = 0, n_tr_t2aq = 0
      character( 16 ), allocatable, save :: gc_emis( : ), ae_emis( : ), nr_emis( : ), tr_emis( : ),
     &                                      gc_icbc( : ), ae_icbc( : ), nr_icbc( : ), tr_icbc( : ),
     &                                      gc_depv( : ), ae_depv( : ), nr_depv( : ), tr_depv( : ),
     &                                      gc_scav( : ), ae_scav( : ), nr_scav( : ), tr_scav( : ),
     &                                      gc_g2ae( : ),               nr_n2ae( : ), tr_t2ae( : ),
     &                                      gc_g2aq( : ), ae_a2aq( : ), nr_n2aq( : ), tr_t2aq( : )
      integer, allocatable,         save :: gc_emis_map( : ), ae_emis_map( : ), nr_emis_map( : ), tr_emis_map( : ),
     &                                      gc_icbc_map( : ), ae_icbc_map( : ), nr_icbc_map( : ), tr_icbc_map( : ),
     &                                      gc_depv_map( : ), ae_depv_map( : ), nr_depv_map( : ), tr_depv_map( : ),
     &                                      gc_scav_map( : ), ae_scav_map( : ), nr_scav_map( : ), tr_scav_map( : ),
     &                                      gc_g2ae_map( : ),                   nr_n2ae_map( : ), tr_t2ae_map( : ),
     &                                      gc_g2aq_map( : ), ae_a2aq_map( : ), nr_n2aq_map( : ), tr_t2aq_map( : )
      real, allocatable,            save :: gc_emis_fac( : ), ae_emis_fac( : ), nr_emis_fac( : ), tr_emis_fac( : ),
     &                                      gc_icbc_fac( : ), ae_icbc_fac( : ), nr_icbc_fac( : ), tr_icbc_fac( : ),
     &                                      gc_depv_fac( : ), ae_depv_fac( : ), nr_depv_fac( : ), tr_depv_fac( : ),
     &                                      gc_scav_fac( : ), ae_scav_fac( : ), nr_scav_fac( : ), tr_scav_fac( : )

c control species
      integer,                      save :: n_gc_trns = 0, n_ae_trns = 0, n_nr_trns = 0, n_tr_adv = 0, n_tr_diff = 0,
     &                                      n_gc_ddep = 0, n_ae_ddep = 0, n_nr_ddep = 0, n_tr_ddep = 0,
     &                                      n_gc_wdep = 0, n_ae_wdep = 0, n_nr_wdep = 0, n_tr_wdep = 0,
     &                                      n_gc_conc = 0, n_ae_conc = 0, n_nr_conc = 0, n_tr_conc = 0
      character( 16 ), allocatable, save :: gc_trns( : ), ae_trns( : ), nr_trns( : ), tr_adv( : ), tr_diff( : ),
     &                                      gc_ddep( : ), ae_ddep( : ), nr_ddep( : ), tr_ddep( : ),
     &                                      gc_wdep( : ), ae_wdep( : ), nr_wdep( : ), tr_wdep( : ),
     &                                      gc_conc( : ), ae_conc( : ), nr_conc( : ), tr_conc( : )
      integer, allocatable,         save :: gc_trns_map( : ), ae_trns_map( : ), nr_trns_map( : ), tr_adv_map( : ), tr_diff_map( : ),
     &                                      gc_ddep_map( : ), ae_ddep_map( : ), nr_ddep_map( : ), tr_ddep_map( : ),
     &                                      gc_wdep_map( : ), ae_wdep_map( : ), nr_wdep_map( : ), tr_wdep_map( : ),
     &                                      gc_conc_map( : ), ae_conc_map( : ), nr_conc_map( : ), tr_conc_map( : )

      type, public :: base_type
         private
         character( 16 ) :: name
         real            :: mw
      end type
      type, public :: surr1_type
         private
         character( 16 ) :: name
         integer         :: map
         real            :: fac
      end type
      type, public :: surr2_type
         private
         character( 16 ) :: name
         integer         :: map
      end type
      type, public :: ctrl_type
         private
         character( 16 ) :: name
         integer         :: map
      end type

      contains

c init routine to allocate the above allocatables
c !! NO   and to populate the allocated arrays from the stnd gc_spc.ext  !!! NO

         function cgrid_spcs_init() result ( success )
            use UTILIO_DEFN
     
            implicit none


            integer, save :: logdev
            
            logical success
!           integer, parameter :: dev_gc_nml = 15, dev_ae_nml = 16, dev_nr_nml = 17, dev_tr_nml = 18
            integer, save :: dev_gc_nml, dev_ae_nml, dev_nr_nml, dev_tr_nml
!           logical gc_matrix_err, ae_matrix_err, nr_matrix_err, tr_matrix_err
            integer i, ios
            integer i1, i2, i3, i4      ! surrogate type 1 counters
            integer j1, j2              ! surrogate type 2 counters
            integer k1, k2, k3, k4, k5  ! control type counters
            integer icall
            character(  1 ), parameter :: bl = ' '
            integer, parameter :: spc_dim = 1000
            logical :: order = .true., found = .true.
            character( 120 ) :: xmsg

c namelist variables

            integer n_surr1, n_surr2, n_ctrl
            character( 256 ) :: type_header, type_matrix( spc_dim )
            namelist / GC_nml / n_surr1, n_surr2, n_ctrl, type_header, type_matrix
            namelist / AE_nml / n_surr1, n_surr2, n_ctrl, type_header, type_matrix
            namelist / NR_nml / n_surr1, n_surr2, n_ctrl, type_header, type_matrix
            namelist / TR_nml / n_surr1, n_surr2, n_ctrl, type_header, type_matrix

            character( 16 ) :: gc_matrix = 'gc_matrix_nml'
            character( 16 ) :: ae_matrix = 'ae_matrix_nml'
            character( 16 ) :: nr_matrix = 'nr_matrix_nml'
            character( 16 ) :: tr_matrix = 'tr_matrix_nml'
            character( 256 ) :: eqname

c intialize the surr1 table
            character( 4 ) :: surr1( 4 )
            integer :: surr1_ndx( 4 )
            integer :: emis_ndx, icbc_ndx, depv_ndx, scav_ndx
c intialize the surr2 table
            character( 4 ) :: gc_surr2( 2 )
            integer :: gc_surr2_ndx( 2 )
            integer :: g2ae_ndx, g2aq_ndx
            character( 4 ) :: ae_surr2( 1 ) = (/ 'A2AQ' /)
            integer :: ae_surr2_ndx( 1 ) = (/ 1 /)
            integer :: a2aq_ndx
            character( 4 ) :: nr_surr2( 2 )
            integer :: nr_surr2_ndx( 2 )
            integer :: n2ae_ndx, n2aq_ndx
            character( 4 ) :: tr_surr2( 2 )
            integer :: tr_surr2_ndx( 2 )
            integer :: t2ae_ndx, t2aq_ndx
c intialize the ctrl table
            character( 4 ) :: ctrl( 5 )
            integer :: ctrl_ndx( 5 )
            integer :: trns_ndx, ddep_ndx, wdep_ndx, conc_ndx
            integer :: adv_ndx, diff_ndx   ! for tr species

c species table structures

            type ( base_type )               :: mc_base
            type ( surr1_type ), allocatable :: mc_surr1( : )
            type ( surr2_type ), allocatable :: mc_surr2( : )
            type ( ctrl_type ),  allocatable :: mc_ctrl ( : )

            type ( base_type )  :: mc_base_init  = base_type ( ' ', 0.0 )
            type ( surr1_type ) :: mc_surr1_init = surr1_type( ' ', 0, 0.0 )
            type ( surr2_type ) :: mc_surr2_init = surr2_type( ' ', 0 )
            type ( ctrl_type )  :: mc_ctrl_init  = ctrl_type ( ' ', 0 )

c----------------------------------------------------------------------------------------
            logdev = init3 ()
            success = .true.

c intialize module public data

c from include file
!!! NADA

c from namelist files

            type_header = bl
            type_matrix = bl   ! array assignment

            call nameval( gc_matrix, eqname )

            write( logdev,* ) ' '
            write( logdev,* ) '    GC Species Namelist: ', trim( eqname )

!           open( file = 'gc_matrix.nml', unit = dev_gc_nml,
            dev_gc_nml = junit()
            open( file = trim( eqname ), unit = dev_gc_nml,
     &            status = 'old', position = 'rewind' )
            read( nml = GC_nml, unit = dev_gc_nml ) 

c create the surr1 table from the namelist header
            surr1( 1 ) = 'EMIS'; surr1_ndx( 1 ) = index( type_header, trim( surr1( 1 ) ) )
            surr1( 2 ) = 'ICBC'; surr1_ndx( 2 ) = index( type_header, trim( surr1( 2 ) ) )
            surr1( 3 ) = 'DEPV'; surr1_ndx( 3 ) = index( type_header, trim( surr1( 3 ) ) )
            surr1( 4 ) = 'SCAV'; surr1_ndx( 4 ) = index( type_header, trim( surr1( 4 ) ) )
c establish surr1 order in the namelist header (surr1 possibly rearranged)
            call bubble( 4, surr1_ndx, surr1 )   ! short sort on surr1_ndx
            emis_ndx = index1a( 'EMIS', 4, surr1 )
            icbc_ndx = index1a( 'ICBC', 4, surr1 )
            depv_ndx = index1a( 'DEPV', 4, surr1 )
            scav_ndx = index1a( 'SCAV', 4, surr1 )

c create the surr2 table from the namelist header
            gc_surr2( 1 ) = 'G2AE'; gc_surr2_ndx( 1 ) = index( type_header, trim( gc_surr2( 1 ) ) )
            gc_surr2( 2 ) = 'G2AQ'; gc_surr2_ndx( 2 ) = index( type_header, trim( gc_surr2( 2 ) ) )
c establish surr2 order in the namelist header (surr2 possibly rearranged)
            call bubble( 2, gc_surr2_ndx, gc_surr2 )   ! short sort on surr2_ndx
            g2ae_ndx = index1a( 'G2AE', 2, gc_surr2 )
            g2aq_ndx = index1a( 'G2AQ', 2, gc_surr2 )

c create the ctrl table from the namelist header
            ctrl( 1 ) = 'TRNS'; ctrl_ndx( 1 ) = index( type_header, trim( ctrl( 1 ) ) )
            ctrl( 2 ) = 'DDEP'; ctrl_ndx( 2 ) = index( type_header, trim( ctrl( 2 ) ) )
            ctrl( 3 ) = 'WDEP'; ctrl_ndx( 3 ) = index( type_header, trim( ctrl( 3 ) ) )
            ctrl( 4 ) = 'CONC'; ctrl_ndx( 4 ) = index( type_header, trim( ctrl( 4 ) ) )
c establish ctrl order in the namelist header (ctrl possibly rearranged)
            call bubble( 4, ctrl_ndx, ctrl )   ! short sort on ctrl_ndx
            trns_ndx = index1a( 'TRNS', 4, ctrl )
            ddep_ndx = index1a( 'DDEP', 4, ctrl )
            wdep_ndx = index1a( 'WDEP', 4, ctrl )
            conc_ndx = index1a( 'CONC', 4, ctrl )

c the surrogate and ctrl arrays active dimensions not known a priori
            allocate ( 
     &      gc_spc( spc_dim ), gc_molwt( spc_dim ),
     &      gc_emis( spc_dim ), gc_emis_map( spc_dim ), gc_emis_fac( spc_dim ),
     &      gc_icbc( spc_dim ), gc_icbc_map( spc_dim ), gc_icbc_fac( spc_dim ),
     &      gc_depv( spc_dim ), gc_depv_map( spc_dim ), gc_depv_fac( spc_dim ),
     &      gc_scav( spc_dim ), gc_scav_map( spc_dim ), gc_scav_fac( spc_dim ),
     &      gc_g2ae( spc_dim ), gc_g2ae_map( spc_dim ),
     &      gc_g2aq( spc_dim ), gc_g2aq_map( spc_dim ),
     &      gc_trns( spc_dim ), gc_trns_map( spc_dim ),
     &      gc_ddep( spc_dim ), gc_ddep_map( spc_dim ),
     &      gc_wdep( spc_dim ), gc_wdep_map( spc_dim ),
     &      gc_conc( spc_dim ), gc_conc_map( spc_dim ),
     &      stat = ios )

            gc_spc = bl; gc_molwt = 0.0                        !  .
            gc_emis = bl; gc_emis_map = 0; gc_emis_fac = 0.0   !  .
            gc_icbc = bl; gc_icbc_map = 0; gc_icbc_fac = 0.0   !  .
            gc_depv = bl; gc_depv_map = 0; gc_depv_fac = 0.0   !  .
            gc_scav = bl; gc_scav_map = 0; gc_scav_fac = 0.0   ! array
            gc_g2ae = bl; gc_g2ae_map = 0                      !  .
            gc_g2aq = bl; gc_g2aq_map = 0                      ! assignments
            gc_trns = bl; gc_trns_map = 0                      !  .
            gc_ddep = bl; gc_ddep_map = 0                      !  .
            gc_wdep = bl; gc_wdep_map = 0                      !  .
            gc_conc = bl; gc_conc_map = 0                      !  .

            allocate ( mc_surr1( n_surr1 ),
     &                 mc_surr2( n_surr2 ),
     &                 mc_ctrl( n_ctrl ), stat = ios )

            mc_base  = mc_base_init    ! structure assignment
            mc_surr1 = mc_surr1_init   ! array structure assignment
            mc_surr2 = mc_surr2_init   ! array structure assignment
            mc_ctrl  = mc_ctrl_init    ! array structure assignment

            i1 = 0; i2 = 0; i3 = 0; i4 = 0
            j1 = 0; j2 = 0
            k1 = 0; k2 = 0; k3 = 0; k4 = 0
            icall = 0
#ifdef Verbose
            write( logdev,'(5x,a)' ) '@@@ GC @@@'
#endif
            do i = 1, spc_dim
               if ( type_matrix( i ) .eq. bl ) then
                  n_gc_spc = i - 1; exit
               end if
               call get_list( icall, type_matrix( i ), mc_base,
     &                        n_surr1, mc_surr1, n_surr2, mc_surr2,
     &                        n_ctrl, mc_ctrl )
               gc_spc( i ) = mc_base%name
               gc_molwt( i ) = mc_base%mw

               if ( emis_ndx .gt. 0 ) then
                  if ( mc_surr1( emis_ndx )%name .ne. bl ) then
                     i1 = i1 + 1
                     gc_emis( i1 )     = mc_surr1( emis_ndx )%name
                     gc_emis_map( i1 ) = mc_surr1( emis_ndx )%map
                     gc_emis_fac( i1 ) = mc_surr1( emis_ndx )%fac
                  end if
               end if
               if ( icbc_ndx .gt. 0 ) then
                  if ( mc_surr1( icbc_ndx )%name .ne. bl ) then
                     i2 = i2 + 1
                     gc_icbc( i2 )     = mc_surr1( icbc_ndx )%name
                     gc_icbc_map( i2 ) = mc_surr1( icbc_ndx )%map
                     gc_icbc_fac( i2 ) = mc_surr1( icbc_ndx )%fac
                  end if
               end if
               if ( depv_ndx .gt. 0 ) then
                  if ( mc_surr1( depv_ndx )%name .ne. bl ) then
                     i3 = i3 + 1
                     gc_depv( i3 )     = mc_surr1( depv_ndx )%name
                     gc_depv_map( i3 ) = mc_surr1( depv_ndx )%map
                     gc_depv_fac( i3 ) = mc_surr1( depv_ndx )%fac
                  end if
               end if
               if ( scav_ndx .gt. 0 ) then
                  if ( mc_surr1( scav_ndx )%name .ne. bl ) then
                     i4 = i4 + 1
                     gc_scav( i4 )     = mc_surr1( scav_ndx )%name
                     gc_scav_map( i4 ) = mc_surr1( scav_ndx )%map
                     gc_scav_fac( i4 ) = mc_surr1( scav_ndx )%fac
                  end if
               end if

               if ( g2ae_ndx .ne. 0 ) then
                  if ( mc_surr2( g2ae_ndx )%name .ne. bl ) then
                     j1 = j1 + 1
                     gc_g2ae( j1 )     = mc_surr2( g2ae_ndx )%name
                     gc_g2ae_map( j1 ) = mc_surr2( g2ae_ndx )%map
                  end if
              end if
               if ( g2aq_ndx .ne. 0 ) then
                  if ( mc_surr2( g2aq_ndx )%name .ne. bl ) then
                     j2 = j2 + 1
                     gc_g2aq( j2 )     = mc_surr2( g2aq_ndx )%name
                     gc_g2aq_map( j2 ) = mc_surr2( g2aq_ndx )%map
                  end if
               end if

               if ( trns_ndx .ne. 0 ) then
                  if ( mc_ctrl( trns_ndx )%name .ne. bl ) then
                     k1 = k1 + 1
                     gc_trns( k1 )     = mc_ctrl( trns_ndx )%name
                     gc_trns_map( k1 ) = mc_ctrl( trns_ndx )%map
                  end if
               end if
               if ( ddep_ndx .ne. 0 ) then
                  if ( mc_ctrl( ddep_ndx )%name .ne. bl ) then
                     k2 = k2 + 1
                     gc_ddep( k2 )     = mc_ctrl( ddep_ndx )%name
                     gc_ddep_map( k2 ) = mc_ctrl( ddep_ndx )%map
                  end if
               end if
               if ( wdep_ndx .ne. 0 ) then
                  if ( mc_ctrl( wdep_ndx )%name .ne. bl ) then
                     k3 = k3 + 1
                     gc_wdep( k3 )     = mc_ctrl( wdep_ndx )%name
                     gc_wdep_map( k3 ) = mc_ctrl( wdep_ndx )%map
                  end if
               end if
               if ( conc_ndx .ne. 0 ) then
                  if ( mc_ctrl( conc_ndx )%name .ne. bl ) then
                     k4 = k4 + 1
                     gc_conc( k4 )     = mc_ctrl( conc_ndx )%name
                     gc_conc_map( k4 ) = mc_ctrl( conc_ndx )%map
                  end if
               end if
            end do

            n_gc_emis = i1
            n_gc_icbc = i2
            n_gc_depv = i3
            n_gc_scav = i4
            n_gc_g2ae = j1
            n_gc_g2aq = j2
            n_gc_trns = k1
            n_gc_ddep = k2
            n_gc_wdep = k3
            n_gc_conc = k4

                 
            gc_fini = gc_strt + n_gc_spc - 1
            n_gc_spcd = gc_fini + 1

            deallocate ( mc_surr1, mc_surr2, mc_ctrl, stat = ios )

!           if ( gc_matrix_err ) then
!              success = .false.; return
!           end if

            type_header = bl
            type_matrix = bl   ! array assignment

            call nameval( ae_matrix, eqname )

            write( logdev,* ) ' '
            write( logdev,* ) '    AE Species Namelist: ', trim( eqname )

!           open( file = 'ae_matrix.nml', unit = dev_ae_nml,
            dev_ae_nml = junit()
            open( file = trim( eqname ), unit = dev_ae_nml,
     &            status = 'old', position = 'rewind' )
            read( nml = AE_nml, unit = dev_ae_nml ) 

c create the surr1 table from the namelist header
            surr1( 1 ) = 'EMIS'; surr1_ndx( 1 ) = index( type_header, trim( surr1( 1 ) ) )
            surr1( 2 ) = 'ICBC'; surr1_ndx( 2 ) = index( type_header, trim( surr1( 2 ) ) )
            surr1( 3 ) = 'DEPV'; surr1_ndx( 3 ) = index( type_header, trim( surr1( 3 ) ) )
            surr1( 4 ) = 'SCAV'; surr1_ndx( 4 ) = index( type_header, trim( surr1( 4 ) ) )
c establish surr1 order in the namelist header (surr1 possibly rearranged)
            call bubble( 4, surr1_ndx, surr1 )   ! short sort on surr1_ndx
            emis_ndx = index1a( 'EMIS', 4, surr1 )
            icbc_ndx = index1a( 'ICBC', 4, surr1 )
            depv_ndx = index1a( 'DEPV', 4, surr1 )
            scav_ndx = index1a( 'SCAV', 4, surr1 )

c create the surr2 table from the namelist header
            ae_surr2_ndx( 1 ) = index( type_header, 'A2AQ' )
c establish surr2 order in the namelist header
            a2aq_ndx = 1

c create the ctrl table from the namelist header
            ctrl( 1 ) = 'TRNS'; ctrl_ndx( 1 ) = index( type_header, trim( ctrl( 1 ) ) )
            ctrl( 2 ) = 'DDEP'; ctrl_ndx( 2 ) = index( type_header, trim( ctrl( 2 ) ) )
            ctrl( 3 ) = 'WDEP'; ctrl_ndx( 3 ) = index( type_header, trim( ctrl( 3 ) ) )
            ctrl( 4 ) = 'CONC'; ctrl_ndx( 4 ) = index( type_header, trim( ctrl( 4 ) ) )
c establish ctrl order in the namelist header (ctrl possibly rearranged)
            call bubble( 4, ctrl_ndx, ctrl )   ! short sort on ctrl_ndx
            trns_ndx = index1a( 'TRNS', 4, ctrl )
            ddep_ndx = index1a( 'DDEP', 4, ctrl )
            wdep_ndx = index1a( 'WDEP', 4, ctrl )
            conc_ndx = index1a( 'CONC', 4, ctrl )

c the surrogate and ctrl arrays active dimensions not known a priori
            allocate ( 
     &      ae_spc( spc_dim ), ae_molwt( spc_dim ),
     &      ae_emis( spc_dim ), ae_emis_map( spc_dim ), ae_emis_fac( spc_dim ),
     &      ae_icbc( spc_dim ), ae_icbc_map( spc_dim ), ae_icbc_fac( spc_dim ),
     &      ae_depv( spc_dim ), ae_depv_map( spc_dim ), ae_depv_fac( spc_dim ),
     &      ae_scav( spc_dim ), ae_scav_map( spc_dim ), ae_scav_fac( spc_dim ),
     &      ae_a2aq( spc_dim ), ae_a2aq_map( spc_dim ),
     &      ae_trns( spc_dim ), ae_trns_map( spc_dim ),
     &      ae_ddep( spc_dim ), ae_ddep_map( spc_dim ),
     &      ae_wdep( spc_dim ), ae_wdep_map( spc_dim ),
     &      ae_conc( spc_dim ), ae_conc_map( spc_dim ),
     &      stat = ios )

            ae_spc = bl; ae_molwt = 0.0                        !  .
            ae_emis = bl; ae_emis_map = 0; ae_emis_fac = 0.0   !  .
            ae_icbc = bl; ae_icbc_map = 0; ae_icbc_fac = 0.0   !  .
            ae_depv = bl; ae_depv_map = 0; ae_depv_fac = 0.0   !  .
            ae_scav = bl; ae_scav_map = 0; ae_scav_fac = 0.0   ! array
            ae_a2aq = bl; ae_a2aq_map = 0                      !  .
            ae_trns = bl; ae_trns_map = 0                      ! assignments
            ae_ddep = bl; ae_ddep_map = 0                      !  .
            ae_wdep = bl; ae_wdep_map = 0                      !  .
            ae_conc = bl; ae_conc_map = 0                      !  .

            allocate ( mc_surr1( n_surr1 ),
     &                 mc_surr2( n_surr2 ),
     &                 mc_ctrl( n_ctrl ), stat = ios )

            mc_base  = mc_base_init    ! structure assignment
            mc_surr1 = mc_surr1_init   ! array structure assignment
            mc_surr2 = mc_surr2_init   ! array structure assignment
            mc_ctrl  = mc_ctrl_init    ! array structure assignment

            i1 = 0; i2 = 0; i3 = 0; i4 = 0
            j1 = 0; j2 = 0
            k1 = 0; k2 = 0; k3 = 0; k4 = 0
            icall = 0
#ifdef Verbose
            write( logdev,'(5x,a)' ) '@@@ AE @@@'
#endif
            do i = 1, spc_dim
               if ( type_matrix( i ) .eq. bl ) then
                  n_ae_spc = i - 1
                  exit
               end if
               call get_list( icall, type_matrix( i ), mc_base,
     &                        n_surr1, mc_surr1, n_surr2, mc_surr2,
     &                        n_ctrl, mc_ctrl )
               ae_spc( i ) = mc_base%name
               ae_molwt( i ) = mc_base%mw

               if ( emis_ndx .ne. 0 ) then
                  if ( mc_surr1( emis_ndx )%name .ne. bl ) then
                     i1 = i1 + 1
                     ae_emis( i1 )     = mc_surr1( emis_ndx )%name
                     ae_emis_map( i1 ) = mc_surr1( emis_ndx )%map
                     ae_emis_fac( i1 ) = mc_surr1( emis_ndx )%fac
                  end if
               end if
               if ( icbc_ndx .ne. 0 ) then
                  if ( mc_surr1( icbc_ndx )%name .ne. bl ) then
                     i2 = i2 + 1
                     ae_icbc( i2 )     = mc_surr1( icbc_ndx )%name
                     ae_icbc_map( i2 ) = mc_surr1( icbc_ndx )%map
                     ae_icbc_fac( i2 ) = mc_surr1( icbc_ndx )%fac
                  end if
               end if
               if ( depv_ndx .ne. 0 ) then
                  if ( mc_surr1( depv_ndx )%name .ne. bl ) then
                     i3 = i3 + 1
                     ae_depv( i3 )     = mc_surr1( depv_ndx )%name
                     ae_depv_map( i3 ) = mc_surr1( depv_ndx )%map
                     ae_depv_fac( i3 ) = mc_surr1( depv_ndx )%fac
                  end if
               end if
               if ( scav_ndx .ne. 0 ) then
                  if ( mc_surr1( scav_ndx )%name .ne. bl ) then
                     i4 = i4 + 1
                     ae_scav( i4 )     = mc_surr1( scav_ndx )%name
                     ae_scav_map( i4 ) = mc_surr1( scav_ndx )%map
                     ae_scav_fac( i4 ) = mc_surr1( scav_ndx )%fac
                  end if
               end if

               if ( a2aq_ndx .ne. 0 ) then
                  if ( mc_surr2( a2aq_ndx )%name .ne. bl ) then
                     j2 = j2 + 1
                     ae_a2aq( j2 )     = mc_surr2( a2aq_ndx )%name
                     ae_a2aq_map( j2 ) = mc_surr2( a2aq_ndx )%map
                  end if
               end if

               if ( trns_ndx .ne. 0 ) then
                  if ( mc_ctrl( trns_ndx )%name .ne. bl ) then
                     k1 = k1 + 1
                     ae_trns( k1 )     = mc_ctrl( trns_ndx )%name
                     ae_trns_map( k1 ) = mc_ctrl( trns_ndx )%map
                  end if
               end if
               if ( ddep_ndx .ne. 0 ) then
                  if ( mc_ctrl( ddep_ndx )%name .ne. bl ) then
                     k2 = k2 + 1
                     ae_ddep( k2 )     = mc_ctrl( ddep_ndx )%name
                     ae_ddep_map( k2 ) = mc_ctrl( ddep_ndx )%map
                  end if
               end if
               if ( wdep_ndx .ne. 0 ) then
                  if ( mc_ctrl( wdep_ndx )%name .ne. bl ) then
                     k3 = k3 + 1
                     ae_wdep( k3 )     = mc_ctrl( wdep_ndx )%name
                     ae_wdep_map( k3 ) = mc_ctrl( wdep_ndx )%map
                  end if
               end if
               if ( conc_ndx .ne. 0 ) then
                  if ( mc_ctrl( conc_ndx )%name .ne. bl ) then
                     k4 = k4 + 1
                     ae_conc( k4 )     = mc_ctrl( conc_ndx )%name
                     ae_conc_map( k4 ) = mc_ctrl( conc_ndx )%map
                  end if
               end if
            end do

            n_ae_emis = i1
            n_ae_icbc = i2
            n_ae_depv = i3
            n_ae_scav = i4
            n_ae_a2aq = j2
            n_ae_trns = k1
            n_ae_ddep = k2
            n_ae_wdep = k3
            n_ae_conc = k4

            ae_strt = gc_fini + 2
            ae_fini = ae_strt + n_ae_spc - 1

            deallocate ( mc_surr1, mc_surr2, mc_ctrl, stat = ios )

!           if ( ae_matrix_err ) then
!              success = .false.; return
!           end if

            type_header = bl
            type_matrix = bl   ! array assignment

            call nameval( nr_matrix, eqname )

            write( logdev,* ) ' '
            write( logdev,* ) '    NR Species Namelist: ', trim( eqname )

!           open( file = 'nr_matrix.nml', unit = dev_nr_nml,
            dev_nr_nml = junit()
            open( file = trim( eqname ), unit = dev_nr_nml,
     &            status = 'old', position = 'rewind' )
            read( nml = NR_nml, unit = dev_nr_nml ) 

c create the surr1 table from the namelist header
            surr1( 1 ) = 'EMIS'; surr1_ndx( 1 ) = index( type_header, trim( surr1( 1 ) ) )
            surr1( 2 ) = 'ICBC'; surr1_ndx( 2 ) = index( type_header, trim( surr1( 2 ) ) )
            surr1( 3 ) = 'DEPV'; surr1_ndx( 3 ) = index( type_header, trim( surr1( 3 ) ) )
            surr1( 4 ) = 'SCAV'; surr1_ndx( 4 ) = index( type_header, trim( surr1( 4 ) ) )
c establish surr1 order in the namelist header (surr1 possibly rearranged)
            call bubble( 4, surr1_ndx, surr1 )   ! short sort on surr1_ndx
            emis_ndx = index1a( 'EMIS', 4, surr1 )
            icbc_ndx = index1a( 'ICBC', 4, surr1 )
            depv_ndx = index1a( 'DEPV', 4, surr1 )
            scav_ndx = index1a( 'SCAV', 4, surr1 )

c create the surr2 table from the namelist header
            nr_surr2( 1 ) = 'N2AE'; nr_surr2_ndx( 1 ) = index( type_header, trim( nr_surr2( 1 ) ) )
            nr_surr2( 2 ) = 'N2AQ'; nr_surr2_ndx( 2 ) = index( type_header, trim( nr_surr2( 2 ) ) )
c establish surr2 order in the namelist header (surr2 possibly rearranged)
            call bubble( 2, nr_surr2_ndx, nr_surr2 )   ! short sort on surr2_ndx
            n2ae_ndx = index1a( 'N2AE', 2, nr_surr2 )
            n2aq_ndx = index1a( 'N2AQ', 2, nr_surr2 )

c create the ctrl table from the namelist header
            ctrl( 1 ) = 'TRNS'; ctrl_ndx( 1 ) = index( type_header, trim( ctrl( 1 ) ) )
            ctrl( 2 ) = 'DDEP'; ctrl_ndx( 2 ) = index( type_header, trim( ctrl( 2 ) ) )
            ctrl( 3 ) = 'WDEP'; ctrl_ndx( 3 ) = index( type_header, trim( ctrl( 3 ) ) )
            ctrl( 4 ) = 'CONC'; ctrl_ndx( 4 ) = index( type_header, trim( ctrl( 4 ) ) )
c establish ctrl order in the namelist header (ctrl possibly rearranged)
            call bubble( 4, ctrl_ndx, ctrl )   ! short sort on ctrl_ndx
            trns_ndx = index1a( 'TRNS', 4, ctrl )
            ddep_ndx = index1a( 'DDEP', 4, ctrl )
            wdep_ndx = index1a( 'WDEP', 4, ctrl )
            conc_ndx = index1a( 'CONC', 4, ctrl )

c the surrogate and ctrl arrays active dimensions not known a priori
            allocate ( 
     &      nr_spc( spc_dim ), nr_molwt( spc_dim ),
     &      nr_emis( spc_dim ), nr_emis_map( spc_dim ), nr_emis_fac( spc_dim ),
     &      nr_icbc( spc_dim ), nr_icbc_map( spc_dim ), nr_icbc_fac( spc_dim ),
     &      nr_depv( spc_dim ), nr_depv_map( spc_dim ), nr_depv_fac( spc_dim ),
     &      nr_scav( spc_dim ), nr_scav_map( spc_dim ), nr_scav_fac( spc_dim ),
     &      nr_n2ae( spc_dim ), nr_n2ae_map( spc_dim ),
     &      nr_n2aq( spc_dim ), nr_n2aq_map( spc_dim ),
     &      nr_trns( spc_dim ), nr_trns_map( spc_dim ),
     &      nr_ddep( spc_dim ), nr_ddep_map( spc_dim ),
     &      nr_wdep( spc_dim ), nr_wdep_map( spc_dim ),
     &      nr_conc( spc_dim ), nr_conc_map( spc_dim ),
     &      stat = ios )

            nr_spc = bl; nr_molwt = 0.0                        !  .
            nr_emis = bl; nr_emis_map = 0; nr_emis_fac = 0.0   !  .
            nr_icbc = bl; nr_icbc_map = 0; nr_icbc_fac = 0.0   !  .
            nr_depv = bl; nr_depv_map = 0; nr_depv_fac = 0.0   !  .
            nr_scav = bl; nr_scav_map = 0; nr_scav_fac = 0.0   ! array
            nr_n2ae = bl; nr_n2ae_map = 0                      !  .
            nr_n2aq = bl; nr_n2aq_map = 0                      ! assignments
            nr_trns = bl; nr_trns_map = 0                      !  .
            nr_ddep = bl; nr_ddep_map = 0                      !  .
            nr_wdep = bl; nr_wdep_map = 0                      !  .
            nr_conc = bl; nr_conc_map = 0                      !  .

            allocate ( mc_surr1( n_surr1 ),
     &                 mc_surr2( n_surr2 ),
     &                 mc_ctrl( n_ctrl ), stat = ios )

            mc_base  = mc_base_init    ! structure assignment
            mc_surr1 = mc_surr1_init   ! array structure assignment
            mc_surr2 = mc_surr2_init   ! array structure assignment
            mc_ctrl  = mc_ctrl_init    ! array structure assignment

            i1 = 0; i2 = 0; i3 = 0; i4 = 0
            j1 = 0; j2 = 0
            k1 = 0; k2 = 0; k3 = 0; k4 = 0
            icall = 0
#ifdef Verbose
            write( logdev,'(5x,a)' ) '@@@ NR @@@'
#endif
            do i = 1, spc_dim
               if ( type_matrix( i ) .eq. bl ) then
                  n_nr_spc = i - 1
                  exit
               end if
               call get_list( icall, type_matrix( i ), mc_base,
     &                        n_surr1, mc_surr1, n_surr2, mc_surr2,
     &                        n_ctrl, mc_ctrl )
               nr_spc( i ) = mc_base%name
               nr_molwt( i ) = mc_base%mw

               if ( emis_ndx .gt. 0 ) then
                  if ( mc_surr1( emis_ndx )%name .ne. bl ) then
                     i1 = i1 + 1
                     nr_emis( i1 )     = mc_surr1( emis_ndx )%name
                     nr_emis_map( i1 ) = mc_surr1( emis_ndx )%map
                     nr_emis_fac( i1 ) = mc_surr1( emis_ndx )%fac
                  end if
               end if
               if ( icbc_ndx .gt. 0 ) then
                  if ( mc_surr1( icbc_ndx )%name .ne. bl ) then
                     i2 = i2 + 1
                     nr_icbc( i2 )     = mc_surr1( icbc_ndx )%name
                     nr_icbc_map( i2 ) = mc_surr1( icbc_ndx )%map
                     nr_icbc_fac( i2 ) = mc_surr1( icbc_ndx )%fac
                  end if
               end if
               if ( depv_ndx .gt. 0 ) then
                  if ( mc_surr1( depv_ndx )%name .ne. bl ) then
                     i3 = i3 + 1
                     nr_depv( i3 )     = mc_surr1( depv_ndx )%name
                     nr_depv_map( i3 ) = mc_surr1( depv_ndx )%map
                     nr_depv_fac( i3 ) = mc_surr1( depv_ndx )%fac
                  end if
               end if
               if ( scav_ndx .gt. 0 ) then
                  if ( mc_surr1( scav_ndx )%name .ne. bl ) then
                     i4 = i4 + 1
                     nr_scav( i4 )     = mc_surr1( scav_ndx )%name
                     nr_scav_map( i4 ) = mc_surr1( scav_ndx )%map
                     nr_scav_fac( i4 ) = mc_surr1( scav_ndx )%fac
                  end if
               end if

               if ( n2ae_ndx .ne. 0 ) then
                  if ( mc_surr2( n2ae_ndx )%name .ne. bl ) then
                     j1 = j1 + 1
                     nr_n2ae( j1 )     = mc_surr2( n2ae_ndx )%name
                     nr_n2ae_map( j1 ) = mc_surr2( n2ae_ndx )%map
                  end if
              end if
               if ( n2aq_ndx .ne. 0 ) then
                  if ( mc_surr2( n2aq_ndx )%name .ne. bl ) then
                     j2 = j2 + 1
                     nr_n2aq( j2 )     = mc_surr2( n2aq_ndx )%name
                     nr_n2aq_map( j2 ) = mc_surr2( n2aq_ndx )%map
                  end if
               end if

               if ( trns_ndx .ne. 0 ) then
                  if ( mc_ctrl( trns_ndx )%name .ne. bl ) then
                     k1 = k1 + 1
                     nr_trns( k1 )     = mc_ctrl( trns_ndx )%name
                     nr_trns_map( k1 ) = mc_ctrl( trns_ndx )%map
                  end if
               end if
               if ( ddep_ndx .ne. 0 ) then
                  if ( mc_ctrl( ddep_ndx )%name .ne. bl ) then
                     k2 = k2 + 1
                     nr_ddep( k2 )     = mc_ctrl( ddep_ndx )%name
                     nr_ddep_map( k2 ) = mc_ctrl( ddep_ndx )%map
                  end if
               end if
               if ( wdep_ndx .ne. 0 ) then
                  if ( mc_ctrl( wdep_ndx )%name .ne. bl ) then
                     k3 = k3 + 1
                     nr_wdep( k3 )     = mc_ctrl( wdep_ndx )%name
                     nr_wdep_map( k3 ) = mc_ctrl( wdep_ndx )%map
                  end if
               end if
               if ( conc_ndx .ne. 0 ) then
                  if ( mc_ctrl( conc_ndx )%name .ne. bl ) then
                     k4 = k4 + 1
                     nr_conc( k4 )     = mc_ctrl( conc_ndx )%name
                     nr_conc_map( k4 ) = mc_ctrl( conc_ndx )%map
                  end if
               end if
            end do

            n_nr_emis = i1
            n_nr_icbc = i2
            n_nr_depv = i3
            n_nr_scav = i4
            n_nr_n2ae = j1
            n_nr_n2aq = j2
            n_nr_trns = k1
            n_nr_ddep = k2
            n_nr_wdep = k3
            n_nr_conc = k4

            nr_strt = ae_fini + 1
            nr_fini = nr_strt + n_nr_spc - 1

            deallocate ( mc_surr1, mc_surr2, mc_ctrl, stat = ios )

!           if ( nr_matrix_err ) then
!              success = .false.; return
!           end if

            type_header = bl
            type_matrix = bl   ! array assignment

            call nameval( tr_matrix, eqname )

            write( logdev,* ) ' '
            write( logdev,* ) '    TR Species Namelist: ', trim( eqname )

!           open( file = 'tr_matrix.nml', unit = dev_tr_nml,
            dev_tr_nml = junit()
            open( file = trim( eqname ), unit = dev_tr_nml,
     &            status = 'old', position = 'rewind' )
            read( nml = TR_nml, unit = dev_tr_nml )

c create the surr1 table from the namelist header
            surr1( 1 ) = 'EMIS'; surr1_ndx( 1 ) = index( type_header, trim( surr1( 1 ) ) )
            surr1( 2 ) = 'ICBC'; surr1_ndx( 2 ) = index( type_header, trim( surr1( 2 ) ) )
            surr1( 3 ) = 'DEPV'; surr1_ndx( 3 ) = index( type_header, trim( surr1( 3 ) ) )
            surr1( 4 ) = 'SCAV'; surr1_ndx( 4 ) = index( type_header, trim( surr1( 4 ) ) )
c establish surr1 order in the namelist header (surr1 possibly rearranged)
            call bubble( 4, surr1_ndx, surr1 )   ! short sort on surr1_ndx
            emis_ndx = index1a( 'EMIS', 4, surr1 )
            icbc_ndx = index1a( 'ICBC', 4, surr1 )
            depv_ndx = index1a( 'DEPV', 4, surr1 )
            scav_ndx = index1a( 'SCAV', 4, surr1 )

c create the surr2 table from the namelist header
            tr_surr2( 1 ) = 'T2AE'; tr_surr2_ndx( 1 ) = index( type_header, trim( tr_surr2( 1 ) ) )
            tr_surr2( 2 ) = 'T2AQ'; tr_surr2_ndx( 2 ) = index( type_header, trim( tr_surr2( 2 ) ) )
c establish surr2 order in the namelist header (surr2 possibly rearranged)
            call bubble( 2, tr_surr2_ndx, tr_surr2 )   ! short sort on surr2_ndx
            t2ae_ndx = index1a( 'T2AE', 2, tr_surr2 )
            t2aq_ndx = index1a( 'T2AQ', 2, tr_surr2 )

c create the ctrl table from the namelist header
            ctrl( 1 ) = 'ADV';  ctrl_ndx( 1 ) = index( type_header, trim( ctrl( 1 ) ) )
            ctrl( 2 ) = 'DIFF'; ctrl_ndx( 2 ) = index( type_header, trim( ctrl( 2 ) ) )
            ctrl( 3 ) = 'DDEP'; ctrl_ndx( 3 ) = index( type_header, trim( ctrl( 3 ) ) )
            ctrl( 4 ) = 'WDEP'; ctrl_ndx( 4 ) = index( type_header, trim( ctrl( 4 ) ) )
            ctrl( 5 ) = 'CONC'; ctrl_ndx( 5 ) = index( type_header, trim( ctrl( 5 ) ) )
c establish ctrl order in the namelist header (ctrl possibly rearranged)
            call bubble( 5, ctrl_ndx, ctrl )   ! short sort on ctrl_ndx
            adv_ndx  = index1a( 'ADV', 5, ctrl )
            diff_ndx = index1a( 'DIFF', 5, ctrl )
            ddep_ndx = index1a( 'DDEP', 5, ctrl )
            wdep_ndx = index1a( 'WDEP', 5, ctrl )
            conc_ndx = index1a( 'CONC', 5, ctrl )

c the surrogate and ctrl arrays active dimensions not known a priori
            allocate (
     &      tr_spc( spc_dim ), tr_molwt( spc_dim ),
     &      tr_emis( spc_dim ), tr_emis_map( spc_dim ), tr_emis_fac( spc_dim ),
     &      tr_icbc( spc_dim ), tr_icbc_map( spc_dim ), tr_icbc_fac( spc_dim ),
     &      tr_depv( spc_dim ), tr_depv_map( spc_dim ), tr_depv_fac( spc_dim ),
     &      tr_scav( spc_dim ), tr_scav_map( spc_dim ), tr_scav_fac( spc_dim ),
     &      tr_t2ae( spc_dim ), tr_t2ae_map( spc_dim ),
     &      tr_t2aq( spc_dim ), tr_t2aq_map( spc_dim ),
     &      tr_adv ( spc_dim ), tr_adv_map ( spc_dim ),
     &      tr_diff( spc_dim ), tr_diff_map( spc_dim ),
     &      tr_ddep( spc_dim ), tr_ddep_map( spc_dim ),
     &      tr_wdep( spc_dim ), tr_wdep_map( spc_dim ),
     &      tr_conc( spc_dim ), tr_conc_map( spc_dim ),
     &      stat = ios )

            tr_spc = bl; tr_molwt = 0.0                        !  .
            tr_emis = bl; tr_emis_map = 0; tr_emis_fac = 0.0   !  .
            tr_icbc = bl; tr_icbc_map = 0; tr_icbc_fac = 0.0   !  .
            tr_depv = bl; tr_depv_map = 0; tr_depv_fac = 0.0   !  .
            tr_scav = bl; tr_scav_map = 0; tr_scav_fac = 0.0   ! array
            tr_t2ae = bl; tr_t2ae_map = 0                      !  .
            tr_t2aq = bl; tr_t2aq_map = 0                      ! assignments
            tr_adv  = bl; tr_adv_map  = 0                      !  .
            tr_diff = bl; tr_diff_map = 0                      !  .
            tr_ddep = bl; tr_ddep_map = 0                      !  .
            tr_wdep = bl; tr_wdep_map = 0                      !  .
            tr_conc = bl; tr_conc_map = 0                      !  .

            allocate ( mc_surr1( n_surr1 ),
     &                 mc_surr2( n_surr2 ),
     &                 mc_ctrl( n_ctrl ), stat = ios )

            mc_base  = mc_base_init    ! structure assignment
            mc_surr1 = mc_surr1_init   ! array structure assignment
            mc_surr2 = mc_surr2_init   ! array structure assignment
            mc_ctrl  = mc_ctrl_init    ! array structure assignment

            i1 = 0; i2 = 0; i3 = 0; i4 = 0
            j1 = 0; j2 = 0
            k1 = 0; k2 = 0; k3 = 0; k4 = 0; k5 = 0
            icall = 0
#ifdef Verbose
            write( logdev,'(5x,a)' ) '@@@ TR @@@'
#endif
            do i = 1, spc_dim
               if ( type_matrix( i ) .eq. bl ) then
                  n_tr_spc = i - 1; exit
               end if
               call get_list( icall, type_matrix( i ), mc_base,
     &                        n_surr1, mc_surr1, n_surr2, mc_surr2,
     &                        n_ctrl, mc_ctrl )
               tr_spc( i ) = mc_base%name
               tr_molwt( i ) = mc_base%mw

               if ( emis_ndx .gt. 0 ) then
                  if ( mc_surr1( emis_ndx )%name .ne. bl ) then
                     i1 = i1 + 1
                     tr_emis( i1 )     = mc_surr1( emis_ndx )%name
                     tr_emis_map( i1 ) = mc_surr1( emis_ndx )%map
                     tr_emis_fac( i1 ) = mc_surr1( emis_ndx )%fac
                  end if
               end if
               if ( icbc_ndx .gt. 0 ) then
                  if ( mc_surr1( icbc_ndx )%name .ne. bl ) then
                     i2 = i2 + 1
                     tr_icbc( i2 )     = mc_surr1( icbc_ndx )%name
                     tr_icbc_map( i2 ) = mc_surr1( icbc_ndx )%map
                     tr_icbc_fac( i2 ) = mc_surr1( icbc_ndx )%fac
                  end if
               end if
               if ( depv_ndx .gt. 0 ) then
                  if ( mc_surr1( depv_ndx )%name .ne. bl ) then
                     i3 = i3 + 1
                     tr_depv( i3 )     = mc_surr1( depv_ndx )%name
                     tr_depv_map( i3 ) = mc_surr1( depv_ndx )%map
                     tr_depv_fac( i3 ) = mc_surr1( depv_ndx )%fac
                  end if
               end if
               if ( scav_ndx .gt. 0 ) then
                  if ( mc_surr1( scav_ndx )%name .ne. bl ) then
                     i4 = i4 + 1
                     tr_scav( i4 )     = mc_surr1( scav_ndx )%name
                     tr_scav_map( i4 ) = mc_surr1( scav_ndx )%map
                     tr_scav_fac( i4 ) = mc_surr1( scav_ndx )%fac
                  end if
               end if

               if ( t2ae_ndx .ne. 0 ) then
                  if ( mc_surr2( t2ae_ndx )%name .ne. bl ) then
                     j1 = j1 + 1
                     tr_t2ae( j1 )     = mc_surr2( t2ae_ndx )%name
                     tr_t2ae_map( j1 ) = mc_surr2( t2ae_ndx )%map
                  end if
               end if
               if ( t2aq_ndx .ne. 0 ) then
                  if ( mc_surr2( t2aq_ndx )%name .ne. bl ) then
                     j2 = j2 + 1
                     tr_t2aq( j2 )     = mc_surr2( t2aq_ndx )%name
                     tr_t2aq_map( j2 ) = mc_surr2( t2aq_ndx )%map
                  end if
               end if

               if ( adv_ndx .ne. 0 ) then
                  if ( mc_ctrl( adv_ndx )%name .ne. bl ) then
                     k1 = k1 + 1
                     tr_adv( k1 )     = mc_ctrl( adv_ndx )%name
                     tr_adv_map( k1 ) = mc_ctrl( adv_ndx )%map
                  end if
               end if
               if ( diff_ndx .ne. 0 ) then
                  if ( mc_ctrl( diff_ndx )%name .ne. bl ) then
                     k2 = k2 + 1
                     tr_diff( k2 )     = mc_ctrl( diff_ndx )%name
                     tr_diff_map( k2 ) = mc_ctrl( diff_ndx )%map
                  end if
               end if
               if ( ddep_ndx .ne. 0 ) then
                  if ( mc_ctrl( ddep_ndx )%name .ne. bl ) then
                     k3 = k3 + 1
                     tr_ddep( k3 )     = mc_ctrl( ddep_ndx )%name
                     tr_ddep_map( k3 ) = mc_ctrl( ddep_ndx )%map
                  end if
               end if
               if ( wdep_ndx .ne. 0 ) then
                  if ( mc_ctrl( wdep_ndx )%name .ne. bl ) then
                     k4 = k4 + 1
                     tr_wdep( k4 )     = mc_ctrl( wdep_ndx )%name
                     tr_wdep_map( k4 ) = mc_ctrl( wdep_ndx )%map
                  end if
               end if
               if ( conc_ndx .ne. 0 ) then
                  if ( mc_ctrl( conc_ndx )%name .ne. bl ) then
                     k5 = k5 + 1
                     tr_conc( k5 )     = mc_ctrl( conc_ndx )%name
                     tr_conc_map( k5 ) = mc_ctrl( conc_ndx )%map
                  end if
               end if
            end do

            n_tr_emis = i1
            n_tr_icbc = i2
            n_tr_depv = i3
            n_tr_scav = i4
            n_tr_t2ae = j1
            n_tr_t2aq = j2
            n_tr_adv  = k1
            n_tr_diff = k2
            n_tr_ddep = k3
            n_tr_wdep = k4
            n_tr_conc = k5

            tr_strt = nr_fini + 1
            tr_fini = tr_strt + n_tr_spc - 1

!           if ( tr_matrix_err ) success = .false.

            nspcsd = n_gc_spcd + n_ae_spc + n_nr_spc + n_tr_spc

         return

         end function cgrid_spcs_init

c----------------------------------------------------------------------------------------
         subroutine get_list( ic, str_in, base_out,
     &                        n_surr1, surr1_out, n_surr2, surr2_out, n_ctrl, ctrl_out )

c assume chem mechanism types are in the order of:
c 1) CGRID species, CGRID species molecular weight <- base type
c 2) EMIS, ICBC, DEPV, SCAV  [any order]           <- surrogate type 1 (surr name, surr fac)
c 3) x2AE, x2AQ  [any order]                       <- surrogate type 2 (surr name only )
c 4) DIFF, DDEP, WDEP, CONC  [any order]           <- control type

            use UTILIO_DEFN

            implicit none

#ifdef Verbose
            integer, save :: logdev
            logical, save :: firstime = .true.
#endif

            integer,            intent ( in out ) :: ic
            character( * ),     intent (  in ) :: str_in
            type( base_type ),  intent ( out ) :: base_out
            integer,            intent (  in ) :: n_surr1
            type( surr1_type ), intent ( out ) :: surr1_out( n_surr1 )
            integer,            intent (  in ) :: n_surr2
            type( surr2_type ), intent ( out ) :: surr2_out( n_surr2 )
            integer,            intent (  in ) :: n_ctrl
            type( ctrl_type ),  intent ( out ) :: ctrl_out( n_ctrl )

            integer, parameter :: nfld = 20
!           character(  1 ), parameter :: dc = ',', bl = ' '  ! namelist field delimiting character, blank character
            character(  1 ), parameter :: dc = ':', bl = ' '  ! namelist field delimiting character, blank character
            character( 32 ) :: str( nfld ) = bl               ! field substring
            character(  1 ) :: chr, lastchr
            integer ip, is, n
            integer fn                                        ! field number
            integer :: jp( nfld ), kp( nfld )                 ! field starting and ending position
            logical :: nbf( nfld )                            ! non-blank field
            integer maxlen
!           integer, save :: ic = 0   ! subroutine call count

#ifdef Verbose
            if ( firstime ) then
               firstime = .false.
               logdev = init3 ()
            end if
#endif

            ic = ic + 1
            maxlen = len_trim( str_in )
            fn = 1; ip = 0

101         continue    ! consume leading blanks in field
            nbf( fn ) = .true.   ! posit non-blank field
            ip = ip + 1
            if ( ip .gt. maxlen ) go to 301   ! end of line
            chr = str_in( ip:ip )
            if ( chr .eq. bl ) go to 101   ! skip leading blanks before valid char
            lastchr = chr
            jp( fn ) = ip   ! 1st non-blank char in field

201         continue
            if ( chr .ne. dc ) then
               if ( chr .ne. bl ) lastchr = chr
               ip = ip + 1
               if ( ip .gt. maxlen ) go to 301   ! end of line
               chr = str_in( ip:ip )
               go to 201
            else
               if ( lastchr .eq. dc ) nbf( fn ) = .false.   ! we've got a blank field
               kp( fn ) = ip - 1
               fn = fn + 1
            end if
            go to 101

301         continue
            kp( fn ) = ip - 1
            if ( chr .eq. dc ) nbf( fn ) = .false.

            do n = 1, fn
#ifdef Verbose
               write( logdev,'(3i4,l)' ) n, jp( n ), kp( n ), nbf( n )
               if ( kp( n ) .lt. jp( n ) ) then   ! don't depend on this - use bf( n )
                  write( logdev,* ) n, 'end of string < start'
               end if
#endif
               if ( nbf( n ) ) then
                  str( n ) = str_in( jp( n ):kp( n ) )
#ifdef Verbose
                  write( logdev,* ) n, len_trim( str( n ) ), str( n )
#endif
               end if
            end do

c assumed group order: {CGRID spc, mw},
c                      {EMIS, ICBC, DEPV, SCAV} group,
c                      {x2AE, x2AQ} group,
c                      {TRNS, DDEP, WDEP, CONC} group
c str( 1 ) = CGRID name, str( 2 ) = CGRID name molec. wt.

            read( str( 1 ),'( a16 )' )  base_out%name
            if ( index( str( 2 ), '.' ) .le. 0 ) then   ! integer
               read( str( 2 ),'( i7 )' ) is
               base_out%mw = real( is )
            else
               read( str( 2 ),'( f7.2 )' ) base_out%mw
            end if

c type 1 surrogate (surr1)
c if there is a surrogate name and the factor field is blank, assume fac = 1.0
            ip = 3
            do n = 1, n_surr1
               if ( nbf( ip ) ) then
                  read( str( ip ),'( a16 )' ) surr1_out( n )%name
                  surr1_out( n )%map = ic
                  if ( nbf( ip+1 ) ) then
                     if ( index( str( ip+1 ), '.' ) .le. 0 ) then   ! integer
                        read( str( ip+1 ),'( i7 )' ) is
                        surr1_out( n )%fac = real( is )
                     else
                        read( str( ip+1 ),'( f7.2 )' ) surr1_out( n )%fac
                     end if
                  else
                     surr1_out( n )%fac = 1.0
                  end if
               else
                  surr1_out( n )%name = bl
                  surr1_out( n )%map = 0
                  surr1_out( n )%fac = 0.0
               end if
#ifdef Verbose
               write( logdev,* ) 'surr1', n, ip, surr1_out( n )%name, surr1_out( n )%map,
     &                            surr1_out( n )%fac
#endif
               ip = ip + 2
            end do

c type 2 surrogate (surr2)
            do n = 1, n_surr2
               if ( nbf( ip ) ) then
                  surr2_out( n )%map = ic
                  read( str( ip ),'( a16 )' ) surr2_out( n )%name
               else
                  surr2_out( n )%name = bl
                  surr2_out( n )%map = 0
               end if
#ifdef Verbose
               write( logdev,* ) 'surr2', n, ip, surr2_out( n )%name, surr2_out( n )%map
#endif
               ip = ip + 1
            end do

            do n = 1, n_ctrl
               if ( nbf( ip ) ) then
                  if ( str( ip ) .eq. 'yes' .or.
     &                 str( ip ) .eq. 'Yes' .or.
     &                 str( ip ) .eq. 'YES' ) then  ! use call upcase
                     read( str( 1 ),'( a16 )' ) ctrl_out( n )%name
                     ctrl_out( n )%map = ic
                  else
                     ctrl_out( n )%name = bl
                  end if
               else
                  ctrl_out( n )%name = bl
                  ctrl_out( n )%map = 0
               end if
#ifdef Verbose
               write( logdev,* ) 'ctrl', n, ip, ctrl_out( n )%name, ctrl_out( n )%map
#endif
               ip = ip + 1
            end do

            return
            end subroutine get_list

c----------------------------------------------------------------------------------------
            subroutine bubble ( n, ia, aa )

            use UTILIO_DEFN

            implicit none

#ifdef Verbose
            integer, save :: logdev
            logical, save :: firstime = .true.
#endif

            integer, intent( in ) :: n
            integer, intent( in out ) :: ia( n )
            character( * ), intent( in out ) :: aa( n )
            character( 8 ) :: at
            integer i, it
            logical :: no_swap

#ifdef Verbose
            if ( firstime ) then
               firstime = .false.
               logdev = init3 ()
            end if

            do i = 1, n
               write( logdev,* ) i, ia( i ), aa( i )
            end do
#endif

            do
               no_swap = .true.
               do i = 1, n - 1
                   if ( ia( i ) .gt. ia( i+1 ) ) then
                      it = ia( i ); ia( i ) = ia( i+1 ); ia( i+1 ) = it
                      at = aa( i ); aa( i ) = aa( i+1 ); aa( i+1 ) = at
                      no_swap = .false.
                   end if
               end do
               if ( no_swap ) exit
             end do

            it = 0
            do i = 1, n
               if ( ia( i ) .gt. 0 ) then
                  it = it + 1
                  ia( i ) = it
               else
                  aa( i ) = ' '
               end if
!              write( logdev,* ) i, ia( i ), aa( i )
            end do

c compress list; move blanks to end
            it = 1
            do i = 1, n
               if ( aa( i ) .ne. ' ' ) then
                  aa( it ) = aa( i )
                  ia( it ) = ia( i )
                  it = it + 1
               end if
            end do
            do i = it, n
               aa( i ) = ' '
               ia( i ) = 0
            end do
#ifdef Verbose
            do i = 1, n
               write( logdev,* ) i, ia( i ), aa( i )
            end do
#endif

            return
            end subroutine bubble

c----------------------------------------------------------------------------------------
            integer function index1a ( name, n, nlist )
            implicit none
            character( * ) name        ! character string being searched for
            integer n                  ! length of array to be searched
            character( * ) nlist( : )  ! array to be searched

            integer i

            do i = 1, n
               if ( name .eq. nlist( i ) ) then
                  index1a = i
                  return
               end if
           end do
           index1a = 0
           return

           end function index1a

      end module cgrid_spcs

