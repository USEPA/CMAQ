
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
      subroutine aero_sedv ( col, row, cgrd, vsed_ae )

C-----------------------------------------------------------------------
C Get accum. and coarse mode grav. settling vel
C used Binkowski`s aerosol dry deposition routine as a guide
C 08 Feb 13 J.Young: initial
C 20 Jun 14 J.Young: restructure
C 22 Oct 14 J.Bash:  replaced P0 with STDATMPA from CONST.EXT and shared 
C                    variables in the asx_data_mod
C May 2015 B. Murphy, H. Pye: Updated treatment of aerosol moments
C-----------------------------------------------------------------------

      use grid_conf           ! horizontal & vertical domain specifications
      use cgrid_spcs          ! CGRID mechanism species
      use utilio_defn      
      use asx_data_mod, Only: Met_Data
      use aero_data           ! aero variable data
      use aeromet_data        ! Includes CONST.EXT

      implicit none

C Includes:

C Arguments
      integer, intent( in )  :: col, row
      real,    intent( in )  :: cgrd( :,: )     ! cgrid subsection (layers,species)
      real,    intent( out ) :: vsed_ae( :,: )  ! settling velocities [ m/s ]

C Parameters
      real,    parameter :: t0 = 288.15      ! [ K ] ! starting standard surface temp.
      real,    parameter :: two3 = 2.0 / 3.0
      integer, parameter :: n_ae_sed_spc = 6 ! no. of surrogates for aero settling velocities

C set up species indices for settling velocity internal array vsed
      integer, parameter :: vgnacc = 1,  ! accumulation mode number
     &                      vgncor = 2,  ! coarse mode number
     &                      vgsacc = 3,  ! accumulation mode surface area
     &                      vgscor = 4,  ! coarse mode surface area
     &                      vgmacc = 5,  ! accumulation mode mass
     &                      vgmcor = 6   ! coarse mode mass

C Local variables:

C follow the Namelist dep vel surrogate name table
      character( 16 ) :: vgae_name( n_ae_sed_spc ) = ! grav. settling vel surrogate table
     &                (/ 'VNUMACC',
     &                   'VNUMCOR',
     &                   'VSRFACC',
     &                   'VSRFCOR',
     &                   'VMASSJ ',
     &                   'VMASSC ' /)

      integer, allocatable, save :: sedi_sur( : )   ! pointer to surrogate

      real, allocatable, save :: xxlsgac( : )   ! log of stnd dev
      real, allocatable, save :: xxlsgco( : )
      real, allocatable, save :: dgacc  ( : )   ! geometric mean diameter
      real, allocatable, save :: dgcor  ( : )
      real, allocatable, save :: pdensac( : )   ! particle density
      real, allocatable, save :: pdensco( : )

      real, allocatable, save :: xlm    ( : )   ! mean free path [ m ]
      real, allocatable, save :: amu    ( : )   ! dynamic viscosity [ kg/m/s ]

      real, allocatable, save :: vsed   ( :,: ) ! grav settling velocity [ m/s ]

      real m2_wet, m2_dry
      real m3_wet, m3subt, m3_dry

      logical, save :: firstime = .true.
      character( 16 ), save :: pname = 'AERO_SEDI'
      character( 16 ) :: vname                  ! variable name
      character( 96 ) :: xmsg = ' '

      integer  l, v, n, j      ! loop counters
      integer  spc, s          ! species loop counter
      integer  astat

      integer :: jdate = 0, jtime = 0

      interface
        subroutine get_sedv ( xlm, amu,
     &                        dgacc, dgcor,
     &                        xxlsgac, xxlsgco,
     &                        pdensac, pdensco,
     &                        vsed )
          real, intent( in ) :: xlm    ( : )  ! atmos mean free path [ m ]
          real, intent( in ) :: amu    ( : )  ! atmos dynamic viscosity [ kg/(m s) ]
          real, intent( in ) :: dgacc  ( : )  ! accum mode geom mean diameter [ m ]
          real, intent( in ) :: dgcor  ( : )  ! coarse mode geom mean diameter  [ m ]
          real, intent( in ) :: xxlsgac( : )  ! accum mode log of stnd dev
          real, intent( in ) :: xxlsgco( : )  ! coarse mode
          real, intent( in ) :: pdensac( : )  ! avg particle density in accum mode
          real, intent( in ) :: pdensco( : )  ! avg particle density in coarse mode
          real, intent( out ) :: vsed  ( :,: ) ! settling velocity [ m/s ]
        end subroutine get_sedv
      end interface

c-----------------------------------------------------------------------

      if ( firstime ) then
         firstime = .false.

C  Allocate arrays
         allocate( xxlsgac( nlays ),
     &             xxlsgco( nlays ),
     &             dgacc  ( nlays ),
     &             dgcor  ( nlays ),
     &             pdensac( nlays ),
     &             pdensco( nlays ),
     &             xlm    ( nlays ),
     &             amu    ( nlays ), stat = astat )
         if ( astat .ne. 0 ) then
            xmsg = 'Failure allocating'
     &           //  ' xxlsgac, xxlsgco,'
     &           //  ' dgacc, dgcor,'
     &           //  ' pdensac, pdensco,'
     &           //  ' xlm, or amu'
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

         allocate( vsed( nlays,n_ae_spc), stat = astat )
         if ( astat .ne. 0 ) then
            xmsg = 'Failure allocating vsed'
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

         allocate( sedi_sur( n_ae_spc ), stat = astat )
         if ( astat .ne. 0 ) then
            xmsg = 'Failure allocating sedi_sur'
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

C Set the settling vel surrogate pointers according to the depv table
         j = 0
         do v = 1, n_ae_depv   ! assume n_ae_spc = n_ae_depv
            n = index1( ae_depv( v ), n_ae_sed_spc, vgae_name )
            if ( n .ne. 0 ) then
               j = j + 1
               sedi_sur( v ) = n
            else
               write( logdev,* ) ' surrogate ', trim( ae_depv( v ) ),
     &                           ' not used for', v, trim( ae_spc( v ) )
               sedi_sur( v ) = 0
            end if
         end do
         n = j

         write( logdev,* ) n, ' Aerosol species with a grav. settling vel'
         do j = 1, n_ae_spc
            n = sedi_sur( j )
            if ( n .ne. 0 ) write( logdev,'( i3, 2x, a9, i3, 2x, a )' )
     &                             j, ae_spc( j ), n, trim( ae_depv( j ) )
         end do

      end if    ! firstime      

      do l = 1, nlays

C Set meteorological data for the grid cell.
         airtemp = Met_Data%ta  ( col,row,l )
         airpres = Met_Data%pres( col,row,l )

C extract grid cell concentrations of aero species from CGRID
C into aerospc_conc in aero_data module
C Also converts dry surface area to wet 2nd moment
#ifdef sens
         call extract_aero( cgrd( l,: ), .true., cgrd( :,: ), .false. )
#else
         call extract_aero( cgrd( l,: ), .true. )  ! set minimum floor
#endif

C Get the geometric mean diameters and standard deviations of the
C "wet" size distribution
         call getpar( .false. )     
C                        | do not fix stnd dev`s to existing value

C Save getpar values to arrays
         xxlsgac( l ) = aeromode_lnsg( 2 )
         xxlsgco( l ) = aeromode_lnsg( 3 )

         dgacc( l )   = aeromode_diam( 2 )
         dgcor( l )   = aeromode_diam( 3 )

         pdensac( l ) = aeromode_dens( 2 )
         pdensco( l ) = aeromode_dens( 3 )
 
C Calculate mean free path [ m ]:
         xlm( l ) = 6.6328e-8 * STDATMPA * airtemp / ( t0 * airpres )

C Calculate dynamic viscosity [ kg/m/s ]:
         amu( l ) = 1.458e-6 * airtemp * sqrt( airtemp ) / ( airtemp + 110.4 )

      end do ! layer loop

C get settling velocities:
      call get_sedv ( xlm, amu,
     &                dgacc, dgcor,
     &                xxlsgac, xxlsgco,
     &                pdensac, pdensco,
     &                vsed )

C Return sedimentation velocities for aerosols and cfl-safe iteration count

C "Stores read an entire cache line, modify the target, then write back the
C  entire line. Thus, non-consecutive stores are worse than non-consecutive
C  loads."

      do l = 1, nlays
         do v = 1, n_ae_spc
            if ( sedi_sur( v ) .gt. 0 ) then
               vsed_ae( v,l ) = vsed( l,sedi_sur( v ) )
            else
               vsed_ae( v,l ) = 0.0
            end if
         end do
      end do

      return
      end subroutine aero_sedv

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine get_sedv ( xlm, amu,
     &                      dgacc, dgcor,
     &                      xxlsgac, xxlsgco,
     &                      pdensac, pdensco,
     &                      vsed )

C Calculate settling velocity for Aitken, accumulation, and coarse modes.
C-----------------------------------------------------------------------

      use asx_data_mod, Only: Met_Data
      use aeromet_data   ! Includes CONST.EXT

      implicit none

C *** arguments

      real, intent( in ) :: xlm    ( : )  ! atmospheric mean free path [m]
      real, intent( in ) :: amu    ( : )  ! atmospheric dynamic viscosity [kg/(m s)]
      real, intent( in ) :: dgacc  ( : )  ! accum mode geom mean diameter [m]
      real, intent( in ) :: dgcor  ( : )  ! coarse mode geom mean diameter [m]
      real, intent( in ) :: xxlsgac( : )  ! accum mode log of modal geom stnd dev`s
      real, intent( in ) :: xxlsgco( : )  ! coarse mode log of modal geom stnd dev`s
      real, intent( in ) :: pdensac( : )  ! avg particle dens in accum mode [kg/m**3]
      real, intent( in ) :: pdensco( : )  ! avg particle dens in coarse mode [kg/m**3]
      real, intent( out ) :: vsed  ( :,: )  ! grav settling velocity [ m/s ]

C *** array indices hardcoded to match SUBROUTINE aero_sedi
      integer, parameter :: vgnacc = 1,  ! accumulation mode number
     &                      vgncor = 2,  ! coarse mode number
     &                      vgsacc = 3,  ! accumulation mode surface area
     &                      vgscor = 4,  ! coarse mode surface area
     &                      vgmacc = 5,  ! accumulation mode mass
     &                      vgmcor = 6   ! coarse mode mass

C modal Knudsen numbers X bhat
      real bknacc   ! accumulation mode 
      real bkncor   ! coarse mode

C modal sedimentation velocities for 0th (number), 2nd (srf area), and 3rd (mass) moments
      real vghat0a, vghat0c
      real vghat2a, vghat2c
      real vghat3a, vghat3c

      integer l

      real    dconst2, dconst3a, dconst3c
      real    bxlm

 !    real, parameter :: bhat    = 1.246 ! Constant from Cunningham slip correction
      real, parameter :: bhat    = 2.492 ! 2 X Constant from Cunningham slip correction

C Scalar variables for VARIABLE standard deviations.

      real    l2sgac, l2sgco   ! log^2( sigmag )

      real    esac01           ! accumu mode " ** 4
      real    esco01           ! coarse      "

      real    esac02           ! accumu mode " ** 8
      real    esco02           ! coarse      "

      real    esac04           ! accumu mode " ** 16
      real    esco04           ! coarse      "

      real    esac05           ! accumu mode " ** 20
      real    esco05           ! coarse      "

      real    esac07           ! accumu mode " ** 28
      real    esco07           ! coarse      "

      real    esac12           ! accumu mode " ** 48    
      real    esco12           ! coarse      "     

      real    esac16           ! accumu mode " ** 64
      real    esco16           ! coarse      "

C-----------------------------------------------------------------------

      do l = 1, size( met_data%ta, 3 )

C Calculate Knudsen numbers * bhat
         bxlm = bhat * xlm( l )
         bknacc = bxlm / dgacc( l )
         bkncor = bxlm / dgcor( l )

C Calculate functions of variable standard deviation.
         l2sgac = xxlsgac( l ) * xxlsgac( l )
         l2sgco = xxlsgco( l ) * xxlsgco( l )

         esac01  = exp( 0.5 * l2sgac )
         esco01  = exp( 0.5 * l2sgco )

         esac02  = esac01 * esac01
         esco02  = esco01 * esco01

         esac04  = esac02 * esac02
         esco04  = esco02 * esco02

         esac05  = esac04 * esac01
         esco05  = esco04 * esco01

         esac07  = esac05 * esac02
         esco07  = esco05 * esco02

         esac12  = esac07 * esac05
         esco12  = esco07 * esco05

         esac16  = esac12 * esac04
         esco16  = esco12 * esco04

         dconst2  = grav / ( 18.0 * amu( l ) )
         dconst3a = dconst2 * pdensac( l ) * dgacc( l ) * dgacc( l )
         dconst3c = dconst2 * pdensco( l ) * dgcor( l ) * dgcor( l )

c acc mode
         vghat0a  = dconst3a * ( esac04  + bknacc * esac01 )
         vghat2a  = dconst3a * ( esac12  + bknacc * esac05 )
         vghat3a  = dconst3a * ( esac16  + bknacc * esac07 )

c coarse mode
         vghat0c  = dconst3c * ( esco04  + bkncor * esco01 )
         vghat2c  = dconst3c * ( esco12  + bkncor * esco05 )
         vghat3c  = dconst3c * ( esco16  + bkncor * esco07 )

C settling velocities

C vsed of 0th moment for the number 
         vsed( l,vgnacc ) = vghat0a   ! accum mode
         vsed( l,vgncor ) = vghat0c   ! coarse mode

c vsed of 2nd moment for the surface area 
         vsed( l,vgsacc ) = vghat2a   ! accum mode
         vsed( l,vgscor ) = vghat2c   ! coarse mode

c vsed of 3rd moment for the mass 
         vsed( l,vgmacc ) = vghat3a   ! accum mode
         vsed( l,vgmcor ) = vghat3c   ! coarse mode

      end do ! end loop on l

      return
      end subroutine get_sedv
