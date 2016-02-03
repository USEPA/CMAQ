
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module aero6_ddm3d

C-----------------------------------------------------------------------
C   15 Aug 13 S.L.Napelenok: initial version
C-----------------------------------------------------------------------

      Use ddm3d_defn, Only : npmax

      Implicit None

c These parameters have to match what is found in the following modules:
c aero_data, precursor_data, soa_defn
      Integer, Private, Parameter :: n_aerospc = 40   ! number of aero species
      Integer, Private, Parameter :: n_mode = 3       ! number of modes:
      Integer, Private, Parameter :: n_precursor = 19 ! # of precursors
      Integer, Private, Parameter :: n_vapor = 12     ! # of partitioning SVOCs
      Integer, Private, Parameter :: n_orgprod = 10   ! # of ROG rxn counter species

      Real( 8 ), Allocatable, Save :: s_aerospc_conc( :,:,: )  ! aero species sensitivity [ ug/m^3 ]

      Real( 8 ), Allocatable, Save :: s_precursor_conc( :,: )  ! precursor sensitivity [ug/m^3]
      Real( 8 ), Allocatable, Save :: s_so4rate( : )

      Real( 8 ), Allocatable, Save :: s_vapor_conc( :,: )      ! vapor sensitivity
      Real( 8 ), Allocatable, Save :: s_orgprod_conc( :,: )    ! orgprod sensitivity

      Logical, Save :: ae_ddm3d_ready = .False.


      Contains
C-----------------------------------------------------------------------
      
      Subroutine init_aero6_ddm3d

      Use UTILIO_DEFN

      Implicit None

      Character( 96 ) :: xmsg = ''
      Integer allocstat
      Integer logdev

      logdev = init3()

      Allocate ( s_aerospc_conc( n_aerospc,n_mode,npmax ), stat = allocstat )
      If ( allocstat .Ne. 0 ) then
         xmsg = 'Failure allocating s_aerospc_conc'
         Call m3exit ( 'init_aero6_ddm3d', 0, 0, xmsg, xstat2 )
      End If

      Allocate ( s_precursor_conc( n_precursor,npmax ), stat = allocstat )
      If ( allocstat .Ne. 0 ) then
         xmsg = 'Failure allocating s_precursor_conc'
         Call m3exit ( 'init_aero6_ddm3d', 0, 0, xmsg, xstat2 )
      End If

      Allocate ( s_so4rate( npmax ), stat = allocstat )
      If ( allocstat .Ne. 0 ) then
         xmsg = 'Failure allocating s_so4rate'
         Call m3exit ( 'init_aero6_ddm3d', 0, 0, xmsg, xstat2 )
      End If

      Allocate ( s_vapor_conc( n_vapor,npmax ), stat = allocstat )
      If ( allocstat .Ne. 0 ) then
         xmsg = 'Failure allocating s_vapor_conc'
         Call m3exit ( 'init_aero6_ddm3d', 0, 0, xmsg, xstat2 )
      End If

      Allocate ( s_orgprod_conc( n_orgprod,npmax ), stat = allocstat )
      If ( allocstat .Ne. 0 ) then
         xmsg = 'Failure allocating s_orgprod_conc'
         Call m3exit ( 'init_aero6_ddm3d', 0, 0, xmsg, xstat2 )
      End If

      ae_ddm3d_ready = .True.

      Return
      End Subroutine init_aero6_ddm3d

C-----------------------------------------------------------------------

      End Module aero6_ddm3d



