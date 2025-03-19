#ifdef sens

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module aero_ddm3d

C-----------------------------------------------------------------------
C   15 Aug 13 S.L.Napelenok: initial version
C   30 Dec 19 S.L.Napelenok: update for cmaq 5.3.1
C-----------------------------------------------------------------------

      Use ddm3d_defn, Only : npmax

      Implicit None

c These parameters have to match what is found in the following modules:
c aero_data, precursor_data, soa_defn

      Real( 8 ), Allocatable, Save :: s_aerospc_conc( :,:,: )  ! aero species sensitivity [ ug/m^3 ]

      Real( 8 ), Allocatable, Save :: s_precursor_conc( :,: )  ! precursor sensitivity [ug/m^3]
      Real( 8 ), Allocatable, Save :: s_so4rate( : )
      Real( 8 ), Allocatable, Save :: s_phg_rate( : )          ! particulate mercury gas-phase production rate [ug/m^3*s]
c     Real( 8 ), Allocatable, Save :: s_xh2so4( : ) 

      Real( 8 ), Allocatable, Save :: og_sens( :,: )           ! vapors sensitivity
      Real( 8 ), Allocatable, Save :: rog_dsens( :,: )         ! reaction counters senstivity

      Logical, Save :: ae_ddm3d_ready = .False.

      Real( 8 ) :: cbsens


      Logical, Save :: wrt_flag = .False.

      Contains
C-----------------------------------------------------------------------
      
      Subroutine init_aero_ddm3d

      Use UTILIO_DEFN

      Implicit None

      Character( 96 ) :: xmsg = ''
      Integer allocstat
      Integer logdev

      logdev = init3()

      Allocate ( s_so4rate( npmax ), stat = allocstat )
      If ( allocstat .Ne. 0 ) then
         xmsg = 'Failure allocating s_so4rate'
         Call m3exit ( 'init_aero_ddm3d', 0, 0, xmsg, xstat2 )
      End If

      Allocate ( s_phg_rate( npmax ), stat = allocstat )
      If ( allocstat .Ne. 0 ) then
         xmsg = 'Failure allocating s_phg_rate'
         Call m3exit ( 'init_aero_ddm3d', 0, 0, xmsg, xstat2 )
      End If

c     Allocate ( s_xh2so4( npmax ), stat = allocstat )
c     If ( allocstat .Ne. 0 ) then
c        xmsg = 'Failure allocating s_xh2so4'
c        Call m3exit ( 'init_aero_ddm3d', 0, 0, xmsg, xstat2 )
c     End If

      ae_ddm3d_ready = .True.

      Return
      End Subroutine init_aero_ddm3d

C-----------------------------------------------------------------------

      End Module aero_ddm3d

#endif

