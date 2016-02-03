module multcomp_mc_inc

  use met_param_inc
  use struct_inc
  use aer_sections_inc, only:MAX_SEC

  implicit none
  save

  integer, parameter :: MAX_NCORR  = 10     !Max # of turbulent correlations
  
  integer, parameter :: MAX_REACTIONS = 250 !Max # of reactions
  
  real    tambient           ! time for ambient concentration
  real    vol
  real    zk                 !Values of zenith angle last use to compute rates
  real, dimension(MAX_REACTIONS) :: kamb !reaction rate coefs without rxn vol ratio

  integer istage             !current stage
  integer nspecies           !# of species
  integer nspectot           !# of total species (incl equilibrium)
  integer nequilibrium       !# of equilibrium species
  integer nambient           !# of ambient species
  logical lamb3d             !use 3D ambient instead of imc file

! Index of OH (for Hg-Br chemistry)
  integer ioh

! Darkness flag      Moved from multcomp_inc to here March 2012, PK, ENVIRON
  logical lflag_dark

  real, dimension(MAX_NCORR) :: corr

  !==== Working space

  type  work_species
    sequence
    logical equil  !Equilibrium flag
    real    m      !Mass
    real    c      !Star
    real    a      !Amb
    real    taudry !Dry dep rate
    real    tauwet !Wet dep rate
  end type  work_species

  type  StepMCdata
    sequence
    type ( puff_str ) p                                     !Puff structure
    type (work_species), dimension(MAX_MC) ::  ps           !(MAX_MC)
    integer  ij                                             ! 
    integer  ipuf                                           ! 
    integer  istage                                         ! 
    integer  ngd                                            !number of good steps 
    integer  nbd                                            !number of bad steps 
    real t                                                  !Current time
    real dt                                                 !Time step (sec)
    real dtmc                                               !Time step (sec)
    real csav                                               !
    real vol                                                !
    real fac_diag                                           !Factor set to determine if step is included in diagnostics
    real zk
    real tab
    real pb
    real hb
    real cldall
    real cldallt
    real cldallp
    real cmassp
    real cmasst
    real pratebl
    real fcc
    real fprcpc
    real radyn
    real us2
    real ws2 
    logical lflag_dark      !Added March 2012, PK, ENVIRON
    real, dimension(MAX_REACTIONS)          :: kamb
    real, dimension(MAX_NCORR)              :: corr
    real, dimension(MAX_MC)                 :: ddepos
    real, dimension(MAX_MC)                 :: wdepos
    real, dimension(MAX_MC)                 :: chem
  end type  StepMCdata

  type  Splitdata
    sequence
    integer  npuf                                          
    real     frac                                        
  end type  Splitdata

  type (Splitdata),  DIMENSION(:), ALLOCATABLE         :: Splitdat

  type (StepMCdata), DIMENSION(:), ALLOCATABLE, TARGET :: StepMCdat
  type (StepMCdata), POINTER                           :: pStepMCdat
  type (work_species), DIMENSION(MAX_MC)               :: ps

  logical laerosol           !Include aerosol phase chemistry
  integer naerp !aerosol particles species
  integer index_aerp(MAX_MC), index_sec(MAX_MC) !pointers
  integer nsec_aer !aerosol particles sections
  integer nbad_chem, ngd_chem

end module multcomp_mc_inc
