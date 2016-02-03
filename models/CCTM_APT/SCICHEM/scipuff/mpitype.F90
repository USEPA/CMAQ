subroutine build_mpi_type()
!*******************************************************************************
!
! FUNCTION:  
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:                  
!
! REVISION HISTORY:
! 01/31/2007 : New routine for multiprocessor code -BC 
! Aug 2010: 
!          1) Add parallel cpp compiler directive
!          2) Add cld variables cldallt, cldallp, cmassp, cmasst, fcc and fprcpc
! -BC(SAGE-MGT)
! March 2012: Updated for CMAQ 5.0 final, PK, ENVIRON
!*******************************************************************************
#ifdef parallel
! use mpi
use param_inc
use multcomp_inc
use common_mpi
use common_mc_puf

implicit none

include 'mpif.h'

type ( puff_str )                  :: p
type (work_species)                :: pfs
type (StepMCdata)                  :: mcdat
type (chem_reaction)               :: react
type (chem_species)                :: cspec

integer (kind=mpi_address_kind), dimension(:), allocatable :: address
integer (kind=mpi_address_kind), dimension(:), allocatable :: displacements
integer, dimension(:), allocatable                         :: block_lengths
integer, dimension(:), allocatable                         :: typelist

integer                                                    :: nsize, ios, i, ncount
integer                                                    :: npint, npreal, npchar


!--- Build a derived datatype for puff_str
!real    xbar,ybar,zbar
!real    sxx,sxy,sxz,syy,syz,szz
!real    axx,axy,axz,ayy,ayz,azz,det
!real    c,cc,xuc,xvc,yvc,yvsc,yvbc,zwc,wc,ccb
!real    si,si2,sv
!real    sr,cfo
!real    zi,zc
!real    uo,vo,wo
!integer ityp,inxt,iprv,ipgd,idtl,idtn,iaux

!type  puff_str_ri 
!  real,    dimension(np_real) :: p_real
!  integer, dimension(np_int)  :: p_int 

nsize = np_real + np_int

allocate(block_lengths(nsize),displacements(nsize),address(nsize+1),typelist(nsize),STAT=ios)

typelist(1:np_real)       = MPI_REAL
typelist(np_real+1:nsize) = MPI_INTEGER
block_lengths(1:nsize)    = 1

i = 1
call MPI_Get_address(p, address(i), ierr)
i = i + 1
call MPI_Get_address(p%xbar, address(i), ierr)
i = i + 1
call MPI_Get_address(p%ybar, address(i), ierr)
i = i + 1
call MPI_Get_address(p%zbar, address(i), ierr)
i = i + 1
call MPI_Get_address(p%sxx, address(i), ierr)
i = i + 1
call MPI_Get_address(p%sxy, address(i), ierr)
i = i + 1
call MPI_Get_address(p%sxz, address(i), ierr)
i = i + 1
call MPI_Get_address(p%syy, address(i), ierr)
i = i + 1
call MPI_Get_address(p%syz, address(i), ierr)
i = i + 1
call MPI_Get_address(p%szz, address(i), ierr)
i = i + 1
call MPI_Get_address(p%axx, address(i), ierr)
i = i + 1
call MPI_Get_address(p%axy, address(i), ierr)
i = i + 1
call MPI_Get_address(p%axz, address(i), ierr)
i = i + 1
call MPI_Get_address(p%ayy, address(i), ierr)
i = i + 1
call MPI_Get_address(p%ayz, address(i), ierr)
i = i + 1
call MPI_Get_address(p%azz, address(i), ierr)
i = i + 1
call MPI_Get_address(p%det, address(i), ierr)
i = i + 1
call MPI_Get_address(p%c, address(i), ierr)
i = i + 1
call MPI_Get_address(p%cc, address(i), ierr)
i = i + 1
call MPI_Get_address(p%xuc, address(i), ierr)
i = i + 1
call MPI_Get_address(p%xvc, address(i), ierr)
i = i + 1
call MPI_Get_address(p%yvc, address(i), ierr)
i = i + 1
call MPI_Get_address(p%yvsc, address(i), ierr)
i = i + 1
call MPI_Get_address(p%yvbc, address(i), ierr)
i = i + 1
call MPI_Get_address(p%zwc, address(i), ierr)
i = i + 1
call MPI_Get_address(p%wc, address(i), ierr)
i = i + 1
call MPI_Get_address(p%ccb, address(i), ierr)
i = i + 1
call MPI_Get_address(p%si, address(i), ierr)
i = i + 1
call MPI_Get_address(p%si2, address(i), ierr)
i = i + 1
call MPI_Get_address(p%sv, address(i), ierr)
i = i + 1
call MPI_Get_address(p%sr, address(i), ierr)
i = i + 1
call MPI_Get_address(p%cfo, address(i), ierr)
i = i + 1
call MPI_Get_address(p%zi, address(i), ierr)
i = i + 1
call MPI_Get_address(p%zc, address(i), ierr)
i = i + 1
call MPI_Get_address(p%uo, address(i), ierr)
i = i + 1
call MPI_Get_address(p%vo, address(i), ierr)
i = i + 1
call MPI_Get_address(p%wo, address(i), ierr)
i = i + 1
call MPI_Get_address(p%ityp, address(i), ierr)
i = i + 1
call MPI_Get_address(p%inxt, address(i), ierr)
i = i + 1
call MPI_Get_address(p%iprv, address(i), ierr)
i = i + 1
call MPI_Get_address(p%ipgd, address(i), ierr)
i = i + 1
call MPI_Get_address(p%idtl, address(i), ierr)
i = i + 1
call MPI_Get_address(p%idtn, address(i), ierr)
i = i + 1
call MPI_Get_address(p%iaux, address(i), ierr)

do i = 1,nsize
  displacements(i) = address(i+1) - address(1)
end do

call MPI_TYPE_CREATE_STRUCT(nsize, block_lengths, displacements,typelist, MPI_PUFFSTRUC, ierr)

call MPI_TYPE_COMMIT(MPI_PUFFSTRUC, ierr)

deallocate(block_lengths, displacements,address, typelist, STAT=ios)


!--- Build a derived datatype for work_species
!logical equil  !Equilibrium flag
!real    m      !Mass
!real    c      !Star
!real    a      !Amb
!real    taudry !Dry dep rate
!real    tauwet !Wet dep rate

nsize = 6

allocate(block_lengths(nsize),displacements(nsize),address(nsize+1),typelist(nsize),STAT=ios)

typelist(1)            = MPI_LOGICAL
typelist(2:nsize)      = MPI_REAL
block_lengths(1:nsize) = 1

i = 1
call MPI_Get_address(pfs, address(i), ierr)
i = i + 1
call MPI_Get_address(pfs%equil, address(i), ierr)
i = i + 1
call MPI_Get_address(pfs%m, address(i), ierr)
i = i + 1
call MPI_Get_address(pfs%c, address(i), ierr)
i = i + 1
call MPI_Get_address(pfs%a, address(i), ierr)
i = i + 1
call MPI_Get_address(pfs%taudry, address(i), ierr)
i = i + 1
call MPI_Get_address(pfs%tauwet, address(i), ierr)

do i = 1,nsize
  displacements(i) = address(i+1) - address(1)
end do

call MPI_TYPE_CREATE_STRUCT(nsize, block_lengths, displacements,typelist, MPI_WORKSPEC, ierr)

call MPI_TYPE_COMMIT(MPI_WORKSPEC, ierr)

deallocate(block_lengths, displacements,address, typelist, STAT=ios)

!--- Build a derived datatype for StepMCdata

!    1 :type ( puff_str ) p                             !Puff structure
!    2 :type (work_species), dimension(MAX_MC)   ::  ps !
!    3 :integer ij                                      ! 
!    4 :integer ipuf                                    ! 
!    5 :integer istage                                  !
!    6 :integer ngd                                     !
!    7 :integer nbd                                     !
!    8 :real t                                          !Time (sec)
!    9 :real dt                                         !Time step (sec)
!    10:real dtmc                                       !Time step (sec)
!    11:real csav                                       !
!    12:real vol                                        !
!    13:real fac_diag                                   !Factor set to determine if step is included in diagnostics
!    14:real zk
!    15:real tab
!    16:real pb
!    17:real hb
!    18:real cldall
!    19:real cldallt
!    20:real cldallp
!    21:real cmassp
!    22:real cmasst
!    23:real pratebl
!    24:real fcc
!    25:real fprcpc
!    26:real radyn
!    27:real us2
!    28:real ws2
!    29:logical lflag_dark
!    30:real kamb(MAX_REACTIONS)
!    31:real corr(MAX_NCORR)
!    32:real, dimension(MAX_MC)                 :: ddepos
!    33:real, dimension(MAX_MC)                 :: wdepos
!    34:real, dimension(MAX_MC)                 :: chem

nsize = 34

allocate(block_lengths(nsize),displacements(nsize),address(nsize+1),typelist(nsize),STAT=ios)

typelist(1)            = MPI_PUFFSTRUC
typelist(2)            = MPI_WORKSPEC !MPI_NWORKSPEC
typelist(3:7)          = MPI_INTEGER
typelist(8:28)         = MPI_REAL
typelist(29)           = MPI_LOGICAL
typelist(30:nsize)     = MPI_REAL
block_lengths(1:nsize) = 1
block_lengths(2)       = MAX_MC
block_lengths(30)      = MAX_REACTIONS
block_lengths(31)      = MAX_NCORR
block_lengths(32:34)   = MAX_MC

i = 1
call MPI_Get_address(mcdat, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%p, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%ps, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%ij, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%ipuf, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%istage, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%ngd, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%nbd, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%t, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%dt, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%dtmc, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%csav, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%vol, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%fac_diag, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%zk, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%tab, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%pb, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%hb, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%cldall, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%cldallt, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%cldallp, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%cmassp, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%cmasst, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%pratebl, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%fcc, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%fprcpc, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%radyn, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%us2,address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%ws2, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%lflag_dark, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%kamb, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%corr, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%ddepos, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%wdepos, address(i), ierr)
i = i + 1
call MPI_Get_address(mcdat%chem, address(i), ierr)

do i = 1,nsize
  displacements(i) = address(i+1) - address(1)
end do

call MPI_TYPE_CREATE_STRUCT(nsize, block_lengths, displacements,typelist, MPI_MCDAT, ierr)

call MPI_TYPE_COMMIT(MPI_MCDAT, ierr)

deallocate(block_lengths, displacements,address, typelist, STAT=ios)

!--- Build a derived datatype for chem_reaction
!  integer class  !Reaction Class
!  integer ID     !Reactant ID
!  integer iA     !Reactant species ID
!  integer iB     !Reactant species ID
!  integer nP     !# of products
!  integer icorr  !Reaction turbulent correla
!  integer ktype  !Rate type
!  integer iP(MAX_PRODUCTS)       !Product species IDs
!  real    k      !Current rate
!  real    fB     !Reactant stoichiometric co
!  real    fP(MAX_PRODUCTS)       !Product stoichiometric coe
!  real    kdata(MAX_KDATA)       !Rate data

npint  = 8
npreal = 4
nsize = npreal + npint

allocate(block_lengths(nsize),displacements(nsize),address(nsize+1),typelist(nsize),STAT=ios)

typelist(1:npint)       = MPI_INTEGER
typelist(npint+1:nsize) = MPI_REAL
block_lengths(1:nsize)  = 1
block_lengths(8)        = MAX_PRODUCTS
block_lengths(11)       = MAX_PRODUCTS
block_lengths(12)       = MAX_KDATA

i = 1
call MPI_Get_address(react, address(i), ierr)
i = i + 1
call MPI_Get_address(react%class, address(i), ierr)
i = i + 1
call MPI_Get_address(react%ID, address(i), ierr)
i = i + 1
call MPI_Get_address(react%iA, address(i), ierr)
i = i + 1
call MPI_Get_address(react%iB, address(i), ierr)
i = i + 1
call MPI_Get_address(react%nP, address(i), ierr)
i = i + 1
call MPI_Get_address(react%icorr, address(i), ierr)
i = i + 1
call MPI_Get_address(react%ktype, address(i), ierr)
i = i + 1
call MPI_Get_address(react%iP(1), address(i), ierr)
i = i + 1
call MPI_Get_address(react%k, address(i), ierr)
i = i + 1
call MPI_Get_address(react%fB, address(i), ierr)
i = i + 1
call MPI_Get_address(react%fP(1), address(i), ierr)
i = i + 1
call MPI_Get_address(react%kdata(1), address(i), ierr)

do i = 1,nsize
  displacements(i) = address(i+1) - address(1)
end do

call MPI_TYPE_CREATE_STRUCT(nsize, block_lengths, displacements,typelist, MPI_REACTSTRUC, ierr)

call MPI_TYPE_COMMIT(MPI_REACTSTRUC, ierr)

deallocate(block_lengths, displacements,address, typelist, STAT=ios)

!--- Build a derived datatype for chem_species
!  integer class !Species type
!  integer star  !Pointer for mean correlation
!  integer dos   !Srf dos pointer
!  integer dep   !Srf dep pointer
!  integer pmode !Particle mode (if particle)
!  real    amb   !Ambient value
!  real    tol   !Tolerance
!  real    vdep  !Deposition velocity
!  real    emis_split    !Fraction of emitted species that is this species
!  real    density       !Particle density (if particle)

!  character*16 name      !Species name
!  character*16 nameemit !Name of surrogate emitted species

npint  = 5
npreal = 5
npchar = 2
nsize = npreal + npint + npchar

allocate(block_lengths(nsize),displacements(nsize),address(nsize+1),typelist(nsize),STAT=ios)

typelist(1:npint)              = MPI_INTEGER
typelist(npint+1:npint+npreal) = MPI_REAL
typelist(npint+npreal+1:nsize) = MPI_CHARACTER
block_lengths(1:nsize)              = 1
block_lengths(npint+npreal+1:nsize) = 16

i = 1
call MPI_Get_address(cspec, address(i), ierr)
i = i + 1
call MPI_Get_address(cspec%class, address(i), ierr)
i = i + 1
call MPI_Get_address(cspec%star, address(i), ierr)
i = i + 1
call MPI_Get_address(cspec%dos, address(i), ierr)
i = i + 1
call MPI_Get_address(cspec%dep, address(i), ierr)
i = i + 1
call MPI_Get_address(cspec%pmode, address(i), ierr)
i = i + 1
call MPI_Get_address(cspec%amb, address(i), ierr)
i = i + 1
call MPI_Get_address(cspec%tol, address(i), ierr)
i = i + 1
call MPI_Get_address(cspec%vdep, address(i), ierr)
i = i + 1
call MPI_Get_address(cspec%emis_split, address(i), ierr)
i = i + 1
call MPI_Get_address(cspec%density, address(i), ierr)
i = i + 1
call MPI_Get_address(cspec%name, address(i), ierr)
i = i + 1
call MPI_Get_address(cspec%nameemit, address(i), ierr)

do i = 1,nsize
  displacements(i) = address(i+1) - address(1)
end do

call MPI_TYPE_CREATE_STRUCT(nsize, block_lengths, displacements,typelist, MPI_CSPEC, ierr)

call MPI_TYPE_COMMIT(MPI_CSPEC, ierr)

deallocate(block_lengths, displacements,address, typelist, STAT=ios)

return
end

subroutine pack_mpi_mchead()
!*******************************************************************************
!
! FUNCTION:  
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:                  
!
! REVISION HISTORY:
!
!*******************************************************************************

! use mpi
use param_inc
use multcomp_inc, s_no=>NO, s_no2=>NO2, s_o3=>O3, s_oh=>OH, s_ho2=>HO2, s_no3=>NO3,&
                  s_hno3=>HNO3, s_n2o5=>N2O5, s_o1d=>O1D, s_c2o3=>C2O3
use common_mpi
use common_mc_puf
use common_mc_met
!use aqueous_species_inc

implicit none

include 'mpif.h'

character*1   :: cdum
integer       :: npint, npreal, npchar, nplogic
integer       :: i, ios, position, size_rstruc, size_cspec

!=== pack multcomp_inc header data derived datatype into MPI_MCHEAD

!--- nspectot, nspecies, ncorrm, ncorrt, nreacts, nequilibrium, nambient,  
!--- nstage, nvoc, nzenith, nkrad, nlim, i_units, ie_units, ic_units, isolve,
!--- nfast, nslow, nrxn_curr, neq_s, nsec_aer, naqueous, naqchem, naero, naerp,
!--- Total above: 25
!
!--- index_aerp, index_aqueous, index_aero, index_sec, (4*MAX_MC)
!--- ikey_spec, (NKEY_SPEC)
!--- indxf, indxs, index_lim, (3*MAX_MC)
!--- indx_rxns, (MAX_REACTIONS)
!
!--- Note: these are now calculated for all processors in aerosol_chem
!--- SULF, HNO3, NH3, HCL, O3, H2O2, PAA, FOA, SO2, MHP, N2O5,
!--- OH, HO2, NO2, NO, NO3, HONO, HNO4, CO, HCHO, ALD2, PAN, PAR, NTR,
!--- OLE, AAC, ETH, TOL, CRES, XYL, MGLY, ISOP
!--- Total above: 32

!pknote--- Additional: SULFP, ALKRXN, ISOPRXN, TRPRXN, TOLNRXN, TOLHRXN,
!                      XYLNRXN, XYLHRXN, BNZNRXN, BNZHRXN, SESQRXN,
!                      VALK, VXYL1, VXYL2, VTOL1, VTOL2, VBNZ1, VBNZ2,
!                      VTRP1, VTRP2, VISO1, VISO2, VSQT
!--- Total above: 23
! 25+32+23 = 80
!npint   = 80 + MAX_MC*7 + NKEY_SPEC + MAX_REACTIONS
! now don't need to pack these
npint   = 25 + MAX_MC*7 + NKEY_SPEC + MAX_REACTIONS

!--- em_conv, ks_conv, kt_conv, rtol(1), param_chem, nitr(MAX_MC),
!--- dm_aer(MAX_SEC), secbnds_aer(MAX_SEC+1)

npreal  = 5 + MAX_MC + 2*MAX_SEC + 1

!--- character*16 mc_units, character*128 amb_file,
!--- character*16 aero_names(MAX_MC)
!--- character*16 aqueous_names(MAX_MC)

npchar  = 1 + 2*MAX_MC

!--- lstep_amb, lstage, lchem_split, laqueous,
!--- laerosol, lstep_amb3d, lstep_tot, ldump_chem, lsolve_ynb, lbalance,
!--- IsLinear, IsStar

nplogic = 10 + SIZE(IsLinear) + SIZE(IsStar)

call MPI_PACK_SIZE(1, MPI_INTEGER, MPI_COMM_WORLD,& 
                    size_int, ierr )

call MPI_PACK_SIZE(1, MPI_REAL, MPI_COMM_WORLD,& 
                    size_real, ierr )

call MPI_PACK_SIZE(1, MPI_CHARACTER, MPI_COMM_WORLD,& 
                    size_char, ierr )

call MPI_PACK_SIZE(1, MPI_LOGICAL, MPI_COMM_WORLD,& 
                    size_logic, ierr )

call MPI_PACK_SIZE(1, MPI_REACTSTRUC, MPI_COMM_WORLD,& 
                     size_rstruc, ierr )

call MPI_PACK_SIZE(1, MPI_CSPEC, MPI_COMM_WORLD,& 
                     size_cspec, ierr )

size_mchead = npint*size_int + npreal*size_real &
               + 128*size_char + npchar*16*size_char &
               + nplogic*size_logic &
               + size_rstruc*MAX_REACTIONS + size_cspec*MAX_MC

allocate(MPI_mchead(size_mchead),STAT=ios)

if (myid == 0) then
  do i = 1,nspectot
    nitr(i) = sparam(i)%nit
  end do
else
  call init_pack 
end if

position = 0

call MPI_Pack ( nspectot, 1, MPI_INTEGER, MPI_MCHEAD,& 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nspecies, 1, MPI_INTEGER, MPI_MCHEAD,& 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( ncorrm, 1, MPI_INTEGER, MPI_MCHEAD,& 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( ncorrt, 1, MPI_INTEGER, MPI_MCHEAD,& 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nreacts, 1, MPI_INTEGER, MPI_MCHEAD,& 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nequilibrium, 1, MPI_INTEGER, MPI_MCHEAD,&
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nambient, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nstage, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nvoc, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nzenith, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nkrad, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nlim, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( i_units, 1, MPI_INTEGER, MPI_MCHEAD, & 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( ie_units, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( ic_units, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( isolve, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nfast, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nslow, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nrxn_curr, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( neq_s, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nsec_aer, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( naqueous, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( naqchem, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( naero, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( naerp, 1, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( index_aerp, MAX_MC, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( index_aqueous, MAX_MC, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( index_aero, MAX_MC, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( index_sec, MAX_MC, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( ikey_spec, NKEY_SPEC, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( indxf, MAX_MC, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( indxs, MAX_MC, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( index_lim, MAX_MC, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( indx_rxns, MAX_REACTIONS, MPI_INTEGER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( SULF, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( HNO3, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( NH3, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( HCL, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( O3, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( H2O2, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( PAA, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( FOA, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( SO2, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( MHP, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( N2O5, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( OH, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( HO2, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( NO2, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( NO, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( NO3, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( HONO, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( HNO4, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( CO, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( HCHO, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( ALD2, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( PAN, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( PAR, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( NTR, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( OLE, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( AAC, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( ETH, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( TOL, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( CRES, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( XYL, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( MGLY, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

!call MPI_Pack ( ISOP, 1, MPI_INTEGER, MPI_MCHEAD, &
!             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( em_conv, 1, MPI_REAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( ks_conv, 1, MPI_REAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( kt_conv, 1, MPI_REAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( rtol(1), 1, MPI_REAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( param_chem, 1, MPI_REAL, MPI_MCHEAD, & 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( nitr, MAX_MC, MPI_REAL, MPI_MCHEAD, & 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( dm_aer, MAX_SEC, MPI_REAL, MPI_MCHEAD, & 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( secbnds_aer, MAX_SEC+1, MPI_REAL, MPI_MCHEAD, & 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( mc_units, 16, MPI_CHARACTER, MPI_MCHEAD, & 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( amb_file, 128, MPI_CHARACTER, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

do i = 1,MAX_MC
  call MPI_Pack ( aero_names(i), 16, MPI_CHARACTER, MPI_MCHEAD, &
                  size_mchead, position, MPI_COMM_WORLD, ierr )
end do

do i = 1,MAX_MC
  call MPI_Pack ( aqueous_names(i), 16, MPI_CHARACTER, MPI_MCHEAD, &
                  size_mchead, position, MPI_COMM_WORLD, ierr )
end do

call MPI_Pack ( lstep_amb, 1, MPI_LOGICAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( lstage, 1, MPI_LOGICAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( lchem_split, 1, MPI_LOGICAL, MPI_MCHEAD, & 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( laqueous, 1, MPI_LOGICAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( laerosol, 1, MPI_LOGICAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( lstep_amb3d, 1, MPI_LOGICAL, MPI_MCHEAD, & 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( lstep_tot, 1, MPI_LOGICAL, MPI_MCHEAD, & 
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( ldump_chem, 1, MPI_LOGICAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( lsolve_ynb, 1, MPI_LOGICAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( lbalance, 1, MPI_LOGICAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( IsLinear, MAX_REACTIONS, MPI_LOGICAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( IsStar, MAX_MC, MPI_LOGICAL, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( reaction, MAX_REACTIONS, MPI_REACTSTRUC, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

call MPI_Pack ( species, MAX_MC, MPI_CSPEC, MPI_MCHEAD, &
             size_mchead, position, MPI_COMM_WORLD, ierr )

return

end

subroutine unpack_mpi_mchead()
!*******************************************************************************
!
! FUNCTION:  
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:                  
!
! REVISION HISTORY:
!*******************************************************************************

! use mpi
use param_inc
use multcomp_inc, s_no=>NO, s_no2=>NO2, s_o3=>O3, s_oh=>OH, s_ho2=>HO2, s_no3=>NO3,&
                  s_hno3=>HNO3, s_n2o5=>N2O5, s_o1d=>O1D, s_c2o3=>C2O3
use common_mpi
use common_mc_puf
use common_mc_met
!use aqueous_species_inc

implicit none

include 'mpif.h'

character*1   :: cdum
integer       :: npint, npreal, npchar, nplogic
integer       :: i, ios, position
integer       :: loc_no

!=== unpack multcomp_inc header data derived datatype into MPI_MCHEAD

position = 0

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nspectot, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nspecies, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                ncorrm, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                ncorrt, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nreacts, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nequilibrium, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nambient, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nstage, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nvoc, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nzenith, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nkrad, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nlim, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                i_units, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                ie_units, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                ic_units, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                isolve, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nfast, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nslow, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nrxn_curr, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                neq_s, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nsec_aer, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                naqueous, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                naqchem, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                naero, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                naerp, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                index_aerp, MAX_MC, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                index_aqueous, MAX_MC, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                index_aero, MAX_MC, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                index_sec, MAX_MC, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                ikey_spec, NKEY_SPEC, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                indxf, MAX_MC, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                indxs, MAX_MC, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                index_lim, MAX_MC, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                indx_rxns, MAX_REACTIONS, MPI_INTEGER, MPI_COMM_WORLD, ierr )

! Now don't need to unpack these
!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
!                SULF, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
!                HNO3, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
!                NH3, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
!                HCL, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                O3, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                H2O2, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                PAA, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                FOA, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                SO2, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                MHP, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                N2O5, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                OH, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                HO2, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                NO2, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                NO, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                NO3, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                HONO, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                HNO4, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                CO, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                HCHO, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                ALD2, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                PAN, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                PAR, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                NTR, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                OLE, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                AAC, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                ETH, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                TOL, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                CRES, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                XYL, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                MGLY, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

!call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, &
!                ISOP, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                em_conv, 1, MPI_REAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                ks_conv, 1, MPI_REAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                kt_conv, 1, MPI_REAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                rtol(1), 1, MPI_REAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                param_chem, 1, MPI_REAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                nitr, MAX_MC, MPI_REAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                dm_aer, MAX_SEC, MPI_REAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                secbnds_aer, MAX_SEC+1, MPI_REAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                mc_units, 16, MPI_CHARACTER, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                amb_file, 128, MPI_CHARACTER, MPI_COMM_WORLD, ierr )

do i = 1,MAX_MC
  call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                    aero_names(i), 16, MPI_CHARACTER, MPI_COMM_WORLD, ierr )
end do

do i = 1,MAX_MC
  call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                    aqueous_names(i), 16, MPI_CHARACTER, MPI_COMM_WORLD, ierr )
end do

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                lstep_amb, 1, MPI_LOGICAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                lstage, 1, MPI_LOGICAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                lchem_split, 1, MPI_LOGICAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                laqueous, 1, MPI_LOGICAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                laerosol, 1, MPI_LOGICAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                lstep_amb3d, 1, MPI_LOGICAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                lstep_tot, 1, MPI_LOGICAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                ldump_chem, 1, MPI_LOGICAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                lsolve_ynb, 1, MPI_LOGICAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                lbalance, 1, MPI_LOGICAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                IsLinear, MAX_REACTIONS, MPI_LOGICAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                IsStar, MAX_MC, MPI_LOGICAL, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                reaction, MAX_REACTIONS, MPI_REACTSTRUC, MPI_COMM_WORLD, ierr )

call MPI_Unpack ( MPI_MCHEAD, size_mchead, position, & 
                species, MAX_MC, MPI_CSPEC, MPI_COMM_WORLD, ierr )

do i = 1,nspectot
  sparam(i)%nit = nitr(i)
end do

return
end

subroutine init_pack
!*******************************************************************************
!
! FUNCTION:  
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:                  
!
! REVISION HISTORY:
!*******************************************************************************

! use mpi
use param_inc
use multcomp_inc, s_no=>NO, s_no2=>NO2, s_o3=>O3, s_oh=>OH, s_ho2=>HO2, s_no3=>NO3,&
                  s_hno3=>HNO3, s_n2o5=>N2O5, s_o1d=>O1D, s_c2o3=>C2O3
use common_mpi
use common_mc_puf
use common_mc_met
!use aqueous_species_inc

implicit none

include 'mpif.h'

integer :: i

nspectot = 0
nspecies = 0
ncorrm = 0
ncorrt = 0
nreacts = 0
nequilibrium = 0
nambient = 0
nstage = 0
nvoc = 0
nzenith = 0
nkrad = 0
nlim = 0
i_units = 0
ie_units = 0
ic_units = 0
isolve = 0
nfast = 0
nslow = 0
nrxn_curr = 0
neq_s = 0
nsec_aer = 0
naqueous = 0
naqchem = 0
naero = 0
naerp = 0
index_aerp = 0
index_aqueous = 0
index_aero = 0
index_sec = 0
ikey_spec = 0
indxf = 0
indxs = 0
index_lim = 0
indx_rxns = 0

!SULF = 0
!HNO3 = 0
!NH3 = 0
!HCL = 0
!O3 = 0
!H2O2 = 0
!PAA = 0
!FOA = 0
!SO2 = 0
!MHP = 0
!N2O5 = 0
!OH = 0
!HO2 = 0
!NO2 = 0
!NO = 0
!NO3 = 0
!HONO = 0
!HNO4 = 0
!CO = 0
!HCHO = 0
!ALD2 = 0
!PAN = 0
!PAR = 0
!NTR = 0
!OLE = 0
!AAC = 0
!ETH = 0
!TOL = 0
!CRES = 0
!XYL = 0
!MGLY = 0
!ISOP = 0

em_conv = 0
ks_conv = 0
kt_conv = 0
rtol = 0
param_chem = 0
nitr = 0
dm_aer = 0
secbnds_aer = 0
mc_units = ''
amb_file = ''
aero_names = ''
aqueous_names = ''
lstep_amb = .false.
lstage = .false.
lchem_split = .false.
laqueous = .false.
laerosol = .false.
lstep_amb3d = .false.
lstep_tot = .false.
ldump_chem = .false.
lsolve_ynb = .false.
lbalance = .false.
IsLinear = .false.
IsStar = .false.

do i = 1,MAX_REACTIONS
  reaction(i)%class = 0
  reaction(i)%ID = 0
  reaction(i)%iA = 0
  reaction(i)%iB = 0
  reaction(i)%nP = 0
  reaction(i)%icorr = 0
  reaction(i)%ktype = 0
  reaction(i)%ip(:) = 0
  reaction(i)%k = 0.
  reaction(i)%fB = 0.
  reaction(i)%fP(:) = 0.
  reaction(i)%kdata(:) = 0.
end do
do i = 1,MAX_MC
  species(i)%class = 0
  species(i)%star = 0
  species(i)%dos = 0
  species(i)%dep = 0
  species(i)%pmode = 0
  species(i)%amb = 0.
  species(i)%tol = 0.
  species(i)%vdep = 0.
  species(i)%emis_split = 0.
  species(i)%density = 0.
  species(i)%name = ''
  species(i)%nameemit = ''
end do

#endif

return
end
