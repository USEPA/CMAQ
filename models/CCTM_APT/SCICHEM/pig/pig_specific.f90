!************************** NOTE **********************************************
!
! All these routines are necessary, even if they are dummy routines
! so that 2 versions of the code can be easily maintained, 
! the regular PC version and the Plume-in-grid version.
!
! They were created so that many other source files are common to both versions.
!
!******************************************************************************

logical function chkgrd_merge(x,y,rxx,ryy,p,cgrid)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Check if a puff is going out of the boundary or 
!            if it is ready to be transferred to the host model
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  siginv         sum_diagnostics                dump_pig
!               ldump_pig
!
! REVISION HISTORY: 
!
!    Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use diagnostics
use interface_definitions, only: DUMP_PIG

implicit none

! --- ARGUMENTS

real x, y            !Dimensionless puff distance from xmin and ymin
real rxx, ryy        !Puff size compared to the grid size
REAL :: CGRID( :,:,:,: )  !3D ambient concentrations from the host model
type ( puff_str ) p  !Puff structure
 
! --- PARAMETERS

real, parameter :: RFAC = 4.0

! --- LOCALS

logical chkx, chky, chkgrd, ldump_pig

real xfac, yfac
integer isp

type ( puff_mc ) pm

!-------- set inverse moments

call siginv(p)

!-------- check if puff is in domain

chkgrd_merge = chkgrd(x,y,rxx,ryy)

! -------- if puff is leaving domain, sum in boundary diagnostics
!          otherwise, check if it should be transferred to the host

if (p%idtl >= 0) then   !not already removed or static
  if (.not. chkgrd_merge) then

    call sum_diagnostics(bndry,p)

  else if(ldump_pig(p)) then

    call dump_pig(p,cgrid)
    chkgrd_merge = .false.

  end if
end if

return
end

subroutine set_pressure(zm,pb)
!******************************************************************************
!
! FUNCTION:  Dummy routine (PC version does something different)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- ARGUMENTS
 
real zm, pb  !Height and pressure

!Pressure is interpolated for pig in get_uvwt_ua

return
end

subroutine set_imc_flags(species_units,rate_species_units, &
                                rate_time_units,ambient_file)
!******************************************************************************
!
! FUNCTION:  Wire IMC flags for PiG version
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!    Updated April 2005 for CMAQ-APT-PM (PKK, AER)
!******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc

implicit none

! --- ARGUMENTS
 
character*(*) species_units       !Units of the ambient concentration
character*(*) rate_species_units  !Concentration units on the rate constants
character*(*) rate_time_units     !Time units on the rate constants
character*(*) ambient_file        !Name of the ambient file

! -- These are the flags that must be set for a PiG run

lstep_amb          = .false.     !wired for PIG
species_units      = 'ppm'       !wired for PIG
rate_species_units = 'ppm'       !wired for PIG
rate_time_units    = 'min'       !wired for PIG
ambient_file       = NOT_SET_C   !wired for PIG

return
end

subroutine setup_ktable
!******************************************************************************
!
! FUNCTION:   This is a dummy routine for the PiG version,
!             since the PC version does something different
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
!There is no ktable for PiG

return
end

subroutine set_reaction_type(itype)
!******************************************************************************
!
! FUNCTION:   This is in case the reaction types on a PiG input file 
!             were set to zero.  The reaction rates would be assumed zero.
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use multcomp_inc

implicit none

! --- ARGUMENTS

integer itype     !Reaction rate type

itype = ID_K_TEMP !(Just a fill-in; not used in PIG)
                  ! may not be constant type (in case all zeros)
return
end

logical function ldark()
!******************************************************************************
!
! FUNCTION:  Let the host model tell SCICHEM if it is night or day
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use multcomp_inc

implicit none

ldark = lflag_dark   !Set from host model darkness flag

return
end

subroutine mapfac_pig( x , y , xmap , ymap )
!******************************************************************************
!
! FUNCTION:   Get map factors from stored arrays as set by the host model
!             (Just get nearest, do not interpolate)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met

implicit none

! --- ARGUMENTS
 
real x, y, xmap, ymap  !Location and map factors at that location

! --- LOCALS

integer i, j, ij

i = nint((x-xmin)/dxb) + 1
j = nint((y-ymin)/dyb) + 1

i = max(1,i)
i = min(i,nxb)
j = max(1,j)
j = min(j,nyb)

ij = (j-1)*nxb + i

xmap = xmap2d(ij)
ymap = ymap2d(ij)

return
end

subroutine read_prj_xmap
!******************************************************************************
!
! FUNCTION:  Read the map factors and roughness height from the project file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc

implicit none

! --- LOCALS

integer ios, i

if (iversion >= 1204) then
  read(lun_prj,iostat=ios) (xmap2d(i),i=1,nxyb),(ymap2d(i),i=1,nxyb), &
                           (zruf2(i),i=1,nxyb)

  if (ios /= 0) then
    nError   = RD_ERROR
    eRoutine = 'read_prj_xmap'
    eMessage = 'Error reading project file'
    eInform  = 'File='//TRIM(file_prj)
  end if
end if

return

end

subroutine read_prj_zb
!******************************************************************************
!
! FUNCTION:  Read the vertical grid from the project file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc

implicit none
 
! --- LOCALS

integer ios, i

if (iversion >= 1212) then
  read(lun_prj,iostat=ios) nzb,(zb(i),i=1,nzb),(zbw(i),i=1,nzb)

  if (ios /= 0) then
    nError   = RD_ERROR
    eRoutine = 'read_prj_zb'
    eMessage = 'Error reading project file'
    eInform  = 'File='//TRIM(file_prj)
  end if
end if

return

end

subroutine read_prj_latlon
!******************************************************************************
!
! FUNCTION:  Read the latitude and longitude arrays from the project file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc

implicit none

! --- LOCALS

integer ios, i

if (iversion >= 1215) then
  read(lun_prj,iostat=ios) (lat2d(i),i=1,nxyb),(lon2d(i),i=1,nxyb)

  if (ios /= 0) then
    nError   = RD_ERROR
    eRoutine = 'read_prj_latlon'
    eMessage = 'Error reading project file'
    eInform  = 'File='//TRIM(file_prj)
  end if
end if

return

end

subroutine read_prj_mc
!******************************************************************************
!
! FUNCTION:   Read the multicomponent data from the project file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!  Updated Feb. 2004 for consistency with stand-alone SCICHEM V1601 (PKK, AER)
!  Updated Apr. 2007 for consistency with stand-alone SCICHEM V1900 (PKK, AER)
!  Updated Sep. 2011 for CMAQ 5.0, Version 2200, PK, ENVIRON
!******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc
use files_inc

implicit none

! --- LOCALS

real ktmp(MAX_ZENITH*MAX_KTAB)
integer ios,i,j, ij

do i = 1, MAX_MC
  sparam(i)%scav = 0.
  sparam(i)%mwt   = 0.
  sparam(i)%nit   = 0.   !zero out nitrogen balance
  sparam(i)%lim   = 0.   !zero out volume limit
  sparam(i)%param = 0.
  i_rel_list(i)   = i    !default release index for old projects
end do

lamb3d   = .false.
amb_file = NOT_SET_C
nequilibrium = 0

if (iversion >= 2200) then

  read(lun_prj,iostat=ios) nspectot,ncorrm,ncorrt,nreacts,&
                   nequilibrium,nambient,nstage,nvoc,&
                   nzenith,nkrad,i_units,ks_conv,kt_conv,rtol,&
                   ie_units, mc_units, isolve, lstep_amb,  &
                   amb_file, lstage, lchem_split, laqueous, &
                   laerosol, ldump_chem, param_chem, &
                   rho_aer, nsec_aer, &
                  (secbnds_aer(i),i=1,nsec_aer+1), &
                  (ambient(i),i=1,nambient), &
                  (equilibrium(i),i=1,nequilibrium), &
                  (species(i),i=1,nspectot), &
                  (reaction(i),i=1,nreacts), &
                  (zenith(i),i=1,nzenith), &
                  (indxkr(i),i=1,nkrad), &
                  (ktmp(ij),ij=1,nzenith*nkrad), &
                  (indx_corr(i),i=1,ncorrt), &
                  (nreact_s(i),i=1,nstage), &
                  (voc_names(i),i=1,nvoc), &
                  (criteria(i),i=1,NCRITERIA), &
                  (sparam(i),i=1,nspectot), &
                  (i_spec_list(i),i=1,nspectot),&
                  (i_hostspec_list(i),i=1,nspectot),&
                  (i_eq_list(i),i=1,nequilibrium), &
                  (i_rel_list(i),i=1,nspectot)

  if (ios /= 0) go to 9999

else if (iversion >= 1900) then

  read(lun_prj,iostat=ios) nspectot,ncorrm,ncorrt,nreacts,&
                   nequilibrium,nambient,nstage,nvoc,&
                   nzenith,nkrad,i_units,ks_conv,kt_conv,rtol,&
                   ie_units, mc_units, isolve, lstep_amb,  &
                   amb_file, lstage, lchem_split, laqueous, &
                   laerosol, ldump_chem, param_chem, &
                   rho_aer, nsec_aer, &
                  (secbnds_aer(i),i=1,nsec_aer+1), &
                  (ambient(i),i=1,nambient), &
                  (equilibrium(i),i=1,nequilibrium), &
                  (species(i),i=1,nspectot), &
                  (reaction(i),i=1,nreacts), &
                  (zenith(i),i=1,nzenith), &
                  (indxkr(i),i=1,nkrad), &
                  (ktmp(ij),ij=1,nzenith*nkrad), &
                  (indx_corr(i),i=1,ncorrt), &
                  (nreact_s(i),i=1,nstage), &
                  (voc_names(i),i=1,nvoc), &
                  (criteria(i),i=1,NCRITERIA), &
                  (sparam(i),i=1,nspectot), &
                  (i_spec_list(i),i=1,nspectot),&
                  (i_eq_list(i),i=1,nequilibrium), &
                  (i_rel_list(i),i=1,nspectot)

  if (ios /= 0) go to 9999

else if (iversion >= 1220) then

  read(lun_prj,iostat=ios) nspectot,ncorrm,ncorrt,nreacts,&
                   nequilibrium,nambient,nstage,nvoc,&
                   nzenith,nkrad,i_units,ks_conv,kt_conv,rtol,&
                   ie_units, mc_units, isolve, lstep_amb,  &
                   amb_file, lstage, lchem_split, laqueous, &
                   laerosol, ldump_chem, param_chem, &
                   rho_aer, nsec_aer, &
                  (secbnds_aer(i),i=1,nsec_aer+1), &
                  (ambient(i),i=1,nambient), &
                  (equilibrium(i),i=1,nequilibrium), &
                  (species(i),i=1,nspectot), &
                  (reaction(i),i=1,nreacts), &
                  (zenith(i),i=1,nzenith), &
                  (indxkr(i),i=1,nkrad), &
                  (ktmp(ij),ij=1,nzenith*nkrad), &
                  (indx_corr(i),i=1,ncorrt), &
                  (nreact_s(i),i=1,nstage), &
                  (voc_names(i),i=1,nvoc), &
                  (criteria(i),i=1,NCRITERIA), &
                  (sparam(i),i=1,nspectot), &
                  (i_spec_list(i),i=1,nspectot),&
                  (i_eq_list(i),i=1,nequilibrium)

  if (ios /= 0) go to 9999

else if (iversion >= 1218) then

  read(lun_prj,iostat=ios) nspectot,ncorrm,ncorrt,nreacts,&
                   nequilibrium,nambient,nstage,nvoc,&
                   nzenith,nkrad,i_units,ks_conv,kt_conv,rtol,&
                   ie_units, mc_units, isolve, lstep_amb,  &
                   amb_file, lstage, lchem_split, laqueous, &
                   laerosol, rho_aer, nsec_aer, &
                  (secbnds_aer(i),i=1,nsec_aer+1), &
                  (ambient(i),i=1,nambient), &
                  (equilibrium(i),i=1,nequilibrium), &
                  (species(i),i=1,nspectot), &
                  (reaction(i),i=1,nreacts), &
                  (zenith(i),i=1,nzenith), &
                  (indxkr(i),i=1,nkrad), &
                  (ktmp(ij),ij=1,nzenith*nkrad), &
                  (indx_corr(i),i=1,ncorrt), &
                  (nreact_s(i),i=1,nstage), &
                  (voc_names(i),i=1,nvoc), &
                  (criteria(i),i=1,NCRITERIA), &
                  (sparam(i),i=1,nspectot), &
                  (i_spec_list(i),i=1,nspectot),&
                  (i_eq_list(i),i=1,nequilibrium)

  ldump_chem = .false.
  param_chem = 0.01
  
  if (ios /= 0) go to 9999

else if (iversion >= 1214) then

  read(lun_prj,iostat=ios) nspectot,ncorrm,ncorrt,nreacts,&
                   nequilibrium,nambient,nstage,nvoc,&
                   nzenith,nkrad,i_units,ks_conv,kt_conv,rtol,&
                   ie_units, mc_units, lstep_amb, amb_file, &
                   lstage, lsolve_ynb, lchem_split,laqueous, &
                   laerosol, rho_aer, nsec_aer, &
                  (secbnds_aer(i),i=1,nsec_aer+1), &
                  (ambient(i),i=1,nambient), &
                  (equilibrium(i),i=1,nequilibrium), &
                  (species(i),i=1,nspectot), &
                  (reaction(i),i=1,nreacts), &
                  (zenith(i),i=1,nzenith), &
                  (indxkr(i),i=1,nkrad), &
                  (ktmp(ij),ij=1,nzenith*nkrad), &
                  (indx_corr(i),i=1,ncorrt), &
                  (nreact_s(i),i=1,nstage), &
                  (voc_names(i),i=1,nvoc), &
                  (criteria(i),i=1,NCRITERIA), &
                  (sparam(i),i=1,nspectot), &
                  (i_spec_list(i),i=1,nspectot),&
                  (i_eq_list(i),i=1,nequilibrium)

  ldump_chem = .false.
  param_chem = 0.01

  if (lsolve_ynb) then
    isolve = ID_YNB
  else
    isolve = ID_LSODE
  end if  

  if (ios /= 0) go to 9999

else if (iversion >= 1211) then

  read(lun_prj,iostat=ios)nspectot,ncorrm,ncorrt,nreacts,&
                   nequilibrium,nambient,nstage,nvoc,&
                   nzenith,nkrad,i_units,ks_conv,kt_conv,rtol,&
                   ie_units, mc_units, lstep_amb, amb_file, &
                   lstage, lsolve_ynb, lchem_split,laqueous, &
                   laerosol, rho_aer, nsec_aer, &
                  (secbnds_aer(i),i=1,nsec_aer+1), &
                  (ambient(i),i=1,nambient), &
                  (equilibrium(i),i=1,nequilibrium), &
                  (species(i),i=1,nspectot), &
                  (reaction(i),i=1,nreacts), &
                  (zenith(i),i=1,nzenith), &
                  (indxkr(i),i=1,nkrad), &
                  (ktmp(ij),ij=1,nzenith*nkrad), &
                  (indx_corr(i),i=1,ncorrt), &
                  (nreact_s(i),i=1,nstage), &
                  (voc_names(i),i=1,nvoc), &
                  (criteria(i),i=1,NCRITERIA), &
                  (sparam(i),i=1,nspectot), &
                  (i_spec_list(i),i=1,nspectot - nequilibrium),&
                  (i_eq_list(i),i=1,nequilibrium)

  ldump_chem = .false.
  param_chem = 0.01

  if (lsolve_ynb) then
    isolve = ID_YNB
  else
    isolve = ID_LSODE
  end if  

  if (ios /= 0) go to 9999

else if (iversion >= 1203) then

  read(lun_prj,iostat=ios) nspectot,ncorrm,ncorrt,nreacts,&
                   nequilibrium,nambient,nstage,nvoc,&
                   nzenith,nkrad,i_units,ks_conv,kt_conv,rtol,&
                   mc_units, lstep_amb, amb_file, &
                   lstage, lsolve_ynb, lchem_split, &
                  (ambient(i),i=1,nambient), &
                  (equilibrium(i),i=1,nequilibrium), &
                  (species(i),i=1,nspectot), &
                  (reaction(i),i=1,nreacts), &
                  (zenith(i),i=1,nzenith), &
                  (indxkr(i),i=1,nkrad), &
                  (ktmp(ij),ij=1,nzenith*nkrad), &
                  (indx_corr(i),i=1,ncorrt), &
                  (nreact_s(i),i=1,nstage), &
                  (voc_names(i),i=1,nvoc), &
                  (criteria(i),i=1,NCRITERIA), &
                  (i_spec_list(i),i=1,nspectot - nequilibrium),&
                  (i_eq_list(i),i=1,nequilibrium)

  ldump_chem = .false.
  param_chem = 0.01

  if (lsolve_ynb) then
    isolve = ID_YNB
  else
    isolve = ID_LSODE
  end if  

  if (ios /= 0) go to 9999

else if (iversion >= 1202) then

  read(lun_prj,iostat=ios) nspectot,ncorrm,ncorrt,nreacts,&
                   nequilibrium,nambient,nstage,nvoc,&
                   nzenith,nkrad,i_units,ks_conv,kt_conv,rtol,&
                   mc_units, lstep_amb, amb_file, &
                   lstage, lsolve_ynb, lchem_split, &
                  (ambient(i),i=1,nambient), &
                  (equilibrium(i),i=1,nequilibrium), &
                  (species(i),i=1,nspectot), &
                  (reaction(i),i=1,nreacts), &
                  (zenith(i),i=1,nzenith), &
                  (indxkr(i),i=1,nkrad), &
                  (ktmp(ij),ij=1,nzenith*nkrad), &
                  (indx_corr(i),i=1,ncorrt), &
                  (nreact_s(i),i=1,nstage), &
                  (voc_names(i),i=1,nvoc), &
                  (criteria(i),i=1,NCRITERIA)

  if (lsolve_ynb) then
    isolve = ID_YNB
  else
    isolve = ID_LSODE
  end if  
  ldump_chem = .false.
  param_chem = 0.01

  if (ios /= 0) go to 9999

else if (iversion >= 1201) then

  read(lun_prj,iostat=ios) nspectot,ncorrm,ncorrt,nreacts,&
                   nequilibrium,nambient,nstage,nvoc,&
                   nzenith,nkrad,i_units,ks_conv,kt_conv,rtol,&
                   mc_units, lstep_amb, amb_file, &
                   lstage, lsolve_ynb, &
                  (ambient(i),i=1,nambient), &
                  (equilibrium(i),i=1,nequilibrium), &
                  (species(i),i=1,nspectot), &
                  (reaction(i),i=1,nreacts), &
                  (zenith(i),i=1,nzenith), &
                  (indxkr(i),i=1,nkrad), &
                  (ktmp(ij),ij=1,nzenith*nkrad), &
                  (indx_corr(i),i=1,ncorrt), &
                  (nreact_s(i),i=1,nstage), &
                  (voc_names(i),i=1,nvoc), &
                  (criteria(i),i=1,NCRITERIA)

  lchem_split = .false.
  ldump_chem = .false.
  param_chem = 0.01

  if (lsolve_ynb) then
    isolve = ID_YNB
  else
    isolve = ID_LSODE
  end if  

  if (ios /= 0) go to 9999

else if (iversion >= 1104) then

  read(lun_prj,iostat=ios) nspectot,ncorrm,ncorrt,nreacts,&
                   nequilibrium,nambient,nstage,nvoc,&
                   nzenith,nkrad,i_units,ks_conv,kt_conv,rtol,&
                   mc_units, lstep_amb, amb_file, lstage, &
                  (ambient(i),i=1,nambient), &
                  (equilibrium(i),i=1,nequilibrium), &
                  (species(i),i=1,nspectot), &
                  (reaction(i),i=1,nreacts), &
                  (zenith(i),i=1,nzenith), &
                  (indxkr(i),i=1,nkrad), &
                  (ktmp(ij),ij=1,nzenith*nkrad), &
                  (indx_corr(i),i=1,ncorrt), &
                  (nreact_s(i),i=1,nstage), &
                  (voc_names(i),i=1,nvoc), &
                  (criteria(i),i=1,NCRITERIA)

  lchem_split = .false.
  isolve = ID_LSODE
  ldump_chem = .false.
  param_chem = 0.01
  
  if (ios /= 0) go to 9999

else if (iversion >= 1103) then

  read(lun_prj,iostat=ios) nspectot, ncorrm,  ncorrt, nreacts, &
                  nequilibrium, nambient, nstage, &
                  nzenith,  nkrad, i_units, lstep_amb, &
                  ks_conv, kt_conv, rtol, &
                  (ambient(i),i=1,nambient), &
                  (equilibrium(i),i=1,nequilibrium), &
                  (species(i),i=1,nspectot), &
                  (reaction(i),i=1,nreacts), &
                  (zenith(i),i=1,nzenith), &
                  (indxkr(i),i=1,nkrad), &
                  (ktmp(ij),ij=1,nzenith*nkrad), &
                  mc_units,(indx_corr(i),i=1,ncorrt), &
                  amb_file, lstage,(nreact_s(i),i=1,nstage), &
                  (criteria(i),i=1,NCRITERIA)

  lchem_split = .false.
  isolve = ID_LSODE
  ldump_chem = .false.
  param_chem = 0.01

! --- Note:  this verion had an error for staged chemistry
!            and did not write the voc names properly.

  nvoc = -1
  do i = 1, MAX_VOC
    voc_names(i) = TRIM(NOT_SET_C)
  end do

  if (ios /= 0) go to 9999

else if (iversion >= 1102) then

  read(lun_prj,iostat=ios) nspectot, ncorrm,  ncorrt, nreacts, &
                              nslow, nfast, nequilibrium, nambient, &
                              nzenith,  nkrad, i_units, lstep_amb, &
                              ks_conv, kt_conv, rtol, &
                              (ambient(i),i=1,nambient), &
                              (equilibrium(i),i=1,nequilibrium), &
                              (species(i),i=1,nspectot), &
                              (indxf_s(i,1),i=1,nfast), &
                              (indxs_s(i,1),i=1,nslow), &
                              (reaction(i),i=1,nreacts), &
                              (zenith(i),i=1,nzenith), &
                              (indxkr(i),i=1,nkrad), &
                              (ktmp(ij),ij=1,nzenith*nkrad), &
                              mc_units, &
                              (indx_corr(i),i=1,ncorrt), &
                              amb_file

  lchem_split = .false.
  isolve = ID_LSODE
  ldump_chem = .false.
  param_chem = 0.01

  nvoc   = 0
  nstage = 1
  istage = 1
  lstage = .false.
  nreact_s(nstage) = nreacts

  do i = 1, NCRITERIA
    criteria(i) = NOT_SET_R
  end do
  do i = 1, MAX_VOC
    voc_names(i) = TRIM(NOT_SET_C)
  end do

  if (ios /= 0) go to 9999

else if (iversion >= 700) then

  read(lun_prj,iostat=ios) nspectot, ncorrm,  ncorrt, nreacts, &
                              nslow, nfast, nequilibrium, nambient, &
                              nzenith,  nkrad, i_units, lstep_amb, &
                              ks_conv, kt_conv, rtol, &
                              (ambient(i),i=1,nambient), &
                              (equilibrium(i),i=1,nequilibrium), &
                              (species(i),i=1,nspectot), &
                              (indxf(i),i=1,nfast), &
                              (indxs(i),i=1,nslow), &
                              (reaction(i),i=1,nreacts), &
                              (zenith(i),i=1,nzenith), &
                              (indxkr(i),i=1,nkrad), &
                              (ktmp(ij),ij=1,nzenith*nkrad), &
                              mc_units, &
                              (indx_corr(i),i=1,ncorrt)

  lchem_split = .false.
  isolve = ID_LSODE

  nstage = 1
  istage = 1
  lstage = .false.
  nreact_s(nstage) = nreacts

  do i = 1, NCRITERIA
    criteria(i) = NOT_SET_R
  end do
  do i = 1, MAX_VOC
    voc_names(i) = TRIM(NOT_SET_C)
  end do
  ldump_chem = .false.
  param_chem = 0.01

  if (ios /= 0) go to 9999

else

  nError   = RD_ERROR
  eRoutine = 'read_prj_ext'
  eMessage = 'Unable to read old style multicomponent files'
  eInform  = 'File='//TRIM(file_prj)

end if

lamb3d   = TRIM(amb_file) /= TRIM(NOT_SET_C)
nspecies = nspectot - nequilibrium

! -- fix for SUN, copy ktable to tmp array

do i = 1, nzenith
  do j = 1, nkrad
    ij = (i-1)*nkrad + j
    ktable(i,j) =ktmp(ij)
  end do
end do

9998    return

9999    nError   = RD_ERROR
        eRoutine = 'read_prj_mc'
        eMessage = 'Error reading project file'
        eInform  = 'File='//TRIM(file_prj)
        go to 9998

end

subroutine write_prj_xmap
!******************************************************************************
!
! FUNCTION:   Write the map factors and roughness heights to the project file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc

implicit none
 
! --- LOCALS

integer ios, i

write(lun_prj,iostat=ios) (xmap2d(i),i=1,nxyb),(ymap2d(i),i=1,nxyb), &
                          (zruf2(i),i=1,nxyb)

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_xmap'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

return

end

subroutine write_prj_latlon
!******************************************************************************
!
! FUNCTION:   Write the latitudes and longitudes to the project file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc

implicit none

! --- LOCALS

integer ios, i

write(lun_prj,iostat=ios) (lat2d(i),i=1,nxyb),(lon2d(i),i=1,nxyb)

if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_prj_latlon'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

return

end

subroutine write_prj_zb
!******************************************************************************
!
! FUNCTION:  Write the vertical grid to the project file
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc

implicit none

! --- LOCALS

integer ios, i

write(lun_prj,iostat=ios) nzb,(zb(i),i=1,nzb),(zbw(i),i=1,nzb)

if (ios /= 0) then
  nError   = RD_ERROR
  eRoutine = 'write_prj_zb'
  eMessage = 'Error writing project file'
  eInform  = 'File='//TRIM(file_prj)
end if

return

end

subroutine set_topog_gridded(i)
!******************************************************************************
!
! FUNCTION:  Set-up for gridded topography
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 build_d               write_prj
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_grd
use files_inc

implicit none

! --- ARGUMENTS
 
integer i  !Not used

!------ Define relative depth arrays and gradients

call build_d

if (.not.lter_prj) then
  lter_prj = .true.
  call write_prj
  if (nError /= NO_ERROR) go to 9999
end if

9999  return

end

subroutine read_domain(lun_inp)
!******************************************************************************
!
! FUNCTION:   Dummy routine for PiG version
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
implicit none

! --- ARGUMENTS

integer lun_inp  !Input file

!Pig version does not read domain information from input files

return

end

subroutine start_met_grid(n)
!******************************************************************************
!
! FUNCTION:  Dummy routine for PiG version
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
implicit none

! --- ARGUMENTS
 
integer n

!Nothing is done here for PiG version

return
end

subroutine get_matdef
!******************************************************************************
!
! FUNCTION:  Get the material definition
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             read_matdef             check_units                 IsMulti
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc

implicit none

! --- LOCALS

integer nmat_mc
logical lok, IsMulti

character*16 mname

mname = 'ALL '

nError = NO_ERROR
nmat_mc = 0
!do while (nError /= EOF_ERROR)
  if (nError /= NO_ERROR) go to 9997
  call read_matdef(mname,.false.,lun_inp,ntypm,material, &
                                                   nmaux,mat_aux,lok)
  if (nError == NO_ERROR) then
    if(IsMulti(material(ntypm)%icls)) nmat_mc = nmat_mc + 1
  end if
!end do  !only one material in PiG

if (nmat_mc > 1) then
  nError   = UK_ERROR
  eMessage = 'Too many multicomponent materials'
  eInform  = 'Limit is one material type'
  go to 9997
else
  multicomp = multicomp .and. (nmat_mc > 0)
  nError = NO_ERROR
end if

if (ntypm <= 0) then
  nError   = UK_ERROR
  eMessage = 'No material definitions found'
  go to 9997
else
  call check_units(ntypm,material,mat_aux,dense_gas)
  if(nError /= NO_ERROR)go to 9999
end if

9999 return

9997    continue
eRoutine = 'get_matdef'
if (eInform == char(0)) eInform  = 'File='//TRIM(file_inp)
go to 9999

end

subroutine c_init_rel(icrel,jrpuf)
!******************************************************************************
!
! FUNCTION:  Initialize a continuous release
!
!    Normal call to initialize continuous release    - icrel=jrpuf=0
!    Normal call to re-initialize continuous release - icrel=0,jrpuf<0
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  mapfac                  c_init
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc

implicit none

! --- ARGUMENTS
 
integer icrel  !Release number
integer jrpuf  !Puff number

! --- LOCALS

integer  ios, nch, nblank

real xrel0, yrel0, zrel0, trel0, tdur0, urel0, vrel0, wrel0
real xmap, ymap, tmov

call mapfac(xrel,yrel,xmap,ymap)
tmov = max(t-trel,0.0)              !Restarting moving sources
xrel = xrel + tmov*urel*xmap
yrel = yrel + tmov*vrel*ymap
zrel = zrel + tmov*wrel
call c_init(icrel,jrpuf)
call mapfac(xrel,yrel,xmap,ymap)

9999 return

end

subroutine c_release( istep, dts , lev1 , lev2 , cgrid )
!******************************************************************************
!
! FUNCTION: 
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                copy_relupdate_static_rel_pointe     remove_static_puffs
!                set_cc_r       init_static_puffs           check_newpuff
!              c_set_puff                  mapfac              get_static
!                  set_ip                add_tlev                 IsMulti
!
!
! REVISION HISTORY: 
!
!    Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc
use cont_rel_inc
use interface_definitions, only: INIT_STATIC_PUFFS

implicit none

! --- ARGUMENTS
 
integer istep        !Current step number
real    dts          !Current timestep
integer lev1, lev2   !Range of time levels
REAL :: CGRID( :,:,:,: )   !3D ambient concentrations from the host model

! --- LOCALS

type ( puff_static ) ps

integer  i, ird, iwrt, ipuf, naux, icls, jpuf
integer  mpuf, ios, nstp, istp, ityp, iout

real dt, dtr, t_init, ur, vr, wr, xmap, ymap

logical ltot, lmc, ldeactivate, IsMulti

!------------

if (ncrel <= 0) go to 9999

!------ deactivate defunct releases

iwrt  = 0
ncaux = 1
do ird = 1,ncrel
  if(c_saux(ird) > 0)then
    ldeactivate = src_aux(c_saux(ird)) <= 0.0
  else
    dt   = c_end(ird) - c_start(ird)
!   ldeactivate = .not.(t+dts < c_end(ird)
    ldeactivate = .not.(dt > 0.001*dts &
                               .or. c_lev(ird) < lev1)
  end if
  if(.not.ldeactivate)then
    iwrt = iwrt + 1
    iout = MAXPUF + 2*(1 - iwrt)
    ipuf = MAXPUF + 2*(1 - ird)
    if (iwrt /= ird)then
      c_start(iwrt) = c_start(ird)
      c_end(iwrt)   = c_end(ird)
      c_plen(iwrt)  = c_plen(ird)
      c_opid(iwrt)  = c_opid(ird)
      c_saux(iwrt)  = c_saux(ird)
      c_lev(iwrt)   = c_lev(ird)
      c_dt(iwrt)    = c_dt(ird)
      c_dtr(iwrt)   = c_dtr(ird)
      c_stat(iwrt)  = c_stat(ird)
      call copy_rel(puff(ipuf),puff(iout),ncaux)
      call copy_rel(puff(ipuf-1),puff(iout-1),ncaux)
      call update_static_rel_pointers(iout-1,iwrt)
    else
      ncaux = ncaux + 2*typeID(puff(ipuf)%ityp)%npaux
    end if
  else
    call remove_static_puffs(ird)
    write(lun_log,111,iostat=ios)'Release deactivated at T = ', &
                 t/3600.,' with ncrel = ',ncrel
111         format(' ',a,f7.2,a,i4)
    if (ios /= 0) then
      nError   = WR_ERROR
      eRoutine = 'c_release'
      eMessage = 'Error writing SCICHEM log file'
      eInform  = 'File='//TRIM(file_log)
      go to 9999
    end if
  end if
end do

lrupdate = (ncrel /= iwrt) .or. lrupdate

ncrel = iwrt

if(istep == 1)then
  if(lrupdate)then
    call set_cc_r
  end if
end if

mpuf = npuf

do i = 1,ncrel

  ipuf = MAXPUF + 2*(1 - i) - 1

  if (t+0.5*dts > c_start(i) .and. puff(ipuf)%idtl >= lev1) then

!---------- Set release timestep

    if(c_saux(i) == 0)then
      dt   = min(t+dts,c_end(i)) - c_start(i)
      nstp = int(dt/c_dtr(i)) + 1
      dt   = dt/float(nstp)
    else
      dt   = t+dts - c_start(i)
      nstp = 1
    end if

    if (puff(ipuf)%c == NOT_SET_R) then
      t_init = float(istep-1)*dts
      call init_static_puffs(i,t_init,dt,cgrid)
      if (nError /= NO_ERROR) go to 9999
      if (puff(ipuf)%idtl < lev1) go to 100
    end if

    ityp = puff(ipuf)%ityp
    ltot = typeID(ityp)%ltot
    naux = typeID(ityp)%npaux
    icls = typeID(ityp)%icls
    lmc  = IsMulti(icls)

    do istp = 1,nstp

      call check_newpuff(naux)
      if (nError /= NO_ERROR)then
         eRoutine = 'c_release'
         go to 9999
      end if

      npuf  = npuf + 1

      dtr = float(nstp-istp)*dt
      call c_set_puff(icls,ltot,lmc,i,ipuf,dt,dtr,lev1,lev2)
      if (nError /= NO_ERROR) go to 9999

    end do

!---------- Move static puffs, if necessary

    if (puff(ipuf)%inxt > 0) then
      ur = puff(ipuf)%uo
      vr = puff(ipuf)%vo
      wr = puff(ipuf)%wo

      if ( ur*ur + vr*vr + wr*wr > 0.0 ) then
        dt = dt*float(nstp)
        call mapfac(puff(ipuf)%xbar,puff(ipuf)%ybar,xmap,ymap)
        jpuf = puff(ipuf)%inxt
        do while (jpuf > 0)
          puff(jpuf)%xbar = puff(jpuf)%xbar + ur*dt*xmap
          puff(jpuf)%ybar = puff(jpuf)%ybar + vr*dt*ymap
          puff(jpuf)%zbar = puff(jpuf)%zbar + wr*dt
          call get_static(puff(jpuf),ps)
          jpuf = ps%isnxt
        end do
      end if
    end if

    c_lev(i)   = puff(ipuf)%idtl
    c_start(i) = t + dts

  end if

100       continue

end do

if (mpuf < npuf) then

  call set_ip(mpuf+1,npuf)
  if (nError /= NO_ERROR) go to 9999

  call add_tlev(mpuf+1,npuf)
  if (nError /= NO_ERROR) go to 9999

end if

9999  return

end

subroutine set_c_plen(c_plen,ncrel)
!******************************************************************************
!
! FUNCTION:   Set the plume length scale
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!  Updated Feb. 2004 for consistency with stand-alone SCICHEM V1601 (PKK, AER)
!
!******************************************************************************
 
! --- MODULES
 
use default_inc

implicit none

! --- ARGUMENTS
 
real c_plen(*)  !Length scale
integer ncrel   !Number of releases

c_plen(ncrel+1) = 1.e+10 !Special for pig - always a plume

return
end

subroutine puff_release( JDATE, JTIME, MSTEP )
!******************************************************************************
!
! FUNCTION:  Releases all puffs at time t
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 set_scn                 get_scn              update_scn
!               check_scn              c_init_rel                set_cc_r
!
!
! REVISION HISTORY: 
!
!  Updated Feb. 2004 for consistency with stand-alone SCICHEM V1601 (PKK, AER)
!  Minor F90 Updates, Feb. 2004 (PKK, AER)
!  Updated April 2005 for CMAQ-APT-PM
!  Updated August 2011 for CMAQ 5.0 Beta (PKK, ENVIRON)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc
use emissions_inc
use multcomp_inc
use constants_fd, only: PI

! CMAQ modules
use precursor_data, only: N_PRECURSOR, precursor_mw, sulf_idx
USE AERO_DATA, only: N_MODE

implicit none

! --- ARGUMENTS

integer         JDATE     !  current model date, coded YYYYDDD
integer         JTIME     !  current model time, coded HHMMSS
integer         MSTEP     !  model time step,  format HHMMSS
 
! --- LOCALS

integer mpuf, mcrel, jrpuf, msrcaux
integer isrc    !current source
integer i, j, spc
real    t_last

integer :: imode
logical, save :: firstime = .TRUE.

! From CMAQ, AERO_EMIS.F:
! Geometric mean diameter by volume (or mass) of emitted particles in
! each mode [ m ].  See paragraph #14 of Binkowski & Roselle (2003)
real, parameter :: DGVEM( N_MODE ) = (/ 0.03E-6, 0.3E-6, 6.0E-6 /)

! Geometric standard deviation of emitted particles in each mode, as
! described in paragraph #14 of Binkowski & Roselle (2003)
real, parameter :: SGEM( N_MODE ) = (/ 1.7, 2.0, 2.2 /)

! Geometric Constants
real, parameter :: F6DPI = 6.0 / PI
real, parameter :: F6DPIM3 = 1.0E-3 * F6DPI   ! 1.0E-3 = Kg/g

! Variables for converting mass emissions rate to number emissions rate
real, save :: facnum( N_MODE )

! Variables for converting mass emissions rate to 2nd moment emissions rate
real, save :: facsrf( N_MODE )

! number and surface area emissions
real, dimension( N_MODE ) :: numemis, srfemis

! particle density
real :: density

! From CMAQ, AERO_EMIS.F
if (firstime) then
! *** Calculate factors for converting 3rd moment emission rates into number
!     emission rates.  See Equation 7b of Binkowski & Roselle (2003)
  do imode = 1, N_MODE
    facnum(imode) = EXP(4.5 * LOG(SGEM(imode))**2) / DGVEM(imode)**3
  end do

! *** Calculate factors for converting 3rd moment emission rates into 2nd
!     moment emission rates.  See Equation 7c of Binkowski & Roselle (2003)
  do imode = 1, N_MODE
    facsrf(imode) = PI * EXP(0.5 * LOG(SGEM(imode))**2) / DGVEM(imode)
  end do

  firstime = .FALSE.
end if

!debug
!write(*,*)'vsulf,vpso4: ',vsulf,vpso4
!debug
mpuf  = npuf
mcrel = ncrel
msrcaux = nsrcaux
t_last = t_old_r

if (trel == DEF_VAL_R) then
  call set_scn
  if (nError /= NO_ERROR) go to 9999
  ncrel = 0
  nsrcaux = 0

  trel = tinit_emit

end if

jrpuf = 0

!debug
!write(*,*)'trel,t+delt: ',trel,t+delt
!write(*,*)'jdate,jtime,mstep: ',jdate,jtime,mstep
!debug
if (trel < t+delt) then
!debug
!write(*,*)'vsulf,vpso4: ',vsulf,vpso4
!debug
  call get_scn( JDATE, JTIME, MSTEP )
!debug
!write(*,*)'vsulf,vpso4: ',vsulf,vpso4
!debug
  if (nError /= NO_ERROR) go to 9999
  do isrc = 1,nsrc

! check if this source needs to be treated as a PinG source or not
    if (ping_src(isrc) == 0) then
      write(6,*)'skipping source: ',isrc
      cycle
    end if

    write(6,1000) 'get_emissions:',t,isrc,trel

    sigx     = 0.
    sigy     = 0.
    sigz     = 0.

    xrel = xsrc(isrc)
    yrel = ysrc(isrc)
    zrel = zsrc(isrc)
    size_rel = dsrc(isrc)
    buoy = tsrc(isrc) - 273.15
    wmom = wsrc(isrc)

    name_prime = bfile_src(isrc)
!debug
!    write(*,*)'isrc: ',isrc
!    write(*,*)'xrel,yrel,zel: ',xrel,yrel,zrel
!    write(*,*)'size_rel,buoy,wmom: ',size_rel,buoy,wmom
!    write(*,*)'name_prime: ',name_prime
!debug

    if ((len_trim(name_prime)>0) .or. (.not.dynamic)) then
      reltyp = 'CSPR'
    else 
      reltyp = 'CS'
    end if

    rel_mc = 0.

!debug
!    write(*,*)'reltyp: ',reltyp
!    write(*,*)'vsulf,vpso4: ',vsulf,vpso4
!debug
! *** Following CMAQ, extract H2SO4 vapor emission rate and add it to
!     the fine-PM sulfate emissions, and remove it from the gas emissions
!debug
!    write(*,*)'Adding ',VSULF,' emissions = ',qsrc(isrc,VSULF), &
!              ' to ',VPSO4,' emissions = ',qsrc(isrc,VPSO4)
!debug
    qsrc(isrc,VPSO4) = qsrc(isrc,VPSO4) + qsrc(isrc,VSULF) * &
                       PRECURSOR_MW( SULF_IDX )
    qsrc(isrc,VSULF) = 0.
!debug
!    write(*,*)'After adding ',VSULF,' emissions = ',qsrc(isrc,VSULF), &
!              ' and ',VPSO4,' emissions = ',qsrc(isrc,VPSO4)
!debug

! Gas and particle mass emissions
    do j = 1, nspecies
      i = index_emit(j)
      if (i == 0) cycle
!debug
!      write(*,*)'assigning ',species(j)%emis_split,' of ',nameemit(i), &
!                ' emissions to ', &
!                species(j)%name,'; species type: ',species(j)%class
!debug
      if ( species(j)%class /= ID_SPECIES_PARTICLE )then
        rel_mc(j) = qsrc(isrc,i)*sparam(j)%mwt*species(j)%emis_split !gas (g/s)
      else
        rel_mc(j) = qsrc(isrc,i)*species(j)%emis_split !particle (g/s)
      end if
    end do

! Do particle number and surface emissions
    numemis = 0.
    srfemis = 0.
    do j = 1, nspecies
      if (species(j)%class /= ID_SPECIES_PARTICLE) CYCLE
      if (rel_mc(j) == 0.) CYCLE
      imode = species(j)%pmode
      density = species(j)%density
      if (density == 0) then
        nError = IV_ERROR
        eMessage = 'Error: species '//species(j)%name//' density is undefined'
        eRoutine = 'puff_release'
        go to 9999
      end if

      numemis(imode) = numemis(imode) + rel_mc(j)/density
      srfemis(imode) = srfemis(imode) + rel_mc(j)/density
    end do

!debug
!    write(*,*)'numemis: ',numemis
!    write(*,*)'srfemis: ',srfemis
!debug
    numemis = F6DPIM3*numemis*facnum  ! in #/s
    srfemis = F6DPIM3*srfemis*facsrf  ! in m2/s
!debug
!    write(*,*)'facnum,facsrf: ',facnum,facsrf
!    write(*,*)'numemis: ',numemis
!    write(*,*)'srfemis: ',srfemis
!debug

    do spc = 1, nspecies
      if (species(spc)%class == ID_SPECIES_NUMBER) then
!debug
!        write(*,*)'species: ',species(spc)%name
!        write(*,*)'class: ',species(spc)%class
!        write(*,*)'mode: ',species(spc)%pmode
!debug 
        rel_mc(spc) = numemis(species(spc)%pmode)
      else if (species(spc)%class == ID_SPECIES_SURFACE) then
!debug
!        write(*,*)'species: ',spc,species(spc)%name
!        write(*,*)'class: ',species(spc)%class
!        write(*,*)'mode: ',species(spc)%pmode
!        write(*,*)'emissions: ',srfemis(species(spc)%pmode)
!debug 
        rel_mc(spc) = srfemis(species(spc)%pmode)
      end if
    end do

    do i = 1, nspecies
      if (rel_mc(i) /= 0.) then
         write(6,*) species(i)%name,rel_mc(i)
      end if
    end do

    call update_scn
    if (nError /= NO_ERROR) go to 9999
    call check_scn(ntypm,material,mat_aux)
    if (nError /= NO_ERROR) go to 9999

    call c_init_rel(0,jrpuf)
    if (nError /= NO_ERROR) go to 9999
    if(restart .and. jrpuf < 0 .and. ncrel > mcrel)then
      nError = IV_ERROR
      eMessage = 'Error updating source data'
      eInform  = 'Continuous sources do not match restart data'
      eRoutine = 'puff_release'
      go to 9999
    end if
    if(restart .and. jrpuf < 0 .and. nsrcaux > msrcaux)then
      nError = IV_ERROR
      eMessage = 'Error updating auxiliary source data'
      eInform  = 'Tried to read beyond end of aux. array'
      eRoutine = 'puff_release'
      go to 9999
    end if

  end do

  trel = trel + tdur

end if

1000    format(a16,f12.3,2x,(2x,i4),2(2x,f12.3))

t_old_r = t_last

if(restart .and. jrpuf < 0 .and. nsrcaux < msrcaux)then
  nError = RD_ERROR
  eMessage = 'Failed to read all Aux. source data'
  eRoutine = 'puff_release'
  go to  9999
end if
if(restart .and. jrpuf < 0 .and. ncrel < mcrel)then
  nError = RD_ERROR
  eMessage = 'Failed to read all continuous source data'
  eRoutine = 'puff_release'
  go to  9999
end if

if (ncrel > mcrel) then
  call set_cc_r
  if (nError /= NO_ERROR) go to 9999
end if

if (nError /= NO_ERROR) go to 9999

9999 return

end

subroutine set_species_lists
!******************************************************************************
!
! FUNCTION:  Set-up pointer to host model species numbers
!            and set molecular weights; also set-up pointers for host
!            model emitted species
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!  Updated April 2005 for CMAQ-APT-PM (PKK, AER)
!  Updated for CMAQ 5.0 Beta, August 2011 (PK, ENVIRON)
!******************************************************************************
 
! --- MODULES
 
use multcomp_inc
use error_inc

! CMAQ modules
USE CGRID_SPCS
!, only: n_gc_spc, n_ae_spc, n_nr_spc, n_tr_spc, &
!                      gc_spc, ae_spc, nr_spc, tr_spc, & 
!                      gc_molwt, ae_molwt, nr_molwt, tr_molwt, & 
!                      gc_strt, ae_strt, nr_strt, tr_strt
USE AERO_DATA, only: N_AEROSPC, aerospc, N_MODE, aeromode, &
                     n_emis_pm, pmem_map, &
                     aso4_idx, ano3_idx, acl_idx, ah2o_idx, acors_idx
USE AERO_EMIS, only: SPFC_ACORS, SPFC_ASO4, SPFC_ANO3, SPFC_ACL, SPFC_AH2O

implicit none

! --- LOCALS

! Loop variables
integer i, j, imode, spc

! Local variables for aerosol properties
character( 16 ) aeroname, emisname, num_name, srf_name
real density, emis_split

integer       strt                ! starting position
integer       fini                ! ending position

!!!! following are declared and calculated in CMAQ module AERO_EMIS, but are
!!!! private variables, so they are redeclared and recalculated here

!!!! *** PVB: temporary hack to speciate PMC
!!!!real :: spfc_acors, spfc_aso4, spfc_ano3, spfc_acl, spfc_ah2o

!!!!spfc_aso4  = 0.00100 * aerospc( aso4_idx )%emis_split( N_MODE )
!!!!spfc_ano3  = 0.00048 * aerospc( ano3_idx )%emis_split( N_MODE )
!!!!spfc_acl   = 0.00145 * aerospc( acl_idx )%emis_split( N_MODE )
!!!!spfc_ah2o  = 0.00032 * aerospc( ah2o_idx )%emis_split( N_MODE )
!!!!spfc_acors = ( 1.0 - 0.00325 ) * aerospc( acors_idx )%emis_split( N_MODE )

!debug
!write(*,*)'spfc_aso4,spfc_ano3,spfc_acl,spfc_ah2o,spfc_acors: ', &
!           spfc_aso4,spfc_ano3,spfc_acl,spfc_ah2o,spfc_acors
!debug

!debug
!write(*,*)'nspecies,nspectot: ',nspecies,nspectot
!write(*,*)'gc_strt,n_gc_spc: ',gc_strt,n_gc_spc
!write(*,*)'ae_strt,n_ae_spc: ',ae_strt,n_ae_spc
!write(*,*)'nr_strt,n_nr_spc: ',nr_strt,n_nr_spc
!debug
do i = 1, nspectot
  i_spec_list(i) = 0
  i_hostspec_list(i) = 0
end do
do i = 1, nequilibrium
  i_eq_list(i) = 0
end do

! --- Loop through SCICHEM species list
scichemloop: do j = 1, nspectot
  
! - Loop through host model reactive gases list
  strt = GC_STRT
  fini = GC_STRT - 1 + N_GC_SPC

  spc = 0

  do i = strt, fini

    spc = spc + 1
    if ( TRIM(species(j)%name) == TRIM(GC_SPC(spc)) ) then
      i_spec_list(j) = i
      i_hostspec_list(i) = j
      if (j > nspecies) i_eq_list(j-nspecies) = i
      sparam(j)%mwt  = GC_MOLWT(spc)
      CYCLE scichemloop
    end if

  end do

! - Loop through host model particle species list
  strt = AE_STRT
  fini = AE_STRT - 1 + N_AE_SPC

  spc = 0

  do i = strt, fini

    spc = spc + 1
    if ( TRIM(species(j)%name) == TRIM(AE_SPC(spc)) ) then
      i_spec_list(j) = i
      i_hostspec_list(i) = j
      sparam(j)%mwt  = AE_MOLWT(spc)
      CYCLE scichemloop
    end if

  end do

! - Loop through host model non-reactive species list
  strt = NR_STRT
  fini = NR_STRT - 1 + N_NR_SPC

  spc = 0

  do i = strt, fini

    spc = spc + 1
    if ( TRIM(species(j)%name) == TRIM(NR_SPC(spc)) ) then
      i_spec_list(j) = i
      i_hostspec_list(i) = j
      sparam(j)%mwt  = NR_MOLWT(spc)
      CYCLE scichemloop
    end if

  end do

! --- No match found
  nError = IV_ERROR
  eMessage = 'Failed to locate required species in host model list'
  eInform  = 'Missing species: '//TRIM(species(j)%name)
  eRoutine = 'set_species_list'
  go to  9999

end do scichemloop

!debug
!do i = 1, fini
!  write(*,*)'i,i_hostspec_list: ',i,i_hostspec_list(i)
!end do
!debug

write(6,*)'Species cross-reference list and molecular weights'
do i = 1, nspectot
  if ( i_spec_list(i) == 0) then
    nError = IV_ERROR
    eMessage = 'Extra species name in IMC file'
    eInform  = 'Extra species: '//TRIM(species(i)%name)
    eAction  = 'Incompatible species names'
    eRoutine = 'set_species_list'
    go to  9999
  end if
  write(6,*) species(i)%name,i_spec_list(i),sparam(i)%mwt
end do
if (nequilibrium > 0 ) then
  write(6,*)'Equilibrium species cross-reference list and molecular weights'
  do i = 1, nequilibrium
    if ( i_eq_list(i) == 0) then
      nError = IV_ERROR
      eMessage = 'Extra species name in IMC file'
      eInform  = 'Extra species: '//TRIM(equilibrium(i)%name)
      eAction  = 'Incompatible species names'
      eRoutine = 'set_species_list'
      go to  9999
    end if
    write(6,*) equilibrium(i)%name,i_eq_list(i),sparam(i+nspecies)%mwt
  end do
end if

! Assign emitted species names and emission distribution factors to 
! SCICHEM species
! Gas-phase species (reactive and non-reactive)
do i = 1, n_gc_emis
  if (gc_emis_map(i) > 0) then
    spc = gc_emis_map(i) + GC_STRT - 1
    species(i_hostspec_list(spc))%nameemit = gc_emis(i)
    species(i_hostspec_list(spc))%emis_split = gc_emis_fac(i)
!debug
!    write(*,*)'i,gc_emis_map(i),spc,i_hostspec_list,nameemit,emis_split: ', &
!               i,gc_emis_map(i),spc,i_hostspec_list(spc), &
!               species(i_hostspec_list(spc))%nameemit, &
!               species(i_hostspec_list(spc))%emis_split
!debug
  end if
end do

do i = 1, n_nr_emis
  if (nr_emis_map(i) > 0) then
    spc = nr_emis_map(i) + NR_STRT - 1
    species(i_hostspec_list(spc))%nameemit = nr_emis(i)
    species(i_hostspec_list(spc))%emis_split = nr_emis_fac(i)
!debug
!    write(*,*)'i,nr_emis_map(i),spc,i_hostspec_list,nameemit,emis_split: ', &
!               i,nr_emis_map(i),spc,i_hostspec_list(spc), &
!               species(i_hostspec_list(spc))%nameemit, &
!               species(i_hostspec_list(spc))%emis_split
!debug
  end if
end do

! Particle-phase species handled differently
do i = 1, n_ae_emis
  if(ae_emis_map(i) > 0)then
    spc = ae_emis_map(i) + AE_STRT - 1
    species(i_hostspec_list(spc))%emis_split = ae_emis_fac(i) 
  end if
end do

do j = 1, n_emis_pm
  i = pmem_map(j)
  emisname = aerospc(i)%emis
  density = aerospc(i)%density
  do imode = 1, N_MODE
    aeroname = aerospc(i)%name(imode)

! skip if this mode is not valid for this emitted species
    if ( LEN_TRIM(aeroname) == 0 ) then

!debug
!    write(*,*)'skipping mode ',imode,' for ',emisname
!debug
      cycle
    end if
    emis_split = aerospc(i)%emis_split(imode)
! Assign to SCICHEM species (skip if not particle species)
    do spc = 1, nspecies
      if ( species(spc)%class /= ID_SPECIES_PARTICLE ) cycle
      if ( TRIM(species(spc)%name) == aeroname ) then
        species(spc)%nameemit = emisname
        species(spc)%pmode = imode
        species(spc)%density = density
        if (i == aso4_idx .and. imode == N_MODE) then
          species(spc)%emis_split = species(spc)%emis_split*SPFC_ASO4 
        else if (i == ano3_idx .and. imode == N_MODE) then
          species(spc)%emis_split = species(spc)%emis_split*SPFC_ANO3
        else if (i == acl_idx .and. imode == N_MODE) then
          species(spc)%emis_split = species(spc)%emis_split*SPFC_ACL
        else if (i == ah2o_idx .and. imode == N_MODE) then
          species(spc)%emis_split = species(spc)%emis_split*SPFC_AH2O
        else if (i == acors_idx .and. imode == N_MODE) then
          species(spc)%emis_split = species(spc)%emis_split*SPFC_ACORS
        else
          species(spc)%emis_split = species(spc)%emis_split*emis_split
        end if
        EXIT  ! match found, go to next mode or emitted species
      end if
!debug
!      if (spc == nspecies) then
!        write(*,*)'Emitted specis ',emisname,' is not a model species'
!      end if 
!debug
    end do
  end do
end do

! Number concentrations and surface area concentrations
do imode = 1,n_mode
  num_name = aeromode(imode)%num_name
  srf_name = aeromode(imode)%srf_name
  do spc = 1,nspecies   ! (SCICHEM species)
    if (species(spc)%class == ID_SPECIES_NUMBER) then
       if ( TRIM(species(spc)%name) == num_name ) then
          species(spc)%pmode = imode            
       end if
    end if
    if (species(spc)%class == ID_SPECIES_SURFACE) then
       if ( TRIM(species(spc)%name) == srf_name ) then
          species(spc)%pmode = imode            
       end if
    end if
  end do
end do

!debug
!do spc = 1, nspecies
!  write(*,*)'spc,name,emitted name,emis_split,mode,density: ', spc, &
!    species(spc)%name,species(spc)%nameemit,species(spc)%emis_split, &
!    species(spc)%pmode,species(spc)%density
!end do
!debug
 
9999 return
end

subroutine get_trst(stdate,sttime)
!******************************************************************************
!
! FUNCTION:  Get the restart time
!            PiG version can restart from any time, 
!            while PC version restarts only from last time
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              julian_day
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer stdate   !Stored in the form YYYYDDD = YEAR*1000  +  DAY
integer sttime   !Stored in the form HHMMSS  = HOUR*10000 +  MINS*100  +  SECS

! --- LOCALS

integer stdate_orig, sttime_orig
integer ihr, imin, isec, jul, julian_day
integer SECSDIFF

jul = julian_day(month_start,day_start,year_start)
stdate_orig =  year_start*1000 + jul

ihr = int(tstart)
imin = int((tstart - float(ihr))/60.)
isec = int((tstart - float(ihr))/3600.)

sttime_orig = ihr*10000 + imin*100 + isec

write(6,*) 'year_start,jday_start:',year_start,jul
write(6,*) 'stdate_orig,sttime_orig:',stdate_orig,sttime_orig
write(6,*) 'stdate,sttime:',stdate,sttime

trst = FLOAT (SECSDIFF(stdate_orig, sttime_orig, stdate, sttime))

write(6,*) 'Time to restart puff file from:',trst
return

end

subroutine init_dmp_file
!******************************************************************************
!
! FUNCTION:   Initialize the 3D file that saves the tracer that has
!             been dumped at each hour
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!    Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
!    Minor F90 Updates, Feb. 2004 (PKK, AER)
!    Additional updates for CMAQ-APT-PM, May 2005 (PKK, AER)
!******************************************************************************
 
! --- MODULES
 
use files_inc
use common_puf
use diagnostics

implicit none

! --- LOCALS

integer i, j, k, ios

cdump = 0.

open(unit=lun_dmp,file=file_dmp,status='NEW',iostat=ios,form='UNFORMATTED')

if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'init_dmp_file'
  eMessage = 'Error opening SCICHEM-PiG 3D PiG transferred-to-host file'
  eInform  = 'File='//TRIM(file_dmp)
end if

return
end

subroutine write_dmp_file
!******************************************************************************
!
! FUNCTION:   Write to the 3D file that saves the tracer that has
!             been dumped at each hour
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          write_progress
!
! REVISION HISTORY: 
!
!    Minor F90 Updates, Feb. 2004 (PKK, AER)
!
!******************************************************************************
 
! --- MODULES
 
use files_inc
use common_puf
use diagnostics

implicit none

! --- LOCALS

character*80 cmsg, cmsg2, cmsg3
integer i, j, k, ios

cmsg='Writing 3D PiG transferred-to-host file'
cmsg2 = char(0)
cmsg3 = char(0)
call write_progress(cmsg,cmsg2,cmsg3)
if(nError /= NO_ERROR)go to 9999
write(lun_dmp,iostat=ios) t/3600.
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_dmp_file'
  eMessage = 'Error writing SCICHEM-PiG 3D PiG transferred-to-host file'
  eInform  = 'File='//TRIM(file_dmp)
  go to 9999
end if

do k = 1, MAXZB
  write(lun_dmp,iostat=ios) &
  ((cdump(i,j,k),i=1, MAXXB),j=1, MAXYB)
  if (ios /= 0) then
    nError   = WR_ERROR
    eRoutine = 'write_dmp_file'
    eMessage = 'Error writing SCICHEM-PiG 3D PiG transferred-to-host file'
    eInform  = 'File='//TRIM(file_dmp)
    go to 9999
  end if
end do

call flush(lun_dmp)

cdump = 0.

9999 return
end

subroutine restart_dmp_file
!******************************************************************************
!
! FUNCTION:   Restart the 3D file that saves the tracer that has
!             been dumped at each hour
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          write_progress
!
! REVISION HISTORY: 
!
!    Minor F90 Updates, Feb. 2004 (PKK, AER)
!
!******************************************************************************
 
! --- MODULES
 
use files_inc
use common_puf
use diagnostics

implicit none

! --- LOCALS

character*80 cmsg, cmsg2, cmsg3
integer i, j, k, ios
real tem

real, parameter :: small = 1.E-2

cmsg='Restarting 3D PiG transferred-to-host file'
cmsg2 = char(0)
cmsg3 = char(0)
call write_progress(cmsg,cmsg2,cmsg3)
if(nError /= NO_ERROR)go to 9999

if (iversion >= 1219) then
  open(unit=lun_dmp,file=file_dmpr,status='OLD',iostat=ios,form='UNFORMATTED')
else
  open(unit=lun_dmp,file=file_dmpr,status='OLD',iostat=ios,recl=7500)
end if
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'restart_dmp_file'
  eMessage = 'Error opening SCICHEM-PiG 3D PiG transferred-to-host file'
  eInform  = 'File='//TRIM(file_dmpr)
  eAction  = 'Make sure file exists'
  go to 9999
end if

tem = -999.
do while ((tem + small) < trst/3600.)
  if (iversion >= 1219) then
    read(lun_dmp,end=9995,err=9995) tem
    do k = 1, MAXZB
      read(lun_dmp,iostat=ios,end=9995,err=9995) &
           ((cdump(i,j,k),i=1, MAXXB),j=1, MAXYB)
    end do
  else
    read(lun_dmp,*,end=9995,err=9995) tem
    read(lun_dmp,*,end=9995,err=9995) &
        (((cdump(i,j,k),i=1, MAXXB),j=1, MAXYB),k=1,MAXZB)
  end if
end do

close(lun_dmp)
open(unit=lun_dmp,file=file_dmp,status='NEW',iostat=ios,form='UNFORMATTED')

if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'restart_dmp_file'
  eMessage = 'Error opening SCICHEM-PiG 3D PiG transferred-to-host file'
  eInform  = 'File='//TRIM(file_dmp)
end if

write(lun_dmp,iostat=ios) tem
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'write_dmp_file'
  eMessage = 'Error writing SCICHEM-PiG 3D PiG transferred-to-host file'
  eInform  = 'File='//TRIM(file_dmp)
  go to 9999
end if

do k = 1, MAXZB
  write(lun_dmp,iostat=ios) &
  ((cdump(i,j,k),i=1, MAXXB),j=1, MAXYB)
  if (ios /= 0) then
    nError   = WR_ERROR
    eRoutine = 'write_dmp_file'
    eMessage = 'Error writing SCICHEM-PiG 3D PiG transferred-to-host file'
    eInform  = 'File='//TRIM(file_dmp)
    go to 9999
  end if
end do

call flush(lun_dmp)

cdump = 0.

9999 return

9995    continue
nError   = RD_ERROR
eRoutine = 'restart_dmp_file'
eMessage = 'Error finding timebreak on SCICHEM-PiG 3D PiG transferred-to-host file'
write(eAction,*) 'Time_rst = ',trst, 'Last transfer time = ',tem
eInform  = 'File='//TRIM(file_dmp)
go to 9999
end

subroutine open_old_mcfile(lun,filename)
!******************************************************************************
!
! FUNCTION:   Open an old multicomponent file (diagnostic or deposition)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS

integer lun
character*(*) filename

! --- LOCALS

integer ios

if (iversion >= 1219) then
  open(unit=lun,file=filename,status='OLD',iostat=ios,form='UNFORMATTED')
else
  open(unit=lun,file=filename,status='OLD',iostat=ios,recl=7500)
end if
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'open_old_mcfile'
  eMessage = 'Error opening SCICHEM file'
  eInform  = 'File='//TRIM(filename)
  eAction  = 'Make sure file exists'
end if

return
end

subroutine open_new_mcfile(lun,filename)
!******************************************************************************
!
! FUNCTION:   Open a new multicomponent file (diagnostic or deposition)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS

integer lun
character*(*) filename

! --- LOCALS

integer ios

if (iversion >= 1219) then
  open(unit=lun,file=filename,status='NEW',iostat=ios,form='UNFORMATTED')
else
  open(unit=lun,file=filename,status='NEW',iostat=ios,recl=7500)
end if
if (ios /= 0) then
  nError   = OP_ERROR
  eRoutine = 'open_new_mcfile'
  eMessage = 'Error opening SCICHEM file'
  eInform  = 'File='//TRIM(filename)
  eAction  = 'Make sure file does not already exist'
end if

return
end
