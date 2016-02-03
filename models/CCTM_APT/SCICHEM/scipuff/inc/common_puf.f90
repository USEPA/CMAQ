!*******************************************************************************
!$RCSfile: common_puf.f90,v $
!$Revision: 1.4 $
!$Date: 2007/02/06 23:33:01 $
!
!REVISION HISTORY: 
! 01/31/2007 : Add new module common_mc_puf for multiprocessor code, -BC
!*******************************************************************************
module common_puf

use common_mc_puf

save

real ASPLT_ZFAC
parameter (ASPLT_ZFAC = 0.866)

type ( puff_str     ) puff(MAXPUF)         !Puff array
type ( type_str     ) typeID(MAXPTYP)      !Puff type info
type ( material_str ) material(MAXMTYP)    !Material info

integer itfrst(0:MAXTLV)  !Pointer to first puff in time list (for each time level)
integer itlast(0:MAXTLV)  !Pointer to last puff
integer ntlev(0:MAXTLV)   !No. of puffs in time level

integer iversion_code     !SCIPUFF run version number
integer iversion          !SCIPUFF project version number (for read)
integer nch_n             !Number of characters in project name
integer mgrd              !Puff splitting criterion
integer nx, ny, nz        !Puff grid dimensions

integer nclass            !Redundant
integer ntypm             !No. of materials
integer ntypp             !No. of puff types
integer ntyps             !No. of deposition fields
integer ntypd             !No. of dosage fields
integer mxsgp             !Max no. of subgroups (particle size bins)

integer npuf              !No. of active puffs
integer mxtlev            !Max active time level

integer lmap              !Coordinate system flag

integer jul_start         !Start time - julian day

integer year_start, month_start, day_start  !Run start YYMMDD
integer year_end,   month_end,   day_end    !Run end YYMMDD

integer istop             !User stop flag (GUI only)

integer ncrel             !No. of active continuous releases
integer ncaux             !No. of auxliary variabls for active continuous releases

integer subgroup          !Release subgroup
integer rel_dist          !Release size distribution flag

integer nsrcaux           !No. of source auxiliary variables (not used)
integer nmaux             !No. of material auxiliary variables 
integer npaux             !No. of puff auxiliary variables  
integer nmcaux            !Not used

integer opid,opmod(SCIPUFF_STATUS_ARRAY_SIZE)   !Not implemented
integer time_status(TIME_STATUS_ARRAY_SIZE)     !Not implemented
integer domain_status(DOMAIN_STATUS_ARRAY_SIZE) !Not implemented

integer utm_zone           !UTM coords not implemented

integer mxlev_smp          !Max sampler output time level (wired to 0)
integer hazard,run_mode    !not implemented

real  decay_rate(MAXMTYP)  !Linear decay rate (for each puff type)
real  buoy_fac(MAXPTYP)    !Buoyancy factor (for each puff type, gases only)

real  wwtrop, sltrop, epstrop !Free atmosphere vertical velocity variance,
                              ! length scale, and energy dissipation rate

real  sle_fac             !Subgrid meso-turbulence scale factor

real  rrmrge              !Puff overlap merge criterion
real  simrge              !Puff scale ratio merge criterion

real  cmin                !Puff minimum mass
real  grdmin, delmin      !Minimum grid size for puff grid and surface grids

real  z_dosage            !Dosage height
  
real  asplt, asplt2, aspltc !Puff split separation factors
 
real  dxsplt, dzsplt        !Grid split factors
real  delx2,  delz2         !Splitting size criteria (squared length)

real  fac_rfl               !Reflection test factor

real  xmin, xmax, ymin, ymax, zmax, dxg, dyg, dzg  !Puff grid parameters

real  hres, vres         !Puff horizontal and vertical grids (user input)

real  lon0 , lat0, xref, yref  !Lat/lon reference point
!real  lon_ref,lat_ref

real  tstart     !Run start hour
real  tend       !Run end hour
real  tend_hr    !Run duration (hrs)
real  tend_r     !Run duration (secs)
real  delt       !Max time step (secs)
real  dt_save    !Output time interval
real  dt_smp     !Minimum sampler output interval (not used)
real  tzone      !Time zone
real  t_avg      !Conditional averaging time (sec) for dispersion calc 

real  t_save     !Next output time

real  t_old_r    !Restart time from old project
real  time_rst   !Restart time
real  trst       !Restart time (secs)

real  trel              !Next continuous release time
real  xrel, yrel, zrel  !Release location
real  cmass             !Release rate
real  ctyp              !Release puff type
real  size_rel          !Release size
real  sigx,sigy,sigz    !Release sigma's
real  tdur              !Release duration
real  urel,vrel,wrel    !Release velocity
real  wmom,buoy         !Release momentum and buoyancy fluxes
real  kyprm,kzprm       ! Diffusivities from PRIME

real  rel_param(SCIPUFF_MAXRELPARAM)   !Release auxiliary data
real  rel_mc(MAX_MC)                   !Release multi-component fluxes

real mat_aux(MAXMAUX)      !Material auxiliary data
real puff_aux(MAXPAUX)     !Puff auxiliary data
real src_aux(MAXSAUX)      !Source auxiliary data

real twash(0:NWASH,MAXPTYP),vwash(0:NWASH) !Washout parameters

character*40 name            !Project name
character*80 title           !Project title
character*4  names(NP_ALL)   !Puff variable names
character*4  namec(MAXCLS)   !Class names (not used)
character*80 metname         !Met profile input filename
character*80 sfcname         !Met surface input filename
character*4  reltyp          !Release type
character*256 name_prime      !Release Input file for PRIME
character*80 name_rel        !Release input file (CLDTRANS)
character*80 smpfile         !Sampler input file
character*16 relmat          !Release material

character*32 audit_class     !Audit strings
character*32 audit_analyst
character*32 audit_date
character*32 audit_version
character*320 audit_space

logical restart         !TRUE for restart
logical surface         !Save deposition data
logical dose            !Save dosage data
logical lter            !Project has terrain
logical lymd            !Project has YYMMDD defined
logical hascal          !Not used
logical create          !Create project files only
logical ldecay          !A material has decay
logical local           !Use local time
logical lsplitz         !Use "lumped" BL
logical lcldshine       !Not used
logical dynamic         !Enable buoyant dynamics
logical dense_gas       !Not used
logical buoy_gas        !A gas material has buoyancy
logical lsmp            !Output samplers
logical static          !Enable static puffs
logical multicomp       !Multi-components

logical buoy_flag(MAXPTYP)   !Puff buoyancy flags
logical wake                 !Building wake effects present if true
logical depint               !Not used
logical lsplit_report        !Flag for reporting turning off splitting 

end module common_puf
