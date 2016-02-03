subroutine initial(lflag,stdate,sttime,tstep,tendhr,cgrid)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Perform all initialization requirements for SCICHEM run
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          init_pig_input          write_progress            init_met_pig
!                set_grid                 zero_ip           create_output
!          init_emissions         write_init_info
!
! REVISION HISTORY: 
!
!    Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
!
!*****************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc
use multcomp_inc
use interface_definitions, only: INIT_PIG_INPUT

implicit none

! --- ARGUMENTS

logical lflag   !Restart flag from host model
integer stdate  !Start date, coded YYYYDDD
integer sttime  !Start time, coded HHMMSS
integer tstep   !Model time step,  coded HHMMSS
real tendhr     !Final hour of simulation (relative to start)
REAL :: CGRID( :,:,:,: )  !3D ambient concentrations from host model
 
! --- LOCALS

integer ios, i, nch, nchp, nblank

character*128 cmsg, cmsg2, cmsg3

!------ initialize

restart = lflag

call init_pig_input(stdate,sttime,tendhr,cgrid)
if (nError /= NO_ERROR) go to 9999

!------ Initialize met file

if(create)then
  cmsg=char(0)
  cmsg2=char(0)
  cmsg3='Validating meteorology input'
  call write_progress(cmsg,cmsg2,cmsg3)
else
  cmsg=char(0)
  cmsg2='Initializing meteorology'
  cmsg3=char(0)
  call write_progress(cmsg,cmsg2,cmsg3)
end if

call init_met_pig(stdate,sttime,tstep)
if (nError /= NO_ERROR) go to 9999

if (.not.restart) then

  call set_grid
  if (nError /= NO_ERROR) go to 9999

  call zero_ip

  call create_output
  if (nError /= NO_ERROR) go to 9999

end if

!------ Initialize emissions from grid model

call init_emissions(stdate,sttime,tstep)
if (nError /= NO_ERROR) go to 9999

trel = DEF_VAL_R

call write_init_info

9999    continue

return

end
