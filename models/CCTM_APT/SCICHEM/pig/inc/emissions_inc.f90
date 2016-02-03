module emissions_inc
use param_inc
!---Emissions variables

!==== parameters

integer nsrc       !# of sources
real tinit_emit    !initial time of 1st hr
                   !with respect to SCICHEM time
real tdur_emit     !duration for each source on file

! Stack parameters
integer, allocatable :: idsrc(:)              !Source id
integer, allocatable :: ping_src(:)  ! flag for treating this source as ping
real, allocatable :: xsrc(:) !x-location of source
real, allocatable :: ysrc(:) !y-location of source
real, allocatable :: zsrc(:) !z-location of source
real, allocatable :: dsrc(:) !diameter of source
real, allocatable :: tsrc(:) !stack temp
real, allocatable :: wsrc(:) !stack exit velocity

!Stack emissions
real, allocatable :: qsrc(:,:) !hourly emissions

!Emissions x-ref array
integer, allocatable :: index_host_emit(:)

character(256), allocatable :: bfile_src(:)  !File name for BPIP input to PRIME

!Indices of oxidized sulfur species in emissions list
integer, save :: VSULF, VPSO4

!Date on stack emissions file
integer, save :: stkdate

end module emissions_inc
