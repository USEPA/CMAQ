!----------------------------------------------------------------------
! --- common block /wakedat/ -- parameters used in the            prime
!                               prime wake and streamline
!                               subroutines
!----------------------------------------------------------------------

integer nwak,ncav,istab

real hb,wb,xlb,rb,hr,xlr,xlc
real xbadj,ybadj,ub,urh
real xzvwak,xyvwak
real xzvcav,xyvcav,fqcav

real, dimension(mxntr):: xwak,szwak,sywak,drwak
real, dimension(mxntr):: xcav,szcav,sycav

logical lrurl

common/wakedat/hb,wb,xlb,rb,hr,xlr,xlc, &
                  xbadj,ybadj,ub,urh, &
                  nwak,xwak,szwak,sywak, &
                  drwak,xzvwak,xyvwak, &
                  ncav,xcav,szcav,sycav, &
                  xzvcav,xyvcav,fqcav,istab,lrurl

! --- common block /wakedat/ variables:

!            hb - real    - building height (m)
!            wb - real    - building width (crosswind) - (m)
!           xlb - real    - building length (alongwind) - (m)
!            rb - real    - scale length (m)
!            hr - real    - maximum cavity height (m) above ground
!           xlr - real    - length of downwind cavity (m) from
!                           downwind face of building
!           xlc - real    - length of roof cavity (m)
!         xbadj - real    - distance along the wind from the stack to
!                           the origin of the building (upwind center
!                           of effective building)
!         ybadj - real    - distance crosswind from the stack to
!                           the origin of the building (upwind center
!                           of effective building)
!            ub - real    - wind speed (m/s) at the height of bldg
!           urh - real    - wind speed (m/s) at release height

!          nwak - integer - number of downwind distances at which
!                           wake properties are tabulated (le mxntr)
!   xwak(mxntr) - real    - downwind distance (m) from source
!  szwak(mxntr) - real    - sigma-z (m) at position xwak
!  sywak(mxntr) - real    - sigma-y (m) at position xwak
!  drwak(mxntr) - real    - plume growth rate at position xwak expressed
!                           as d/dx(plume radius) for equivalent top-hat
!        xzvwak - real    - virtual distance (m) for sigma-z beyond wake
!        xyvwak - real    - virtual distance (m) for sigma-y beyond wake
!          ncav - integer - number of downwind distances at which
!                           wake properties of cavity source are
!                           tabulated (le mxntr)
!   xcav(mxntr) - real    - downwind distance (m) from primary source
!  szcav(mxntr) - real    - sigma-z (m) for cavity source
!  sycav(mxntr) - real    - sigma-y (m) for cavity source
!        xzvcav - real    - virtual distance (m) for sigma-z beyond wake
!                           for cavity source
!        xyvcav - real    - virtual distance (m) for sigma-y beyond wake
!                           for cavity source
!         fqcav - real    - fraction of plume mass captured by cavity
!         istab - integer - pg stability class
!         lrurl - logical - rural dispersion when .true.
