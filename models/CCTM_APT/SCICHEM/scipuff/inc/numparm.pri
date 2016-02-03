!----------------------------------------------------------------------
! --- common block /numparm/ -- parameters used in the            prime
!                               numerical plume rise algorithm
!----------------------------------------------------------------------

integer nent
real gravi,rgas,zmin,ds,nstep,slast,rp
real, dimension(mxent)   :: alphap,betap
real, dimension(mxentp1) :: xcat

common/numparm/gravi,rgas,zmin,ds,nstep,slast,rp, &
                  alphap,betap,xcat,nent

! --- common block /numparm/ variables:

!         gravi - real    - acceleration due to gravity (m/s**2)
!          rgas - real    - gas constant (m**2/s**2/deg. k)
!          zmin - real    - minimum plume centerline height (m)
!            ds - real    - step size (m) in the numerical plume
!                           rise algorithm
!         nstep - integer - internal save frequency of plume rise
!                           calculations (i%e., every ds*nstep meters)
!                           (note: this the frequency with which the
!                           results are saved internally -- not that
!                           passed back from the numrise routine)
!         slast - real    - termination distance (m) of the plume rise
!                           calculation
!            rp - real    - radiation coefficient (kg/m**2/deg. k**3/s)
!   alphap(mxent) - real array - perturbed entrainment coefficients
!                                (parallel)
!    betap(mxent) - real array - perturbed entrainment coefficients
!                                (normal)
!   xcat(mxentp1) - real array - downwind distances (m) for which each
!                                perturbed entrainment coefficient
!                                (alphap, betap) is valid (nent+1 values
!                                for nent entrainment coefficients).
!            nent - integer    - number of perturbed entrainment
!                                coefficients entered
