!----------------------------------------------------------------------
! --- PARAMETER statements                                        PRIME
!----------------------------------------------------------------------
! --- Specify model version
      character*12 mver, mlevel
      parameter(mver='alpha',mlevel='950610')

! --- Specify parameters
      parameter(mxnz=100)
      parameter(mxntr=50)
      parameter(mxnw=5000)
      parameter(mxent=10)
      parameter(io5=5,io6=6)

! --- Compute derived parameters
      parameter(mxnzp1=mxnz+1)
      parameter(mxentp1=mxent+1)

! --- GENERAL PARAMETER definitions:
!          MXNZ - Maximum number of vertical layers in
!                 the meteorological data
!         MXNTR - Maximum number of downwind distances for which
!                 numerical plume rise will be reported
!          MXNW - Maximum number of downwind distances for numerical
!                 plume rise integration (should be set equal to
!                 SLAST/DS)
!         MXENT - Maximum number of perturbed entrainment coefficients
!                 entered

! --- FORTRAN I/O unit numbers:
!           IO5 - Control file                  - input  - formatted
!           IO6 - List file                     - output - formatted
