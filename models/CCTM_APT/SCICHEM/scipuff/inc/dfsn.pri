!----------------------------------------------------------------------
! --- common block /dfsn/ -- parameters used in the            prime
!                            prime turbulence and diffusion
!                            subroutines
!----------------------------------------------------------------------

real afac,mxbyrmax,wiz0,wiy0,wfz,wfy,dua_ua,xdecay,xdecayi

real rurliz(6),rurliy(6),urbniz(6),urbniy(6)

common/dfsn/afac,xbyrmax,wiz0,wiy0,wfz,wfy, &
               dua_ua,xdecay,xdecayi, &
               rurliz,rurliy, &
               urbniz,urbniy

! --- common block /dfsn/ variables:

!          afac - real    - diffusion transitions to ambient (with
!                           virtual source) when wake turbulence decays
!                           to afac*(ambient turbulence intensity) for
!                           pg classes 4, 5, and 6
!       xbyrmax - real    - upper limit on distance from upwind face
!                           of bldg to transition point for ambient
!                           diffusion
!       wiz,wiy - real    - base turbulence intensities in wake
!       wfz,wfy - real    - scaling factors for sigmaz and sigmay
!        dua_ua - real    - [ua-u]/ua in wake at downwind face of bldg
!                                u: average speed in wake
!                               ua: ambient speed
!         decay - real    - exponent for turbulence intensity change
!                           with distance from downwind face of bldg
!        decayi - real    - 1/decay
!     rurliz(6) - real    - rural turbulence intensities in z
!     rurliy(6) - real    - rural turbulence intensities in y
!     urbniz(6) - real    - urban turbulence intensities in z
!     urbniy(6) - real    - urban turbulence intensities in y
! --- ambient turbulence intensities are inferred from briggs (1973)
! --- "diffusion estimation for small emissions", atdl-106;
