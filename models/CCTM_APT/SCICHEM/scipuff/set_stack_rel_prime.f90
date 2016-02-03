!*******************************************************************************
!$RCSfile: set_stack_rel_prime.f90,v $
!$Revision: 1.2 $
!$Date: 2010/08/24 21:31:05 $
!*******************************************************************************
subroutine set_stack_rel_prime(lmc,xmap,ymap,zbar,frac)
!*******************************************************************************
!
! FUNCTION:  Set true release parameters for STACK release
!            Use PRIME
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use sciprime_inc
use common_puf
use common_met
use get_met_inc
use files_inc

implicit none

! --- ARGUMENTS

logical lmc        ! - multicomponent release flag
real xmap, ymap    ! - map factors
real zbar, frac    ! - puff height (includes plume rise on output)
                   !   mass fraction in primary plume

! --- LOCALS

CHARACTER*20              :: rdfrm
REAL,PARAMETER            :: gravi = 9.807,rgas = 287.026  ! from params.pri of PRIME.FOR
REAL                      :: hstack,dstack,tstack,wstack,hseff,reff
REAL                      :: ustack,ubldg,afv,dtemdz,pvel,uref

REAL                      :: xmlinv
REAL                      :: ddz, thv1, thv2

INTEGER                   :: ios
INTEGER                   :: i,k,ifvsec,isrc
INTEGER                   :: nza,ntr
REAL,DIMENSION(nst)       :: dturb,dtrur,purb,prur
REAL,DIMENSION(mxnz)      :: uamb,ramb,tamb,zgpta   ! common data in ambient
REAL,DIMENSION(mxnzp1)    :: dedz,zfacea      ! common data in ambient
REAL,DIMENSION(mxntr)     :: xtr,ytr,ztr,rtr

REAL                      :: xr,yr,zr,ybadj,sz,sy,szc,syc
REAL                      :: dszdx,dsydx,dszcdx,dsycdx
REAL                      :: h,tamb0,ramb0,fqcav
REAL                      :: dsbh,dsbw,dsbl,xadj,yadj
!      DSBH - real    - Effective building height (m)
!      DSBW - real    - Effective building width (m) across flow
!      DSBL - real    - Effective building length (m) along flow
!      XADJ - real    - Distance (m) from source to upwind face of bldg along flow
!      YADJ - real    - Distance (m) from source to center of upwind face of bldg across flow

LOGICAL                   :: eof, nopath, nokey
LOGICAL                   :: ldbhr
LOGICAL                   :: primeprof

!***********************************************************************
!     Initialize Default Wind Profile Exponents and DTHETADZ
!***********************************************************************

!STAB.CLASS  A    B     C     D      E      F
!           ***  ***   ***   ***    ***    ***
DATA dturb  /0.,  0.,   0.,   0.,   0.02, 0.035/, &
     dtrur  /0.,  0.,   0.,   0.,   0.02, 0.035/, &
     purb  /0.15, 0.15, 0.20, 0.25, 0.30, 0.30/,  &
     prur  /0.07, 0.07, 0.10, 0.15, 0.35, 0.55/   

! Initialise variables

iwrk2 = 0
isstat = 0
keywd(1:ikn) = (/'RURAL   ','BUILDHGT','BUILDWID',&
                'BUILDLEN','XBADJ   ','YBADJ   '/)
eof = .false.
! LDBHR - Debug output written when .TRUE.
ldbhr   = .false.
wake    = .false.
kyprm   = 0.0
kzprm   = 0.0
adsbh   = 0.0
adsbw   = 0.0
adsbl   = 0.0
adsxadj = 0.0
adsyadj = 0.0
rural   = .false.
urban   = .true.
! Call NUMMET to set up wind, temp profiles when .TRUE. else use GETMET
primeprof = .false.

iline = 0

WRITE(rdfrm,9100) istrg, istrg
9100 FORMAT('(a',i3.3,',t1,',i3.3,'a1)')
OPEN(inunit,FILE = TRIM(name_prime),STATUS = "old",IOSTAT = ios)
IF (ios /= 0) THEN
   nError = IV_ERROR
   eRoutine='set_stack_rel_prime'
   eMessage=' '
   WRITE(eInform,*)'Missing Prime input File   ',TRIM(name_prime)
   eAction  = 'Stopping SCICHEM run'
   GO TO 9999
END IF
DO WHILE (.not. eof)
    ! Increment the Line Counter
    iline = iline + 1
    ! READ Record to Buffers, as A80 and 80A1 for ISTRG = 80.
    ! Length of ISTRG is Set in PARAMETER Statement in MAIN1.INC
    READ(inunit,rdfrm,END=999) runst1, (runst(i), i = 1, istrg) 
    ! Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
    CALL lwrupr
    ! Define Fields on Card                              ---   CALL DEFINE
    CALL define
    ! Get the Contents of the Fields                     ---   CALL GETFLD
    CALL getfld
    !  If Blank Line, Then CYCLE to Next Card
    IF (bline) GO TO 11
    ! Extract Pathway ID From Field 1                    ---   CALL EXPATH
    CALL expath(field(1),nopath)
    ! For Invalid Pathway and Comment Lines Skip to Next Record
    IF (nopath .or. path == '**') GO TO 11
    ! Extract Keyword From Field 2                       ---   CALL EXKEY
    CALL exkey(field(2),nokey)
    ! When Keyword Is Wrong, Save Keyword and Skip To The Next Record
    IF (nokey) THEN
      pkeywd = keywrd
      GO TO 11
    END IF
    ! Process Input Card Based on Pathway
    IF (path == 'SO') THEN
     ! Process SOurce Pathway Cards                    ---   CALL SOCARD
      CALL socard      
    END IF

    ! Store the Current Keyword as the Previous Keyword
    pkeywd = keywrd

    GO TO 11
    999     eof = .true.
    11      CONTINUE
END DO
CLOSE(inunit)
! Check Source Array Limits for Too Few Values
CALL srcqa
IF (nError /= NO_ERROR) GO TO 9999
! Check for missing keywords
DO i = 1,ikn
  IF (isstat(i) == 0) THEN
    nError = WN_ERROR
    eRoutine='set_stack_rel_prime'
    WRITE(eInform,*)'Using default value of 0.0'
    SELECT CASE(i) 
      CASE(1)
        eMessage= 'Missing rural keyword'
        WRITE(eInform,*)'Using default/previously set value of ',rural
      CASE(2)
        eMessage= 'Missing BUILDHGT keyword'
      CASE(3)
        eMessage= 'Missing BUILDWID keyword'
      CASE(4)
        eMessage= 'Missing BUILDLEN keyword'
      CASE(5)
        eMessage= 'Missing XBADJ keyword'
      CASE(6)
        eMessage= 'Missing YBADJ keyword'
     END SELECT
     eAction  = CHAR(0)
     CALL WarningMessage(0,.true.)
     IF (nError /= NO_ERROR) GO TO 9999
  END IF
END DO

CALL numpr1
CALL prime1

hstack    = zrel                          ! stack height (m)
dstack    = size_rel                      ! stack diameter (m)
tstack    = buoy + 273.15                 ! exhaust temperature (deg K)
wstack    = wmom                          ! exhaust velocity (m/s)

! KST - integer - PG stability class
IF (xml /= 0.0) THEN
   xmlinv = 1./xml
   IF (xmlinv < -0.14) THEN
      kst = 1
   ELSE IF (xmlinv < -0.05) THEN
      kst = 2
   ELSE IF (xmlinv < -0.01) THEN
      kst = 3
   ELSE IF (xmlinv < 0.01) THEN
      kst = 4
   ELSE IF (xmlinv < 0.06) THEN
      kst = 5
   ELSE 
      kst = 6
   END IF
ELSE 
   kst = 4
END IF

! Only single stack case
isrc = 1
IF (isrural(isrc)) THEN
    rural = .true.
    urban = .false.
ELSE 
    urban = .true.
    rural = .false.
END IF

! Power law coefficient and temperature gradient
IF (rural) THEN
    pvel   = prur(kst) 
    dtemdz = dtrur(kst)  
ELSE 
    pvel   = purb(kst)
    dtemdz = dturb(kst)
END IF

! ---- Set stack parameters
CALL get_met(xrel,yrel,zref,0.,0.,xmap,ymap,1,.false.)
CALL get_sector(ub,vb,uref,ifvsec,afv)
dsbh = adsbh(ifvsec,isrc)
dsbw = adsbw(ifvsec,isrc)
dsbl = adsbl(ifvsec,isrc)
xadj = adsxadj(ifvsec,isrc)
yadj = adsyadj(ifvsec,isrc)
CALL wakflg(hstack,dsbh,dsbw,wake)

IF (wake) THEN
   tamb0  = tb*(pb**0.285714)
   ! --- Set meteorological arrays for plume rise calculation
   IF (primeprof) THEN
       CALL nummet(uref,zref,pvel,tamb0,dtemdz,ldbhr)
      !ustack - Wind speed (m/s) at release height
      CALL wsadj(ustack,hstack,uref,zref,pvel)
      !ubldg - Wind speed (m/s) at top of building
      CALL wsadj(ubldg,dsbh,ustack,zrel,pvel)
   ELSE 
      CALL get_prime_grid(nza,mxnz,zgpta,zfacea,ramb0)
      CALL get_met(xrel,yrel,0.,0.,0.,xmap,ymap,1,.false.) 
      thv1   = (1. + 0.608*hb)*tb
      CALL get_met(xrel,yrel,zgpta(1),0.,0.,xmap,ymap,1,.false.) 
      uamb(1)  = SQRT(ub*ub + vb*vb)
      tamb(1)  = tb*(pb**0.285714)
      thv2     = (1. + 0.608*hb)*tb
      dedz(1)  = (thv2-thv1)/zgpta(1)
      DO k = 2, nza
         ddz = 1./(zgpta(k) - zgpta(k-1))
         thv1 = thv2
         CALL get_met(xrel,yrel,zgpta(k),0.,0.,xmap,ymap,1,.false.)
         uamb(k)  = SQRT(ub*ub + vb*vb)
         tamb(k)  = tb*(pb**0.285714)
         thv2     = (1. + 0.608*hb)*tb
         dedz(k) = ddz*(thv2-thv1)
      END DO
      dedz(nza+1) = dedz(nza)
      ! Create ambient density profile on grid center 
      ! (Code section from nummet of PRIME.FOR
      DO i = 1,nza
         ! Compute average temperature in layer
         thv1 = 0.5*(tamb(i)+tamb0)
         ! Compute ambient air density at height, ZGPTA(i)
         ramb(i)=ramb0*(tamb0/tamb(i))*EXP(-gravi*zgpta(i)/(rgas*thv1))
      END DO
      CALL set_ambient(nza,uamb,ramb,dedz,tamb,tamb0,ramb0)
      CALL get_met(xrel,yrel,hstack,0.,0.,xmap,ymap,1,.false.)
      ustack = SQRT(ub*ub+vb*vb) ! Wind speed (m/s) at release height
      CALL get_met(xrel,yrel,dsbh,0.,0.,xmap,ymap,1,.false.)
      ubldg = SQRT(ub*ub+vb*vb)  ! Wind speed (m/s) at top of building
   END IF
   CALL wake_ini(ldbhr,kst,rural,dsbh,dsbw,dsbl,xadj,yadj,ubldg,ustack)
   hseff = hstack
   reff  = 0.5*dstack
   ntr = mxntr
   CALL numrise(ldbhr,hseff,reff,tstack,wstack,ntr,xtr,ytr,ztr,rtr)
   CALL get_wakedat(xr,yr,zr,ybadj,sy,sz,dsydx,dszdx,syc,szc,dsycdx, &
                    dszcdx,ntr,xtr,ytr,ztr,hseff,fqcav,wmom, &
                    buoy,nError)
   IF (nError /= NO_ERROR) THEN
      eRoutine= 'get_wakedat'
      eMessage= 'xr Point lies outside range of tabulated values '
      WRITE(eInform,*)'PRIME Model not implemented'
      eAction  = 'Using Plume rise algorithm'
      CALL WarningMessage(0,.true.)
      wake = .false.
      GO TO 9999
   END IF
!  Reset release location     
   CALL mapfac( xrel , yrel , xmap , ymap )
   xrel_prm(1) = xrel + xr*SIN(afv)*xmap - yr*COS(afv)*xmap  ! afv = -pi to +pi from North.
   xrel_prm(2) = xrel + xr*SIN(afv)*xmap - (yr+ybadj)*COS(afv)*xmap
   yrel_prm(1) = yrel + xr*COS(afv)*ymap + yr*SIN(afv)*xmap
   yrel_prm(2) = yrel + xr*COS(afv)*ymap + (yr+ybadj)*SIN(afv)*xmap

!  set scichem release parameters for the two source
   if (lter) then
      call get_topog(xrel_prm(1),yrel_prm(1),h,hx,hy)
      if (nError /= NO_ERROR) go to 9999
   else 
       h = 0.0
   end if
   zrel_prm(1)  = zr  + h
   if (lter) then
      call get_topog(xrel_prm(2),yrel_prm(2),h,hx,hy)
      if (nError /= NO_ERROR) go to 9999
   end if 
   zrel_prm(2)  = h
   sigy_prm(1)  = sy
   sigy_prm(2)  = syc
   sigz_prm(1)  = sz
   sigz_prm(2)  = szc
   frac_prm(1)  = 1.0 - fqcav
   frac_prm(2)  = fqcav
   cmass_prm    = cmass
   rel_mc_prm   = rel_mc
   wmom_prm(1)  = wmom
   wmom_prm(2)  = 0.0
   buoy_prm(1)  = buoy
   buoy_prm(2)  = 0.0
  !   Reset bouyancy to zero
   wmom        = 0.0
   buoy        = 0.0
   size_rel    = 0.0
   frac        = frac_prm(1)
   CALL get_met(xrel_prm(1),yrel_prm(1),zrel_prm(1),0.,0.,xmap,ymap,1,.false.)
   ustack    = SQRT(ub*ub+vb*vb) ! Wind speed (m/s) at primary src release height
   ky_prm(1) = sy*dsydx*ustack
   kz_prm(1) = sz*dszdx*ustack
   CALL get_met(xrel_prm(2),yrel_prm(2),zrel_prm(2),0.,0.,xmap,ymap,1,.false.)
   ustack    = SQRT(ub*ub+vb*vb) ! Wind speed (m/s) at secondary src release location
   ky_prm(2) = syc*dsycdx*ustack
   kz_prm(2) = szc*dszcdx*ustack
    
   if (fqcav > 0.0) then
       write(lun_log,*)'Prime calculation: Primary source (xrel,yrel,zrel,1-fqcav): ',&
                         xrel_prm(1), yrel_prm(1), zrel_prm(1),frac_prm(1)
       write(lun_log,*)'Prime calculation: Secondary source (xrel,yrel,zrel,fqcav): ',&
                         xrel_prm(2), yrel_prm(2), zrel_prm(2),frac_prm(2)
    else 
       write(lun_log,*)'Prime calculation (xrel,yrel,zrel): ',&
                         xrel_prm(1), yrel_prm(1), zrel_prm(1)
    end if
ELSE 
    nError   =  WN_ERROR
    eRoutine = 'set_stack_rel_prime'
    eMessage = 'No wake effects '
    WRITE(eInform,*)'PRIME Model not implemented'
    eAction  = 'Using Plume rise algorithm'
    CALL WarningMessage(0,.true.)
END IF

9999 RETURN
END

SUBROUTINE wakflg(hs,dsbh,dsbw,wake)
!***********************************************************************
!                 WAKFLG Module of ISC2 Short Term Model - ISCST2
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        D. Strimaitis
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------

!        PURPOSE: To Set Wake Flags for Building Downwash Algorithms

!        PROGRAMMER: Roger Brode, Jeff Wang

!        DATE:    March 2, 1992

!        INPUTS:  Building Dimensions
!                 Source Parameters
!                 Meteorological Variables for One Hour

!        OUTPUTS: Logical Flags for Wake Switche, WAKE 
!***********************************************************************
IMPLICIT NONE
REAL            :: dsbh,dsbw,hs
LOGICAL         :: wake

!     Set Initial Wake Switches Based on Building Dimensions
IF (dsbh==0.0 .or. dsbw==0.0 .or. &
    hs >= (dsbh + 1.5*AMIN1(dsbh,dsbw))) THEN
    wake   = .false.
ELSE IF (hs > (dsbh + 0.5*AMIN1(dsbh,dsbw))) THEN
    wake   = .true.
ELSE
    wake   = .true.
END IF

RETURN
END

SUBROUTINE wsadj(us,hs,uref,zref,p)
!***********************************************************************
!                 WSADJ Module of ISC2 Short Term Model - ISCST2
!
!        PURPOSE: Adjusts Wind Speed from Anemometer Height to Stack Height
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        INPUTS:  Arrays of Source Parameters
!                 Meteorological Variables for One Hour
!                 Wind Speed Profile Exponents (Default or User-defined)
!
!        OUTPUTS: Stack Top Wind Speed, US
!
!***********************************************************************
IMPLICIT NONE
REAL         :: us,hs,uref,zref,p

!     Adjust Wind Speed -- Assume Wind Speed Constant Below 10 meters
IF (hs >= 10.0) THEN
    us = uref * (hs/zref)**p
ELSE IF (zref > 10.0) THEN
    us = uref * (10.0/zref)**p
ELSE
    us = uref
END IF

!     Do Not Allow Stack Height Wind Speed < 1.0 m/s
IF (us < 1.0) THEN
    us = 1.0
END IF

RETURN
END

SUBROUTINE get_sector(u,v,s,ifvsec,afv)
IMPLICIT NONE
! Input u,v; Output s,ifvsec,afv
REAL    :: u,v,s,beta,deg2rad,afv
INTEGER :: ifvsec

deg2rad = ATAN(1.0)/45.0
s = SQRT(u*u+v*v)
afv = ATAN2(u,v)    ! -pi to +pi from North.
beta = afv/deg2rad   
IF (beta < 0.0) beta = beta + 360.0
ifvsec = INT(beta*0.1 + 0.4999)
IF (ifvsec == 0) ifvsec = 36

RETURN 
END

SUBROUTINE load_prime_rel(irel,zbar)
USE common_puf
USE sciprime_inc

IMPLICIT NONE

INTEGER :: irel
REAL    :: zbar

cmass   = cmass_prm*frac_prm(irel)
rel_mc  = rel_mc_prm*frac_prm(irel)

sigx    = sigy_prm(irel)
sigy    = sigy_prm(irel)
sigz    = sigz_prm(irel)
xrel    = xrel_prm(irel)
yrel    = yrel_prm(irel)
zbar    = zrel_prm(irel)
kyprm   = ky_prm(irel)
kzprm   = kz_prm(irel)
wmom    = wmom_prm(irel)
buoy    = buoy_prm(irel)

RETURN
END 
