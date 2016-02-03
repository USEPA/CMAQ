!*******************************************************************************
!$RCSfile: prime.f,v $
!$Revision: 1.4 $
!$Date: 2010/08/24 21:16:49 $
!*******************************************************************************
c----------------------------------------------------------------------
      subroutine get_prime_grid(nz,mxz,zg,zf,r0)
c----------------------------------------------------------------------
c
c --- purpose:  get the grid setup in numpr1
c
c --- output:
c
c     common block /ambient/ variables:
c           ramb0,zgpta,zfacea
c
c----------------------------------------------------------------------

c --- include parameters
      use ambient_pri

      real zg(mxz),zf(mxz+1)

      nz = nza
      do i = 1,nza
         zg(i) = zgpta(i)  
         zf(i) = zfacea(i)
      end do
      zf(nza+1) = zfacea(nza+1)
        
      r0 = 1.2  ! From nummet 

      return
      end           

c----------------------------------------------------------------------
      subroutine set_ambient(nz,ua,ra,dtmdz,ta,t0,r0)
c----------------------------------------------------------------------
c
c --- purpose:  set the variables in common ambient data.
c
c --- output:
c
c     common block /ambient/ variables:
c           uamb(mxnz),ramb(mxnz),tamb(mxnz),dedz(mxnzp1),
c           tamb0,ramb0
c
c----------------------------------------------------------------------

c --- include parameters

      use ambient_pri

      real ua(nz),ra(nz),dtmdz(nz+1),ta(nz)

c --- set the surface temperature (deg. k) & air density (kg/m**3)
      tamb0= t0
      ramb0= r0 

c --- extrapolate the winds to the grid point levels
      do i=1,nza
         uamb(i) = ua(i)
         tamb(i) = ta(i)
         dedz(i) = dtmdz(i)
         ramb(i) = ra(i)
      end do
      dedz(nza+1) = dtmdz(nza+1)

      return
      end

c----------------------------------------------------------------------
      subroutine get_wakedat(xr,yr,zr,yb,sy,sz,dsydx,dszdx,syc,szc,
     &                       dsycdx,dszcdx,ntr,xtr,ytr,ztr,hseff,
     &                       frac,wmom,buoy,nError)
c
c --- scichem and prime interface file
c --- purpose:  get wakedat variables in common
c
c --- 01/31/07 : Fix bug in call to sigy with xzvwak. -BC

c----------------------------------------------------------------------
c --- include parameters
      use wakedat_pri
      use ambient_pri
      common /scivars/xplm,uplm,wplm,tplm,rplm,fbini
      save /scivars/
        
      parameter (IV_ERROR = 3)  ! From error_inc.f90
      parameter (WN_ERROR = 10) ! From error_inc.f90
      real xr,yr,zr,sy,sz,syc,szc
      real xrp,syp,szp
      real dsydx,dszdx,dsycdx,dszcdx
      real xtr(ntr),ytr(ntr),ztr(ntr)
      logical nobid

      nobid  = .false.
      dszdx  = 0.0
      dsydx  = 0.0
      dszcdx = 0.0
      dsycdx = 0.0
      pi     = 4.*ATAN(1.0)

!     Transition from near wake to far wake region        
      xr = xplm ! set to xlb + 1.15*xlr + MAX(xbadj,0.0) in NUMRISE
!     Interpolate plume centerline and height
      if (xr >= xtr(ntr)) then
         zr = ztr(ntr)
         yr = ytr(ntr)
      else
         nm1 = ntr - 1
         zr = ztr(1)
         yr = ytr(1)
         do i=nm1,1,-1
            if (xr >= xtr(i))then
               ip1 = i + 1
               fac = (xtr(ip1)-xr)/(xtr(ip1)-xtr(i))
               yr  = ytr(ip1)-(ytr(ip1)-ytr(i))*fac
               zr  = ztr(ip1)-(ztr(ip1)-ztr(i))*fac
               exit
            endif
         end do
      endif
      if(xr <= 0.0) then
c ---    report null values (these should never get used!)
         sz=0.0
         sy=0.0
         nError = WN_ERROR
         return
      elseif((nwak <= 1).or.(xr < xwak(1))) then
c ---    plume never altered by wake turbulence or 
c        point lies upwind of wake region; use host curves
         call sigz(xr,sz)
         call sigy(xr,sy)
           ! Assuming rise = 0.0 as in cav_src
         !  sz=SQRT(sz**2+bidsq)
         !  sy=SQRT(sy**2+bidsq)
         dxi = xtr(2)-xtr(1)
         xrp = xr + dxi
         call sigz(xrp,szp)
         call sigy(xrp,syp)
         dszdx = (szp-sz)/dxi
         dsydx = (syp-sy)/dxi
      elseif(xr > xwak(nwak)) then
c ---    point lies downwind of transition to ambient growth; use
c ---    host curves with virtual distance adjustment
         call sigz(xr+xzvwak,sz)
         call sigy(xr+xyvwak,sy)
         dxi = xwak(nwak)-xwak(nwak-1)
         xrp = xr + dxi
         call sigz(xrp+xzvwak,szp)
         call sigy(xrp+xyvwak,syp)
         dszdx = (szp-sz)/dxi
         dsydx = (syp-sy)/dxi
      else 
         nm1   = nwak-1
         sz    = szwak(1)
         sy    = sywak(1)
         dxi   = 1./(xwak(2)-xwak(1))
         dszdx = (szwak(2)-szwak(1))*dxi
         dsydx = (sywak(2)-sywak(1))*dxi
         do i=nm1,1,-1
           if(xr >= xwak(i))then
             ip1   = i+1
             fac   = (xwak(ip1)-xr)/(xwak(ip1)-xwak(i))
             sz    = szwak(ip1)-(szwak(ip1)-szwak(i))*fac
             sy    = sywak(ip1)-(sywak(ip1)-sywak(i))*fac 
             dxi   = 1./(xwak(ip1)- xwak(i))
             dszdx = (szwak(ip1) - szwak(i))*dxi
             dsydx = (sywak(ip1) - sywak(i))*dxi
             exit
           end if
         end do                
      endif
!     cavity source         
      if((ncav <= 1).or.(xr < xcav(1))) then
c ---    no contribution from cavity source (report initial values)
c ---    or point lies upwind of cavity region (report initial values)
         szc=szcav(1)
         syc=sycav(1)
      elseif(xr > xcav(ncav)) then
c ---    point lies downwind of transition to ambient growth; use
c ---    host curves with virtual distance adjustment
         call sigz(xr+xzvcav,szc)
         call sigy(xr+xyvcav,syc)
      else
         nm1=ncav-1
         szc=szcav(1)
         syc=sycav(1)
         dszcdx = (szcav(2)-szcav(1))*dxi
         dsycdx = (sycav(2)-sycav(1))*dxi
         do i=nm1,1,-1
            if (xr >= xcav(i)) then
               ip1 = i+1
               fac = (xcav(ip1)-xr)/(xcav(ip1)-xcav(i))
               szc = szcav(ip1)-(szcav(ip1)-szcav(i))*fac
               syc = sycav(ip1)-(sycav(ip1)-sycav(i))*fac
               dxi = 1./(xcav(ip1)- xcav(i))
               dszcdx = (szcav(ip1)-szcav(i))*dxi
               dsycdx = (sycav(ip1)-sycav(i))*dxi
               exit
            end if
         end do
      end if

      frac = fqcav
      yb   = ybadj

      flux = pi*rplm*rplm*SQRT(uplm*uplm + wplm*wplm)
      wmom = flux*wplm
      call zmet(zr,ua,ra,ta,dudz,dpdz)
      buoy = flux*(tplm-ta)
      bouy = MAX(buoy,0.0)

      return
      end

c----------------------------------------------------------------------
      subroutine prime1
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812                prime1
c                j. scire, d. strimaitis, earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  initialize the variables used by the prime
c               building downwash algorithm
c
c --- inputs:  none
c
c --- output:
c
c     common block /dfsn/ variables:
c           afac,xbyrmax,wiz0,wiy0,wfz,wfy,
c           dua_ua,xdecay,xdecayi,
c           rurliz,rurliy,urbniz,urbniy
c
c --- prime1 called by:  main (host)
c --- prime1 calls:      none
c----------------------------------------------------------------------

c --- include common blocks
      use dfsn_pri

c----------------------------------------------------------------------
c --- common block /dfsn/ -- parameters used in the            prime
c                            prime turbulence and diffusion
c                            subroutines
c----------------------------------------------------------------------
c
      real riz(6),riy(6),uiz(6),uiy(6)

c --- ambient turbulence intensities are inferred from briggs (1973)
c --- "diffusion estimation for small emissions", atdl-106;
      data riz/.20,.12,.08,.06,.03,.016/
      data riy/.22,.16,.11,.08,.06,.04/
      data uiz/.24,.24,.20,.14,.08,.08/
      data uiy/.32,.32,.22,.16,.11,.11/

c -----------------------
c --- /dfsn/ variables
c -----------------------

c --- set the factor for defining when turb approaches asymptotic
c --- value, and also define the maximum allowed scaled distance
      afac=1.3
      xbyrmax=15.

c --- turbulence intensities in wake (from briggs rural curves)
      wiz0=0.06
      wiy0=0.08
c --- wake factors for sigw and sigv from weil (1996)
      wfz=1.7
      wfy=1.7
c --- deltau0/u0
      dua_ua=0.7
c --- power-law exponent for turbulence intensity change in distance
      xdecay=0.666667
      xdecayi=1.5

c --- pass turbulence intensities to /dfsn/ arrays
      do kst=1,6
         rurliz(kst)=riz(kst)
         rurliy(kst)=riy(kst)
         urbniz(kst)=uiz(kst)
         urbniy(kst)=uiy(kst)
      end do
c
      return
      end
c----------------------------------------------------------------------
      subroutine numpr1
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812                numpr1
c                j. scire, earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  initialize the variables used by the numerical
c               plume rise algorithm
c
c --- inputs:
c
c       parameters:
c           mxent, mxentp1, mxnz, mxnzp1, io6
c
c --- output:
c
c       common block /numparm/ variables:
c          gravi,rgas,zmin,ds,nstep,slast,rp,alphap(mxent),
c          betap(mxent),xcat(mxentp1),nent
c       common block /ambient/ variables:
c          adia,ptgrad0,zgpta(mxnz),zfacea(mxnzp1)
c
c --- numpr1 called by:  main (host)
c --- numpr1 calls:      none
c----------------------------------------------------------------------

c --- include common blocks
      use numparm_pri
      use ambient_pri

c -----------------------
c --- /numparm/ variables
c -----------------------
c
c --- set the acceleration due to gravity (m/s**2)
      gravi=9.807

c --- set the gas constant (m**2/s**2/deg. k)
      rgas=287.026

c --- set the minimum plume centerline height (m)
      zmin=0.001

c --- set the step size (m) in the numerical plume rise algorithm
      ds=1.0

c --- set the internal save frequency of plume rise calculations (i.e.,
c     every ds*nstep meters) (note: this the frequency with which the
c     results are saved internally -- not that passed back from the
c     numrise routine)
      nstep=1

c --- set the termination distance (m) of the plume rise calculation
      slast=5000.

c --- set the radiation coefficient (kg/m**2/deg. k**3/s)
      rp=9.1e-11

c --- set the perturbed entrainment coefficients
c     alphap (parallel direction), betap (normal direction)
      nent=0
      alphap(1)=0.11
      betap(1)=0.6
      xcat(1)=-9.e9
      xcat(2)= 9.e9

c -----------------------
c --- /ambient/ variables
c -----------------------

c --- set dry adiabatic lapse rate (deg. k/m)
      adia=.0098

c --- set minimum potential temperature lapse rate (deg. k/m)
      ptgrad0=0.0

c --- set the default number of layers
      nza=45
      nzap1=nza+1
      if(nza > mxnz)then
         write(io6,*)'error in subr. numpr1 -- nza is too large -- ',
     1   'nza = ',nza,' mxnz = ',mxnz
         stop
      endif
      if(nzap1 > mxnzp1)then
         write(io6,*)'error in subr. numpr1 -- nzap1 is too large -- ',
     1   'nzap1 = ',nzap1,' mxnzp1 = ',mxnzp1
         stop
      endif

c --- define the meteorological grid
c ---    set grid points every 10 m from 10-200 m
      dz=10.
      nn=1
      zgpta(nn)=dz
      do i=2,20
         nn=nn+1
         zgpta(nn)=zgpta(nn-1)+dz
      end do
c ---    set grid points every 50 m from 250-500 m
      dz=50.
      do i=21,26
         nn=nn+1
         zgpta(nn)=zgpta(nn-1)+dz
      end do
c ---    set grid points every 100 m from 600-2000 m
      dz=100.
      do i=27,41
         nn=nn+1
         zgpta(nn)=zgpta(nn-1)+dz
      end do
c ---    set grid points every 500 m from 2500-4000 m
      dz=500.
      do i=42,45
         nn=nn+1
         zgpta(nn)=zgpta(nn-1)+dz
      end do

c --- compute the cell face heights from the grid point values
      zfacea(1)=0.0
      do i=2,nza
         zfacea(i)=0.5*(zgpta(i)+zgpta(i-1))
      end do
      zfacea(nzap1)=zgpta(nza)+0.5*(zgpta(nza)-zgpta(nza-1))

      return
      end
c----------------------------------------------------------------------
      subroutine numrise(ldbhr,h,reff,texit,wexit,
     &                   ntr,xtr,ytr,ztr,rtr)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  980310               numrise
c                x.(j.) zhang, j. scire, d. strimaitis,  earth tech
c
c                adapted from calpuff routine numrise
c                for epri under contract wo3527-01
c
c --- purpose:  compute plume rise using a numerical solution to the
c               non-boussinesq conservation laws.  model allows:
c
c               (1) arbitrary ambient temperature stratifications
c               (2) arbitrary uni-directional wind stratification
c               (3) any size of finite emission source
c               (4) is free of the boussinesq approximation
c               (5) radiative heat loss
c
c               concurrently, compute diffusion (sigmas) in bldg wake
c               and determine plume/cavity interaction
c
c --- inputs:
c         ldbhr - logical       - flag for debug write statements
c             h - real          - release height (m)
c          reff - real          - effective radius of release (m)
c            tp - real          - exit temperature (deg. k)
c         wexit - real          - exit velocity (m/s)
c           ntr - integer       - number of points in trajectory passed
c                                 back to calling program (final point
c                                 is "final rise")
c
c     common block /ambient/ variables:
c           nza,uamb(mxnz),ramb(mxnz),dedz(mxnzp1),tamb(mxnz),
c           zfacea(mxnzp1),tamb0,ramb0
c     common block /numparm/ variables:
c           zmin, ds, nstep, slast, gravi
c     common block /wakedat/ variables:
c           hb, wb, xlb, rb, hr, xlr, xlc, xbadj, ybadj
c     parameters:
c           mxnz, mxnzp1, mxent, mxentp1, io6
c
c --- output:
c        xtr(ntr) - real          - downwind distance from source (m)
c        ytr(ntr) - real          - crosswind distance from source (m)
c        ztr(ntr) - real          - plume centerline height (m)
c        rtr(ntr) - real          - plume radius (m)
c
c     common block /wakedat/ variables:
c           fqcav
c
c --- numrise called by:  pheff (host subroutine)
c --- numrise calls:      zmet,lump,rate,marching,unlump
c                         zstream, position, wake_u
c                         wake_drdx, wake_dfsn, sigz, sigy,
c                         wake_fqc
c----------------------------------------------------------------------
c --- notation --- in (kg,m,s) units
c               nn:     number of points along rise trajectory
c               x:      plume location (downwind from source)
c               y:      plume location (crosswind from source)
c               z:      plume height
c               h:      release height (flame ht., stack ht.)
c               ze:     plume equilibrium height
c               s:      length along plume centerline
c               r:      plume radius
c               u:      plume horizontal velocity
c               w:      plume vertical velocity
c               usc:    velocity along plume centerline
c               phi:    angle between plume trajectory and ground
c               tp:     plume temperature
c               ua:     horizontal wind speed
c               dudz:   wind shear
c               ta:     ambient temperature
c               dpdz:   ambient potential temperature gradient
c               ramb:   ambient density
c               ra:     plume density
c               zmin:   minimum plume centerline height (m)
c               ds:     step size (m) in the numerical plume rise calc.
c               nstep:  reporting frequency of numerical calc.
c               slast:  termination distance (m) of plume rise calc.
c               gravi:  acceleration due to gravity (m/s**2)
c----------------------------------------------------------------------
c --- include files
      use ambient_pri
      use numparm_pri
      use wakedat_pri

      common /plu/ s,x,y,z,r,u,v,w,usc,phi,den,tp
      common /scivars/xplm,uplm,wplm,tplm,rplm,fbini
      save /scivars/,/plu/
      real xtr(ntr),ytr(ntr),ztr(ntr),rtr(ntr)
      real xt(mxnw),yt(mxnw),zt(mxnw),rt(mxnw)
      dimension rhs(7),rhstemp(7),f(7),ftemp(7)
      logical ldb,ldbnn,ldbu,ldbhr,linwake

c --- use ldb as a local switch for more extensive debug output
c !!! ldb=ldbhr
      ldb=.false.
c !!! ldbu=ldb
      ldbu=.false.

      linwake=.false.
      x=0.
      y=0.
      z=MAX(h,zmin)
      s=0.
      r=reff
      u=0.000002
      w=wexit
      tp=texit
      drdxa=0.0
      ipositn=4

c --- store stepping length
      ds0=ds

c --- introduce zcumul to track change in the vertical coordinate of the
c --- trajectory that arises from streamline inclination from horizontal
c --- this adjustment is applied outside of the system of rise equations
c --- out to a distance of 15r from downwind face of building
      zcumul=0.0
      r15src=xbadj+(xlb+15.*rb)

c --- get met. variables at release height
      call zmet(z,ua0,ra,ta,dudz0,dpdz)

c --- apply reduction in wake wind speed
      xb=x-xbadj
      yb=y-ybadj
      zb=MAX(z,zmin)
      call position(xb,yb,zb,ipositn)
      ufac=1.0
      dufac=0.0
      if(ipositn < 4) call wake_u(.false.,xb,yb,zb,ufac,dufac)
      ua=ufac*ua0
      dudz=ufac*dudz0+dufac*ua0

c --- use briggs plume rise estimates to set distance scale
c --- compute initial buoyancy flux (m**4/s**3)
      deltat=MAX(tp-ta,0.0)
      fdum=w*r*r/tp
      fb=gravi*fdum*deltat
c --- compute momentum flux (m**4/s**2)
      fm=w*fdum*ta
c --- final neutral rise distance
      uam=MAX(ua,1.0)
c --- momentum only: (do not base xmax on case where w<uam)
      wbyu=MAX(1.,w/uam)
      xmaxm=8.*r*wbyu*(1.+3./wbyu)**2
      if(fb <= 0.0)then
c ---    no buoyancy, momentum only
         xmax=xmaxm
      elseif(fb < 55)then
c ---    buoyancy flux < 55 m**4/s**3
         xmaxb=49.*fb**0.625
         xmax=MAX(xmaxm,xmaxb)
      else
c ---    buoyancy flux >= 55 m**4/s**3
         xmax=119*fb**0.4
      endif

c --- use briggs neutral rise to identify "minimal rise" cases
c --- compute briggs neutral final rise
      if(fb <= 0.0) then
c ---    no buoyancy, momentum only
         znf=6.*r*w/uam
      elseif(fb < 55.) then
c ---    buoyancy flux < 55 m**4/s**3
         znf=21.425*(fb**0.75)/uam
      else
c ---    buoyancy flux >= 55 m**4/s**3
         znf=38.71*(fb**0.60)/uam
      endif
c --- set minimum rise to 0.1 m
      znf0=MAX(0.1,znf)

c --- guard against step length greater than likely rise
      dmin=0.5*znf0
      if(ds > dmin) then
         ds=dmin
         if(ldb) then
            write(io6,*)'numrise - initial step reset'
            write(io6,*)'znf,ds0,ds  :',znf,ds0,ds
         endif
      endif

c --- indirect variables
      usc=SQRT(u*u+w*w)
      phi=ATAN(w/u)
c --- parameters
      np=nstep
      xnp=FLOAT(np)
      nnp=1

c --- start marching loop
      den=ra*ta/texit
      call lump(ua,ta,f)

c-----SCICHEM variable
      xplm     = xlb + 1.15*xlr + MAX(xbadj,0.0)
      fbini    = fb
      uplm     = 0.0
      wplm     = 0.0
      tplm     = 0.0 
      rplm     = 0.0

999   continue

c-----SCICHEM variables
      xplmold = x
      uplmold = u
      wplmold = w
      tplmold = tp 
      rplmold = r

c --- set local debug logical
      if(nnp < 150) then
         ldbnn=ldb
      else
         ldbnn=.false.
      endif

c --- define coordinates of plume relative to bldg.-defined origin
      xb=x-xbadj
      yb=y-ybadj
      zb=MAX(z+zcumul,zmin)
c --- obtain mean streamline slopes here (within 15r of building)
      dxds=0.0
      dzdx=0.0
      dzstrm=0.0
      call position(xb,yb,zb,ipositn)
      if(ipositn > 2 .and. x <= r15src) then
         call zstream(hb,wb,xlb,rb,xlr,hr,xb,yb,zb,dzdx)
         dxds=u/usc
         dzds=dzdx*dxds
         dzstrm=dzds*ds
      endif
c --- define the crosswind velocity component = zero
      dyds=0.0
      v=0.0

c --- compute rhs of rate equations for this location
      call rate(ua,dudz,ra,dpdz,ta,drdxa,rhs)

c --- predictor marching
      call marching(f,ftemp,rhs,ds)
      call unlump(ua,ta,ra,ftemp)

c --- extract met and apply reduction in wake wind speed
      zb=MAX(z+zcumul+dzstrm,zmin)
      call zmet(zb,ua0,ra,ta,dudz0,dpdz)
      call position(xb,yb,zb,ipositn)
      ufac=1.0
      dufac=0.0
      if(ipositn < 4) call wake_u(ldbu,xb,yb,zb,ufac,dufac)
      ua=ufac*ua0
      dudz=ufac*dudz0+dufac*ua0
      call rate(ua,dudz,ra,dpdz,ta,drdxa,rhstemp)

c --- corrector
      do i=1,7
         rhs(i)=0.5*(rhstemp(i)-rhs(i))
      end do
      call marching(ftemp,f,rhs,ds)
      call unlump(ua,ta,ra,f)

c --- compute incremental change in plume height to account for
c --- streamline ascent/descent, and add to cumulative change
      zcumul=zcumul+dzstrm
c --- apply cumulative adjustment to plume height
      zc=MAX(z+zcumul,zmin)
c --- define coordinates of plume relative to bldg.-defined origin
      xb=x-xbadj
      yb=y-ybadj
      zb=zc
      call position(xb,yb,zb,ipositn)

c --- numerical procedure may result in small negative downwind
c --- distance:  reset to zero and go to next step
      if(x < 0.0) then
         x=0.0
         s=s+ds
         nnp=nnp-1
         goto 96
      endif

c --- write debug output if in debug mode
      if(ldbnn)then
         if(MOD(nnp,1000) == 1)write(io6,112)
112      format(1x,2x,'nnp',7x,'x',6x,'y',6x,'z',6x,'r',6x,'u',5x,'v',
     1   6x,'w',4x,'usc',5x,'phi',4x,'den',5x,'tp',4x,'ua',5x,'ra',5x,
     2   'ta',4x,'dudz',5x,'dpdz',3x,'dzds',3x,'dyds',2x,'ipos',
     3   1x,'deltaz')
         deltaz=zc-h
         write(io6,114)nnp,x,y,zc,r,u,v,w,usc,phi,den,tp,ua,ra,ta,dudz,
     1    dpdz,dzds,dyds,ipositn,deltaz
114      format(1x,i5,f9.2,3f7.2,4f7.2,
     1    f8.4,f6.3,f7.2,f6.2,f7.3,f7.2,f9.4,
     2    f8.4,2f7.4,i5,f7.3)
      endif

c --- when trajectory inclination falls below 45 degrees, ignoring
c --- streamline descent, check for wake influence
      if(phi <= 0.7854 .and. ipositn < 4) then
         if(.not.linwake) then
c ---       plume centerline has just entered wake
            linwake=.true.
            xbi=xb
c ---       use unadjusted rise for bid
            base=MAX(h,zmin)
            rise=MAX(0.0,z-base)
            bidsq=(rise/3.5)**2
c ---       guard against x <= 0 due to precision
            if(x <= 0.0) then
               szi=SQRT(bidsq)
               syi=szi
            else
               call sigz(x,szi)
               szi=SQRT(szi**2+bidsq)
               call sigy(x,syi)
               syi=SQRT(syi**2+bidsq)
            endif

c --- normal debug output
            if(ldbhr) then
               write(io6,*)'numrise call to wake_dfsn'
               write(io6,*)'x,y,z,z+zcum:',x,y,z,zc
               write(io6,*)'ds,u,w      :',ds,u,w
               write(io6,*)'xb,phi      :',xb,phi
               write(io6,*)'szi,syi     :',szi,syi
            endif

c ---       compute table of sigmas and growth rate in wake region
            call wake_dfsn(ldb,xbi,szi,syi)
         endif
c ---    select plume radius growth rate for this location
         call wake_drdx(x,drdxa)
      endif

c --- process new position
      s=s+ds
      if(FLOAT(nnp/np) == FLOAT(nnp)/xnp) then
         nn=nnp/np
         if(nn > mxnw)then
            write(io6,*)'error in subr. numrise -- nn too large -- ',
     1      'nn = ',nn,' mxnw = ',mxnw
            stop
         endif
         xt(nn)=x
         yt(nn)=y
         zt(nn)=zc
         rt(nn)=r
c --- check for plume equilibrium height
         if(x >= xmax) then
            zfin=zc
            yfin=y
            xfin=x
            rfin=r
c  --- SCICHEM variables
            if ((xplm <= x).and.(xplm >= xplmold)) then
               facx  = (xplm-xplmold)/(x-xplmold)
               uplm  = uplmold + facx*(u-uplmold)
               wplm  = wplmold + facx*(w-wplmold)
               tplm  = tplmold + facx*(tp-tplmold)
               rplm  = rplmold + facx*(r-rplmold)
            end if
            goto 97
         endif
      endif

c --- extract met and apply reduction in wake wind speed
96    call zmet(zb,ua0,ra,ta,dudz0,dpdz)
      ufac=1.0
      dufac=0.0
      if(ipositn < 4) call wake_u(.false.,xb,yb,zb,ufac,dufac)
      ua=ufac*ua0
      dudz=ufac*dudz0+dufac*ua0

c --- next increment
      nnp=nnp+1

c  --- SCICHEM variables
         if ((xplm <= x).and.(xplm >= xplmold)) then
            facx  = (xplm-xplmold)/(x-xplmold)
            uplm  = uplmold + facx*(u-uplmold)
            wplm  = wplmold + facx*(w-wplmold)
            tplm  = tplmold + facx*(tp-tplmold)
            rplm  = rplmold + facx*(r-rplmold)
         end if

c --- stop rise at local maximum (excludes streamline descent effects)
      if(w < 0.0)then
         zfin=zc
         yfin=y
         xfin=x
         rfin=r
         go to 97
      endif

c --- adjust ds toward ds0 for next step
      if(ds < ds0) ds=MIN(ds0,2.*ds)

      if (s < slast) goto 999
      zfin=zc
      yfin=y
      xfin=x
      rfin=r

97    continue

c --- complete trajectory out to "15r" if required, to account for
c --- streamline slope (vertical only)
      xfin0=xfin
      x15r=r15src-xfin0
      if(x15r > 0.0) then
c ---    set stepsize
         dsfin=nstep*ds
         dx15r=x15r/(mxnw-nn)
         dx15r=MAX(dsfin,dx15r)
c ---    set range for additional steps
         n15r=MIN(INT(x15r/dx15r),mxnw-nn)
         nbeg=nn+1
         nend=nn+n15r
         do in=nbeg,nend
c ---       define coordinates of plume relative to bldg.-defined origin
            xbb=xt(in-1)-xbadj
            xbe=xbb+dx15r
            yb=yt(in-1)-ybadj
            zb=zt(in-1)
c ---       obtain mean streamline slope
            dzdx=0.0
            call position(xbb,yb,zb,ipos)
            if(ipos > 2) then
               call zstream(hb,wb,xlb,rb,xlr,hr,xbb,yb,zb,dzdxb)
               call zstream(hb,wb,xlb,rb,xlr,hr,xbe,yb,zb,dzdxe)
               dzdx=0.5*(dzdxb+dzdxe)
            endif
            xt(in)=xt(in-1)+dx15r
            yt(in)=yfin
            zt(in)=MAX(zmin,zt(in-1)+dzdx*dx15r)
            rt(in)=rfin
            zcumul=zcumul+dzdx*dx15r

c ---       check for wake entry if this has not already happened
            if(.not.linwake) then
               if(ipos < 4) then
c ---             plume centerline has just entered wake
                  linwake=.true.
c ---             set "internal" variable names
                  x=xt(in)
                  z=zt(in)-zcumul
                  xbi=x-xbadj
c ---             use unadjusted rise for bid
                  base=MAX(h,zmin)
                  rise=MAX(0.0,z-base)
                  bidsq=(rise/3.5)**2
                  call sigz(x,szi)
                  szi=SQRT(szi**2+bidsq)
                  call sigy(x,syi)
                  syi=SQRT(syi**2+bidsq)

c --- normal debug output
                  if(ldbhr) then
                     write(io6,*)'numrise call to wake_dfsn'
                     write(io6,*)'x,y,z,z+zcum:',x,yfin,z,zt(in)
                     write(io6,*)'xb,phi      :',xbi,phi
                     write(io6,*)'szi,syi     :',szi,syi
                  endif

c ---             compute table of sigmas and growth rate in wake region
                  call wake_dfsn(ldb,xbi,szi,syi)
               endif
c ---          select plume radius growth rate for this location
               call wake_drdx(x,drdxa)
            endif

         end do
c ---    update nn and reset "fin" data
         nn=nend
         xfin=xt(nn)
         zfin=zt(nn)
      endif

c --- construct trajectory arrays for calling program
      if(nn > ntr) then
c ---    sample a subset of the nn points
         xtr(ntr)=xfin
         ytr(ntr)=yfin
         ztr(ntr)=zfin
         rtr(ntr)=rfin
         if(nn <= 2*ntr) then
c ---       fill elements with nearest values
            deln=FLOAT(nn)/FLOAT(ntr)
            do in=1,ntr-1
               jn=in*deln
               xtr(in)=xt(jn)
               ytr(in)=yt(jn)
               ztr(in)=zt(jn)
               rtr(in)=rt(jn)
            end do
         else
c ---       use sliding step-size to sample nearfield more frequently
            deln=2.*FLOAT(nn-ntr)/FLOAT(ntr*(ntr-1))
            rn=0.0
            do in=1,ntr-1
               rn=rn+1.0+(in-1)*deln
               jn=rn
               xtr(in)=xt(jn)
               ytr(in)=yt(jn)
               ztr(in)=zt(jn)
               rtr(in)=rt(jn)
            end do
         endif
      else
c ---    fill elements directly
         do in=1,nn
            xtr(in)=xt(in)
            ytr(in)=yt(in)
            ztr(in)=zt(in)
            rtr(in)=rt(in)
         end do
c ---    fill excess elements with final rise properties
         do it=nn+1,ntr
            xtr(it)=xfin
            ytr(it)=yfin
            ztr(it)=zfin
            rtr(it)=rfin
         end do
      endif

c --- restore step size (may have changed)
      ds=ds0

c --- determine maximum fraction of plume captured in cavity
      if(linwake .and. xbi < (xlb+xlr)) then
c ---    plume centerline enters wake boundary before clearing cavity
         call wake_fqc(ldb,xbi,xtr,ztr,mxntr)
      else
         fqcav=0.0
      endif

c --- normal debug output
      if(ldbhr) then
         delzfin=zfin-h
         write(io6,*)
         write(io6,*)'      initial plume temperature = ',texit
         write(io6,*)'             buoyancy flux (fb) = ',fb
         write(io6,*)'             momentum flux (fm) = ',fm
         write(io6,*)'  neutral dist. to final rise   = ',xmax
         write(io6,*)'  calc distance to final rise   = ',xfin0
         write(io6,*)'distance from final rise to 15r = ',x15r
         write(io6,*)'total distance tabulated (xfin) = ',xfin
         write(io6,*)'    final y displacement (yfin) = ',yfin
         write(io6,*)'      final plume height (zfin) = ',zfin
         write(io6,*)'     final plume rise (delzfin) = ',delzfin
         write(io6,*)'      final plume radius (rfin) = ',rfin
         write(io6,*)'cumul. streamline adj. (zcumul) = ',zcumul
         write(io6,*)
         write(io6,*)'    fraction of plume in cavity = ',fqcav
         write(io6,*)
          
      endif
c

c --- extended debug output
      if(ldb) then
c ---    write the arrays passed back to the calling routine
         write(io6,28)
28       format(/4x,'i',10x,'xtr',8x,'ytr',8x,'ztr',8x,'rtr',8x,'sz?'/)
         do i=1,ntr
            write(io6,32)i,xtr(i),ytr(i),ztr(i),rtr(i),(rtr(i)*0.8)
32          format(i5,3x,5(f10.4,1x))
         end do
         write(io6,*)
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine rate(ua,dudz,ra,dpdz,ta,drdxa,rhs)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812                  rate
c                x. zhang, j. scire,   earth tech
c
c                adapted from calpuff routine rate
c                for epri under contract wo3527-01
c
c --- purpose:  compute the right-hand side of the equations
c
c --- inputs:
c         ua - real    - current ambient wind speed (m/s)
c       dudz - real    - current wind shear (1/s)
c         ra - real    - current ambient air density (kg/m**3)
c       dpdz - real    - current ambient potential temperature gradient
c                        (deg. k/m)
c         ta - real    - current ambient air temperature (deg. k)
c     alpha0 - real    - plume entrainment coefficient (parallel)
c      drdxa - real    - growth rate of plume radius due to ambient turb
c
c     common block /plu/ variables:
c           x,r,u,v,w,usc,phi,den,tp
c     common block /numparm/ variables:
c           gravi, rp,
c           nent, alphap(mxent), betap(mxent), xcat(mxentp1),
c     parameters:
c           mxent, mxentp1
c
c --- output:
c        rhs(7) - real     - right hand terms
c
c --- rate called by:  numrise
c --- rate calls:      none
c----------------------------------------------------------------------
c --- include files
      use numparm_pri
      common /plu/ s,x,y,z,r,u,v,w,usc,phi,den,tp
      save /plu/
      dimension rhs(7)
c ---   constants:
c          gravi - gravitational acceleration (m/s**2)
c          rp    - radiation coefficient (kg/m**2/deg. k**3/s)
c --- set default entrainment coefficients
      data alpha0/.11/, beta0/0.6/
c --- define the entrainment coefficients
      alpha=alpha0
      beta=beta0
      if(nent > 0)then
c ---    check if the plume is in the area where perturbed entrainment
c ---    coefficients apply
         if(x < xcat(1))go to 99
         nentp1=nent+1
         do i=2,nentp1
            if(x <= xcat(i))then
               alpha=alphap(i-1)
               beta=betap(i-1)
               go to 99
            endif
         end do
c ---    override any ambient growth rate
         drdxa=0.0
      endif
99    continue
      rhs(1)=2.0*r*alpha*ra*ABS(usc-ua*u/usc)+
     1            2.0*r*beta*ra*ABS(ua*sin(phi))
c
c ---   condition entrainment to be .ge. growth due to ambient turb.
      rhs1a =2.0*r*ra*ua*drdxa
      rhs(1)=MAX(rhs(1),rhs1a)
c
      rhs(2)=-r*r*den*w*dudz
      rhs(3)=gravi*r*r*(ra-den)
      rhs(4)=-dpdz*den*w*r*r-rp*r*(tp**4-ta**4)
      rhs(5)=w/usc
      rhs(6)=u/usc
      rhs(7)=v/usc
      return
      end
c----------------------------------------------------------------------
      subroutine lump(ua,ta,f)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812                  lump
c                x.(j.) zhang, j. scire,  earth tech
c
c --- purpose:  calculate the lumped variables
c
c --- inputs:
c         ua - real    - current ambient wind speed (m/s)
c         ta - real    - current ambient air temperature (k)
c
c --- output:
c          f(7) - real     - lumped variables
c
c --- lump called by:  numrise
c --- lump calls:      none
c----------------------------------------------------------------------
      common /plu/ s,x,y,z,r,u,v,w,usc,phi,den,tp
      save /plu/
      dimension f(7)
      f(1)=den*usc*r*r
      f(2)=f(1)*(u-ua)
      f(3)=f(1)*w
      f(4)=f(1)*(tp-ta)
      f(5)=z
      f(6)=x
      f(7)=y
      return
      end
c----------------------------------------------------------------------
      subroutine marching(fold,fnew,rhs,ds)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812              marching
c                x.(j.) zhang, j. scire,  earth tech
c
c --- purpose:  marching s one step, either predictor or corrector
c
c --- inputs:
c       fold(7) - real     - old values
c        rhs(7) - real     - right hand terms
c            ds - real     - distance (m) along plume axis
c
c
c --- output:
c       fnew(7) - real     - new values
c
c --- marching called by:  numrise
c --- marching calls:      none
c----------------------------------------------------------------------
        dimension fnew(7),fold(7),rhs(7)
        do i=1,7
           fnew(i)=fold(i)+rhs(i)*ds
        end do
        return
        end
c----------------------------------------------------------------------
      subroutine unlump(ua,ta,ra,f)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812                unlump
c                x.(j.) zhang, j. scire,  earth tech
c
c --- purpose:  calculate physical variables from lumped variables
c
c --- inputs:
c         ua - real    - current ambient wind speed (m/s)
c         ta - real    - current ambient air temperature (k)
c         ra - real    - current ambient air density (kg/m^3)
c       f(7) - real    - lumped variables
c
c --- output:
c       common /plu/:
c          u,v,w,usc,r,tp,phi,z,y,x
c
c --- unlump called by:  numrise
c --- unlump calls:      none
c----------------------------------------------------------------------
        common /plu/ s,x,y,z,r,u,v,w,usc,phi,den,tp
        save /plu/
        dimension f(7)
        u=ua+f(2)/f(1)
        w=f(3)/f(1)
        usc=SQRT(u*u+w*w)
        tp=ta+f(4)/f(1)
        den=ra*ta/tp
        r=SQRT(f(1)/usc/den)
        phi=ATAN(w/u)
        z=f(5)
        x=f(6)
        y=f(7)
        return
        end
c----------------------------------------------------------------------
      subroutine zmet(z,ua,ra,ta,dudz,dpdz)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812                  zmet
c                x.(j.) zhang, j. scire,  earth tech
c
c --- purpose:  obtain ambient met parameters at height z
c               by interpolation of gridded values
c
c --- inputs:
c          z - real    - height (m)
c
c     common block /ambient/ variables:
c           nza,uamb(mxnz),ramb(mxnz),tamb(mxnz),zfacea(mxnzp1),
c           zgpta(mxnz),tamb0,ramb0,adia,ptgrad0
c     parameters:
c           mxnz, mxnzp1
c
c --- output:
c         ua - real    - current ambient wind speed (m/s)
c         ra - real    - current ambient air density (kg/m**3)
c         ta - real    - current ambient air temperature (deg. k)
c       dudz - real    - current wind shear (1/s)
c       dpdz - real    - current ambient potential temperature gradient
c                        (deg. k/m)
c
c --- zmet called by:  numrise
c --- zmet calls:      none
c----------------------------------------------------------------------
c     defined at grid center: uamb,tamb,ramb
c     defined at zface:       dedz
c----------------------------------------------------------------------
c --- include files
      use ambient_pri

c --- interpolate variables defined at grid cell center
      if (z < zgpta(1))then

c ---    height is below first grid point
         zfact=(zgpta(1)-z)/zgpta(1)
         ta=tamb(1)-(tamb(1)-tamb0)*zfact
         ra=ramb(1)-(ramb(1)-ramb0)*zfact
c ---    wind speed at z=0 is assumed to be zero
         ua=uamb(1)*(1.0-zfact)
         dudz=uamb(1)/zgpta(1)
         dpdz=adia+(tamb(1)-tamb0)/zgpta(1)
         dpdz=MAX(dpdz,ptgrad0)

      else if(z < zgpta(nza))then

c ---    find the layer containing height, z
         do i=2,nza
            if (z <= zgpta(i))then
               im1=i-1
               delz=zgpta(i)-zgpta(im1)
               zfact=(zgpta(i)-z)/delz
               ta=tamb(i)-(tamb(i)-tamb(im1))*zfact
               ra=ramb(i)-(ramb(i)-ramb(im1))*zfact
               ua=uamb(i)-(uamb(i)-uamb(im1))*zfact
c ---          compute wind speed gradient & pot. temp. gradient
               dudz=(uamb(i)-uamb(im1))/delz
               dpdz=adia+(tamb(i)-tamb(im1))/delz
               dpdz=MAX(dpdz,ptgrad0)
               go to 101
            endif
         end  do

      else

c ---    height is at or above the top grid point -- persist values
c ---    at the top grid cell
         ta=tamb(nza)
         ra=ramb(nza)
         ua=uamb(nza)
c ---    hold wind speed and temperature constant above top layer at
c ---    values at top grid point
         dudz=0.0
         dpdz=adia
         dpdz=MAX(dpdz,ptgrad0)
      endif

101   continue

      return
      end
c----------------------------------------------------------------------
      subroutine nummet(ws,zanem,p,tsurf,ptgrad,ldbhr)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812                nummet
c                j. scire, earth tech
c
c --- purpose:  initialize the variables used by the numerical
c               plume rise algorithm
c
c --- inputs:
c
c         ws - real    - wind speed (m/s) measured at anemometer ht.
c      zanem - real    - anemometer height (m)
c          p - real    - power law wind shear exponent (dimensionless)
c      tsurf - real    - surface ambient air temperature (deg. k)
c     ptgrad - real    - ambient potential temperature lapse rate (k/m)
c      ldbhr - logical - debug switch controlling debug output
c
c     common block /ambient/ variables:
c           nza,zfacea(mxnzp1),zgpta(mxnz),adia,ptgrad0
c     common block /numparm/ variables:
c           gravi,rgas
c     parameters:
c           mxnz, mxnzp1
c
c --- output:
c
c     common block /ambient/ variables:
c           uamb(mxnz),ramb(mxnz),tamb(mxnz),dedz(mxnzp1),
c           tamb0,ramb0
c
c --- nummet called by:  prime
c --- nummet calls:      none
c----------------------------------------------------------------------

c --- include common blocks
      use ambient_pri
      use numparm_pri

      logical ldbhr

c --- set the surface temperature (deg. k) & air density (kg/m**3)
      tamb0=tsurf
      ramb0=1.2

c --- extrapolate the winds to the grid point levels
      do i=1,nza
         uamb(i)=ws*(zgpta(i)/zanem)**p
      end do

c --- compute the temperature at each grid point

c ---    do not allow the potential temp. gradient < min. value
      gradact=MAX(ptgrad,ptgrad0)
      do i=1,nza
            tamb(i)=tamb0+(gradact-adia)*zgpta(i)
      end do

c ---    compute the potential temperature lapse rates at cell faces
      nzap1=nza+1
      do i=2,nza
         dedz(i)=adia+(tamb(i)-tamb(i-1))/(zgpta(i)-zgpta(i-1))
      end do
      dedz(1)=adia+(tamb(1)-tamb0)/zgpta(1)
      dedz(nzap1)=dedz(nza)

c --- create ambient density profile on grid center
      do i=1,nza
c ---    compute average temperature in layer
         tbar=0.5*(tamb(i)+tamb0)
c ---    compute ambient air density at height, zgpta(i)
         ramb(i)=ramb0*(tamb0/tamb(i))*exp(-gravi*zgpta(i)/
     1    (rgas*tbar))
      end do
c
c ----------------
c --- debug output
c ----------------
      if(ldbhr)then

         write(io6,*)
         write(io6,*)'ws      = ',ws,' (m/s)'
         write(io6,*)'zanem   = ',zanem,' (m)'
         write(io6,*)'p       = ',p
         write(io6,*)'tsurf   = ',tsurf,' (deg. k)'
         write(io6,*)'ptgrad  = ',ptgrad,' (deg. k/m)'
         write(io6,*)'nza     = ',nza
         write(io6,*)'adia    = ',adia,' (deg. k/m)'
         write(io6,*)'ptgrad0 = ',ptgrad0,' (deg. k/m)'
         write(io6,*)

         write(io6,*)'tamb0   = ',tamb0,' (deg. k)'
         write(io6,*)'ramb0   = ',ramb0,' (kg/m**3)'
         write(io6,*)

         write(io6,8)
8        format(/4x,'i',8x,'zgpta',7x,'uamb',7x,'tamb'/)
         do i=1,nza
            write(io6,10)i,zgpta(i),uamb(i),tamb(i)
10          format(i5,3x,f10.1,1x,f10.2,1x,f10.2)
         end do

c ---    echo back pot. temp. gradient & air density
         write(io6,18)
18       format(/4x,'i',7x,'zfacea',7x,'dedz',6x,'zgpta',7x,'ramb'/)
         do i=1,nza
            write(io6,22)i,zfacea(i),dedz(i),zgpta(i),ramb(i)
22          format(i5,3x,f10.1,1x,f10.5,1x,f10.1,1x,f10.3)
         end do
         write(io6,22)nzap1,zfacea(nzap1),dedz(nzap1)
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine zstream(h,w,l,r,lr,hr,x,y,z,dzdx)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812               zstream
c                l. schulman, j. scire,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  estimates the local mean slope of streamlines in the
c               vicinity of buildings.  the local slope is equal to
c               w/u, where w is the local mean vertical velocity and
c               u the local mean longitudinal velocity.  for modeling
c               plume rise, the streamline deflection effect is modeled
c               as (w/u)(dx).
c
c --- inputs:
c                h - real              - building height above ground
c                w - real              - projected building width
c                l - real              - along-wind building length
c                r - real              - scale length from h and w
c               lr - real              - length of downwind cavity from
c                                         lee face
c               hr - real              - maximum cavity height above
c                                         ground
c                x - real              - downwind distances
c                y - real              - crosswind distances
c                z - real              - heights above ground
c
c --- output:
c             dzdx - real              - vertical streamline slope
c
c --- zstream called by:  numrise
c --- zstream calls:      none
c----------------------------------------------------------------------
c
      real h,w,l,r,hr,lr,expz1,expz2,expzg,expx
      real x,y,z,dzdx2,zslope,dzdx,ypos
      data expx/1./,expz1/3./,expz2/1./
c
c --- check for a building
      if(h <= 0.0)then
         dzdx=0.0
         go to 900
      endif

c --- set a few constants
      hbyw=h/w
      ypos=ABS(y)
      onebyr=1.0/r
      wby2=0.5*w
      rby2=0.5*r

c --- power law exponent for slope approaching zero at ground
c --- exponent modified for tall, narrow buidings
c --- zg is level below which slope is zero in far wake
      zg=0.
      expzg=0.3
      if(hbyw >= 2.) expzg=expzg*(0.5*hbyw)**2

c
c --- local streamline slope (zslope) at z=h
c --- local two-dimensional streamline slope (dzdx2)
c --- local three-dimensional streamline slope (dzdx)
c --- (x,y,z)=(0,0,0) at ground of center of upwind building face
c
      if(x < -r) then
c ---    upwind of building influence
         zslope = 0.
         dzdx2  = 0.

      elseif(x < 0.) then
c ---    ascent upwind of building:
c ---    parobolic fit to slope=0 at (-r,0) with increasing slope
c ---    to (0,(hr-h))
c ---    vertical decay above building using expz1
c ---    below building nonzero slope above 2/3 h for r<h reducing
c ---    to ground as r approaches 2h
         zslope = 2.*(hr-h)*(x+r)*onebyr**2.
         if(z > h) then
            dzdx2 = zslope/((z-h+r)*onebyr)**expz1
         elseif(r <= h .and. z <= 0.67*h) then
            dzdx2 = 0.
         elseif(r <= h .and. z > 0.67*h) then
            dzdx2 = zslope
         elseif(r > h .and. z <= 0.67*(2*h-r)) then
            dzdx2 = 0.
         elseif(r > h .and. z > 0.67*(2*h-r)) then
            dzdx2 = zslope
         else
            print *,'z out of bounds      ',x,z
         endif

      elseif (x <= rby2) then
c ---    ascent over building
c ---    parobolic fit from (0,0) with decreasing slope to
c ---    to (0.5r,(hr-h))
c ---    vertical decay above building using expz1
         zslope = (-(hr-h)*4.*onebyr)*(2.*x*onebyr-1.)
         if(z <= h) then
            dzdx2 = zslope
         else
            dzdx2 = zslope/((z-h+r)*onebyr)**expz1
         endif

      elseif (x <= l+lr) then
c ---    descent over building to end of near wake
c ---    parobolic fit from (.5r,(hr-h)) with increasing slope to
c ---    to (l+lr,-h/2)
c ---    vertical decay above z=h using expz2
c ---    vertical decay below z=h using expzg
         zslope = (hr-h)*(r-2.*x)/((l-rby2+lr)**2)
         if(z > h) then
            dzdx2 = zslope/((z-h+r)*onebyr)**expz2
         elseif(z <= zg) then
            dzdx2 = 0.
         else
            dzdx2 = zslope*((z-zg)/(h-zg))**expzg
         endif

      else
c ---    descent beyond near wake (far wake)
c ---    horizontal decay beyond l+lr using expx
c ---    vertical decay above z=h using expz2
c ---    vertical decay below z=h using expzg
         zslopelr  = -2.*(hr-h)/(l-rby2+lr)
         zslope = zslopelr/((x-(l+lr-r))*onebyr)**expx
         if(z > h) then
            dzdx2 = zslope/((z-h+r)*onebyr)**expz2
         elseif(z <= zg) then
            dzdx2 = 0.
         else
            dzdx2 = zslope*((z-zg)/(h-zg))**expzg
         endif

      endif

c --- calculate 3-d slopes,: dzdx : from 2-d centerplane slope,: dzdx2
      if(ypos > (wby2+r/3.))then
         dzdx=0.
      elseif(ypos <= wby2)then
         dzdx=dzdx2
      else
         yscale=1.+(3.*onebyr)*(wby2-ypos)
         dzdx=dzdx2*yscale
      endif

900   continue
      return
      end
c-----------------------------------------------------------------------
      subroutine position(x,y,z,ipositn)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812              position
c                l. schulman, j. scire, d. strimaitis,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  identifies if (x,y,z) location is in the building,
c               in the near wake, in the far wake, or outside.
c               ipositn is set to: 1 if within the bldg.
c                                  2 if within the near wake
c                                  3 if within the far wake
c                                  4 if outside wake region
c
c --- inputs:
c                x - real              - downwind distance from center
c                                        of upwind face of bldg
c                y - real              - crosswind distance from center
c                                        of upwind face of bldg
c                z - real              - height above ground
c
c     common block /wakedat/ variables:
c           hb,wb,xlb,rb,hr,xlr,xlc
c
c --- output:
c
c          ipositn - integer           - 1 if (x,y,z) within building
c                                        2 if location within near wake
c                                        3 if location within far wake
c                                        4 if location outside
c
c --- position called by:  numrise, pcalc (host subroutine)
c --- position calls:      wake_dim
c----------------------------------------------------------------------
c

c --- include commons
      use wakedat_pri

      data zero/0.0/, third/0.333333/, half/0.5/

c --- define a fractional boundary just inside building
      data skin/0.99998/

c --- initialize
      iposy=4
      iposz=4
      ipositn=4

c --- screen out any cases without building
      if(hb <= zero) return

c --- screen out positions upwind of building (and on windward face)
      if(x <= zero) return

c --- set y positive for calculations
      ypos=ABS(y)

c --- set selected length scale products
      rby2=half*rb
      rby3=third*rb
      wby2=half*wb

c --- set ipositn to 1 if location within building
      xtest=x/xlb
      ytest=ypos/wby2
      ztest=z/hb
      if(xtest < skin .and. ztest < skin .and. ytest < skin) then
         ipositn=1
         return
      endif

c --- calculate if location below height of near wake boundary
      if(xlc < xlb)then
c ---    reattachment
         if(x < xlb) then
c ---       cavity height equal to building height
            zcav=hb
            if(z <= zcav) iposz=2
         elseif(x < (xlb+xlr)) then
c ---       cavity height is ellipse with a=lr and b=h
            zcav=hb*SQRT(1.-((x-xlb)/xlr)**2)
            if(z <= zcav) iposz=2
         endif
      else
c ---    no reattachment
         if(x <= rby2) then
c ---       cavity height is parabola with vertex at height max(0.5r,hr)
c ---       and passing thru upwind building edge (0,h)
            zcav=hr+4.*(x-rby2)**2*(hb-hr)/(rb**2)
            if(z <= zcav) iposz=2
         elseif(x < (xlb+xlr)) then
c ---       cavity height is ellipse with a=lr+l-0.5r and b=hr
            zcav=hr*SQRT(1.-((x-rby2)/(xlb+xlr-rby2))**2)
            if(z <= zcav) iposz=2
         endif
      endif

c --- calculate x-y near wake boundary
      if(x <= rb) then
c ---    cavity width is parabola with vertex @ width max(r,w/2+r/3)
c ---    and passing thru upwind building edge (0,w/2)
         ycav=(wby2+rby3)-(x-rb)**2/(3.*rb)
         if(ypos <= ycav) iposy=2
      elseif(x < (xlb+xlr)) then
c ---    cavity width is ellipse with a=w/2+r/3 and b=lr+l-r
         ycav=(wby2+rby3)*SQRT(1.-((x-rb)/(xlb+xlr-rb))**2)
         if(ypos <= ycav) iposy=2
      endif

c --- set ipositn to 2 if (x,y,z) location within near wake
      if( iposz == 2 .and. iposy == 2) ipositn=2

c --- test for position in far wake if still 4
      if(ipositn == 4) then
         call wake_dim(x,hb,wb,rb,zwake,ywake)
         if(z <= zwake .and. ypos <= ywake) ipositn=3
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine numgrad(x,xtr,ztr,ntr,zeff)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812               numgrad
c                j. scire,  earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  compute the effective gradual plume height by
c               interpolation of the stored values.  effective
c               plume height is the stack height + plume rise.
c
c --- inputs:
c                x - real       - downwind distance (m)
c         xtr(ntr) - real array - array of stored downwind distances (m)
c         ztr(ntr) - real array - array of stored effective plume height
c                                 at each downwind distance
c              ntr - integer    - number of stored values in xtr, ztr
c
c --- output:
c             zeff - real       - effective plume height (m) at
c                                 downwind distance x
c
c --- numgrad called by:  pheff
c --- numgrad calls:      none
c----------------------------------------------------------------------
c
      real xtr(ntr),ztr(ntr)
c
      if(x >= xtr(ntr))then
         zeff=ztr(ntr)
      else
         ntrm1=ntr-1
         zeff=ztr(1)
         do i=ntrm1,1,-1
            if (x >= xtr(i))then
               ip1=i+1
               zeff=ztr(ip1)-(ztr(ip1)-ztr(i))*(xtr(ip1)-x)/
     1           (xtr(ip1)-xtr(i))
               return
            endif
         end do
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine wake_drdx(x,drdx)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812             wake_drdx
c                j. scire, d. strimaitis,  earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  compute the plume radius growth rate in the wake
c               by interpolating among the stored values.
c
c --- inputs:
c                x - real       - downwind distance (m) from source
c
c     common block /params/ variables:
c           mxntr
c     common block /wakedat/ variables:
c           nwak, xwak(mxntr), drwak(mxntr)
c
c --- output:
c             drdx - real       - rate of growth of plume radius at
c                                 downwind distance x from source
c
c --- wake_drdx called by:  numrise
c --- wake_drdx calls:      none
c----------------------------------------------------------------------
c
      use wakedat_pri
c
c --- set growth rate to zero outside interpolation region
c --- (all x outside wake)
      if(x > xwak(nwak) .or. x < xwak(1))then
         drdx=0.0
      elseif(nwak <= 1) then
c ---    wake turbulence does not alter this plume
         drdx=0.0
      else
         nwkm1=nwak-1
         drdx=drwak(1)
         do i=nwkm1,1,-1
            if(x >= xwak(i))then
               ip1=i+1
               drdx=drwak(ip1)-(drwak(ip1)-drwak(i))*(xwak(ip1)-x)/
     1              (xwak(ip1)-xwak(i))
               return
            endif
         end do
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine wake_ini(ldbhr,kst,rural,dsbh,dsbw,dsbl,
     &                    xadj,yadj,ubldg,ustack)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812              wake_ini
c                d. strimaitis, earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  refreshes variables in /wakedat/ common
c
c --- inputs:
c
c      ldbhr - logical - debug output written when .true.
c        kst - integer - pg stability class
c      rural - logical - denotes rural dispersion when .true.
c       dsbh - real    - effective building height (m)
c       dsbw - real    - effective building width (m) across flow
c       dsbl - real    - effective building length (m) along flow
c       xadj - real    - distance (m) from source to upwind face of bldg
c                        along flow
c       yadj - real    - distance (m) from source to center of upwind
c                        face of bldg across flow
c      ubldg - real    - wind speed (m/s) at top of building
c     ustack - real    - wind speed (m/s) at release height
c
c     parameters:
c           mxntr
c
c --- output:
c
c     common block /wakedat/ variables:
c           hb,wb,xlb,rscale,hr,xlr,xlc,xbadj,ybadj,
c           nwak, xwak(mxntr), szwak(mxntr), sywak(mxntr),
c           drwak(mxntr), xzvwak, xyvwak, ub, urh,
c           lrurl, istab
c
c --- wake_ini called by:  pcalc (host subroutine)
c --- wake_ini calls:      wake_scales
c----------------------------------------------------------------------

c --- include common blocks
      use wakedat_pri

      logical rural,ldbhr
      data zero/0.0/

c --- transfer arguments to /wakedat/ variables
      istab =kst
      lrurl =rural
      hb    =dsbh
      wb    =dsbw
      xlb   =dsbl
      xbadj =xadj
      ybadj =yadj
      ub    =ubldg
      urh   =ustack

c --- compute wake dimensions and related parameters
      call wake_scales(ldbhr)

c --- reset contents of sigma arrays for wake region
      nwak=1
      xwak(1)=zero
      szwak(1)=zero
      sywak(1)=zero
      drwak(1)=zero

c --- reset virtual distances for sigmas beyond wake
      xzvwak=zero
      xyvwak=zero

      return
      end
c-----------------------------------------------------------------------
      subroutine wake_scales(ldbhr)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812           wake_scales
c                l. schulman, d. strimaitis,  j. scire,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  calculates length scale and wake dimensions
c
c --- inputs:
c            ldbhr - logical           - control variable for debug
c                                        write statements
c
c     common block /wakedat/ variables:
c           hb,wb,xlb
c     parameters:  io6
c
c --- output:
c
c     common block /wakedat/ variables:
c           rb,hr,xlr,xlc
c
c --- wake_scales called by:  wake_ini
c --- wake_scales calls:      none
c----------------------------------------------------------------------

c --- include commons
      use wakedat_pri

      logical ldbhr

c --- set misc. constants
      data third/0.3333333/, twoby3/0.6666667/

      if(hb <= 0.) then
c ---    no building
         rb=0.0
         hb=0.0
         xlr=0.0
         xlc=0.0
      else
c
c ---    set ratios
         rw = wb/hb
         rl = xlb/hb
c ---    fackrell limits on aspect ratio l/h
         if(rl < 0.3) rl=0.3
         if(rl > 3.0) rl=3.0
c
c ---    length scale r --- wilson
c ---    wilson limits for length scale r
c ---    h/w or w/h not greater than 8 --  already behaves as 2-d
         hh=hb                    ! only modify h to calculate r
         ww=wb                    ! only modify w to calculate r
         if(hh > 8.0*ww)hh=8.0*ww
         if(ww > 8.0*hh)ww=8.0*hh
         rb= (MIN(hh,ww)**twoby3) * (MAX(hh,ww)**third)
c
c ---    reattachment for lc < l
         xlc = 0.9*rb
c
c ---    recirculation cavity length---fackrell
c ---    modify fackrell for w/h less than 1 by weakening dependence
c ---    on l/h.  snyder found that cavity did not increase in length
c ---    as w/h = l/h decreased from 1 to 0.33.
c ---    let l/h dependence decrease from fackrell dependence at w/h=1
c ---    to no dependence at w/h=0.33.
         explh = 0.3
         if(rw < 1.) explh=MAX(0.0,0.3*(rw-0.33)/0.67)
         xlr = 1.8*wb/(rl**explh*(1.+0.24*rw))
c
c ---    maximum cavity height  (wilson,ashrae):
         hr = hb+0.22*rb

      endif

c --- write the results
      if(ldbhr)then
         write(io6,*)
         write(io6,*)'wake_scales inputs: '
         write(io6,*)'   hb    = ',hb,' (m)'
         write(io6,*)'   wb    = ',wb,' (m)'
         write(io6,*)'   lb    = ',xlb,' (m)'
         write(io6,*)
         write(io6,*)'wake_scales output: '
         write(io6,*)'   scale length (r)               = ',rb
         write(io6,*)'   max. cavity height (hr)        = ',hr
         write(io6,*)'   length of downwind cavity (lr) = ',xlr
         write(io6,*)'   length of roof cavity (lc)     = ',xlc
         write(io6,*)
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine wake_dfsn(ldbhr,xi,szi,syi)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812             wake_dfsn
c                l. schulman, d. strimaitis,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose: tabulates sigmas and rate of plume growth as function
c              of location within the wake from modified weil (1996)
c              analytical expressions
c
c --- inputs:
c            ldbhr - logical     - flag for debug write statements
c                                  to upwind bldg wall
c               xi - real        - distance (m) from upwind bldg wall
c                                  to point where plume intersects wake
c              szi - real        - sigma-z (m) at xi
c              syi - real        - sigma-y (m) at xi
c
c     common block /params/ variables:
c           mxntr, mxnw
c     common block /wakedat/ variables:
c           xbadj, hb, wb, xlb, rb, xlr
c
c --- output:
c
c     common block /wakedat/ variables:
c           nwak, xwak(mxntr), szwak(mxntr), sywak(mxntr),
c           drwak(mxntr), xzvwak, xyvwak,
c           ncav, xcav(mxntr), szcav(mxntr), sycav(mxntr),
c           xzvcav, xyvcav, lrurl, istab
c
c --- wake_dfsn called by:  numrise
c --- wake_dfsn calls    :  sigz, sigy, xvz, xvy,
c                           wake_xa, wake_cav0, wake_turb, wake_sig
c----------------------------------------------------------------------
c
      use numparm_pri
      use wakedat_pri

c --- define local variable arrays for fine-steps
      real dist(mxnw),asigz(mxnw),asigy(mxnw),dsz(mxnw)
      real csigz(mxnw),csigy(mxnw)

      logical ldbhr,lcav,lwak,lrevcav

c --- misc. constants
      data zero/0.0/, half/0.5/, one/1.0/
      data rtpiby2/1.253314/

c --- compute xa, where turbulent growth in the wake transitions
c --- to ambient growth rate, measured from upwind face of bldg
      call wake_xa(istab,lrurl,xlb,rb,xaz,xay)
      xamx=MAX(xaz,xay)
      xamn=MIN(xaz,xay)

c --- initialize cavity parameters
c --------------------------------
c --- set distance from upwind face of bldg to end of cavity
      xcave=xlb+xlr
c --- set distance from source to start of cavity
      distc=xlb+xbadj
c --- set downwind distance to effective cavity source (when present),
c --- from the upwind face of bldg
      xbc=MAX(xi,xlb)
      xbc=MIN(xbc,xcave)
c --- location of downwind edge of pdf region from effective cavity
c --- source
      xdc=xbc+xlr
c --- set initial sigmas for cavity source using sigma-y at xi
      call wake_cav0(syi,szcav0,sycav0)
c --- the cavity sigma-y will need to be revised if xi lies upwind of
c --- the downwind face of the bldg.
      if(xi < xlb) then
         lrevcav=.true.
      else
         lrevcav=.false.
      endif

c --- determine if any plume material in cavity may be modeled
c ------------------------------------------------------------
c --- initialize output arrays
      ncav=1
      xcav(1)=xbc+xbadj
      szcav(1)=szcav0
      sycav(1)=sycav0
c --- compute corresponding virtual distances
      xasrc=xcav(1)
      call xvz(szcav0,xasrc,xzvcav)
      xzvcav=xzvcav-xasrc
      call xvy(sycav0,xyvcav)
      xyvcav=xyvcav-xasrc
      if(xi >= xcave) then
         lcav=.false.
         lrevcav=.false.
      else
         lcav=.true.
      endif

c --- is plume affected by wake turbulence?
c -------------------------------------------
c --- initialize output arrays
      nwak=1
      xwak(1)=xi+xbadj
      szwak(1)=szi
      sywak(1)=syi
      drwak(1)=zero
c --- compute corresponding virtual distances
      xasrc=xwak(1)
      call xvz(szi,xasrc,xzvwak)
      xzvwak=xzvwak-xasrc
      call xvy(syi,xyvwak)
      xyvwak=xyvwak-xasrc
      if(xi >= xamx) then
         lwak=.false.
         if(ldbhr) then
            write(io6,*)'----- wake_dfsn:        nwak = ',nwak
            write(io6,*)'z-dispersion reaches ambient at: ',(xaz+xbadj)
            write(io6,*)'y-dispersion reaches ambient at: ',(xay+xbadj)
            write(io6,*)'z,y virtual distances (m)    = ',xzvwak,xyvwak
            write(io6,*)'xadj, yadj, xi        (m)    = ',xbadj,ybadj,xi
            write(io6,*)'plume not altered by wake turbulence!'
            write(io6,*)
         endif
      else
         lwak=.true.
      endif

c --- return now if sigmas in wake do not need to be tabulated
      if(.not.lwak .and. .not.lcav) return

c --- compute location of downwind edge of pdf region from xi
      xd=xi+xlr

c --- set stepping parameters
      dx=2.0
c --- range of table is from point of entry into wake (xi), to the point
c --- at which ambient growth rate resumes (xa), plus one "ds" so that
c --- both sigmas reach ambient, and virtual distances are computed.
c --- when cavity sigmas are also computed, range may start at the
c --- downwind bldg face, and extend to end of cavity.
      xlow=xi
      xhi=xamx
      if(lcav) then
         xlow=MIN(xi,xbc)
         xhi=MAX(xamx,xcave)
      endif
      xrange=xhi-xlow+dx
      np=NINT(xrange/dx)+1
      np=MIN(np,mxnw-1)
      dx=xrange/(FLOAT(np)-one)
      dxi=one/dx
      nws=0
      ncs=0

c --- fill first element of marching arrays using values at xlow
      dist(1)=xlow+xbadj
      if(lwak) then
         asigz(1)=szi
         asigy(1)=syi
c ---    set inital plume growth rate in wake to zero
         dsz(1)=zero
      endif
      if(lcav) then
         csigz(1)=szcav0
         csigy(1)=sycav0
      endif

c --- initialize distance (from upwind face of bldg)
      x=xlow

c --- loop over steps in wake region
c -----------------------------------
      do n=2,np
         xold=x
         x=x+dx
         dist(n)=dist(n-1)+dx
c ---    check to see if cavity data should be revised based on
c ---    data from previous step
         if(lrevcav .and. xold >= xlb) then
            call wake_cav0(asigy(n-1),szcav0,sycav0r)
            if(sycav0r > sycav0) then
               sycav0=sycav0r
               sycav(1)=sycav0
c ---          compute corresponding virtual distance
               xasrc=xcav(1)
               call xvy(sycav0,xyvcav)
               xyvcav=xyvcav-xasrc
c ---          replace sigma-y values in stepping arrays
               do ir=1,n-1
                  csigy(ir)=MAX(csigy(ir),sycav0)
               end do
            endif
            lrevcav=.false.
         endif

c ---    obtain sigmas for this step

c ---    first, persist initial values if upwind of starting point
         if(lwak .and. (xi >= x)) then
            asigz(n)=asigz(n-1)
            asigy(n)=asigy(n-1)
            dsz(n)=dsz(n-1)
c ---       set index for skipping entry when filling wake arrays
            nws=n
         endif
         if(lcav .and. (xbc >= x)) then
            csigz(n)=szcav0
            csigy(n)=sycav0
c ---       set index for skipping entry when filling cav arrays
            ncs=n
         endif

c ---    now test again and apply full treatment when needed
         if(xold > xamx) then
c ---       ambient growth region in wake: use virtuals
            if(lwak .and. (xi < x)) then
               call sigz(dist(n)+xzvwak,asigz(n))
               call sigy(dist(n)+xyvwak,asigy(n))
               dsz(n)=(asigz(n)-asigz(n-1))*dxi
            endif
c ---       cavity source ---
            if(lcav .and. (xbc < x)) then
               call sigz(dist(n)+xzvcav,csigz(n))
               call sigy(dist(n)+xyvcav,csigy(n))
            endif
         else
            if(x < xamn) then
c ---          wake growth for both sigz and sigy
c ---          set x at mid-point of step
               xmid=half*(x+xold)
c ---          compute turbulence intensities at midpoint
               call wake_turb(istab,lrurl,xmid,xlb,rb,wakiz,wakiy)
               if(lwak .and. (xi <= x)) then
c ---             compute sigmas in wake
                  call wake_sig(x,xd,xold,wakiz,wakiy,asigz(n-1),
     &                          asigy(n-1),hb,wb,rb,zk,yk,
     &                          asigz(n),asigy(n),dsz(n))
               endif
c ---          cavity source ---
               if(lcav .and. (xbc <= x)) then
                  call wake_sig(x,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                          csigy(n-1),hb,wb,rb,zkc,ykc,
     &                          csigz(n),csigy(n),dzrate)
               endif
            else
c ---          at least one of the sigmas reaches ambient growth in wake
c ---          process sigma-z
               if(xold >= xaz) then
c ---             ambient growth region in wake: use virtual x
                  if(lwak .and. (xi <= x)) then
                     call sigz(dist(n)+xzvwak,asigz(n))
                     dsz(n)=(asigz(n)-asigz(n-1))*dxi
                  endif
c ---             cavity source ---
                  if(lcav .and. (xbc <= x)) then
                     call sigz(dist(n)+xzvcav,csigz(n))
                  endif
               elseif(x >= xaz) then
c ---             transition from wake to ambient
                  xnew=xaz
                  xmid=half*(xnew+xold)
c ---             compute turbulence intensities at midpoint
                  call wake_turb(istab,lrurl,xmid,xlb,rb,wakiz,wakiy)
                  if(lwak .and. (xi <= xnew)) then
c ---                compute sigma at xaz
                     call wake_sig(xnew,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),hb,wb,rb,zk,ykdum,
     &                             sigzxa,sydum,dzrate)
c ---                compute corresponding virtual distance
                     xasrc=xaz+xbadj
                     call xvz(sigzxa,xasrc,xzvwak)
                     xzvwak=xzvwak-xasrc
c ---                compute sigma at x
                     call sigz(dist(n)+xzvwak,asigz(n))
                     dsz(n)=(asigz(n)-asigz(n-1))*dxi
                  endif
c ---             cavity source ---
                  if(lcav .and. (xbc <= xnew)) then
                     call wake_sig(xnew,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),hb,wb,rb,zkc,ykdum,
     &                             sigzxa,sydum,dzrate)
                     call xvz(sigzxa,xasrc,xzvcav)
                     xzvcav=xzvcav-xasrc
                     call sigz(dist(n)+xzvcav,csigz(n))
                  endif
               else
c ---             wake growth for sigz
c ---             set x at mid-point of step
                  xmid=half*(x+xold)
c ---             compute turbulence intensities at midpoint
                  call wake_turb(istab,lrurl,xmid,xlb,rb,wakiz,wakiy)
                  if(lwak .and. (xi <= x)) then
c ---                compute sigmaz
                     call wake_sig(x,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),hb,wb,rb,zk,ykdum,
     &                             asigz(n),sydum,dsz(n))
                  endif
c ---             cavity source ---
                  if(lcav .and. (xbc <= x)) then
                     call wake_sig(x,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),hb,wb,rb,zkc,ykdum,
     &                             csigz(n),sydum,dzrate)
                  endif
               endif
c ---          process sigma-y
               if(xold >= xay) then
c ---             ambient growth region in wake: use virtual x
                  if(lwak .and. (xi <= x)) then
                     call sigy(dist(n)+xyvwak,asigy(n))
                  endif
c ---             cavity source ---
                  if(lcav .and. (xbc <= x)) then
                     call sigy(dist(n)+xyvcav,csigy(n))
                  endif
               elseif(x >= xay) then
c ---             transition from wake to ambient
                  xnew=xay
                  xmid=half*(xnew+xold)
c ---             compute turbulence intensities at midpoint
                  call wake_turb(istab,lrurl,xmid,xlb,rb,wakiz,wakiy)
                  if(lwak .and. (xi <= xnew)) then
c ---                compute sigma at xay
                     call wake_sig(xnew,xd,xold,turbz,turby,asigz(n-1),
     &                             asigy(n-1),hb,wb,rb,zkdum,yk,
     &                             szdum,sigyxa,dzrate)
c ---                compute corresponding virtual distance
                     xasrc=xay+xbadj
                     call xvy(sigyxa,xyvwak)
                     xyvwak=xyvwak-xasrc
c ---                compute sigma at x
                     call sigy(dist(n)+xyvwak,asigy(n))
                  endif
c ---             cavity source ---
                  if(lcav .and. (xbc <= xnew)) then
                     call wake_sig(xnew,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),hb,wb,rb,zkdum,ykc,
     &                             szdum,sigyxa,dzrate)
                     call xvy(sigyxa,xyvcav)
                     xyvcav=xyvcav-xasrc
                     call sigy(dist(n)+xyvcav,csigy(n))
                  endif
               else
c ---             wake growth for sigy
c ---             set x at mid-point of step
                  xmid=half*(x+xold)
c ---             compute turbulence intensities at midpoint
                  call wake_turb(istab,lrurl,xmid,xlb,rb,wakiz,wakiy)
                  if(lwak .and. (xi <= x)) then
c ---                compute sigmay
                     call wake_sig(x,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),hb,wb,rb,zkdum,yk,
     &                             szdum,asigy(n),dzrate)
                  endif
c ---             cavity source
                  if(lcav .and. (xbc <= x)) then
                     call wake_sig(x,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),hb,wb,rb,zkdum,ykc,
     &                             szdum,csigy(n),dzrate)
                  endif
               endif
            endif
         endif

c --- next distance
      end do

c --- construct arrays for /wakedat/
c ----------------------------------

      if(lwak) then
c ---    wak arrays:
         npw=np-nws

c ---    place initial values into first element
         xwak(1)=xi+xbadj
         szwak(1)=szi
         sywak(1)=syi
         drwak(1)=zero
         if(npw >= mxntr) then
c ---       sample a subset of the npw points
            nwak=mxntr
            xwak(nwak)=dist(np)
            szwak(nwak)=asigz(np)
            sywak(nwak)=asigy(np)
            drwak(nwak)=rtpiby2*dsz(np)
            if(npw <= 2*mxntr) then
c ---          fill elements with nearest values
               deln=FLOAT(npw)/FLOAT(nwak)
               do in=2,nwak-1
                  jn=in*deln+nws
                  xwak(in)=dist(jn)
                  szwak(in)=asigz(jn)
                  sywak(in)=asigy(jn)
                  drwak(in)=rtpiby2*dsz(jn)
               end do
            else
c ---          use sliding step-size to sample nearfield more frequently
               deln=2.*FLOAT(npw-mxntr)/FLOAT(mxntr*(mxntr-1))
               rn=one
               do in=2,nwak-1
                  rn=rn+one+(in-1)*deln
                  jn=rn+nws
                  xwak(in)=dist(jn)
                  szwak(in)=asigz(jn)
                  sywak(in)=asigy(jn)
                  drwak(in)=rtpiby2*dsz(jn)
               end do
            endif
         else
c ---       fill only those elements used
            nwak=npw
            do in=2,npw
               inp=in+nws
               xwak(in)=dist(inp)
               szwak(in)=asigz(inp)
               sywak(in)=asigy(inp)
               drwak(in)=rtpiby2*dsz(inp)
            end do
         endif
      endif

      if(lcav) then
c ---    cav arrays:
         npc=np-ncs

c ---    place initial values into first element
         xcav(1)=xbc+xbadj
         szcav(1)=szcav0
         sycav(1)=sycav0
         if(npc >= mxntr) then
c ---       sample a subset of the npc points
            ncav=mxntr
            xcav(ncav)=dist(np)
            szcav(ncav)=csigz(np)
            sycav(ncav)=csigy(np)
            if(npc <= 2*mxntr) then
c ---          fill elements with nearest values
               deln=FLOAT(npc)/FLOAT(ncav)
               do in=2,ncav-1
                  jn=in*deln+ncs
                  xcav(in)=dist(jn)
                  szcav(in)=csigz(jn)
                  sycav(in)=csigy(jn)
              end do
            else
c ---          use sliding step-size to sample nearfield more frequently
               deln=2.*FLOAT(npc-mxntr)/FLOAT(mxntr*(mxntr-1))
               rn=one
               do in=2,ncav-1
                  rn=rn+one+(in-1)*deln
                  jn=rn+ncs
                  xcav(in)=dist(jn)
                  szcav(in)=csigz(jn)
                  sycav(in)=csigy(jn)
               end do
            endif
         else
c ---       fill only those elements used
            ncav=npc
            do in=2,npc
               inp=in+ncs
               xcav(in)=dist(inp)
               szcav(in)=csigz(inp)
               sycav(in)=csigy(inp)
            end do
         endif
      endif

      if(ldbhr) then

         write(io6,*)
         write(io6,*)'----- wake_dfsn:        nwak = ',nwak
         write(io6,*)'z-dispersion reaches ambient at: ',(xaz+xbadj)
         write(io6,*)'y-dispersion reaches ambient at: ',(xay+xbadj)
         write(io6,*)'z,y virtual dist (m) - wake  = ',xzvwak,xyvwak
         write(io6,*)'z,y virtual dist (m) - cav   = ',xzvcav,xyvcav
         write(io6,*)'xadj, yadj, xi        (m)    = ',xbadj,ybadj,xi
         write(io6,*)'xbc,distc,xdc         (m)    = ',xbc,distc,xdc
         write(io6,*)'lwak, nws, npw               = ',lwak,nws,npw
         write(io6,*)'lcav, ncs, npc               = ',lcav,ncs,npc
         write(io6,*)
c
c ---    write the arrays passed back to the calling routine
         write(io6,28)
28       format(/4x,'i',9x,'xwak',6x,'szwak',6x,'sywak',6x,'drwak',/)
         do i=1,nwak
            write(io6,32)i,xwak(i),szwak(i),sywak(i),drwak(i)
32          format(i5,3x,4(f10.4,1x))
         end do
         write(io6,*)

         write(io6,29)
29       format(/4x,'i',9x,'xcav',6x,'szcav',6x,'sycav',/)
         do i=1,ncav
            write(io6,33)i,xcav(i),szcav(i),sycav(i)
33          format(i5,3x,3(f10.4,1x))
         end do
         write(io6,*)
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_turb(kst,lrurl,x,l,r,tiz,tiy)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812             wake_turb
c                l. schulman, d. strimaitis,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose: calculates turbulence intensity as a function of
c              location within the wake from modified weil (1996)
c              analytical expressions
c
c --- inputs:
c              kst - integer     - pg stability class (1-6)
c            lrurl - logical     - rural flag (t=rural, f=urban)
c                x - real        - distance (m) from upwind bldg wall
c                l - real        - dist (m) of downwind bldg wall from
c                                  upwind bldg wall
c                r - real        - wake scaling length (m)
c
c     common block /dfsn/ variables:
c           wiz0,wiy0,wfz,wfy,
c           dua_ua,xdecay,xdecayi,
c           rurliz,rurliy,urbniz,urbniy
c
c --- output:
c
c              tiz - real        - turbulence intensity sigw/u
c              tiy - real        - turbulence intensity sigv/u
c
c --- wake_turb called by:  wake_dfsn
c --- wake_turb calls    :  none
c----------------------------------------------------------------------
c
      use dfsn_pri

      real l
      logical lrurl

c --- misc. constants
      data one/1.0/, zero/0.0/

c --- identify ambient turbulence intensity
      if(lrurl) then
         ambiz=rurliz(kst)
         ambiy=rurliy(kst)
      else
         ambiz=urbniz(kst)
         ambiy=urbniy(kst)
      endif

c --- compute asymptotic turbulence intensity in far wake
      fariz=MIN(wiz0,ambiz)
      fariy=MIN(wiy0,ambiy)

c --- compute turbulence intensity at position downwind of bldg
      xml=MAX(zero,x-l)
      xfac=one/(((xml+r)/r)**xdecay-dua_ua)
      tiz=fariz*(one+((wfz*wiz0/fariz-one)+dua_ua)*xfac)
      tiy=fariy*(one+((wfy*wiy0/fariy-one)+dua_ua)*xfac)

c --- interpolate turbulence intensity if over roof of bldg
      if(x < l) then
         xfrac=x/l
         tiz=ambiz+xfrac*(tiz-ambiz)
         tiy=ambiy+xfrac*(tiy-ambiy)
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_u(ldb,x,y,z,ubyua,dufac)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812                wake_u
c                d. strimaitis,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose: calculates speed ratio u(wake)/u(ambient) as a function
c              of location within the wake
c
c --- inputs:
c              ldb - logical     - flag for debug output
c                x - real        - downwind distance (m) from upwind
c                                  bldg wall
c                y - real        - crosswind distance (m) from center of
c                                  upwind bldg wall
c                z - real        - height (m) above ground
c
c     common block /params/ variables:
c           mxntr, mxnw
c     common block /wakedat/ variables:
c           hb, wb, xlb, rb, xlr
c     common block /dfsn/ variables:
c           dua_ua,xdecay,xdecayi
c
c --- output:
c
c            ubyua - real        - u(x,z)/ua speed in wake scaled by
c                                  ambient speed
c            dufac - real        - gradient in speed factor above
c                                  zcav
c
c --- wake_u called by:  numrise, wake_dbg
c --- wake_u calls    :  cavity_ht, wake_dim
c----------------------------------------------------------------------
c
      use dfsn_pri
      use wakedat_pri

      logical ldb

c --- misc. constants
      data two/2.0/, one/1.0/, zero/0.0/

c --- compute cavity height above ground, and width
      call cavity_ht(hb,wb,xlb,rb,xlc,xlr,hr,x,zcav,ycav)

c --- compute far wake height above ground, and width
      call wake_dim(x,hb,wb,rb,hwake,wwake)

c --- return "null" values if point is outside wake
      yabs=ABS(y)
      ubyua=one
      dufac=zero
      if(z >= hwake .or. yabs >= wwake) return

c --- adjust "base" speed deficit dua_ua if lateral position is
c --- beyond bldg width projection, but within the wake
      ymin=MAX(0.5*wb,wwake-rb/3.)
      du_ua=dua_ua
      ydiff=wwake-ymin
      if(yabs > ymin .and. ydiff > zero) then
         du_ua=dua_ua*(one-(yabs-ymin)/ydiff)
      endif

c --- scale speed deficit (ua-u)/ua =  du_ua in wake for
c --- position x downwind of bldg face
      xml=MAX(zero,x-xlb)
      du_ua=du_ua*((xml+rb)/rb)**(-xdecay)
c --- interpolate factor if over roof of bldg (linear)
      if(x < xlb) then
         xfrac=x/xlb
         du_ua=xfrac*du_ua
      endif

c --- compute speed factor ucav/ua at top of cavity
c --- assume that speed is constant below zcav, and increases linearly
c --- with height to ambient speed at hwake
      ucbyua=MAX(zero,(one-two*hwake*du_ua/(hwake+zcav)))

c --- compute gradient in speed factor (zero below zcav)
      dufac=zero
      if(z > zcav) then
         dufac=(one-ucbyua)/(hwake-zcav)
      endif

c --- compute speed factor u/ua at height z
      zz=MIN(z,hwake)
      ubyua=ucbyua+dufac*(zz-zcav)

      if(ldb) then
        write(io6,*)'wake_u         '
        write(io6,*)'       x,y,z = ',x,y,z
        write(io6,*)'hwake, zcav  = ',hwake, zcav
        write(io6,*)'wwake, ymin  = ',wwake, ymin
        write(io6,*)'du_ua, ucbyua= ',du_ua, ucbyua
        write(io6,*)'ubyua, dufac = ',ubyua,dufac
        write(io6,*)
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_xa(kst,lrurl,l,r,xaz,xay)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  980310               wake_xa
c                d. strimaitis,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose: calculates the distance from the upwind face of the
c              building to the point at which the turbulence intensity
c              in the wake approaches that in the ambient flow.
c
c              final distances are limited to "xbyrmax" scale-lengths
c              (r) set in prime1, measured from downwind bldg face
c
c --- inputs:
c              kst - integer     - pg stability class (1-6)
c            lrurl - logical     - rural flag (t=rural, f=urban)
c                l - real        - dist (m) of downwind bldg wall from
c                                  upwind bldg wall
c                r - real        - wake scaling length (m)
c
c     common block /dfsn/ variables:
c           afac,xbyrmax,wiz0,wiy0,wfz,wfy,
c           dua_ua,xdecay,xdecayi,
c           rurliz,rurliy,urbniz,urbniy
c
c --- output:
c
c              xaz - real        - distance (m) from upwind bldg wall
c                                  at which wake turbulence iz = ambient
c              xay - real        - distance (m) from upwind bldg wall
c                                  at which wake turbulence iy = ambient
c
c --- wake_xa called by:  wake_dfsn
c --- wake_xa calls    :  none
c----------------------------------------------------------------------
c
      use dfsn_pri
      real l
      logical lrurl

c --- misc. constants
      data one/1.0/

c --- select ambient turbulence intensity
      if(lrurl) then
         ambiz=rurliz(kst)
         ambiy=rurliy(kst)
      else
         ambiz=urbniz(kst)
         ambiy=urbniy(kst)
      endif

c --- compute asymptotic turbulence intensity in far wake
      fariz=MIN(wiz0,ambiz)
      fariy=MIN(wiy0,ambiy)

c --- define the turbulence intensity at the transition point
      farizt=MAX(ambiz,afac*fariz)
      fariyt=MAX(ambiy,afac*fariy)

c --- compute leading term
      x0byr=l/r-one

c --- compute scaled distance at which iz equals transition iz
      xaz=x0byr+(dua_ua+(wfz*wiz0-fariz*(one-dua_ua))/
     &       (farizt-fariz))**xdecayi

c --- compute distance at which iy equals transition iy
      xay=x0byr+(dua_ua+(wfy*wiy0-fariy*(one-dua_ua))/
     &       (fariyt-fariy))**xdecayi

c --- cap distances
      xbyr=l/r+xbyrmax
      xaz=r*MIN(xbyr,xaz)
      xay=r*MIN(xbyr,xay)

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_dim(x,h,w,r,hwake,wwake)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812              wake_dim
c                d. strimaitis,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose: calculates the vertical height and lateral half-width
c              of a building wake at a distance x from the upwind
c              face of the bldg
c
c --- inputs:
c                x - real        - dist (m) from upwind bldg face
c                h - real        - building height (m)
c                w - real        - building width (m)
c                r - real        - wake scaling length (m)
c
c --- output:
c
c            hwake - real        - wake height (m)
c            wwake - real        - wake half-width (m)
c
c --- wake_dim called by:  position, wake_sig
c --- wake_dim calls    :  none
c----------------------------------------------------------------------
c --- wake height from combination of wilson (1979) and weil (1996)
c --- limits for uniform approach wind

c --- set const. used in wake height formula
      data cwkht/1.2/

c --- misc. constants
      data half/0.5/, third/0.3333333/, zero/0.0/

c --- scale distance by r
      xbyr=x/r
      xbyr3rd=xbyr**third

c --- compute match to bdlg height at x=0
      xpbyr=-(h/(cwkht*r))**3
      dxbyr=xbyr-xpbyr

c --- wake height
      hwake=zero
      if(xbyr > zero) hwake =cwkht*r*dxbyr**third

c --- wake half-width from empirical fit to snyder wind tunnel data
      wwake=zero
      if(xbyr > zero) wwake=half*w+(third*r)*xbyr3rd

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_sig(x,xd,xold,turbz,turby,szold,syold,
     &                    h,w,r,zk,yk,sz,sy,dsz)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812              wake_sig
c                d. strimaitis,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose: calculates sigmas and d(sigma)/dx within the wake
c              at a distance x from the upwind face of the bldg,
c              prior to the ambient growth regime, for a "small"
c              distance increment
c
c --- inputs:
c                x - real        - dist (m) from upwind bldg face
c               xd - real        - dist (m) at which pdf growth ends
c             xold - real        - starting x of this step (m)
c     turbz, turby - real        - current turbulence intensities
c     szold, syold - real        - sigmas (m) at start of step
c   htwake, hwwake - real        - height and half-width (m) of wake
c           zk, yk - real        - matching constants for pdf transition
c
c --- output:
c
c           zk, yk - real        - matching constants for pdf transition
c           sz, sy - real        - sigmas (m) at end of step
c              dsz - real        - d(sigmaz)/dx over step
c
c --- wake_sig called by:  wake_dfsn
c --- wake_sig calls    :  wake_dim
c----------------------------------------------------------------------
c --- wake height from combination of wilson (1979) and weil (1996)
c --- limits for uniform approach wind

c --- misc. constants
      data two/2.0/

c --- get wake dimensions
      call wake_dim(x,h,w,r,htwake,hwwake)

c --- use full width of the wake to scale lateral diffusivity
      fwwake=two*hwwake

      delx=x-xold
      xstepi=1./delx
      if(x < xd) then
c ---    pure pdf form
         dsz=turbz
         sz=szold + delx*turbz
         sy=syold + delx*turby
      elseif(xold > xd) then
c ---    pure wake diffusivity form
         dsz2=zk*turbz*htwake
         dsy2=yk*turby*fwwake
         sz=SQRT(szold**2+delx*dsz2)
         sy=SQRT(syold**2+delx*dsy2)
         dsz=(sz-szold)*xstepi
      else
c ---    transition from pdf to diffusivity form
c ---    to end of pdf:
         delx=xd-xold
         sigzd=szold + delx*turbz
         sigyd=syold + delx*turby
         zk=two*sigzd/htwake
         yk=two*sigyd/fwwake
c ---    beyond end of pdf:
         delx=x-xd
         dsz2=zk*turbz*htwake
         dsy2=yk*turby*fwwake
         sz=SQRT(sigzd**2+delx*dsz2)
         sy=SQRT(sigyd**2+delx*dsy2)
         dsz=(sz-szold)*xstepi
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine wake_dbg(io,ntr,xtr,ytr,ztr,rtr,nobid,hstack)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812              wake_dbg
c                d. strimaitis,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose: reports salient features of prime results to
c              file for debug purposes
c
c --- inputs:
c               io - integer     - unit for output file
c         xtr(ntr) - real        - downwind distance (m)
c         ytr(ntr) - real        - crosswind distance (m)
c         ztr(ntr) - real        - plume centerline height (m)
c         rtr(ntr) - real        - plume radius (m)
c            nobid - logical     - flag for bid
c           hstack - real        - height (m) of release
c
c     common block /params/ variables:
c           mxntr, mxnw
c     common block /wakedat/ variables:
c           xbadj, hb, wb, xlb, rb, xlr, xlc, hr,
c           xcav, szcav, sycav
c
c --- output: (written to file)
c
c          dbxb - real    - distance (m) from upwind bldg face
c           dbx - real    - distance (m) from source along wind
c           dbz - real    - plume centerline height above ground (m)
c          dbhc - real    - cavity height above ground (m)
c          dbhw - real    - wake height above ground (m)
c          dbsz - real    - sigma-z (m)
c          dbsy - real    - sigma-y (m)
c          dbuw - real    - wind speed factor at dbz  (u/ua)
c         dbrsz - real    - sigma-y (m) inferred from plume radius
c       ipositn - integer - 1: in bldg
c                           2: in cavity
c                           3: in far wake
c                           4: outside bldg influence
c         dbszc - real    - sigma-z (m) for cavity source
c         dbsyc - real    - sigma-y (m) for cavity source
c
c --- wake_dbg called by:  pheff
c --- wake_dbg calls    :  wake_xsig, wake_dim, cavity_ht,
c                          position, wake_u
c----------------------------------------------------------------------
c
      use wakedat_pri

      real xtr(ntr),ytr(ntr),ztr(ntr),rtr(ntr)
      logical nobid,ldb

      ldb=.false.

      data rt2bypi/0.797885/

c --- write section header to file
      write(io,*)
      write(io,*)'------------------------------------------------'
      write(io,*)'prime module results for current source and hour'
      write(io,*)'          (all lengths in meters)'
      write(io,*)'------------------------------------------------'
      write(io,*)
      write(io,100)
      write(io,*)

c --- report start of cavity as first point if it lies upwind of source
      if(xcav(1) < 0.0) then
c ---    set plume coordinates
         dbx=xcav(1)
         dby=ytr(1)
         dbz=0.0

c ---    set initial values
         dbsz=0.0
         dbsy=0.0
         dbhw=0.0
         dbhc=0.0
         dbrsz=0.0
         dbuw=1.0
         ipositn=4

c ---    compute related data
         rise=0.0
         xb=dbx-xbadj
         yb=dby-ybadj
         zb=dbz
         dbxb=xb

c ---    set sigmas
         dbsz=0.0
         dbsy=0.0
         dbszc=szcav(1)
         dbsyc=sycav(1)

c ---    set dr/dx of plume radius within wake region
         dbdrdx=0.0

         if(xb >= 0.0) then
c ---       set wake dimension along center plane from bldg
            call wake_dim(xb,hb,wb,rb,dbhw,dbww)

c ---       set cavity dimension along centerplane from bldg
            call cavity_ht(hb,wb,xlb,rb,xlc,xlr,hr,xb,dbhc,dbwc)

c ---       set speed factor
            call position(xb,yb,zb,ipositn)
            dbuw=1.0
            if(ipositn < 4) call wake_u(ldb,xb,yb,zb,dbuw,dbduw)
         endif

c ---    report values
         write(io,101) dbxb,dbx,dbz,dbhw,dbhc,dbsz,dbsy,dbuw,dbduw,
     &                 dbrsz,dbdrdx,ipositn,dbszc,dbsyc
      endif

c --- process point of release
c --- set plume coordinates
      dbx=0.0
      dby=ytr(1)
      dbz=hstack

c --- set initial values
      dbsz=0.0
      dbsy=0.0
      dbhw=0.0
      dbhc=0.0
      dbrsz=0.0
      dbuw=1.0
      ipositn=4

c --- compute related data
      rise=dbz-hstack
      xb=dbx-xbadj
      yb=dby-ybadj
      zb=dbz
      dbxb=xb

c --- set sigmas just downwind of source
      xzero=0.001
      call wake_xsig(xzero,rise,nobid,dbsz,dbsy,dbszc,dbsyc)

c --- set dr/dx of plume radius within wake region
      call wake_drdx(dbx,dbdrdx)

      if(xb >= 0.0) then
c ---    set wake dimension along center plane from bldg
         call wake_dim(xb,hb,wb,rb,dbhw,dbww)

c ---    set cavity dimension along centerplane from bldg
         call cavity_ht(hb,wb,xlb,rb,xlc,xlr,hr,xb,dbhc,dbwc)

c ---    set speed factor
         call position(xb,yb,zb,ipositn)
         dbuw=1.0
         if(ipositn < 4) call wake_u(ldb,xb,yb,zb,dbuw,dbduw)
      endif

c --- report values
      write(io,101) dbxb,dbx,dbz,dbhw,dbhc,dbsz,dbsy,dbuw,dbduw,dbrsz,
     &              dbdrdx,ipositn,dbszc,dbsyc

c --- now loop over entries in plume rise array
      do it=1,ntr

c ---    set plume coordinates
         dbx=xtr(it)
         dby=ytr(it)
         dbz=ztr(it)
         dbrsz=rtr(it)*rt2bypi

c ---    set initial values
         dbhw=0.0
         dbhc=0.0
         dbuw=1.0
         ipositn=4

c ---    compute related data
         rise=dbz-hstack
         xb=dbx-xbadj
         yb=dby-ybadj
         zb=dbz
         dbxb=xb

c ---    set sigmas
         call wake_xsig(dbx,rise,nobid,dbsz,dbsy,dbszc,dbsyc)

c ---    set dr/dx of plume radius within wake region
         call wake_drdx(dbx,dbdrdx)

         if(xb >= 0.0) then
c ---       set wake dimension along center plane from bldg
            call wake_dim(xb,hb,wb,rb,dbhw,dbww)

c ---       set cavity dimension along centerplane from bldg
            call cavity_ht(hb,wb,xlb,rb,xlc,xlr,hr,xb,dbhc,dbwc)

c ---       set speed factor
            call position(xb,yb,zb,ipositn)
            dbuw=1.0
            if(ipositn < 4) call wake_u(ldb,xb,yb,zb,dbuw,dbduw)
         endif

c ---    report values
         write(io,101) dbxb,dbx,dbz,dbhw,dbhc,dbsz,dbsy,dbuw,dbduw,
     &                 dbrsz,dbdrdx,ipositn,dbszc,dbsyc

      end do
      write(io,*)

100   format('     xb      x      z   hwake   hcav    sz     s',
     &       'y   ufac  dufac  r->sz   drdx  pos  szcav  sycav')
101   format(1x,7f7.1,2f7.3,f7.1,f7.3,i4,2f7.1)

      return
      end
c-----------------------------------------------------------------------
      subroutine wake_cav0(sycapt,szcav0,sycav0)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812              wake_cav0
c                d. strimaitis, l. schulman,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  compute the sigmas for a source placed on the floor
c               of the cavity, which produce the target cavity
c               concentration
c
c --- inputs:
c
c        sycapt - real    - sigma-y (m) of plume at point where
c                           mass is captured in cavity
c
c     common block /wakedat/ variables:
c           hb, wb, xlr, xlc, hr, ub, urh
c
c --- output:
c
c        szcav0 - real    - initial sigma-z (m) of cavity source
c        sycav0 - real    - initial sigma-y (m) of cavity source
c
c                 note    - these sigmas reproduce the cavity
c                           concentration when the used in:
c                           c = qc / (pi * us * szcav0 * sycav0)
c                           where us is the wind speed for the primary
c                           source, and qc is the mass flux captured by
c                           and released from the cavity.
c
c --- wake_cav0 called by:  wake_dfsn
c --- wake_cav0 calls    :  none
c----------------------------------------------------------------------
c
      use wakedat_pri

      data rt2pi/2.5066283/, rt2bypi/.7978846/, third/0.3333333/

c --- interpret plume sigma-y at cavity entry point as equivalent
c --- top-hat;  limit to full width of bldg
      wcapt=MIN(rt2pi*sycapt,wb)

c --- set width scale for lateral distribution in cavity
      wscale=MIN(wb,3.*hb)
      wscale=MAX(wscale,third*hb)
      wscale=MAX(wscale,wcapt)

c --- sigma-y for equivalent top-hat distribution
      sycav0=wscale/rt2pi

c --- set height of cavity behind the bldg
      if(xlc  <  xlb) then
c ---    reattachment
         hcav=hb
      else
c ---    no reattachment
         hcav=hr
      endif

c --- set sigma-z that results in centerline concentration equal to
c --- cavity concentration
c --- wilson & britter 1982 approach to cavity concentration
      uratio=ub/urh
      szcav0=rt2bypi*uratio*hcav*third

      return
      end
c-----------------------------------------------------------------------
      subroutine cav_src(xr,yr,zr,fqcav0,qc,hc,yrc,zrc,szc,syc,n1,n2)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812               cav_src
c                d. strimaitis,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  select plume data for computing concentration at a
c               receptor due to mass contained in / released from cavity
c
c --- inputs:
c            xr - real    - downwind distance (m) from stack to receptor
c            yr - real    - crosswind distance (m) from stack to receptor
c            zr - real    - receptor height (m) above ground
c
c     common block /wakedat/ variables:
c           hb, wb, xlb, rb, xlc, xlr, hr,
c           xbadj, ybadj, szcav, sycav, fqcav
c
c --- output:
c
c        fqcav0 - real    - fraction of plume mass rate captured
c                           and released by cavity
c         qc(3) - real    - normalized emission rate (q/s) for cavity
c                           sources --- qc(1)+qc(2)=1.0
c         hc(3) - real    - height (m) of cavity sources
c        yrc(3) - real    - sigma-z (m) for cavity sources
c        yrc(3) - real    - crosswind distance (m) from centerline
c                           of cavity sources to receptor
c        zrc(3) - real    - receptor height (m) above cavity
c        szc(3) - real    - sigma-z (m) for cavity sources
c        syc(3) - real    - sigma-y (m) for cavity sources
c         n1,n2 - integer - index range for active sources
c                           1,1: primary source only (no cavity source
c                                contributions)
c                           1,2: primary and "outside" cavity source
c                                contribution
c                           1,3: primary and both "outside" and "inside"
c                                cavity source contributions
c                           2,2: "outside" cavity source only
c                           2,3: both "outside" and "inside" cavity
c                                sources
c                           3,3: "inside" cavity source only
c
c ------------------------------------
c     note:  3 sources are considered:
c                           (1)- the primary (point) source
c                           (2)- the cavity source that dominates
c                                "outside" of the cavity
c                           (3)- the cavity source that dominates
c                                "inside" of the cavity
c            for the 2 cavity sources, array data elements are ordered:
c                           (1)- reserved for primary source data
c                           (2)- "outside" cavity source
c                           (3)- "inside" cavity source
c
c --- cav_src called by:  psimpl(host subroutine)
c --- cav_src calls    :  position, cavity_ht, wake_xsig
c----------------------------------------------------------------------
c
      use wakedat_pri

      real qc(3),hc(3),yrc(3),zrc(3),szc(3),syc(3)
      logical nobid

      data rt2pi/2.5066283/

      nobid = .false.
c --- extract cavity sigmas from the first entry in the cavity arrays
      szcav0=szcav(1)
      sycav0=sycav(1)

c --- pass mass fraction to calling program
      fqcav0=fqcav

c --- set cavity source heights to zero
      hc(2)=0.0
      hc(3)=0.0

c --- initialize cavity source mode
c --- (0: none, 1: "outside", 2: "inside", 3: both)
      mode=0

      if(fqcav <= 0.0) then
c ---    no mass in cavity
         n1=1
         n2=1
         do i=2,3
            qc(i)=0.0
            yrc(i)=yr
            zrc(i)=zr
            szc(i)=szcav0
            syc(i)=sycav0
         end do
      else
c ---    find receptor location relative to center of upwind bldg face
         xrb=xr-xbadj
         yrb=yr-ybadj
         zrb=zr
         call position(xrb,yrb,zrb,ipositn)

c ---    set limits of transition zone at end of cavity
         x115b=xlb+1.15*xlr
         x85b=xlb+0.85*xlr
c ---    adjust relative contribution of cavity sources near end
c ---    of cavity region
         if (xrb >= x115b) then
c ---       receptor well outside cavity; use only "outside" source
            qc(2)=1.0
            qc(3)=0.0
            mode=1
         elseif(xrb > x85b) then
c ---       mix relative contribution so that they are equal at
c ---       end of cavity
            qc(2)=(xrb-x85b)/(x115b-x85b)
            qc(3)=1.0-qc(2)
            mode=3
         elseif(xrb > xlb) then
c ---       receptor well within cavity; use only "inside" source
            qc(2)=0.0
            qc(3)=1.0
            mode=2
         else
c ---       receptor upwind of trailing edge of projected bldg;
c ---       use "inside" source, but drop mass fraction linearly
c ---       to zero at windward face of projected bldg
            qc(2)=0.0
            qc(3)=MAX(0.0,xrb/xlb)
            mode=2
         endif

         if(ipositn == 4) then
c ---       not within wake, so drop cavity source contribution
            mode=0
            n1=1
            n2=1
            do i=2,3
               qc(i)=0.0
               yrc(i)=yr
               zrc(i)=zr
               szc(i)=szcav0
               syc(i)=sycav0
            end do
         else
c ---       set receptor offset from centerline of cavity plume
c ---       top-hat equivalent width of cavity sigma-y
            wtop=sycav0*rt2pi
c ---       max distance from bldg center to centerline of cavity plume
            ybmax=0.5*(wb-wtop)
            if(ybmax <= 0.0) then
c ---          plume spread exceeds bldg width so cavity source is
c ---          centered on bldg
               yrc(2)=yrb
            else
c ---          source location relative to center of bldg
               ysb=-ybadj
               if(ysb < 0.0) then
                  yrc(2)=yrb-MAX(ysb,-ybmax)
               else
                  yrc(2)=yrb-MIN(ysb,ybmax)
               endif
            endif
            yrc(3)=yrc(2)

            if(ipositn <= 2) then
c ---          within cavity/bldg, so drop primary source contribution,
c ---          and place receptor on ground
               if(mode == 3) then
                  n1=2
                  n2=3
               elseif(mode == 2) then
                  n1=3
                  n2=3
               elseif(mode == 1) then
                  n1=2
                  n2=2
               endif
               do i=n1,n2
                  zrc(i)=0.0
                  szc(i)=szcav0
                  syc(i)=sycav0
               end do
               if((mode == 1 .or. mode == 3) .and. xr > 0.0) then
                 nobid = .false.  ! Needs checking
                 call wake_xsig(xr,0.0,nobid,dumz,dumy,szc(2),syc(2))
               end if
            else
c ---          contributions from primary & possibly both cavity plumes
               n1=1
               n2=3
c ---          set pole height to height above cavity boundary
               if(xrb >= (xlb+xlr)) then
                  zrc(2)=zr
               else
                  call cavity_ht(hb,wb,xlb,rb,xlc,xlr,hr,xrb,zcav,wcav)
                  zrc(2)=MAX(0.0,zr-zcav)
               endif
               zrc(3)=zrc(2)
               if(mode == 2) then
c ---             no contribution from "outside" cavity source, so swap
c ---             data for "inside" source into "outside" source arrays
c ---             and reset n2=2
                  qc(2)=qc(3)
                  szc(2)=szcav0
                  syc(2)=sycav0
                  n2=2
               elseif(mode == 1) then
c ---             no contribution from "inside" cavity source, so
c ---             reset n2=2
                  nobid = .false.  ! Needs checking
                  call wake_xsig(xr,0.0,nobid,dumz,dumy,szc(2),syc(2))
                  n2=2
               else
c ---             both cavity sources are used
                  szc(2)=szcav0
                  syc(2)=sycav0
                  szc(3)=szcav0
                  syc(3)=sycav0
                  nobid = .true.
                  if(xr >= 0.0) call wake_xsig(xr,0.0,nobid,dumz,dumy,
     +                                         szc(2),syc(2))
               endif
            endif
         endif
      endif

c --- final check: receptor upwind of primary source, or all mass in cav
c --- do not allow n1=1 (primary source contribution)
      if(n1 == 1 .and. (xr <= 0.0 .or. fqcav == 1.0)) n1=2

      return
      end
c-----------------------------------------------------------------------
      subroutine wake_fqc(ldb,xbi,xtr,ztr,ntr)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812              wake_fqc
c                d. strimaitis, l. schulman,   earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose: computes the maximum plume mass captured by cavity.
c ---          plume centerline enters wake boundary before clearing
c ---          downwind end of cavity, so wake_fqc is used to find
c ---          point where mass in cavity is greatest.  note that
c ---          distances are measured from center of upwind face of bldg
c
c --- inputs:
c              ldb - logical     - debug output switch
c              xbi - real        - downwind distance (m) from upwind
c                                  face of bldg to point where plume
c                                  centerline enters wake
c         xtr(ntr) - real        - downwind distance from source (m)
c         ztr(ntr) - real        - plume centerline height (m)
c              ntr - integer     - number of entries in arrays
c
c     common block /params/ variables:
c           mxntr, mxnw
c     common block /wakedat/ variables:
c           hb, wb, xlb, rb, xlc, xlr, hr, xbadj, ybadj
c
c --- output:
c
c     common block /wakedat/ variables:
c           fqcav
c
c --- wake_fqc called by:  numrise
c --- wake_fqc calls    :  numgrad, wake_xsig, cavity_ht, frgauss
c----------------------------------------------------------------------
c
      use wakedat_pri

      real xtr(ntr),ztr(ntr)
      logical ldb,  nobid

      fqcav=0.0

c --- define range of distances from upwind face of bldg at which to
c --- evaluate plume mass fraction within cavity
      xbstart=MAX(xbi,xlb)
      xbend=xlb+xlr
      xrange=0.99*(xbend-xbstart)
      yba=ABS(ybadj)
c --- distance from source to end of cavity
      xend=xbend+xbadj

c --- use at least 5 steps, with a maximum length of 10 m
      nstep=MAX(5,INT(1+xrange/10.))
      xstep=xrange/FLOAT(nstep)

c --- for vertical plane, compute mass fraction below hb at the
c --- downwind end of the cavity.  this allows the influence of plume
c --- rise to continue lifting plume mass out of the influence of
c --- of the cavity structure for strongly buoyant releases.
c --- use this value as a cap to fractz.
      call numgrad(xend,xtr,ztr,ntr,zplm)
      nobid = .true.
      call wake_xsig(xend,0.0,nobid,sz,sy,szc,syc)
      call frgauss(zplm,sz,hb,-hb,fractz0)

      do is=0,nstep
         xb=xbstart+is*xstep
         x=xb+xbadj
         call numgrad(x,xtr,ztr,ntr,zplm)
         call cavity_ht(hb,wb,xlb,rb,xlc,xlr,hr,xb,zcav,ycav)
         nobid = .true.
         call wake_xsig(x,0.0,nobid,sz,sy,szc,syc)
         call frgauss(zplm,sz,zcav,-zcav,fractz)
         call frgauss(yba,sy,ycav,-ycav,fracty)
         fz=MIN(fractz,fractz0)
         fract=fz*fracty
         if(fract > fqcav) then
            fqcav=fract
            xmax=x
            fzmax=fz
         endif
      end do

c --- additional constraint:  account for fluctuations in cavity
c --- boundary on capturing low-level plumes by imposing a MAXimum
c --- capture fraction that linearly drops from 1.0 at 85% of the
c --- cavity length, to 0% at the end of the cavity.
      xb85=xlb+0.85*xlr
      if(xbstart > xb85) then
         fq85=MAX( 0.0, 1.-(xbstart-xb85)/(xbend-xb85) )
         fqcav=MIN(fqcav,fq85)
      endif

      if(ldb) then
         write(io6,*)
         write(io6,*)'wake_fqc:'
         write(io6,*)'xbi,xbstart,xbend  = ',xbi, xbstart, xbend
         write(io6,*)'xstep,nstep,fractz0= ',xstep, nstep, fractz0
         write(io6,*)'xb85,xmax          = ',xb85,xmax
         write(io6,*)'fqcav,fzmax        = ',fqcav,fzmax
         write(io6,*)
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine frgauss(hcntr,sigma,h1,h2,fract)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812               frgauss
c                j. scire, d. strimaitis,  earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  compute the fraction of a gaussian distribution between
c               two limits
c
c --- inputs:
c            hcntr - real    - center of gaussian distribution (m)
c            sigma - real    - standard deviation (m) of the
c                              distribution
c           h1, h2 - real    - limits between which the distribution
c                              is to be integrated
c
c --- output:
c            fract - real    - fraction of the gaussian distribution
c                              between h1 and h2
c
c --- frgauss called by: wake_fqc
c --- frgauss calls:     erfdif
c----------------------------------------------------------------------
c
      data SQRT2/1.4142136/,small/1.e-5/
c
c --- prevent numerical problems with very small sigmas
      s=SQRT2*max(sigma,small)
c
      z1=(h1-hcntr)/s
      z2=(h2-hcntr)/s
c
      fract=0.5*ABS(erfdif(z1,z2))
c
      return
      end
c----------------------------------------------------------------------
      function erfdif(x1,x2)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812                erfdif
c
c     taken from:
c --- calpuff    version: 4.0       level: 900228                erfdif
c                r. yamartino, src
c
c --- purpose:  computes the difference: erfdif = erf(x1) - erf(x2).
c               various methods are used to avoid roundoff errors
c               depending on the values of the two arguments.
c
c --- inputs:
c
c                x1 - real    - argument 1 (no units)
c                x2 - real    - argument 1 (no units)
c
c --- outputs:
c
c            erfdif - real    - erf(x1) - erf(x2)
c
c --- erfdif called by:  frgauss
c --- erfdif calls:      erf,erfc
c----------------------------------------------------------------------
c *** v3.21
c
      erfdif=0.0
      if(x1 == x2) go to 40
      if((x1*x2) <= 0.0) go to 50
      xtest=ABS(x2)
      if(ABS(x1) < xtest) xtest=ABS(x1)
c --- some compilers cannot handle reals  <  1.18e-38, so reset cut
c     if(xtest >= 13.306) go to 40
      if(xtest  >=  9.15) go to 40
      if(xtest < 0.47) go to 50
c     can only reach here when x1 and x2 have same sign.
      isign=1
      xx1=x1
      xx2=x2
      if(x1 > 0.0) go to 30
      isign=-1
      xx1=-xx1
      xx2=-xx2
c  30 erfdif=isign*(erfc(xx2)-erfc(xx1))
   30 erfcx1=0.0
      erfcx2=0.0
c --- some compilers cannot handle reals  <  1.18e-38, so reset cut
c     if(xx1 < 13.306) erfcx1=erfc(xx1)
c     if(xx2 < 13.306) erfcx2=erfc(xx2)
      if(xx1  <  9.15) erfcx1=erfc_prime(xx1)
      if(xx2  <  9.15) erfcx2=erfc_prime(xx2)
      erfdif=isign*(erfcx2-erfcx1)
c --- protect against flakey lahey compiler 4/9/89
      if(erfcx2 == erfcx1) erfdif=0.0
   40 return
   50 erfdif=erf(x1)-erf(x2)
      return
      end
c-----------------------------------------------------------------------
      function erf(xx)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812                   erf
c
c     taken from:
c --- calpuff    version: 4.0       level: 941228                   erf
c                r. yamartino, src
c
c --- purpose:  computes the error function, erf(x).
c ---           this is the quick medium accuracy error function from
c ---           nbs 55.  using an approximation due to hastings;
c ---           absolute error about 3e-7
c
c
c --- inputs:
c
c                xx - real    - argument  (no units)
c
c --- outputs:
c
c               erf - real    - error function of x
c
c --- erf called by:  erfdif
c --- erf calls:   no routines
c----------------------------------------------------------------------
c
      real x, xx ,t, t16, a(6)
      data a/0.0000430638, 0.0002765672, 0.0001520143,
     *       0.0092705272, 0.0422820123, 0.0705230784/
      data xcut/ 3.919206/
c
      x = ABS(xx)
      if(x  >  xcut) then
         t16 = 0.0
      else
c
         t = ((((((((( a(1)*x + a(2) ) * x ) + a(3) ) * x ) + a(4) ) *
     x                    x ) + a(5) ) * x ) + a(6) ) * x
c
         t = 1.0 / (t + 1.0)
c
         t16 = t * t * t * t
         t16 = t16 * t16 * t16 * t16
      endif
c
      if(xx  >  0.0) then
         erf =  1.0 - t16
      else
         erf =  t16 - 1.0
      endif
c
      return
      end
c-----------------------------------------------------------------------
      function erfc_prime(xx)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812                  erfc
c
c     taken from:
c --- calpuff    version: 4.0       level: 941228                  erfc
c                r. yamartino, src
c
c --- purpose:  computes the complementary error function, 1-erf(x).
c ---           this is the quick medium accuracy comp. error function
c ---           from nbs 55.  using an approximation due to hastings;
c ---           absolute error about 3e-7.  asymptotic expression added
c ---           for large xx to reduce percent error.
c
c
c --- inputs:
c
c                xx - real    - argument  (no units)
c
c --- outputs:
c
c              erfc - real    - complementary error function of x
c
c --- erfc called by:  erfdif
c --- erfc calls:   no routines
c-----------------------------------------------------------------------
c
      real x, xx ,t, t16, a(6)
      data a/0.0000430638, 0.0002765672, 0.0001520143,
     *       0.0092705272, 0.0422820123, 0.0705230784/
      data xcutl/-3.919206/
      data xcuth/13.306   /
      data rtpii/0.5641896/
c
      if(xx  >  xcuth) then
         erfc_prime = 0.0
c
      elseif(xx  <  xcutl) then
         erfc_prime = 2.0
c
      elseif(xx  >  2.79) then
        x = ABS(xx)
        z = 1.0 / x
        erfc_prime = rtpii * z * exp(-x*x) * 
     *               ( 1.0 - 0.5*z*z*(1.0-1.5*z*z) )
c
      else
         x = ABS(xx)
         t = ((((((((( a(1)*x + a(2) ) * x ) + a(3) ) * x ) + a(4) ) *
     x                    x ) + a(5) ) * x ) + a(6) ) * x
c
         t = 1.0 / (t + 1.0)
c
c        erfc = t**16   for x > 0
         t16 = t * t * t * t
         t16 = t16 * t16 * t16 * t16
c
         if(xx  >  0.0) then
            erfc_prime =  t16
         else
            erfc_prime =  2.0 - t16
         endif
c
      endif
c
      return
      end
c----------------------------------------------------------------------
      subroutine wake_xsig(x,rise,nobid,sz,sy,szc,syc)
c----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812             wake_xsig
c                d. strimaitis,  earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  extract sigmas in the wake by interpolating among the
c               stored values; compute sigmas outside tabulated range
c               using host sigma curves with bid or virtual source
c               adjustments
c
c --- inputs:
c                x - real       - downwind distance (m) from source
c             rise - real       - gradual plume rise (m)
c            nobid - logical    - directs use of buoyancy enhancement
c
c     common block /params/ variables:
c           mxntr
c     common block /wakedat/ variables:
c           nwak, xwak(mxntr), szwak(mxntr), sywak(mxntr),
c           xzvwak, xyvwak
c           ncav, xcav(mxntr), szcav(mxntr), sycav(mxntr),
c           xzvcav, xyvcav
c
c --- output:
c               sz - real       - sigma-z (m) at downwind distance x
c                                 due to primary source
c               sy - real       - sigma-y (m) at downwind distance x
c                                 due to primary source
c              szc - real       - sigma-z (m) of cavity source at
c                                 downwind distance x from primary source
c              syc - real       - sigma-y (m) of cavity source at
c                                 downwind distance x from primary source
c
c --- wake_xsig called by:  pdis
c --- wake_xsig calls:      sigz, sigy
c----------------------------------------------------------------------
c
      use wakedat_pri
c
      logical nobid

c --- primary source:
c -------------------
      if(x <= 0.0) then
c ---    report null values (these should never get used!)
         sz=0.0
         sy=0.0
      elseif(nwak <= 1) then
c ---    plume never altered by wake turbulence; use host curves
         call sigz(x,sz)
         call sigy(x,sy)
         if(.not.nobid) then
            bidsq=(rise/3.5)**2
            sz=SQRT(sz**2+bidsq)
            sy=SQRT(sy**2+bidsq)
         endif
      elseif(x < xwak(1)) then
c ---    point lies upwind of wake region; use host curves
         call sigz(x,sz)
         call sigy(x,sy)
         if(.not.nobid) then
            bidsq=(rise/3.5)**2
            sz=SQRT(sz**2+bidsq)
            sy=SQRT(sy**2+bidsq)
         endif
      elseif(x > xwak(nwak)) then
c ---    point lies downwind of transition to ambient growth; use
c ---    host curves with virtual distance adjustment
         call sigz(x+xzvwak,sz)
         call sigy(x+xyvwak,sy)
      else
c ---    point lies within range of tabulated values
         nwkm1=nwak-1
         sz=szwak(1)
         sy=sywak(1)
         do i=nwkm1,1,-1
            if(x >= xwak(i))then
               ip1=i+1
               fac=(xwak(ip1)-x)/(xwak(ip1)-xwak(i))
               sz=szwak(ip1)-(szwak(ip1)-szwak(i))*fac
               sy=sywak(ip1)-(sywak(ip1)-sywak(i))*fac
               goto 50
            endif
        end do
      endif

c --- cavity source:
c -------------------
50    if(ncav <= 1) then
c ---    no contribution from cavity source (report initial values)
         szc=szcav(1)
         syc=sycav(1)
      elseif(x < xcav(1)) then
c ---    point lies upwind of cavity region (report initial values)
         szc=szcav(1)
         syc=sycav(1)
      elseif(x > xcav(ncav)) then
c ---    point lies downwind of transition to ambient growth; use
c ---    host curves with virtual distance adjustment
         call sigz(x+xzvcav,szc)
         call sigy(x+xyvcav,syc)
      else
c ---    point lies within range of tabulated values
         ncvm1=ncav-1
         szc=szcav(1)
         syc=sycav(1)
         do i=ncvm1,1,-1
            if(x >= xcav(i))then
               ip1=i+1
               fac=(xcav(ip1)-x)/(xcav(ip1)-xcav(i))
               szc=szcav(ip1)-(szcav(ip1)-szcav(i))*fac
               syc=sycav(ip1)-(sycav(ip1)-sycav(i))*fac
               return
            endif
         end do
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine cavity_ht(h,w,l,r,lc,lr,hr,x,zcav,ycav)
c-----------------------------------------------------------------------
c
c --- prime      version:  1.0     level:  970812             cavity_ht
c                l. schulman, earth tech
c                prepared for epri under contract wo3527-01
c
c --- purpose:  calculates height of cavity envelope as function of x
c
c --- inputs:
c                h - real              - building height above ground
c                w - real              - projected building width
c                l - real              - along-wind building length
c                                        building face
c                r - real              - scale length from h and w
c               lc - real              - length of roof cavity
c               lr - real              - length of downwind cavity from
c                                         lee face
c               hr - real              - maximum cavity height above
c                                         ground
c                x - real              - downwind distances
c
c --- output:
c
c          zcav    - real              - cavity height as function of x
c
c          ycav    - real              - cavity half-width as f(x)

c --- cavity_ht called by:  prime
c --- cavity_ht calls:      none
c----------------------------------------------------------------------
c
c
c
      real h,w,l,r,hr,lr,lc
      real x

c --- initialize
      zcav=0.0
      ycav=0.0

c --- cavity is not present upwind of bldg or at/beyond l+lr
      if(x >= (l+lr)) then
         return
      elseif(x < 0.0) then
         return
      endif
c
c     calculate x-y near wake boundary
c
      if(x >= 0. .and. x <= r) then
        ycav=(w/2.+r/3.)-(x-r)**2/(3.*r)
      elseif(x > r .and. x <= (l+lr)) then
        ycav=(w/2.+r/3.)*(1.-((x-r)/(l+lr-r))**2)**0.5
      endif

c     calculate x-z near wake boundary
c
      if(lc  <  l)then       ! reattachment
c
        if(x >= 0. .and. x <= l) then
          zcav=h
        elseif(x >= l .and. x <= (l+lr)) then
          zcav=h*(1.-((x-l)/lr)**2)**0.5
        endif
c
      else                    ! nonreattachment
        if(x >= 0. .and. x <= 0.5*r) then
        zcav=hr+4.*(x-0.5*r)**2*(h-hr)/(r**2)
        elseif(x > 0.5*r .and. x <= (l+lr)) then
          zcav=hr*(1.-((x-0.5*r)/(l+lr-0.5*r))**2)**0.5
        endif
      endif
c
      return
      end
