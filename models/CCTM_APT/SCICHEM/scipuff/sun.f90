!*******************************************************************************
!$RCSfile: sun.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine sunrs(jul,lat,n,tsr,tss)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Calculates time of sunrise and sunset on the prime meridian
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS

integer jul  !Julian day
integer n    !Number of y grid points

real lat(n)  !Latitude array
real tsr(n)  !Time of sunrise
real tss(n)  !Time of sunset

! --- PARAMETERS

real PI, PI180, YRFAC, DEGHR, HFAC
parameter (PI    = 3.141592      )
parameter (PI180 = PI/180.       )
parameter (YRFAC = 2.*PI/365.242 )
parameter (DEGHR = 15.           )
parameter (HFAC  = 1./DEGHR/PI180)

! --- LOCALS

integer i
real day, snd, sigma, capd, tcapd, tem, hsr, arg

day     = (float(jul)-1.)*YRFAC
snd     = sin(day)
sigma   = 4.871 + day + 0.033*snd
capd    = asin(0.398*sin(sigma))
tcapd   = tan(capd)
tem     = 0.043*sin(2.*sigma) - 0.033*snd

do i = 1,n
  arg    = -tan(PI180*lat(i))*tcapd
  if (arg <= -1.) then
    tss(i) =  25.
    tsr(i) = -1.
  else if (arg >= 1.) then
    tss(i) = -1.
    tsr(i) =  25.
  else
    hsr    = acos(arg)
    tss(i) = 12. + ( hsr - tem)*HFAC
    tsr(i) = 12. + (-hsr - tem)*HFAC
  end if
end do

return
end

subroutine sun(lat,lon,jul,t_local,sun_ang)
!*******************************************************************************
!
! FUNCTION: Calculates the solar elevation angle for a given hour of the day from
!           the date, latitude
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none


! --- ARGUMENTS
 
integer jul    !Julian day

real lat, lon  !Latitude and longitude
real t_local   !Local time
real sun_ang   !Sun angle

! --- PARAMETERS

real PI, PI180, YRFAC, DEGHR, HFAC
parameter (PI    = 3.141592     )
parameter (PI180 = PI/180.      )
parameter (YRFAC = 2.*PI/365.242)
parameter (DEGHR = 15.          )
parameter (HFAC  = DEGHR*PI180  )

! --- LOCALS

real day, snd, sigma, capd, solha

day     = (float(jul)-1.)*YRFAC
snd     = sin(day)
sigma   = 4.871 + day + 0.033*snd
capd    = asin(0.398*sin(sigma))

solha   = (t_local-12.)*HFAC &
                      + 0.043*sin(2.*sigma) - 0.033*snd

sun_ang = asin(sin(PI180*lat)*sin(capd)+cos(PI180*lat)*cos(capd)*cos(solha))
sun_ang = sun_ang/PI180

return
end

subroutine total(sun_ang,cc,qr)
!*******************************************************************************
!
! FUNCTION:  Calculates the total incoming solar radiation from cloud cover and
!            solar elevation angle using the Holtslag-Van Ulden Technique.
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real sun_ang !Sun angle
real cc      !Fractional cloud cover
real qr      !Total incoming solar radiation

! --- PARAMETERS

real PI, PI180
parameter (PI    = 3.141592     )
parameter (PI180 = PI/180.      )

real R0, C0, B1, B2
parameter (R0 = 990.)
parameter (C0 = 30. )
parameter (B1 = 0.75)
parameter (B2 = 3.4 )

!------ sun_ang assumed >= 0 (in degrees)

qr = R0*sin(PI180*sun_ang) - C0

if (cc > 0) qr = qr*(1.0-B1*(cc**B2))

return
end


function local_time(tz,lon)
!*******************************************************************************
!
! FUNCTION:  Calculate local sun time
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real tz   !time in UTC
real lon  !Longitude

! --- PARAMETERS

real DEGHR, HR24
parameter (DEGHR = 15.)
parameter (HR24  = 24.)

! --- LOCALS

real local_time


local_time = tz + lon/DEGHR

if (local_time >= HR24) then
  local_time = local_time - HR24
else if (local_time < 0.) then
  local_time = local_time + HR24
end if

return
end


subroutine decay_fac
!*******************************************************************************
!
! FUNCTION:  Calculate the diurnal decay factor based on the time of day
!            (peaks at noon)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                     sun              local_time
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met

implicit none

! --- PARAMETERS

real HR24
parameter (HR24 = 24.)

! --- LOCALS

integer imat, ilat

real tz, rlon, t_local, sun_ang, frac, sun_fac, r1, r2
real local_time

if (bl_type == 'CALC') then

  if (local) then
    tz = t/3600. + tstart - tzone
  else
    tz = t/3600. + tstart
  end if

  tz      = tz - HR24*float(int(tz/HR24))
  rlon    = lon_bl(max0(nxbl/2,1))
  t_local = local_time(tz,rlon)
  ilat    = max0(nybl/2,1)
  if (t_local > sr(ilat) .and. t_local < ss(ilat)) then
    call sun(lat_bl(ilat),rlon,jul_met,t_local,sun_ang)
    sun_fac = sin(pi180*sun_ang)
  else
    sun_fac = 0.
  end if

else

  if (local) then
    frac = (t/3600. + tstart)/HR24
  else
    frac = (t/3600. + tstart + tzone)/HR24
  end if
  frac = frac - int(frac)
  if (frac < 0.) frac = frac + 1.
  if (frac > 0.25 .and. frac < 0.75) then
    sun_fac = sin((frac-0.25)*pi2)
  else
    sun_fac = 0.
  end if

end if

do imat = 1,ntypm
  r1 = material(imat)%prop(1)
  r2 = material(imat)%prop(2)
  decay_rate(imat) = max(r1*sun_fac,r2)
end do

return
end
