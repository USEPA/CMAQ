
! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/mcip2arl/cxy2ll.f,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $

	SUBROUTINE CXY2LL (STCPRM, X,Y, XLAT,XLONG)
!*  WRITTEN ON 3/31/94 BY Dr. Albion Taylor  NOAA / OAR / ARL
      PARAMETER (REARTH=6371.2)
      REAL STCPRM(9)
      XI0 = ( X - STCPRM(3) ) * STCPRM(7) / REARTH
      ETA0 = ( Y - STCPRM(4) ) * STCPRM(7) /REARTH
      XI = XI0 * STCPRM(5) - ETA0 * STCPRM(6)
      ETA = ETA0 * STCPRM(5) + XI0 * STCPRM(6)
      CALL CNXYLL(STCPRM, XI,ETA, XLAT,XLONG)
      XLONG = CSPANF(XLONG, -180., 180.)
      RETURN
      END
