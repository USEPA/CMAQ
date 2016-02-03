
! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/mcip2arl/stcm2p.f,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $

      SUBROUTINE STCM2P(STRCMP, X1,Y1, XLAT1,XLONG1,                           &
       X2,Y2, XLAT2,XLONG2)
!*  WRITTEN ON 3/31/94 BY Dr. Albion Taylor  NOAA / OAR / ARL
      REAL STRCMP(9)
      DO K=3,6
        STRCMP (K) = 0.
      ENDDO
      STRCMP (5) = 1.
      STRCMP (7) = 1.
      CALL CLL2XY (STRCMP, XLAT1,XLONG1, X1A,Y1A)
      CALL CLL2XY (STRCMP, XLAT2,XLONG2, X2A,Y2A)
      DEN = SQRT( (X1 - X2)**2 + (Y1 - Y2)**2 )
      DENA = SQRT( (X1A - X2A)**2 + (Y1A - Y2A)**2 )
      STRCMP(5) = ((X1A - X2A)*(X1 - X2) + (Y1A - Y2A) * (Y1 - Y2))            &
        /DEN /DENA
      STRCMP(6) = ((Y1A - Y2A)*(X1 - X2) - (X1A - X2A) * (Y1 - Y2))            &
        /DEN /DENA
      STRCMP (7) = STRCMP(7) * DENA / DEN
      CALL CLL2XY (STRCMP, XLAT1,XLONG1, X1A,Y1A)
      STRCMP(3) = STRCMP(3) + X1 - X1A
      STRCMP(4) = STRCMP(4) + Y1 - Y1A
      RETURN
      END
