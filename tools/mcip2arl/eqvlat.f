
! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/mcip2arl/eqvlat.f,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $

      REAL FUNCTION EQVLAT (XLAT1,XLAT2)
!*  WRITTEN ON 3/31/94 BY Dr. Albion Taylor  NOAA / OAR / ARL
      PARAMETER (PI=3.14159265358979,RADPDG=PI/180,DGPRAD=180/PI)
      SIND(X) = SIN (RADPDG*X)
      SINL1 = SIND (XLAT1)
      SINL2 = SIND (XLAT2)
      IF (ABS(SINL1 - SINL2) .GT. .001) THEN
        AL1 = LOG((1. - SINL1)/(1. - SINL2))
        AL2 = LOG((1. + SINL1)/(1. + SINL2))
      ELSE
!  CASE LAT1 NEAR OR EQUAL TO LAT2
        TAU = - (SINL1 - SINL2)/(2. - SINL1 - SINL2)
        TAU = TAU*TAU
        AL1  = 2. / (2. - SINL1 - SINL2) * (1.    + TAU *                      &
                                          (1./3. + TAU *                       &
                                         (1./5. + TAU *                        &
                                         (1./7.))))
        TAU =   (SINL1 - SINL2)/(2. + SINL1 + SINL2)
        TAU = TAU*TAU
        AL2  = -2. / (2. + SINL1 + SINL2) * (1.    + TAU *                     &
                                           (1./3. + TAU *                      &
                                          (1./5. + TAU *                       &
                                          (1./7.))))
      ENDIF
      EQVLAT = ASIN((AL1 + AL2) / (AL1 - AL2))/RADPDG
      RETURN
      END
