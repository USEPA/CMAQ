
      SUBROUTINE CNLLXY (STRCMP, XLAT,XLONG, XI,ETA)
!*  WRITTEN ON 3/31/94 BY Dr. Albion Taylor  NOAA / OAR / ARL
!  MAIN TRANSFORMATION ROUTINE FROM LATITUDE-LONGITUDE TO
!  CANONICAL (EQUATOR-CENTERED, RADIAN UNIT) COORDINATES
      PARAMETER (PI=3.14159265358979,RADPDG=PI/180,DGPRAD=180/PI)
      PARAMETER (ALMST1=.99999)
      REAL STRCMP(9)
       DOUBLE PRECISION GAMMA
      DOUBLE PRECISION DLONG,DLAT,SLAT,MERCY,GMERCY
      GAMMA = STRCMP(1)
      DLAT = XLAT
      DLONG = CSPANF(XLONG - STRCMP(2), -180., 180.)
      DLONG = DLONG * RADPDG
      GDLONG = GAMMA * DLONG
      IF (ABS(GDLONG) .LT. .01) THEN
!  CODE FOR GAMMA SMALL OR ZERO.  THIS AVOIDS ROUND-OFF ERROR OR DIVIDE-
!  BY ZERO IN THE CASE OF MERCATOR OR NEAR-MERCATOR PROJECTIONS.
        GDLONG = GDLONG * GDLONG
        SNDGAM = DLONG * (1. - 1./6. * GDLONG *                                &
                           (1. - 1./20. * GDLONG *                             &
                           (1. - 1./42. * GDLONG )))
        CSDGAM = DLONG * DLONG * .5 *                                          &
                           (1. - 1./12. * GDLONG *                             &
                           (1. - 1./30. * GDLONG *                             &
                           (1. - 1./56. * GDLONG )))
      ELSE
! CODE FOR MODERATE VALUES OF GAMMA
        SNDGAM = SIN (GDLONG) /GAMMA
        CSDGAM = (1. - COS(GDLONG) )/GAMMA /GAMMA
      ENDIF
      SLAT = SIN(RADPDG * DLAT)
      IF ((SLAT .GE. ALMST1) .OR. (SLAT .LE. -ALMST1)) THEN
        ETA = 1./STRCMP(1)
        XI = 0.
        RETURN
      ENDIF
      MERCY = .5 * LOG( (1. + SLAT) / (1. - SLAT) )
      GMERCY = GAMMA * MERCY
      IF (ABS(GMERCY) .LT. .001) THEN
!  CODE FOR GAMMA SMALL OR ZERO.  THIS AVOIDS ROUND-OFF ERROR OR DIVIDE-
!  BY ZERO IN THE CASE OF MERCATOR OR NEAR-MERCATOR PROJECTIONS.
        RHOG1 = MERCY * (1. - .5 * GMERCY *                                    &
                          (1. - 1./3. * GMERCY *                               &
                          (1. - 1./4. * GMERCY ) ) )
      ELSE
! CODE FOR MODERATE VALUES OF GAMMA
        RHOG1 = (1. - EXP(-GMERCY)) / GAMMA
      ENDIF
      ETA = RHOG1 + (1. - GAMMA * RHOG1) * GAMMA * CSDGAM
      XI = (1. - GAMMA * RHOG1 ) * SNDGAM
      END
