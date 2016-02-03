	SUBROUTINE CLL2XY (STCPRM, XLAT,XLONG, X,Y)
!*  WRITTEN ON 3/31/94 BY Dr. Albion Taylor  NOAA / OAR / ARL
      PARAMETER (REARTH=6371.2)
      REAL STCPRM(9)
      CALL CNLLXY(STCPRM, XLAT,XLONG, XI,ETA)
      X = STCPRM(3) + REARTH/STCPRM(7) *                                       &
                   (XI * STCPRM(5) + ETA * STCPRM(6) )
      Y = STCPRM(4) + REARTH/STCPRM(7) *                                       &
                   (ETA * STCPRM(5) - XI * STCPRM(6) )
      RETURN
      END
