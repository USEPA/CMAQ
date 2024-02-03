C ===================================================================
C This file is the include file for calculating sensitivity of 
C the activity coefficient of ISORROPIAII
C 
C Written by Wenxian Zhang in August 2011
C
C Reference: 
C Zhang, W., Capps, S. L., Hu, Y., Nenes, A., Napelenok, S. L., & 
C     Russell, A. G. (2012). Development of the high-order decoupled 
C     direct method in three dimensions for particulate matter: 
C     enabling advanced sensitivity analysis in air quality models. 
C     Geoscientific Model Development, 5(2), 355-368. 
C     doi: 10.5194/gmd-5-355-2012
C ===================================================================



      DOUBLE PRECISION DI(NIONSPC)
      DOUBLE PRECISION G0P(NPAIR)
      DOUBLE PRECISION DG0(NIONSPC,NPAIR)
      DOUBLE PRECISION H,DH
      DOUBLE PRECISION X(6,4), Y(6,4)
      DOUBLE PRECISION DX(NIONSPC,6,4),DY(NIONSPC,6,4)
      INTEGER IJMAP(6,4)
      DOUBLE PRECISION AGAMA

      COMMON /HDDMISO/ H,   G0P,  X,   Y,
     &                 DI,  DY,  DX,  DG0, DH,
     &                 AGAMA, IJMAP
