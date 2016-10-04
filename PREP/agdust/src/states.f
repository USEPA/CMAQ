
      character( 2 ) function state( lon, lat )
      implicit none

C Projection (Lambert Conformal)
C  P_ALP = 33.0
C  P_BET = 45.0
C  P_GAM = -97.0
C  XCENT = -97.0
C  YCENT = 40.0

      character( 2 ) :: st
      real lon, lat, x, y

      st = 'xx'
      X = LON
      Y = LAT
      IF      ( ( X .LE. - 85.1 .AND. X .GT. - 88.4   .AND.
     &            Y .GE.   31.0 .AND. Y .LT.   35.0 ) .OR.
     &          ( X .LE. - 87.3 .AND. X .GT. - 88.4   .AND.
     &            Y .GE.   30.0 .AND. Y .LT.   31.0 ) ) THEN
         st = 'AL'                                      ! AL
      ELSE IF ( ( X .LE. - 91.0 .AND. X .GT. - 94.5   .AND.
     &            Y .GE.   33.0 .AND. Y .LT.   36.5 ) .OR.
     &          ( X .LE. - 90.0 .AND. X .GT. - 91.0   .AND.
     &            Y .GE.   35.0 .AND. Y .LT.   36.5 ) ) THEN
         st = 'AR'                                      ! AR
         IF ( X .LE. - 94.0 .AND. X .GT. - 94.5 .AND.
     &        Y .GE.   33.0 .AND. Y .LT.   33.7 ) st = 'TX'   ! TX
      ELSE IF ( X .LE. -109.0 .AND. X .GT. -114.0 .AND.
     &          Y .GE.   31.2 .AND. Y .LT.   37.0) THEN             
         st = 'AZ'                                      ! AZ
      ELSE IF ( X .LE. -120.0 .AND. X .GT. -125.0 .AND.
     &          Y .GE.   34.2 .AND. Y .LT.   42.0 ) THEN
         st = 'CA'                                      ! CA
      ELSE IF ( X .LE. -102.0 .AND. X .GT. -109.0 .AND.
     &          Y .GE.   37.0 .AND. Y .LT.   41.0 ) THEN
         st = 'CO'                                      ! CO
      ELSE IF ( X .LE. - 71.9 .AND. X .GT. - 73.4 .AND.
     &          Y .GE.   41.0 .AND. Y .LT.   42.0 ) THEN
         st = 'CT'                                      ! CT
      ELSE IF ( ( X .LE. - 80.0 .AND. X .GT. - 85.0   .AND.
     &            Y .GE.   25.0 .AND. Y .LT.   30.8 ) .OR.
     &          ( X .LE. - 85.0 .AND. X .GT. - 87.3   .AND.
     &            Y .GE.   29.5 .AND. Y .LT.   31.0 ) ) THEN
         st = 'FL'                                      ! FL
      ELSE IF ( X .LE. - 75.0 .AND. X .GT. - 85.1  .AND.
     &          Y .GE.   30.8 .AND. Y .LT.   35.0 ) THEN
         st = 'GA'                                      ! GA
         IF ( ( X .GE. - 83.0 .AND. Y .GE.   34.0 ) .OR.
     &        ( X .GE. - 82.0 .AND. Y .GE.   33.0 ) .OR.
     &        ( X .GE. - 81.3 .AND. Y .GE.   32.0 ) ) THEN
            st = 'SC'                                   ! SC
            IF ( X .GE. -79.2 .AND. Y .GE.  34.0 ) THEN
               st = 'NC'                                ! NC south
            END IF
         END IF
      ELSE IF ( ( X .LE. - 91.0 .AND. X .GT. - 96.0   .AND.
     &            Y .GE.   40.6 .AND. Y .LT.   43.5 ) .OR.
     &          ( X .LE. - 90.0 .AND. X .GT. - 91.0   .AND.
     &            Y .GE.   41.3 .AND. Y .LT.   42.7 ) .OR.
     &          ( X .LE. - 96.0 .AND. X .GT. - 96.6   .AND.
     &            Y .GE.   43.0 .AND. Y .LT.   43.5 ) ) THEN
         st = 'IA'                                      ! IA
      ELSE IF ( ( X .LE. - 90.0 .AND. X .GT. - 91.0      .AND.
     &               Y .GE.   39.0 .AND. Y .LE.   41.3 ) .OR.
     &             ( X .LE. - 87.6 .AND. X .GT. - 90.0   .AND.
     &               Y .GE.   38.0 .AND. Y .LT.   42.5 ) .OR.
     &             ( X .LE. - 88.0 .AND. X .GT. - 89.3   .AND.
     &               Y .GE.   37.0 .AND. Y .LT.   38.0 ) ) THEN
         st = 'IL'                                      ! IL
      ELSE IF ( ( X .LE. - 84.9 .AND. X .GT. - 87.6   .AND.
     &            Y .GE.   38.8 .AND. Y .LT.   41.9 ) .OR.
     &          ( X .LE. - 85.6 .AND. X .GT. - 87.6   .AND.
     &            Y .GE.   37.9 .AND. Y .LT.   38.9 ) ) THEN
         st = 'IN'                                      ! IN
      ELSE IF ( X .LE. -111.0 .AND. X .GT. -117.0 .AND.
     &          Y .GE.   42.0 .AND. Y .LE.   49.0 ) THEN
         st = 'ID'                                      ! ID
         IF ( ( X .LE. -111.0 .AND. X .GT. -113.0   .AND.
     &          Y .GE.   44.6 .AND. Y .LE.   49.0 ) .OR.
     &        ( X .LE. -113.0 .AND. X .GT. -114.0   .AND.
     &          Y .GE.   45.0 .AND. Y .LE.   49.0 ) .OR.
     &        ( X .LE. -114.0 .AND. X .GT. -114.6   .AND.
     &          Y .GE.   45.5 .AND. Y .LE.   49.0 ) .OR.
     &        ( X .LE. -114.6 .AND. X .GT. -116.0   .AND.
     &          Y .GE.   47.0 .AND. Y .LE.   49.0 ) ) THEN
            st = 'MT'                                   ! MT west
         END IF
      ELSE IF ( X .LE. - 94.7 .AND. X .GT. -102.0 .AND.
     &          Y .GE.   37.0 .AND. Y .LT.   40.0 ) THEN
         st = 'KS'                                      ! KS
      ELSE IF ( ( X .LE. - 88.0 .AND. X .GT. - 89.0   .AND.
     &            Y .GE.   36.7 .AND. Y .LE.   37.0 ) .OR.
     &          ( X .LE. - 85.6 .AND. X .GT. - 88.0   .AND.
     &            Y .GE.   36.7 .AND. Y .LT.   38.0 ) .OR.
     &          ( X .LE. - 82.6 .AND. X .GT. - 85.6   .AND.
     &            Y .GE.   36.7 .AND. Y .LT.   38.8 ) ) THEN
         st = 'KY'                                      ! KY
      ELSE IF ( ( X .LE. - 91.0 .AND. X .GT. - 93.8   .AND.
     &            Y .GE.   29.0 .AND. Y .LE.   33.0 ) .OR.
     &          ( X .LE. - 89.8 .AND. X .GT. - 91.0   .AND.
     &            Y .GE.   29.0 .AND. Y .LT.   31.0 ) .OR.
     &          ( X .LE. - 88.8 .AND. X .GT. - 89.8   .AND.
     &            Y .GE.   29.0 .AND. Y .LT.   30.1 ) ) THEN
         st = 'LA'                                      ! LA
      ELSE IF ( ( X .LE. - 70.6 .AND. X .GT. - 73.2   .AND.
     &            Y .GE.   42.0 .AND. Y .LT.   42.8 ) .OR.
     &          ( X .LE. - 70.4 .AND. X .GT. - 71.2   .AND.
     &            Y .GE.   41.4 .AND. Y .LT.   42.0 ) ) THEN
         st = 'MA'                                      ! MA
      ELSE IF ( X .LE. - 75.0 .AND. X .GT. - 77.2 .AND.
     &          Y .GE.   38.0 .AND. Y .LT.   39.8 ) THEN
         st = 'MD'                                      ! MD 
         IF ( X .LE. - 75.0 .AND. X .GT. - 75.8 .AND.
     &        Y .GE.   38.2 .AND. Y .LT.   39.8 ) st = 'DE'   ! DE
      ELSE IF ( X .LE. - 67.0 .AND. X .GT. - 71.0 .AND.
     &          Y .GE.   43.0 .AND. Y .LE.   47.3 ) THEN
         st = 'ME'                                      ! ME
      ELSE IF ( X .LE. - 82.5 .AND. X .GT. - 86.6 .AND.
     &          Y .GE.   41.8 .AND. Y .LT.   46.0 ) THEN
         st = 'MI'                                      ! MI
      ELSE IF ( ( X .LE. - 92.2 .AND. X .GT. - 97.0   .AND.
     &            Y .GE.   46.0 .AND. Y .LE.   49.0 ) .OR.
     &          ( X .LE. - 92.8 .AND. X .GT. - 96.6   .AND.
     &            Y .GE.   43.5 .AND. Y .LT.   46.0 ) .OR.
     &          ( X .LE. - 92.0 .AND. X .GT. - 92.8   .AND.
     &            Y .GE.   43.5 .AND. Y .LT.   44.4 ) .OR.
     &          ( X .LE. - 89.5 .AND. X .GT. - 92.2   .AND.
     &            Y .GE.   46.9 .AND. Y .LE.   48.1 ) .OR.
     &          ( X .LE. - 91.0 .AND. X .GT. - 92.0   .AND.
     &            Y .GE.   43.5 .AND. Y .LT.   44.0 ) ) THEN
         st = 'MN'                                      ! MN
      ELSE IF ( ( X .LE. - 91.0 .AND. X .GT. - 94.7   .AND.
     &            Y .GE.   36.5 .AND. Y .LE.   40.6 ) .OR.
     &          ( X .LE. - 94.7 .AND. X .GT. - 96.0   .AND.
     &            Y .GE.   40.0 .AND. Y .LT.   40.6 ) .OR.
     &          ( X .LE. - 90.0 .AND. X .GT. - 91.0   .AND.
     &            Y .GE.   36.5 .AND. Y .LT.   39.0 ) .OR.
     &          ( X .LE. - 89.3 .AND. X .GT. - 90.0   .AND.
     &            Y .GE.   36.5 .AND. Y .LT.   38.0 ) ) THEN
         st = 'MO'                                      ! MO
         IF ( X .LE. - 95.7 .AND. X .GT. - 96.0 .AND.
     &        Y .GE.   40.0 .AND. Y .LT.   40.6 ) st = 'NE'   ! NE
      ELSE IF ( ( X .LE. - 88.4 .AND. X .GT. - 91.0   .AND.
     &            Y .GE.   31.0 .AND. Y .LT.   35.0 ) .OR.
     &          ( X .LE. - 88.4 .AND. X .GT. - 89.8   .AND.
     &            Y .GE.   30.1 .AND. Y .LT.   31.0 ) ) THEN
         st = 'MS'                                      ! MS
         IF ( X .LE. - 90.5 .AND. X .GT. - 91.0 .AND.
     &        Y .GE.   34.3 .AND. Y .LT.   35.0 ) st = 'AR'   ! AR 
      ELSE IF ( X .LE. -104.0 .AND. X .GT. -111.0 .AND.
     &          Y .GE.   45.0 .AND. Y .LE.   49.0 ) THEN
         st = 'MT'                                      ! MT
      ELSE IF ( X .LE. - 75.0 .AND. X .GT. - 82.0 .AND.
     &          Y .GE.   35.0 .AND. Y .LT.   36.7 ) THEN
         st = 'NC'                                      ! NC
      ELSE IF ( X .LE. - 97.0 .AND. X .GT. -104.0 .AND.
     &          Y .GE.   46.0 .AND. Y .LE.   49.0 ) THEN
         st = 'ND'                                      ! ND
      ELSE IF ( ( X .LE. - 96.0 .AND. X .GT. -102.0   .AND.
     &            Y .GE.   40.0 .AND. Y .LT.   43.0 ) .OR.
     &          ( X .LE. -102.0 .AND. X .GT. -104.0   .AND.
     &            Y .GE.   41.0 .AND. Y .LT.   43.0 ) ) THEN
         st = 'NE'                                      ! NE
      ELSE IF ( ( X .LE. - 71.0 .AND. X .GT. - 71.7   .AND.
     &            Y .GE.   42.8 .AND. Y .LT.   45.0 ) .OR.
     &          ( X .LE. - 71.7 .AND. X .GT. - 72.4   .AND.
     &            Y .GE.   42.8 .AND. Y .LT.   44.0 ) ) THEN
         st = 'NH'                                      ! NH
      ELSE IF ( X .LE. - 74.0 .AND. X .GT. - 75.0 .AND.
     &          Y .GE.   39.0 .AND. Y .LT.   41.3 ) THEN
         st = 'NJ'                                      ! NJ
      ELSE IF ( ( X .LE. -103.0 .AND. X .GT. -109.0 .AND.
     &            Y .GE.   32.0 .AND. Y .LT.   37.0 ).OR.
     &          ( X .LE. -108.1 .AND. X .GT. -109.0 .AND.
     &            Y .GE.   31.2 .AND. Y .LT.   32.0 ) ) THEN
         st = 'NM'                                      ! NM
      ELSE IF ( X .LE. -114.0 .AND. X .GT. -120.0 .AND.
     &          Y .GE.   32.6 .AND. Y .LT.   42.0 ) THEN
         st = 'NV'                                      ! NV 
         IF ( X .LE. -114.0 .AND. X .GT. -114.7 .AND.
     &        Y .GE.   32.2 .AND. Y .LT.   36.0 ) THEN             
            st = 'AZ'                                   ! AZ west
         ELSE IF ( ( X .LE. -114.7 .AND. X .GT. -116.0   .AND.
     &               Y .GE.   32.6 .AND. Y .LT.   35.5 ) .OR.
     &             ( X .LE. -116.0 .AND. X .GT. -117.0   .AND.
     &               Y .GE.   32.6 .AND. Y .LT.   36.5 ) .OR.
     &             ( X .LE. -117.0 .AND. X .GT. -118.0   .AND.
     &               Y .GE.   32.6 .AND. Y .LT.   37.0 ) .OR.
     &             ( X .LE. -118.0 .AND. X .GT. -119.0   .AND.
     &               Y .GE.   32.6 .AND. Y .LT.   38.0 ) .OR.
     &             ( X .LE. -119.0 .AND. X .GT. -120.0   .AND.
     &               Y .GE.   32.6 .AND. Y .LT.   38.6 ) ) THEN
            st = 'CA'                                   ! CA 
         END IF
      ELSE IF ( ( X .LE. - 76.0 .AND. X .GT. - 79.8   .AND.
     &            Y .GE.   42.0 .AND. Y .LE.   43.3 ) .OR.
     &          ( X .LE. - 75.0 .AND. X .GT. - 76.0   .AND.
     &            Y .GE.   42.0 .AND. Y .LT.   45.0 ) .OR.
     &          ( X .LE. - 73.2 .AND. X .GT. - 75.0   .AND.
     &            Y .GE.   41.2 .AND. Y .LT.   45.0 ) ) THEN
         st = 'NY'                                      ! NY
      ELSE IF ( ( X .LE. - 80.6 .AND. X .GT. - 84.9   .AND.
     &            Y .GE.   39.8 .AND. Y .LT.   41.8 ) .OR.
     &          ( X .LE. - 82.0 .AND. X .GT. - 84.9   .AND.
     &            Y .GE.   38.8 .AND. Y .LT.   39.8 ) ) THEN
         st = 'OH'                                      ! OH
      ELSE IF ( ( X .LE. - 94.5 .AND. X .GT. -100.0   .AND.
     &            Y .GE.   34.0 .AND. Y .LT.   37.0 ) .OR.
     &          ( X .LE. -100.0 .AND. X .GT. -103.0   .AND.
     &            Y .GE.   36.5 .AND. Y .LT.   37.0 ) ) THEN
         st = 'OK'                                      ! OK
      ELSE IF ( X .LE. -117.0 .AND. X .GT. -125.0 .AND.
     &          Y .GE.   42.0 .AND. Y .LT.   46.0 ) THEN
         st = 'OR'                                      ! OR
      ELSE IF ( X .LE. - 75.0 .AND. X .GT. - 80.6 .AND.
     &          Y .GE.   39.8 .AND. Y .LT.   42.0 ) THEN
         st = 'PA'                                      ! PA
      ELSE IF ( X .LE. - 71.2 .AND. X .GT. - 71.9 .AND.
     &          Y .GE.   41.0 .AND. Y .LT.   42.0 ) THEN
         st = 'RI'                                      ! RI
      ELSE IF ( X .LE. - 96.6 .AND. X .GT. -104.0 .AND.
     &          Y .GE.   43.0 .AND. Y .LT.   46.0 ) THEN
         st = 'SD'                                      ! SD
      ELSE IF ( X .LE. - 82.0 .AND. X .GT. - 90.0 .AND.
     &          Y .GE.   35.0 .AND. Y .LT.   36.7 ) THEN
         st = 'TN'                                      ! TN
         IF ( ( X .LE. - 83.0 .AND. X .GT. - 84.0   .AND.
     &          Y .GE.   35.0 .AND. Y .LT.   35.5 ) .OR.
     &        ( X .LE. - 82.0 .AND. X .GT. - 83.0   .AND.
     &          Y .GE.   35.0 .AND. Y .LT.   36.0 ) ) st = 'NC'   ! NC west
      ELSE IF ( ( X .LE. -105.0 .AND. X .GT. -106.5   .AND.
     &            Y .GE.   31.0 .AND. Y .LT.   32.0 ) .OR.
     &          ( X .LE. -103.0 .AND. X .GT. -105.0   .AND.
     &            Y .GE.   29.0 .AND. Y .LT.   32.0 ) .OR.
     &          ( X .LE. -100.0 .AND. X .GT. -103.0   .AND.
     &            Y .GE.   29.8 .AND. Y .LT.   36.5 ) .OR.
     &          ( X .LE. -100.0 .AND. X .GT. -101.0   .AND.
     &            Y .GE.   29.0 .AND. Y .LT.   29.8 ) .OR.
     &          ( X .LE. - 94.0 .AND. X .GT. -100.0   .AND.
     &            Y .GE.   28.0 .AND. Y .LT.   34.0 ) .OR.
     &          ( X .LE. - 93.8 .AND. X .GT. - 94.0   .AND.
     &            Y .GE.   29.6 .AND. Y .LT.   33.0 ) .OR.
     &          ( X .LE. - 97.0 .AND. X .GT. - 99.5   .AND.
     &            Y .GE.   26.0 .AND. Y .LE.   28.0 ) ) THEN
         st = 'TX'                                      ! TX
      ELSE IF ( ( X .LE. -109.0 .AND. X .GT. -114.0   .AND.
     &            Y .GE.   37.0 .AND. Y .LT.   41.0 ) .OR.
     &          ( X .LE. -111.0 .AND. X .GT. -114.0   .AND.
     &            Y .GE.   41.0 .AND. Y .LT.   42.0 ) ) THEN
         st = 'UT'                                      ! UT
      ELSE IF ( ( X .LE. - 80.0 .AND. X .GT. - 82.5   .AND.
     &            Y .GE.   36.5 .AND. Y .LT.   37.2 ) .OR.
     &          ( X .LE. - 78.5 .AND. X .GT. - 80.0   .AND.
     &            Y .GE.   36.5 .AND. Y .LT.   38.5 ) .OR.
     &          ( X .LE. - 77.2 .AND. X .GT. - 78.5   .AND.
     &            Y .GE.   36.5 .AND. Y .LT.   39.8 ) .OR.
     &          ( X .LE. - 75.0 .AND. X .GT. - 77.2   .AND.
     &            Y .GE.   36.5 .AND. Y .LT.   38.0 ) ) THEN
         st = 'VA'                                      ! VA
      ELSE IF ( ( X .LE. - 72.4 .AND. X .GT. - 73.2   .AND.
     &            Y .GE.   42.8 .AND. Y .LT.   45.0 ) .OR.
     &          ( X .LE. - 71.7 .AND. X .GT. - 72.4   .AND.
     &            Y .GE.   44.0 .AND. Y .LT.   45.0 ) ) THEN
         st = 'VM'                                      ! VM
      ELSE IF ( X .LE. -117.0 .AND. X .GT. -125.0 .AND.
     &          Y .GE.   46.0 .AND. Y .LE.   49.0 ) THEN
         st = 'WA'                                      ! WA
      ELSE IF ( ( X .LE. - 87.7 .AND. X .GT. - 91.0   .AND.
     &            Y .GE.   42.5 .AND. Y .LE.   46.2 ) .OR.
     &          ( X .LE. - 91.0 .AND. X .GT. - 92.2   .AND.
     &            Y .GE.   44.0 .AND. Y .LE.   46.9 ) .OR.
     &          ( X .LE. - 92.2 .AND. X .GT. - 92.8   .AND.
     &            Y .GE.   44.4 .AND. Y .LT.   46.0 ) .OR.
     &          ( X .LE. - 90.0 .AND. X .GT. - 91.0   .AND.
     &            Y .GE.   46.2 .AND. Y .LT.   46.6 ) ) THEN
         st = 'WI'                                      ! WI
      ELSE IF ( ( X .LE. - 82.0 .AND. X .GT. - 82.6   .AND.
     &            Y .GE.   37.2 .AND. Y .LE.   38.8 ) .OR.
     &          ( X .LE. - 80.0 .AND. X .GT. - 82.0   .AND.
     &            Y .GE.   37.2 .AND. Y .LT.   39.8 ) .OR.
     &          ( X .LE. - 78.5 .AND. X .GT. - 80.0   .AND.
     &            Y .GE.   38.5 .AND. Y .LT.   39.8 ) ) THEN
         st = 'WV'                                      ! WV
         IF ( X .LE. - 82.0 .AND. X .GT. - 82.6 .AND.
     &        Y .GE.   37.2 .AND. Y .LT.   37.6 ) st = 'KY'   ! KY
         IF ( X .LE. - 81.7 .AND. X .GT. - 82.0 .AND.
     &        Y .GE.   39.2 .AND. Y .LT.   39.8 ) st = 'OH'   ! OH
      ELSE IF ( X .LE. -104.0 .AND. X .GT. -111.0 .AND.
     &          Y .GE.   41.0 .AND. Y .LT.   45.0 ) THEN
         st = 'WY'                                      ! WY
      END IF

      state = st

      end function state
