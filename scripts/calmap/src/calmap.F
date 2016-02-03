C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      PROGRAM CALMAP

C-----------------------------------------------------------------------
C Description:
C   read crop calendar file 
C   then create time map of planting and harvesting
C   write ioapi file
 
C Preconditions
C
C-----------------------------------------------------------------------
      USE M3UTILIO
      IMPLICIT NONE

C Parameters:

      INTEGER, PARAMETER :: NLAYS = 1

C External fuctions:

      CHARACTER(  2 ), EXTERNAL :: STATE

C Local Variables:

      INTEGER      SDATE        ! start date , coded YYYYDDD
      INTEGER      STIME        ! start time , coded HHMMSS
      INTEGER      MY_NCOLS
      INTEGER      MY_NROWS
        
      CHARACTER( 16 ) :: GRID_CRO_2D = 'GRID_CRO_2D'   ! for LON, LAT
      CHARACTER( 16 ) :: BELD01 = 'BELD01'
      CHARACTER( 16 ) :: FNAME( 8 ) = ( /
     &                              'CROPMAP01','CROPMAP02',
     &                              'CROPMAP03','CROPMAP04',
     &                              'CROPMAP05','CROPMAP06',
     &                              'CROPMAP07','CROPMAP08'/ )

c    &         ( / 'CROPMAP01',        ! begin planting
c    &             'CROPMAP04',        ! end planting
c    &             'CROPMAP08' / )     ! end harvesting

      CHARACTER( 16 ) :: PNAME = 'CALMAP'
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 120 ) :: MESG = ' '

      INTEGER C, R, I, J, S, M
        
      INTEGER :: DPM( 12 ) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
        
      REAL, ALLOCATABLE :: LON   ( :,: )   ! longitude
      REAL, ALLOCATABLE :: LAT   ( :,: )   ! latitude
      REAL, ALLOCATABLE :: NOLAND( :,: )   ! non-land
      REAL X, Y
      
      INTEGER NSTT, TT
      INTEGER, PARAMETER :: NSPEC = 18
      
      INTEGER :: T( 16 ), SPST( NSPEC )
      REAL, ALLOCATABLE    :: DATEALL( :,:,:,: )
      INTEGER :: DAYS( 50,8 ), CALD( 50,NSPEC,8 )
      
      CHARACTER(  2 ) :: NMST( 50 ), LTST( 50 )
      CHARACTER(2), ALLOCATABLE :: ST( :,: )
      CHARACTER( 16 ) :: CROPSP( NSPEC )

      character( 16 ) :: vn( nspec )
      character( 16 ) :: fn( 8 )

C-----------------------------------------------------------------------

      fn = FNAME   ! array assignment

      SDATE = 0000000
      STIME = 000000
           
C-------------------crop calendar file-------------------
C Read state names
      OPEN( UNIT=11, FILE='USSTATE.txt', STATUS='OLD' )
      DO I = 1, 50
         READ( 11,* ) NMST( I )
         IF ( NMST( I ) .EQ. 'WY' ) THEN   ! last in the list
            NSTT = I
            EXIT
         END IF
      END DO
      CLOSE( UNIT=11 )
      write( *,* ) '-------- Finished reading US state list --------'

C Read in crop calendar
      OPEN( UNIT=12, FILE='CPCALED.txt', STATUS='OLD' )
      DO S = 1, NSPEC   ! Crop Species
         READ( 12,* ) CROPSP( S )
         write( *,* ) S, CROPSP( S )
            DO I = 1, 50   ! State in Crop Calendar
               READ( 12,* ) LTST( I ), ( T( J ), J = 1, 16 )   ! activity
               DO M = 1, 8   ! Activity
                  DAYS( I,M ) = 0
                  TT = 0
                  IF ( T( 1 ) .NE. 0 ) THEN
                     DO J = 1, T( 2*M-1 )-1   ! add up the days in the prior months
                        TT = TT + DPM( J )
                     END DO
                     DAYS( I,M ) = TT + T( 2*M )   ! tack on the day of the last month
                     IF ( DAYS( I,M ) .GT. 365 ) THEN
                        MESG = '*** Days of year > 365 ***'
                        CALL M3EXIT( PNAME, SDATE, STIME, MESG, XSTAT1 )
                     END IF
                  END IF
               END DO   ! Activity
               write( *,2011 ) LTST( I ), ( DAYS( I,M ), M = 1, 8 )
2011  format( 1x, a, 1x, 8i5 )
               IF ( LTST( I ) .EQ. 'WY' ) THEN   ! end of states list
                  SPST( S ) = I
                  EXIT
               END IF
            END DO   ! States 

         DO I = 1, SPST( S )   ! I state in crop calendar
            DO J = 1, NSTT     ! J state whole list
               IF ( LTST( I ) .EQ. NMST( J ) ) THEN
                  DO M = 1, 8
                     CALD( J,S,M ) = DAYS( I,M )
                  END DO
                  EXIT
               END IF
            END DO
         END DO
  
      END DO   ! Crop Species
      CLOSE( UNIT=12 )     
      write( *,* ) '-------- Finished reading crop calendar --------'

C----------------------grid and landuse file------------------------
C Open input file and get description and read in input data ---

      IF ( .NOT. OPEN3( GRID_CRO_2D, FSREAD3, PNAME ) ) THEN
         MESG = 'Could not open '// GRID_CRO_2D
         CALL M3EXIT( PNAME, SDATE, STIME, MESG, XSTAT1 )
      END IF

C Get file header information for output files

      IF ( .NOT. DESC3( GRID_CRO_2D ) ) THEN
         MESG = 'Could not get file description'
         CALL M3EXIT( PNAME,  SDATE, STIME, MESG, XSTAT1 )
      END IF

      print*,NCOLS3D,NROWS3D
      ALLOCATE( LON(NCOLS3D,NROWS3D) )
      ALLOCATE( LAT(NCOLS3D,NROWS3D) )
      ALLOCATE( NOLAND(NCOLS3D,NROWS3D) )
      
      VNAME = 'LAT'
      MESG = 'Could not read LAT from ' // GRID_CRO_2D
      IF ( .NOT. INTERP3 ( GRID_CRO_2D, VNAME, PNAME,
     &                     SDATE, STIME, NCOLS3D*NROWS3D, LAT ) )
     &   CALL M3EXIT ( PNAME, SDATE, STIME, MESG, XSTAT1 )
         
      VNAME = 'LON'
      MESG = 'Could not read LON from ' // GRID_CRO_2D
      IF ( .NOT. INTERP3 ( GRID_CRO_2D, VNAME, PNAME,
     &                     SDATE, STIME, NCOLS3D*NROWS3D, LON ) )
     &   CALL M3EXIT ( PNAME, SDATE, STIME, MESG, XSTAT1 )
     
      IF ( .NOT. OPEN3( BELD01, FSREAD3, PNAME ) ) THEN
         MESG = 'Could not open '// BELD01
         CALL M3EXIT( PNAME, SDATE, STIME, MESG, XSTAT1 )
      END IF
      
      VNAME = 'USGS_water'
      MESG = 'Could not read USGS WATER from ' // BELD01
      IF ( .NOT. INTERP3 ( BELD01, VNAME, PNAME,
     &                     SDATE, STIME, NCOLS3D*NROWS3D, NOLAND ) )
     &   CALL M3EXIT ( PNAME, SDATE, STIME, MESG, XSTAT1 )
     
      write( *,* ) '-------- Finished reading grid & landuse --------'
     
      ALLOCATE( ST(NCOLS3D,NROWS3D)  )
      ALLOCATE( DATEALL(NCOLS3D,NROWS3D,NSPEC+1,8) )
      LON = 0.0
      LAT = 0.0
      ST = ' '
      DATEALL = 0
C Locate cell to state
      DO R = 1, NROWS3D
      DO C = 1, NCOLS3D
      
      X = LON( C,R ); Y = LAT( C,R )
      ST( C,R ) = STATE( X, Y )
      
      DO J = 1, NSTT   ! state list
         IF ( ST( C,R ) .EQ. NMST( J ) .AND. NOLAND( C,R ) .LT. 80.0 ) THEN
            DO M = 1, 8       ! 8 activity dates
               DO S = 1, NSPEC   ! crop species
                  DATEALL( C,R,S,M ) = FLOAT( CALD( J,S,M ) )
                  IF ( DATEALL( C,R,S,M ) .LT. 1.0 ) DATEALL( C,R,S,M ) = 0.0
                  IF ( DATEALL( C,R,S,M ) .GT. 365.0 ) THEN
                     MESG = '*** day more than 365 ***'
                     CALL M3EXIT ( PNAME, SDATE, STIME, MESG, XSTAT1 )
                  END IF
               END DO   ! S
            END DO   ! M
            EXIT
         ELSE
            DO M = 1, 8
               DO S = 1, NSPEC+1
                  DATEALL( C,R,S,M ) = 0.0
               END DO
            END DO
         END IF
      END DO   ! J

      END DO   ! C
      END DO   ! R

      write( *,* ) '-------- Finished mapping --------'

C-----------------------create ioapi map--------------------------------
C Complete header for CALENDARMAP file
 
      FTYPE3D = GRDDED3
      NVARS3D = NSPEC + 1
      NLAYS3D = 1
c      NCOLS3D = NCOLS
c      NROWS3D = NROWS

      SDATE3D = SDATE
      STIME3D = STIME
      TSTEP3D = 000000

      OPEN( UNIT=16, FILE='NCROP.txt' )    ! crop species list
      WRITE( 16,* ) ' Number of crop species:', NVARS3D - 1

      DO I = 1, NVARS3D - 1
         VTYPE3D( I ) = M3REAL
         UNITS3D( I ) = 'Days'    ! Days in year
         VNAME3D( I ) = ' '
         J = INDEX( CROPSP( I ), '-')
         S = INDEX( CROPSP( I ), ' ')
         IF ( J .GT. 1 ) THEN
            VNAME3D( I )( 1:J-1 ) = CROPSP( I )( 1:J-1 )
            VNAME3D( I )( J:S-1 ) = CROPSP( I )( J+1:S )
         ELSE
            VNAME3D( I ) = CROPSP( I )
         END IF
         vn( i ) = vname3d( i )
         VDESC3D( I ) = CROPSP( I )
         WRITE( 16,* ) VNAME3D( I )
      END DO

      VTYPE3D( NVARS3D ) = M3REAL
      UNITS3D( NVARS3D ) = ' '
      VNAME3D( NVARS3D ) = 'Nothing'
      VDESC3D( NVARS3D ) = ' '
            
C Write header and files

      DO J = 1, 8   ! 8 activity dates

         IF ( .NOT. OPEN3( FNAME( J ), FSUNKN3, PNAME ) ) THEN
            MESG = 'Could not open ' // FNAME( J )
            CALL M3EXIT( PNAME, SDATE, STIME, MESG, XSTAT1 )
         END IF

         DO I = 1, NVARS3D   ! crop species
            IF ( .NOT. WRITE3( FNAME( J ), VNAME3D( I ),
     &                         SDATE, STIME, DATEALL( 1,1,I,J ) ) ) THEN
               MESG = 'Could not write ' // TRIM( VNAME3D( I ) )
     &              // ' to ' // FNAME( J )
               CALL M3EXIT( PNAME, SDATE, STIME, MESG, XSTAT1 )
            END IF
         END DO

      END DO   ! J

      write( *,* ) '-------- ', TRIM( PNAME ), ' Completed --------'

      STOP
      END

