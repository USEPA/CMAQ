
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/BCON/src/m3conc/m3_vinterp.F,v 1.2 2011/10/21 16:52:35 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE M3_VINTERP ( LOGUNIT, JDATE, JTIME,
     &                        NCOLS_IN, NROWS_IN, NLAYS_IN, NSPCS_IN,
     &                        COL_LOC, ROW_LOC,
     &                        BCIN, BCVI, CTM_FL_NAME )

C*************************************************************************
 
C Function: Interpolates/Extrapolates concentrations in vertical.
C           The number of layers in CONCIN is collapsed or expanded
C           according to the number of layers in COORD.EXT.
C           Interpolation is done using rational function interpolation
C           ( Numerical Recipes, Press et al.) or linear 
C           interpolation.  When extapolation is required, the 
C           concentration of the outside layer is used. If the input 
C           file has only one layer, the concentrtaions in that layer
C           are used for all output layers.
              
C Preconditions: None
  
C Key Subroutines/Functions Called: LR_INTERP  
 
C Revision History:
C    Prototype created by Jerry Gipson, January, 1998
C    Modified by JG 4/26/99 to change variables SDATE and STIME to JDATE and
C                           JTIME for consistency
C    Modified by JG 5/26/99 to treat PinG plumes 
C    02/25/02 Steve Howard (Jeff Young) - dynamic allocation
C    01/05/05 J.Young: vert dyn alloc - Use VGRD_DEFN
C                      eliminate malloc calls
C    13 Jul 11 J.Young: Replaced I/O API include files with M3UTILIO
C    23 May 12 J.Young: Replaced BC_PARMS include file with an F90 module
C    10 June 19 F. Sidi: Commented out INTEGER STATUS unused variable
C    20 April 21 C. Hogrefe: Force height or pressure interpolation if either 
C                            grid uses hybrid vertical coordinates 
                    
C*************************************************************************

      USE HGRD_DEFN   ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN   ! vertical layer specifications
      USE M3UTILIO    ! IOAPI module
      USE BC_PARMS    ! BCON parameters

      IMPLICIT NONE     

C Arguments:
      INTEGER, INTENT( IN ) :: LOGUNIT      ! Unit number for output log
      INTEGER, INTENT( IN ) :: JDATE        ! Date for IC Output
      INTEGER, INTENT( IN ) :: JTIME        ! Time for IC output
      INTEGER, INTENT( IN ) :: NCOLS_IN     ! No. of columns in input conc file
      INTEGER, INTENT( IN ) :: NROWS_IN     ! No. of rows in input conc file
      INTEGER, INTENT( IN ) :: NLAYS_IN     ! No. of layers in input conc file
      INTEGER, INTENT( IN ) :: NSPCS_IN     ! No. of species in input conc file
      INTEGER, INTENT( IN ) :: COL_LOC( : ) ! Output IC col corresponding to
                                            ! a cell in the input CTM file
      INTEGER, INTENT( IN ) :: ROW_LOC( : ) ! Output IC row corresponding to
                                            ! a cell in the input CTM file
      REAL, INTENT( IN )    :: BCIN( :,:,: ) ! Input conc array
      REAL, INTENT( OUT )   :: BCVI( :,:,: ) ! Output IC array
      CHARACTER( 16 ), INTENT( IN ) :: CTM_FL_NAME( : ) ! CTM_CONC file name(s)

C Parameters: None

C External Functions: None

C Local Variables:
      LOGICAL, SAVE :: LFIRST = .TRUE.  ! Flag for first call
      LOGICAL, SAVE :: LDEC             ! Flag for monotonic decreasing layer levels
      LOGICAL, SAVE :: LINC             ! Flag for monotonic increasing layer levels
      LOGICAL, SAVE :: L_IDENTICAL      ! Flag for identical vert coord systems 
      LOGICAL, SAVE :: L_RATINT         ! Flag to use rational function interpolation
      LOGICAL, SAVE :: L_SAME_SCALE     ! Flag for same vert coord systems but
                                        ! different resolutions  
      CHARACTER( 20 ) :: CHR1       ! Value of variable 1 in character data
      CHARACTER( 20 ) :: CHR2       ! Value of variable 1 in character data
      CHARACTER( 80 ) :: MSG        ! Log message
      CHARACTER( 16 ) :: PNAME = 'M3_VINTERP'  ! Procedure Name
      CHARACTER( 16 ), SAVE :: ZP_VNAME      ! ZH or PRES Variable Name

      INTEGER C              ! Loop indices for columns
      INTEGER L              ! Loop index for vertical layers
      INTEGER MXLEV          ! Largest no. of levels
      INTEGER N              ! Loop index for boundary cells
      INTEGER R              ! Loop indices for rows
!      INTEGER STATUS         ! Status code
      INTEGER V              ! Loop index for variables
      INTEGER ALLOCSTAT      ! Status returned from array allocation

      REAL    DELY  ! Error estimate for conc interpolated by rational func
      REAL    X3    ! Vertical coordinate used in interpolation
      REAL    Y     ! Interpolated concentration

      REAL, ALLOCATABLE, SAVE :: WORKA( : )      ! Work array for conc input
      REAL, ALLOCATABLE, SAVE :: X3_OLD( : )     ! Old Vertical coordinate values
      REAL, ALLOCATABLE, SAVE :: HT_BNDY( :,: )  ! New mid-layer heights
      REAL, ALLOCATABLE, SAVE :: HT_CTM( :,:,: ) ! Old mid-layer heights
     
      INTERFACE

         SUBROUTINE LR_INTERP ( L_RATINT, XA, YA, N, X, Y, DELY )
            LOGICAL, INTENT( IN ) :: L_RATINT
            REAL, INTENT( IN )  :: XA( : )
            REAL, INTENT( IN )  :: YA( : )
            REAL, INTENT( IN )  :: X
            REAL, INTENT( OUT ) :: Y
            REAL, INTENT( OUT ) :: DELY
            INTEGER, INTENT( IN ) :: N
         END SUBROUTINE LR_INTERP

      END INTERFACE

C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, write log info and set flags
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  allocate arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         ALLOCATE( WORKA( NLAYS_IN ), X3_OLD( NLAYS_IN ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            MSG = 'Failure allocating WORKA, X3_OLD'
            CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
         END IF

         ALLOCATE( HT_BNDY( NBNDY,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            MSG = 'Failure allocating HT_BNDY'
            CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
         END IF

         ALLOCATE( HT_CTM( NCOLS_IN,NROWS_IN,NLAYS_IN ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            MSG = 'Failure allocating HT_CTM'
            CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
         END IF

         IF ( .NOT. DESC3( CTM_FL_NAME( 1 ) ) ) THEN
            MSG = 'Could not read DESC of  ' // CTM_FL_NAME( 1 ) 
     &         // ' file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT2 )
         END IF

C Determine type of interpolation to use: linear or rational function
         WRITE( LOGUNIT, 92000 )

         L_RATINT = .FALSE.
         MSG = 'Flag for interpolation by rational function'
!        L_RATINT = ENVYN( 'RATIONAL_FUNC', MSG, L_RATINT, STATUS )  
         IF ( .NOT. L_RATINT ) THEN
            MSG = 'Vertical interpolation method: Linear'
         ELSE
            MSG = 'Vertical interpolation method: Rational Function.'
         END IF

C Check if vertical grids are the same or different
         L_IDENTICAL  = .TRUE.
         L_SAME_SCALE = .TRUE.

C The following two lines are for testing only
!        L_IDENTICAL  = .FALSE.
!        L_SAME_SCALE = .FALSE.

         IF ( VGTYP_GD .NE. VGTYP3D ) THEN 
            L_IDENTICAL  = .FALSE.
            L_SAME_SCALE = .FALSE.
         END IF

         IF ( VGTOP_GD .NE. VGTOP3D ) THEN 
            L_IDENTICAL  = .FALSE.
            L_SAME_SCALE = .FALSE.
         END IF   

         IF ( NLAYS .EQ. NLAYS_IN ) THEN
            DO L = 1, NLAYS + 1
               WRITE( CHR1, 94000 ) VGLVS_GD( L )
               WRITE( CHR2, 94000 ) VGLVS3D(  L )
               IF ( CHR1 .NE. CHR2 ) L_IDENTICAL  = .FALSE.
            END DO
         ELSE
            L_IDENTICAL  = .FALSE. 
         END IF
	 
C If either grid uses hybrid vertical coordinates, force height
C or pressure vertical interpolation

         IF ( ( VGTYP_GD . EQ. -9999 ) .OR. ( VGTYP3D . EQ. -9999 ) ) THEN
            L_IDENTICAL  = .FALSE.
            L_SAME_SCALE = .FALSE.
         ENDIF
            
C For same grids, simply report
         IF ( L_IDENTICAL ) THEN

            WRITE( LOGUNIT, 92020 ) 

C For same type but different resolution, list on output log
         ELSE IF ( L_SAME_SCALE ) THEN

            WRITE( LOGUNIT, 92040 )
            WRITE( LOGUNIT, 92060 ) VGDESC( VGTYP_GD )
            WRITE( LOGUNIT, 92080 )
  
            MXLEV = MAX( NLAYS + 1, NLAYS_IN + 1 )

            DO L = 1, MXLEV 
               IF ( L .LE. NLAYS + 1 .AND. L .LE. NLAYS_IN + 1 ) THEN
                  WRITE( LOGUNIT, 92100 ) L, VGLVS_GD( L ), VGLVS3D( L )
               ELSE IF ( L .LE. NLAYS + 1 .AND. L .GT. NLAYS_IN + 1 ) THEN
                  WRITE( LOGUNIT, 92100 ) L, VGLVS_GD( L )
               ELSE IF ( L .GT. NLAYS + 1 .AND. L .LE. NLAYS_IN + 1 ) THEN
                  WRITE( LOGUNIT, 92120 ) L, VGLVS3D( L )
               END IF
            END DO    

            WRITE( LOGUNIT, 92140 ) MSG

            DO L = 1, NLAYS3D 
               X3_OLD( L ) = 0.5 * ( VGLVS3D ( L ) +  VGLVS3D ( L + 1 ) )
            END DO

            LINC = .FALSE.
            LDEC = .FALSE.
            IF ( VGLVS3D ( NLAYS_IN ) .GT. VGLVS3D ( 1 ) ) THEN
               LINC = .TRUE.
            ELSE
               LDEC = .TRUE.
            END IF
 
C For different types, check for files
         ELSE

            IF ( .NOT. DESC3( MET_CRO_3D_CRS ) ) THEN
               MSG = 'Could not read DESC of  ' // MET_CRO_3D_CRS 
     &            // ' file'
               CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT2 )
            END IF

            ZP_VNAME = 'ZH'
            V = INDEX1( ZP_VNAME, NVARS3D, VNAME3D )
            IF ( V .NE. 0 ) THEN
              WRITE( LOGUNIT, 92160 ) ZP_VNAME
            ELSE
               ZP_VNAME = 'PRES'
               V = INDEX1( ZP_VNAME, NVARS3D, VNAME3D )
               IF ( V .NE. 0 ) THEN
                  WRITE( LOGUNIT, 92160 ) ZP_VNAME
               ELSE
                  MSG = 'Could not find ZH or PRES in file ' // MET_CRO_3D_CRS 
                  CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
               END IF
            END IF

         END IF

         LFIRST = .FALSE.

         RETURN

      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  For identical vertical coordinates, copy the CTM concs to the output
C  IC array and return
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( L_IDENTICAL ) THEN

         DO N =1, NBNDY
            DO L = 1, NLAYS
               DO V = 1, NSPCS_IN
                  BCVI( N,L,V ) = BCIN( N,L,V )
               END DO
            END DO
         END DO

         RETURN

      END IF
         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Interpolate by VGLEVS for vertical coords of same type but different
c  resolution
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( L_SAME_SCALE ) THEN

         IF ( .NOT. DESC3( CTM_FL_NAME( 1 ) ) ) THEN
            MSG = 'Could not read DESC of  ' // CTM_FL_NAME( 1 ) 
     &         // ' file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT2 )
         END IF

         DO V = 1, NSPCS_IN    

            DO N = 1, NBNDY

               DO L = 1, NLAYS_IN
                  WORKA( L ) = BCIN( N,L,V )
               END DO

               DO L = 1, NLAYS

                  IF ( NLAYS_IN .EQ. 1 ) THEN
                     BCVI( N,L,V ) = WORKA( 1 )
                  ELSE
                     X3 = 0.5 * ( VGLVS_GD ( L ) +  VGLVS_GD ( L + 1 ) )
                     IF ( LINC .AND. X3 .LE. X3_OLD( 1 ) ) THEN
                        BCVI( N,L,V ) = WORKA( 1 )
                     ELSE IF ( LDEC .AND. X3 .GE. X3_OLD( 1 ) ) THEN
                        BCVI( N,L,V ) = WORKA( 1 )
                     ELSE IF ( LINC .AND. X3 .GE. X3_OLD( NLAYS_IN ) ) THEN
                        BCVI( N,L,V ) = WORKA( NLAYS_IN )
                     ELSE IF ( LDEC .AND. X3 .LE. X3_OLD( NLAYS_IN ) ) THEN
                        BCVI( N,L,V ) = WORKA( NLAYS_IN )
                     ELSE
                        CALL LR_INTERP ( L_RATINT, X3_OLD, WORKA, NLAYS_IN,
     &                                   X3, Y, DELY )
                        BCVI( N,L,V ) = Y
                     END IF
                  END IF

               END DO 
 
            END DO

         END DO

         RETURN

      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Interpolate by height or pressure for all other vertical grid types; 
c   a dynamic array holding heights will need to be allocated
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C Get the layer mid-point heights or pressure
      IF ( .NOT. INTERP3( MET_CRO_3D_CRS, ZP_VNAME, PNAME, JDATE, JTIME,
     &                    NCOLS_IN*NROWS_IN*NLAYS_IN, HT_CTM ) ) THEN
         MSG = 'Could not read '// ZP_VNAME // ' from file ' // MET_CRO_3D_CRS 
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF

      IF ( .NOT. READ3( MET_BDY_3D_FIN, ZP_VNAME, ALLAYS3, JDATE, JTIME,
     &                  HT_BNDY ) ) THEN
         MSG = 'Could not read '// ZP_VNAME //' from file ' // MET_BDY_3D_FIN 
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF

C Do the interpolation

C...  for height interpolation
      IF ( ZP_VNAME .EQ. 'ZH' ) THEN

         DO N = 1, NBNDY
            C = COL_LOC( N )
            R = ROW_LOC( N )
         
            DO V = 1, NSPCS_IN    
         
               DO L = 1, NLAYS_IN
                  WORKA( L ) = BCIN( N,L,V )
                  X3_OLD( L ) = HT_CTM( C,R,L )
               END DO
         
               DO L = 1, NLAYS
         
                  IF ( NLAYS_IN .EQ. 1 ) THEN
                     BCVI( N,L,V ) = WORKA( 1 )
                  ELSE
                     X3 = HT_BNDY( N,L )
                     IF ( X3 .LT. X3_OLD( 1 ) ) THEN
                        BCVI( N,L,V ) = WORKA( 1 )
                     ELSE IF ( X3 .GT. X3_OLD( NLAYS_IN ) ) THEN
                        BCVI( N,L,V ) = WORKA( NLAYS_IN )
                     ELSE
                        CALL LR_INTERP ( L_RATINT, X3_OLD, WORKA, NLAYS_IN,
     &                                   X3, Y, DELY )
                        BCVI( N,L,V ) = Y
                     END IF
                  END IF
         
               END DO
         
            END DO
         
         END DO

C...  for pressure interpolation
      ELSE IF ( ZP_VNAME .EQ. 'PRES' ) THEN

         DO N = 1, NBNDY
            C = COL_LOC( N )
            R = ROW_LOC( N )
         
            DO V = 1, NSPCS_IN    
         
               DO L = 1, NLAYS_IN
                  WORKA( L ) = BCIN( N,L,V )
                  X3_OLD( L ) = HT_CTM( C,R,L )
               END DO
         
               DO L = 1, NLAYS
         
                  IF ( NLAYS_IN .EQ. 1 ) THEN
                     BCVI( N,L,V ) = WORKA( 1 )
                  ELSE
                     X3 = HT_BNDY( N,L )
                     IF ( X3 .GT. X3_OLD( 1 ) ) THEN
                        BCVI( N,L,V ) = WORKA( 1 )
                     ELSE IF ( X3 .LT. X3_OLD( NLAYS_IN ) ) THEN
                        BCVI( N,L,V ) = WORKA( NLAYS_IN )
                     ELSE
                        CALL LR_INTERP ( L_RATINT, X3_OLD, WORKA, NLAYS_IN,
     &                                   X3, Y, DELY )
                        BCVI( N,L,V ) = Y
                     END IF
                  END IF
         
               END DO
         
            END DO
         
         END DO

      END IF

      RETURN

C************************* FORMAT STATEMENTS ***************************

92000 FORMAT( // 1X, 79( '#' ) 
     &         / 1X, '#  Vertical Interpolation Section '
     &         / 1X, 79( '#' ) ) 

92020 FORMAT( // 5X, 'The vertical structure in COORD.EXT is',
     &               ' identical to that in the CTM input file. '
     &        // 5X, 'No vertical interpolation necessary' )

92040 FORMAT( // 5X, 'The COORD.EXT and CTM vertical grid types are',
     &               ' the same, but the resolution is different.' /
     &           5X, 'Vertical interpolation using',
     &               ' VGLVS (listed below). ' )

92060 FORMAT( // 5X, 'Vertical grid type: ', A )

92080 FORMAT( // 5X, 'Vertical layer surface values (VGLVS) : '
     &         /10X, ' K    COORD.EXT    Input CTM' )

92100 FORMAT(   10X, I2, 1X, F12.3, 1X, F12.3 )

92120 FORMAT(   10X, I2,       13X, 1X, F12.3 )

92140 FORMAT( //5X, A )

92160 FORMAT( //5X, 'The COORD.EXT and CTM vertical grid types are ',
     &               'different. '
     &         / 5X, 'Vertical interpolation using ', A16, 1X,
     &               'from the MET_CRO_3D files' )

94000 FORMAT( 1PE20.4 )

      END
