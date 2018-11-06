
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
C $Header: /project/yoj/arc/BCON/src/profile/prof_vinterp.F,v 1.2 2011/10/21 16:52:36 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE PROF_VINTERP( LOGUNIT, NSPCS_IN, NLAYS_IN, ZH_IN,
     &                         CONCIN, CONCOUT )    

C***********************************************************************
 
C Function: Interpolates/Extrapolates concentrations in vertical.
C           The number of layers in CONCIN is collapsed or expanded
C           according to the number of layers in COORD.EXT.
C           Interpolation is done using rational function interpolation
C           ( Numerical Recipes, Press et al.) or linear 
C           interpolation.  When extapolation is required, the 
C           concentration of the outside layer is used. If the input 
C           file has only one layer, the concentrations in that layer
C           are used for all output layers.
              
C Preconditions: None
  
C Key Subroutines/Functions Called: LR_INTERP  
C
C Revision History:
C    Prototype created by Jerry Gipson, January, 1998          
C    02/25/02 Steve Howard (Jeff Young) - dynamic allocation
C    01/05/05 J.Young: vert dyn alloc - Use VGRD_DEFN
C    13 Jul 11 J.Young: Replaced I/O API include files with M3UTILIO
C    23 May 12 J.Young: Replaced BC_PARMS include file with an F90 module
C    21 Sep 19 S.Roselle: Reconfigured to do vertical interpolation to
C                       target boundary vertical layer heights (ZH)
                    
C***********************************************************************

      USE HGRD_DEFN   ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN   ! vertical layer specifications
      USE M3UTILIO    ! IOAPI module
      USE BC_PARMS    ! BCON parameters

      IMPLICIT NONE     

C Include Files:

C Arguments: 
      INTEGER LOGUNIT                   ! Unit number for output log
      INTEGER NSPCS_IN                  ! No. of species in input conc file
      INTEGER NLAYS_IN                  ! No. of layers in input conc file

      REAL :: ZH_IN( * )                ! Input layer heights
      REAL :: CONCIN( MXLAYS3,* )       ! Input conc array
      REAL :: CONCOUT( NBNDY,NLAYS,* )  ! Output BC array

C Parameters: None

C External Functions: None

C Local Variables:
      CHARACTER( 80 ) :: MSG      ! Log message
      CHARACTER( 16 ) :: PNAME = 'PROF_VINTERP'    ! Procedure Name
      CHARACTER( 16 ) :: VNAME    ! Variable Name

      INTEGER L              ! Loop index for vertical layers
      INTEGER MXLEV          ! Max no. of levels
      INTEGER N              ! Loop index
      INTEGER C              ! Loop index for boundary cells
      INTEGER STATUS         ! Status code
      INTEGER JDATE          ! Date for first record on BDY file
      INTEGER JTIME          ! Time for first record on BDY file

      LOGICAL L_RATINT       ! Flag to use rational function interpolation 

      REAL    DELY  ! Error estimate for conc interpolated by rational func
      REAL    X3    ! Vertical coordinate used in interpolation
      REAL    Y     ! Interpolated concentration

      REAL    :: WORKA( MXLAYS3 )      ! Work array for conc input
      REAL    :: X3_OLD( MXLAYS3 )     ! Old Vertical coordinate values
      REAL    :: ZH_OUT( NBNDY,NLAYS ) ! mid-layer heights for target BDY file

C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  open MET_BDY_3D_FIN file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. OPEN3 ( MET_BDY_3D_FIN, FSREAD3, PNAME ) ) THEN
         MSG = 'Could not open ' // MET_BDY_3D_FIN // ' file'
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF
      JDATE = SDATE3D
      JTIME = STIME3D

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Determine type of interpolation to use: linear or rational function
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      L_RATINT = .FALSE.
      MSG = 'Flag for interpolation by rational function'
!     L_RATINT = ENVYN( 'RATIONAL_FUNC', MSG, L_RATINT, STATUS )  
      IF ( .NOT. L_RATINT ) THEN
         MSG = 'Linear vertical interpolation used'
      ELSE
         MSG = 'Vertical interpolation by rational function'
      END IF
      CALL M3MESG( MSG )
         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Interpolate by ZH
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C Get the layer mid-point heights
      VNAME = 'ZH'
      IF ( .NOT. READ3( MET_BDY_3D_FIN, VNAME, ALLAYS3, JDATE, JTIME,
     &                  ZH_OUT ) ) THEN
         MSG = 'Could not read layer heights form file  ' // MET_BDY_3D_FIN 
         CALL M3ERR( PNAME, JDATE, JTIME, MSG, .TRUE. )
      END IF

      DO L = 1, NLAYS_IN 
         X3_OLD( L ) = ZH_IN( L )
      END DO

      DO C = 1, NBNDY
         DO N = 1, NSPCS_IN    

            DO L = 1, NLAYS_IN
               WORKA( L ) = CONCIN( L,N )
            END DO

            DO L = 1, NLAYS

               IF ( NLAYS_IN .EQ. 1 ) THEN
                  CONCOUT( C,L,N ) = WORKA( 1 )
               ELSE
                  X3 = ZH_OUT( C,L )
                  IF ( X3 .LT. X3_OLD( 1 ) ) THEN
                     CONCOUT( C,L,N ) = WORKA( 1 )
                  ELSE IF ( X3 .GT. X3_OLD( NLAYS_IN ) ) THEN
                     CONCOUT( C,L,N ) = WORKA( NLAYS_IN )
                  ELSE
                     CALL LR_INTERP( L_RATINT, X3_OLD, WORKA, NLAYS_IN,
     &                               X3, Y, DELY )
                     CONCOUT( C,L,N ) = Y
                  END IF
               END IF

            END DO 

         END DO
      END DO

      RETURN

      END
