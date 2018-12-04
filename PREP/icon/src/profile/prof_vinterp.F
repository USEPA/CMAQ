
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
C $Header: /project/yoj/arc/ICON/src/profile/prof_vinterp.F,v 1.2 2011/10/21 16:41:57 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE PROF_VINTERP( LOGUNIT, NSPCS_IN, NLAYS_IN, ZH_IN,
     &                         CONCIN, CONCOUT )    

C*************************************************************************
 
C  Function: Interpolates/Extrapolates concentrations in vertical.
C            The number of layers in CONCIN is collapsed or expanded
C            according to the number of layers in COORD.EXT.
C            Interpolation is done using rational function interpolation
C            ( Numerical Recipes, Press et al.) or linear 
C            interpolation.  When extapolation is required, the 
C            concentration of the outside layer is used. If the input 
C            file has only one layer, the concentrations in that layer
C            are used for all output layers.
              
C  Preconditions: None
  
C  Key Subroutines/Functions Called: LR_INTERP  
 
C  Revision History: Prototype created by Jerry Gipson, January, 1998          
C    01/24/02 Steve Howard (Jeff Young) - dynamic allocation
C    12/13/04 J.Young: vert dyn alloc - Use VGRD_DEFN
C    06 Jun 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C    21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
C    02 Nov 19 S.Roselle: Reconfigured to do vertical interpolation to
C                       target vertical layer heights (ZH)
C    06 Nov 18 S.Roselle: Replaced UTILIO_DEFN with M3UTILIO
C    30 Nov 18 S.Roselle: To provide spatially uniform ICs, target domain 
C                       average mid-layer heights are used for the vertical
C                       interpolation
                  
C*************************************************************************

      USE HGRD_DEFN   ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN   ! vertical layer specifications
      USE M3UTILIO    ! IOAPI module
      USE IC_PARMS    ! ICON parameters

      IMPLICIT NONE     

C Include Files:

C Arguments: 
      INTEGER LOGUNIT                ! Unit number for output log
      INTEGER NSPCS_IN               ! No. of species in input profile
      INTEGER NLAYS_IN               ! No. of layers in input profile

      REAL :: ZH_IN( * )             ! Input layer heights
      REAL :: CONCIN( MXLAYS3,* )    ! Input conc array
      REAL :: CONCOUT( NCOLS,NROWS,NLAYS,* )     ! Output IC array

C Parameters: None

C External Functions: None

C Local Variables:
      CHARACTER( 80 ) :: MSG      ! Log message
      CHARACTER( 16 ) :: PNAME = 'PROF_VINTERP'   ! Procedure Name
      CHARACTER( 16 ) :: VNAME    ! Variable Name

      INTEGER L              ! Loop index for vertical layers
      INTEGER MXLEV          ! Max no. of levels
      INTEGER N              ! Loop index
      INTEGER C, R           ! Loop indices for col, row
      INTEGER STATUS         ! Status code
      INTEGER JDATE          ! Date for first record on CRO file
      INTEGER JTIME          ! Time for first record on CRO file

      LOGICAL L_RATINT       ! Flag to use rational function interpolation 

      REAL DELY  ! Error estimate for conc interpolated by rational func
      REAL X3    ! Vertical coordinate used in interpolation
      REAL Y     ! Interpolated concentration

      REAL :: WORKA(  MXLAYS3 )  ! Work array for conc input
      REAL :: X3_OLD( MXLAYS3 )  ! Old Vertical coordinate values
      REAL :: ZH_OUT( NCOLS,NROWS,NLAYS )    ! mid-layer heights for target file
      REAL :: ZH_OUT_AVG( NLAYS )! domain average mid-layer heights for target file
      
C**********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  open MET_CRO_3D_FIN file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. OPEN3 ( MET_CRO_3D_FIN, FSREAD3, PNAME ) ) THEN
         MSG = 'Could not open ' // MET_CRO_3D_FIN // ' file'
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF
      JDATE = SDATE3D
      JTIME = STIME3D

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Determine type of interpolation to use: linear or rational function
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      L_RATINT = .FALSE.
      MSG = 'Flag for interpolation by rational function'
!     L_RATINT = ENVYN( 'RATIONAL_FUNC', MSG, L_RATINT, STATUS )  
      IF ( .NOT. L_RATINT ) THEN
         MSG = 'Linear vertical interpolation used'
      ELSE
         MSG = 'Vertical interpolation by rational function'
      END IF
      CALL M3MESG( MSG )
         
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Interpolate by ZH
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C Get the layer mid-point heights
      VNAME = 'ZH'
      IF ( .NOT. READ3( MET_CRO_3D_FIN, VNAME, ALLAYS3, JDATE, JTIME,
     &                  ZH_OUT ) ) THEN
         MSG = 'Could not read layer heights form file  ' // MET_CRO_3D_FIN 
         CALL M3ERR( PNAME, JDATE, JTIME, MSG, .TRUE. )
      END IF

C Compute domain average layer mid-point heights

      ZH_OUT_AVG = 0.0
      DO L = 1, NLAYS
         DO C = 1, MY_NCOLS
            DO R = 1, MY_NROWS
               ZH_OUT_AVG( L ) = ZH_OUT_AVG( L ) + ZH_OUT( C,R,L )
            END DO
         END DO
         ZH_OUT_AVG( L ) = ZH_OUT_AVG( L ) / ( MY_NCOLS * MY_NROWS )
         print *, L, ZH_OUT_AVG( L )
      END DO

      DO L = 1, NLAYS_IN 
         X3_OLD( L ) = ZH_IN( L )
      END DO

      DO N = 1, NSPCS_IN   

         DO L = 1, NLAYS_IN
            WORKA( L ) = CONCIN( L,N )
         END DO

         IF ( NLAYS_IN .EQ. 1 ) THEN

            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CONCOUT( C,R,L,N ) = WORKA( 1 )
                  END DO
               END DO
            END DO

         ELSE

            DO L = 1, NLAYS

               X3 = ZH_OUT_AVG( L )

               IF ( X3 .LT. X3_OLD( 1 ) ) THEN

                  DO R = 1, MY_NROWS
                     DO C = 1, MY_NCOLS
                        CONCOUT( C,R,L,N ) = WORKA( 1 )
                     END DO
                  END DO

               ELSE IF ( X3 .GT. X3_OLD( NLAYS_IN ) ) THEN

                  DO R = 1, MY_NROWS
                     DO C = 1, MY_NCOLS
                        CONCOUT( C,R,L,N ) = WORKA( NLAYS_IN )
                     END DO
                  END DO

               ELSE

                  CALL LR_INTERP( L_RATINT, X3_OLD, WORKA, NLAYS_IN,
     &                            X3, Y, DELY )
                  DO R = 1, MY_NROWS
                     DO C = 1, MY_NCOLS
                        CONCOUT( C,R,L,N ) = Y
                     END DO
                  END DO

               END IF

            END DO

         END IF

      END DO

      RETURN

      END
