
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
C $Header: /project/yoj/arc/CCTM/src/gas/smvgear/grsmvgear.F,v 1.4 2011/10/21 16:11:14 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)grsmvgear.F       1.1 /project/mod3/CMAQ/src/chem/smvgear/SCCS/s.grsmvgear.F 07 Jul 1997 12:45:29

      SUBROUTINE SMVGEAR( IRUN, JDATE, JTIME, CHSTEP, LIRRFLAG,
     &                    NIRRCLS, IRRCELL )

C***********************************************************************
C
C  FUNCTION: To perform the Gear chemistry integration for one block
C            of cells for the requested time interval
C
C  PRECONDITIONS: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: BACKSUB
C                                    DECOMP
C                                    PDERIV
C                                    SUBFUN
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, June, 1995.
C                      Based on  the code originally developed by 
C                      M. Jacobson, (Atm. Env., Vol 28, No 2, 1994).
C                    Revised 3/14/96 by Jerry Gipson to conform to
C                      the Models-3 minimum IOV configuration    
C                    Revised December 1996 by Jerry Gipson to conform
C                      to the Models-3 interim CTM that includes emissions
C                      in chemistry.
C                    Revised April 1997 to distinguish NSPCS from NSPCSD
C                    Revised April 1997 to conform to Models-3 framework
C                    Modified June, 1997 by Jerry Gipson to be consistent
C                      with beta CTM
C                    Modified September, 1997 by Jerry Gipson to be 
C                      consistent with the targeted CTM
C                    16 Aug 01 J.Young: Use HGRD_DEFN; Use GRVARS
C                    31 Jan 05 J.Young: get BLKSIZE from dyn alloc horizontal
C                    & vertical domain specifications module (GRID_CONF)
C                    29 Jul 05     WTH: Added IF blocks that call degrade 
C                                       routines if CALL_DEG is true, i.e.,
C                                       if MECHNAME contains 'TX' substring.
C                    28 Jun 10 J.Young: convert for Namelist redesign
C                    30 Jun 10 J.Young: convert for Namelist redesign; move all
C                    local include file variables into GRVARS module
C                    29 Mar 11 S.Roselle: Replaced I/O API include files 
C                    with UTILIO_DEFN
C                    15 Jul 14 B.Hutzell: replaced mechanism include files with 
C                    RXNS_DATA module and added intent declarations to arguments
C**********************************************************************

      USE RUNTIME_VARS, ONLY: LOGDEV
      USE RXNS_DATA
      USE CGRID_SPCS          ! CGRID species number and offsets
      USE UTILIO_DEFN
      USE GRVARS              ! inherits GRID_CONF
      USE DEGRADE_ROUTINES, ONLY: DEGRADE_BLK
      USE PA_IRR_MODULE

      IMPLICIT NONE
      
C..INCLUDES: None
      
C..ARGUMENTS:
      INTEGER,   INTENT( IN ) :: IRUN               ! Counter of calls to calling subroutine
      INTEGER,   INTENT( IN ) :: JDATE              ! Date at start of integration
      INTEGER,   INTENT( IN ) :: JTIME              ! Time at start of integration
      INTEGER,   INTENT( IN ) :: NIRRCLS            ! No. of cells in block for IRR
      INTEGER,   INTENT( IN ) :: IRRCELL( : )       ! Cell No. of an IRR cell
      LOGICAL,   INTENT( IN ) :: LIRRFLAG           ! Flag for IRR calculations
      REAL( 8 ), INTENT( IN ) :: CHSTEP             ! Chemistry integration interval (min) 

C..PARAMETERS:
      INTEGER, PARAMETER :: NZERO = 0      ! Integer  zero
      INTEGER, PARAMETER :: N999 = 999     ! Integer 999
!     INTEGER, PARAMETER :: MXLOWRED = 10  ! Max number of times min time step lowered
      INTEGER, PARAMETER :: MXLOWRED = 20  ! Max number of times min time step lowered
       
      REAL( 8 ), PARAMETER :: CONCOFM = 1000000.0D0 ! Concentration of M
      REAL( 8 ), PARAMETER ::     ONE = 1.0D0
      REAL( 8 ), PARAMETER ::    ZERO = 0.0D0
      REAL( 8 ), PARAMETER ::     PT2 = 0.2D0
      REAL( 8 ), PARAMETER ::   PT003 = 0.003D0
      REAL( 8 ), PARAMETER ::    OMIC = 0.000001D0
      REAL( 8 ), PARAMETER :: OPT2MIC = 0.0000012D0
      REAL( 8 ), PARAMETER :: OPT3MIC = 0.0000013D0
      REAL( 8 ), PARAMETER :: OPT4MIC = 0.0000014D0
      REAL( 8 ), PARAMETER :: DTMIN_DEGRADE = 90.0D0  ! WTH: Minimum degradation time step (sec) 

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
      LOGICAL, SAVE :: LFIRST = .TRUE. ! Flag for first call to this subroutine

C..LOCAL VARIABLES:
      CHARACTER( 16 )  :: PNAME = 'SMVGEAR' ! Procedure name
      CHARACTER( 132 ) :: MSG = ' '        ! Message text
      CHARACTER( 12 )  :: REAL_CLOCK( 3 )
      
      LOGICAL HIGHER_ORDER   ! use next order for relative timestep
      
      INTEGER COL            ! Column number of cell
      INTEGER EXITSTAT       ! Exit status code
      INTEGER I              ! Pointer to species location in arrays
      INTEGER I1             ! Pointer to deriv location in arrays
      INTEGER IDOUB          ! Counter for testing order and step size
      INTEGER IFSUCCESS      ! Flag for successful time step, 1=Y,0=N
      INTEGER IPTR           ! Pointer for array holding the P matrix
      INTEGER ISCHAN1        ! One less than # of chemistry species
      INTEGER ISPC, JSPC     ! Loop index for species
      INTEGER ISPNEW         ! Index for species in sorted order
      INTEGER ISPOLD, JSPOLD ! Index for species in original order
      INTEGER J, JB          ! Loop indices for order
      INTEGER JEVAL          ! Flag for Jacobian update (1=yes)
      INTEGER JFAIL          ! # of error test failures in one step
      INTEGER JRESTAR        ! Counter for # of restarts
      INTEGER JS1            ! Loop index for derivative location
      INTEGER JSPC1, JSPC2   ! Pointers to species locations in arrays
      INTEGER JSPC3          ! Pointers to species locations in arrays
      INTEGER KSTEP          ! Current order + 1
      INTEGER L3             ! Newton iteration counter
      INTEGER LEV            ! Level number of cell
      INTEGER NCELL          ! Loop index for cell number
      INTEGER NEGFLAG        ! Flag for negative corrected concs
      INTEGER NQQ            ! Order of current time step
      INTEGER NQISC          ! Product of order and number of species
      INTEGER NQQISC         ! Current order times # of species
      INTEGER NQQOLD         ! Order of previous time step
      INTEGER NQUSED         ! Order of last successful step
      INTEGER NRX            ! Loop index for # of reactions
      INTEGER NSLP           ! Last step number for a Jacobian update
      INTEGER NTRCFAIL       ! Conv. failure count for trace output
      INTEGER NTREFAIL       ! Error test failure count for trace output
      INTEGER NTRNEWT        ! Newton iteration count for trace output
      INTEGER NTRPDV         ! Jacobian update count for trace output
      INTEGER NTRSTEP        ! Step count for trace output
      INTEGER NTRSUB         ! RHS evaluation count for trace output
      INTEGER NUMCELL        ! Number of cell as currently ordered
      INTEGER NYLOWRED       ! Number of times min time step lowered
      INTEGER ROW            ! Row number of cell
      INTEGER  SPC           ! species index
      INTEGER IOS            ! Allocate status
      INTEGER NCELLMAX       ! cell with maximum total error

      INTEGER, SAVE :: S_DATE_TIME( 8 )
      INTEGER, SAVE :: F_DATE_TIME( 8 )
      INTEGER, SAVE :: NSPCS1           ! # of species plus 1

      INTEGER,   ALLOCATABLE, SAVE :: DUMMY( : )    ! Dummy array for IRR call
      REAL( 8 )                    :: DTCELL        ! Time step for each cell for IRR
      REAL( 8 ), ALLOCATABLE, SAVE :: CIRR ( :, : ) ! Species concs for IRR analysis
      REAL( 8 ), ALLOCATABLE, SAVE :: RKIRR( :, : ) ! Rate constants for IRR analysis
       
      REAL( 8 ) :: ABST2             ! Inverse of time step squared
      REAL( 8 ) :: ASN1              ! Gear BDF coefficient
      REAL( 8 ) :: CCONOUT           ! Debug corrected concentration
      REAL( 8 ) :: CHEMSTEP          ! Chemistry integration interval (min)
      REAL( 8 ) :: CONSMULT          ! Parameter to increase order
      REAL( 8 ) :: DCON              ! Convergence test parameter 
      REAL( 8 ) :: DELT              ! Size of Gear time step 
      REAL( 8 ) :: DER1MAX           ! Max rms error for current order-1
      REAL( 8 ) :: DER2MAX           ! Max rms error for current order
      REAL( 8 ) :: DER3MAX           ! Max rms error for current order+1
      REAL( 8 ) :: DRATE             ! Convergence test parameter
      REAL( 8 ) :: DT_DEGRADE        ! WTH: Time step for degradation routine (sec)
      REAL( 8 ) :: EDWN              ! Trunc. error parameter for order - 1
      REAL( 8 ) :: ENQQ              ! Trunc. error parameter for current order
      REAL( 8 ) :: EPS               ! Gear relative error tolerance used
      REAL( 8 ) :: EPS1              ! reciprocal of relative error tolerance
      REAL( 8 ) :: EPSLOW            ! Relative tolerance used in h0 calc
      REAL( 8 ) :: ERRYMAX           ! Error term 
      REAL( 8 ) :: EUP               ! Trunc. error parameter for order + 1
      REAL( 8 ) :: HLAST             ! Size of previous successful time step
      REAL( 8 ) :: HRATIO            ! Relative change in a*h
      REAL( 8 ) :: HRATNTR           ! Ratio of current to previous step size
      REAL( 8 ) :: HRMAX             ! Max relative change in a*h allowed
      REAL( 8 ) :: HUSED             ! Size of current successful time step
      REAL( 8 ) :: H0FAC             ! Factor to apply to first step size
      REAL( 8 ) :: ORDER             ! # of dc/dt equations
      REAL( 8 ) :: PR1               ! 1.0 / time-step at current order - 1
      REAL( 8 ) :: PR2               ! 1.0 / time-step at current order
      REAL( 8 ) :: PR3               ! 1.0 / time-step at current order + 1
      REAL( 8 ) :: RDELMAX           ! Max relative step size change allowed
      REAL( 8 ) :: RDELT             ! Time-step ratio
      REAL( 8 ) :: RDELTA            ! Product of time step ratios
      REAL( 8 ) :: RDELTDN           ! Time step at current order - 1
      REAL( 8 ) :: RDELTSM           ! Time step at current order 
      REAL( 8 ) :: RDELTUP           ! Time step at current order + 1 
      REAL( 8 ) :: RMSERR = 0.0D0    ! Max RMS error for convergence test
      REAL( 8 ) :: RMSERRP           ! Previous max RMS error for
      REAL( 8 ) :: RMSRAT            ! Convergence rate
      REAL( 8 ) :: RMSTOP            ! Max sum of squares of error estimates
      REAL( 8 ) :: TIMREMAIN         ! Remaining integration time (min)
      REAL( 8 ) :: TOLD              ! Elapsed time at previous step (min)
      REAL( 8 ) :: XELAPS            ! Elasped time (min)
      REAL( 8 ), ALLOCATABLE, SAVE :: DELY ( : )   ! Sum of squares of error estimates
      REAL( 8 ), ALLOCATABLE, SAVE :: CEST ( :,: ) ! Error estimates for previous step 
      REAL( 8 ), ALLOCATABLE, SAVE :: CHOLD( :,: ) ! Factor for error tests
      REAL( 8 ), ALLOCATABLE, SAVE :: DTLOS( :,: ) ! Correction array for Newton iter.
      REAL( 8 ), ALLOCATABLE, SAVE :: CONC ( :,: ) ! Old concs and derivatives  
      REAL( 8 ), ALLOCATABLE, SAVE :: ERROR_CNEW ( :,: ) ! error in species predictions
      REAL( 8 ), ALLOCATABLE, SAVE :: YNEW_DEGRADE( :,:) ! concs for degradation routines
      REAL( 8 ), ALLOCATABLE, SAVE :: YLOWA( : )    ! Gear absolute error tolerance used
      REAL( 8 ), ALLOCATABLE, SAVE :: YLOWEPS( : )  ! Tolerance ratio used in h0 calc
      REAL( 8 ), ALLOCATABLE, SAVE :: YLOWEPS1( : ) ! Absolute tol over relative tol



C**********************************************************************      

      IF ( LFIRST ) THEN
         LFIRST = .FALSE.

         NSPCS1 = NUMB_MECH_SPC + 1
      
         ALLOCATE ( CEST ( BLKSIZE, NSPCS1 ),
     &              CHOLD( BLKSIZE, NUMB_MECH_SPC ),
     &              DTLOS( BLKSIZE, NSPCS1 ),
     &              CONC ( BLKSIZE, NUMB_MECH_SPC*MXORDER ),
     &              ERROR_CNEW( BLKSIZE, NUMB_MECH_SPC ),
     &              YNEW_DEGRADE( BLKSIZE, NSPCSD),
     &              DELY( BLKSIZE ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = '*** Memory allocation failure for '
     &          // 'CEST, CHOLD, DTLOS, CONC, YNEW_DEGRADE, or DELY'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )             
         END IF

         ALLOCATE ( YLOWA   ( BLKSIZE ),
     &              YLOWEPS ( BLKSIZE ),
     &              YLOWEPS1( BLKSIZE ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = '*** Memory allocation failure for '
     &          // 'YLOWA, YLOWEPS, or YLOWEPS1'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )             
         END IF
! initial absolute tolerance to run script option         
         YLOWA     = YLOW( 1 ) 

!        IF ( LIRRFLAG ) THEN
            ALLOCATE ( CIRR(  BLKSIZE, NUMB_MECH_SPC ),
     &                 DUMMY( BLKSIZE ),
     &                 RKIRR(  BLKSIZE, NRXNS ), STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
               MSG = '*** Memory allocation failure for '
     &             // 'CIRR, DUMMY, DTCELL, or RKIRR'
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )             
            END IF
            
!        END IF

      END IF

#ifdef sens
       CAVEG = 0.0D0
#endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set up for debug output and initialize variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      NSUBFUN   = 0 
      NPDERIV   = 0 
      NSTEPS    = 0
      NUMNEWT   = 0
      NCFAIL    = 0
      NEFAIL    = 0
      MXORDUSED = 0
      NUMBKUPS  = 0
      IRSTART   = -1
      NYLOWRED  = 0
      IF ( LORDERING ) THEN
         YLOWA = YLOW( 1 )
      ELSE
         CALL OPTIMAL_ATOL_PPM( CNEW, NUMCELLS, YLOWA )
      END IF
      EPS       = ERRMAX( NCS ) 
      EPS1      = 1.0D0 / EPS
      EPSLOW    = MIN( EPS, PT003 )
      ISCHAN    = ISCHANG( NCS )
      ISCHAN1   = ISCHAN - 1
      ORDER     = REAL( ISCHAN, 8 )
      JFAIL     = 0
      IF( LSUNLIGHT ) THEN
         H0FAC = 0.0625D0
      ELSE
         H0FAC = 1.0D0
      ENDIF
      CHEMSTEP = CHSTEP
      ABST2 = 1.0D0 / ( CHEMSTEP * CHEMSTEP )

      IF( LIRRFLAG ) THEN
         DO NRX = 1, NRXNS
            DO NCELL = 1, NIRRCLS
               RKIRR( NCELL, NRX ) = RK( IRRCELL( NCELL ), NRX )
            ENDDO
         ENDDO
      ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Enter here either at beginning of interval or if delt < hmin
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LOOP_BEGIN: DO 
         IFSUCCESS = 1
         IDOUB     = 2
         XELAPS    = 0.0D0 
         TOLD      = XELAPS
         RDELMAX   = 10000.0D0
         NSLP      = MBETWEEN
         JRESTAR   = 0 
         HRMAX     = 0.3D0
         HRATIO    = 0.0D0
         ASN1      = 1.0D0
         YLOWEPS1  = YLOWA * EPS1 
         TIMREMAIN = CHEMSTEP
C:WTH    
         DT_DEGRADE = 0.0D0      
      

         LOOP_INITIALIZE: DO
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Calculate initial values of first derivatives and do debug output 
c  if necessary
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            CALL SUBFUN

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  At the integration start or restart (e.g., the first time-step   
c  or if excessive failures occur), the time-step is predicted with
c  the formula from LSODES (Hindmarsh and Sherman, 1983 - ODEPACK).
c  However, if the cells are being reordered, return after estimating
c  the stiffness.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            YLOWEPS = YLOWA / EPSLOW 
            DO NCELL = 1, NUMCELLS
              DELY( NCELL ) = 0.0D0
            ENDDO        
            
c...compute sum of squares of dcdt/conc
            DO 220 JSPC = 1, ISCHAN     
               DO 220 NCELL = 1, NUMCELLS 
                  ERRYMAX  = GLOSS( NCELL, JSPC ) / 
     &                      ( CNEW( NCELL, JSPC ) + YLOWEPS( NCELL ) )
                  DELY( NCELL ) = DELY( NCELL ) + ERRYMAX * ERRYMAX  
                  ERROR_CNEW(NCELL,JSPC) = ERRYMAX
 220        CONTINUE
         
c.....if reordering, store stiffness estimate and return  
            IF( LORDERING ) THEN
               DO NCELL = 1, NUMCELLS 
                  ERRMX2( OFFSET + NCELL ) = DELY( NCELL )
               ENDDO
               RETURN
            ENDIF
         
c.....get largest stiffness estimate and compute from 
c.....sqrt(rmstop / [epslow * order]) = rmsnorm of error scaled 
c.....to epslow * cnew + ylow
            RMSTOP  = 0.0
            DO 260 NCELL = 1, NUMCELLS
               IF( DELY( NCELL ) .GT. RMSTOP ) THEN
                  RMSTOP = DELY( NCELL )   
               END IF
  260       CONTINUE
            DELT = SQRT( EPSLOW / ( ABST2 + RMSTOP / ORDER ) ) * H0FAC
            DELT = MIN( DELT, CHEMSTEP ) 
            DELT = MIN( DELT, HMAX ) 
            DELT = MAX( DELT, HMIN )
            NQQ      = 1
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Store initial concentration and first derivatives x time-step and
c  initialize/save some data 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            DO 280 JSPC = 1, ISCHAN 
               JS1 = ISCHAN + JSPC
               DO 280 NCELL = 1, NUMCELLS
                  CONC( NCELL, JSPC ) = CNEW( NCELL, JSPC )
                  CONC( NCELL, JS1 )  = DELT * GLOSS( NCELL, JSPC ) 
  280       CONTINUE
            
            NQQOLD = 0
            RDELT  = 1.0D0 
            JEVAL  = 1
            
            IF( LIRRFLAG ) THEN
               DTCELL = 0.0D0
               DO I = 1, ISCHAN
                  DO NCELL = 1, NIRRCLS
                     CIRR( NCELL, I ) = BLKCONC( IRRCELL( NCELL ), I )
                  ENDDO
               ENDDO
               CALL PA_IRR ( .TRUE., .FALSE., RKIRR, CIRR, DTCELL, 
     &                          NIRRCLS, DUMMY )
            ENDIF
            
            IF( CALL_DEG ) THEN   !:WTH Initialize for degradation routines
               YNEW_DEGRADE = 0.0D0
               DO I = 1, ISCHAN
                  SPC    = CGRID_INDEX( I )
                  DO NCELL = 1, NUMCELLS
                     YNEW_DEGRADE( NCELL,SPC ) = BLKCONC( NCELL, I )
                  END DO
               END DO
            ENDIF
         
#ifdef sens
            DO ISPOLD = 1, ISCHAN
                DO NCELL = 1, NUMCELLS
                   CFINI( NCELL,ISPOLD ) = BLKCONC( NCELL,ISPOLD )
                END DO
            END DO
#endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Update coefficients of the order. NQQ is the order. ASET and 
c  PERTST are defined in SUBROUTINE JSPARSE. Note that PERTST    
c  is the original pertst**2. Also, exit smvgear if we reach the 
c  end of the time-interval
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            LOOP_ORDER: DO
            
               HIGHER_ORDER = .TRUE.
               TIMREMAIN = CHEMSTEP - XELAPS
               IF( NSTEPS .EQ. 0 ) IRSTART = IRSTART + 1
         
         
c.....quit or update coefficients
              IF( TIMREMAIN .LE. OMIC )EXIT LOOP_BEGIN
              
              IF( NQQ .NE. NQQOLD ) THEN
                 NQQOLD  = NQQ 
                 KSTEP   = NQQ + 1
                 HRATIO  = HRATIO * ASET( NQQ, 1 ) / ASN1
                 ASN1    = ASET(   NQQ, 1 )  
                 ENQQ    = PERTST( NQQ, 1 ) * ORDER
                 EUP     = PERTST( NQQ, 2 ) * ORDER 
                 EDWN    = PERTST( NQQ, 3 ) * ORDER  
                 NQQISC  = NQQ * ISCHAN 
              ENDIF
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Update the time step and do debug output if necessary
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
              RDELT   = MIN( RDELT, RDELMAX, HMAX/DELT, TIMREMAIN/DELT )
              DELT    = DELT   * RDELT
              HRATIO  = HRATIO * RDELT
            
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  If delt < hmin, decrease ylowa and re-start the time-interval
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
              IF( DELT .LT. HMIN ) THEN
                 WRITE( MSG, 94000 ) DELT, XELAPS, TIMREMAIN, MAXVAL(YLOWA), EPS
                 CALL M3MESG( MSG )  
                 WRITE( MSG, 94010 ) NSTEPS, BLKID, IRUN, NQQ
                 CALL M3MESG( MSG )  
                 WRITE( MSG, 94015 ) RDELT, DER2MAX, ENQQ
                 CALL M3MESG( MSG )  
                 YLOWA     =  YLOWA * 0.1 
                 NYLOWRED  =  NYLOWRED + 1
                 IF( NYLOWRED .LT. MXLOWRED ) THEN
                    DO 340 ISPNEW = 1, ISCHAN 
                       ISPOLD   = INEW2OLD( ISPNEW, NCS )
                       DO 340 NCELL = 1, NUMCELLS
                          CNEW( NCELL, ISPNEW ) = BLKCONC( NCELL, ISPOLD )
 340                CONTINUE
                    CYCLE LOOP_BEGIN
                 ELSE
              
                  WRITE(LOGDEV,'(A16,6(1X,A12))')'CHEMSTRY_SPECIES',' Pred.Conc  ',' Init.Conc  ',' Squar.Erro  '
                  DO ISPOLD = 1, ISCHAN
                     ISPNEW   = IOLD2NEW( ISPOLD, NCS )
                     WRITE(LOGDEV,'(A16,6(1X,ES12.4))')CHEMISTRY_SPC(ISPOLD),
     &               CNEW( NCELLMAX, ISPNEW ), BLKCONC( NCELLMAX, ISPOLD ),
     &               (ERROR_CNEW(NCELLMAX,ISPNEW)*ERROR_CNEW(NCELLMAX,ISPNEW))
                  END DO
                  write(logdev,'(A,ES12.4)')'For error = ',DELY( NCELLMAX )
                  WRITE(LOGDEV,'(A16,3(1X,A12))')'Reaction.Label',' Rate.Const ',' React.Rate  '
                  DO J = 1,NRXNS
                     WRITE(LOGDEV,'(A16,3(1X,ES12.4))')RXLABEL(J),RK(NCELLMAX,J),RXRAT(NCELLMAX,J)
                  END DO
              
                    WRITE( MSG, 94020 )
                    CALL M3EXIT( 'SMVGEAR', JDATE, JTIME, MSG, XSTAT2 )             
                 ENDIF
              ENDIF
            
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
c  If delt is different than during the last step (if rdelt.ne.1),
c  scale the derivatives
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
              IF( RDELT .NE. 1.0 ) THEN
                 RDELTA = 1.0
                 I1 = 1
                 DO 360 J = 2, KSTEP 
                    RDELTA = RDELTA * RDELT 
                    I1 = I1 + ISCHAN 
                    DO 360 I = I1, I1 + ISCHAN1
                       DO 360 NCELL = 1, NUMCELLS
                          CONC( NCELL, I )  = CONC( NCELL, I ) * RDELTA 
  360            CONTINUE
              ENDIF
              IF( RDELT .LT. 1.0 ) NUMBKUPS = NUMBKUPS + 1
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  If the last step was successful, reset rdelmax = 10 and update
c  the chold array with current values of cnew.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
              IF( IFSUCCESS .EQ. 1 ) THEN
                 RDELMAX = 10.0
                 IF( MOD( NSTEPS,3 ) .EQ. 2 )THEN
                    CALL OPTIMAL_ATOL_PPM( CNEW, NUMCELLS, YLOWA )       
                 END IF  
                 YLOWEPS1  = YLOWA * EPS1 
                 DO 380 JSPC = 1, ISCHAN
                    DO 380 NCELL = 1, NUMCELLS
                       CHOLD( NCELL, JSPC )  = EPS1 / 
     &                     ( ABS( CNEW( NCELL, JSPC ) ) + YLOWEPS1(NCELL) )
  380            CONTINUE
              ENDIF
            
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                           PREDICTION
c  Check whether the Jacobian should be updated, compute the predicted 
c  concentration and derivatives by multiplying previous values by
c  the pascal triangle matrix, and  do debug output if necessary
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
              IF( ABS( HRATIO-1.0 ) .GT. HRMAX. OR. NSTEPS .GE. NSLP ) JEVAL = 1
              XELAPS = XELAPS + DELT 
              I1 = NQQISC + 1
              DO 400 JB = 1,   NQQ
                 I1 = I1 - ISCHAN
                 DO 400 I = I1, NQQISC
                    J = I + ISCHAN
                    DO 400 NCELL  = 1, NUMCELLS
                       CONC( NCELL, I ) = CONC( NCELL, I ) + CONC( NCELL, J )
  400         CONTINUE
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                          NEWTON ITERATION
c  This is the top of the corrector loop. Take up to mstep corrector 
c  iterations. Test convergence by requiring that changes be less
c  than the rms norm weighted by chold. Accumulate the correction in
c  the array dtlos(). It equals the j-th derivative of conc() 
c  multiplied by delt**kstep / (factorial(kstep-1)*ASET(kstep)); thus, 
c  it is proportional to the actual errors to the lowest power of
c  delt present (delt**kstep)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  440         L3 = 0
         
c.....Increment counter, initialize cnew, and zero the correction array
              NUMNEWT = NUMNEWT + 1
              DO 460 I = 1, ISCHAN
              DO 460 NCELL = 1, NUMCELLS
                 CNEW(  NCELL, I ) = CONC( NCELL, I )
                 DTLOS( NCELL, I ) = 0.0D0
  460         CONTINUE
  
c.....If JEVAL = 1, re-evaluate predictor matrix before starting the
c.....corrector iteration. After calling PDERIV and DECOMP, set 
c.....JEVAL = -1 to prevent recalling PDERIV until necessary 
              IF( JEVAL .EQ. 1 ) THEN
                 R1DELT = -ASN1 * DELT
                 CALL PDERIV         
                 CALL DECOMP
                 JEVAL    = -1 
                 HRATIO   = 1.0D0
                 NSLP     = NSTEPS + MBETWEEN
                 DRATE    = 0.70D0
              ENDIF
      
c.....evaluate the first derivative using latest cnew and do debug
c.....output if necessary
  520         CONTINUE
              CALL SUBFUN
              
c.....compute error (gloss) from the corrected calculation of the 
c.....first derivative and do debug output if necessary
              DO 620  ISPC = 1,  ISCHAN
                 JSPC  = ISPC + ISCHAN
                 DO 620 NCELL = 1, NUMCELLS
                    GLOSS( NCELL, ISPC ) = DELT * GLOSS( NCELL, ISPC ) -
     &                                     ( CONC( NCELL, JSPC ) +
     &                                       DTLOS( NCELL, ISPC ) )
 620          CONTINUE
            
c.....Solve the linear system of equations with the corrector error.
c.....Backsub.f originally from numerical recipes (Press et al.)
              CALL BACKSUB
         
         
c.....Sum-up the accumulated error, correct the concentration with the
c.....error, and begin to calculate the rmsnorm of the error relative
c.....to chold. If a negative conc occurs, treat it like a 
c.....convergence failure.
              DO NCELL = 1, NUMCELLS
                 DELY( NCELL ) = 0.0D0
              ENDDO         
              
              NEGFLAG = + 1
              DO 700 ISPC = 1, ISCHAN
                 DO 700 NCELL  = 1, NUMCELLS
                    DTLOS( NCELL, ISPC ) = DTLOS( NCELL, ISPC ) +
     &                                     GLOSS( NCELL, ISPC )
                    CNEW(  NCELL, ISPC ) = CONC(  NCELL, ISPC ) + ASN1 * 
     &                                     DTLOS( NCELL ,ISPC )
                    ERRYMAX  = GLOSS( NCELL, ISPC ) * CHOLD( NCELL, ISPC )
                    DELY( NCELL ) = DELY( NCELL ) + ERRYMAX * ERRYMAX
                    ERROR_CNEW(NCELL,ISPC) = ERRYMAX
 700          CONTINUE
              NEGFLAG = 0
              
c....Save the previous rms error and calculate the new rms error.
              RMSERRP   = RMSERR
              DER2MAX   = 0.0D0
              DO NCELL = 1, NUMCELLS
                 IF( DELY( NCELL ).GT.DER2MAX )THEN
                        DER2MAX = DELY( NCELL )   
                        NCELLMAX = NCELL
                 END IF
              ENDDO
              RMSERR   = SQRT( DER2MAX / ORDER )
              L3       = L3 + 1
              RMSRAT   = 0.0D0
              IF( L3.GT.1 ) THEN
                 RMSRAT   =  RMSERR / RMSERRP
                 DRATE    =  MAX( 0.2 * DRATE, RMSRAT )
              ENDIF
      
c....If dcon < 1, then sufficient convergence has occurred. Otherwise,
c....if the ratio of the current to previous rmserr is decreasing,
c....iterate more. If it is not, then the convergence test failed.
              DCON = RMSERR * MIN( CONPST( NQQ ), CONP15( NQQ ) * DRATE )
              IF( DCON .LE. 1.0 ) GO TO 780 ! convergence achieved
              
                      
              IF( L3 .LT. MSTEP ) GO TO 520 ! continue Newton Iteration
          
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                        CONVERGENCE FAILURE
c  The next block of code is executed when the convergence test fails.
c  If the Jacobian matrix is more than one step old, update it and
c  try for convergence again. If the Jacobian is current, reduce the 
c  time-step, re-set the accumulated derivatives to their values
c  before the failed step, and retry with the smaller step.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
               NCFAIL = NCFAIL + 1
               
               IF( JEVAL .EQ. 0 ) THEN
                  JEVAL   = 1
                  GO TO 440  ! restart Newton Iteration with updated Jacobian
               ENDIF
               RDELMAX    = 2.0
               XELAPS     = TOLD
               RDELT      = FRACDEC
               JEVAL      = 1
               IFSUCCESS  = 0
               I1         = NQQISC + 1
               DO 760 JB = 1,   NQQ
                  I1 = I1 - ISCHAN
                  DO 760 I = I1,  NQQISC
                     J = I + ISCHAN
                     DO 760 NCELL = 1, NUMCELLS
                        CONC( NCELL, I ) = CONC( NCELL, I ) - CONC( NCELL, J )
 760           CONTINUE
               
               
               CYCLE LOOP_ORDER   
      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                        CONVERGENCE ACHIEVED
c  The program comes here when the corrector iteration converges. Set
c  JEVAL = 0 so that it does not need to be called on the next step and
c  then test the accumulated error.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  780          JEVAL  = 0
               
               
               IF( L3 .GT. 1 ) THEN
                  DO NCELL  = 1, NUMCELLS
                     DELY( NCELL )  = 0.0D0
                  ENDDO
                  DO 840 JSPC   = 1, ISCHAN     
                     DO 840 NCELL = 1, NUMCELLS 
                        ERRYMAX  = DTLOS( NCELL, JSPC ) * CHOLD( NCELL, JSPC )
                        DELY( NCELL ) = DELY( NCELL ) + ERRYMAX * ERRYMAX
                        ERROR_CNEW(NCELL,JSPC) = ERRYMAX
  840             CONTINUE
                  DER2MAX  = 0.0D0
                  DO NCELL  = 1, NUMCELLS
                     IF( DELY( NCELL ) .GT. DER2MAX )THEN
                         DER2MAX = DELY( NCELL )   
                         NCELLMAX = NCELL
                     END IF
                  ENDDO
               ENDIF
      
               IF( DER2MAX .GT. ENQQ ) THEN
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                          ERROR TEST FAILURE
c  The next block of code is executed when the error test fails.  In 
c  all cases, the derivatives are re-set to their values before trying
c  this time-step. Then,
c  (a) if the number of error test failures is LE 6, the time-step
c      is re-estimated at the same or one lower order and the step
c      retried;    
c  (b) if the number of failures is GT 6 and LE 20, the time-step is
c      lowered by fracdec and the step retried;
c  (c) If the number of failures is GT 20, the order is reset to one,
c      the time-step lowered by 0.1, and the step retried;
c  (d) if the number of failures is GE 100, the program is stopped.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                  NEFAIL = NEFAIL + 1
                           
                  XELAPS = TOLD
                  JFAIL  = JFAIL  + 1
                  I1     = NQQISC + 1
                  DO 880 JB = 1, NQQ
                     I1  = I1 - ISCHAN
                     DO 880 I = I1, NQQISC
                        J = I + ISCHAN
                        DO 880 NCELL   = 1, NUMCELLS
                           CONC( NCELL, I ) = CONC( NCELL, I ) - CONC( NCELL, J )
 880              CONTINUE
                  RDELMAX          = 2.0D0
                  
                  IF( JFAIL .LE. 6 ) THEN
                     IFSUCCESS       = 0 
                     RDELTUP         = 0.0D0
                     HIGHER_ORDER    = .FALSE.
                  ELSEIF( JFAIL .LE. 20 ) THEN
                     IFSUCCESS       = 0 
                     RDELT           = FRACDEC
                     CYCLE LOOP_ORDER
                  ELSE
                     IFSUCCESS       = 1  
                     DELT            = DELT * 0.1D0
                     RDELT           = 1.0D0
                     JFAIL           = 0 
                     JRESTAR         = JRESTAR + 1
                     IDOUB           = 5
                     DO 900 I = 1, ISCHAN
                        DO 900 NCELL   = 1, NUMCELLS
                           CNEW( NCELL, I ) = CONC( NCELL, I )
  900                CONTINUE
                     WRITE( MSG, 94040 ) DELT, XELAPS 
                     IF( JRESTAR .EQ. 100 ) THEN
                        MSG = 'Integration stopped because of excessive errors.'
                        CALL M3EXIT( 'SMVGEAR', JDATE, JTIME, MSG, XSTAT2 )
                     ENDIF
                     CYCLE LOOP_INITIALIZE
                  ENDIF
               
               ELSE      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                          ERROR TEST PASSED
c  The following block of code is executed when the error test is
c  passed (i.e., the step was successful). Reset JFAIL = 0, set 
c  IFSUCCESS = 1, reset TOLD, increment NSTEPS, update the
c  concentration and all derivatives, zero concentrations and 
c  derivatives below the specified lower concentration bound, do
c  any requested outputs, and go the end if this is the last step.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                  JFAIL     = 0
                  IFSUCCESS = 1
                  TOLD      = XELAPS
                  NSTEPS    = NSTEPS + 1
                  NQUSED    = NQQ
                  HUSED     = DELT
                  MXORDUSED = MAX( MXORDUSED, NQUSED )
                  
                  I1 = 1 - ISCHAN
                  DO 920 J = 1, KSTEP
                     I1 = I1 + ISCHAN
                     DO 920 I = I1, I1 + ISCHAN1
                        JSPC = I - I1 + 1
                        DO 920 NCELL = 1, NUMCELLS
                           CONC( NCELL, I ) = CONC( NCELL, I ) + ASET( NQQ, J ) *
     &                                        DTLOS( NCELL, JSPC )
  920             CONTINUE
                  
                  DO 940 I = 1, ISCHAN
                     DO 940 NCELL = 1, NUMCELLS
                        IF( CONC( NCELL, I ) .LE. CONCMIN ) THEN
                            CONC( NCELL, I ) = ZBOUND
                            CNEW( NCELL, I ) = ZBOUND
                            CONC( NCELL, I +     ISCHAN ) = 0.0D0
                            CONC( NCELL, I + 2 * ISCHAN ) = 0.0D0
                            CONC( NCELL, I + 3 * ISCHAN ) = 0.0D0
                            CONC( NCELL, I + 4 * ISCHAN ) = 0.0D0
                            CONC( NCELL, I + 5 * ISCHAN ) = 0.0D0
                        ENDIF
  940             CONTINUE 
                  
                  IF( LIRRFLAG ) THEN
                     DTCELL = DELT
                     DO I = 1, ISCHAN
                        ISPOLD = INEW2OLD( I, NCS )
                        DO NCELL = 1, NIRRCLS
                           CIRR( NCELL, ISPOLD ) = CONC( IRRCELL( NCELL ), I )
                        ENDDO
                     ENDDO
                     CALL PA_IRR ( .FALSE., .FALSE., RKIRR, CIRR, DTCELL, 
     &                                NIRRCLS, DUMMY )
                  ENDIF
                  
                  IF( CALL_DEG ) THEN   !:WTH update DT_DEGRADE and apply
                     DT_DEGRADE = 60.0D0 * DELT + DT_DEGRADE
                     IF( DT_DEGRADE .GE. DTMIN_DEGRADE .OR. ( CHEMSTEP - XELAPS ) .LE. OMIC ) THEN
                        DO I = 1, ISCHAN
                           ISPOLD = INEW2OLD( I, NCS )
                           SPC      = CGRID_INDEX( ISPOLD )
                           DO NCELL = 1, NUMCELLS
                              YNEW_DEGRADE( NCELL, SPC ) = CONC( NCELL, I )
                           ENDDO
                        ENDDO
                        CALL DEGRADE_BLK( YNEW_DEGRADE, DT_DEGRADE, JDATE, JTIME, BLKID )
                        DT_DEGRADE = 0.0D0
                     ENDIF
                  ENDIF
                  
#ifdef sens       
                  DO ISPOLD = 1, ISCHAN
                    ISPNEW = IOLD2NEW( ISPOLD,1 )
                    DO NCELL = 1, NUMCELLS
                       CAVEG( NCELL, ISPOLD ) = CAVEG( NCELL, ISPOLD )
     &                                         + 0.5D0 * ( CFINI( NCELL, ISPOLD ) + CONC( NCELL, ISPNEW ) )
     &                                         * DELT
                       CFINI( NCELL, ISPOLD ) = CONC( NCELL, ISPNEW )
                    ENDDO
                  ENDDO
#endif         
                 
         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                    SELECTION OF NEXT TIME-STEP
c  The next block of code determines the next whether to test the 
c  time-step and order for a change. IDOUB counts the number of 
c  successful steps before testing for a change.
c  If IDOUB > 1, decrease IDOUB and go on to the next time-step with
c                the current step-size and order.
c  If IDOUB = 1, store the value of the error (DTLOS) for the time- 
c                step prediction, which will occur when idoub = 0,
c                but go on to the next step with the current step-
c                size and order. 
c  If IDOUB = 0, test the time-step and order for a change.  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                  IF( IDOUB .GT. 1 ) THEN
                     IDOUB = IDOUB - 1
                     IF( IDOUB .EQ. 1 ) THEN
                        DO 980 JSPC = 1, ISCHAN, 2 
                           JSPC1 = JSPC + 1
                           DO 980 NCELL = 1, NUMCELLS
                              CEST( NCELL,  JSPC ) = DTLOS( NCELL,  JSPC )
                              CEST( NCELL, JSPC1 ) = DTLOS( NCELL, JSPC1 )
  980                   CONTINUE
                     ENDIF
                     RDELT = 1.0
                                 
                     CYCLE LOOP_ORDER
                  ENDIF
            
               ENDIF        ! End of error test block

               IF( HIGHER_ORDER )THEN
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  The following block of code gets estimates for the next step-size by 
c  computing step sizes at the curent order, one order higher than
c  the current order, and one order lower than the current order.
c  In each case, the smallest step-size among all cells is selected.
c  The final step-size and order selected are the ones that give the
c  largest step-size.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c.....estimate the time-step ratio at one order higher
                  IF( NQQ .LT. MAXORD ) THEN  
                     DO NCELL = 1, NUMCELLS
                        DELY( NCELL ) = 0.0D0
                     ENDDO
                     DO 1020 JSPC = 1, ISCHAN     
                        DO 1020 NCELL = 1, NUMCELLS 
                           ERRYMAX = ( DTLOS( NCELL, JSPC ) - 
     &                                 CEST( NCELL, JSPC ) ) * CHOLD( NCELL, JSPC )
                           DELY( NCELL ) = DELY( NCELL ) + ERRYMAX * ERRYMAX
                           ERROR_CNEW(NCELL,JSPC) = ERRYMAX
 1020                CONTINUE
                     DER3MAX = 0.0D0
                     DO NCELL = 1, NUMCELLS
                        IF( DELY( NCELL ) .GT. DER3MAX )THEN
                            DER3MAX = DELY( NCELL )   
                            NCELLMAX = NCELL
                        END IF
                     ENDDO       
                     PR3  = 1.4 * ( DER3MAX / EUP ) ** ENQQ3( NQQ )
                     RDELTUP  = 1.0D0 / ( PR3 + OPT4MIC )
                  ELSE
                     RDELTUP  = 0.0D0
                  ENDIF
               END IF ! HIGHER_ORDER
               HIGHER_ORDER = .TRUE.
      
c.....estimate the time-step ratio at the current order
 1060          PR2       = 1.2D0 * ( DER2MAX / ENQQ ) ** ENQQ2( NQQ )
               RDELTSM   = 1.0D0 / ( PR2 + OPT2MIC )
               
c.....estimate the time-step ratio at one order lower
               IF( NQQ .GT. 1 ) THEN
                  DO NCELL = 1, NUMCELLS
                     DELY( NCELL ) = 0.0D0
                  ENDDO
                  DO 1100 JSPC = 1, ISCHAN     
                     I = JSPC + ( KSTEP - 1 ) * ISCHAN
                     DO 1100 NCELL = 1, NUMCELLS 
                         ERRYMAX       = CONC( NCELL, I ) * CHOLD( NCELL, JSPC )
                         DELY( NCELL ) = DELY( NCELL ) + ERRYMAX * ERRYMAX
                         ERROR_CNEW(NCELL,JSPC) = ERRYMAX
 1100             CONTINUE
                  DER1MAX       = 0.0D0
                  DO NCELL = 1, NUMCELLS
                     IF( DELY( NCELL ) .GT. DER1MAX )THEN
                         DER1MAX = DELY( NCELL )   
                         NCELLMAX = NCELL
                     END IF
                  ENDDO        
                  PR1      = 1.3D0 * ( DER1MAX / EDWN ) ** ENQQ1( NQQ )
                  RDELTDN  = 1.0D0 / ( PR1 + OPT3MIC )
               ELSE
                  RDELTDN  = 0.0D0
               ENDIF
         
c.....choose the largest of the predicted time-steps ratios 
               RDELT      = MAX( RDELTSM, RDELTUP, RDELTDN )
            
            
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  The next block of code selects the final size of the next time
c  step and then returns to statement 440 to do the next step.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c.....If the last step was successful and rdelt is small, keep the
c.....current step and order and require three successful steps before
c.....re-checking the time-step and order
               IF( RDELT .LT. 1.1 .AND. IFSUCCESS .EQ. 1 ) THEN
                  IDOUB = 3
                  CYCLE LOOP_ORDER
               ENDIF
      
c.....If the maximum time-step ratio is that of one order lower than
c.....the current order, decrease the order. If the current step failed,
c.....make sure the next step-size is no larger than the current size
               IF( RDELT .EQ. RDELTDN ) THEN
                  NQQ = NQQ - 1
                  IF( IFSUCCESS .EQ. 0 ) RDELT = MIN( RDELT, ONE )
               ELSEIF( RDELT .EQ. RDELTUP ) THEN
         
c.....If the maximum time-step ratio is that of one order higher than
c.....the current order, increase the order and add a derivative term
c.....for the higher order.
                  CONSMULT   = ASET( NQQ, KSTEP ) / REAL( KSTEP, 8 )
                  NQQ        = KSTEP
                  NQISC      = NQQ * ISCHAN
                  DO 1140 JSPC = 1, ISCHAN
                     I1 = JSPC + NQISC
                     DO 1140 NCELL  = 1, NUMCELLS 
                        CONC( NCELL, I1 ) = DTLOS( NCELL, JSPC ) * CONSMULT
 1140             CONTINUE
               ENDIF

c.....If the last two steps have failed, minimize the time-step ratio.
c.....In all cases, re-set idoub to the current order + 1.
               IF( JFAIL .GE. 2 ) RDELT = MIN( RDELT, PT2 )
               IDOUB = NQQ + 1
              END DO LOOP_ORDER
             
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  The next block of code is performed when the integration has been
c  succesfully completed for the required time period. Set final
c  concentrations, update counters,do any required output, and return
c  to the driver subroutine -- CHEM.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
         END DO LOOP_INITIALIZE
      END DO LOOP_BEGIN
         
      DO 1180 ISPC = 1, ISCHAN
         DO 1180 NCELL = 1, NUMCELLS
            IF( CNEW( NCELL, ISPC ) .LE. ZBOUND ) 
     &          CNEW( NCELL, ISPC ) = CONCMIN
 1180 CONTINUE
 
#ifdef sens 
         CHEMSTEP  = 1.0D0 / CHEMSTEP
         DO I = 1, ISCHAN
            DO NCELL = 1, NUMCELLS
               CAVEG( NCELL,I ) = MAX( CAVEG( NCELL,I )*CHEMSTEP,CONCMIN )
            ENDDO
        ENDDO
#endif
       
      RETURN

C********************** FORMAT STATEMENTS ******************************
      
93000 FORMAT( /1X, 'Rate constants and reaction rates',
     &             ' used for irun=',I4,', nstep=',I4,', nsubfun=',I4,
     &             ' delt=0.0' )
93020 FORMAT(  1X, 'n= ', I3, ' k=', 1PE20.8, ' R=', 1PE20.8 )
93040 FORMAT( /1X, 'Species concentrations used for irun=', I4,
     &             ' nstep=', I4, ' nsubfun=', I4 )
93060 FORMAT(  1X, 'C= ', I3, 2X, A4, 2X, 1PE20.8 )
93080 FORMAT(  1X, 'C= ', I3, 2X, 'M   ', 2X, 1PE20.8 ) 
93100 FORMAT(  1X, 'C= ', I3, 2X, 'O2  ', 2X, 1PE20.8 ) 
93120 FORMAT(  1X, 'C= ', I3, 2X, 'N2  ', 2X, 1PE20.8 ) 
93140 FORMAT(  1X, 'C= ', I3, 2X, 'H2O ', 2X, 1PE20.8 ) 
93160 FORMAT( /1X, 'dc/dt for irun=', I4, ' nstep=', I4,
     &             ' nsubfun=', I4, ' delt=0.0' )
93180 FORMAT(  1X, 'dcdt= ', I3, 1X, A10, 4X, 1X, 1PE20.8 )
93200 FORMAT(  1X, I3, I5, 5I4, I5, F11.6, 1X, 1PE11.4, 1X, 0PF10.3 )
93220 FORMAT( /1X, '****Start or restart step =', I4, ' for irun=', I3,
     &             ' block=', I3 )
93240 FORMAT(  5X, 'Data for irun', I5, ' block', I3, ' cell', I7,
     &             ' (',I3, ',', I3, ',', I2, ')' ) 
93260 FORMAT(  5X, 'xelaps   = ', 1PE15.8, ' old delt    = ', 1PE15.8/
     &        10X, 'new delt = ', 1PE15.8, ' timremain   = ', 1PE15.8/
     &        10X, 'rdelt=', 1PE15.8, ' nqq    = ', I4,
     &             ' kstep = ', I4, ' idoub  = ', I4, ' jeval=', I3 )
93280 FORMAT( /1X, 'Conc and derivs at beginning of step' )
93300 FORMAT(  1X, 'Conc', I4, 1X, A16, 4X, 1X, 4( 1PE15.8 ) )
93320 FORMAT( /1X, 'Predicted Conc for irun=', I4, ' nstep=', I4,
     &             ' delt=', 1PE15.8 )
93340 FORMAT(  1X, 'C(p)=', I3, 1X, A16, 4X, 1X, 4( 1PE15.8 ) )
93360 FORMAT( /1X )
93380 FORMAT(  5X, 'Pderiv called for irun=', I4, ' nstep=', I4,
     &             ' npderiv=', I4, ' hratio=', 1PE12.4 )
93400 FORMAT( /1X, 'Jacobian for irun=', I4,' nstep=', I4,
     &             ' npdout=', I4, ' l3=', I2, ' delt=', 1PE15.8 )
93420 FORMAT(  1x, 'jac ', 3I4, 1X, A16, 4X, 1x, A16, 4X, 1X, 1PE20.8 )
93440 FORMAT(  5X, 'Subfun was called for irun=', I3, ' nstep=', I3,
     &             ' nsubfun=', I3,' iter=', I3 )
93460 FORMAT( /1X, 'Concs at start of iter=',I3,
     &             ' irun=', I4, ' nstep=', I4, ' delt=', 1PE15.8 )
93480 FORMAT(  1X, 'Cm(', I1, ') =', I3, 1X, A10, 4X, 1X, 4( 1PE15.8 ) )
93500 FORMAT( /1X, 'dc/dt for irun=', I4, ' nstep=' , I4, ' nsubfun=',
     &              I4 )
93520 FORMAT(  1X, 'dcdt= ', I3, 1X, A10, 4X, 1X, 1PE20.8 )
93540 FORMAT( /1X, 'Error for irun=', I4, ' nstep=', I4, ' iter=', I4,
     &             ' delt=',1PE15.8 )
93560 FORMAT(  1X, 'Cerr=', I3, 1X, A10, 4X, 1X, 1PE20.8 )
93580 FORMAT( /1X, 'Correction for irun=', I4,' nstep=', I4,' iter=',
     &              I4, ' delt=', 1PE15.8 )
93600 FORMAT(  1X, 'Coor=', I3, 1X, A10, 4X, 1X, 1PE20.8 )
93620 FORMAT(  5X, 'No convergence for irun=', I4, ' step=', I3,
     &             ' iter=', I1,' delt=', 1PE15.8 / 10x, 'dcon=',
     &               1PE15.8, ' rmsrat=', 1PE15.8 )
93640 FORMAT(  5X, 'Corrector failed to converge for irun=', I4,
     &             ' nsteps=', I4,' after ', I4,' tries , numneg=', I6 )
93660 FORMAT(  5X, 'Time step being reduced for irun=', I4, ' step=',
     &              I4, ' rdelt=', 1PE15.8 )         
93680 FORMAT(  5X, 'Convergence achieved for irun=', I4, ' nsteps=', I4,
     &             ' after ', I4,' tries , delt=', 1PE15.8 )
93700 FORMAT( /1X, ' Conc after successful convergence' )
93720 FORMAT(  1X, 'C(e)=', I3, 1X, A10, 4X, 1X, 4( 1PE15.8 ) )
93740 FORMAT(  5X, 'Error test failed for irun=', I4,' nsteps=', I4,
     &             ' delt=', 1PE15.8 / 10x,' d=', E15.8, 1X, ' e=',
     &              E15.8 )
93760 FORMAT(  5X, 'Error test passed for step=', I4,
     &             ' of irun=',I4/10x,'delt=', 1PE20.8,
     &             ' xelaps=', 1PE20.8 / 10X, 'nqq=', I3,
     &             ' kstep=', I3, ' idoub=', I3 )
93780 FORMAT( /1X, 'C and Cprimes after ', I4,
     &             ' steps of irun=',I4 )
93800 FORMAT(  1X, 'C(n)=', I4, 1X, A10, 4X, 1X, 4( 1PE15.8 ) )
93820 FORMAT(  5X, 'rdelt=', 1PE15.8, ' idoub=', I3, ' at irun=', I4,
     &             '  nstep=', I4 )
93840 FORMAT(  1X, '****** End of step ', I4, '  **********' )
93860 FORMAT(  5X, 'rdelt @ nq=', E15.8, ' rdelt @ nq+1=', E15.8,
     &             ' rdelt @ nq-1=', E15.8 )
93900 FORMAT( 'SMVGEAR: NCELL, RMSTOP, CCOL, CROW, NLEV =', I8, E15.8, 3I8 )
94000 FORMAT( 'SMVGEAR: delt too small =', 1PE8.2,' time, ',
     &        'timremain, ylowa, eps = ',4( 1PE9.3, 1X ) )
94010 FORMAT( 'NSTEPS, BLKID, IRUN, NQQ:', 4I8 )
94015 FORMAT( 'RDELT, DER2MAX, ENQQ: ', 3E15.8 )
94020 FORMAT( 'SMVGEAR: YLOWA reduced too many times. check original',
     &        ' YLOWR' )
94040 FORMAT( 'delt dec to =', E13.5, '; time ', E13.5, ' because ',
     &        'excessive errors' )                         
       END
