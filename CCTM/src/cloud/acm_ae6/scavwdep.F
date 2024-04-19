
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
C $Header: /project/yoj/arc/CCTM/src/cloud/cloud_acm_ae5/scavwdep.F,v 1.7 2011/10/21 16:10:28 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SCAVWDEP ( JDATE, JTIME, WTBAR, WCBAR, TBARC, PBARC,
     &                      CTHK1, AIRM, PRATE1, TAUCLD, POLC, CEND,
     &                      REMOV, REMOVAC, ALFA0, ALFA2, ALFA3 )
C-----------------------------------------------------------------------
C  Description: Compute simplistic incloud scavenging and wet removal
 
C  Revision History:
C      No   Date   Who  What
C      -- -------- ---  -----------------------------------------
C       0 01/15/98 sjr  created program
C       1 07/27/99 sjr  added scavenging coefficient scaling factors
C       3 Dec 00   Jeff move CGRID_MAP into f90 module
C       4 Dec 02   sjr  revised calls to HLCONST
C       5 Jun 10   J.Young: convert for Namelist redesign
C       6 Mar 11   S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                       removed deprecated TRIMLEN
C       6 Mar 11   B.Hutzell: added TRACER string for AE_SPCs to omit while
C                       computing Aitken mode mass
C    1 Dec 15 S.L.Napelenok:  ddm-3d implementaiton for version 5.1.1
 
C  Called by:  RADMCLD and RESCLD
 
C  Calls the following subroutines:  GETALPHA
 
C  Calls the following functions:  HLCONST
 
C  Arguments    Type      I/O       Description
C  ---------   -------  ------  --------------------------------
C    JDATE     integer   input  current model julian date (yyyyddd)
C    JTIME     integer   input  current model time (hhmmss)
C    WTBAR      real     input  avg total water content (kg/m3)
C    WCBAR      real     input  avg liquid water content (kg/m3)
C    TBARC      real     input  avg cloud temperature (K)
C    PBARC      real     input  avg cloud pressure (Pa)
C    CTHK1      real     input  cloud thickness (m)
C    AIRM       real     input  total air mass (moles/m2) in cloudy air
C    PRATE1     real     input  precipitation rate (mm/hr)
C    TAUCLD     real     input  cloud lifetime (s)
C    POLC       real     input  ave vert conc incloud (moles sp/ mole air)
C    CEND       real    output  ending incloud conc (moles/mole)
C    REMOV      real    output  moles/m2 or mm*mol/lit scavenged
C    REMOVAC    real    output  variable storing H+ deposition
C    ALFA0      real    output  scav coef for aitken aerosol number
C    ALFA2      real    output  scav coef for aitken mode sfc area
C    ALFA3      real    output  scav coef for aitken aerosol mass
 
C-----------------------------------------------------------------------

      USE CGRID_SPCS                    ! CGRID mechanism species
      USE UTILIO_DEFN

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX, S_POLC, S_CEND, S_REMOV, S_REMOVAC
#endif 

      IMPLICIT NONE

C...........Includes:

      INCLUDE SUBST_CONST               ! constants

      CHARACTER( 120 ) :: XMSG = ' '    ! Exit status message

C...........Parameters:

      REAL, PARAMETER :: H2ODENS = 1000.0  ! density of water at 20 C
                                           ! and 1 ATM (kg/m3)

C Number of species in CGRID
      INTEGER, SAVE :: MXSPCS

C Number of species scavenged
      INTEGER, SAVE :: N_SPC_SCAV

      REAL, PARAMETER :: TWOTHIRDS = 2.0 / 3.0

      REAL, PARAMETER :: KGPG = 1.0E-03   ! kilograms per gram

C...........Arguments:

      INTEGER, INTENT(  IN ) :: JDATE   ! current model date, coded YYYYDDD
      INTEGER, INTENT(  IN ) :: JTIME   ! current model time, coded HHMMSS
      REAL,    INTENT(  IN ) :: WTBAR   ! total wat cont (kg/m2) int. thru cloud depth
      REAL,    INTENT(  IN ) :: WCBAR   ! liq water content of cloud (kg/m3)
      REAL,    INTENT(  IN ) :: TBARC   ! mean cloud temp (K)
      REAL,    INTENT(  IN ) :: PBARC   ! mean cloud pressure (Pa)
      REAL,    INTENT(  IN ) :: CTHK1   ! aq chem calc cloud thickness
      REAL,    INTENT(  IN ) :: AIRM    ! total air mass (moles/m2) in cloudy air
      REAL,    INTENT(  IN ) :: PRATE1  ! storm rainfall rate (mm/hr)
      REAL,    INTENT(  IN ) :: TAUCLD  ! cloud lifetime
      REAL,    INTENT( OUT ) :: ALFA0   ! scav coef for aitken aerosol number
      REAL,    INTENT( OUT ) :: ALFA2   ! scav coef for aitken aerosol sfc area
      REAL,    INTENT( OUT ) :: ALFA3   ! scav coef for aitken aerosol mass
      REAL,    INTENT( OUT ) :: REMOVAC ! variable storing H+ deposition
      REAL,    INTENT(  IN ) :: POLC ( : )  ! avg vert conc incloud (moles/mole)
      REAL,    INTENT( OUT ) :: CEND ( : )  ! ending incloud conc (moles/mole)
      REAL,    INTENT( OUT ) :: REMOV( : )  ! moles/m2 or mm*mol/lit scavenged

C...........Local Variables:

      LOGICAL, SAVE :: FIRSTIME = .TRUE.   ! flag for first pass thru
      CHARACTER( 16 ), SAVE :: PNAME = 'SCAVWDEP'  ! program name

      CHARACTER( 16 ), ALLOCATABLE, SAVE :: SCAV_SPC( : )  ! CGRID species scavenged
      INTEGER, ALLOCATABLE, SAVE :: SCAV_MAP( : ) ! CGRID map to scavenged spc
      REAL, ALLOCATABLE,    SAVE :: SCAV_FAC( : )  ! CGRID scav coef factors
      LOGICAL, ALLOCATABLE, SAVE :: SCAV_MASK_GAS( : )   ! Mask for Gases on Scav list
      LOGICAL, ALLOCATABLE, SAVE :: SCAV_MASK_AERO( : )  ! Mask for Aerosols on Scav list
      LOGICAL, ALLOCATABLE, SAVE :: SCAV_MASK_AITKEN( : )! Mask for Aitken PM on Scav list
      LOGICAL, ALLOCATABLE, SAVE :: SCAV_MASK_NUM( : )   ! Mask for Aero Number on Scav list
      LOGICAL, ALLOCATABLE, SAVE :: SCAV_MASK_SRF( : )   ! Mask for Aero Surface on Scav list
      LOGICAL, ALLOCATABLE, SAVE :: SCAV_MASK_NR( : )    ! Mask for Non-Reactives on Scav list
      LOGICAL, ALLOCATABLE, SAVE :: SCAV_MASK_TRAC( : )  ! Mask for Tracers on Scav list
      INTEGER, ALLOCATABLE, SAVE :: L_NUMAKN( : ) ! pointers to aitken aerosol #
      INTEGER, ALLOCATABLE, SAVE :: L_MASAKN( : ) ! pointers to aitken aerosols
      INTEGER, ALLOCATABLE, SAVE :: L_SRFAKN( : ) ! pntrs to aitken aerosol surface area
      INTEGER ASTAT

      INTEGER       I
      INTEGER, SAVE :: N_NUMAKN            ! # aitken aerosol number species
      INTEGER, SAVE :: N_MASAKN            ! # aitken aerosol mass species
      INTEGER, SAVE :: N_SRFAKN            ! # aitken aerosol sfc area species
      INTEGER       PNTR                ! relative pointer variable
      INTEGER       SPC                 ! liquid species loop counter
      INTEGER       VAR                 ! variable loop counter

      REAL          ALFA                ! scavenging coefficient (1/s)
      REAL          KH                  ! Henry's law constant (mol/l/atm)
      REAL          NUMAKN              ! Aitken mode aerosol # (#/m3)
      REAL          MASAKN              ! Total Aitken mode mass (ug/m3)
      REAL(8)       MASAKN_D            ! Double-precision verson of MASAKN
      REAL          SRFAKN
      REAL          ONE_OVER_TWASH      ! 1 / TWASH
      REAL,    SAVE :: HPLUS = 1.0E-4   ! typical value hydrogen ion concentration [mol/l]
      REAL          RHOAIR              ! air density in kg/m3
      REAL          RTCH                ! chemical gas const times temp
      REAL          TWASH               ! washout time for clouds (sec) with low liq wat content
      REAL          TWF                 ! washout scaling factor (mol/l/atm)

C...........External Functions:

      INTEGER, EXTERNAL :: INDEXN
      REAL,    EXTERNAL :: HLCONST

C-----------------------------------------------------------------------

C...INITIALIZATION SCAVWDEP module:
C...  event-statistics variables.  Open output files.

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

        N_SPC_SCAV = N_GC_SCAV + N_AE_SCAV + N_NR_SCAV + N_TR_SCAV

C...first check to make sure that some species in CGRID were specified
C...  for scavenging, otherwise notify the user and return

        IF ( N_SPC_SCAV .LE. 0 ) THEN
          XMSG = 'No species were specified for scavenging by cloud ' //
     &           'or rain water...SCAVENGING WILL NOT BE PERFORMED!'
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
          RETURN
        END IF

        ALLOCATE ( SCAV_SPC( N_SPC_SCAV ),
     &             SCAV_MAP( N_SPC_SCAV ),
     &             SCAV_FAC( N_SPC_SCAV ),
     &             SCAV_MASK_GAS( N_SPC_SCAV ),
     &             SCAV_MASK_AERO( N_SPC_SCAV ),
     &             SCAV_MASK_AITKEN( N_SPC_SCAV ),
     &             SCAV_MASK_NUM( N_SPC_SCAV ),
     &             SCAV_MASK_SRF( N_SPC_SCAV ),
     &             SCAV_MASK_NR( N_SPC_SCAV ),
     &             SCAV_MASK_TRAC( N_SPC_SCAV ), STAT = ASTAT )
        IF ( ASTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating SCAV_SPC, SCAV_MAP or SCAV_FAC'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        MXSPCS = N_GC_SPCD + N_AE_SPC + N_NR_SPC + N_TR_SPC

        ALLOCATE ( L_NUMAKN( MXSPCS ),
     &             L_MASAKN( MXSPCS ),
     &             L_SRFAKN( MXSPCS ), STAT = ASTAT )
        IF ( ASTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating L_NUMAKN, L_MASAKN or L_SRFAKN'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...prepare indices for scavenged species
        SCAV_SPC = ''
        SCAV_MAP = 0
        SCAV_FAC = 0
        SCAV_MASK_GAS = .FALSE.
        SCAV_MASK_AERO = .FALSE.
        SCAV_MASK_AITKEN = .FALSE.
        SCAV_MASK_NUM = .FALSE.
        SCAV_MASK_SRF = .FALSE.
        SCAV_MASK_NR = .FALSE.
        SCAV_MASK_TRAC = .FALSE.

C...  load the CGRID to scavenged species pointers for the gases
        SPC = 0
        DO VAR=1, N_GC_SCAV
          SPC = SPC + 1
          SCAV_SPC( SPC ) = GC_SCAV( VAR )
          SCAV_MAP( SPC ) = GC_SCAV_MAP( VAR ) + GC_STRT - 1
          SCAV_FAC( SPC ) = GC_SCAV_FAC( VAR )
          SCAV_MASK_GAS( SPC ) = .TRUE.
        END DO

C...  load the CGRID to scavenged species pointers for the aerosols

        DO VAR=1, N_AE_SCAV
          SPC = SPC + 1
          SCAV_SPC( SPC ) = AE_SCAV( VAR )
          SCAV_MAP( SPC ) = AE_SCAV_MAP( VAR ) + AE_STRT - 1
          SCAV_FAC( SPC ) = AE_SCAV_FAC( VAR )
          IF ( INDEX( SCAV_SPC( SPC ), 'NUM' ) .NE. 0 ) SCAV_MASK_NUM( SPC ) = .TRUE.
          IF ( INDEX( SCAV_SPC( SPC ), 'SRF' ) .NE. 0 ) SCAV_MASK_SRF( SPC ) = .TRUE.
          IF ( INDEX( SCAV_SPC( SPC ), 'AITKEN' ) .NE. 0 ) SCAV_MASK_AITKEN( SPC ) = .TRUE.
          SCAV_MASK_AERO( SPC ) = .TRUE.
        END DO

C...  load the CGRID to scavenged species pointers for the non-reactives

        DO VAR=1, N_NR_SCAV
          SPC = SPC + 1
          SCAV_SPC( SPC ) = NR_SCAV( VAR )
          SCAV_MAP( SPC ) = NR_SCAV_MAP( VAR ) + NR_STRT - 1
          SCAV_FAC( SPC ) = NR_SCAV_FAC( VAR )
          SCAV_MASK_NR( SPC ) = .TRUE.
        END DO

C...  load the CGRID to scavenged species pointers for the tracers

        DO VAR=1, N_TR_SCAV
          SPC = SPC + 1
          SCAV_SPC( SPC ) = TR_SCAV( VAR )
          SCAV_MAP( SPC ) = TR_SCAV_MAP( VAR ) + TR_STRT - 1
          SCAV_FAC( SPC ) = TR_SCAV_FAC( VAR )
          SCAV_MASK_TRAC( SPC ) = .TRUE.
        END DO

C...create the pointers from CGRID to the species needed by AQCHEM
        N_NUMAKN = INDEXN( 'NUM_AITKEN', N_SPC_SCAV, SCAV_SPC, L_NUMAKN )
        N_MASAKN = INDEXN( 'AITKEN    ', N_SPC_SCAV, SCAV_SPC, L_MASAKN )
        N_SRFAKN = INDEXN( 'SRF_AITKEN', N_SPC_SCAV, SCAV_SPC, L_SRFAKN )

      END IF

C...for subsequent calls, check to make sure some species were
C...  specified, otherwise there is no need to perform scavenging

      IF ( N_SPC_SCAV .LE. 0 ) RETURN

      RTCH = ( MOLVOL / STDTEMP ) * TBARC
      TWASH = WTBAR * 1000.0 * CTHK1 * 3600.0
     &      / ( H2ODENS * AMAX1( 1.0E-20, PRATE1 ) )
ccc          TWASH = AMAX1( TWASH, TAUCLD / 60.0 )   ! <different units?sec&min
      TWASH = AMAX1( TWASH, TAUCLD )
      ONE_OVER_TWASH = 1.0 / TWASH
      TWF = H2ODENS / ( WTBAR * RTCH )
      REMOVAC = 0.0

#ifdef sens
      S_REMOVAC = 0.0
#endif

      RHOAIR = ( AIRM / CTHK1 ) * MWAIR * KGPG

C...compute total Aitken mode number (#/m3)

      NUMAKN = 0.0

      DO I = 1, N_NUMAKN
        PNTR = SCAV_MAP( L_NUMAKN( I ) )
        NUMAKN = NUMAKN + ( POLC( PNTR ) * AIRM / CTHK1 )
      END DO

C...compute total Aitken mode mass (ug/m3)

      MASAKN_D = 0.0D0

      DO I = 1, N_MASAKN
        PNTR = SCAV_MAP( L_MASAKN( I ) )
        IF (( INDEX( SCAV_SPC( L_MASAKN( I ) ), 'NUM' ) .EQ. 0 ) .AND.
     &      ( INDEX( SCAV_SPC( L_MASAKN( I ) ), 'SRF' ) .EQ. 0 ) .AND.
     &      ( INDEX( SCAV_SPC( L_MASAKN( I ) ), 'H2O' ) .EQ. 0 ) .AND.
     &      ( INDEX( SCAV_SPC( L_MASAKN( I ) ), 'TRACER' ) .EQ. 0 ) ) THEN
          MASAKN_D = MASAKN_D + REAL( ( POLC( PNTR ) * AIRM / CTHK1
     &           * CGRID_MW( PNTR ) ),8 ) * 1.0D6
        END IF
      END DO

      MASAKN = REAL( MASAKN_D, 4 )

C...compute total Aitken mode surface area (m2/m3)

      SRFAKN = 0.0

      DO I = 1, N_SRFAKN
        PNTR = SCAV_MAP( L_SRFAKN( I ) )
        SRFAKN = SRFAKN + ( POLC( PNTR ) * AIRM / CTHK1 )
      END DO

C...compute the scavenging coefficients for aitken mode aerosol mass
C...  and number
C...  NOTE:  for now, scavenging coefficients are computed for only
C...         the liquid water content, not on the total water content
C...         therefore, no ice phase scavenging is considered at this
C...         time, but it should be added in the future!

      CALL GETALPHA ( NUMAKN, MASAKN, SRFAKN, WCBAR, TBARC, PBARC,
     &                RHOAIR, ALFA0, ALFA2, ALFA3 )

      ! Compute Wet Scavenging Removal
      SPC = 0
      DO SPC = 1, N_SPC_SCAV
        IF ( SCAV_MASK_GAS( SPC ) .OR.
     &       SCAV_MASK_NR( SPC )  .OR.
     &       SCAV_MASK_TRAC( SPC ) ) THEN

          ! Gases, Non-Reactives, and Tracers
          PNTR = SCAV_MAP( SPC )
          KH = HLCONST( SCAV_SPC( SPC ), TBARC, .TRUE., HPLUS )
          IF ( KH .GT. 0.0 ) THEN
            ALFA = SCAV_FAC( SPC ) * ONE_OVER_TWASH / ( 1.0 + TWF / KH )
            CEND ( PNTR ) = POLC( PNTR ) * EXP( -ALFA * TAUCLD )
            REMOV( PNTR ) = ( POLC( PNTR ) - CEND( PNTR ) ) * AIRM
#ifdef sens
            DO NP = 1, NPMAX
              S_CEND ( NP, PNTR ) = S_POLC( NP, PNTR ) * EXP( -ALFA * TAUCLD )
              S_REMOV( NP, PNTR ) = ( S_POLC( NP, PNTR ) - S_CEND( NP, PNTR ) ) * AIRM
            END DO
#endif
          ELSE
            ALFA = 0.0
            CEND ( PNTR ) = POLC( PNTR )
            REMOV( PNTR ) = 0.0
#ifdef sens
            DO NP = 1, NPMAX
              S_CEND( NP, PNTR )  = S_POLC( NP, PNTR )
              S_REMOV( NP, PNTR ) = 0.0
            END DO
#endif
          END IF
        ELSE  
          ! Aerosol scavenging and wet deposition
          PNTR = SCAV_MAP( SPC )
          IF ( SCAV_MASK_AITKEN( SPC ) ) THEN
            ! Aitken Mode Particles
            IF ( SCAV_MASK_NUM( SPC ) ) THEN
              ! Aitken Number
              ALFA = SCAV_FAC( SPC ) * ALFA0 
            ELSE IF ( SCAV_MASK_SRF( SPC ) ) THEN
              ! Aitken Surface Area
              ALFA = SCAV_FAC( SPC ) * ALFA2
            ELSE
              ! Aitken Mass
              ALFA = SCAV_FAC( SPC ) * ALFA3
            END IF
          ELSE
            ! Accumulation and Coarse Number, Surface Area and Mass
            ALFA = SCAV_FAC( SPC ) * ONE_OVER_TWASH
          END IF
      
          CEND ( PNTR ) = POLC( PNTR ) * EXP( -ALFA * TAUCLD )
          REMOV( PNTR ) = ( POLC( PNTR ) - CEND( PNTR ) ) * AIRM
      
#ifdef sens
          DO NP = 1, NPMAX
            S_CEND ( NP,PNTR ) = S_POLC( NP,PNTR ) * EXP( -ALFA * TAUCLD )
            S_REMOV( NP,PNTR ) = ( S_POLC( NP,PNTR ) - S_CEND( NP, PNTR ) ) * AIRM
          ENDDO
#endif
        END IF
      END DO
      RETURN
      END
