! --- Stubs for some CMAQ routines
      REAL FUNCTION HLCONST ( NAME, TEMP, EFFECTIVE, HPLUS )

      IMPLICIT NONE

      CHARACTER*(*) NAME                ! name of substance
      REAL          TEMP                ! temperature (K)
      LOGICAL       EFFECTIVE           ! true=compute the effective henry's law constant
      REAL          HPLUS               ! hydrogen ion concentration (mol/l)

      HLCONST = 0.

      RETURN
      END
!****************************************************************************
      SUBROUTINE AEROPROC( DT, COL, ROW, LAYER, GAMMA_N2O5 )


      IMPLICIT NONE

C *** ARGUMENTS

      REAL,    INTENT( IN ) :: DT          ! synchronization time step, sec
      INTEGER, INTENT( IN ) :: COL         ! Column of cell
      INTEGER, INTENT( IN ) :: ROW         ! Row of cell
      INTEGER, INTENT( IN ) :: LAYER       ! Layer of cell
      REAL,    INTENT( OUT ) :: GAMMA_N2O5 ! N2O5 heterogeneous reaction probab\ility [ ]

      GAMMA_N2O5 = 0.0

      RETURN
      END

      Subroutine getpar( m3_wet_flag, limit_sg  )
C Arguments:
      Logical, Intent( In ) :: m3_wet_flag ! true = include H2O and SOA in 3rd moment
                                           ! false = exclude H2O and SOA from 3rd moment

      Logical, Intent( In ) :: limit_sg  ! fix coarse and accum Sg's to the input value?

      return
      end

      INTEGER FUNCTION INDEXN ( NAME1, N, NAME2, INDICES )

C***********************************************************************
C
C  FUNCTION:
C
C    This routine searches for all occurrences of NAME1 in list NAME2
C
C  REVISION HISTORY:
C
C    5/88   Modified for ROMNET
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C
C      NAME1       Character string being searched for
C      N           Length of array to be searched
C      NAME2       Character array to be searched
C      INDICES     Index array of all occurrences
C
C    Output arguments:
C
C      INDEXN      The number of occurrences of NAME1 within the NAME2
C                  array.  If string was not found, INDEXN = 0
C
C  LOCAL VARIABLE DESCRIPTION:
C
C      None
C
C***********************************************************************

      IMPLICIT NONE

      INTEGER     N
      INTEGER     I
      INTEGER     INDICES(*)

      CHARACTER*(*) NAME1
      CHARACTER*(*) NAME2(*)

C...Assume NAME1 is not in list NAME2    

      INDEXN = 0

      DO I = 1, N

        IF ( INDEX( NAME2( I ), NAME1 ) .GT. 0 ) THEN
          INDEXN = INDEXN + 1
          INDICES( INDEXN ) = I
        END IF

      END DO

      RETURN
      END              
      SUBROUTINE GETALPHA ( NUMI, MASSI, SURFI,
     &                      LWC, T, P, RHOAIR, ALFA0, ALFA2, ALFA3 )
C...........Arguments:

      REAL          NUMI       ! total number concentration for the Aitken mode  [ #/m**3 ]
      REAL          MASSI      ! total mass concentration in AItken mode [ ug/m**3 ]
      REAL          SURFI      ! total surface area in Aitken mode [ m**2 / m**3  ]
      REAL          LWC        ! cloud liquid water content [ kg/m**3 ]
      REAL          T          ! average cloud temperature [ K ]
      REAL          P          ! average cloud pressure [ Pa ]
      REAL          RHOAIR     ! average cloud density [ kg/m**3 ]
      REAL          ALFA0      ! scavenging coefficient for number [ 1/s ]
      REAL          ALFA2      ! scavenging coefficient for surface area [ 1/s ]
      REAL          ALFA3      ! scavenging coefficient for mass [ 1/s ]

      RETURN
      END
      SUBROUTINE PLMRIS( EMLAYS, LSTK, HFX, HMIX,
     &                   STKDM, STKHT, STKTK, STKVE,
     &                   TSTK, USTAR, DTHDZ, TA, WSPD,
     &                   ZF, ZH, ZSTK, WSTK, ZPLM )
C Arguments:
      INTEGER, INTENT( IN )  :: EMLAYS          ! no. of emission layers
      INTEGER, INTENT( IN )  :: LSTK            ! lyr of top of stack, = RADM's K STK
      REAL,    INTENT( IN )  :: HFX             ! sensible heat flux [m K/s]
      REAL,    INTENT( IN )  :: HMIX            ! mixing height [m]
      REAL,    INTENT( IN )  :: STKDM           ! stack diameter [m]
      REAL,    INTENT( IN )  :: STKHT           ! stack height [m]
      REAL,    INTENT( IN )  :: STKTK           ! exhaust temperature [deg K]
      REAL,    INTENT( IN )  :: STKVE           ! exhaust velocity [m/s]
      REAL,    INTENT( IN )  :: TSTK            ! tmptr at top of stack [deg K]
      REAL,    INTENT( IN )  :: USTAR           ! friction velocity [m/s]
      REAL,    INTENT( IN )  :: DTHDZ( EMLAYS ) ! gradient of THETV
      REAL,    INTENT( IN )  :: TA   ( EMLAYS ) ! temperature [deg K]
      REAL,    INTENT( IN )  :: WSPD ( EMLAYS ) ! wind speed [m/s]
      REAL,    INTENT( IN )  :: ZF ( 0:EMLAYS ) ! layer surface height [m]
      REAL,    INTENT( IN )  :: ZH   ( EMLAYS ) ! layer center height [m]
      REAL,    INTENT( IN )  :: ZSTK ( EMLAYS ) ! zf( l ) - stkht [m]
      REAL,    INTENT( INOUT ) :: WSTK          ! wind speed @ top of stack [m/s]
                                                ! OUT for reporting, only
      REAL,    INTENT( OUT ) :: ZPLM            ! temporarily, plume top height

      ZPLM = 0.0

      RETURN
      END

      SUBROUTINE PLSPRD( DTHDZ, ZF, KZ, CEFSTK,        PLTOP, PLBOT )
       REAL,    INTENT ( IN ) :: DTHDZ( KZ ) ! potential temperature lapse rate (K/m)
       REAL,    INTENT ( IN ) :: ZF( 0:KZ )  ! full-layer heights (m)
       INTEGER, INTENT ( IN ) :: KZ          ! number of emissions layers
       REAL,    INTENT ( IN ) :: CEFSTK      ! effective stack height (m)
       REAL,    INTENT( OUT ) :: PLTOP       ! plume top (m)
       REAL,    INTENT( OUT ) :: PLBOT       ! plume bottom (m)

      PLTOP = 0.0
      PLBOT = 0.0

      return
      end
