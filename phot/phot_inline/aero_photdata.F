
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/phot/phot_inline/aero_photdata.F,v 1.2 2011/04/01 15:41:52 sjr Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AERO_PHOTDATA ( COL, ROW, NLAYS, CGRID,
     &                           BLK_AE_VOL, BLK_AE_LSG, BLK_AE_DGN,
     &                           BLK_AE_NR, BLK_AE_NI )

C-----------------------------------------------------------------------
C  FUNCTION:  This subroutine calculates the volume, geometric mean
C     diameters (Dg) and the natural logs of geometric standard
C     deviations (Sg) of all 3 modes.  The code also calculates the
C     average modal aerosol refractive indices
C
C     Internally mixed wet aerosols are assumed.
C
C     NOTE
C       2nd moment concentrations (M2) are passed into GETPAR in the
C       CBLK array.  The GETPAR calculations implicitly assume
C       that the input value of M2 is consistent with the input value of
C       M3_WET_FLAG.  If, for example, the input M2 value was calculated
C       for a "dry" aerosol and the M3_WET_FLAG is .TRUE., GETPAR would
C       incorrectly adjust the M2 concentrations!
C
C  KEY SUBROUTINES/FUNCTIONS CALLED:  EXTRACT_AERO, EXTRACT_SOA, GETPAR
C
C  REVISION HISTORY
C     01/10/11 B.Hutzell - initial version created based on AERO_DESC module
C                          in version 4.7 and aero_depv for version 5.0
C     03/29/11 S.Roselle - Replaced I/O API include files with UTILIO_DEFN
C
C  REFERENCES:
C     Horvath, H., Size segrated light absorption coefficient for
C     the atmospheric aerosol, Atmos. Environ., Vol. 29, No. 8,
C     pp 875-883, 1995.
C
C     Beyer, K.D. A.R. Ravishankara, and E. R. Lovejoy, Meanurements
C     of H2SO4/H2O and H2SO4/HNO3/H2O solutions, J. Geophys. Res. Vol.
C     101, D9, pp 14519-14524, 1996.
C
C-----------------------------------------------------------------------

!     USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN

      USE AERO_DATA           ! aero variable data
      USE SOA_DEFN            ! gas soa data
      USE AEROMET_DATA        ! constants and met data

      IMPLICIT NONE

C Includes:

      INCLUDE SUBST_RXCMMN    ! to get mech name

C Arguments:

      INTEGER, INTENT( IN ) :: COL     ! specified column index
      INTEGER, INTENT( IN ) :: ROW     ! specified row index
      INTEGER, INTENT( IN ) :: NLAYS   ! # of vertical layers
!     REAL, POINTER, INTENT( INOUT ) :: CGRID( :,:,:,: )
      REAL, POINTER         :: CGRID( :,:,:,: )

      REAL, INTENT( OUT ) :: BLK_AE_NR ( :,: )     ! aerosol real part of refractive index
      REAL, INTENT( OUT ) :: BLK_AE_NI ( :,: )     ! aerosol imaginary part of refractive index
      REAL, INTENT( OUT ) :: BLK_AE_VOL( :,: )     ! aerosol modal volumes
      REAL, INTENT( OUT ) :: BLK_AE_LSG( :,: )     ! aerosol modal log of geo. stand. dev
      REAL, INTENT( OUT ) :: BLK_AE_DGN( :,: )     ! aerosol modal mean geometeric diam. [ m ]

C Parameters:

      TYPE MODAL_COMPLEX
         CHARACTER( 16 ) :: NAME      ! name of complex property
         REAL            :: REAL_PART( N_MODE ) ! real part
         REAL            :: IMAG_PART( N_MODE ) ! imaginary part
      END TYPE MODAL_COMPLEX

      REAL, PARAMETER :: NUM_REFRACTIVE = 5

      TYPE( MODAL_COMPLEX ), PARAMETER :: REFRACTIVE_INDEX( NUM_REFRACTIVE ) = (/
C *** FSB Water soluble values are modeled on data from Beyer et al. (1996)
C                                        Real_Part             Imag_Part
C                                    ------------------     ------------------
C                         Name         I     J     K         I     J     K
C                        --------    ------------------     ------------------
     &   MODAL_COMPLEX( 'SOLUTE  ', (/1.50, 1.50, 1.50/), (/0.00, 0.00, 0.00/)),
     &   MODAL_COMPLEX( 'SEASALT ', (/1.50, 1.50, 1.50/), (/0.00, 0.00, 0.00/)),
     &   MODAL_COMPLEX( 'DUST    ', (/1.53, 1.53, 1.53/), (/0.01, 0.01, 0.01/)),
     &   MODAL_COMPLEX( 'WATER   ', (/1.34, 1.34, 1.34/), (/0.00, 0.00, 0.00/)),
C *** FSB The following values for soot like particles are modified from
C         Table 1 of Horvath (1995). They are size dependent effective
C         refractive indices fit to data on absorption by carbon
     &   MODAL_COMPLEX( 'SOOT    ', (/1.53, 1.53, 0.00/), (/0.14, 0.05, 0.00/))/)

C Local saved variables:

      CHARACTER( 16 ), SAVE :: AE_VRSN                  ! Aerosol version name
      CHARACTER( 16 ), SAVE :: PNAME = 'AERO_PHOTDATA'  ! routine name

      LOGICAL, SAVE :: FIRSTIME = .TRUE.                ! first pass flag

      INTEGER, SAVE :: LOGDEV                           ! unit number for the log file
      INTEGER, SAVE :: REFRACT_INDX_MAP( N_AEROSPC )    ! map array for refactive index

C Local variables:

      CHARACTER( 16 ) :: VNAME            ! varable name
      CHARACTER( 96 ) :: XMSG = ' '

      REAL      :: FACTOR
      REAL( 8 ) :: SUMRI_R
      REAL( 8 ) :: SUMRI_I
      REAL      :: INDX_REFRACT_REAL( N_MODE )
      REAL      :: INDX_REFRACT_IMAG( N_MODE )

      INTEGER L, V, N, M            ! loop counters
      INTEGER SPC                   ! species loop counter

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.
         LOGDEV = INIT3()

C ***    Create map for refactive index for each species
         DO SPC = 1, N_AEROSPC
            DO M = 1, NUM_REFRACTIVE
               IF ( AEROSPC( SPC )%OPTIC_SURR .EQ. REFRACTIVE_INDEX( M )%NAME ) THEN
                  REFRACT_INDX_MAP( SPC ) = M
                  EXIT
               END IF
            END DO
            IF ( REFRACT_INDX_MAP( SPC ) .LT. 1 ) THEN
               DO M = 1, N_MODE
                  IF ( AEROSPC( SPC )%NAME( M )(1:1) .EQ. ' ' ) CYCLE
                  XMSG = 'Species ' // TRIM( AEROSPC( SPC )%NAME( M ) )
     &                 // ' does not use defined refractive index.'
                  CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT3 )
               END DO
            END IF
         END DO

      END IF    ! FIRSTIME

      IF ( N_AE_SPC .LE. 0 ) RETURN

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Put the grid cell physical data in the block arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DO L = 1, NLAYS

C ***    extract grid cell concentrations of aero species from CGRID
C        into aerospc_conc in aero_data module
         CALL EXTRACT_AERO ( CGRID( COL,ROW,L,: ), .TRUE. )

C ***    extract soa concentrations from CGRID
         CALL EXTRACT_SOA ( CGRID( COL,ROW,L,: ) )

C ***    Calculate aerosol surface area to 2nd moment.
         DO M = 1, N_MODE
            N = AEROSRF_MAP( M )
            MOMENT2_CONC( M ) = CGRID( COL,ROW,L,N ) / SNGL( PI )
         END DO

C ***    Calculate geometric mean diameters and standard deviations of the
C        "dry" size distribution
         CALL GETPAR ( .TRUE., .FALSE. )

         DO N = 1, N_MODE
            SUMRI_R = 0.0D+0
            SUMRI_I = 0.0D+0

            DO SPC = 1, N_AEROSPC
               IF ( AEROSPC( SPC )%TRACER ) CYCLE              ! neglect tracer species
               IF ( AEROSPC( SPC )%NAME( N ) .EQ. ' ' ) CYCLE  ! neglect undefined species

               FACTOR = 1.0E-9 * F6DPI / AEROSPC( SPC )%DENSITY
               M = REFRACT_INDX_MAP( SPC )
               SUMRI_R = SUMRI_R + FACTOR
     &                 * AEROSPC_CONC( SPC,N )
     &                 * REFRACTIVE_INDEX( M )%REAL_PART( N )
               SUMRI_I = SUMRI_I + FACTOR
     &                 * AEROSPC_CONC( SPC,N )
     &                 * REFRACTIVE_INDEX( M )%IMAG_PART( N )
            END DO  ! species loop

C ***       Update output array values
            BLK_AE_NR ( L, N ) = SUMRI_R / MOMENT3_CONC( N )
            BLK_AE_NI ( L, N ) = SUMRI_I / MOMENT3_CONC( N )
            BLK_AE_LSG( L, N ) = AEROMODE_SDEV( N )
            BLK_AE_DGN( L, N ) = AEROMODE_DIAM( N )
            BLK_AE_VOL( L, N ) = PI6 * MOMENT3_CONC( N )

         END DO   ! mode loop

      END DO   ! layer LOOP

      RETURN
      END SUBROUTINE AERO_PHOTDATA
