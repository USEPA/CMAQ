
!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/subgrid.F,v 1.9 2011/10/29 01:03:56 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)subgrid.F 1.2 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.subgrid.F 04 Jun 1997 10:48:24

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SUBGRID ( NWL, STWL, MIDWL, ENDWL, CS, CSZ, QY, QYZ,
     &                     AIR, HAIR, VAIR, CVO2, O3ABS, O3, HO3, VO3,
     &                     AO3, IBASE, ITOP, CLOUD, NSUBKM, VCLD,
     &                     AER, VAER, HAER, T, VT, Z, ZMID,
     &                     NLAYS, NLEVS )
        
C*********************************************************************
C
C  This subroutine computes all altitude dependent quantities over the
C       sub-divided grid
C  The following quantities are computed at each LEVEL (altitude)
C       Z   (ILEV) = altitude (km) of each level
C       ZAIR(ILEV) = air concentration at each level
C       ZT  (ILEV) = temperature at each level
C       CSZ(ILEV,IWL,IPHOT) = abs. cross sect at each altitude (T,P corrected)
C       QYZ(ILEV,IWL,IPHOT) = quantum yield at each altitude (T,P corrected)
C
C  The following quantities are computed at each LAYER (thickness)
C       ZMID(ILAY)     = altitude of modpoint of layer
C       VAIR(ILAY)     = air column in layer (vertical)
C       VO3 (ILAY)     = ozone column   "       "
C       VAER(ILAY)     = aerosol "      "       '
C       VCLD(ILAY)     = cloud   "      "       "
C       VT  (ILAY)     = average temperature of column
C       AO3 (ILAY,IWL) = average O3 cross sect in layer (with ave. layer
C
C*********************************************************************

      USE RXNS_DATA

      IMPLICIT NONE

      INCLUDE 'JVALPARMS.EXT'    ! jproc parameters

C...........PARAMETERS and their descriptions
      
      INTEGER, PARAMETER :: NWLO3 = 29  ! number of wl bands for O3 T cor

C...........ARGUMENTS and their descriptions

      INTEGER      NWL                ! number of wavelength bands
      INTEGER      NSUBKM             ! cloud sublayers/km
      INTEGER      IBASE              ! cloud base index
      INTEGER      ITOP               ! cloud top index
      INTEGER      NLAYS              ! total # of atm layers
      INTEGER      NLEVS              ! number of levels

      REAL         HAIR               ! air scale height
      REAL         HO3                ! ozone scale height
      REAL         HAER               ! aerosol scale ht at atm top
      REAL         CLOUD( 48 )        ! cloud optical depth profile
      REAL         T  ( MXLEV )       ! interpolated temp profile
      REAL         AER( MXLEV )       ! aerosol attenuation profile
      REAL         O3 ( MXLEV )       ! ozone profile
      REAL         AIR( MXLEV )       ! interpolated air profile
      REAL         STWL( MXWL )       ! wavelength band starting point
      REAL         MIDWL( MXWL )      ! wavelength band midpoints
      REAL         ENDWL( MXWL )      ! wavelength band ending point
      REAL         O3ABS( MXWL )      ! O3 absorption cross section
      REAL         VT  ( NJ )         ! average temp of column
      REAL         Z   ( NJ )         ! altitude of each level
      REAL         ZMID( NJ )         ! altitude of midpoint of layer
      REAL         VAER( NJ )         ! aerosol column in layer
      REAL         VCLD( NJ )         ! cloud column in layer
      REAL         VO3 ( NJ )         ! ozone column in layer
      REAL         VAIR( NJ )         ! air column in layer
      REAL         CVO2( NJ )         ! vertical column O2
      REAL         AO3( NJ, MXWL )    ! average O3 cross sect in layer
      REAL         CS( MXWL, NPHOTAB )  ! cross sections
      REAL         QY( MXWL, NPHOTAB )  ! quantum yields
      REAL         CSZ( 100, MXWL, NPHOTAB ) ! cross section at each level
      REAL         QYZ( 100, MXWL, NPHOTAB ) ! quantum yields T&P corrected

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(8), SAVE :: SRO1D = '        ' ! source for o1d data
      CHARACTER(1), SAVE :: TYPE = 'B' ! cs spectra type (B=beginning wl)

      LOGICAL, SAVE :: FIRSTIME = .TRUE. ! Flag for first call to SUBGRID

      INTEGER      ILEV               ! level index
      INTEGER      I                  ! index
      INTEGER      K                  ! level index
      INTEGER      ILAY               ! layer index
      INTEGER      IWL                ! wavelength index
      INTEGER      II                 ! layer index
      INTEGER      IPHOT              ! reaction index
      INTEGER, SAVE :: IO3O1D             ! O3O1D reaction index 
      INTEGER, SAVE :: IO3O3P             ! O3O3P reaction index
      INTEGER, SAVE :: IHCHOM             ! HCHOM reaction index
      INTEGER, SAVE :: IHCHOR             ! HCHOR reaction index
      INTEGER, SAVE :: IALD               ! ALD reaction index
      INTEGER, SAVE :: IACETONE           ! ACETONE reaction index
      INTEGER, SAVE :: IKETONE            ! KETONE reaction index
      INTEGER, SAVE :: IGLYF              ! GLYF reaction index
      INTEGER, SAVE :: IGLYM              ! GLYM reaction index
      INTEGER, SAVE :: IGLY_R             ! GLY_R reaction index
      INTEGER, SAVE :: IMGLY              ! MGLY reaction index
      INTEGER      IWLO3              ! index for wl bands for O3 t cor

      REAL         DZ                 ! atmosphere depth
      REAL         DZCLD              ! cloud thickness
      REAL         HLOCAL             ! local air scale
      REAL         HLOCA1             ! local air scale
      REAL         HLOCA2             ! local aerosol scale
      REAL         X0                 ! height fraction
      REAL         X1                 ! height above fraction
      REAL         AKT                ! temp corrected form diff
      REAL         AK300              ! diff in qy(2) for form
      REAL         PHI20              ! qy for form react 2 diff
      REAL         SUM                ! total aerosol opt depth
      REAL         DZ1                ! x0 - dz
      REAL         DZ2                ! x0 + dz
      REAL         TDIFFX             ! temp diff for o3 c-s
      REAL         PHI1               ! qy for form react 1
      REAL         PHI2               ! qy for form react 2
      REAL         C                  ! coef 3 for o3 qy
      REAL         A                  ! coef 1 for o3 qy
      REAL         B                  ! coef 2 for o3 qy
      REAL         TN1                ! temp diff for o3 qy nasa94
      REAL         TR1                ! temp diff for o3 qy radm
      REAL         XL                 ! wavelength
      REAL         XL0                ! wavelength
      REAL         DXL                ! wl delta from 305nm
      REAL         A0                 ! 1st coef for O3 qy
      REAL         A1                 ! 2nd coef for O3 qy
      REAL         A2                 ! 3rd coef for O3 qy
      REAL         A3                 ! 4th coef for O3 qy
      REAL         A4                 ! 5th coef for O3 qy
      REAL         A5                 ! 6th coef for O3 qy
      REAL         A6                 ! 7th coef for O3 qy

C...coefficients for JPL06-2 and IUPAC04 O3O1D QY temperature adjustment

      REAL, SAVE :: jA1 = 0.8036        
      REAL, SAVE :: jA2 = 8.9061        
      REAL, SAVE :: jA3 = 0.1192        
      REAL, SAVE :: jX1 = 304.225 ! nm  
      REAL, SAVE :: jX2 = 314.957 ! nm  
      REAL, SAVE :: jX3 = 310.737 ! nm  
      REAL, SAVE :: jw1 = 5.576 ! nm    
      REAL, SAVE :: jw2 = 6.601 ! nm    
      REAL, SAVE :: jw3 = 2.187 ! nm    
      REAL, SAVE :: jv1 = 0.0 ! cm-1    
      REAL, SAVE :: jv2 = 825.518 ! cm-1
      REAL, SAVE :: jc = 0.0765        
      REAL, SAVE :: jR = 0.695 ! cm-1/K
      REAL          jq1, jq2, jq112, jq212
      REAL          jTEMP, jT300

C...

      REAL         ZAIR( NJ )         ! air concentration profile
      REAL         ZT  ( NJ )         ! temperature profile

      REAL         CSO3W1( NWLO3 )    ! temp O3 CS on t coef wl's
      REAL         CSO3W2( MXWL )     ! temp O3 CS on ET's wls
      REAL         WLO3L( NWLO3+1 )   ! lower wl's for O3 c-s temp coef.
      DATA         WLO3L /
     &       263.158,   266.667,   270.270,   273.973,   277.778,
     &       281.690,   285.714,   289.855,   294.118,   298.500,
     &       302.500,   303.500,   304.500,   305.500,   306.500,
     &       307.500,   308.500,   309.500,   310.500,   311.500,
     &       312.500,   313.500,   314.500,   317.500,   322.500,
     &       327.500,   332.500,   337.500,   342.500,   347.500 /
      SAVE         WLO3L

      REAL         SO3TX( NWLO3, 3 )  ! O3 c-s temperature cooef.
      DATA ( SO3TX( IWLO3, 1 ), IWLO3 = 1, NWLO3 ) /
     &       9.630E+00, 8.320E+00, 6.880E+00, 5.370E+00, 3.960E+00,
     &       2.710E+00, 1.750E+00, 1.060E+00, 5.960E-01, 3.330E-01,
     &       2.400E-01, 2.100E-01, 1.800E-01, 1.600E-01, 1.400E-01,
     &       1.200E-01, 1.050E-01, 9.000E-02, 8.000E-02, 7.000E-02,
     &       6.000E-02, 5.500E-02, 4.000E-02, 2.190E-02, 1.010E-02,
     &       5.080E-03, 2.120E-03, 8.290E-04, 2.940E-04 /

      DATA ( SO3TX( IWLO3, 2 ), IWLO3 = 1, NWLO3 ) /
     &       1.190E-03, 3.640E-04, 2.460E-04, 1.030E-03, 1.690E-03,
     &       1.450E-03, 8.940E-04, 7.830E-04, 4.940E-04, 3.550E-04,
     &       2.950E-04, 2.750E-04, 2.500E-04, 2.300E-04, 2.080E-04,
     &       1.860E-04, 1.640E-04, 1.450E-04, 1.280E-04, 1.121E-04,
     &       1.000E-04, 9.200E-05, 7.500E-05, 4.830E-05, 3.430E-05,
     &       1.820E-05, 8.850E-06, 4.270E-06, 5.300E-06 /

      DATA ( SO3TX( IWLO3, 3 ), IWLO3 = 1, NWLO3 ) /
     &      -1.740E-05, 2.470E-06, 1.170E-05, 1.260E-06,-6.860E-06,
     &      -2.890E-06, 3.590E-06, 2.000E-06, 3.660E-06, 2.600E-06,
     &       2.170E-06, 1.950E-06, 1.380E-06, 1.650E-06, 1.550E-06,
     &       1.460E-06, 1.340E-06, 1.210E-06, 1.130E-06, 1.060E-06,
     &       9.400E-07, 8.700E-07, 7.500E-07, 5.200E-07, 2.660E-07,
     &       1.630E-07, 1.260E-07, 8.710E-08, 3.500E-08 /
      SAVE         SO3TX

C...........EXTERNAL FUNCTIONS and their descriptions:

      INTEGER      INDEX2              ! index of array for a string

C*********************************************************************
C     begin body of subroutine SUBGRID2

      IF ( FIRSTIME ) THEN

C... Setup indices for reactions

        IO3O1D  = INDEX2( 'O3O1D',   NPHOTAB, PHOTAB )
        IF ( IO3O1D .EQ. 0 ) IO3O1D  = INDEX2( 'O3_O1D', NPHOTAB, PHOTAB )

        IO3O3P  = INDEX2( 'O3O3P',   NPHOTAB, PHOTAB )
        IF ( IO3O3P .EQ. 0 ) IO3O3P  = INDEX2( 'O3_O3P', NPHOTAB, PHOTAB )

        IHCHOM  = INDEX2( 'HCHOM', NPHOTAB, PHOTAB )
        IF ( IHCHOM .EQ. 0 ) IHCHOM  = INDEX2( 'HCHO_M', NPHOTAB, PHOTAB )
        IF ( IHCHOM .EQ. 0 ) IHCHOM  = INDEX2( 'FORM_M', NPHOTAB, PHOTAB )

        IHCHOR  = INDEX2( 'HCHOR', NPHOTAB, PHOTAB )
        IF ( IHCHOR .EQ. 0 ) IHCHOR  = INDEX2( 'HCHO_R', NPHOTAB, PHOTAB )
        IF ( IHCHOR .EQ. 0 ) IHCHOR  = INDEX2( 'FORM_R', NPHOTAB, PHOTAB )

        IALD    = INDEX2( 'ALD',     NPHOTAB, PHOTAB )
        IF (IALD .EQ. 0) IALD = INDEX2( 'CCHO',     NPHOTAB, PHOTAB )

        IACETONE= INDEX2( 'ACET_06', NPHOTAB, PHOTAB )
        IF (IACETONE .EQ. 0) IACETONE = INDEX2( 'CH3COCH3',     NPHOTAB, PHOTAB )

        IKETONE = INDEX2( 'KET',  NPHOTAB, PHOTAB )

        IGLYF   = INDEX2( 'GLYF',  NPHOTAB, PHOTAB )

        IGLYM   = INDEX2( 'GLY_07M',  NPHOTAB, PHOTAB )
        IF( IGLYM .EQ. 0 ) IGLYM   = INDEX2( 'GLYH2',  NPHOTAB, PHOTAB )

        IGLY_R  = INDEX2( 'GLY_07R',   NPHOTAB, PHOTAB )
        IF(  IGLY_R .EQ. 0 ) IGLY_R  = INDEX2( 'GLYHX',   NPHOTAB, PHOTAB )
  
       IMGLY   = INDEX2( 'MGLY',    NPHOTAB, PHOTAB )
        
        write(6,*) 'Photolysis pressure/temperature dependencies:'
        write(6,*) '0: disabled; >0 enabled'
        write(6,*) ''
        write(6,*) 'O3 O3P', IO3O3P
        write(6,*) 'O3 O1D', IO3O1D
        write(6,*) 'formaldehyde 2*HO2:', IHCHOR
        write(6,*) 'formaldehyde H2', IHCHOM
        write(6,*) 'Acetaldehyde:', IALD
        write(6,*) 'Acetone:', IACETONE
        write(6,*) 'Ketone:', IKETONE
        write(6,*) 'Glyoxal formaldehyde:', IGLYF
        write(6,*) 'Glyoxal molecular', IGLYM
        write(6,*) 'Glyoxal radical', IGLY_R
        write(6,*) 'Methyl glyoxal', IMGLY

C...check to see if O1D is from IUPAC, NASA94, or RADM data

        IF ( IO3O1D .GT. 0 ) THEN
          IF ( INDEX( PHOTAB( IO3O1D ), 'IUPAC04' ) .GT. 0 ) THEN
            SRO1D = 'IUPAC04'
          ELSE IF ( INDEX( PHOTAB( IO3O1D ), 'NASA94' ) .GT. 0 ) THEN
            SRO1D = 'NASA94'
          ELSE IF ( INDEX( PHOTAB( IO3O1D ), 'JPL06-2' ) .GT. 0 ) THEN
            SRO1D = 'JPL06-2'
          ELSE IF ( INDEX( PHOTAB( IO3O1D ), 'RADM' ) .GT. 0 ) THEN
            SRO1D = 'RADM'
          ELSE
            SRO1D = 'JPL06-2'
          END IF
        END IF

        FIRSTIME = .FALSE.
        
      END IF
      
      DZ = 1.0E+05
      DZCLD = 1.0E+05 / FLOAT( NSUBKM )

C...levels

      ILEV = 0
      DO I = 1, IBASE - 1
        ILEV = ILEV + 1
        Z   ( ILEV ) = FLOAT( I - 1 )
        ZAIR( ILEV ) = AIR( I )
        ZT  ( ILEV ) = T( I )
      END DO

      DO I = IBASE, ITOP-1
        HLOCAL = 1.0 / ALOG( AIR( I ) / AIR( I + 1 ) )
        DO K = 1, NSUBKM
          X0 = FLOAT( K - 1 ) * DZCLD / DZ
          X1 = 1.0 - X0
          ILEV = ILEV + 1
          Z   ( ILEV ) = FLOAT( I - 1 ) + X0
          ZAIR( ILEV ) = AIR( I ) * EXP( -X0 / HLOCAL )
          ZT  ( ILEV ) = X0 * T( I + 1 ) + X1 * T( I )
        END DO
      END DO

      DO I = ITOP, 51
        ILEV = ILEV + 1
        Z   ( ILEV ) = FLOAT( I - 1 )
        ZAIR( ILEV ) = AIR( I )
        ZT  ( ILEV ) = T( I )
      END DO

      Z   ( NLEVS ) = 51.0
      ZAIR( NLEVS ) = AIR( 51 ) * EXP( -1.0 / HAIR )
      ZT  ( NLEVS ) = T( 51 ) + ( T( 51 ) - T( 50 ) )

C...assign default yields

      DO ILEV = 1, NLEVS
        DO IWL = 1, NWL
          DO IPHOT = 1, NPHOTAB
            QYZ( ILEV, IWL, IPHOT ) = QY( IWL, IPHOT )
            CSZ( ILEV, IWL, IPHOT ) = CS( IWL, IPHOT )
          END DO
        END DO
      END DO

C...correct absorption cross sections for T and P

      IF ( ( IO3O1D .GT. 0 ) .AND. ( IO3O3P .GT. 0 ) ) THEN
        DO ILEV = 1, NLEVS   ! level loop

C...compute O3 cross sections for wavelength bands corresponding
C...  to the reference data for the temperature coefficients

          TDIFFX = ZT( ILEV ) - 230.0
          DO IWLO3 = 1, NWLO3        ! wavelength loop
            CSO3W1( IWLO3 ) = 1.0E-18 * ( SO3TX( IWLO3, 1 )
     &                      + SO3TX( IWLO3, 2 ) * TDIFFX
     &                      + SO3TX( IWLO3, 3 ) * TDIFFX * TDIFFX )
          END DO   ! wavelength loop

C...now transfrom the computed O3 cross sections from their
C...  reference wavelength bands to the same wavelength bands
C...  as the extraterrestrial irradiance data

          CALL INTAVG ( WLO3L, CSO3W1, NWLO3+1, TYPE,
     &                  STWL, ENDWL, CSO3W2, NWL )

C...replace the O3 cross sections data only within the wavelengths
C...  which are sensitive to temperature changes

          DO IWL = 1, NWL            ! wavelength loop
            IF ( ( STWL( IWL ) .GE. 263.158 ) .AND. 
     &           ( ENDWL( IWL ) .LE. 347.5 ) ) THEN
              CSZ( ILEV, IWL, IO3O1D ) = CSO3W2( IWL )
              CSZ( ILEV, IWL, IO3O3P ) = CSZ( ILEV, IWL, IO3O1D )
            END IF
          END DO    ! wavelength loop
        END DO    ! level loop

      END IF

C...Adjust quantum yields for temperature dependencies

C...O3O1D:  jpl06-2 and iupac04
        
      IF ( ( SRO1D .EQ. 'IUPAC04' ) .OR. ( SRO1D .EQ. 'JPL06-2' ) ) THEN

        DO ILEV = 1, NLEVS   ! level loop
          jTEMP = MIN( 320.0, MAX( 200.0, ZT( ILEV ) ) )
          jq1 = EXP( -jv1 / ( jR * jTEMP ) )
          jq2 = EXP( -jv2 / ( jR * jTEMP ) )
          jq112 = jq1 / ( jq1 + jq2 )
          jq212 = jq2 / ( jq1 + jq2 )
          jT300 = jTEMP / 300

          DO IWL = 1, NWL   ! wavelength loop
            XL = MIDWL( IWL )
            IF ( ( XL .GE. 305.0 ) .AND. ( XL .LE. 328.0 ) ) THEN
              QYZ( ILEV, IWL, IO3O1D ) = jq112 * jA1
     &                                 * EXP( -( ( jX1 - XL ) / jw1 )**4 )
     &                                 + jq212 * jA2 * jT300**2
     &                                 * EXP( -( ( jX2 - XL ) / jw2 )**2 )
     &                                 + jA3 * jT300**1.5
     &                                 * EXP( -( ( jX3 - XL ) / jw3 )**2 )
     &                                 + jC
            END IF
          END DO   ! wavelength loop

        END DO   ! level loop
      END IF   ! jpl06-2 and iupac04
        
C...O3O1D:  radm

      IF ( SRO1D .EQ. 'RADM' ) THEN
        DO ILEV = 1, NLEVS        ! level loop

          TR1 = ZT( ILEV ) - 230.0
          A = 0.9 * ( 0.369
     &              + 2.85E-4 * TR1
     &              + 1.28E-5 * TR1 * TR1
     &              + 2.57E-8 * TR1 * TR1 * TR1 )
          B =       - 0.575
     &              + 5.59E-3  * TR1
     &              - 1.439E-5 * TR1 * TR1
     &              - 3.27E-8  * TR1 * TR1 * TR1
          C = 0.9 * ( 0.518
     &              + 9.87E-4 * TR1
     &              - 3.94E-5 * TR1 * TR1
     &              + 3.91E-7 * TR1 * TR1 * TR1 )
          XL0 =       308.20
     &              + 4.4871E-2 * TR1
     &              + 6.9380E-5 * TR1 * TR1
     &              - 2.5452E-6 * TR1 * TR1 * TR1

          DO IWL = 1, NWL        ! wavelength loop
            XL = MIDWL( IWL )
            QYZ( ILEV, IWL, IO3O1D ) = A * ATAN( B * ( XL - XL0 ) ) + C
            IF ( QYZ( ILEV, IWL, IO3O1D ) .LT. 0.0 ) THEN
              QYZ( ILEV, IWL, IO3O1D ) = 0.0
            ELSE IF ( QYZ( ILEV, IWL, IO3O1D ) .GT. 0.9 ) THEN
              QYZ( ILEV, IWL, IO3O1D ) = 0.9
            END IF
          END DO   ! wavelength loop

        END DO   ! level loop
      END IF  ! radm o3o1d

C...O3O1D:  nasa94
        
      IF ( SRO1D .EQ. 'NASA94' ) THEN
        DO ILEV = 1, NLEVS        ! level loop

          TN1 = 298.0 - ZT( ILEV )
          A0 =  0.94932   - 1.7039E-4 * TN1 + 1.4072E-6 * TN1 * TN1
          A1 = -2.4052E-2 + 1.0479E-3 * TN1 - 1.0655E-5 * TN1 * TN1
          A2 =  1.8771E-2 - 3.6401E-4 * TN1 - 1.8587E-5 * TN1 * TN1
          A3 = -1.454E-2  - 4.7787E-5 * TN1 + 8.1277E-6 * TN1 * TN1
          A4 =  2.3287E-3 + 1.9891E-5 * TN1 - 1.1801E-6 * TN1 * TN1
          A5 = -1.4471E-4 - 1.7188E-6 * TN1 + 7.2661E-8 * TN1 * TN1
          A6 =  3.183E-6  + 4.6209E-8 * TN1 - 1.6266E-9 * TN1 * TN1

          DO IWL = 1, NWL        ! wavelength loop
            XL = MIDWL( IWL )
            IF ( XL .LT. 290.0 ) THEN
              QYZ( ILEV, IWL, IO3O1D ) = 0.9
            ELSE IF ( ( XL .GE. 290.0 ) .AND. ( XL .LT. 305.0 ) ) THEN
              QYZ( ILEV, IWL, IO3O1D ) = 0.95
            ELSE IF ( ( XL .GE. 305.0 ) .AND. ( XL .LE. 320.0 ) ) THEN
              DXL = MIDWL( IWL ) - 305.0
              QYZ( ILEV, IWL, IO3O1D ) = A0          + A1 * DXL
     &                                 + A2 * DXL**2 + A3 * DXL**3
     &                                 + A4 * DXL**4 + A5 * DXL**5
     &                                 + A6 * DXL**6
              IF ( QYZ( ILEV, IWL, IO3O1D ) .LT. 0.02 ) THEN
                QYZ( ILEV, IWL, IO3O1D ) = 0.0
              END IF
            ELSE IF ( XL .GT. 320.0 ) THEN
              QYZ( ILEV, IWL, IO3O1D ) = 0.0
            END IF
          END DO   ! wavelength loop

        END DO   ! level loop
      END IF   ! nasa94 o3o1d
        
C...O3O3P
C...  The yields for O3->O(3P) are calculated as:  (1.- singlet D yield)

      IF ( ( IO3O3P .GT. 0 ) .AND. ( IO3O1D .GT. 0 ) ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            QYZ( ILEV, IWL, IO3O3P ) = 1.0 - QYZ( ILEV, IWL, IO3O1D )
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...CH2O formaldehyde:
C...  the CH2O yield recalculated only for wavelengths longer than 329 nm

      IF ( ( IHCHOM .GT. 0 ) .AND. ( IHCHOR .GT. 0 ) ) THEN
          
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            IF ( ( XL .GE. 330.0 ) .AND.
     &           ( QYZ( ILEV, IWL, IHCHOM ) .GT. 0.0 ) ) THEN
              PHI1 = QYZ( ILEV, IWL, IHCHOR )
              PHI2 = QYZ( ILEV, IWL, IHCHOM )
              PHI20 = 1.0 - PHI1
              AK300 = ( ( 1.0 / PHI2 ) - ( 1.0 / PHI20 ) ) / 2.54E+19
              AKT = AK300 * ( 1.0 + 61.69 * ( 1.0 - ZT( ILEV ) / 300.0 )
     &            * ( XL / 329.0 - 1.0 ) )
              QYZ( ILEV, IWL, IHCHOM ) = 1.0 / ( ( 1.0 / PHI20 )
     &                                 + ZAIR( ILEV ) * AKT )
            END IF
            
            IF ( QYZ( ILEV, IWL, IHCHOM ) .GT. 1.0 ) THEN
              QYZ( ILEV, IWL, IHCHOM ) = 1.0
            ELSE IF ( QYZ( ILEV, IWL, IHCHOM ) .LT. 0.0 ) THEN
              QYZ( ILEV, IWL, IHCHOM ) = 0.0
            END IF
          END DO   ! wavelength loop
        END DO   ! level loop

      END IF

C...CH3CHO and the dicarbonyls yields are calculated
C...  from the NTP yield by linear adjustment to 1/yield.  
C...Ketones yield is calculated from fit equations


C...CH3CHO acetaldehyde:

      IF ( IALD .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            IF ( QY( IWL, IALD ) .NE. 0.0 ) THEN
              QYZ( ILEV, IWL, IALD ) = 1.0 /
     &                               ( 1.0 + (1.0 / QY( IWL, IALD )
     &                               - 1.0 ) * ZAIR( ILEV ) / 2.465E19 )
            END IF
          END DO   ! wavelength loop
        END DO   ! level loop

      END IF

C...ACETONE + HV -> CH3CO. + CH3.

      IF ( IACETONE .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            QYZ( ILEV, IWL, IACETONE ) = 0.0766 + 0.09415
     &                                 * EXP( -ZAIR( ILEV ) / 3.222E18 )
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...CH3COC2H5 + hv -> ACO3 + ETH

      IF ( IKETONE .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            QYZ( ILEV, IWL, IKETONE ) = 0.0766 + 0.09415
     &                                * EXP( -ZAIR( ILEV ) / 3.222E18 )
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...HCOCHO  glyoxal PROCESS A:

      IF ( IGLYF .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            IF ( QY( IWL, IGLYF ) .NE. 0.0 ) THEN
              QYZ( ILEV, IWL, IGLYF ) = 1.0 / ( 1.0
     &                                + (1.0 / QY( IWL, IGLYF ) - 1.0 )
     &                                * ZAIR( ILEV ) / 2.465E19 )
            END IF
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...HCOCHO  glyoxal PROCESS B:

      IF ( IGLYM .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            IF ( QY( IWL, IGLYM ) .NE. 0.0 ) THEN
              QYZ( ILEV, IWL, IGLYM ) = 1.0 / ( 1.0 
     &                                + (1.0 / QY( IWL, IGLYM ) - 1.0 )
     &                                * ZAIR( ILEV ) / 2.465E19 )
            END IF
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...Glyoxal + hv = 2 HCO

      IF ( IGLY_R .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            IF ( QY( IWL, IGLY_R ) .NE. 0.0 ) THEN
              QYZ( ILEV, IWL, IGLY_R ) = 1.0 / ( 1.0 
     &                                + (1.0 / QY( IWL, IGLY_R ) - 1.0 )
     &                                * ZAIR( ILEV ) / 2.465E19 )
            END IF
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...CH3COCHO  methylglyoxal:

      IF ( IMGLY .GT. 0 ) THEN
        DO ILEV = 1, NLEVS   ! level loop
          DO IWL = 1, NWL   ! wavelength loop
            IF ( QY( IWL, IMGLY ) .NE. 0.0) THEN
              QYZ( ILEV, IWL, IMGLY ) = 1.0 / ( 1.0 
     &                                + ( 1.0 / QY( IWL, IMGLY ) - 1.0 )
     &                                * ZAIR( ILEV ) / 2.465E19 )
            END IF
          END DO   ! wavelength loop
        END DO   ! level loop
      END IF

C...layers

      ILAY = 0
      DO I = 1, IBASE-1
        ILAY = ILAY + 1
        ZMID( ILAY ) = FLOAT( I - 1 ) + 0.5
        VAIR( ILAY ) = DZ * ( AIR( I + 1 ) - AIR( I ) )
     &               / ALOG( AIR( I + 1 ) / AIR( I ) )
        VO3 ( ILAY ) = DZ * ( O3( I + 1 ) + O3( I ) ) / 2.0
        VCLD( ILAY ) = 0.0
        VAER( ILAY ) = ( AER( I + 1 ) - AER( I ) )
     &               / ALOG( AER( I + 1 ) / AER( I ) )
        VT  ( ILAY ) = ( T( I + 1 ) + T( I ) ) / 2.0
      END DO

      DO I = IBASE, ITOP - 1
        HLOCA1 = 1.0 / ALOG( AIR( I ) / AIR( I + 1 ) )
        HLOCA2 = 1.0 / ALOG( AER( I ) / AER( I + 1 ) )
        DO K = 1, NSUBKM
          X0 = ( FLOAT( K - 1 ) + 0.5 ) * DZCLD / DZ
          X1 = 1.0 - X0
          ILAY = ILAY + 1
          ZMID( ILAY ) = FLOAT( I ) + X0
          DZ1 = X0 - 0.5 * DZCLD / DZ
          DZ2 = X0 + 0.5 * DZCLD / DZ
          VAIR( ILAY ) = AIR( I ) * HLOCA1 * 1.0E5
     &                 * ( EXP( -DZ1 / HLOCA1 )
     &                   - EXP( -DZ2 / HLOCA1 ) )
          VO3 ( ILAY ) = DZCLD * ( X0 * O3( I + 1 ) + X1 * O3( I ) )
          VCLD( ILAY ) = CLOUD( NSUBKM * ( I - IBASE ) + K )
          VAER( ILAY ) = AER( I ) * HLOCA2
     &                 * ( EXP( -DZ1 / HLOCA2 )
     &                   - EXP( -DZ2 / HLOCA2 ) )
          VT( ILAY ) = X0 * T( I + 1 ) + X1 * T( I )
        END DO
      END DO

      DO I = ITOP, 50
        ILAY = ILAY + 1
        ZMID( ILAY ) = FLOAT( I - 1 ) + 0.5
        VAIR( ILAY ) = DZ * ( AIR( I + 1 ) - AIR( I ) )
     &                 / ALOG( AIR( I + 1 ) / AIR( I ) )
        VO3 ( ILAY ) = DZ * ( O3( I + 1 ) + O3( I ) ) / 2.0
        VCLD( ILAY ) = 0.0
        VAER( ILAY ) = ( AER( I + 1 ) - AER( I ) )
     &               / ALOG( AER( I + 1 ) / AER( I ) )
        VT  ( ILAY ) = ( T( I + 1 ) + T( I ) ) / 2.0
      END DO
      ZMID( NLAYS ) = 50.5
      VAIR( NLAYS ) = DZ * HAIR * AIR( 51 )
      VO3 ( NLAYS ) = DZ * HO3 * O3( 51 )
      VCLD( NLAYS ) = 0.0
      VAER( NLAYS ) = HAER * AER( 51 )
      VT  ( NLAYS ) = T( 51 )

C...normalize aerosol optical depth to unity sum

      SUM = 0.0
      DO ILAY = 1, NLAYS
        SUM = SUM + VAER( ILAY )
      END DO
      DO ILAY = 1, NLAYS
        VAER( ILAY ) = VAER( ILAY ) / SUM
      END DO

C...calculated vertical column of O2 above the midpoint of each layer:
C...  want to use this for computing the average Schumann-Runge cross 
C...  section in each layer.
C...  so use half of current layer and half of previous higher layer

      CVO2( NLAYS ) = 0.2095 * VAIR( NLAYS ) / 2.0
      DO II = 2, NLAYS
        ILAY = NLAYS - II + 1
        CVO2( ILAY ) = CVO2( ILAY + 1 ) + 0.2095 * ( VAIR( ILAY )
     &               + VAIR( ILAY + 1 ) ) / 2.0
      END DO

C...correct attenuation coefficients for pressure and/or temperature
C...  dep. for now do only ozone absorption.

      DO ILAY = 1, NLAYS              ! layer loop

C...compute O3 cross sections for wavelength bands corresponding
C...  to the reference data for the temperature coefficients

        TDIFFX = VT( ILAY ) - 230.0
        DO IWLO3 = 1, NWLO3
          CSO3W1( IWLO3 ) = 1.0E-18 * ( SO3TX( IWLO3, 1 )
     &                      + SO3TX( IWLO3, 2 ) * TDIFFX
     &                      + SO3TX( IWLO3, 3 ) * TDIFFX * TDIFFX )
        END DO

C...now transfrom the computed O3 cross sections from their
C...  reference wavelength bands to the same wavelength bands
C...  as the extraterrestrial irradiance data

        CALL INTAVG ( WLO3L, CSO3W1, NWLO3+1, TYPE,
     &                STWL, ENDWL, CSO3W2, NWL )

C...replace the O3 cross sections data only within the wavelengths
C...  which are sensitive to temperature changes

        DO IWL = 1, NWL
          AO3( ILAY, IWL ) = O3ABS( IWL )
          IF ( ( STWL( IWL ) .GE. 263.158 ) .AND. 
     &         ( ENDWL( IWL ) .LE. 347.5 ) ) THEN
            AO3( ILAY, IWL ) =  CSO3W2( IWL )
          END IF
        END DO

      END DO   ! layer loop

      RETURN
      END
