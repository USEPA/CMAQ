
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
C $Header: /project/yoj/arc/CCTM/src/biog/beis3/beis3.f,v 1.2 2011/10/21 16:10:17 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE BEIS( JDATE, JTIME, NX, NY, MSPCS, SEMIS, BIPOL )

C-----------------------------------------------------------------------
C Description:
 
C   Uses PAR and sfc temperature data to calculate
C   biogenic ISOP and MBO emissions.  Other emissions are
C   calculated using the temperature data only.
 
C Preconditions:
C   PAR and Surface Temperature
 
C Subroutines and Functions Called:
 
C Revision History:
C   4/01 : Prototype by JMV
C   6/05 : updates for BEIS3.3 by D. Schwede (BEIS3.13)
C   8/05 : additional diagnostic messages for PAR out of bounds (G. Pouliot)
C  10/06 : yoj
C   1/10 : yoj remove ck & report if TAIR > 315
C   7/14 : JOB added leaf temperature and two layer canopy model
C  11/07 : JOB updated for ASX_DATA_MOD and corrected the Cl algorithm 
C          to be consitent with Gunther et al. 1999 doi:10.1029/1999JD900391
C  5/7/18: D. Schwede Removed call to CZANGLE. COSZEN now calculated in ASX_DATA_MOD
C-----------------------------------------------------------------------
C Modified from:

C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C              System
C File: @(#)$Id: beis3.f,v 1.2 2011/10/21 16:10:17 yoj Exp $
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C smoke@unc.edu
C Pathname: $Source: /project/yoj/arc/CCTM/src/biog/beis3/beis3.f,v $
C Last updated: $Date: 2011/10/21 16:10:17 $
C-----------------------------------------------------------------------

      USE BIOG_EMIS, ONLY: NSEF, NLAI, LAITYPES
      Use ASX_DATA_MOD

      IMPLICIT NONE
C Includes:

C Arguments:
      INTEGER, INTENT( IN ) :: JDATE   ! current simulation date (YYYYDDD)
      INTEGER, INTENT( IN ) :: JTIME   ! current simulation time (HHMMSS)
      INTEGER, INTENT( IN ) :: NX      ! no. columns
      INTEGER, INTENT( IN ) :: NY      ! no. rows
      INTEGER, INTENT( IN ) :: MSPCS   ! no. of output species

      REAL,    INTENT( IN ) :: SEMIS ( :,:,: ) ! normalized emissions
      REAL,    INTENT( OUT ) :: BIPOL( :,:,: ) ! output emissions

C Local Variables:
      INTEGER        R, C, L, I   ! counters

      REAL           CFOTHR       ! isop corr fac -- non-forest
      REAL           CFCLAI       ! isop corr fac -- LAI
      REAL           CFNO         ! NO correction factor
      REAL           CFOVOC       ! non-isop corr fac
      REAL           CFSESQT      ! sesquiterpene corr fac
      REAL           PAR          ! photo. actinic flux (UE/M**2-S) (UE=micro-einsteins)
      REAL           CT_SUN       ! temperature correction
      REAL           DT_SUN       ! temperature correction
      REAL           CT_SHADE     ! temperature correction       
      REAL           DT_SHADE     ! temperature correction      
      REAL           TAIR         ! local 2 meter temperature
      REAL           DTLEAF_SUN   ! Difference between mean canopy leaf and ambient temperature [K]
      REAL           DTLEAF_SHADE ! Difference between mean canopy leaf and ambient temperature [K]      
      REAL           TLEAF_SUN    ! Mean canopy leaf temperature [K]
      REAL           TLEAF_SHADE  ! Mean canopy leaf temperature [K]
      REAL           RBW          ! Quasi-laminar boundary layer resistance for water vapor [s/m]
      REAL           RBH          ! Quasi-laminar boundary layer resistance for heat [s/m]     
      REAL           RH           ! Relative humidity [ratio 0-1]
      REAL           ES           ! Saturation vapor pressure for 2 meter T  [Pa]          
      REAL           SHF          ! Soil heat flux [W/m**2]    
      REAL           DVAP         ! vapor pressure deficit [Pa/Pa]  
      REAL           SSVP         ! Slope of the saturation vapor pressure curve over P [1/K] 
      REAL           GVAP         ! canopy water vapor conuctance m/s
      REAL           GHT          ! canopy heat conductance m/s      
      REAL           CPAIR        ! specific heat of air
      REAL           LHV          ! Latent heat of vaporization       
      REAL           CPOT         ! potential temperature conversion 
      REAL           DENS         ! Dry air density kg/m**3
      REAL           LHSH_DIV     ! W/m**2 to K units conversion 
      REAL           LHSH_COMP    ! latent/sensible heat flux component of leaf energy bal
      REAL           RK           ! k from Geron and Guenther
      REAL           CSUBL_SUN    ! C sub l
      REAL           CSUBL_SHADE  ! C sub l
      REAL           FRACSUN      ! Fraction sun
      REAL           FRACSHADE    ! Fraction shade            
      REAL           TLAI         ! local LAI
      REAL           SOLRAD       ! local solar radiation [W/m**2]
      REAL           PSFC         ! local sfc pressure (mb)
      REAL           ZEN          ! zenith angle
      REAL           PARDB        ! PAR direct beam
      REAL           PARDIF       ! PAR diffuse
      REAL           COSZ         ! local cosine of zenith angle
      
      REAL, PARAMETER :: CV     = 8.0e-6  ! Resistance to soil heat conductance under vegetation [s/m]
      REAL, PARAMETER :: RRAD   = 230.0   ! Atmospheric radiative resistance Monteith 1973 [s/m]
      REAL, PARAMETER :: SCW    = KVIS / DWAT ! schmidt number for water vapor
      REAL, PARAMETER :: REFLDV = 0.057 ! visible light reflection coefficient from MEGAN 2.10 

      CHARACTER( 5 )   :: BTMP    ! temporary variable name
      CHARACTER( 256 ) :: MESG    ! message buffer

      CHARACTER( 16 )  :: PROCNAME = 'BEIS'   ! procedure name

C-----------------------------------------------------------------------
      
C Loop through cells
      DO R = 1, NY
         DO C = 1, NX

            TAIR = MET_DATA%TEMP2( C,R )         ! unit in degree K
            COSZ = MET_DATA%COSZEN( C,R )            

C Check min bounds for temperature
C Note we no longer cap temperature for isoprene
            IF ( TAIR .LT. 200.0 ) THEN
               WRITE( MESG, 94010 ) 'TAIR=', TAIR,
     &              'out of range at (C,R)=', C, R
               CALL M3EXIT( PROCNAME, JDATE, JTIME, MESG, 2 )
            END IF

            SOLRAD = MET_DATA%RGRND( C,R )

C Cosine of zenith angle to zenith angle (radians)
            ZEN =  ACOS( COSZ )
            PSFC = MET_DATA%PRSFC( C,R )
    
C atmospheric water vapor variables used for leaf latent heat flux
            IF ( TAIR .LE. STDTEMP ) THEN
               ES = VP0 * EXP( 22.514 - (6.15e3 / TAIR) )
            ELSE
               ES = VP0 * EXP( SVP2 * (TAIR - STDTEMP) /
     &                                (TAIR - SVP3) )
            END IF
            RH   = MET_DATA%RH2( C,R ) / 100.0
            DVAP = ES*(1.0-RH)/PSFC
            SSVP = (SVP2*(STDTEMP-SVP3)*ES/(TAIR-SVP3)**2)/
     &              PSFC

C calculate the soil heaf flux under a canopy folowing WRF3.4.1 PX
            SHF = -2.0*PI/SIDAY*(MET_DATA%TEMPG( C,R ) - TAIR)/CV

C calculate the heat and water vapor quasilaminar boundary layer resistance
            RBH = 5.0/MET_DATA%USTAR( C,R )
            RBW = RBH*(SCW/PR)**TWOTHIRDS
   
C calculate the specific heat of air and latent heat of vaporization
            CPAIR = CPD * (1.0 + 0.84 * MET_DATA%Q2( C,R ))
            LHV   = LV0 - 2370.0 * (TAIR - STDTEMP)

C calculate the leaf  water vapor and heat conductance 
            GHT  = 1 / (MET_DATA%RA( C,R ) + RBH  ) + 1/RRAD
            GVAP = 1 / (MET_DATA%RA( C,R ) + RBW + MET_DATA%RS( C,R ) * MET_DATA%LAI( C,R ))

C Calculate the potential temperature conversion from the sensible heat flux in WRF 3.4.1
            CPOT = (STDATMPA/PSFC)**(RDGAS/CPAIR)

C Calculate the solar radiation and soil heat flux of the leaf energy budget
            DENS     =  PSFC /( RDGAS * TAIR )
            LHSH_DIV =  DENS * CPOT * CPAIR * (GHT + 1 / (MET_DATA%RA( C,R ) + RBH  )) + 
     &                  DENS * LHV * SSVP * GVAP

C calculate the latent heat flux portion of the leaf energy budget 
            LHSH_COMP = SHF - LHV * DENS * GVAP * ( ES - RH * ES) / PSFC

C Direct and diffuse photosynthetically active radiation
            CALL GETPARB( SOLRAD, PSFC, COSZ, PARDB, PARDIF )

            PAR = PARDB + PARDIF

C Check max/min bounds of PAR and calculate biogenic ISOP
            IF ( PAR .LT. 0.0 .OR. PAR .GT. 2600.0 ) THEN
               
               WRITE( MESG, 94030 ) 'PAR=', PAR,
     &              'out of range at (C,R)=', C, R,
     &              'PARDB  = ', PARDB,
     &              'PARDIF = ', PARDIF,
     &              'SOLRAD = ', SOLRAD,
     &              'PSFC   = ', PSFC,
     &              'ZEN    = ', ZEN
    
                CALL M3MSG2( MESG )
            END IF
            TLAI = MET_DATA%LAI( C,R )
C Compute ISOP and MBO and METH emissions first
C Note assumption that these are the first 3 species in LAITYPE and BIOTYPE
C arrays
            DO I = 1, NLAI

               BTMP = LAITYPES( I )
C Adjust methanol based on T. Pierce recommendation (1-16-03)
               IF ( BTMP == 'METH' ) THEN
                  TLAI = MAX( 3.0, TLAI )
               END IF

               IF ( TLAI .GT. 10.0 ) THEN
                  WRITE( MESG, 94010 ) 'LAI=', TLAI,
     &              'out of range at (C,R)=', C, R
                  CALL M3EXIT( PROCNAME, JDATE, JTIME, MESG, 2 )
               END IF

C Initialize csubl
               CSUBL_SUN   = 0.0
               CSUBL_SHADE = 0.0       

               IF ( PARDB + PARDIF .EQ. 0.0 ) THEN
                  BIPOL( C,R,I ) = 0.0
               ELSE
                  CALL CLNEW_SUB( ZEN, PARDB, PARDIF, TLAI, LHSH_DIV,
     &                            LHSH_COMP, DTLEAF_SUN, DTLEAF_SHADE, 
     &                            CSUBL_SUN, CSUBL_SHADE, FRACSUN, FRACSHADE,
     &                            SOLRAD, REFLDV )

                  TLEAF_SUN   = DTLEAF_SUN   + TAIR
                  TLEAF_SHADE = DTLEAF_SHADE + TAIR
C Calculate temperature correction term
                  DT_SUN   = 28668.514 / TLEAF_SUN
                  DT_SHADE = 28668.514 / TLEAF_SHADE 
                  CT_SUN   = EXP( 37.711 - 0.398570815 * DT_SUN ) /
     &                          ( 1.0 + EXP( 91.301 - DT_SUN ) )
                  CT_SHADE = EXP( 37.711 - 0.398570815 * DT_SHADE ) /
     &                          ( 1.0 + EXP( 91.301 - DT_SHADE ) )     
                  BIPOL( C,R,I ) = SEMIS( C,R,I )*( FRACSUN   * CT_SUN   * CSUBL_SUN + 
     &                                              FRACSHADE * CT_SHADE * CSUBL_SHADE )
               END IF

            END DO ! end ISOP and MBO calculations loop
C Only estimate BCOV emissions for vegitation
            IF( TLAI .GT. 0.0 ) THEN

               CALL CLNEW_SUB( ZEN, PARDB, PARDIF, TLAI, LHSH_DIV,
     &                         LHSH_COMP, DTLEAF_SUN, DTLEAF_SHADE, 
     &                         CSUBL_SUN, CSUBL_SHADE, FRACSUN, FRACSHADE,
     &                         SOLRAD, REFLDV )
     
               TLEAF_SUN   = TAIR + DTLEAF_SUN
               TLEAF_SHADE = TAIR + DTLEAF_SHADE
C Calculate other biogenic emissions except NO
C Note not speciated here
C Limit temerature to 315 K for monoterpenes and other VOCs
               TLEAF_SUN   = MIN( TLEAF_SUN, 315.0 )
               TLEAF_SHADE = MIN( TLEAF_SHADE, 315.0 )    

               CFOVOC  = FRACSUN   * EXP( 0.09 * ( TLEAF_SUN   - 303.0 ) ) + 
     &                   FRACSHADE * EXP( 0.09 * ( TLEAF_SHADE - 303.0 ) ) 
               CFSESQT = FRACSUN   * EXP( 0.17 * ( TLEAF_SUN   - 303.0 ) ) + 
     &                   FRACSHADE * EXP( 0.17 * ( TLEAF_SHADE - 303.0 ) )
            ELSE
C If LAI = 0 zero out emission factors
               CFOVOC  = 0.0
               CFSESQT = 0.0
            END IF

            DO I = NLAI + 1, NSEF - 2
               BIPOL( C,R,I ) = SEMIS( C,R,I ) * CFOVOC
            END DO

            I = NSEF - 1
            BIPOL( C,R,I ) = SEMIS( C,R,I ) * CFSESQT

         END DO ! end loop over columns
      END DO ! end loop over rows

      RETURN

C-----------------------------------------------------------------------

94010 FORMAT( 1X, A, F10.2, 1X, A, I3, ',', I3 )
94020 FORMAT( 1X, A, F10.2, 1X, A, I3, ',', I3, A )
94030 FORMAT( 1X, A, F10.2, 1X, A, I3, ',', I3, 1X, 5(A, F10.2) )

C-----------------------------------------------------------------------
      CONTAINS

C Function to calculate csubl based on zenith angle, par, and lai
         SUBROUTINE CLNEW_SUB( ZEN, PARDB, PARDIF, TLAI, LHSH_DIV,
     &                              LHSH_COMP, DTLSUN, DTLSHADE, 
     &                              CSUBL_SUN, CSUBL_SHADE, FRACSUN, FRACSHADE,
     &                              SOLRAD, REFLDV )

         IMPLICIT NONE

C Function arguments:
         REAL, INTENT( IN )  :: PARDB    ! direct beam PAR( umol/m2-s)
         REAL, INTENT( IN )  :: PARDIF   ! diffuse PAR ( umol/m2-s)
         REAL, INTENT( IN )  :: ZEN      ! solar zenith angle (radians)
         REAL, INTENT( IN )  :: TLAI     ! leaf area index for grid cell
         REAL, INTENT( IN )  :: LHSH_DIV  
         REAL, INTENT( IN )  :: LHSH_COMP
         REAL, INTENT( IN )  :: SOLRAD
         REAL, INTENT( IN )  :: REFLDV
         REAL, INTENT( OUT ) :: CSUBL_SUN
         REAL, INTENT( OUT ) :: CSUBL_SHADE
         REAL, INTENT( OUT ) :: DTLSUN           ! Sun leaf temperature [K]
         REAL, INTENT( OUT ) :: DTLSHADE         ! Sun leaf temperature [K]
         REAL, INTENT( OUT ) :: FRACSUN          ! fraction of leaves that are sunlit
         REAL, INTENT( OUT ) :: FRACSHADE        ! fraction of leaves that are shaded

C Parameters:
         REAL, PARAMETER :: ALPHA = 0.8 ! leaf absorptivity
         REAL, PARAMETER :: KD = 0.68   ! extinction coefficient for diffuse radiation
         
         
C Local variables:
         REAL, SAVE :: SQALPHA ! square root of alpha
         REAL KBE              ! extinction coefficient for direct beam
         REAL CANPARSCAT       ! exponentially wtd scattered PAR (umol/m2-s)
         REAL CANPARDIF_SUN    ! exponentially wtd diffuse PAR at the top of the canopy (umol/m2-s)
         REAL CANPARDIF_SHADE  ! exponentially wtd diffuse PAR in the shaded part of the canopy (umol/m2-s)
         REAL PARSHADE         ! PAR on shaded leaves (umol/m2-s)
         REAL PARSUN           ! PAR on sunlit leaves (umol/m2-s)
         REAL SOLSUN           ! RS transmitted to sunlit leaves W/m**2
         REAL SOLSHADE         ! RS transmitted to shaded leaves W/m**2
         REAL LAISUN           ! LAI that is sunlit
         REAL LAISHADE         ! LAI that is shaded


         LOGICAL, SAVE :: FIRSTIME = .TRUE.

C-----------------------------------------------------------------------
         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            SQALPHA = SQRT( ALPHA )
         END IF
C CN98 - eqn 15.4, assume x=1 (can use a table or atributes to change this)
C Set a ceiling for KBE to prevent a blow up at high zenith angles. This has
C little impact on the results because direct PAR is low under these conditions
         IF( ZEN .GE. 1.57 ) THEN
            KBE = 627.9
         ELSE
            KBE = 0.5 * SQRT( 1.0 + TAN( ZEN )**2 )
         END IF
         IF ( TLAI .GT. 0.1 ) THEN
            IF ( PARDB + PARDIF .GT. 0.0 ) THEN

C CN98 p-259 Sun and shaded areas of the canopy
               LAISUN     = ( 1.0 - EXP( -1.0 * KBE * TLAI ) ) / KBE
               LAISHADE   = MAX( TLAI - LAISUN, 0.0 )
               FRACSUN    = LAISUN / TLAI             
               FRACSHADE  = 1.0 - FRACSUN

C CN98 - p. 261 (this is usually small)
               CANPARSCAT = 0.5 * PARDB * ( EXP( -1.0 * SQALPHA * KBE * TLAI )
     &                    - EXP( -1.0 * KBE * TLAI ) )

C CN98 - p. 261 (assume exponentially wtd avg)
               CANPARDIF_SUN    = PARDIF * ( 1.0 - EXP( -1.0 * SQALPHA * KD * LAISUN ) )
     &                                   / ( SQALPHA * KD * LAISUN )

               CANPARDIF_SHADE  = CANPARDIF_SUN * ( EXP( -1.0 * SQALPHA * KD * LAISUN ) 
     &                                            - EXP( -1.0 * SQALPHA * KD * TLAI ) )
     &                                   / ( SQALPHA * KD * (TLAI - LAISUN) )

C CN98 - p. 261 (for next 3 eqns)
C note that we use the incoming (not absorbed) PAR
               PARSHADE   = CANPARDIF_SHADE + CANPARSCAT
               PARSUN     = KBE * PARDB + CANPARDIF_SUN + CANPARSCAT
     
C calculate the leaf temperature following Campbel and Norman 1998 eq 14.6 
C with the addition of incomming atmospheric long wave irradiation resulting 
C in the cacelation of the long wave radiation budget
         
               SOLSUN    = SOLRAD * PARSUN / ( PARSUN + PARSHADE )
               SOLSHADE  = SOLRAD * PARSHADE / ( PARSUN + PARSHADE )
               DTLSUN    = ((1.0 - REFLDV) * SOLSUN + LHSH_COMP ) / LHSH_DIV
               DTLSHADE  = ((1.0 - REFLDV) * SOLSHADE + LHSH_COMP ) / LHSH_DIV
               DTLSUN    = MIN(DTLSUN,  10.0) 
               DTLSUN    = MAX(DTLSUN, -10.0) 
               DTLSHADE  = MIN(DTLSHADE,  10.0) 
               DTLSHADE  = MAX(DTLSHADE, -10.0) 

C cguen is Guenther's eqn for computing light correction as a function of
C PAR...fracSun should probably be higher since sunlit leaves tend to be
C thicker than shaded leaves. But since we need to make crude assumptions
C regarding leaf orientation (x=1), we will not attempt to fix at the moment.

               CSUBL_SUN   = CGUEN( PARDB + PARDIF, 0.0, LAISUN, KBE )
C By definition diffusive radiation, use the diffusive attenuation coefficient and 
C Diffusive par at the bottom of the sunlit layer
               CSUBL_SHADE = CGUEN( CANPARDIF_SUN, LAISUN, TLAI, KD )
            
            ELSE ! to prevent divide by 0 when there is no solar rad
               CSUBL_SUN   = 0.0
               CSUBL_SHADE = 0.0
               FRACSUN     = 0.2
               FRACSHADE   = 0.8
               DTLSUN      = LHSH_COMP / LHSH_DIV
               DTLSHADE    = LHSH_COMP / LHSH_DIV
               DTLSUN      = MIN(DTLSUN,  10.0) 
               DTLSUN      = MAX(DTLSUN, -10.0) 
               DTLSHADE    = MIN(DTLSHADE,  10.0) 
               DTLSHADE    = MAX(DTLSHADE, -10.0) 
            END IF       
    
         ELSE 
            CSUBL_SUN   = CGUEN( PARDB + PARDIF, 0.0, TLAI, KBE )
            CSUBL_SHADE = 0.0
            FRACSUN     = 1.0
            FRACSHADE   = 0.0
            DTLSUN      = ((1.0 - REFLDV) * SOLRAD + LHSH_COMP ) / LHSH_DIV
            DTLSHADE    = 0.0
            DTLSUN      = MIN(DTLSUN,  10.0) 
            DTLSUN      = MAX(DTLSUN, -10.0)      
         END IF

         END SUBROUTINE CLNEW_SUB

C-----------------------------------------------------------------------

C Function to calculate Guenther's equation for computing light correction
         REAL FUNCTION CGUEN( PAR, LAI1, LAI2, KBE )

C 11/14 J. Bash - Updated to Niinemets et al. 2010a doi:10.1029/2010JG001436 
C                 Big leaf model which updates Guenther et al. 1993 doi:10.1029/93JD00527 for 
C                 in-canopy gradients

         IMPLICIT NONE

C Function arguments:
         REAL, INTENT( IN ) :: PAR
         REAL, INTENT( IN ) :: LAI1 ! top of the layer LAI
         REAL, INTENT( IN ) :: LAI2 ! bottom of the layer LAI
         REAL, INTENT( IN ) :: KBE  ! light extenction coefficient

C Parameters: updated to Niinemets et al. 2010b https://doi.org/10.5194/bg-7-1809-2010 to be 
C consistent with Niinemets et al. 2010a
         REAL, PARAMETER :: ALPHA = 0.0015 ! Mean of reported Quercus rubra and Liquidambar styraciflua
         REAL, PARAMETER :: CL    = 1.2716 ! Calculated according Niinemets et al. 2010b to return 1 at
                                           ! standard conditions ( PAR=1000, KBE = 0.68 )

C-----------------------------------------------------------------------
         IF ( PAR .LE. 0.01 ) THEN
            CGUEN = 0.0
         ELSE
C Niinemets et al. 2010 equation A9 integrated from LAI1 to LAI2
            CGUEN = CL * ( SQRT(1+ALPHA**2 * PAR**2 * EXP(-2*LAI1*KBE)) -
     &                     SQRT(1+ALPHA**2 * PAR**2 * EXP(-2*LAI2*KBE)) ) /
     &                   ( ALPHA * KBE * PAR )
         END IF

         RETURN

         END FUNCTION CGUEN

C-----------------------------------------------------------------------

      END SUBROUTINE BEIS

