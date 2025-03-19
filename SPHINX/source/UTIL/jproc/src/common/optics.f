
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/optics.f,v 1.5 2011/10/29 01:03:53 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)optics.F	1.1 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.optics.F 23 May 1997 12:44:21

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPTICS ( NWL, COSZEN, ENDIR, ENDN, ENUP,
     &                    VAIR, ARAYL, GRAY, OMRAY, AO2, VO3, AO3,
     &                    VCLD, GCLD, OMCLD, VAER, AAER, GAER, OMAER,
     &                    ALBEDO, NLAYER, NLEVEL, NSURF )

C*********************************************************************
C
C This subroutine prepares the data needed for the flux calculation,
C   then calls the scattering subroutine DELTED. It returns values of
C   the flux FLUX(LEV,IWL) for altitude LEV-1, wavelength IWL.
C It calculates the optical depths (vertical)
C   for each layer, from the vertical profiles of O2, O3,
C   air, cloud, and aerosol, and from the associated "cross sections
C
C*********************************************************************

      IMPLICIT NONE

C...........PARAMETERS and their descriptions

      INTEGER, PARAMETER :: MXWL = 130  ! number of wavelength bands
      INTEGER, PARAMETER :: NJ = 200    ! maximum levels

C...........ARGUMENTS and their descriptions

      INTEGER      NLAYER             ! total # of atm layers
      INTEGER      NLEVEL             ! number of levels
      INTEGER      NSURF              ! ground elev above sea level
      INTEGER      NWL                ! number of wl bands
      
      REAL         COSZEN             ! cosine zenith angle
      REAL         GAER               ! aerosol asymetry factor
      REAL         GCLD               ! cloud asymetry factor
      REAL         GRAY               ! asymetry fact for Rayleigh scat
      REAL         OMAER              ! aerosol single scat albedo
      REAL         OMCLD              ! cloud single scat cross sect
      REAL         OMRAY              ! single scat albedo, Rayleigh
      REAL         VAER( NJ )         ! aerosol column in layer
      REAL         VAIR( NJ )         ! air column in layer
      REAL         VCLD( NJ )         ! cloud column in layer
      REAL         VO3 ( NJ )         ! ozone column in layer
      REAL         AAER  ( MXWL )     ! aerosol total vert opt depth
      REAL         ALBEDO( MXWL )     ! ground albedo
      REAL         ARAYL ( MXWL )     ! Rayleigh scat cross section
      REAL         AO2  ( NJ, MXWL )  ! O2 cross section
      REAL         AO3  ( NJ, MXWL )  ! average O3 cross sect in layer
      REAL         ENDIR( NJ, MXWL )  ! direct flux
      REAL         ENDN ( NJ, MXWL )  ! diffuse down-flux
      REAL         ENUP ( NJ, MXWL )  ! diffuse up-flux

C...........LOCAL VARIABLES and their descriptions:

      INTEGER      II                 ! layer index
      INTEGER      IWL                ! wavelength index
      INTEGER      LAY                ! layer index
      INTEGER      LEV                ! level index
      INTEGER      NZ                 ! number of levels above sfc

      REAL         DTABS              ! sum of O2 & O3 absorption
      REAL         DTAER              ! aerosol scattering
      REAL         DTAIR              ! air scattering
      REAL         DTCLD              ! cloud column
      REAL         DTO2               ! O2 absorption
      REAL         DTO3               ! ozone absorption
      REAL         DTSCAT             ! total scattering 
      REAL         DTAU( NJ )         ! optical depth of layer
      REAL         EDIR( NJ )         ! irradiance of direct solar beam
      REAL         EDN ( NJ )         ! irradiance of down-welling diffuse light
      REAL         EUP ( NJ )         ! irradiance of up-welling diffuse light
      REAL         FDIR( NJ )         ! direct actinic flux
      REAL         FDN ( NJ )         ! downward actinic flux
      REAL         FUP ( NJ )         ! upward actinic flux
      REAL         G   ( NJ )         ! asymmetry factor for layer
      REAL         OM  ( NJ )         ! single-scat albedo of layer

C*********************************************************************
C     begin body of subroutine RTLINK

C...loop over wavelengths

      DO 301 IWL = 1, NWL

C...calculate optical depths for all layers (including cloud sublayers)'

        DO LAY = NSURF, NLAYER
          II = NLAYER + 1 - LAY
          DTAIR = VAIR( LAY ) * ARAYL( IWL )
          DTO2  = 0.2095 * VAIR( LAY ) * AO2( LAY, IWL )
          DTO3  = VO3( LAY ) * AO3( LAY, IWL )
          DTCLD = VCLD( LAY )
          DTAER = VAER( LAY ) * AAER( IWL )

          DTSCAT = DTAIR + DTCLD * OMCLD + DTAER * OMAER
          DTABS  = DTO2 + DTO3 + DTCLD * ( 1.0 - OMCLD )
     &           + DTAER * ( 1.0 - OMAER )
          DTABS  = AMAX1( DTABS,  1.0E-36 )
          DTSCAT = AMAX1( DTSCAT, 1.0E-36 )

          DTAU( II ) = DTABS + DTSCAT
          OM  ( II ) = DTSCAT / ( DTSCAT + DTABS )
          G   ( II ) = ( GCLD * DTCLD * OMCLD
     &               + GAER * DTAER * OMAER )
     &               / DTSCAT
        END DO

C...initialize fluxes and other DELTED parameters

        NZ = NLEVEL - NSURF + 1

        DO II = 1, NJ
          FDIR( II ) = 0.0
          FUP ( II ) = 0.0
          FDN ( II ) = 0.0
          EDIR( II ) = 0.0
          EUP ( II ) = 0.0
          EDN ( II ) = 0.0
        END DO

        CALL TWOSTR ( NZ, COSZEN, ALBEDO( IWL ), DTAU, OM, G,
     &                FDIR, FUP, FDN, EDIR, EUP, EDN )

C...return to upright grid

        DO II = 1, NLEVEL - NSURF + 1
          LEV = NLEVEL + 1 - II
          ENDIR( LEV, IWL ) = FDIR( II )
          ENDN ( LEV, IWL ) = FDN ( II )
          ENUP ( LEV, IWL ) = FUP ( II )
        END DO

301   CONTINUE

      RETURN
      END
