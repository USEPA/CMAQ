!------------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in           !
!  continuous development by various groups and is based on information        !
!  from these groups: Federal Government employees, contractors working        !
!  within a United States Government contract, and non-Federal sources         !
!  including research institutions.  These groups give the Government          !
!  permission to use, prepare derivative works of, and distribute copies       !
!  of their work in the CMAQ system to the public and to permit others         !
!  to do so.  The United States Environmental Protection Agency                !
!  therefore grants similar permission to use the CMAQ system software,        !
!  but users are requested to provide copies of derivative works or            !
!  products designed to operate in the CMAQ system to the United States        !
!  Government without restrictions as to use by others.  Software              !
!  that is used with the CMAQ system but distributed under the GNU             !
!  General Public License or the GNU Lesser General Public License is          !
!  subject to their copyright restrictions.                                    !
!------------------------------------------------------------------------------!

SUBROUTINE setup_mm5v3 (ctmlays)

!-------------------------------------------------------------------------------
! Name:     Set Up the MM5 Domain Attributes from MM5 Version 3.
! Purpose:  Establishes bounds for MM5 post-processing.
! Revised:  17 Sep 2001  Original version.  (T. Otte)
!           11 Oct 2001  Corrected definitions of MET_P_ALP_D, MET_P_BET_D, and
!                        MET_P_GAM_D based on map projection.  (T. Otte)
!           23 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  Changed missing
!                        value for MET_P_ALP_D, MET_P_BET_D, and MET_P_GAM_D
!                        to BADVAL3.  (T. Otte)
!           09 Aug 2004  Corrected header settings for polar stereographic
!                        and Mercator projections.  Modified code so that
!                        arrays are made available in output only if user
!                        options in MM5 generate those data.  Added definition
!                        for MET_SNOW_OPT, and error-checking for using MM5
!                        output with Pleim-Xiu LSM and IFSNOW=2.  Added MM5
!                        cone factor (MET_CONE_FAC) and true latitude 1.
!                        Removed unused variables MET_SDATE and MET_STIME.
!                        Added IFT2M, and rearranged code to search for
!                        2-m temperature in MM5 file.  (T. Otte)
!           30 Nov 2004  Added setting for fractional land use fields flag
!                        based on file availability and based on whether or
!                        not the fields are there.  (MM5 users must have set
!                        IEXTRA flag in TERRAIN namelist to generate these
!                        fields.)  Added NUMMETLU to include number of land
!                        use categories in incoming meteorology data.  (T. Otte)
!           26 May 2005  Changed I and J variable names to Y and X to make
!                        code more general.  Changed subroutine name from
!                        SETUPV3 to SETUP_MM5V3.  Removed unused variables
!                        MET_IEXPAND, MET_IOFFSET, and MET_JOFFSET.  Added
!                        MET_TRU2 and IFW10M.  Added capability to use all MM5
!                        layers for MCIP without defining a priori.  (T. Otte)
!           18 Jul 2005  Corrected RADM seasons for Southern Hemisphere.
!                        Improved error-checking on header comparisons for
!                        MMOUT and TERRAIN files.  (T. Otte)
!           18 Aug 2005  Corrected declaration of START_INDEX from CHARACTER*4
!                        to INTEGER.  Changed internal variable INDEX to IDX
!                        and internal variable TIME to TIMEOUT to avoid
!                        confusion with F90 intrinsic functions.  Removed
!                        inadvertent TAB in format.  (T. Otte)
!           07 Apr 2006  Corrected checking of I/O API variables for Mercator
!                        projection.  (T. Otte)
!           28 Jun 2006  Corrected settings of I/O API variables for polar
!                        stereographic projection...again.  Added new
!                        variables MET_PROJ_CLAT and MET_PROJ_CLON.
!                        Corrected format statement for label 9500.  (T. Otte)
!           30 Jul 2007  Corrected setting of NQSPECIES for warm rain scheme.
!                        Removed settings for RADMdry variable ISESN and for
!                        MET_INHYD.  Changed local variable NAME to VNAME to
!                        avoid confusion with F90 intrinsic function.  Removed
!                        NPXFIELDS.  Added definition for MET_LU_SRC.  Added
!                        settings for logicals that determine whether or not
!                        some fields will be in the input meteorology file.
!                        Added settings for MET_NS, MET_RELEASE, MET_FDDA_3DAN,
!                        MET_FDDA_SFAN, and MET_FDDA_OBS.  Changed MET_RADIATION
!                        into MET_LW_RAD and MET_SW_RAD.  (T. Otte)
!           05 May 2008  Added checks to determine if 2-m mixing ratio (Q2) and
!                        turbulent kinetic energy (TKE) arrays exist, and set
!                        flags appropriately.  Extract nudging coefficients
!                        from header to use in metadata.  Added MET_UCMCALL
!                        value (always 0 for MM5).  (T. Otte)
!           27 Oct 2009  Changed MET_UCMCALL to MET_URBAN_PHYS.  Added
!                        MET_CEN_LAT, MET_CEN_LON, MET_RICTR_DOT, MET_RJCTR_DOT,
!                        and MET_REF_LAT.  Compute MET_XXCTR and MET_YYCTR.
!                        Updated grid checking information to reflect changes
!                        in specification of I/O API header variables for
!                        polar stereographic and Mercator.  Added settings for
!                        IFLUWRFOUT and IFZNT.  (T. Otte)
!           15 Dec 2010  Initialized MET_RAIN_BUCKET to -1.0 because it is only
!                        used for WRF runs.  (T. Otte)
!           01 Sep 2011  Replaced module PARMS3 with I/O API module M3UTILIO.
!                        Changed name of module FILE to FILES and DATA to
!                        DATA to DATA_IN to avoid conflicts with F90 protected
!                        intrinsics.  Improved error handling.  Changed TOL
!                        from 1.0e-7 to 1.0e-6.  Initialized MET_SHAL_CU.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           21 Aug 2012  Added default value for new variable MET_PCP_INCR
!                        which is specifically for WRFv3.2+.  (T. Otte)
!           26 Nov 2014  Defined values of ice, lake, and urban land use
!                        indices, and moved those definitions from getluse.f90
!                        to this routine.  (T. Spero)
!           10 Apr 2015  Fill new logical variable IFCLD3D as "false" because
!                        3D resolved cloud fraction is not part of MM5 output.
!                        That field is now available in METCRO3D, if available
!                        in incoming meteorology.  (T. Spero)
!           21 Aug 2015  Fill new logical variable IFMOLACM as "false" because
!                        it pertains to WRF/ACM2 only.  (T. Spero)
!           17 Sep 2015  Changed IFMOLACM to IFMOLPX.  (T. Spero)
!           22 Jun 2017  Added default value for MET_HYBRID.  (T. Spero)
!-------------------------------------------------------------------------------

  USE files
  USE metinfo
  USE mcipparm
  USE m3utilio, ONLY: badval3

  IMPLICIT NONE

  INTEGER,            ALLOCATABLE   :: bhi       ( : , : )
  CHARACTER(LEN=80),  ALLOCATABLE   :: bhic      ( : , : )
  REAL,               ALLOCATABLE   :: bhr       ( : , : )
  CHARACTER(LEN=80),  ALLOCATABLE   :: bhrc      ( : , : )
  REAL,               INTENT(INOUT) :: ctmlays   ( maxlays )
  CHARACTER(LEN=24)                 :: currentdate
  REAL,               ALLOCATABLE   :: data_in   ( : , : , : , : )
  INTEGER                           :: dd
  CHARACTER(LEN=2)                  :: ddc
  CHARACTER(LEN=46)                 :: description
  INTEGER                           :: end_index ( 4 )
  LOGICAL                           :: gotlays
  INTEGER                           :: hh
  CHARACTER(LEN=2)                  :: hhc
  INTEGER                           :: idx
  INTEGER                           :: iflag
  LOGICAL                           :: ifter
  LOGICAL                           :: ifu10m
  LOGICAL                           :: ifv10m
  INTEGER                           :: istat
  INTEGER                           :: k
  INTEGER                           :: km1
  CHARACTER(LEN=2)                  :: minc
  INTEGER                           :: minorrev
  INTEGER                           :: mins
  INTEGER                           :: mm
  CHARACTER(LEN=2)                  :: mmc
  CHARACTER(LEN=1)                  :: mrev
  INTEGER                           :: ndim
  INTEGER,            PARAMETER     :: numprogs  = 20
  INTEGER,            PARAMETER     :: numvalsi  = 50
  INTEGER,            PARAMETER     :: numvalsr  = 20
  LOGICAL                           :: ok        = .TRUE.
  CHARACTER(LEN=4)                  :: ordering
  CHARACTER(LEN=16),  PARAMETER     :: pname     = 'SETUP_MM5V3'
  REAL                              :: rictr_coarse
  REAL                              :: rjctr_coarse
  CHARACTER(LEN=4)                  :: staggering
  INTEGER                           :: start_index ( 4 )
  REAL,               ALLOCATABLE   :: temp1d      ( : )
  CHARACTER(LEN=256)                :: terfile
  REAL                              :: timeout
  REAL,               PARAMETER     :: tol       = 1.0e-6
  CHARACTER(LEN=25)                 :: units
  CHARACTER(LEN=9)                  :: var
  CHARACTER(LEN=1)                  :: verc
  INTEGER                           :: version
  CHARACTER(LEN=9)                  :: vname
  INTEGER                           :: yyyy
  CHARACTER(LEN=4)                  :: yyyyc

  INTERFACE

    SUBROUTINE getgist (hdr_int, hdr_real, hdr_int_desc, hdr_real_desc)
      IMPLICIT NONE
      INTEGER,            INTENT(IN)    :: hdr_int       ( : , : )
      CHARACTER(LEN=80),  INTENT(IN)    :: hdr_int_desc  ( : , : )
      REAL,               INTENT(IN)    :: hdr_real      ( : , : )
      CHARACTER(LEN=80),  INTENT(IN)    :: hdr_real_desc ( : , : )
    END SUBROUTINE getgist

  END INTERFACE

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f6000 = "(/, 1x, &
    & '- SUBROUTINE SETUP_MM5V3 - READING MM5 HEADER')"
  CHARACTER(LEN=256), PARAMETER :: f6100 = "(3x, &
    & 'MM5 GRID DIMENSIONS (I,J,K) ', i4, 1x, i4, 1x, i3)"
  CHARACTER(LEN=256), PARAMETER :: f6200 = "(//, 1x, &
    & '- PRINTING CONTENTS OF MM5 HEADER ON UNIT ', i3, //)"

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR READING FILE ON UNIT = ', i3, &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNEXPECTED END-OF-FILE REACHED ON UNIT ', i3, &
    & /, 1x, '***   IOSTAT = ', i5, &
    & /, 1x, '***   VERIFY THAT THE FILE EXISTS!!!', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNEXPECTED FLAG FOUND IN VERSION 3 HEADER', &
    & /, 1X, '***   IFLAG = ', i3, &
    & /, 1x, '***   UNIT  = ', i3, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   INAPPROPRIATE INPUT FILE', &
    & /, 1x, '***   MUST BE MM5 OUTPUT (MMOUT)', &
    & /, 1x, '***   INDEX IS ', i2, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9350 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   INCOMPATIBLE OPTIONS IN MM5 OUTPUT', &
    & /, 1x, '***   RAN PLEIM-XIU LSM WITH IFSNOW=2', &
    & /, 1x, '***   QUESTIONABLE OUTPUT, ESP. SNOWCOVR/WEASD', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9400 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   CANNOT USE SiB LAND USE CATEGORIES WITH MCIP', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9600 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3, &
    & /, 1x, '***   VARIABLE = SMALL HEADER', &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9700 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3, &
    & /, 1x, '***   VARIABLE = ', a, &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9800 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   DID NOT FIND TERRAIN FILE', &
    & /, 1x, '***   WILL NOT USE FRACTIONAL LAND USE DATA', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9810 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR OPENING TERRAIN FILE', &
    & /, 1x, '***   UNIT = ', i3, &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9900 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   INPUT FILES DO NOT SEEM TO BE SAME GRID', &
    & /, 1x, '***   CHECK FORTRAN UNITS ', i3, ' AND ', i3, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9950 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   DID NOT FIND ARRAY ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

  ALLOCATE ( bhi  (numvalsi, numprogs) )
  ALLOCATE ( bhic (numvalsi, numprogs) )
  ALLOCATE ( bhr  (numvalsr, numprogs) )
  ALLOCATE ( bhrc (numvalsr, numprogs) )

  WRITE (*,f6000)

!-------------------------------------------------------------------------------
! Loop through one time period of the MM5V3 output file.  Extract information
! from the MM5 header.  Then, look for the 2-m temperature field, the 2-m
! mixing ratio, the 10-m wind components, the turbulent kinetic energy, and,
! if necessary, vertical structure.
!-------------------------------------------------------------------------------

  ift2m  = .FALSE.      ! initialize flags to false
  ifq2m  = .FALSE.
  ifu10m = .FALSE.
  ifv10m = .FALSE.
  ifw10m = .FALSE.
  iftke  = .FALSE.
  iftkef = .FALSE.

  ifznt      = .TRUE.   ! roughness length is available in MM5, by default
  ifluwrfout = .FALSE.  ! false for MM5; not used

  IF ( needlayers ) THEN
    gotlays = .FALSE.
  ELSE
    gotlays = .TRUE.
  ENDIF

  v3data: DO

    var = 'IFLAG    '
    READ (iutmm, IOSTAT=istat) iflag

    IF ( istat > 0 ) THEN  ! error on read
      WRITE (*,f9000) TRIM(pname), iutmm, istat
      CALL graceful_stop (pname)
    ELSE IF ( istat < 0 ) THEN  ! end-of-file reached
      EXIT v3data
    ENDIF
  
    IF ( iflag == 0 ) THEN
      var = 'BIG HEADR'
      READ (iutmm, IOSTAT=istat) bhi, bhr, bhic, bhrc

      IF ( istat > 0 ) THEN  ! error on read
        WRITE (*,f9000) TRIM(pname), iutmm, istat
        CALL graceful_stop (pname)
      ELSE IF ( istat < 0 ) THEN  ! end-of-file reached
        WRITE (*,f9100) TRIM(pname), iutmm, istat
        CALL graceful_stop (pname)
      ENDIF

!-------------------------------------------------------------------------------
!     Extract NX, NY, and NZ.
!-------------------------------------------------------------------------------

      idx = bhi(1,1)
      IF ( idx /= 11 ) THEN
        WRITE (*,f9300) TRIM(pname), idx
        CALL graceful_stop (pname)
      ENDIF

      met_ny = bhi(16,1)     ! MM5 IX
      met_nx = bhi(17,1)     ! MM5 JX
      met_nz = bhi(12,idx)   ! MM5 KX

      WRITE (*,f6100) met_ny, met_nx, met_nz

      met_nycoarse = bhi(5,1)  ! MM5 IXCOARSE
      met_nxcoarse = bhi(6,1)  ! MM5 JXCOARSE

      WRITE (*,f6200) iuthdr
      CALL getgist (bhi, bhr, bhic, bhrc)

!-------------------------------------------------------------------------------
!     Extract domain attributes.
!-------------------------------------------------------------------------------

      met_gratio    = bhi(20,1)    ! grid ratio w.r.t. coarse domain
      met_y_centd   = bhr(2,1)     ! center latitude [degrees]
      met_proj_clat = bhr(2,1)     ! center latitude of projection [deg]
      met_x_centd   = bhr(3,1)     ! center longitude [degrees]
      met_proj_clon = bhr(3,1)     ! center longitude of projection [deg]
      met_resoln    = bhr(9,1)     ! horizontal grid spacing [m]
      met_y_11      = bhr(10,1)    ! coarse dom loc of this dom's i=1
      met_x_11      = bhr(11,1)    ! coarse dom loc of this dom's j=1
      met_cone_fac  = bhr(4,1)     ! cone factor
      met_tru1      = bhr(5,1)     ! true latitude 1 [degrees]
      met_tru2      = bhr(6,1)     ! true latitude 2 [degrees]

      met_mapproj   = bhi(7,1)     ! map projection

      met_cen_lat   = met_proj_clat  ! [degrees]
      met_cen_lon   = met_proj_clon  ! [degrees]
      met_ref_lat   = met_proj_clat  ! [degrees]

      rictr_coarse  = FLOAT(met_nxcoarse - 1) / 2.0 + 1.0
      met_rictr_dot = (rictr_coarse - met_x_11) * FLOAT(met_gratio) + 1.0

      rjctr_coarse  = FLOAT(met_nycoarse - 1) / 2.0 + 1.0
      met_rjctr_dot = (rjctr_coarse - met_y_11) * FLOAT(met_gratio) + 1.0

      SELECT CASE ( met_mapproj )

        CASE (1)  ! Lambert conformal
          met_p_alp_d = MIN(met_tru1, met_tru2)  ! true latitude 1  [degrees]
          met_p_bet_d = MAX(met_tru1, met_tru2)  ! true latitude 2  [degrees]
          met_p_gam_d = met_x_centd              ! central meridian [degrees]

          CALL ll2xy_lam (met_cen_lat, met_cen_lon, met_tru1, met_tru2,  &
                          met_proj_clon, met_ref_lat, met_xxctr, met_yyctr)

        CASE (2)  ! polar stereographic
          met_p_alp_d = SIGN(1.0, bhr(7,1))      ! +/-1.0 for North/South Pole
          met_p_bet_d = met_tru1                 ! true latitude [degrees]
          met_p_gam_d = met_x_centd              ! central meridian [degrees]

          CALL ll2xy_ps (met_cen_lat, met_cen_lon, met_tru1, met_proj_clon,  &
                         met_xxctr, met_yyctr)

        CASE (3)  ! Mercator
          met_p_alp_d = 0.0                      ! lat of coord origin [deg]
          met_p_bet_d = 0.0                      ! (not used)
          met_p_gam_d = met_proj_clon            ! lon of coord origin [deg]

          CALL ll2xy_merc (met_cen_lat, met_cen_lon, met_proj_clon,  &
                           met_xxctr, met_yyctr)

        CASE DEFAULT
          met_p_bet_d = badval3   ! missing
          met_p_alp_d = badval3   ! missing
          met_p_gam_d = badval3   ! missing

      END SELECT

!-------------------------------------------------------------------------------
!     Extract model run options.
!-------------------------------------------------------------------------------

      met_lu_water   = bhi(23,1)    ! water category in land use

      IF ( ( met_lu_water == 7 ) .OR. ( met_lu_water == -999 ) ) THEN
        nummetlu     = 13
        met_lu_src   = 'MM5'
        met_lu_ice   = 11
        met_lu_urban =  1
        met_lu_lake  = -1 
      ELSE IF ( met_lu_water == 16 ) THEN
        nummetlu     = 24
        met_lu_src   = 'USGS'
        met_lu_ice   = 24
        met_lu_urban =  1
        met_lu_lake  = -1 
      ELSE IF ( met_lu_water == 15 ) THEN
        nummetlu     = 16 
        met_lu_src   = 'SiB'
        met_lu_ice   = 16
        met_lu_urban = -1
        met_lu_lake  = -1 
      ENDIF

      met_restart    = bhi(1,12)    ! "restarted" run? 1=yes, 0=no

      met_lw_rad     = bhi(1,13)    ! longwave radiation scheme
      met_sw_rad     = 1            ! (Dudhia) shortwave radiation scheme
      met_cumulus    = bhi(2,13)    ! cumulus parameterization scheme
      met_shal_cu    = 0            ! shallow convection option
      met_expl_moist = bhi(3,13)    ! explicit moist physics scheme
      met_pbl        = bhi(4,13)    ! PBL scheme
      met_soil_lsm   = bhi(5,13)    ! surface/soil or LSM
      met_sfc_lay    = 1            ! (standard) surface layer scheme

      met_pcp_incr    =  0.0        ! not used with MM5
      met_rain_bucket = -1.0        ! not used with MM5

      met_snow_opt   = bhi(16,13)   ! snow option (0=off, 1=yes/no, 2=liq equiv)

      met_urban_phys = 0            ! urban canopy model (WRF only; always "no")

      met_hybrid     = -1           ! hybrid vert coord (WRF only; always "no")

      met_fdda_3dan  = bhi(1,16)    ! 3d analysis nudging?  1=yes, 0=no
      met_fdda_sfan  = bhi(6,16)    ! sfc analysis nudging?  1=yes, 0=no
      met_fdda_obs   = bhi(14,16)   ! obs nudging?  1=yes, 0=no

      met_fdda_gv3d  = bhr(5,16)    ! 3d analysis nudging coeff. for wind
      met_fdda_gt3d  = bhr(6,16)    ! 3d analysis nudging coeff. for temp.
      met_fdda_gq3d  = bhr(7,16)    ! 3d analysis nudging coeff. for mois.

      met_fdda_gvsfc = bhr(9,16)    ! sfc analysis nudging coeff. for wind
      met_fdda_gtsfc = bhr(10,16)   ! sfc analysis nudging coeff. for temp.
      met_fdda_gqsfc = bhr(11,16)   ! sfc analysis nudging coeff. for mois.

      met_fdda_giv   = bhr(13,16)   ! obs nudging coeff. for wind
      met_fdda_git   = bhr(14,16)   ! obs nudging coeff. for temp.
      met_fdda_giq   = bhr(15,16)   ! obs nudging coeff. for mois.

      IF ( ( met_pbl == 7 ) .AND. ( met_soil_lsm == 3 ) ) THEN  ! Pleim-Xiu
        IF ( met_snow_opt == 2 ) THEN
          WRITE (*,f9350) TRIM(pname)
          CALL graceful_stop (pname)
        ENDIF
        iflai    = .TRUE.
        ifmol    = .TRUE.
        ifresist = .TRUE.
        ifveg    = .TRUE.
        ifwr     = .TRUE.
        ifsoil   = .TRUE.
        met_ns   =  2
      ELSE IF ( met_soil_lsm == 2 ) THEN  ! NOAH LSM
        iflai    = .FALSE.
        ifmol    = .FALSE.
        ifresist = .FALSE.
        ifveg    = .TRUE.
        ifwr     = .TRUE.
        ifsoil   = .FALSE.
        met_ns   =  4
      ELSE
        iflai    = .FALSE.
        ifmol    = .FALSE.
        ifresist = .FALSE.
        ifveg    = .FALSE.
        ifwr     = .FALSE.
        ifsoil   = .FALSE.
        met_ns   =  0
      ENDIF

      ifmolpx = .FALSE.

      SELECT CASE ( met_expl_moist )

        CASE ( 0:2 )  ! simple microphysics, no ice
          nqspecies = 0

        CASE ( 3:4 )  ! warm rain or simple ice scheme
          nqspecies = 2
 
        CASE (  5  )  ! mixed-phase with ice and snow 
          nqspecies = 4

        CASE ( 6:8 )  ! mixed-phase plus graupel
          nqspecies = 5

        CASE DEFAULT
          nqspecies = 0

      END SELECT

      ifcld3d = .FALSE.

      IF ( met_lu_water == 15 ) THEN  ! SiB land-use database
        WRITE (*,f9400) TRIM(pname)
        CALL graceful_stop (pname)
      ENDIF

!-------------------------------------------------------------------------------
!     Extract MM5 start date and time information.
!-------------------------------------------------------------------------------

      yyyy = bhi(5,11)
      mm   = bhi(6,11)
      dd   = bhi(7,11)
      hh   = bhi(8,11)
      mins = bhi(9,11)

      WRITE ( yyyyc, '(i4.4)' ) yyyy
      WRITE ( mmc,   '(i2.2)' ) mm
      WRITE ( ddc,   '(i2.2)' ) dd
      WRITE ( hhc,   '(i2.2)' ) hh
      WRITE ( minc,  '(i2.2)' ) mins

      met_startdate =  &
          yyyyc //'-'// mmc //'-'// ddc //'-'// hhc //':'// minc // ':00.0000'

      met_tapfrq = bhr(4,12)
  
!-------------------------------------------------------------------------------
!     Extract non-hydrostatic reference variables from header.  There is no
!     option for hydrostatic run in standard releases of MM5v3, so these
!     variables will always be defined.
!-------------------------------------------------------------------------------

      met_ptop  = bhr(2,2)  ! model top [Pa]
      met_p00   = bhr(2,5)  ! base state sea-level pressure [Pa]
      met_ts0   = bhr(3,5)  ! base state sea-level temperature [K]
      met_tlp   = bhr(4,5)  ! base state lapse rate d(T)/d(ln P)
      met_tiso  = bhr(5,5)  ! base state stratospheric isothermal T [K]

!-------------------------------------------------------------------------------
!     Build release from header information.
!       Note:  May be incorrect for data converted from V2 format to V3 format.
!-------------------------------------------------------------------------------

      met_release = 'V3      '

      version = bhi(3,11)  ! MM5 program version number
      IF ( version /= -999 ) THEN
        WRITE (verc, '(i1)') version
        met_release(3:4) = '.' // verc
        minorrev = bhi(4,11)  ! MM5 program minor revision number
        IF ( minorrev /= -999 ) THEN
          WRITE (mrev, '(i1)') minorrev
          met_release(5:6) = '.' // mrev
        ENDIF
      ENDIF

!-------------------------------------------------------------------------------
!   Read fields in MM5 output.
!-------------------------------------------------------------------------------

    ELSE IF ( iflag == 1 ) THEN

      var = 'SM HEADER'
      READ (iutmm, IOSTAT=istat) ndim, start_index, end_index, timeout,  &
            staggering, ordering, currentdate, vname, units, description

      IF ( istat > 0 ) THEN  ! error on read
        WRITE (*,f9000) TRIM(pname), iutmm, istat
        CALL graceful_stop (pname)
      ELSE IF ( istat < 0 ) THEN  ! end-of-file reached
        WRITE (*,f9600) TRIM(pname), iutmm, istat
        CALL graceful_stop (pname)
      ENDIF

      IF ( vname == 'T2       ' ) THEN
        ift2m = .TRUE.
      ENDIF

      IF ( vname == 'Q2       ' ) THEN
        ifq2m = .TRUE.
      ENDIF

      IF ( vname == 'U10      ' ) THEN
        ifu10m = .TRUE.
      ENDIF

      IF ( vname == 'V10      ' ) THEN
        ifv10m = .TRUE.
      ENDIF

      IF ( vname == 'TKE      ' ) THEN
        iftke = .TRUE.
      ENDIF

      IF ( ndim == 1 ) THEN
        ALLOCATE ( data_in(end_index(1), 1, 1, 1) )
      ELSE IF ( ndim == 2 ) THEN
        ALLOCATE ( data_in(end_index(1), end_index(2), 1, 1) )
      ELSE IF ( ndim == 3 ) THEN
        ALLOCATE ( data_in(end_index(1), end_index(2), end_index(3), 1) )
      ELSE IF ( ndim == 4 ) THEN
        ALLOCATE ( data_in(end_index(1), end_index(2), end_index(3),  &
                           end_index(4)))
      ENDIF

      var = vname
      READ (iutmm, IOSTAT=istat) data_in

      IF ( istat > 0 ) THEN  ! error on read
        WRITE (*,f9000) TRIM(pname), iutter, istat
        CALL graceful_stop (pname)
      ELSE IF ( istat < 0 ) THEN  ! end-of-file reached
        WRITE (*,f9700) TRIM(pname), iutter, TRIM(var), istat
        CALL graceful_stop (pname)
      ENDIF

      IF ( ( vname == 'SIGMAH   ' ) .AND. ( .NOT. gotlays ) ) THEN
        nlays = met_nz
        ALLOCATE ( temp1d (SIZE(data_in,1)) )
        temp1d(:) = data_in(met_nz:1:-1,1,1,1)
        ctmlays(1)        = 1.0
        ctmlays(met_nz+1) = 0.0
        DO k = 2, met_nz
          km1 = k - 1
          ctmlays(k) = ( 2.0 * temp1d(km1) ) - ctmlays(km1)
        ENDDO
        DEALLOCATE ( temp1d)
        gotlays = .TRUE.
      ENDIF

      DEALLOCATE ( data_in )

!-------------------------------------------------------------------------------
!   End-of-period flag found.
!-------------------------------------------------------------------------------

    ELSE IF ( iflag == 2 ) THEN

      IF ( ( ifu10m ) .AND. ( ifv10m ) ) THEN
        ifw10m = .TRUE.
      ENDIF

      IF ( iftke ) THEN
        IF ( ( met_pbl == 3 ) .OR.  &   ! Burk-Thompson
             ( met_pbl == 4 ) ) THEN    ! Mellor-Yamada (Eta)
          iftkef = .FALSE.  ! TKE on half-levels
        ELSE IF ( met_pbl == 6 ) THEN   ! Gayno-Seaman
          iftkef = .TRUE.   ! TKE on full-levels
        ENDIF
      ELSE
        iftkef = .FALSE.
      ENDIF

      EXIT v3data

    ELSE

      WRITE (*,f9200) TRIM(pname), iflag, iutmm
      CALL graceful_stop (pname)

    ENDIF

  ENDDO v3data

!-------------------------------------------------------------------------------
! Make sure we collected the arrays we need.
!-------------------------------------------------------------------------------

  IF ( .NOT. gotlays ) THEN
    WRITE (*,f9950) TRIM(pname), 'SIGMAH'
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Rewind model output file.
!-------------------------------------------------------------------------------

  REWIND (iutmm)

!-------------------------------------------------------------------------------
! Determine if TERRAIN file exists.  If so, make sure the domain is the same
! as contained in the MM5 file, above.  Then determine if the fractional
! land use fields are in the file.
!-------------------------------------------------------------------------------

  terfile = TRIM( file_ter )
  INQUIRE ( FILE=terfile, EXIST=ifter )

  IF ( .NOT. ifter ) THEN

    WRITE (*,f9800) TRIM(pname)
    iflufrc = .FALSE.

  ELSE

    OPEN (UNIT=iutter,  FILE=terfile, FORM='UNFORMATTED', STATUS='OLD',  &
          IOSTAT=istat)

    IF ( istat > 0 ) THEN  ! error on open
      WRITE (*,f9810) TRIM(pname), iutter, istat
      CALL graceful_stop (pname)
    ENDIF

    terdata: DO

      var = 'IFLAG    '
      READ (iutter, IOSTAT=istat) iflag

      IF ( istat > 0 ) THEN  ! error on read
        WRITE (*,f9000) TRIM(pname), iutter, istat
        CALL graceful_stop (pname)
      ELSE IF ( istat < 0 ) THEN  ! end-of-file reached
        EXIT terdata
      ENDIF
  
      IF ( iflag == 0 ) THEN
        var = 'BIG HEADR'
        READ (iutter, IOSTAT=istat) bhi, bhr, bhic, bhrc

        IF ( istat > 0 ) THEN  ! error on read
          WRITE (*,f9000) TRIM(pname), iutter, istat
          CALL graceful_stop (pname)
        ELSE IF ( istat < 0 ) THEN  ! end-of-file reached
          WRITE (*,f9100) TRIM(pname), iutter, istat
          CALL graceful_stop (pname)
        ENDIF

        ! Verify that the header for this file matches the MM5 file above.

        ok = ok .AND. ( met_ny          == bhi(16,1) )
        ok = ok .AND. ( met_nx          == bhi(17,1) )
        ok = ok .AND. ( met_nycoarse    == bhi(5,1)  )
        ok = ok .AND. ( met_nxcoarse    == bhi(6,1)  )
        ok = ok .AND. ( met_mapproj     == bhi(7,1)  )
        ok = ok .AND. ( met_gratio      == bhi(20,1) )

        ok = ok .AND. ( ABS(met_y_centd  - bhr(2,1)) < tol )
        ok = ok .AND. ( ABS(met_x_centd  - bhr(3,1)) < tol )
        ok = ok .AND. ( ABS(met_tru1     - bhr(5,1)) < tol )
        ok = ok .AND. ( ABS(met_cone_fac - bhr(4,1)) < tol )

        SELECT CASE ( met_mapproj )
          CASE (1)  ! Lambert conformal
            ok = ok .AND. ( ABS(met_p_alp_d - MIN(bhr(5,1),bhr(6,1))) < tol )
            ok = ok .AND. ( ABS(met_p_bet_d - MAX(bhr(5,1),bhr(6,1))) < tol )
            ok = ok .AND. ( ABS(met_p_gam_d - bhr(3,1))               < tol )
          CASE (2)  ! polar stereographic
            ok = ok .AND. ( ABS(met_p_alp_d - SIGN(1.0,bhr(7,1)))     < tol )
            ok = ok .AND. ( ABS(met_p_bet_d - bhr(5,1))               < tol )
            ok = ok .AND. ( ABS(met_p_gam_d - bhr(3,1))               < tol )
          CASE (3)  ! Mercator
            ok = ok .AND. ( ABS(met_p_gam_d - bhr(3,1))               < tol )
        END SELECT

        ok = ok .AND. ( ABS(met_resoln     - bhr(9,1))  < tol )
        ok = ok .AND. ( ABS(met_y_11       - bhr(10,1)) < tol )
        ok = ok .AND. ( ABS(met_x_11       - bhr(11,1)) < tol )
        ok = ok .AND. ( met_lu_water      == bhi(23,1)  )

        IF ( .NOT. ok ) THEN
          WRITE (*,f9900) TRIM(pname), iutter, iutmm
          CALL graceful_stop (pname)
        ENDIF

      ELSE IF ( iflag == 1 ) THEN

        var = 'SM HEADER'
        READ (iutter, IOSTAT=istat) ndim, start_index, end_index, timeout,  &
              staggering, ordering, currentdate, vname, units, description

        IF ( istat > 0 ) THEN  ! error on read
          WRITE (*,f9000) TRIM(pname), iutter, istat
          CALL graceful_stop (pname)
        ELSE IF ( istat < 0 ) THEN  ! end-of-file reached
          WRITE (*,f9600) TRIM(pname), iutter, istat
          CALL graceful_stop (pname)
        ENDIF

        IF ( ndim == 1 ) THEN
          ALLOCATE ( data_in(end_index(1), 1, 1, 1) )
        ELSE IF ( ndim == 2 ) THEN
          ALLOCATE ( data_in(end_index(1), end_index(2), 1, 1) )
        ELSE IF ( ndim == 3 ) THEN
          ALLOCATE ( data_in(end_index(1), end_index(2), end_index(3), 1) )
        ELSE IF ( ndim == 4 ) THEN
          ALLOCATE ( data_in(end_index(1), end_index(2), end_index(3),  &
                             end_index(4)))
        ENDIF

        IF ( vname(1:6) == 'VEGCAT' ) THEN
          iflufrc = .TRUE.
          EXIT terdata
        ENDIF

        var = vname
        READ (iutter, IOSTAT=istat) data_in

        IF ( istat > 0 ) THEN  ! error on read
          WRITE (*,f9000) TRIM(pname), iutter, istat
          CALL graceful_stop (pname)
        ELSE IF ( istat < 0 ) THEN  ! end-of-file reached
          WRITE (*,f9700) TRIM(pname), iutter, TRIM(var), istat
          CALL graceful_stop (pname)
        ENDIF

        DEALLOCATE ( data_in )

      ELSE IF ( iflag == 2 ) THEN

        iflufrc = .FALSE.  ! did not find fractional land use data in TERRAIN file
        EXIT terdata

      ELSE

        WRITE (*,f9200) TRIM(pname), iflag, iutter
        CALL graceful_stop (pname)

      ENDIF

    ENDDO terdata

    REWIND (iutter)

  ENDIF

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( bhi  )
  DEALLOCATE ( bhic )
  DEALLOCATE ( bhr  )
  DEALLOCATE ( bhrc )

END SUBROUTINE setup_mm5v3
