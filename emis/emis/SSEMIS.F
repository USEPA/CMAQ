
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/SSEMIS.F,v 1.2 2011/02/23 16:33:18 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE SSEMIS

      USE HGRD_DEFN

C number of chemical species in fresh sea-salt aerosol
      INTEGER, PARAMETER :: NSSSPC = 4

C Indices to sea-salt species in mass-emission arrays
      INTEGER, PARAMETER :: KNA  = 1   ! position of sodium
      INTEGER, PARAMETER :: KCL  = 2   ! position of chloride
      INTEGER, PARAMETER :: KSO4 = 3   ! position of sulfate
      INTEGER, PARAMETER :: KH2O = 4   ! position of water

C number of lognormal modes in sea-salt aerosol (only accumulation and coarse modes used)
      INTEGER, PARAMETER :: NSSMODE = 3

C Sea-Salt Emissions Rates
!     REAL  SSOUTM( NSSSPC,NSSMODE,NCOLS,NROWS )   ! mass emission rates [ug/m3/s]
!     REAL  SSOUTN( NSSMODE,NCOLS,NROWS )          ! number emission rates [1/m3/s]
!     REAL  SSOUTS( NSSMODE,NCOLS,NROWS )          ! surface-area emisrates [m2/m3/s]
      REAL, ALLOCATABLE, SAVE :: SSOUTM( :,:,:,: )  ! mass emission rates [ug/m3/s]
      REAL, ALLOCATABLE, SAVE :: SSOUTN( :,:,: )    ! number emission rates [1/m3/s]
      REAL, ALLOCATABLE, SAVE :: SSOUTS( :,:,: )    ! surface-area emisrates [m2/m3/s]

C Variables for handling sea-salt emissions
!     REAL, PRIVATE :: OCEAN( NCOLS,NROWS )       ! fractional sea water cover
!     REAL, PRIVATE :: SZONE( NCOLS,NROWS )       ! fractional surf-zone cover
      REAL, PRIVATE, ALLOCATABLE, SAVE :: OCEAN( :,: )  ! fractional sea water cover
      REAL, PRIVATE, ALLOCATABLE, SAVE :: SZONE( :,: )  ! fractional surf-zone cover

C Variables for the sea-salt diagnostic file
!     INTEGER, PRIVATE, PARAMETER :: NSSDIAG = 12 ! number of species in sea-salt
      INTEGER,          PARAMETER :: NSSDIAG = 12 ! number of species in sea-salt
                                                  ! diagnostic emission file
      REAL,    PRIVATE               :: SSOUTD( NSSDIAG )    ! emission rates
      LOGICAL, PRIVATE, SAVE         :: SSEMDIAG   ! flag for SSEMIS diagnostic file
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: SSBF( :,:,: ) ! seasalt emiss accumulator
      REAL,    PRIVATE, ALLOCATABLE, SAVE :: WRSS( :,: )   ! seasalt emiss write buffer
      INTEGER, PRIVATE, SAVE         :: SDATE, STIME  ! scenario start date & time

C Species names in the speciated sea-salt-emissions
      CHARACTER( 16 ), SAVE :: WRSS_SPC( NSSDIAG )  ! species names
      DATA WRSS_SPC(  1 ) / 'ANAJ'  /   ! accumulation mode sodium
      DATA WRSS_SPC(  2 ) / 'ACLJ'  /   ! accumulation mode chloride
      DATA WRSS_SPC(  3 ) / 'ASO4J' /   ! accumulation mode sulfate
      DATA WRSS_SPC(  4 ) / 'AH2OJ' /   ! accumulation mode water
      DATA WRSS_SPC(  5 ) / 'ANAK'  /   ! coarse mode sodium
      DATA WRSS_SPC(  6 ) / 'ACLK'  /   ! coarse mode chloride
      DATA WRSS_SPC(  7 ) / 'ASO4K' /   ! coarse mode sulfate
      DATA WRSS_SPC(  8 ) / 'AH2OK' /   ! coarse mode water
      DATA WRSS_SPC(  9 ) / 'ANUMJ' /   ! accumulation mode number
      DATA WRSS_SPC( 10 ) / 'ANUMK' /   ! coarse mode number
      DATA WRSS_SPC( 11 ) / 'ASRFJ' /   ! accumulation mode surface area
      DATA WRSS_SPC( 12 ) / 'ASRFK' /   ! coarse mode surface area

C Variables interpolated from the meteorological input files
!     REAL  PRES( NCOLS,NROWS )                   ! atmospheric pressure [Pa]
!     REAL  TA  ( NCOLS,NROWS )                   ! air temperature [K]
!     REAL  QV  ( NCOLS,NROWS )                   ! H2O mass mixing ratio [kg/kg]
!     REAL  WSPD10( NCOLS,NROWS )                 ! wind speed at 10m [m/s]
      REAL, PRIVATE, ALLOCATABLE, SAVE :: PRES( :,: )    ! atmospheric pressure [Pa]
      REAL, PRIVATE, ALLOCATABLE, SAVE :: TA  ( :,: )    ! air temperature [K]
      REAL, PRIVATE, ALLOCATABLE, SAVE :: QV  ( :,: )    ! H2O mass mixing ratio [kg/kg]
      REAL, PRIVATE, ALLOCATABLE, SAVE :: WSPD10( :,: )  ! wind speed at 10m [m/s]

C Lognormal parameters fit to the open-ocean flux function
      REAL, PRIVATE, SAVE :: DGNJ( 136 )     ! geom mean diam of accum-mode [um]
      REAL, PRIVATE, SAVE :: DGNK( 136 )     ! geom mean diam of coarse-mode [um]
      REAL, PRIVATE, SAVE :: SIGJ( 136 )     ! geom std deviation of accum-mode flux
      REAL, PRIVATE, SAVE :: SIGK( 136 )     ! geom std deviation of coarse-mode flux
      REAL, PRIVATE, SAVE :: FNJ ( 136 )     ! magnitude of accum-mode flux [1/m2/s]
      REAL, PRIVATE, SAVE :: FNK ( 136 )     ! magnitude of coarse-mode flux [1/m2/s]

C Open-ocean flux numerically integrated over size range of interest
      REAL, PRIVATE, SAVE :: VFLUX( 136 )    ! total particle volume flux [m3/m2/s]

C Polycoeffs from Zhang et al. (2005)
      REAL( 8 ), PRIVATE, SAVE :: C0_COEFF( 6 )   ! Eq 8
      REAL( 8 ), PRIVATE, SAVE :: X_COEFF( 6 )    ! Eq 1

C-------------------------- Data Statements ----------------------------

C RH-dependent values calculated using MATLAB

C Geometric mean diameter of accumulation mode [um]
      DATA ( DGNJ( I ), I = 1, 136 ) /
     &      0.2651, 0.2677, 0.2696, 0.2713, 0.2736, 0.2752, 0.2768, 0.2784, 
     &      0.2806, 0.2823, 0.2840, 0.2858, 0.2883, 0.2903, 0.2924, 0.2951, 
     &      0.2974, 0.2998, 0.3028, 0.3054, 0.3080, 0.3113, 0.3140, 0.3174, 
     &      0.3203, 0.3232, 0.3267, 0.3297, 0.3332, 0.3363, 0.3394, 0.3430, 
     &      0.3462, 0.3499, 0.3533, 0.3573, 0.3611, 0.3652, 0.3700, 0.3752, 
     &      0.3806, 0.3871, 0.3945, 0.4030, 0.4129, 0.4246, 0.4259, 0.4271, 
     &      0.4284, 0.4298, 0.4311, 0.4328, 0.4342, 0.4356, 0.4371, 0.4385, 
     &      0.4401, 0.4416, 0.4434, 0.4451, 0.4467, 0.4484, 0.4501, 0.4518, 
     &      0.4538, 0.4556, 0.4575, 0.4594, 0.4613, 0.4633, 0.4655, 0.4676, 
     &      0.4697, 0.4718, 0.4740, 0.4764, 0.4787, 0.4811, 0.4835, 0.4859, 
     &      0.4886, 0.4912, 0.4938, 0.4965, 0.4994, 0.5022, 0.5051, 0.5081, 
     &      0.5112, 0.5143, 0.5175, 0.5208, 0.5242, 0.5276, 0.5312, 0.5348, 
     &      0.5386, 0.5424, 0.5463, 0.5503, 0.5544, 0.5587, 0.5631, 0.5675, 
     &      0.5722, 0.5769, 0.5818, 0.5867, 0.5919, 0.5972, 0.6026, 0.6083, 
     &      0.6140, 0.6201, 0.6263, 0.6325, 0.6392, 0.6458, 0.6529, 0.6600, 
     &      0.6676, 0.6751, 0.6833, 0.6914, 0.7001, 0.7088, 0.7177, 0.7276, 
     &      0.7372, 0.7478, 0.7582, 0.7690, 0.7810, 0.7928, 0.8050, 0.8187 / 

C Geometric mean diameter of coarse mode [um]
      DATA ( DGNK( I ), I = 1, 136 ) /
     &      1.7618, 1.8130, 1.8257, 1.8375, 1.8861, 1.8973, 1.9084, 1.9194, 
     &      1.9678, 1.9796, 1.9919, 2.0046, 2.0548, 2.0690, 2.0840, 2.1359, 
     &      2.1525, 2.1698, 2.2236, 2.2423, 2.2617, 2.3171, 2.3378, 2.3936, 
     &      2.4154, 2.4376, 2.4940, 2.5169, 2.5732, 2.5969, 2.6209, 2.6776, 
     &      2.7027, 2.7599, 2.7868, 2.8453, 2.8754, 2.9076, 2.9723, 3.0398, 
     &      3.0835, 3.1604, 3.2439, 3.3357, 3.4386, 3.5555, 3.5660, 3.5767, 
     &      3.5876, 3.5986, 3.6099, 3.6421, 3.6539, 3.6659, 3.6782, 3.6906, 
     &      3.7034, 3.7163, 3.7487, 3.7623, 3.7761, 3.7903, 3.8047, 3.8194, 
     &      3.8519, 3.8673, 3.8830, 3.8990, 3.9154, 3.9321, 3.9651, 3.9826, 
     &      4.0005, 4.0188, 4.0374, 4.0710, 4.0906, 4.1106, 4.1311, 4.1520, 
     &      4.1860, 4.2080, 4.2305, 4.2535, 4.2878, 4.3120, 4.3367, 4.3621, 
     &      4.3969, 4.4236, 4.4510, 4.4790, 4.5146, 4.5442, 4.5745, 4.6104, 
     &      4.6424, 4.6752, 4.7089, 4.7463, 4.7816, 4.8182, 4.8565, 4.8952, 
     &      4.9350, 4.9746, 5.0169, 5.0563, 5.1010, 5.1470, 5.1884, 5.2369, 
     &      5.2789, 5.3308, 5.3844, 5.4289, 5.4856, 5.5313, 5.5922, 5.6394, 
     &      5.7042, 5.7532, 5.8228, 5.8741, 5.9486, 6.0022, 6.0564, 6.1392, 
     &      6.1968, 6.2860, 6.3476, 6.4099, 6.5098, 6.5772, 6.6466, 6.7590 /

C Geometric standard deviation of accumulation mode
      DATA ( SIGJ( I ), I = 1, 136 ) /
     &      1.7572, 1.7612, 1.7612, 1.7612, 1.7651, 1.7651, 1.7651, 1.7651, 
     &      1.7689, 1.7689, 1.7689, 1.7689, 1.7727, 1.7727, 1.7727, 1.7764, 
     &      1.7764, 1.7764, 1.7801, 1.7801, 1.7801, 1.7837, 1.7837, 1.7873, 
     &      1.7873, 1.7873, 1.7907, 1.7907, 1.7941, 1.7941, 1.7941, 1.7974, 
     &      1.7974, 1.8006, 1.8006, 1.8037, 1.8037, 1.8037, 1.8066, 1.8095, 
     &      1.8095, 1.8122, 1.8147, 1.8172, 1.8195, 1.8217, 1.8217, 1.8217, 
     &      1.8217, 1.8217, 1.8217, 1.8237, 1.8237, 1.8237, 1.8237, 1.8237, 
     &      1.8237, 1.8237, 1.8255, 1.8255, 1.8255, 1.8255, 1.8255, 1.8255, 
     &      1.8272, 1.8272, 1.8272, 1.8272, 1.8272, 1.8272, 1.8287, 1.8287, 
     &      1.8287, 1.8287, 1.8287, 1.8300, 1.8300, 1.8300, 1.8300, 1.8300, 
     &      1.8312, 1.8312, 1.8312, 1.8312, 1.8322, 1.8322, 1.8322, 1.8322, 
     &      1.8330, 1.8330, 1.8330, 1.8330, 1.8336, 1.8336, 1.8336, 1.8340, 
     &      1.8340, 1.8340, 1.8340, 1.8342, 1.8342, 1.8342, 1.8343, 1.8343, 
     &      1.8343, 1.8342, 1.8342, 1.8339, 1.8339, 1.8339, 1.8334, 1.8334, 
     &      1.8327, 1.8327, 1.8327, 1.8318, 1.8318, 1.8307, 1.8307, 1.8295, 
     &      1.8295, 1.8281, 1.8281, 1.8266, 1.8266, 1.8248, 1.8229, 1.8230, 
     &      1.8209, 1.8209, 1.8188, 1.8164, 1.8164, 1.8140, 1.8115, 1.8115 /
              
C Geometric standard deviation of coarse mode
      DATA ( SIGK( I ), I = 1, 136 ) /
     &      2.1039, 2.0834, 2.0834, 2.0834, 2.0639, 2.0639, 2.0639, 2.0639, 
     &      2.0452, 2.0452, 2.0452, 2.0452, 2.0273, 2.0273, 2.0273, 2.0101, 
     &      2.0101, 2.0101, 1.9937, 1.9937, 1.9937, 1.9780, 1.9780, 1.9631, 
     &      1.9631, 1.9631, 1.9488, 1.9488, 1.9351, 1.9351, 1.9351, 1.9222, 
     &      1.9222, 1.9098, 1.9098, 1.8981, 1.8981, 1.8981, 1.8870, 1.8765, 
     &      1.8765, 1.8666, 1.8573, 1.8486, 1.8405, 1.8329, 1.8329, 1.8329, 
     &      1.8329, 1.8329, 1.8329, 1.8259, 1.8259, 1.8259, 1.8259, 1.8259, 
     &      1.8259, 1.8259, 1.8195, 1.8195, 1.8195, 1.8195, 1.8195, 1.8195, 
     &      1.8137, 1.8137, 1.8137, 1.8137, 1.8137, 1.8137, 1.8085, 1.8085, 
     &      1.8085, 1.8085, 1.8085, 1.8038, 1.8038, 1.8038, 1.8038, 1.8038, 
     &      1.7997, 1.7997, 1.7997, 1.7997, 1.7963, 1.7963, 1.7963, 1.7963, 
     &      1.7934, 1.7934, 1.7934, 1.7934, 1.7913, 1.7913, 1.7913, 1.7897, 
     &      1.7897, 1.7897, 1.7897, 1.7888, 1.7889, 1.7889, 1.7886, 1.7886, 
     &      1.7886, 1.7891, 1.7891, 1.7904, 1.7903, 1.7903, 1.7923, 1.7924, 
     &      1.7952, 1.7952, 1.7952, 1.7988, 1.7989, 1.8034, 1.8034, 1.8089, 
     &      1.8090, 1.8155, 1.8154, 1.8230, 1.8230, 1.8318, 1.8417, 1.8417, 
     &      1.8529, 1.8529, 1.8656, 1.8799, 1.8799, 1.8960, 1.9138, 1.9138 /
              
C Number flux in accumulation mode at 1 m/s [1/m2/s]
            DATA ( FNJ( I ), I = 1, 136 ) /
     &      89.326, 89.524, 89.524, 89.524, 89.709, 89.709, 89.709, 89.709, 
     &      89.881, 89.881, 89.881, 89.881, 90.040, 90.040, 90.040, 90.188, 
     &      90.188, 90.188, 90.325, 90.324, 90.324, 90.451, 90.451, 90.569, 
     &      90.569, 90.569, 90.678, 90.678, 90.779, 90.779, 90.779, 90.873, 
     &      90.873, 90.959, 90.959, 91.039, 91.039, 91.039, 91.113, 91.181, 
     &      91.181, 91.243, 91.301, 91.353, 91.401, 91.445, 91.445, 91.445, 
     &      91.445, 91.445, 91.445, 91.484, 91.484, 91.484, 91.484, 91.484, 
     &      91.484, 91.484, 91.520, 91.520, 91.520, 91.520, 91.520, 91.520, 
     &      91.551, 91.551, 91.551, 91.551, 91.551, 91.551, 91.579, 91.579, 
     &      91.579, 91.579, 91.579, 91.604, 91.604, 91.604, 91.604, 91.604, 
     &      91.625, 91.625, 91.625, 91.625, 91.642, 91.642, 91.642, 91.642, 
     &      91.656, 91.656, 91.656, 91.656, 91.667, 91.667, 91.667, 91.675, 
     &      91.675, 91.675, 91.675, 91.679, 91.679, 91.679, 91.680, 91.680, 
     &      91.680, 91.678, 91.678, 91.672, 91.672, 91.672, 91.663, 91.663, 
     &      91.650, 91.650, 91.650, 91.633, 91.633, 91.612, 91.612, 91.587, 
     &      91.587, 91.558, 91.558, 91.524, 91.524, 91.485, 91.441, 91.441, 
     &      91.392, 91.392, 91.337, 91.274, 91.274, 91.205, 91.129, 91.129 /
              
C Number flux in coarse mode at 1 m/s [1/m2/s]
      DATA ( FNK( I ), I = 1, 136 ) /
     &      11.315, 11.110, 11.110, 11.109, 10.919, 10.919, 10.919, 10.919, 
     &      10.740, 10.740, 10.740, 10.740, 10.573, 10.573, 10.573, 10.417, 
     &      10.417, 10.417, 10.270, 10.271, 10.271, 10.134, 10.134, 10.006, 
     &      10.006, 10.006, 9.886, 9.886, 9.774, 9.774, 9.774, 9.669, 
     &      9.669, 9.571, 9.571, 9.480, 9.480, 9.480, 9.395, 9.315, 
     &      9.315, 9.243, 9.174, 9.111, 9.053, 8.999, 8.999, 8.999, 
     &      9.000, 9.000, 9.000, 8.951, 8.951, 8.951, 8.951, 8.951, 
     &      8.951, 8.951, 8.907, 8.907, 8.907, 8.907, 8.907, 8.907, 
     &      8.867, 8.867, 8.867, 8.867, 8.867, 8.867, 8.832, 8.832, 
     &      8.832, 8.832, 8.832, 8.800, 8.800, 8.800, 8.800, 8.800, 
     &      8.773, 8.773, 8.773, 8.773, 8.751, 8.751, 8.751, 8.751, 
     &      8.732, 8.732, 8.732, 8.732, 8.718, 8.718, 8.718, 8.708, 
     &      8.708, 8.708, 8.708, 8.702, 8.703, 8.703, 8.701, 8.701, 
     &      8.701, 8.704, 8.704, 8.712, 8.712, 8.711, 8.724, 8.725, 
     &      8.742, 8.742, 8.742, 8.765, 8.765, 8.794, 8.794, 8.828, 
     &      8.828, 8.869, 8.869, 8.915, 8.915, 8.970, 9.031, 9.031, 
     &      9.101, 9.101, 9.179, 9.268, 9.268, 9.366, 9.476, 9.476 /
              
C Volume flux at 1 m/s numerically integrated over full size range [m3/m2/s]
      DATA ( VFLUX( I ), I = 1, 136 ) /
     &      3.3799E-16, 3.4012E-16, 3.4728E-16, 3.5407E-16, 3.5489E-16,  
     &      3.6124E-16, 3.6758E-16, 3.7402E-16, 3.7473E-16, 3.8152E-16,  
     &      3.8864E-16, 3.9617E-16, 3.9799E-16, 4.0632E-16, 4.1518E-16,  
     &      4.1825E-16, 4.2807E-16, 4.3846E-16, 4.4284E-16, 4.5420E-16,  
     &      4.6612E-16, 4.7169E-16, 4.8449E-16, 4.9073E-16, 5.0429E-16,  
     &      5.1829E-16, 5.2529E-16, 5.3992E-16, 5.4734E-16, 5.6260E-16,  
     &      5.7831E-16, 5.8648E-16, 6.0311E-16, 6.1215E-16, 6.3024E-16,  
     &      6.4087E-16, 6.6140E-16, 6.8386E-16, 6.9948E-16, 7.1775E-16,  
     &      7.4926E-16, 7.7574E-16, 8.0807E-16, 8.4803E-16, 8.9805E-16,  
     &      9.6150E-16, 9.7003E-16, 9.7878E-16, 9.8774E-16, 9.9692E-16,  
     &      1.0063E-15, 1.0026E-15, 1.0124E-15, 1.0224E-15, 1.0327E-15,  
     &      1.0432E-15, 1.0541E-15, 1.0652E-15, 1.0624E-15, 1.0739E-15,  
     &      1.0858E-15, 1.0980E-15, 1.1106E-15, 1.1235E-15, 1.1216E-15,  
     &      1.1351E-15, 1.1490E-15, 1.1633E-15, 1.1780E-15, 1.1931E-15,  
     &      1.1924E-15, 1.2083E-15, 1.2246E-15, 1.2415E-15, 1.2588E-15,  
     &      1.2593E-15, 1.2776E-15, 1.2964E-15, 1.3159E-15, 1.3360E-15,  
     &      1.3380E-15, 1.3592E-15, 1.3811E-15, 1.4038E-15, 1.4071E-15,  
     &      1.4311E-15, 1.4559E-15, 1.4816E-15, 1.4867E-15, 1.5139E-15,  
     &      1.5422E-15, 1.5715E-15, 1.5786E-15, 1.6098E-15, 1.6422E-15,  
     &      1.6509E-15, 1.6855E-15, 1.7215E-15, 1.7590E-15, 1.7706E-15,  
     &      1.8107E-15, 1.8526E-15, 1.8666E-15, 1.9115E-15, 1.9585E-15,  
     &      1.9754E-15, 2.0260E-15, 2.0447E-15, 2.0993E-15, 2.1565E-15,  
     &      2.1790E-15, 2.2411E-15, 2.2660E-15, 2.3335E-15, 2.4045E-15, 
     &      2.4346E-15, 2.5121E-15, 2.5456E-15, 2.6304E-15, 2.6678E-15, 
     &      2.7610E-15, 2.8028E-15, 2.9056E-15, 2.9525E-15, 3.0662E-15, 
     &      3.1191E-15, 3.1734E-15, 3.3053E-15, 3.3669E-15, 3.5144E-15, 
     &      3.5847E-15, 3.6572E-15, 3.8310E-15, 3.9146E-15, 4.0012E-15, 
     &      4.2082E-15 /                                                  

C Polynomial coefficients from Zhang et al. (2005)         
      DATA C0_COEFF / 28.376, -205.44, 653.37, -1031.7, 803.18, -247.08 /
      DATA X_COEFF  / 3.1657, -19.079,  55.72, -83.998, 63.436, -19.248 /

C Domain decomposition info from emission and meteorology files
      INTEGER, PRIVATE, SAVE :: GXOFF, GYOFF             ! origin offset
      INTEGER, PRIVATE, SAVE :: STRTCOL_O1, ENDCOL_O1, STRTROW_O1, ENDROW_O1
      INTEGER, PRIVATE, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

      INTEGER, PRIVATE, SAVE :: LOGDEV

      CONTAINS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         FUNCTION SSEMIS_INIT( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

C Revision History:
C   26 Feb 10 J.Young:  Eliminate BARRIER, etc. to prevent MPI race condition

            IMPLICIT NONE

C Arguments:
            INTEGER :: JDATE, JTIME, TSTEP
            LOGICAL :: SUCCESS

C Includes:
            INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
            INCLUDE SUBST_IODECL    ! I/O definitions and declarations
!           INCLUDE SUBST_IOFDESC   ! file header data structure
            INCLUDE SUBST_FILES_ID  ! file name parameters

C External Functions:
            INTEGER, EXTERNAL :: ENVINT, SETUP_LOGDEV
            LOGICAL, EXTERNAL :: ENVYN

C Local Variables:
            CHARACTER( 16 ), SAVE :: CTM_SSEMDIAG = 'CTM_SSEMDIAG' ! env var for
                                                     ! diagnostic file

            CHARACTER( 16 ), SAVE :: PNAME = 'SSEMIS_INIT'
            CHARACTER( 80 ) :: VARDESC
            CHARACTER( 96 ) :: XMSG = ' '
            INTEGER STATUS

C-----------------------------------------------------------------------

            LOGDEV = SETUP_LOGDEV()
            SUCCESS = .TRUE.

            ALLOCATE ( SSOUTM( NSSSPC,NSSMODE,NCOLS,NROWS ),
     &                 SSOUTN( NSSMODE,NCOLS,NROWS ),
     &                 SSOUTS( NSSMODE,NCOLS,NROWS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = '*** SSOUTM, SSOUTN or SSOUTS memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            ALLOCATE ( OCEAN( NCOLS,NROWS ),
     &                 SZONE( NCOLS,NROWS ),
     &                 PRES ( NCOLS,NROWS ), 
     &                 TA   ( NCOLS,NROWS ), 
     &                 QV   ( NCOLS,NROWS ), 
     &                 WSPD10( NCOLS,NROWS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = '*** OCEAN, SZONE, PRES, TA, QV, or WSPD10 memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

C Open the ocean file, which contains the ocean and surf-zone fractions

            IF ( .NOT. OPEN3( OCEAN_1, FSREAD3, PNAME ) ) THEN
               XMSG = 'Open failure for ' // OCEAN_1
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            CALL SUBHFILE ( OCEAN_1, GXOFF, GYOFF,
     &                      STRTCOL_O1, ENDCOL_O1, STRTROW_O1, ENDROW_O1 )

C Open the 3D cross-point met file

            IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
               XMSG = 'Open failure for ' // MET_CRO_3D
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                      STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

C Get sea-salt-emission diagnostic output file flag.

            SSEMDIAG = .FALSE.         ! default
            VARDESC = 'Flag for writing the sea-salt-emission diagnostic file'
            SSEMDIAG = ENVYN( CTM_SSEMDIAG, VARDESC, SSEMDIAG, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               XMSG =
     &             'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               XMSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            END IF

            IF ( SSEMDIAG ) THEN    ! Open the sea-salt emission dignostic file

               SDATE = ENVINT( 'CTM_STDATE', 'Scenario Start (YYYYJJJ)', 0, STATUS )
               STIME = ENVINT( 'CTM_STTIME', 'Scenario Start (HHMMSS)', 0, STATUS )

               IF ( MYPE .EQ. 0 ) CALL OPSSEMIS ( SDATE, STIME, TSTEP, NSSDIAG, WRSS_SPC )

               ALLOCATE ( SSBF( NSSDIAG,MY_NCOLS,MY_NROWS ),
     &                    WRSS( NCOLS,NROWS ), STAT = STATUS )
               IF ( STATUS .NE. 0 ) THEN
                  XMSG = '*** SSBF or WRSS memory allocation failed'
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

            END IF

         END FUNCTION SSEMIS_INIT

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE OPSSEMIS ( JDATE, JTIME, TSTEP, NSSDIAG, WRSS_SPC )

C   16 May 05 P.Bhave: original version, using OPDDEP as a template
C    4 Mar 10 J.Young: accomodate Steve Howard`s reengineering

         USE GRID_CONF           ! horizontal & vertical domain specifications

         IMPLICIT NONE

         INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
         INCLUDE SUBST_IOFDESC   ! file header data structure
         INCLUDE SUBST_IODECL    ! I/O definitions and declarations
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER      JDATE      ! current model date, coded YYYYDDD
         INTEGER      JTIME      ! current model time, coded HHMMSS
         INTEGER      TSTEP      ! output time step
         INTEGER      NSSDIAG
         CHARACTER( 16 ) :: WRSS_SPC( NSSDIAG )

C Local variables:
         CHARACTER( 16 ) :: PNAME = 'OPSSEMIS'
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER      V, L       ! loop induction variables

C-----------------------------------------------------------------------

C Try to open existing file for update
         IF ( .NOT. OPEN3( CTM_SSEMIS_1, FSRDWR3, PNAME ) ) THEN
            XMSG = 'Could not open CTM_SSEMIS_1 for update - '
     &           // 'try to open new'
            CALL M3MESG( XMSG )

C Set output file characteristics based on COORD.EXT and open diagnostic file
            FTYPE3D = GRDDED3
            SDATE3D = JDATE
            STIME3D = JTIME
            TSTEP3D = TSTEP
            CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

            NVARS3D = NSSDIAG
            NCOLS3D = GL_NCOLS
            NROWS3D = GL_NROWS
            NLAYS3D = 1
            NTHIK3D = 1
            GDTYP3D = GDTYP_GD
            P_ALP3D = P_ALP_GD
            P_BET3D = P_BET_GD
            P_GAM3D = P_GAM_GD
            XORIG3D = XORIG_GD
            YORIG3D = YORIG_GD
            XCENT3D = XCENT_GD
            YCENT3D = YCENT_GD
            XCELL3D = XCELL_GD
            YCELL3D = YCELL_GD
            VGTYP3D = VGTYP_GD
            VGTOP3D = VGTOP_GD
!           VGTPUN3D = VGTPUN_GD ! currently, not defined
            DO L = 1, NLAYS3D + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
!           GDNAM3D = GDNAME_GD
            GDNAM3D = GRID_NAME  ! from HGRD_DEFN

            DO V = 1, NSSDIAG
               VTYPE3D( V ) = M3REAL
               VNAME3D( V ) = WRSS_SPC( V )
               IF ( VNAME3D( V )(1:4) .EQ. 'ANUM' ) THEN
                  UNITS3D( V ) = 'number/s'
               ELSE IF ( VNAME3D( V )(1:4) .EQ. 'ASRF' ) THEN
                  UNITS3D( V ) = 'm2/s'
               ELSE
                  UNITS3D( V ) = 'g/s'
               END IF
               VDESC3D( V ) = 'hourly ' // TRIM( VNAME3D( V ) )
     &                     // ' sea-salt emission rate'
            END DO

            FDESC3D( 1 ) = 'hourly layer-1 sea-salt emission rates'
            DO L = 2, MXDESC3
               FDESC3D( L ) = ' '
            END DO

C Open sea-salt emissions diagnostic file
            IF ( .NOT. OPEN3( CTM_SSEMIS_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create the CTM_SSEMIS_1 file'
               CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF

         END IF

         RETURN

         END SUBROUTINE OPSSEMIS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE GET_SSEMIS ( JDATE, JTIME, TSTEP, CELLVOL, RJACM, CELLHGT )

C GET_SSEMIS calculates the sea-salt emission rates in a grid cell
C given the fractional grid-cell area covered by open ocean and surf zone
 
C Key Subroutines/Functions Called:  NONE
 
C Revision History:
 
C   13 Jun 05 P.Bhave:  first version for public release
C   11 Apr 08 J.Kelly:  added code for (1) emission of coarse surface area,
C                       (2) emission of coarse water, (3) enhanced surf zone
C                       emissions, and (4) variable coarse std. deviation
C   20 Feb 10 J.Young:  move out of AERO_EMIS proper into this module
 
C References:
C    Alduchov and Eskridge, "Improved Magnus Form Approximations of
C          Saturation Vapor Pressure," Jour. of Applied Meteorology, vol. 35,
C          pp 601-609, April, 1996.
C    Binkowski F.S., S.J. Roselle. Models-3 Community Multiscale Air Quality 
C          (CMAQ) model aerosol component 1: Model Description.  J. Geophys. 
C          Res., Vol 108, No D6, 4183, doi:10.1029/2001JD001409 (2003).
C    Seinfeld, J.H., S.N. Pandis.  "Atmospheric Chemistry and Physics - from
C          air pollution to climate change"  John Wiley & Sons, Inc. (1998).
C    Zhang, K.M., E.M. Knipping, A.S. Wexler, P.V. Bhave, G.S. Tonnesen
C          "Size distribution of sea-salt emissions as a function of relative
C          humidity"  Atmospheric Environment, 39, 3373-3379 (2005).
C    Clarke, A.D., S.R. Owens, and J. Zhou "An ultrafine sea-salt flux from
C          breaking waves: Implications for cloud condensation nuclei in the
C          remote marine atmosphere" J. Geophys. Res. (2006)
C-----------------------------------------------------------------------

            IMPLICIT NONE

C Arguments
            INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP( 2 )
            REAL,    INTENT( IN ) :: CELLVOL               ! grid-cell volume [m2*sigma]
            REAL,    INTENT( IN ) :: RJACM( NCOLS,NROWS )  ! reciprocal Jacobian [1/m]
            REAL,    INTENT( IN ) :: CELLHGT               ! grid-cell height [sigma]

C Includes:
            INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
!           INCLUDE SUBST_IODECL    ! I/O definitions and declarations
#include            SUBST_IODECL    # I/O definitions and declarations
!           INCLUDE SUBST_IOFDESC   ! file header data structure
            INCLUDE SUBST_FILES_ID  ! file name parameters
            INCLUDE SUBST_CONST     ! constants (for PI)

C External Functions:
            INTEGER, EXTERNAL :: TIME2SEC      ! fxn not declared in IODECL3.EXT
            LOGICAL, EXTERNAL :: CURRSTEP

C Local Variables:
            REAL    :: OFRAC              ! fractional seawater cover
            REAL    :: SFRAC              ! fractional surf-zone cover
            REAL    :: BLKPRS             ! atmospheric pressure [Pa]
            REAL    :: BLKTA              ! air temperature [K]
            REAL    :: BLKQV              ! H2O mass mixing ratio [ kg/kg ]
            REAL    :: U10                ! wind speed at 10m [m/s]
            REAL    :: RLAY1HT            ! reciprocal of layer-1 height [1/m]
            REAL    :: AIRVOL             ! grid-cell volume [m3]

            CHARACTER( 16 ), SAVE :: PNAME = 'GET_SSEMIS'
            CHARACTER( 96 ) :: XMSG = ' '
            INTEGER C, R, L, N, V         ! loop indices

C Variables for calculating ambient RH as done in the aerosol module
            REAL, PARAMETER :: AA = 610.94      ! coefficients from Alduchov
            REAL, PARAMETER :: BB = 17.625      !  and Eskridge (1996)
            REAL, PARAMETER :: CC = 243.04
            REAL, PARAMETER :: EPSWATER = MWWAT / MWAIR
            REAL    :: ESAT               ! saturation vapor pressure
            REAL    :: BLKRH              ! fractional relative humidity

C Variables for calculating solute mass fraction & size-correction factors
            REAL    :: RH                 ! BLKRH constrained to 45-99% range
            REAL( 8 ) :: DRH              ! double precision RH
            REAL    :: C0                 ! (Diam @ formation) / (Diam @ ambient RH)
            REAL    :: C80                ! (Diam @ 80% RH) / (Diam @ ambient RH)    
            REAL    :: XSW                ! fractional solute weight fraction

C Sea-salt speciation factors based on dry mass
            REAL, PARAMETER :: SPCFAC_NA  = 0.3856
            REAL, PARAMETER :: SPCFAC_CL  = 0.5389
            REAL, PARAMETER :: SPCFAC_SO4 = 0.0755
         
C Bulk component densities [ kg/m3 ]
            REAL, PARAMETER :: RHOH2O  = 1.0E3  ! density of water
            REAL, PARAMETER :: RHOSO4  = 1.8E3  ! density of sulfate aerosol
            REAL, PARAMETER :: RHOSEAS = 2.2E3  ! density of marine aerosol

C Density of dry sea salt [kg/m3] using volume additivity
            REAL, PARAMETER :: RHOSSDRY = 1.0 / ( (SPCFAC_NA+SPCFAC_CL) / RHOSEAS
     &                                        +    SPCFAC_SO4           / RHOSO4 )
                                        !   (= 2.1261E3)
            REAL    :: RHOSW       ! sea-salt particle density at ambient RH [g/m3]

C Maximum and minimum diameters for emission-flux integrations [m]
            REAL, PARAMETER :: DPMAX = 20.0E-6 ! upper limit used for the
                                               ! numerical integrations
            REAL    :: DPMINO             ! minimum diameter for open-ocean flux
            REAL    :: DPMAXO             ! maximum diameter for open-ocean flux
!           REAL    :: DPMINS             ! minimum diameter for surf-zone flux
!           REAL    :: DPMAXS             ! maximum diameter for surf-zone flux
   
C Wind-speed-dependent scaling factors for emissions flux functions
            REAL    :: WDSCALO            ! Gong-Monahan open-ocean flux
!           REAL    :: WDSCALS            ! deLeeuw surf-zone flux

C Fraction of whitecap coverage
            REAL    :: WCAP

C Lognormal parameters and numerically-integrated fluxes at ambient RH
            INTEGER :: IPOS               ! position of ambient RH in data arrays
            REAL    :: DGNJRH             ! geometric mean of accum-mode flux [m]
            REAL    :: DGNKRH             ! geometric mean of coarse-mode flux [m]
            REAL    :: SIGJRH             ! geom std deviation of accum-mode flux
            REAL    :: SIGKRH             ! geom std deviation of coarse-mode flux
            REAL    :: FNJRH              ! magnitude of accum-mode flux [1/m2/s]
            REAL    :: FNKRH              ! magnitude of coarse-mode flux [1/m2/s]
            REAL    :: VFLXRH             ! total particle volume [m3/m2/s]

C Alternate forms of geometric std devs stored for efficiency
            REAL    :: LNSGJ, LNSGK       ! natural log of std dev
            REAL    :: RDIVJ, RDIVK       ! reciprocal of sqrt(2)*lnsg

C Geometric mean diameters by volume/mass [m]
            REAL    :: DGVJRH             ! accumulation mode
            REAL    :: DGVKRH             ! coarse mode

C Variables for converting 3rd moment emission rates to M0 and M2 rates
            REAL    :: FACTNUMJ           ! conversion for accumulation mode M0
            REAL    :: FACTNUMK           ! conversion for coarse mode M0
            REAL    :: FACTM2J            ! conversion for accumulation mode M2
            REAL    :: FACTM2K            ! conversion for coarse mode M2 

C Variables for calculating fraction of mass emissions assigned to each mode
            REAL    :: FFINJ              ! finite integral for accumulation mode
            REAL    :: FFINK              ! finite integral for coarse mode
            REAL    :: FRACMJ             ! mass fraction assigned to accumulation mode
            REAL    :: FRACMK             ! mass fraction assigned to coarse mode

C Mode-specific sea-salt mass emission rates [g/m3/s]
            REAL    :: ESEASJ             ! accumulation mode
            REAL    :: ESEASK             ! coarse mode

C 3rd moment emission rates [m3/m3/s]
            REAL    :: M3J                ! accumulation mode, 3rd moment
            REAL    :: M3K                ! coarse mode, 3rd moment
         
C Variables for calculating dry surface-area emission rate for accum mode
            REAL    :: WET_M3J, DRY_M3J   ! wet & dry 3rd moment emis rates [m3/m3/s] 
            REAL    :: WET_M2J, DRY_M2J   ! wet & dry 2nd moment emis rates [m3/m3/s] 
            REAL    :: WET_M3K, DRY_M3K   ! wet & dry 3rd moment emis rates [m3/m3/s] 
            REAL    :: WET_M2K, DRY_M2K   ! wet & dry 2nd moment emis rates [m3/m3/s] 

            INTEGER, SAVE     :: WSTEP  = 0    ! local write counter
            INTEGER           :: MDATE, MTIME  ! SSEMIS write date&time

C Mathematical constants; statement function for ERF approximation
            REAL,      PARAMETER :: F6DPI = 6.0 / PI
            REAL,      PARAMETER :: ONE3 = 1.0 / 3.0
            REAL,      PARAMETER :: TWO3 = 2.0 / 3.0
            REAL    :: ERF                ! ERF approx. statement function
            REAL    :: X                  ! dummy argument for ERF
            ERF( X ) = SIGN( 1.0, X ) * SQRT( 1.0 - EXP( -4.0 * X * X / PI ) )

C----------------------------- Begin calc ------------------------------

            IF ( .NOT. INTERPX( MET_CRO_3D, 'PRES', PNAME,
     &                          STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                          1,1, JDATE, JTIME, PRES ) ) THEN
               XMSG = 'Could not interpolate PRES from ' // MET_CRO_3D
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            IF ( .NOT. INTERPX( MET_CRO_3D, 'TA', PNAME,
     &                          STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                          1,1, JDATE, JTIME, TA ) ) THEN
               XMSG = 'Could not interpolate TA from ' // MET_CRO_3D
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            IF ( .NOT. INTERPX( MET_CRO_3D, 'QV', PNAME,
     &                          STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                          1,1, JDATE, JTIME, QV ) ) THEN
               XMSG = 'Could not interpolate QV from ' // MET_CRO_3D
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            IF ( .NOT. INTERPX( MET_CRO_2D, 'WSPD10', PNAME,
     &                          STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                          1,1, JDATE, JTIME, WSPD10 ) ) THEN
               XMSG = 'Could not find WSPD10 in ' // MET_CRO_2D //
     &                ' search for WIND10.'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               IF ( .NOT. INTERPX( MET_CRO_2D, 'WIND10', PNAME,
     &                             STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                             1,1, JDATE, JTIME, WSPD10 ) ) THEN
                  XMSG = 'Could not find WIND10 or WSPD10 in ' // MET_CRO_2D
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            END IF

C Read fractional seawater and surf-zone coverage from the OCEAN file.
C Store results in the OCEAN and SZONE arrays.
            IF ( .NOT. INTERPX( OCEAN_1, 'OPEN', PNAME,
     &                          STRTCOL_O1,ENDCOL_O1, STRTROW_O1,ENDROW_O1,
     &                          1,1, JDATE, JTIME, OCEAN ) ) THEN
               XMSG = 'Could not interpolate OPEN from ' // OCEAN_1
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            IF ( .NOT. INTERPX( OCEAN_1, 'SURF', PNAME,
     &                          STRTCOL_O1,ENDCOL_O1, STRTROW_O1,ENDROW_O1,
     &                          1,1, JDATE, JTIME, SZONE ) ) THEN
               XMSG = 'Could not interpolate SURF from ' // OCEAN_1 //
     &                '.  Surf-zone emissions set to zero.'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            END IF

C Initialize sea-salt output buffer
            IF ( SSEMDIAG .AND. WSTEP .EQ. 0 ) SSBF = 0.0

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  IF ( OCEAN( C,R ) + SZONE( C,R ) .GT. 0.0 ) THEN
                     OFRAC   = OCEAN ( C,R )
                     SFRAC   = SZONE ( C,R )
                     BLKPRS  = PRES  ( C,R )
                     BLKTA   = TA    ( C,R )
                     BLKQV   = QV    ( C,R )
                     U10     = WSPD10( C,R )
                     RLAY1HT = RJACM ( C,R ) / CELLHGT
                  ELSE
                     DO V = 1, NSSMODE
                        DO N = 1, NSSSPC
                           SSOUTM( N,V,C,R ) = 0.0
                        END DO
                        SSOUTN( V,C,R ) = 0.0
                        SSOUTS( V,C,R ) = 0.0
                     END DO
                     CYCLE
                  END IF

C Calculate fractional relative humidity in the grid cell
                  ESAT  = AA * EXP( BB * ( BLKTA - 273.15 ) / ( BLKTA - 273.15 + CC ) )
                  BLKRH = BLKPRS * BLKQV / ( ( EPSWATER + BLKQV ) *  ESAT )       
                  BLKRH = MIN( 0.99, MAX( 0.005, BLKRH ) )

C Calculate RH-dependent size-correction factors and solute mass fraction
C using equations from Zhang et al. (2005), which are valid over the
C 45-99% RH range.  Store powers of RH for polynomial calculations.
                  RH = BLKRH
                  RH = MAX( 0.45, MIN( 0.99, RH ) )
                  DRH = REAL( RH, 8 )

C Calculate size-correction factors from Equations 8 & 9 of Zhang et al.
                  C0 =         C0_COEFF( 1 )
     &               + DRH * ( C0_COEFF( 2 )
     &               + DRH * ( C0_COEFF( 3 )
     &               + DRH * ( C0_COEFF( 4 )
     &               + DRH * ( C0_COEFF( 5 )
     &               + DRH * ( C0_COEFF( 6 ) ) ) ) ) )
                  C80 = C0 / 1.97

C Calculate solute mass fraction using Equation 1 of Zhang et al.
                  XSW =         X_COEFF( 1 )
     &                + DRH * ( X_COEFF( 2 )
     &                + DRH * ( X_COEFF( 3 )
     &                + DRH * ( X_COEFF( 4 )
     &                + DRH * ( X_COEFF( 5 )
     &                + DRH * ( X_COEFF( 6 ) ) ) ) ) )

C Calculate sea-salt-particle density [g/m3] at ambient RH, assuming
C volume additivity of dry salt plus water
                  RHOSW = 1.0E+3 / ( XSW / RHOSSDRY + ( 1.0 - XSW ) / RHOH2O )

C Set minimum and maximum diameters for integration using the size-
C correction factors of Zhang et al. (2005)

C Gong-Monahan flux function is valid from 0.03-5.0 um dry radius;
C multiply by 4 to get wet diameter at 80% RH
                  DPMINO = 1.2E-7 / C80 
                  DPMAXO = MIN( DPMAX, DPMAX / C80 ) 

C deLeeuw flux function is valid from 1.6-20 um diameter at formation
!                 DPMINS = 1.6E-6 / C0 
!                 DPMAXS = MIN( DPMAX, DPMAX / C0 )
            
C Calculate scaling factors to correct the tabulated fluxes for the 10m
C wind speed in this cell.  Note: tabulated values are based on 1 m/s.
                  WDSCALO = MIN( U10, 20.0 ) ** 3.41        ! Gong-Monahan flux function
!                 WDSCALS = EXP( 0.23 * MIN( U10, 9.0 ) )   ! deLeeuw flux function

C Calculate whitecap coverage fraction 
                  WCAP = 3.84E-6 * WDSCALO ! Eq. 5, Clarke et al. (2006) JGR

C Find position in the data arrays that corresponds to ambient RH in this
C grid cell and set the appropriate data values
                  IF ( BLKRH .LE. 0.45 ) THEN
                     IPOS = 1
                  ELSE IF ( BLKRH .LE. 0.90 ) THEN
                     IPOS = NINT( 100.0 * BLKRH - 44.0 )
                  ELSE IF ( BLKRH .LE. 0.99 ) THEN
                     IPOS = NINT( 1000.0 * BLKRH - 854.0 )
                  ELSE
                     IPOS = 136
                  END IF

                  DGNJRH = DGNJ ( IPOS ) * 1.0E-6   ! convert to [m]
                  DGNKRH = DGNK ( IPOS ) * 1.0E-6   ! convert to [m]
                  SIGJRH = SIGJ ( IPOS )
                  SIGKRH = SIGK ( IPOS ) 
                  FNJRH  = FNJ  ( IPOS )
                  FNKRH  = FNK  ( IPOS )
                  VFLXRH = VFLUX( IPOS ) * ( OFRAC + SFRAC / WCAP )

C Save certain functions of the geometric standard deviations for later use
                  LNSGJ  = LOG( SIGJRH )
                  LNSGK  = LOG( SIGKRH )
                  RDIVJ  = 1.0 / ( SQRT( 2.0 ) * LNSGJ )
                  RDIVK  = 1.0 / ( SQRT( 2.0 ) * LNSGK )

C Calculate geometric-mean diameters by volume using Equation 7.52 of
C Seinfeld & Pandis (1998).
                  DGVJRH = DGNJRH * EXP( 3.0 * LNSGJ * LNSGJ )
                  DGVKRH = DGNKRH * EXP( 3.0 * LNSGK * LNSGK )

C Calculate modal volume fluxes [m3/m2/s] by evaluating finite integrals 
C from DPMIN to DPMAX over each lognormal distribution.  Use resulting
C values to calculate the fraction of the total number emissions to 
C assign to each mode.  See Equations 19 and 20 of Uma Shankar`s
C "Integration of Sea-Salt Fluxes" memo.
                  FFINJ  = 0.5 * FNJRH * DGVJRH ** 3 * EXP( -4.5 * LNSGJ * LNSGJ )
     &                         * ( ERF( LOG( DPMAXO / DGVJRH ) * RDIVJ )  
     &                           - ERF( LOG( DPMINO / DGVJRH ) * RDIVJ ) )
                  FFINK  = 0.5 * FNKRH * DGVKRH ** 3 * EXP( -4.5 * LNSGK * LNSGK )
     &                         * ( ERF( LOG( DPMAXO / DGVKRH ) * RDIVK )  
     &                           - ERF( LOG( DPMINO / DGVKRH ) * RDIVK ) )
                  FRACMJ = FFINJ / ( FFINJ + FFINK )
                  FRACMK = 1.0 - FRACMJ

C Calculate mode-specific mass emission rates [g/m3/s], by multiplying
C numerically-integrated volume fluxes by the modal volume fractions,
C scaling for wind speed, dividing by grid-cell height, and multiplying
C by particle density.  Multiply by chemical speciation factors to
C obtain speciated mass emissions.

                  ESEASJ = VFLXRH * FRACMJ * WDSCALO * RLAY1HT * RHOSW
                  ESEASK = VFLXRH * FRACMK * WDSCALO * RLAY1HT * RHOSW

                  SSOUTM( KNA, 1,C,R ) = 0.0
                  SSOUTM( KCL, 1,C,R ) = 0.0
                  SSOUTM( KSO4,1,C,R ) = 0.0
                  SSOUTM( KH2O,1,C,R ) = 0.0
                  SSOUTM( KNA, 2,C,R ) = ESEASJ * XSW * SPCFAC_NA  ! ac mode sodium
                  SSOUTM( KCL, 2,C,R ) = ESEASJ * XSW * SPCFAC_CL  ! ac mode chloride
                  SSOUTM( KSO4,2,C,R ) = ESEASJ * XSW * SPCFAC_SO4 ! ac mode sulfate
                  SSOUTM( KH2O,2,C,R ) = ESEASJ * ( 1.0 - XSW )    ! ac mode water
                  SSOUTM( KNA, 3,C,R ) = ESEASK * XSW * SPCFAC_NA  ! crse mode sodium  
                  SSOUTM( KCL, 3,C,R ) = ESEASK * XSW * SPCFAC_CL  ! crse mode chloride
                  SSOUTM( KSO4,3,C,R ) = ESEASK * XSW * SPCFAC_SO4 ! crse mode sulfate 
                  SSOUTM( KH2O,3,C,R ) = ESEASK * ( 1.0 - XSW )    ! crse mode water 

C Calculate mode-specific 3rd moment emission rates [m3/m3/s]
                  M3J = ESEASJ * F6DPI / RHOSW
                  M3K = ESEASK * F6DPI / RHOSW

C Calculate factors for converting 3rd moment emission rates into number
C and 2nd moment emission rates.  See Equations 7b and 7c of Binkowski &
C Roselle (2003)
                  FACTNUMJ = EXP( 4.5 * LNSGJ * LNSGJ ) / DGVJRH ** 3
                  FACTNUMK = EXP( 4.5 * LNSGK * LNSGK ) / DGVKRH ** 3
                  FACTM2J  = EXP( 0.5 * LNSGJ * LNSGJ ) / DGVJRH
                  FACTM2K  = EXP( 0.5 * LNSGK * LNSGK ) / DGVKRH 

C Calculate mode-specific emission rates of particle number [1/s]
                  SSOUTN( 1,C,R ) = 0.0
                  SSOUTN( 2,C,R ) = M3J * FACTNUMJ
                  SSOUTN( 3,C,R ) = M3K * FACTNUMK

C Calculate mode-specific dry surface area emission rates [m2/m3/s].  
C Subtract water from 3rd moment to obtain dry 3rd moment emission rate.  
C Calculate dry 2nd moment while holding the standard deviation constant.
C Multiply dry 2nd moment by PI to obtain dry surface area emission rate.
                  WET_M3J = M3J 
                  WET_M2J = M3J * FACTM2J
                  DRY_M3J = WET_M3J - F6DPI * SSOUTM( KH2O,2,C,R ) * 1.0E-06
                  DRY_M2J = WET_M2J * ( DRY_M3J / WET_M3J ) ** TWO3

                  WET_M3K = M3K
                  WET_M2K = M3K * FACTM2K
                  DRY_M3K = WET_M3K - F6DPI * SSOUTM( KH2O,3,C,R ) * 1.0E-06
                  DRY_M2K = WET_M2K * ( DRY_M3K / WET_M3K ) ** TWO3

                  SSOUTS( 1,C,R ) = 0.0
                  SSOUTS( 2,C,R ) = PI * DRY_M2J
                  SSOUTS( 3,C,R ) = PI * DRY_M2K

C Update the SSBF array, for writing the diagnostic sea-salt-emission file.
                  IF ( SSEMDIAG ) THEN
                     AIRVOL       = CELLVOL / RJACM( C,R )
                     SSOUTD(  1 ) = SSOUTM( KNA, 2,C,R )
                     SSOUTD(  2 ) = SSOUTM( KCL, 2,C,R )
                     SSOUTD(  3 ) = SSOUTM( KSO4,2,C,R )
                     SSOUTD(  4 ) = SSOUTM( KH2O,2,C,R )
                     SSOUTD(  5 ) = SSOUTM( KNA, 3,C,R )
                     SSOUTD(  6 ) = SSOUTM( KCL, 3,C,R )
                     SSOUTD(  7 ) = SSOUTM( KSO4,3,C,R )
                     SSOUTD(  8 ) = SSOUTM( KH2O,3,C,R )
                     SSOUTD(  9 ) = SSOUTN( 2,C,R )
                     SSOUTD( 10 ) = SSOUTN( 3,C,R )
                     SSOUTD( 11 ) = SSOUTS( 2,C,R )
                     SSOUTD( 12 ) = SSOUTS( 3,C,R )
                     DO V = 1, NSSDIAG
                        SSBF( V,C,R ) = SSBF( V,C,R ) + SSOUTD( V ) * AIRVOL
     &                                * FLOAT( TIME2SEC ( TSTEP( 1 ) ) )
                     END DO
                  END IF  ! SSEMDIAG

               END DO   ! C
            END DO   ! R

!           write( logdev,* ) 'sum ssoutm, ssoutn, ssouts: ',
!    &                         sum( ssoutm ), sum( ssoutn ), sum( ssouts )

C If last call this hour, write out the total sea-salt emissions [g/s].
C Then reset the sea-salt emissions array and local write counter.
            IF ( SSEMDIAG ) THEN

               WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )

               IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN
                  IF ( .NOT. CURRSTEP( JDATE, JTIME, SDATE, STIME, TSTEP( 1 ),
     &                                 MDATE, MTIME ) ) THEN
                     XMSG = 'Cannot get step date and time'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
                  END IF
                  CALL NEXTIME( MDATE, MTIME, TSTEP( 1 ) )
                  DO V = 1, NSSDIAG
                     DO R = 1, MY_NROWS
                        DO C = 1, MY_NCOLS
                           WRSS( C,R ) = SSBF( V,C,R ) / FLOAT( WSTEP )
                        END DO
                     END DO
                     IF ( .NOT. WRITE3( CTM_SSEMIS_1, WRSS_SPC( V ),
     &                          MDATE, MTIME, WRSS ) ) THEN
                        XMSG = 'Could not write ' // CTM_SSEMIS_1 // ' file'
                        CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
                     END IF
                  END DO
                  WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                  'Timestep written to', CTM_SSEMIS_1,
     &                  'for date and time', MDATE, MTIME
                  WSTEP = 0
                  SSBF = 0.0
               END IF

            END IF  ! SSEMDIAG

            RETURN
            
         END SUBROUTINE GET_SSEMIS

      END MODULE SSEMIS
