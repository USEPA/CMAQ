
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE SSEMIS
C-----------------------------------------------------------------------
C Description:

C Revision History:
C   04 Apr 11 S.Howard: generalization to accomodate different sea-salt speciation
C                       in aero5 and aero6
C   07 Apr 11 J.Young: additional mods
C   07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module.
C   12 Aug 15 D.Wong: Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O implementation
C   09 Dec 15 D.Wong: Open CTM_SSEMIS_1 file in a IO_PE_INCLUSIVE conditional block
C   24 Mar 16 G.Sarwar: updated to calculate NACL emissions (needed for BR2 emissions) 
C   16 Aug 18 G. Sarwar: Removed NACL
C    1 Feb 19 David Wong: removed all MY_N clauses
C-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE HGRD_DEFN
      USE AERO_DATA, ONLY: N_MODE
      USE DESID_VARS

      IMPLICIT NONE

C Number of chemical species in fresh sea-salt aerosol
      INTEGER :: NSSSPC

C Sea-Salt Emissions Rates
      PUBLIC NSSSPC, SSEMIS_INIT, GET_SSEMIS, SEA_FACTNUM, SEA_FACTSRF, SSOUTM
      PRIVATE

C Position of H2O in sea-salt emission arrays
      INTEGER :: KH2O

C Variables for the sea-salt diagnostic file
      INTEGER :: NSSDIAG                ! number of species in sea-salt
                                        ! diagnostic emission file
      Real,    ALLOCATABLE, SAVE :: SSOUTM( :,:,: )  ! SeaSpray Mass Emiss Rate [ug/m3/s]
      Real,    ALLOCATABLE, SAVE :: SSOUTN( :,:,: )  ! SeaSpray Number Emiss Rate [1/m3/s]
      Real,    ALLOCATABLE, SAVE :: SSOUTS( :,:,: )  ! SeaSpray Surface Area Emiss Rate [m2/m3/s]
      REAL,    ALLOCATABLE       :: SSOUTD( : )   ! emission rates
      REAL,    ALLOCATABLE, SAVE :: SSBF( :,:,: ) ! seasalt emiss accumulator
      REAL,    ALLOCATABLE, SAVE :: WRSS( :,: )   ! seasalt emiss write buffer
      REAL,    ALLOCATABLE, SAVE :: SEA_FACTNUM( :,:,: ) ! Array for scaling Number Emissions
      REAL,    ALLOCATABLE, SAVE :: SEA_FACTSRF( :,:,: ) ! Array for scaling Surface Area Emissions 

C Species names in the speciated sea-salt-emissions
      CHARACTER( 16 ), ALLOCATABLE :: WRSS_SPC( : )  ! species names

C Lognormal parameters fit to the open-ocean flux function
      REAL,    SAVE :: DGNJ( 136 )     ! geom mean diam of accum-mode [um]
      REAL,    SAVE :: DGNK( 136 )     ! geom mean diam of coarse-mode [um]
      REAL,    SAVE :: SIGJ( 136 )     ! geom std deviation of accum-mode flux
      REAL,    SAVE :: SIGK( 136 )     ! geom std deviation of coarse-mode flux
      REAL,    SAVE :: FNJ ( 136 )     ! magnitude of accum-mode flux [1/m2/s]
      REAL,    SAVE :: FNK ( 136 )     ! magnitude of coarse-mode flux [1/m2/s]

C Open-ocean flux numerically integrated over size range of interest
      REAL,    SAVE :: VFLUX( 136 )    ! total particle volume flux [m3/m2/s]

C Polycoeffs from Zhang et al. (2005)
      REAL( 8 ), SAVE :: C0_COEFF( 6 )   ! Eq 8
      REAL( 8 ), SAVE :: X_COEFF( 6 )    ! Eq 1

      INTEGER I

C-------------------------- Data Statements ----------------------------

C RH-dependent values calculated using MATLAB for THETA = 8 from Gong (2003)

C Geometric mean diameter of accumulation mode [um]
      DATA ( DGNJ( I ), I = 1, 136 ) /
     &      0.0913, 0.0917, 0.0921, 0.0925, 0.0929, 0.0933, 0.0938, 0.0942,
     &      0.0946, 0.0951, 0.0956, 0.0961, 0.0967, 0.0972, 0.0977, 0.0984,
     &      0.0990, 0.0996, 0.1002, 0.1010, 0.1017, 0.1024, 0.1032, 0.1040,
     &      0.1048, 0.1058, 0.1067, 0.1077, 0.1087, 0.1098, 0.1110, 0.1123,
     &      0.1136, 0.1150, 0.1165, 0.1181, 0.1198, 0.1217, 0.1237, 0.1259,
     &      0.1283, 0.1309, 0.1338, 0.1371, 0.1407, 0.1448, 0.1453, 0.1457,
     &      0.1462, 0.1466, 0.1471, 0.1476, 0.1481, 0.1486, 0.1491, 0.1496,
     &      0.1501, 0.1506, 0.1511, 0.1517, 0.1522, 0.1528, 0.1533, 0.1539,
     &      0.1545, 0.1551, 0.1557, 0.1563, 0.1570, 0.1576, 0.1582, 0.1589,
     &      0.1596, 0.1603, 0.1610, 0.1617, 0.1624, 0.1631, 0.1639, 0.1647,
     &      0.1655, 0.1663, 0.1671, 0.1679, 0.1688, 0.1697, 0.1706, 0.1715,
     &      0.1724, 0.1734, 0.1744, 0.1754, 0.1764, 0.1775, 0.1786, 0.1797,
     &      0.1809, 0.1821, 0.1833, 0.1845, 0.1858, 0.1872, 0.1885, 0.1899,
     &      0.1914, 0.1929, 0.1945, 0.1961, 0.1978, 0.1995, 0.2013, 0.2032,
     &      0.2052, 0.2072, 0.2093, 0.2115, 0.2138, 0.2162, 0.2188, 0.2214,
     &      0.2243, 0.2272, 0.2304, 0.2336, 0.2371, 0.2409, 0.2449, 0.2492,
     &      0.2538, 0.2587, 0.2641, 0.2702, 0.2765, 0.2837, 0.2918, 0.3009 /

C Geometric mean diameter of coarse mode [um]
      DATA ( DGNK( I ), I = 1, 136 ) /
     &      1.7786, 1.8180, 1.8254, 1.8330, 1.8408, 1.8489, 1.8891, 1.8979,
     &      1.9069, 1.9163, 1.9259, 1.9673, 1.9778, 1.9887, 2.0000, 2.0425,
     &      2.0549, 2.0677, 2.0812, 2.1252, 2.1400, 2.1554, 2.2009, 2.2180,
     &      2.2359, 2.2833, 2.3034, 2.3244, 2.3746, 2.3983, 2.4504, 2.4773,
     &      2.5319, 2.5627, 2.6207, 2.6563, 2.7187, 2.7831, 2.8286, 2.9000,
     &      2.9750, 3.0545, 3.1395, 3.2312, 3.3313, 3.4419, 3.4524, 3.4631,
     &      3.4855, 3.4965, 3.5077, 3.5190, 3.5305, 3.5422, 3.5540, 3.5760,
     &      3.5882, 3.6007, 3.6133, 3.6262, 3.6392, 3.6607, 3.6742, 3.6879,
     &      3.7019, 3.7161, 3.7306, 3.7517, 3.7667, 3.7820, 3.7976, 3.8134,
     &      3.8296, 3.8506, 3.8674, 3.8845, 3.9019, 3.9197, 3.9405, 3.9591,
     &      3.9781, 3.9974, 4.0172, 4.0380, 4.0587, 4.0798, 4.1015, 4.1221,
     &      4.1448, 4.1680, 4.1918, 4.2126, 4.2375, 4.2632, 4.2895, 4.3108,
     &      4.3385, 4.3671, 4.3964, 4.4185, 4.4495, 4.4816, 4.5040, 4.5381,
     &      4.5732, 4.5965, 4.6340, 4.6728, 4.6974, 4.7390, 4.7640, 4.8087,
     &      4.8552, 4.8827, 4.9330, 4.9618, 5.0166, 5.0472, 5.1071, 5.1403,
     &      5.2062, 5.2426, 5.3158, 5.3566, 5.3985, 5.4852, 5.5338, 5.5844,
     &      5.6378, 5.6944, 5.7547, 5.8867, 5.8898, 5.9665, 6.0510, 6.1446 /

C Geometric standard deviation of accumulation mode
      DATA ( SIGJ( I ), I = 1, 136 ) /
     &      2.1420, 2.1442, 2.1442, 2.1442, 2.1442, 2.1442, 2.1464, 2.1464,
     &      2.1464, 2.1464, 2.1464, 2.1485, 2.1485, 2.1485, 2.1485, 2.1506,
     &      2.1506, 2.1506, 2.1506, 2.1526, 2.1526, 2.1526, 2.1545, 2.1545,
     &      2.1545, 2.1564, 2.1564, 2.1564, 2.1582, 2.1582, 2.1599, 2.1599,
     &      2.1616, 2.1616, 2.1632, 2.1632, 2.1647, 2.1661, 2.1661, 2.1674,
     &      2.1687, 2.1698, 2.1709, 2.1719, 2.1727, 2.1735, 2.1735, 2.1735,
     &      2.1742, 2.1742, 2.1742, 2.1742, 2.1742, 2.1742, 2.1742, 2.1747,
     &      2.1747, 2.1747, 2.1747, 2.1747, 2.1747, 2.1752, 2.1752, 2.1752,
     &      2.1752, 2.1752, 2.1752, 2.1756, 2.1756, 2.1756, 2.1756, 2.1756,
     &      2.1756, 2.1758, 2.1758, 2.1758, 2.1758, 2.1758, 2.1760, 2.1760,
     &      2.1760, 2.1760, 2.1760, 2.1760, 2.1760, 2.1760, 2.1760, 2.1759,
     &      2.1759, 2.1759, 2.1759, 2.1757, 2.1757, 2.1757, 2.1757, 2.1754,
     &      2.1754, 2.1754, 2.1754, 2.1750, 2.1750, 2.1750, 2.1745, 2.1745,
     &      2.1745, 2.1739, 2.1739, 2.1739, 2.1731, 2.1731, 2.1723, 2.1723,
     &      2.1723, 2.1713, 2.1713, 2.1703, 2.1703, 2.1691, 2.1691, 2.1679,
     &      2.1679, 2.1665, 2.1665, 2.1651, 2.1636, 2.1636, 2.1619, 2.1602,
     &      2.1584, 2.1566, 2.1546, 2.1546, 2.1506, 2.1484, 2.1462, 2.1440 /

C Geometric standard deviation of coarse mode
      DATA ( SIGK( I ), I = 1, 136 ) /
     &      2.1083, 2.0900, 2.0900, 2.0900, 2.0900, 2.0900, 2.0725, 2.0725,
     &      2.0725, 2.0725, 2.0725, 2.0558, 2.0558, 2.0558, 2.0558, 2.0399,
     &      2.0399, 2.0399, 2.0399, 2.0248, 2.0248, 2.0248, 2.0103, 2.0103,
     &      2.0103, 1.9967, 1.9967, 1.9967, 1.9836, 1.9836, 1.9713, 1.9713,
     &      1.9597, 1.9597, 1.9487, 1.9487, 1.9384, 1.9288, 1.9288, 1.9198,
     &      1.9114, 1.9036, 1.8965, 1.8901, 1.8843, 1.8791, 1.8791, 1.8791,
     &      1.8746, 1.8746, 1.8746, 1.8746, 1.8746, 1.8746, 1.8746, 1.8708,
     &      1.8708, 1.8708, 1.8708, 1.8708, 1.8708, 1.8676, 1.8676, 1.8676,
     &      1.8676, 1.8676, 1.8676, 1.8652, 1.8652, 1.8652, 1.8652, 1.8652,
     &      1.8652, 1.8635, 1.8635, 1.8635, 1.8635, 1.8635, 1.8625, 1.8625,
     &      1.8625, 1.8625, 1.8625, 1.8622, 1.8622, 1.8622, 1.8622, 1.8628,
     &      1.8628, 1.8628, 1.8628, 1.8642, 1.8642, 1.8642, 1.8642, 1.8664,
     &      1.8664, 1.8664, 1.8664, 1.8696, 1.8696, 1.8696, 1.8737, 1.8737,
     &      1.8737, 1.8788, 1.8788, 1.8788, 1.8850, 1.8850, 1.8923, 1.8923,
     &      1.8923, 1.9009, 1.9009, 1.9108, 1.9108, 1.9221, 1.9221, 1.9351,
     &      1.9351, 1.9497, 1.9497, 1.9662, 1.9849, 1.9849, 2.0058, 2.0295,
     &      2.0560, 2.0859, 2.1197, 2.1197, 2.2014, 2.2510, 2.3080, 2.3740 /

C Number flux in accumulation mode at 1 m/s [1/m2/s]
            DATA ( FNJ( I ), I = 1, 136 ) /
     &      1249.0821, 1248.2831, 1248.2831, 1248.2831, 1248.2831, 1248.2831,
     &      1247.4863, 1247.4863, 1247.4863, 1247.4863, 1247.4863, 1246.6948,
     &      1246.6948, 1246.6948, 1246.6948, 1245.9114, 1245.9114, 1245.9114,
     &      1245.9114, 1245.1389, 1245.1389, 1245.1389, 1244.3799, 1244.3799,
     &      1244.3799, 1243.6370, 1243.6370, 1243.6370, 1242.9115, 1242.9115,
     &      1242.2081, 1242.2081, 1241.5281, 1241.5281, 1240.8739, 1240.8739,
     &      1240.2476, 1239.6513, 1239.6513, 1239.0872, 1238.5571, 1238.0628,
     &      1237.6062, 1237.1869, 1236.8099, 1236.4749, 1236.4749, 1236.4749,
     &      1236.1833, 1236.1833, 1236.1833, 1236.1833, 1236.1833, 1236.1833,
     &      1236.1833, 1235.9361, 1235.9361, 1235.9361, 1235.9361, 1235.9361,
     &      1235.9361, 1235.7343, 1235.7343, 1235.7343, 1235.7343, 1235.7343,
     &      1235.7343, 1235.5787, 1235.5787, 1235.5786, 1235.5786, 1235.5786,
     &      1235.5786, 1235.4698, 1235.4698, 1235.4698, 1235.4698, 1235.4698,
     &      1235.4082, 1235.4082, 1235.4082, 1235.4082, 1235.4082, 1235.3941,
     &      1235.3941, 1235.3941, 1235.3941, 1235.4277, 1235.4277, 1235.4277,
     &      1235.4277, 1235.5087, 1235.5087, 1235.5087, 1235.5087, 1235.6351,
     &      1235.6351, 1235.6351, 1235.6351, 1235.8099, 1235.8099, 1235.8099,
     &      1236.0306, 1236.0306, 1236.0306, 1236.2963, 1236.2963, 1236.2963,
     &      1236.6058, 1236.6058, 1236.9575, 1236.9575, 1236.9575, 1237.3498,
     &      1237.3498, 1237.7807, 1237.7807, 1238.2480, 1238.2480, 1238.7491,
     &      1238.7491, 1239.2812, 1239.2812, 1239.8412, 1240.4256, 1240.4256,
     &      1241.0307, 1241.6543, 1242.2878, 1242.9282, 1243.5702, 1243.5702,
     &      1244.8344, 1245.4428, 1246.0247, 1246.5734 /

C Number flux in coarse mode at 1 m/s [1/m2/s]
      DATA ( FNK( I ), I = 1, 136 ) /
     &      11.2976, 11.1324, 11.1324, 11.1324, 11.1324, 11.1324, 10.9787,
     &      10.9787, 10.9787, 10.9787, 10.9787, 10.8356, 10.8356, 10.8356,
     &      10.8356, 10.7024, 10.7024, 10.7024, 10.7024, 10.5785, 10.5785,
     &      10.5785, 10.4632, 10.4632, 10.4632, 10.3561, 10.3561, 10.3561,
     &      10.2565, 10.2565, 10.1641, 10.1641, 10.0786, 10.0786, 9.9994,
     &      9.9994, 9.9263, 9.8591, 9.8591, 9.7973, 9.7408, 9.6894, 9.6430,
     &      9.6011, 9.5640, 9.5315, 9.5315, 9.5315, 9.5034, 9.5034, 9.5034,
     &      9.5034, 9.5034, 9.5034, 9.5034, 9.4797, 9.4797, 9.4797, 9.4797,
     &      9.4797, 9.4797, 9.4604, 9.4604, 9.4604, 9.4604, 9.4604, 9.4604,
     &      9.4455, 9.4455, 9.4455, 9.4455, 9.4455, 9.4455, 9.4351, 9.4351,
     &      9.4351, 9.4351, 9.4351, 9.4291, 9.4291, 9.4291, 9.4291, 9.4291,
     &      9.4278, 9.4278, 9.4278, 9.4278, 9.4311, 9.4311, 9.4311, 9.4311,
     &      9.4393, 9.4393, 9.4393, 9.4393, 9.4523, 9.4523, 9.4523, 9.4523,
     &      9.4708, 9.4708, 9.4708, 9.4948, 9.4948, 9.4948, 9.5247, 9.5247,
     &      9.5247, 9.5608, 9.5608, 9.6036, 9.6036, 9.6036, 9.6536, 9.6536,
     &      9.7112, 9.7112, 9.7773, 9.7773, 9.8524, 9.8524, 9.9376, 9.9376,
     &      10.0336, 10.1418, 10.1418, 10.2634, 10.4001, 10.5533, 10.7252,
     &      10.9184, 10.9184, 11.3807, 11.6576, 11.9718, 12.3303 /

C Volume flux at 1 m/s numerically integrated over full size range [m3/m2/s]
      DATA ( VFLUX( I ), I = 1, 136 ) /
     &      3.5750e-16, 3.5600e-16, 3.6036e-16, 3.6489e-16, 3.6960e-16,
     &      3.7449e-16, 3.7367e-16, 3.7889e-16, 3.8434e-16, 3.9002e-16,
     &      3.9596e-16, 3.9603e-16, 4.0243e-16, 4.0913e-16, 4.1615e-16,
     &      4.1720e-16, 4.2484e-16, 4.3289e-16, 4.4136e-16, 4.4371e-16,
     &      4.5303e-16, 4.6290e-16, 4.6656e-16, 4.7752e-16, 4.8919e-16,
     &      4.9453e-16, 5.0765e-16, 5.2171e-16, 5.2932e-16, 5.4535e-16,
     &      5.5493e-16, 5.7342e-16, 5.8547e-16, 6.0711e-16, 6.2234e-16,
     &      6.4805e-16, 6.6748e-16, 6.8942e-16, 7.2379e-16, 7.5245e-16,
     &      7.8530e-16, 8.2321e-16, 8.6731e-16, 9.1914e-16, 9.8076e-16,
     &      1.0551e-15, 1.0648e-15, 1.0746e-15, 1.0704e-15, 1.0806e-15,
     &      1.0909e-15, 1.1015e-15, 1.1124e-15, 1.1234e-15, 1.1347e-15,
     &      1.1310e-15, 1.1427e-15, 1.1546e-15, 1.1668e-15, 1.1793e-15,
     &      1.1921e-15, 1.1889e-15, 1.2021e-15, 1.2157e-15, 1.2295e-15,
     &      1.2437e-15, 1.2583e-15, 1.2559e-15, 1.2711e-15, 1.2866e-15,
     &      1.3025e-15, 1.3189e-15, 1.3357e-15, 1.3343e-15, 1.3518e-15,
     &      1.3699e-15, 1.3884e-15, 1.4075e-15, 1.4072e-15, 1.4272e-15,
     &      1.4477e-15, 1.4690e-15, 1.4909e-15, 1.4919e-15, 1.5149e-15,
     &      1.5387e-15, 1.5633e-15, 1.5657e-15, 1.5916e-15, 1.6185e-15,
     &      1.6464e-15, 1.6504e-15, 1.6799e-15, 1.7106e-15, 1.7425e-15,
     &      1.7487e-15, 1.7827e-15, 1.8181e-15, 1.8550e-15, 1.8640e-15,
     &      1.9036e-15, 1.9450e-15, 1.9566e-15, 2.0013e-15, 2.0481e-15,
     &      2.0629e-15, 2.1138e-15, 2.1673e-15, 2.1862e-15, 2.2448e-15,
     &      2.2666e-15, 2.3310e-15, 2.3993e-15, 2.4276e-15, 2.5035e-15,
     &      2.5366e-15, 2.6216e-15, 2.6607e-15, 2.7566e-15, 2.8033e-15,
     &      2.9126e-15, 2.9689e-15, 3.0950e-15, 3.1640e-15, 3.2387e-15,
     &      3.3973e-15, 3.4912e-15, 3.5945e-15, 3.7089e-15, 3.8366e-15,
     &      3.9802e-15, 4.2603e-15, 4.3308e-15, 4.5485e-15, 4.8050e-15,
     &      5.1124e-15 /


C Polynomial coefficients from Zhang et al. (2005)         
      DATA C0_COEFF / 28.376D0, -205.44D0, 653.37D0, -1031.7D0, 803.18D0, -247.08D0 /
      DATA X_COEFF  / 3.1657D0, -19.079D0,  55.72D0, -83.998D0, 63.436D0, -19.248D0 /

      CONTAINS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         FUNCTION SSEMIS_INIT( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

C Revision History:
C   26 Feb 10 J.Young:  Eliminate BARRIER, etc. to prevent MPI race condition
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   04 Apr 11 S.Howard: dynamic allocation and generalization of indices for
C                       SSOUTD, WRSS_SPC, and SSSPC_MAP
C   11 May 11 D.Wong: incorporated twoway model implementation
C   12 Aug 15 D.Wong: added code to handle parallel I/O implementation

            USE UTILIO_DEFN
            USE AERO_DATA

#ifndef mpas
#ifdef parallel
            USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
            USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif

            IMPLICIT NONE

C Arguments:
            INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP
            LOGICAL :: SUCCESS

C Includes:
            INCLUDE SUBST_FILES_ID  ! file name parameters

C Local Variables:
            CHARACTER( 16 ), SAVE :: PNAME = 'SSEMIS_INIT'
            CHARACTER( 80 ) :: VARDESC
            CHARACTER( 250 ) :: XMSG = ' '
            INTEGER STATUS
            INTEGER S, N, L, ISEA, SPC, IM
            LOGICAL SEA_MATCH
            CHARACTER( 16 ) :: SN

C-----------------------------------------------------------------------

            SUCCESS = .TRUE.

C Map aerospc data
            CALL MAP_AERO()

            CALL LOG_MESSAGE( LOGDEV, 'Initialize Sea Spray Aerosol Emissions' )

            ! Specify H2O in mapping
            KH2O = NSEA_SPC

C Populate Master Emissions Map Vector So That Diagnostics 
C can be printed in EMIS_MAP
            NSSSPC = NSEA_SPC
            DESID_EMVAR( iseasrm )%len = nssspc*2
            Allocate( DESID_EMVAR( iseasrm )%arry ( nssspc*2 ) )
            Allocate( DESID_EMVAR( iseasrm )%units( nssspc*2 ) )
            Allocate( DESID_EMVAR( iseasrm )%mw   ( nssspc*2 ) )
            Allocate( DESID_EMVAR( iseasrm )%used ( nssspc*2 ) )
            Allocate( DESID_EMVAR( iseasrm )%conv ( nssspc*2 ) )
            Allocate( DESID_EMVAR( iseasrm )%basis( nssspc*2 ) )
            Allocate( DESID_EMVAR( iseasrm )%larea( nssspc*2 ) )
            Allocate( DESID_EMVAR( iseasrm )%lareaadj( nssspc*2 ) )
            DESID_EMVAR( iseasrm )%arry( : ) = 'NOT_AVAILABLE'
            do i = 1,nssspc
              IF ( sea_spc( i )%spcfac(1) .NE. 0.0 .or. i .EQ. nssspc ) 
     &           DESID_EMVAR( iseasrm )%arry( i )  =
     &                         "PMFINE_" // sea_spc(i)%name
              IF ( sea_spc( i )%spcfac(2) .NE. 0.0 .or. i .EQ. nssspc ) 
     &           DESID_EMVAR( iseasrm )%arry( i+nssspc )  =
     &                         "PMCOARSE_" // sea_spc(i)%name
               DESID_EMVAR( iseasrm )%mw   ( i ) = sea_spc(i)%mw
               DESID_EMVAR( iseasrm )%mw   ( i+nssspc ) = sea_spc(i)%mw
            end do
            DESID_EMVAR( iseasrm )%units( : ) = 'G/S'
            DESID_EMVAR( iseasrm )%used ( : ) = .FALSE.
            DESID_EMVAR( iseasrm )%conv ( : ) = 1.0
            DESID_EMVAR( iseasrm )%basis( : ) = 'MASS'
            DESID_EMVAR( iseasrm )%larea( : ) = .FALSE.
            DESID_EMVAR( iseasrm )%lareaadj( : ) = .FALSE.
 
C Allocate SS arrays using NSSSPC value
            ALLOCATE ( SSOUTM( NSSSPC*2,NCOLS,NROWS ),
     &                 SSOUTN( 2,NCOLS,NROWS ),
     &                 SSOUTS( 2,NCOLS,NROWS ), 
     &                 SEA_FACTNUM( N_MODE,NCOLS,NROWS ),
     &                 SEA_FACTSRF( N_MODE,NCOLS,NROWS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = '*** SSOUTM, SSOUTN or SSOUTS memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF


            IF ( SSEMDIAG ) THEN    ! Open the sea-salt emission diagnostic file
C Determine number of sea-salt species For diagnostic file
               NSSDIAG = 0
               DO ISEA = 1,NSSSPC
                  NSSDIAG = NSSDIAG + COUNT( SEA_SPC( ISEA )%SPCFAC( : ) .GT. 0. )
               ENDDO
C Include H2O
               NSSDIAG = NSSDIAG + 2   !! count both J and K modes
C Include NUMBER and SURFACE_AREA to diagnostic (J & K)
               NSSDIAG = NSSDIAG + 4

C Allocate diagnostic arrays
               ALLOCATE ( SSOUTD( NSSDIAG ),
     &                    WRSS_SPC( NSSDIAG ), STAT = STATUS )
               IF ( STATUS .NE. 0 ) THEN
                  XMSG = '*** SSOUTD or WRSS_SPC memory allocation failed'
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

C Build diagnostic file write buffer (WRSS_SPC) array
               NSSDIAG = 0
               DO N = 1, 2
                  DO S = 1, NSSSPC
                     IF ( SEA_SPC( S )%SPCFAC( N ) .GT. 0.0 ) THEN
                        NSSDIAG = NSSDIAG + 1
                        WRSS_SPC( NSSDIAG ) = TRIM( SEA_SPC( S )%NAME ) // MODESUFF( N ) 
                     END IF
                  END DO
C Include H2O
                  NSSDIAG = NSSDIAG + 1
                  WRSS_SPC( NSSDIAG ) = 'AH2O' // MODESUFF( N )
               END DO

C Include Mode NUMBER to diagnostic
               NSSDIAG = NSSDIAG + 1
               WRSS_SPC( NSSDIAG ) = "NUMFINE"
               NSSDIAG = NSSDIAG + 1
               WRSS_SPC( NSSDIAG ) = "NUMCOARSE"
 
C Include Mode SURFACE_AREA to diagnostic
               NSSDIAG = NSSDIAG + 1
               WRSS_SPC( NSSDIAG ) = "SRFFINE"
               NSSDIAG = NSSDIAG + 1
               WRSS_SPC( NSSDIAG ) = "SRFCOARSE"

C Open the sea-salt emission dignostic file
               IF ( IO_PE_INCLUSIVE ) CALL OPSSEMIS ( STDATE, STTIME, TSTEP, NSSDIAG, WRSS_SPC )

               ALLOCATE ( SSBF( NSSDIAG,NCOLS,NROWS ),
     &                    WRSS( NCOLS,NROWS ), STAT = STATUS )
               IF ( STATUS .NE. 0 ) THEN
                  XMSG = '*** SSBF or WRSS memory allocation failed'
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

#ifndef mpas
#ifdef parallel_io
               CALL SE_BARRIER
               IF ( .NOT. IO_PE_INCLUSIVE ) THEN
                  IF ( .NOT. OPEN3( CTM_SSEMIS_1, FSREAD3, PNAME ) ) THEN
                     XMSG = 'Could not open ' // TRIM(CTM_SSEMIS_1)
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                  END IF
               END IF
#endif
#endif

            END IF

         END FUNCTION SSEMIS_INIT

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE OPSSEMIS ( JDATE, JTIME, TSTEP, NSSDIAG, WRSS_SPC )

C   16 May 05 P.Bhave: original version, using OPDDEP as a template
C    4 Mar 10 J.Young: accomodate Steve Howard's reengineering
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN

         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE UTILIO_DEFN

         IMPLICIT NONE

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

#ifndef mpas
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
               IF ( VNAME3D( V )(1:3) .EQ. 'NUM' ) THEN
                  UNITS3D( V ) = 's-1'
               ELSE IF ( VNAME3D( V )(1:3) .EQ. 'SRF' ) THEN
                  UNITS3D( V ) = 'm2 s-1'
               ELSE
                  UNITS3D( V ) = 'g s-1'
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
#endif

         RETURN

         END SUBROUTINE OPSSEMIS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE GET_SSEMIS ( JDATE, JTIME, TSTEP, CELLVOL, CELLHGT,
     &                           L_DESID_DIAG )

C GET_SSEMIS calculates the sea-salt emission rates in a grid cell
C given the fractional grid-cell area covered by open ocean and surf zone
 
C Key Subroutines/Functions Called:  NONE
 
C Revision History:
 
C   13 Jun 05 P.Bhave:  first version for public release
C   11 Apr 08 J.Kelly:  added code for (1) emission of coarse surface area,
C                       (2) emission of coarse water, (3) enhanced surf zone
C                       emissions, and (4) variable coarse std. deviation
C   20 Feb 10 J.Young:  move out of AERO_EMIS proper into this module
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   04 Apr 11 S.Howard: moved speciation to AERO_DATA and generalized species
C                       and modal references and density calculation
C   11 May 11 D.Wong: incorporated twoway model implementation
C   08 Aug 14 B.Gantt: added code for (1) increased accumulation mode
C                      emissions, (2) sea surface temperature dependence
C                      of emissions, and (3) reduction of surf zone
C                      emisions
 
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
C    Lewis, E.R. and Schwartz, S.E. "Comment on "Size distribution of sea-salt
C           emissions as a funciton of relative humidity", Atmospheric Environment,
C           40, 588-590 (2006)
C    Zhang, K.M., E.M. Knipping, A.S. Wexler, P.V. Bhave, G.S. Tonnesen, "Reply
C           to comment on "Size distribution of sea-salt emissions as a funciton 
C           of relative humidity", Atmospheric Environment, 40, 591-592 (2006)
C    Clarke, A.D., S.R. Owens, and J. Zhou "An ultrafine sea-salt flux from
C          breaking waves: Implications for cloud condensation nuclei in the
C          remote marine atmosphere" J. Geophys. Res. (2006)
C    Gong, S. L., A parameterization of sea-salt aerosol source function
C          for sub- and super-micron particles, Global Biogeochem. Cycles, 17,
C          1097, (2003).
C    Jaeglé, L., P. K. Quinn, T. S. Bates, B. Alexander, and J. T. Lin, Global 
C          distribution of sea salt aerosols: New constraints from in situ and
C          remote sensing observations, Atmos. Chem. Phys., 11, 3137–3157,
C          (2011).
C    Ovadnevaite, J., Manders, A., de Leeuw, G., Ceburnis, D., Monahan, C., 
C          Partanen, A.-I., Korhonen, H., and O'Dowd, C. D.: A sea spray 
C          aerosol flux parameterization encapsulating wave state, Atmos. 
C          Chem. Phys., 14, 1837-1852, (2014).
C-----------------------------------------------------------------------

            USE UTILIO_DEFN
            USE AERO_DATA
            USE AEROMET_DATA   ! Includes CONST.EXT
            USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA, svp2, svp3
#ifdef mpas
            use coupler_module, only : cell_vol, cell_thickness
#endif

            IMPLICIT NONE

C Arguments
            INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP( 3 )
            REAL,    INTENT( IN ) :: CELLVOL(:,:)          ! grid-cell volume [m2*sigma]
            REAL,    INTENT( IN ) :: CELLHGT(:,:) ! grid-cell height [sigma] 
            LOGICAL, INTENT( IN ) :: L_DESID_DIAG ! flag for whether or not DESID 
                                                  !     is in diagnostic mode
C Includes:
            INCLUDE SUBST_FILES_ID  ! file name parameters

C Local Variables:
            REAL    :: OFRAC              ! fractional seawater cover
            REAL    :: SFRAC              ! fractional surf-zone cover
            REAL    :: BLKPRS             ! atmospheric pressure [Pa]
            REAL    :: BLKTA              ! air temperature [K]
            REAL    :: BLKQV              ! H2O mass mixing ratio [ kg/kg ]
            REAL    :: U10                ! wind speed at 10m [m/s]
            REAL    :: RLAY1HT            ! reciprocal of layer-1 height [1/m]
            REAL    :: AIRVOL             ! grid-cell volume [m3]
            REAL    :: SST                ! sea surface temperature[C]

            CHARACTER( 16 ), SAVE :: PNAME = 'GET_SSEMIS'
            CHARACTER( 96 ) :: XMSG = ' '
            INTEGER C, R, L, N, V, S         ! loop indices

C Variables for calculating ambient RH as done in the aerosol module
            REAL, PARAMETER :: AA = 610.94      ! coefficients from Alduchov
            REAL, PARAMETER :: BB = 17.625      !  and Eskridge (1996)
            REAL, PARAMETER :: CC = 243.04
            REAL, PARAMETER :: EPSWATER = MWWAT / MWAIR
            REAL      :: ESAT             ! saturation vapor pressure
            REAL      :: BLKRH            ! fractional relative humidity

C Variables for calculating solute mass fraction & size-correction factors
            REAL( 8 ), PARAMETER :: ONE3  = 1.0D0 / 3.00D0
            REAL( 8 ), PARAMETER :: A0    = 1.0D0 / 3.70D0
            REAL( 8 ), PARAMETER :: A80   = 3.7D0 / 1.82D0
            REAL      :: RH               ! BLKRH constrained to 45-99% range
            REAL( 8 ) :: DRH              ! double precision RH
            REAL( 8 ) :: C0               ! (Diam @ formation) / (Diam @ ambient RH)
            REAL( 8 ) :: C80              ! (Diam @ 80% RH) / (Diam @ ambient RH)    
            REAL( 8 ) :: XSW              ! fractional solute weight fraction

C Density of dry sea salt [kg/m3] using volume additivity
            REAL( 8 )       :: RHOSW( 2 ) ! sea-salt particle density at ambient RH [g/m3]

C Maximum and minimum diameters for emission-flux integrations [m]
            REAL, PARAMETER :: DPMAX = 20.0E-6 ! upper limit used for the
                                               ! numerical integrations
            REAL      :: DPMINO           ! minimum diameter for open-ocean flux
            REAL      :: DPMAXO           ! maximum diameter for open-ocean flux
   
C Wind-speed-dependent scaling factors for emissions flux functions
            REAL      :: WDSCALO          ! Gong-Monahan open-ocean flux

C SST-dependent scaling factor for emissions flux functions
            REAL      :: SSTSCALO          ! Jaegle flux

C Fraction of whitecap coverage
            REAL      :: WCAP

C Lognormal parameters and numerically-integrated fluxes at ambient RH
            INTEGER   :: IPOS             ! position of ambient RH in data arrays
            REAL      :: DGNJRH           ! geometric mean of accum-mode flux [m]
            REAL      :: DGNKRH           ! geometric mean of coarse-mode flux [m]
            REAL      :: SIGJRH           ! geom std deviation of accum-mode flux
            REAL      :: SIGKRH           ! geom std deviation of coarse-mode flux
            REAL      :: FNJRH            ! magnitude of accum-mode flux [1/m2/s]
            REAL      :: FNKRH            ! magnitude of coarse-mode flux [1/m2/s]
            REAL      :: VFLXRH           ! total particle volume [m3/m2/s]

C Alternate forms of geometric std devs stored for efficiency
            REAL      :: LNSGJ, LNSGK     ! natural log of std dev
            REAL      :: RDIVJ, RDIVK     ! reciprocal of sqrt(2)*lnsg

C Geometric mean diameters by volume/mass [m]
            REAL      :: DGVJRH           ! accumulation mode
            REAL      :: DGVKRH           ! coarse mode

C Variables for converting 3rd moment emission rates to M0 and M2 rates
            REAL      :: FACTNUMJ         ! conversion for accumulation mode M0
            REAL      :: FACTNUMK         ! conversion for coarse mode M0
            REAL      :: FACTM2J          ! conversion for accumulation mode M2
            REAL      :: FACTM2K          ! conversion for coarse mode M2 

C Variables for calculating fraction of mass emissions assigned to each mode
            REAL      :: FFINJ            ! finite integral for accumulation mode
            REAL      :: FFINK            ! finite integral for coarse mode
            REAL      :: FRACMJ           ! mass fraction assigned to accumulation mode
            REAL      :: FRACMK           ! mass fraction assigned to coarse mode

C Mode-specific sea-salt mass emission rates [g/m3/s]
            REAL( 8 ) :: ESEASJ           ! accumulation mode
            REAL( 8 ) :: ESEASK           ! coarse mode

C 3rd moment emission rates [m3/m3/s]
            REAL      :: M3J              ! accumulation mode, 3rd moment
            REAL      :: M3K              ! coarse mode, 3rd moment
         
C Variables for calculating dry surface-area emission rate for accum mode
            REAL      :: WET_M3J, DRY_M3J ! wet & dry 3rd moment emis rates [m3/m3/s] 
            REAL      :: WET_M2J, DRY_M2J ! wet & dry 2nd moment emis rates [m3/m3/s] 
            REAL      :: WET_M3K, DRY_M3K ! wet & dry 3rd moment emis rates [m3/m3/s] 
            REAL      :: WET_M2K, DRY_M2K ! wet & dry 2nd moment emis rates [m3/m3/s] 

            INTEGER, SAVE     :: WSTEP  = 0    ! local write counter
            INTEGER           :: MDATE, MTIME  ! SSEMIS write date&time

            LOGICAL, SAVE :: FIRSTIME      = .TRUE.

C Mathematical constants; statement function for ERF approximation
            REAL, PARAMETER      :: TWO3 = 2.0 / 3.0
            REAL                 :: ERF        ! ERF approx. statement function
            REAL                 :: X          ! dummy argument for ERF
            ERF( X ) = SIGN( 1.0, X ) * SQRT( 1.0 - EXP( -4.0 * X * X / PI ) )

C----------------------------- Begin calc ------------------------------

C Initialize sea-salt output buffer
            IF ( SSEMDIAG .AND. WSTEP .EQ. 0 .AND. .NOT. L_DESID_DIAG ) SSBF = 0.0
            SEA_FACTNUM = 0.0
            SEA_FACTSRF = 0.0

            DO R = 1, NROWS
               DO C = 1, NCOLS
                  IF ( Grid_Data%OCEAN( C,R ) + Grid_Data%SZONE( C,R ) .GT. 0.0 ) THEN
                     OFRAC   = Grid_Data%OCEAN     ( C,R )
                     SFRAC   = Grid_Data%SZONE     ( C,R )
                     BLKPRS  = Met_Data%PRES       ( C,R,1 )
                     BLKTA   = Met_Data%TA         ( C,R,1 )
                     BLKQV   = Met_Data%QV         ( C,R,1 )
                     U10     = Met_Data%WSPD10     ( C,R )
                     SST     = max(Met_Data%TSEASFC( C,R ) - 273.15, 0.0 )
#ifdef mpas
                     RLAY1HT = Met_Data%RJACM ( C,R,1 ) / cell_thickness(C,1,1)
#else
                     RLAY1HT = Met_Data%RJACM ( C,R,1 ) / CELLHGT(c,r)
#endif
                  ELSE
                     DO N = 1, 2
                        DO S = 1, NSSSPC
                           SSOUTM( (N-1)*NSSSPC+S,C,R ) = 0.0
                        END DO
                        SSOUTN( N,C,R ) = 0.0
                        SSOUTS( N,C,R ) = 0.0
                     END DO
                     CYCLE
                  END IF

C Calculate fractional relative humidity in the grid cell
C Follow the same methodology as in other portions of the code just for the 10m RH
                  If ( BLKTA .Lt. stdtemp ) Then
                     ESAT = vp0 *Exp( 22.514 - ( 6.15e3 / BLKTA ) )
                  Else
                     ESAT = vp0 *Exp( svp2 * ( BLKTA -stdtemp ) / ( BLKTA -svp3 ) ) 
                  End If
                  BLKRH = BLKPRS * BLKQV / ( ( EPSWATER + BLKQV ) *  ESAT )       
                  BLKRH = MIN( 0.99, MAX( 0.005, BLKRH ) )

C Calculate RH-dependent size-correction factors and solute mass fraction
C using equations from Zhang et al. (2005), which are valid over the
C 45-99% RH range.  Store powers of RH for polynomial calculations.
                  RH = BLKRH
                  RH = MAX( 0.45, MIN( 0.99, RH ) )
                  DRH = REAL( RH, 8 )

C Calculate inverse of size-correction factors from Equation 3 of Lewis & Schwartz 2006 and Zhang et al 2006 equation 2. 
!                  C0  = 3.70D0 * ( (1.0D0-DRH) / (2.0D0-DRH) ) ** (1.0D0/3.0D0)
!                  C80 = 1.82D0 * ( (1.0D0-DRH) / (2.0D0-DRH) ) ** (1.0D0/3.0D0)
                  C0  = A0  * ( (2.0D0-DRH) / (1.0D0-DRH) ) ** ONE3
                  C80 = A80 * C0

C Calculate solute mass fraction using Equation 1 of Zhang et al.
                  XSW =         X_COEFF( 1 )
     &                + DRH * ( X_COEFF( 2 )
     &                + DRH * ( X_COEFF( 3 )
     &                + DRH * ( X_COEFF( 4 )
     &                + DRH * ( X_COEFF( 5 )
     &                + DRH * ( X_COEFF( 6 ) ) ) ) ) )


C Calculate sea-salt-particle density [g/m3] at ambient RH, assuming
C volume additivity of dry salt plus water
                  RHOSW( 1 ) = 1.0D+3 
     &                  / ( XSW / SEASPRAY_DENS( 2 ) + ( 1.0D0 - XSW ) * SPECIFIC_VOL_H2O )
                  RHOSW( 2 ) = 1.0D+3 
     &                  / ( XSW / SEASPRAY_DENS( 3 ) + ( 1.0D0 - XSW ) * SPECIFIC_VOL_H2O )

C Set minimum and maximum diameters for integration using the size-
C correction factors of Zhang et al. (2005)

C Gong-Monahan flux function is valid from 0.005-5.0 um dry radius;
C multiply by 4 to get wet diameter at 80% RH
!                  DPMINO = 2.0E-8 / REAL( C80, 4 )
!                  DPMAXO = MIN( DPMAX, DPMAX / REAL( C80, 4 ) )
                  DPMINO = 2.0E-8 * REAL( C80, 4 )
                  DPMAXO = MIN( DPMAX, DPMAX * REAL( C80, 4 ) )


C deLeeuw flux function is valid from 1.6-20 um diameter at formation
!                 DPMINS = 1.6E-6 * C0 
!                 DPMAXS = MIN( DPMAX, DPMAX * C0 )
            
C Calculate scaling factors to correct the tabulated fluxes for the 10m
C wind speed in this cell.  Note: tabulated values are based on 1 m/s.
                  WDSCALO = MIN( U10, 20.0 ) ** 3.41        ! Gong-Monahan flux function
!                 WDSCALS = EXP( 0.23 * MIN( U10, 9.0 ) )   ! deLeeuw flux function

C SST scaling factor based on Jaegle et al. (2011)
C linearized by Ovadnevaite et al. (2014)
                  SSTSCALO = 0.38 + 0.054 * SST

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
                  VFLXRH = VFLUX( IPOS ) * ( OFRAC * SSTSCALO
     &            + (SFRAC * 0.5) / WCAP )

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

                  ESEASJ = VFLXRH * FRACMJ * WDSCALO * RLAY1HT * REAL( RHOSW( 1 ), 4 )

                  ESEASK = VFLXRH * FRACMK * WDSCALO * RLAY1HT * REAL( RHOSW( 2 ), 4 )

                  DO S = 1, NSSSPC
                     IF ( S .EQ. KH2O ) THEN    ! Water
                        SSOUTM( S,C,R ) = ESEASJ * REAL( ( 1.0D0 - XSW ), 4 )
                        SSOUTM( S+NSSSPC,C,R ) = ESEASK * REAL( ( 1.0D0 - XSW ), 4 )
                     ELSE
                        SSOUTM( S,C,R ) = ESEASJ * REAL( XSW, 4 )
     &                                    * SEA_SPC( S )%SPCFAC( 1 )
                        SSOUTM( S+NSSSPC,C,R ) = ESEASK * REAL( XSW, 4 )
     &                                    * SEA_SPC( S )%SPCFAC( 2 )
                     END IF

                  END DO

C Calculate mode-specific 3rd moment emission rates [m3/m3/s]
                  M3J = ESEASJ * F6PI / REAL( RHOSW( 1 ), 4 ) + TINY(0.0)
                  M3K = ESEASK * F6PI / REAL( RHOSW( 2 ), 4 ) + TINY(0.0)

C Calculate factors for converting 3rd moment emission rates into number
C and 2nd moment emission rates.  See Equations 7b and 7c of Binkowski &
C Roselle (2003)
                  FACTNUMJ = EXP( 4.5 * LNSGJ * LNSGJ ) / DGVJRH ** 3
                  FACTNUMK = EXP( 4.5 * LNSGK * LNSGK ) / DGVKRH ** 3
                  FACTM2J  = EXP( 0.5 * LNSGJ * LNSGJ ) / DGVJRH
                  FACTM2K  = EXP( 0.5 * LNSGK * LNSGK ) / DGVKRH 

C Calculate mode-specific emission rates of particle number [1/s]
                  SSOUTN( 1,C,R ) = M3J * FACTNUMJ
                  SSOUTN( 2,C,R ) = M3K * FACTNUMK

C Calculate mode-specific dry surface area emission rates [m2/m3/s].  
C Subtract water from 3rd moment to obtain dry 3rd moment emission rate.  
C Calculate dry 2nd moment while holding the standard deviation constant.
C Multiply dry 2nd moment by PI to obtain dry surface area emission rate.
                  WET_M3J = M3J 
                  WET_M2J = M3J * FACTM2J
                  DRY_M3J = WET_M3J - F6PI * SSOUTM( KH2O,C,R ) * 1.0E-06
                  DRY_M2J = WET_M2J * ( DRY_M3J / WET_M3J ) ** TWO3

                  WET_M3K = M3K
                  WET_M2K = M3K * FACTM2K
                  DRY_M3K = WET_M3K - F6PI * SSOUTM( KH2O+NSSSPC,C,R ) * 1.0E-06
                  DRY_M2K = WET_M2K * ( DRY_M3K / WET_M3K ) ** TWO3

                  SSOUTS( 1,C,R ) = PI * DRY_M2J
                  SSOUTS( 2,C,R ) = PI * DRY_M2K

! Propagate Number and Surface Area Scaling Factors back to Emissions
! Module so that the sea spray emissions can be scaled appropriately            
                  SEA_FACTNUM( 1,C,R ) = 0.0
                  SEA_FACTNUM( 2,C,R ) = FACTNUMJ
                  SEA_FACTNUM( 3,C,R ) = FACTNUMK
                  SEA_FACTSRF( 1,C,R ) = 0.0
                  SEA_FACTSRF( 2,C,R ) = SSOUTS( 1,C,R ) / M3J
                  SEA_FACTSRF( 3,C,R ) = SSOUTS( 2,C,R ) / M3K

C Update the SSBF array, for writing the diagnostic sea-salt-emission file.
                  IF ( SSEMDIAG .AND. .NOT. L_DESID_DIAG ) THEN
                     V = 0
                     DO N = 1, 2
                        DO S = 1, NSSSPC
                           IF ( S .EQ. KH2O .OR. SEA_SPC( S )%SPCFAC( N ) .GT. 0 ) THEN
                              V = V + 1
                              SSOUTD( V ) = SSOUTM( (N-1)*NSSSPC+S,C,R )
                           END IF
                        END DO
                     END DO

                     DO N = 1, 2
                        V = V + 1
                        SSOUTD( V ) = SSOUTN( N,C,R )
                     END DO

                     DO N = 1, 2
                        V = V + 1
                        SSOUTD( V ) = SSOUTS( N,C,R )
                     END DO

#ifdef mpas
                     DO S = 1, NSSDIAG
                        SSBF( S,C,R ) = SSBF( S,C,R ) + SSOUTD( S ) * cell_vol(c,1,1)
     &                                * FLOAT( TIME2SEC ( TSTEP( 2 ) ) )
                     END DO
#else
                     AIRVOL = CELLVOL(C,R) / Met_Data%RJACM( C,R,1 )

                     DO S = 1, NSSDIAG
                        SSBF( S,C,R ) = SSBF( S,C,R ) + SSOUTD( S ) * AIRVOL
     &                                * FLOAT( TIME2SEC ( TSTEP( 2 ) ) )
                     END DO
#endif

                  END IF  ! SSEMDIAG

               END DO   ! C
            END DO   ! R

C If last call this hour, write out the total sea-salt emissions [g/s].
C Then reset the sea-salt emissions array and local write counter.
#ifndef mpas
            IF ( SSEMDIAG .AND. .NOT. L_DESID_DIAG ) THEN

#ifdef parallel_io
               IF ( FIRSTIME ) THEN
                  FIRSTIME = .FALSE.
                  IF ( .NOT. IO_PE_INCLUSIVE ) THEN
                     IF ( .NOT. OPEN3( CTM_DRY_DEP_1, FSREAD3, PNAME ) ) THEN
                        XMSG = 'Could not open ' // TRIM( CTM_DRY_DEP_1 )
                        CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                     END IF
                  END IF
               END IF
#endif

               WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )

               IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN
                  IF ( .NOT. CURRSTEP( JDATE, JTIME, STDATE, STTIME, TSTEP( 1 ),
     &                                 MDATE, MTIME ) ) THEN
                     XMSG = 'Cannot get step date and time'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
                  END IF
                  CALL NEXTIME( MDATE, MTIME, TSTEP( 1 ) )
                  DO V = 1, NSSDIAG
                     DO R = 1, NROWS
                        DO C = 1, NCOLS
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
#endif

            RETURN
            
         END SUBROUTINE GET_SSEMIS

      END MODULE SSEMIS
