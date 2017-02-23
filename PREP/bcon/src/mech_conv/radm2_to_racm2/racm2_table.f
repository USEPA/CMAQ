
      module racm2_table

      integer, parameter :: nsptab = 56    ! No. of RACM2 species defined in this table
      integer, parameter :: mxcvsp = 1     ! Max no. of RADM species required for a table species

      type mech_conv
         character( 16 ) :: tab_spc        ! table spec names
         integer         :: n_radm2_spc    ! no. of RADM2 species defining each table species
         character( 16 ) :: radm2_spc( mxcvsp )
         real            :: coeff( mxcvsp )
      end type mech_conv

      type ( mech_conv ), parameter :: spcmap( nsptab ) = (/
C                         n_radm_spc             coeff( 1 )
C                 tab_spc     |   radm2_spc( 1 )    |
     & mech_conv( 'NO2     ', 1, (/'NO2    '/), (/ 1.0 /)),   !  1
     & mech_conv( 'NO      ', 1, (/'NO     '/), (/ 1.0 /)),   !  2
     & mech_conv( 'O3P     ', 1, (/'O3P    '/), (/ 1.0 /)),   !  3
     & mech_conv( 'O3      ', 1, (/'O3     '/), (/ 1.0 /)),   !  4
     & mech_conv( 'NO3     ', 1, (/'NO3    '/), (/ 1.0 /)),   !  5
     & mech_conv( 'N2O5    ', 1, (/'N2O5   '/), (/ 1.0 /)),   !  6
     & mech_conv( 'HNO3    ', 1, (/'HNO3   '/), (/ 1.0 /)),   !  7
     & mech_conv( 'O1D     ', 1, (/'O1D    '/), (/ 1.0 /)),   !  8
     & mech_conv( 'HO      ', 1, (/'HO     '/), (/ 1.0 /)),   !  9
     & mech_conv( 'HONO    ', 1, (/'HONO   '/), (/ 1.0 /)),   ! 10
     & mech_conv( 'HO2     ', 1, (/'HO2    '/), (/ 1.0 /)),   ! 11
     & mech_conv( 'CO      ', 1, (/'CO     '/), (/ 1.0 /)),   ! 12
     & mech_conv( 'HNO4    ', 1, (/'HNO4   '/), (/ 1.0 /)),   ! 13
     & mech_conv( 'H2O2    ', 1, (/'H2O2   '/), (/ 1.0 /)),   ! 14
     & mech_conv( 'SO2     ', 1, (/'SO2    '/), (/ 1.0 /)),   ! 15
     & mech_conv( 'SULF    ', 1, (/'SULF   '/), (/ 1.0 /)),   ! 16
     & mech_conv( 'MO2     ', 1, (/'MO2    '/), (/ 1.0 /)),   ! 17
     & mech_conv( 'HCHO    ', 1, (/'HCHO   '/), (/ 1.0 /)),   ! 18 
     & mech_conv( 'OP1     ', 1, (/'OP1    '/), (/ 1.0 /)),   ! 19
     & mech_conv( 'OP2     ', 1, (/'OP2    '/), (/ 1.0 /)),   ! 20
     & mech_conv( 'ONIT    ', 1, (/'ONIT   '/), (/ 1.0 /)),   ! 21
     & mech_conv( 'ACT     ', 1, (/'KET    '/), (/ 0.5 /)),   ! 22
     & mech_conv( 'KET     ', 1, (/'KET    '/), (/ 0.5 /)),   ! 23     
     & mech_conv( 'ACO3    ', 1, (/'ACO3   '/), (/ 1.0 /)),   ! 24
     & mech_conv( 'PAN     ', 1, (/'PAN    '/), (/ 1.0 /)),   ! 25
     & mech_conv( 'PAA     ', 1, (/'PAA    '/), (/ 1.0 /)),   ! 26
     & mech_conv( 'ORA2    ', 1, (/'ORA2   '/), (/ 1.0 /)),   ! 27
     & mech_conv( 'PPN     ', 1, (/'TPAN   '/), (/ 1.0 /)),   ! 28
     & mech_conv( 'ACD     ', 1, (/'ALD    '/), (/ 0.85/)),   ! 29
     & mech_conv( 'ALD     ', 1, (/'ALD    '/), (/ 0.15/)),   ! 30     
     & mech_conv( 'ORA1    ', 1, (/'ORA1   '/), (/ 1.0 /)),   ! 31
     & mech_conv( 'GLY     ', 1, (/'GLY    '/), (/ 1.0 /)),   ! 32
     & mech_conv( 'MGLY    ', 1, (/'MGLY   '/), (/ 1.0 /)),   ! 33
     & mech_conv( 'CSL     ', 1, (/'CSL    '/), (/ 1.0 /)),   ! 34
     & mech_conv( 'MACR    ', 1, (/'MACR   '/), (/ 1.0 /)),   ! 35
     & mech_conv( 'MVK     ', 1, (/'MVK    '/), (/ 1.0 /)),   ! 36
     & mech_conv( 'UALD    ', 1, (/'ISOPROD'/), (/ 1.0 /)),   ! 37
     & mech_conv( 'DCB1    ', 1, (/'DCB    '/), (/ 0.34/)),   ! 38
     & mech_conv( 'DCB2    ', 1, (/'DCB    '/), (/ 0.33/)),   ! 39
     & mech_conv( 'DCB3    ', 1, (/'DCB    '/), (/ 0.33/)),   ! 40         
     & mech_conv( 'ETE     ', 1, (/'OL2    '/), (/ 1.0 /)),   ! 41
     & mech_conv( 'ISO     ', 1, (/'ISO    '/), (/ 1.0 /)),   ! 42
     & mech_conv( 'API     ', 1, (/'TERP   '/), (/ 1.0 /)),   ! 43
     & mech_conv( 'ETH     ', 1, (/'ETH    '/), (/ 1.0 /)),   ! 44
     & mech_conv( 'HC3     ', 1, (/'HC3    '/), (/ 1.0 /)),   ! 45
     & mech_conv( 'HC5     ', 1, (/'HC5    '/), (/ 1.0 /)),   ! 46
     & mech_conv( 'HC8     ', 1, (/'HC8    '/), (/ 1.0 /)),   ! 47
     & mech_conv( 'TOL     ', 1, (/'TOL    '/), (/ 1.0 /)),   ! 48
     & mech_conv( 'XYM     ', 1, (/'XYL    '/), (/ 0.34/)),   ! 49    
     & mech_conv( 'XYP     ', 1, (/'XYL    '/), (/ 0.33/)),   ! 50     
     & mech_conv( 'XYO     ', 1, (/'XYL    '/), (/ 0.33/)),   ! 51     
     & mech_conv( 'OLT     ', 1, (/'OLT    '/), (/ 1.0 /)),   ! 52
     & mech_conv( 'OLI     ', 1, (/'OLI    '/), (/ 1.0 /)),   ! 53
     & mech_conv( 'BEN     ', 1, (/'BENZENE'/), (/ 1.0 /)),   ! 54
     & mech_conv( 'HG      ', 1, (/'HG     '/), (/ 1.0 /)),   ! 55
     & mech_conv( 'HGIIGAS ', 1, (/'HGIIGAS'/), (/ 1.0 /)) /) ! 56

      character( 128 ) :: line1 = " RACM2 gas mechanism with AERO6 species and toxics"
      include './out_fl_name.ext'
      character(  51 ) :: str = ' in RADM2 part of RACM2 list, not in profile data'

      end module racm2_table
