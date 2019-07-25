# Implementation of New Chemical Namelist Format

[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

## Brief Description
With this improvement the chemical namelists which specify names and properties for the reactive gas, aerosol, non-reactive, and tracer species have been revised to be fixed-width and comma delimted. In this way, the files may now be read in with standard Fortran namelist input functionality rather than relying on the manual parsing method that had been installed previously. 

## Significance and Impact
This update has no effect on model results or runtime but will make it possible to synthesize and generalize chemical property inputs in future versions of CMAQ.

## Affected Files
CCTM/src/MECHS/cb6mp_ae6_aq/AE_cb6mp_ae6_aq.nml  
CCTM/src/MECHS/cb6mp_ae6_aq/NR_cb6mp_ae6_aq.nml  
CCTM/src/MECHS/cb6r3_ae6_aq/AE_cb6r3_ae6_aq.nml  
CCTM/src/MECHS/cb6r3_ae6_aq/GC_cb6r3_ae6_aq.nml  
CCTM/src/MECHS/cb6r3_ae6_aq/NR_cb6r3_ae6_aq.nml  
CCTM/src/MECHS/cb6r3_ae7_aq/AE_cb6r3_ae7_aq.nml  
CCTM/src/MECHS/cb6r3_ae7_aq/GC_cb6r3_ae7_aq.nml  
CCTM/src/MECHS/cb6r3_ae7_aq/NR_cb6r3_ae7_aq.nml  
CCTM/src/MECHS/cb6r3_ae7_aqkmt2/AE_cb6r3_ae7_aq.nml  
CCTM/src/MECHS/cb6r3_ae7_aqkmt2/GC_cb6r3_ae7_aq.nml  
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/AE_cb6r3m_ae7_kmtbr.nml  
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/GC_cb6r3m_ae7_kmtbr.nml  
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/NR_cb6r3m_ae7_kmtbr.nml  
CCTM/src/MECHS/racm2_ae6_aq/AE_racm2_ae6_aq.nml  
CCTM/src/MECHS/racm2_ae6_aq/GC_racm2_ae6_aq.nml  
CCTM/src/MECHS/racm2_ae6_aq/NR_racm2_ae6_aq.nml  
CCTM/src/MECHS/saprc07tc_ae6_aq/AE_saprc07tc_ae6_aq.nml  
CCTM/src/MECHS/saprc07tc_ae6_aq/GC_saprc07tc_ae6_aq.nml  
CCTM/src/MECHS/saprc07tc_ae6_aq/NR_saprc07tc_ae6_aq.nml   
CCTM/src/MECHS/saprc07tic_ae6i_aq/AE_saprc07tic_ae6i_aq.nml  
CCTM/src/MECHS/saprc07tic_ae6i_aq/GC_saprc07tic_ae6i_aq.nml  
CCTM/src/MECHS/saprc07tic_ae6i_aq/NR_saprc07tic_ae6i_aq.nml  
CCTM/src/MECHS/saprc07tic_ae6i_aqkmti/AE_saprc07tic_ae6i_aq.nml  
CCTM/src/MECHS/saprc07tic_ae6i_aqkmti/GC_saprc07tic_ae6i_aq.nml  
CCTM/src/MECHS/saprc07tic_ae7i_aq/AE_saprc07tic_ae7i_aq.nml  
CCTM/src/MECHS/saprc07tic_ae7i_aq/GC_saprc07tic_ae7i_aq.nml  
CCTM/src/MECHS/saprc07tic_ae7i_aq/NR_saprc07tic_ae7i_aq.nml  
CCTM/src/MECHS/saprc07tic_ae7i_aqkmt2/AE_saprc07tic_ae7i_aq.nml  
CCTM/src/MECHS/saprc07tic_ae7i_aqkmt2/GC_saprc07tic_ae7i_aq.nml  
CCTM/src/MECHS/trac0/Species_Table_TR_0.nml  


## References

-----
## Internal Records:
#### Relevant Pull Requests:
73e21ed739f81f0d0425654b744ff3a854eecbf3  
0789e98b2e513dc4f23a75a2233945d5ec6a1a26  
3d52d783e5e8c7e1dd47177bcba43a8fe3d7a403  
79fb07d612d3794f0da2b25e43fba0cbdc17281b  
e440040b0b137659b2accf922324b5d153241e6f  
6335a3e4c2a422109c965d4152a20790ed0df777  
bf4ca9b5b9716b3dcc9b878d3f21cd80301c0e51  
30f9c524ce1cb77ea769f705588c2e9c5f781bba  
99a608e2d4abf052fe5edd8a8c3a968a3635884d  
de6dd6de23b5634419d376097e02ad2cd03c4b2c  
8d5c6712544e1e4067dbb85b9786cb247cc76406  
f73ee6741f5426f890aac0864ff3ee234bab0a19  
c9b20608770123e39ceefd1bcb3e78fa40f28a63  
2ed789e028b9c23337ab4444a5b25a60f638c6f6  
4bfed6b0f62ce1c9348c6900d3d3decaf952d66c  
f7f94ef71dfb6066bcfd70672db05d9f0882985a  
0e1227d721416b15b0b0a6cbc8ab2ddfe27f32d3  

-----

