# Improve Consistency of the Treatment of Aerosol Wet/Dry Moments Throughout CMAQ

**Author/P.O.C.:**, [Ben Murphy](mailto:murphy.benjamin@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
The numerical treatment of aerosol water has been made consistent throughout the aerosol module. It has been noted for some time that, due to the large amount of mass and fast equilibration times, transporting the second moment with aerosol water included caused numerical instabilities. Thus, the contribution of aerosol water to the second moment is 'removed' before exiting the aerosol module -- a fixed mode width is assumed for this calculation. As a result, at times it was not clear in code when the locally defined second and third moments referred to the 'wet' moments and when they referred to the 'dry' moments.

The code has been updated with a new routine and a flag that identify the state of the second and third moments and easily convert between the two.

## Significance and Impact

This affects the stability of the mode widths (standard deviation). It's overall effect on aerosol concentrations is mixed.

## Affected Files:
  aero/aero6/AEROSOL_CHEMISTRY.F  
  aero/aero6/AERO_DATA.F  
  aero/aero6/AOD_DEFN.F  
  aero/aero6/SOA_DEFN.F  
  aero/aero6/aero_depv.F  
  aero/aero6/aero_driver.F  
  aero/aero6/aero_subs.F  
  aero/aero6/getpar.f  
  aero/aero6/isocom.f  
  aero/aero6/opdiam.F  
  aero/aero6i/AEROSOL_CHEMISTRY.F  
  aero/aero6i/AERO_DATA.F  
  aero/aero6i/SOA_DEFN.F  
  aero/aero6i/aero_driver.F  
  aero/aero6i/opdiam.F  
  gas/ebi_cb05e51_ae6_aq/hrdriver.F  
  gas/ebi_cb05mp51_ae6_aq/hrdriver.F  
  gas/ebi_cb05tucl_ae6_aq/hrdriver.F  
  gas/ebi_cb05tump_ae6_aq/hrdriver.F  
  gas/ebi_cb6r3_ae6_aq/hrdriver.F  
  gas/ebi_racm2_ae6_aq/hrdriver.F  
  gas/ebi_saprc07tb_ae6_aq/hrdriver.F  
  gas/ebi_saprc07tc_ae6_aq/hrdriver.F  
  gas/ebi_saprc07tic_ae6i_aq/hrdriver.F   
  gas/ros3/rbdriver.F  
  gas/smvgear/grdriver.F  
  phot/inline/AERO_PHOTDATA.F  
  vdiff/acm2/aero_sedv.F  

## References:
  NA

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #63](https://github.com/usepa/cmaq_dev/pull/63)  
  [PR #97](https://github.com/usepa/cmaq_dev/pull/97)  
  [PR #70](https://github.com/usepa/cmaq_dev/pull/70)  

### Commit IDs:
  b775f74dabd842020d5636ea60ed250a64ee66bc  
  dc225183ab13afc66b09d5c14565ddf58997c176  
  9306b5a677ee89cb2b34acc39d30748908bdefe8  
  dd74a9b5043b70521206d713347105d455a84ec9  
  dbefcaa8525fb0a034dad2208d2c57c0c77620dd  
  944c8a2a2b62c55ba2940637af0421e7674753fa  
  a5345ab688768d0f871f9c6a750f02a027bc8dd7  
  da6d3f968ead4562799710db9fa5252e8f27af2b  
  784d6b5a790b1f5a860201b0f65275baeeb855c0  
  1fc2103b37a1f34d11d713925eb5c60441b75599  
  aca3b4276462755364e78266383156ed3e456f81  
  3469d83ba73c32edf9f230ede1e666a79a97d503  
  d877ae6386388f70ce94244c555c998dd6a567c9  
  ac742b7a17313327fca21da5ece7622506a10fa3  
  946db3e475b700ee33c84d4d76873013925fe674  
  1fbcacefab39e2812c37c1dfa7732fab5f5f5212  
  3faaf2cba50019a28a1dd9569d236c92f38627d3  
  419edf5a9640abcd51ba60c180a60995e1fda49a  
  e834c3d2aa53a2ef00a23e49ad6535caac1a9da9  
  25f033395ceaec41775dfcbb1b0228c1f7178d96  
  b98829fa5e7b30f8e3ff2ca1327e7ac1d70f8b2b  
  03fed13b114955cd9c759213e8b6a19c1f7af9bd  
