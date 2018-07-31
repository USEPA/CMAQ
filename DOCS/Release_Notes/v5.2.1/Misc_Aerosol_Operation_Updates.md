# Update and homogenize Species Definition Files for Post-processing CMAQ output variables
**Author/P.O.C.:**, [Ben Murphy](mailto:murphy.benjamin@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
  Due to the large number of chemical/aerosol processing options available in CMAQ, the template for post-processing raw output species has been incorporated into the folder for each mechanism. These files, which all beginning with "SpecDef\_", map CMAQ internal species to values that can be compared to observed quantities. For example, they provide the necessary map to values like O:C for organic aerosol, PM_coarse, NOy, etc. An analogous file for post-processing deposition quantities has also been added.

## Significance and Impact

No impact on results. These changes were made to benefit users and to maintain consistency across environments and within the CMAQ community.

## Affected Files:
  MECHS/cb05e51_ae6_aq/SpecDef_Dep_cb05e51_ae6_aq.txt  
  MECHS/cb05e51_ae6_aq/SpecDef_cb05e51_ae6_aq.txt  
  MECHS/cb05e51_ae6nvPOA_aq/SpecDef_Dep_cb05e51_ae6nvPOA_aq.txt  
  MECHS/cb05e51_ae6nvPOA_aq/SpecDef_cb05e51_ae6nvPOA_aq.txt  
  MECHS/cb05eh51_ae6_aq/SpecDef_Dep_cb05eh51_ae6_aq.txt  
  MECHS/cb05eh51_ae6_aq/SpecDef_cb05eh51_ae6_aq.txt  
  MECHS/cb05mp51_ae6_aq/SpecDef_Dep_cb05mp51_ae6_aq.txt  
  MECHS/cb05mp51_ae6_aq/SpecDef_cb05mp51_ae6_aq.txt  
  MECHS/cb05tucl_ae6_aq/SpecDef_Dep_cb05tucl_ae6_aq.txt  
  MECHS/cb05tucl_ae6_aq/SpecDef_cb05tucl_ae6_aq.txt  
  MECHS/cb05tump_ae6_aq/SpecDef_Dep_cb05tump_ae6_aq.txt  
  MECHS/cb05tump_ae6_aq/SpecDef_cb05tump_ae6_aq.txt  
  MECHS/cb6r3_ae6_aq/SpecDef_Dep_cb6r3_ae6_aq.txt  
  MECHS/cb6r3_ae6_aq/SpecDef_cb6r3_ae6_aq.txt  
  MECHS/cb6r3_ae6nvPOA_aq/SpecDef_Dep_cb6r3_ae6nvPOA_aq.txt  
  MECHS/cb6r3_ae6nvPOA_aq/SpecDef_cb6r3_ae6nvPOA_aq.txt  
  MECHS/racm2_ae6_aq/SpecDef_Dep_racm2_ae6_aq.txt  
  MECHS/racm2_ae6_aq/SpecDef_racm2_ae6_aq.txt  
  MECHS/saprc07tb_ae6_aq/SpecDef_Dep_saprc07tb_ae6i_aq.txt  
  MECHS/saprc07tb_ae6_aq/SpecDef_saprc07tb_ae6i_aq.txt  
  MECHS/saprc07tc_ae6_aq/SpecDef_Dep_saprc07tc_ae6_aq.txt  
  MECHS/saprc07tc_ae6_aq/SpecDef_saprc07tc_ae6_aq.txt  
  MECHS/saprc07tc_ae6nvPOA_aq/SpecDef_Dep_saprc07tc_ae6nvPOA_aq.txt  
  MECHS/saprc07tc_ae6nvPOA_aq/SpecDef_saprc07tc_ae6nvPOA_aq.txt  
  MECHS/saprc07tic_ae6i_aq/SpecDef_Dep_saprc07tic_ae6i_aq.txt  
  MECHS/saprc07tic_ae6i_aq/SpecDef_saprc07tic_ae6i_aq.txt  
  MECHS/saprc07tic_ae6i_aqkmti/SpecDef_Dep_saprc07tic_ae6i_aqkmti.txt  
  MECHS/saprc07tic_ae6i_aqkmti/SpecDef_saprc07tic_ae6i_aqkmti.txt  
  MECHS/saprc07tic_ae6invPOA_aq/SpecDef_Dep_saprc07tic_ae6invPOA_aq2.txt  
  MECHS/saprc07tic_ae6invPOA_aq/SpecDef_saprc07tic_ae6invPOA_aq.txt  

## References:
NA

-----
## Internal Records:

### Relevant Pull Requests:   
  [PR #120](https://github.com/usepa/cmaq_dev/pull/120)

### Commit IDs:
  b7e9682ef5ac055ee7006b3fe41409824a7cfd5b  
  137313c66b63a18527c319b8fe3d0053424e643f  
  7597be34efdec9904f90a197404db5e66fdb512f  
  bf7dbd1ea827f672d6736b49be0496210bf72687  
  f8c9b2be461370afe21dfee24d7cbea3975cfbf1  
  036d6f00767dff7c26f38d4184f10855f6d43cec  
  fec1da7555c9e318135b4173506aeaecb364b3ee  
  06965d0d48df8733f81d67075d1f95825b5cfdc1  
  dc471c432d0f98f34a44279a59f0d7f79013a96f  
  daafd9f8fd177d3e0c42938c04822dbbc01db9cd  
  338c6db762c1660e0270abbd5b53b591e9c96bd4  
  20c6a90dbf749b3ba7c793ac4a8337653088c130  
  ada953c8b402cf2138f680d477b00689b526656c  
  221ae3074d581e37244dad2ce81068de276fd3c5  
