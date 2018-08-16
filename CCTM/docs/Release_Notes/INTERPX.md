# Implement unique INTERPX offsets for METCRO2D

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA  

## Brief Description


The pull request allows the METCRO2D file to have different horizontal dimensions and origin than the METCRO3D file in **hrdriver.F**. The code change defines the INTERPX offsets for the METCRO2D file unique to this input file. It provides greater compatibilty with the two-way version of the CMAQ model because because this model version does not use the METCRO3D file. Note the ros3 and smvgear solvers do not require this fix.

## Significance and Impact

A user can more easily use the repository version of an ebi solver for a photochemical mechanism when switching from the off-line to the two-way version of the CMAQ model.

## Affected Files:

CCTM/src/gas/ebi_cb05e51_ae6_aq/hrdriver.F  
CCTM/src/gas/ebi_cb05e51_ae6nvPOA_aq/hrdriver.F  
CCTM/src/gas/ebi_cb05eh51_ae6_aq/hrdriver.F  
CCTM/src/gas/ebi_cb05mp51_ae6_aq/hrdriver.F  
CCTM/src/gas/ebi_cb05tucl_ae6_aq/hrdriver.F  
CCTM/src/gas/ebi_cb6r3_ae6_aq/hrdriver.F  
CCTM/src/gas/ebi_cb6r3_ae6nvPOA_aq/hrdriver.F  
CCTM/src/gas/ebi_racm2_ae6_aq/hrdriver.F  
CCTM/src/gas/ebi_saprc07tb_ae6_aq/hrdriver.F  
CCTM/src/gas/ebi_saprc07tc_ae6_aq/hrdriver.F  
CCTM/src/gas/ebi_saprc07tc_ae6nvPOA_aq/hrdriver.F  
CCTM/src/gas/ebi_saprc07tic_ae6i_aq/hrdriver.F  
CCTM/src/gas/ebi_saprc07tic_ae6invPOA_aq/hrdriver.F  
UTIL/create_ebi/template_RXNSU_OPT/hrdriver.F  

## References:    

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #249](https://github.com/USEPA/CMAQ_Dev/pull/249)  

### Commit IDs:

52f016be7f9ed0ae8a1e25145697c6d39971fefd  
61ce9b8ae441414411f733870b0a3c7a1e21776f  
a839d83b5a5a757e370496862cf9e0433db2ec1f  
13375fdcce05d3f02eb9c4ef00b2820fa1a5141b  

