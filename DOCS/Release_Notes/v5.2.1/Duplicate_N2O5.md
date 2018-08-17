# Remove duplicative heterogeneous uptake of N2O5

**Author/P.O.C.:**, [Golam Sarwar](mailto:sarwar.golam@epa.gov), Computational Exposure Division, U.S. EPA  

## Brief Description
The mechanism contained two heterogeneous hydrolysis reactions for N2O5. Removed one reaction.

## Significance and Impact
CMAQ with the corrected mechanism produces more N2O5 and less HNO3

## Affected Files:

CCTM/src/MECHS/saprc07tic_ae6i_aq/RXNS_DATA_MODULE.F90  
CCTM/src/MECHS/saprc07tic_ae6i_aq/RXNS_FUNC_MODULE.F90  
CCTM/src/MECHS/saprc07tic_ae6i_aq/mech_saprc07tic_ae6i_aq.def   
CCTM/src/MECHS/saprc07tic_ae6invPOA_aq/RXNS_DATA_MODULE.F90  
CCTM/src/MECHS/saprc07tic_ae6invPOA_aq/RXNS_FUNC_MODULE.F90  
CCTM/src/MECHS/saprc07tic_ae6invPOA_aq/mech_saprc07tic_ae6invPOA_aq.def  
CCTM/src/gas/ebi_saprc07tic_ae6i_aq/hrdata_mod.F  
CCTM/src/gas/ebi_saprc07tic_ae6i_aq/hrdriver.F  
CCTM/src/gas/ebi_saprc07tic_ae6i_aq/hrg1.F  
CCTM/src/gas/ebi_saprc07tic_ae6i_aq/hrg2.F  
CCTM/src/gas/ebi_saprc07tic_ae6i_aq/hrg3.F  
CCTM/src/gas/ebi_saprc07tic_ae6i_aq/hrg4.F  
CCTM/src/gas/ebi_saprc07tic_ae6i_aq/hrinit.F  
CCTM/src/gas/ebi_saprc07tic_ae6i_aq/hrprodloss.F  
CCTM/src/gas/ebi_saprc07tic_ae6i_aq/hrrates.F  
CCTM/src/gas/ebi_saprc07tic_ae6i_aq/hrsolver.F  
CCTM/src/gas/ebi_saprc07tic_ae6invPOA_aq/hrdata_mod.F  
CCTM/src/gas/ebi_saprc07tic_ae6invPOA_aq/hrdriver.F  
CCTM/src/gas/ebi_saprc07tic_ae6invPOA_aq/hrg1.F  
CCTM/src/gas/ebi_saprc07tic_ae6invPOA_aq/hrg2.F  
CCTM/src/gas/ebi_saprc07tic_ae6invPOA_aq/hrg3.F  
CCTM/src/gas/ebi_saprc07tic_ae6invPOA_aq/hrg4.F  
CCTM/src/gas/ebi_saprc07tic_ae6invPOA_aq/hrinit.F  
CCTM/src/gas/ebi_saprc07tic_ae6invPOA_aq/hrprodloss.F  
CCTM/src/gas/ebi_saprc07tic_ae6invPOA_aq/hrrates.F  
CCTM/src/gas/ebi_saprc07tic_ae6invPOA_aq/hrsolver.F  

## References:    

-----
## Internal Records:


### Relevant Pull Requests:  
  [PR #251](https://github.com/USEPA/CMAQ_Dev/pull/251)  

### Commit IDs:
b12435782761a2e9149eddbda811038a6f772782  
6b343f8d141a6bc3a11b1620b5b091b8f8ccc4c1  
089637fefea18b671411f0f6cf921f76610fa280  
7bb278641f3dee936db4421bea05cc57e75325c0  
254f78d1310d64e42c3096833849b52e9b6af192  
228fb8df96cf296d52fdbb4d78542ff759bb356b  

