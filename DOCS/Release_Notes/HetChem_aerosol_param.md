# Simplify Propagation of Aerosol Surface Area to Heterogeneous Chemistry Algorithm

[Ben Murphy](mailto:murphy.ben@epa.gov),  U.S. Environmental Protection Agency


## Brief Description
This update abstracts the algorithm for calculating aerosol surface area and aerosol diameter from the heterogeneous chemistry algorithm and locates it in the AERO_SUBS module instead. In this way, as modifications are made to the physics of the aerosol module, they will propagate automatically to the heterogeneous chemistry algorithm with limited maintenance required. An update was also made to generalize the calculation for the sum of the total organic aerosol mass in each grid cell. Previously this was accomplished by looking up specific species IDs. Now it utilizes a pre-populated mask of length (naero) which identifies each aerosol species as OM or not. 


## Significance and Impact
This update has no effect on model results or runtime but will make it possible to generalize the aerosol module in future CMAQ versions.

## Affected Files
CCTM/src/aero/aero6/AEROSOL_CHEMISTRY.F  
CCTM/src/aero/aero6/AERO_DATA.F  
CCTM/src/aero/aero6/aero_subs.F  
CCTM/src/gas/ebi_cb6r3_ae6_aq/hrdriver.F  
CCTM/src/gas/ebi_cb6r3_ae7_aq/hrdriver.F  
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/hrdriver.F  
CCTM/src/gas/ebi_racm2_ae6_aq/hrdriver.F  
CCTM/src/gas/ebi_saprc07tc_ae6_aq/hrdriver.F  
CCTM/src/gas/ebi_saprc07tic_ae6i_aq/hrdriver.F  
CCTM/src/gas/ebi_saprc07tic_ae7i_aq/hrdriver.F  
CCTM/src/gas/ros3/rbdriver.F  
CCTM/src/gas/smvgear/grdriver.F  
UTIL/create_ebi/template_RXNSU_OPT/hrdriver.F  


## References

-----
## Internal Records:
#### Relevant Pull Requests:
917c471ca948403eda506167c946c46c8c47cebf  
dfb263f486669f402e5362d9a842b37ba8e3c905  
99dd2c2032a041ed22c2a417dcbcaab133012d0b  
c55b6871d4f149255d5a08413a3b590e746639b5  
92840461255da6bcf7a1769eace60f8dfa620ca5  
1c8271660d83432cdbfc2e50f0daacaa445f3d62        
241991ececfe4451b0fe746e5a7b23dbe0931cd1  



-----

