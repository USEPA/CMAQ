# Species Definition File Organic Aerosol Update

**Author/P.O.C.:**, [Havala O. T. Pye](mailto:pye.havala@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
Species definition files (SpecDef*.txt, available in CCTM/src/MECHS/\*) provide guidance
in post-processing CMAQ species into quantities such as OC, PM2.5, and deposition fluxes.  The cb6r3_ae6 SpecDef descriptions 
of OC and OM needed to be corrected (AGLYJ was missing in v5.2.1). Species 
definition files were created for aero7.                    

## Significance and Impact
Allows for aero7 post-processing. Aero6 post-processed OC will slightly increase due to corrected
formula. v5.2.1 simulations should also correct their SpecDef files to 
include AGLYJ.
                       

## Affected Files
CCTM/src/MECHS/cb6r3_ae6_aq/SpecDef_Dep_cb6r3_ae6_aq.txt  
CCTM/src/MECHS/cb6r3_ae6_aq/SpecDef_cb6r3_ae6_aq.txt   
CCTM/src/MECHS/cb6r3_ae7_aq/SpecDef_Dep_cb6r3_ae7_aq.txt  
CCTM/src/MECHS/cb6r3_ae7_aq/SpecDef_cb6r3_ae7_aq.txt                  

## References
NA           

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #327]

#### Commit 
IDs:                        


-----

