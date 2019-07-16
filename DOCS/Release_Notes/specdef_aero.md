# Updates to the Species Definition Files for Organic Aerosols

[Havala O. T. Pye](mailto:pye.havala@epa.gov), U.S. Environmental Protection Agency

## Brief Description
Species definition files (SpecDef*.txt, available in CCTM/src/MECHS/\*) provide guidance
for post-processing CMAQ species into quantities such as OC, PM<sub>2.5</sub>, and deposition fluxes.  The *cb6r3_ae6* SpecDef descriptions 
of OC and OM were corrected (AGLYJ was missing in v5.2.1). Species 
definition files were created for *aero7*.                    

## Significance and Impact
Allows for *aero7* post-processing. *Aero6* post-processed OC will slightly increase due to corrected
formula. CMAQv5.2.1 simulations should also correct their SpecDef files to 
include AGLYJ.
                       

## Affected Files
CCTM/src/MECHS/cb6r3_ae6_aq/SpecDef_Dep_cb6r3_ae6_aq.txt  
CCTM/src/MECHS/cb6r3_ae6_aq/SpecDef_cb6r3_ae6_aq.txt   
CCTM/src/MECHS/cb6r3_ae7_aq/SpecDef_Dep_cb6r3_ae7_aq.txt  
CCTM/src/MECHS/cb6r3_ae7_aq/SpecDef_cb6r3_ae7_aq.txt                  


-----
## Internal Records:
#### Relevant Pull Requests:
[PR #327](https://github.com/USEPA/CMAQ_Dev/pull/327)

#### Commit 
IDs:                        


-----

