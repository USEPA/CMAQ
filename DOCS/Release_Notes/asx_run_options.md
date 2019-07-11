# Air-Surface Exchange Option Updates

[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

## Brief Description
Three minor updates were made to the options used by CMAQ to drive air-surface exchange processes.  
1) The option for diagnostic output of dry deposition velocities was added for CB6 species 
IOLE, APIN, EPOX, and NAPH.  
2) Specification of MEDIACONC output from the previous day was corrected in the default run scripts.  
3) The calculation of land-water fraction was updated to correctly consider grid cells that are entirely 
snow/ice. 

## Significance and Impact  
1) Dry deposition velocties are now output for these species.  
2) CMAQ will correctly initialize the model with the soil properties from the previous midnight rather 
than starting from clean conditions.
3) Grid cell areas covered in snow/ice will no longer count as water, thereby avoiding instabilities 
in the algorithm that requires normalizing by the land area fraction.

## Affected Files
CCTM/src/MECHS/cb6r3_ae7_aq/GC_cb6r3_ae7_aq.nml  
CCTM/scripts/run_cctm.csh  
CCTM/scripts/run_cctm_2014_12US1.csh  
CCTM/scripts/run_cctm_2015_HEMI.csh  
CCTM/src/depv/m3dry/ABFLUX_MOD.F  
CCTM/src/depv/m3dry/m3dry.F  

## References
NA           

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #348](https://github.com/USEPA/CMAQ_Dev/pull/348)

[PR #379](https://github.com/USEPA/CMAQ_Dev/pull/379) 

[PR #380](https://github.com/USEPA/CMAQ_Dev/pull/380)

#### Commit 
IDs:                        
c8d5996e1ac4b7c8ceab2f2f8cdc2a3fc6baffd6  
9efc55f5a57c9e29a721466f99bc8d502e136394  
0688055c68726ed2863aaaff9e5999f43204b9c3  
421964c760e33744f950cdf42c56fdc0b79cd969  
-----

