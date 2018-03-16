# Extract and place SpecDef files from the CMAQ repo to the project space  

**Author/P.O.C.:**, [Ben Murphy](mailto:murphy.benjamin@epa.gov), Computational Exposure Division, U.S. EPA  

## Brief Description

A few small but necessary changes to the SpecDef files involve correcting symbolic links in the combine subfolder, extracting specdef files for use with combine in the project space, and changing the location of log files.

## Significance and Impact

No impacts were observed results. These changes all fit within the category of model workflow.

## Affected Files:

CCTM/scripts/run_cctm.csh  
POST/combine/scripts/spec_def_files/SpecDef_Dep_cb05tump_ae6_aq.txt  
POST/combine/scripts/spec_def_files/SpecDef_Dep_saprc07tb_ae6_aq.txt  
POST/combine/scripts/spec_def_files/SpecDef_Dep_saprc07tb_ae6i_aq.txt  
POST/combine/scripts/spec_def_files/SpecDef_Dep_saprc07tic_ae6invPOA_aq.txt  
POST/combine/scripts/spec_def_files/SpecDef_Dep_saprc07tic_ae6invPOA_aq2.txt  
POST/combine/scripts/spec_def_files/SpecDef_cb05tump_ae6_aq.txt  
POST/combine/scripts/spec_def_files/SpecDef_saprc07tb_ae6_aq.txt  
POST/combine/scripts/spec_def_files/SpecDef_saprc07tb_ae6i_aq.txt  
POST/combine/scripts/spec_def_files/linkem  
bldit_project.csh  


-----
## Internal Records:


### Relevant Pull Requests:
  [PR #240](https://github.com/USEPA/CMAQ_Dev/pull/240)  
  [PR #241](https://github.com/USEPA/CMAQ_Dev/pull/241)  

### Commit IDs:

ee34b3e5ca4f344ed80171c7afd5e765199a8a57    
b4bef724d35ebd2a5307812384f5ce28174be970    
3b7a2cfe9c834f31b0149cb9d3cf037fccf2a197  

