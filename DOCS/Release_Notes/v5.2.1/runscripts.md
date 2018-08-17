# Update build and run scripts to include new science and improved workflow

**Author/P.O.C.:**, [Ben Murphy](mailto:murphy.benjamin@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The build and run scripts used in CMAQv5.1 have been updated to include the most recent science options (e.g. pcSOA, windblown dust, etc). Further, they have been modified to improved and streamline the installation and run procedure for CMAQ in general. For example, the linux 'date' command is now used to navigate between Julian and Gregorian days. The build scripts have been updated to improve the traceability of compiler options and computing environment conditions, like netcdf and ioapi version numbers.

All scripts now end with a *.csh extension to help identify them when browsing the code or extracting important files. The outck.q file has been removed from the CMAQ system and replaced with inline code directly in the CCTM run script. This modification should improve portability among compute environments.

Finally, the bldit_project.csh script has been added to enable users to extract all scripts from the repo and place them in an outside folder of their choice. The script modifies the new config_cmaq.csh file to point back to the repo of origin so there is no need for the user to link to the repo, manually. This functionality is expected to help users maintain project space independent of source code updates and new code submissions back to EPA.

## Significance and Impact

The impact is expected to be most important for new users learning the CMAQ workflow. It should also enable further developments in automated build/test procedures which will improve the robustness of CMAQ development.

## Affected Files:

config_cmaq.csh  
bldit_project.csh  
bldit_cctm.csh  
run_cctm.csh  
bldit_combine.csh  
run_combine.csh  

for other PREP and POST tools...  
  bldit_*.csh  
  run_*.csh  


## References:

Not Applicable

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #214]

### Commit IDs:  
d0ffe0a  
f090b49  
d397d29  
8c97cd3  
8e70277  
764de39  
fb4abef  
37b1305  
711da06  
2ad2063  
80810ec  
8e822eb  
b2da2dc  
d7c9858  
97c41b5  
