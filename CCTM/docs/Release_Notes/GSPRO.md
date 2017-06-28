# Online speciation of biogenic emissions now available in the CMAQ repo

**Author/P.O.C.:**, [Jesse Bash](mailto:bash.jesse@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The Biogenic Emission Inventory System (BEIS) may be applied online in CMAQv5.2. This algorithm includes speciation of biogenic emissions into significant volatile organic compound species. The species produced are then mapped into CMAQ-available chemical mechanisms as determined by an input namelist called gspro, which is now distributed with the CMAQ repo. The file contains a header describing the various qquantities present inside. Briefly, each line is identified by a mechanism, followed by a mapping from a BEIS species to a surrogate species in that mechanism.


## Significance and Impact

Now that this file is distributed with the repo, users should have full access to modify the mapping of biogenic emissions into their chemical mechanism. For example, monoterpenes may be split into alpha-pinene, beta-pinene and limonene if the corresponding tracers are added to the model.

## Affected Files:

biog/beis3/gspro/gspro_biogenics_1mar2017.txt 

## References:

Not Applicable

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #197]

### Commit IDs:
cae9ef1  
03a596f  
4374727  
e1cc83f  
d7713ae  
