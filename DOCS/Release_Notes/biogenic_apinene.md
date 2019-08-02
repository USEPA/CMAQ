# Biogenic Speciation Update for *AERO7*

[Havala O. T. Pye](mailto:pye.havala@epa.gov), U.S. Environmental Protection Agency

## Brief Description
A new biogenic speciation profile was created to separate APIN from TERP
in cb6r3-aero7 mechanisms. This required allowing profile names with more than
5 characters. Up to 16 characters are now allowed. The *gspro_biogenics_1mar2017.txt* 
was also renamed more generically to *gspro_biogenic.txt*. Current profile name options:

 B10C5: CB05-based mechanisms  
 B10C6: CB6r3-ae6-based mechanisms  
 B10C6AE7: CB6r3-ae7-based mechanisms  
 B10RD: RACM2-based mechanisms  
 B10SP: SAPRC07t-based mechanisms (aero6 and aero7 compatible)  

Default mapping (using above information) is available in CCTM/src/emis/emis/BIOG_EMIS.F.
The biogenic profile name need not be set in the run script.

## Significance and Impact
Allows for proper aero7 speciation of inline biogenics in which APIN is separate
and not duplicated in TERP.

## Affected Files  
CCTM/src/biog/beis3/gspro_biogenic.txt (contains all profiles)  
other biogenic emission files

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #310](https://github.com/USEPA/CMAQ_Dev/pull/310)

[PR #307](https://github.com/USEPA/CMAQ_Dev/pull/307)

#### Commit
IDs:


-----

