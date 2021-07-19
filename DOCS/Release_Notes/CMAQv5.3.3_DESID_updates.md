# Updates to the Detailed Emission Scaling Isolation and Diagnostic (DESID) Module

[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

## Brief Description

Three minor updates are provided for the DESID module operation. 
1) The first update is revises the algorithm for assigning the base scaling instructions in the subroutine EMISS_SCALING in EMIS_DEFN.F. Previosuly, the algorithm dependent on allocating and loading a temporary array (MAP_FACtoISTR) mapping the individual factors to instruction indices for each stream. This approach was identified as potentially being vulnerable to memory leaks and so a more direct algorithm which loops through each instruction index explicitly was used. This update is protective and was not necessarily causing an issue in CMAQv5.3.2. 

2) The second update resolves a bug causing the model to crash if no emissions were selected for DESID. Because the VDEMIS_DIFF array was intialized outside of the check for emission stream numbers, a segmentation fault would occur if no emissions were selected and the array were not allocated. This initalization has now been preceeded by a check for N_EM_SRM > 0. This is for both initialization and assignment of emissions in vdiffacmx.F.    

3) The third and most extensive update concerns warning users if DESID finds unexpected variable units on emission stream offline files. Previously if any variable were on an emission file and it did not have allowed emissions units, then CMAQ would crash. This has been resolved by printing a warning when unrecognized or incompatible units are present. In most cases the variable will probably not be used for emissions by DESID anyway. In some cases where a user wants to use a variable with unrecognized units, they can do so using the 'UNIT' descriptor in the BASIS field of the emission control interface rules.

## Significance and Impact

These updates do not affect CMAQ predictions. They avoid model crashes and improve feedback to users in case there are issue with input consistency.

## Affected Files

CCTM/src/emis/emis/EMIS_DEFN.F
CCTM/src/vdiff/acm2_m3dry/vdiffacmx.F
CCTM/src/vdiff/acm2_stage/vdiffacmx.F


## References

N/A

-----
## Internal Records

#### Relevant Pull Requests:

PR696  
PR695  
PR694   
PR748   

#### Commit IDs:

0860c85ab1ad6e46fbc5d185477013c1fdadd2a8    
8ec300bc1d64322d4c0ed91c5d6fae41df1e8b28   
20d00f6bd3885b34049b2d2ffe571851b8b8e9c9   
b2666e8843e9eeca25e66477ad6e9c4c5c1911da  
3719dfdd64b51cfaaa617539222eccfff0ed93b8   


-----
