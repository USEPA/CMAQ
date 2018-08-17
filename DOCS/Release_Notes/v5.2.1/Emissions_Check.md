# Check for Errors in Mapping Emissions Species to CMAQ Surrogates

**Author/P.O.C.:**, [Ben Murphy](mailto:murphy.benjamin@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

This update implements a comprehensive error check by comparing all of the users surrogate emissions species (i.e. from the namelists and AERO_DATA table) to all of the species available from the emissions files and online modules. The code has been successfully compiled with intel, gcc, and pgi.

First a comprehensive and unique list is generated of all of the species used for input emissions to the program. Then each namelist species vector is compared to the master list and CMAQ aborts if any of those species are not found on the master list. The user may set the "CTM_EMISCHK" run-time option to FALSE to turn off the Abort command and just print a warning if namelist species are missing from the emissions. Doing so will results in runtime warnings every synchronization step when the inconsistency is detected.

## Significance and Impact

No impact on quantitative results. This update is specifically designed to help users detect and diagnose problems with the mapping of emissions to internal CMAQ species so that they dont, for example, accidentally ignore a species.

## Affected Files:

aero/aero6/AERO_EMIS.F  
emis/emis/EMIS_DEFN.F  
emis/emis/LTNG_DEFN.F  
emis/emis/MGEMIS.F  
emis/emis/STK_EMIS.F  
emis/emis/opemis.F  

## References:

Not Applicable

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #201]  

### Commit IDs: 
52593ec  
0eafc01  
793ebd3  
1af635c  
2690f98  
bd27175  
a041e70  
15733b8  
b8d6246  
