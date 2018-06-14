# Add Capacity for Multiple Fire Input Files

**Author/P.O.C.:**, [Matthew Woody](mailto:woody.matthew@epa.gov), U.S. EPA

## Brief Description

Multiple fire point source emission files may now be read in to CMAQ rather than just one file as was the case in CMAQv5.1.


## Significance and Impact

The code can now work with several fire sources at once. Instrumented versions of CMAQ like ISAM will be able to take advantage of this capability to assess multiple fire sources simultaneously.

## Affected Files:

emis/emis/PT3D_DEFN.F  
emis/emis/STK_EMIS.F  
emis/emis/STK_PRMS.F  

## References:

Not Applicable

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #71]

### Commit IDs:
9e71bc8  
890f498  
