# Add ERF and SIGN to COMBINE's grid cell functions

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

## Brief Description

The changes add Sign and Gauss Error functions to available grid cell functions. 
Sign has one argument and is not identical to the FORTRAN SIGN function that has two 
arguments.  Changes to module_evaluate.F also attempt to accomplish the following:

1.   Allow the conditional `?` statement to function more like a FORTRAN `IF` statement so the below species definition does not cause combine to stop when evaluating `PH_TEST0`.

```
AH2OIJ, ug m-3, AH2OI[1]+AH2OJ[1]
AHPLUSIJ, nmol m-3, (AH3OPI[1]+AH3OPJ[1])/19.0*1000.0
HPMOLAL_TEST, mol L-1, AH2OIJ[0]<0.01 ? -999999 : AHPLUSIJ[0]/AH2OIJ[0]*1000.0
PH_TEST0, na, AH2OIJ[0]<0.01 ? -999999: -LOG10(HPMOLAL_TEST[0])
```

2.   Improve error messages by writing out the formula used in the species definition file instead of the processed string evaluated.

## Significance and Impact

New functions allows to calculate aerosol properties based on the modal distribution. They also improve error checking and messaging.

## Affected Files

* POST/combine/src/module_evaluator.F 
* POST/combine/src/combine.F 
* POST/combine/README.md 

# Internal Records
#### Relevant Pull Requests:
[PR #627](https://github.com/usepa/cmaq_dev/pull/627)

#### Commit IDs:  
d79037bc256492d1762a8d7232113c2e2ade8a5a   
c018fa7fb6f5e3060b941fa0d3241350740e706c

-----------------------
