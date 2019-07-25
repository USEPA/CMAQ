# BEIS Default Chemical Mapping 

[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

## Brief Description
Two minor updates were made to BEIS input and diagnostic output. 
1) For input, a hard-coded lookup table was introduced that matches CMAQ chemical mechanisms to 
the mechanism field on the BEIS *gspro* file. Previsouly, users were responsible for selecting 
mechanisms on the *gspro* file to be compatible with the CMAQ chemical mechanism selected.
2) For diagnostic output, an error was found in the initialization of NTICS which caused BEIS 
diagnostic emissions to appear overestimated, since the running sum was accumulating too much
mass from output time step to output time step.

## Significance and Impact  
Protects users from incorrectly matching CMAQ chemical mechanisms to mechanism IDs on the *gspro*
input file. Resolves error in BEIS diagnostic output.

## Affected Files
CCTM/src/biog/beis3/tmpbeis.F  

CCTM/scripts/run_cctm.csh  

CCTM/src/emis/emis/BIOG_EMIS.F  

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #307](https://github.com/USEPA/CMAQ_Dev/pull/307)

[PR #308](https://github.com/USEPA/CMAQ_Dev/pull/308)

[PR #309](https://github.com/USEPA/CMAQ_Dev/pull/309)

[PR #318](https://github.com/USEPA/CMAQ_Dev/pull/318)

#### Commit 
IDs:                        
9edba294cf49a160b70c9b50aa8a63e8706ecd9b  
372fdaf89c1339bba318ef839fed91071e1b7081  
aa023b87c5b1d5fa77b3fa46a7976ca992543024  
7aa328e1838a8ad9c2daf812f95291818d1f8584  
372fdaf89c1339bba318ef839fed91071e1b7081  
aa023b87c5b1d5fa77b3fa46a7976ca992543024  
5b2b364d8c3f780801ccbac20dbe5cd7911387a0  
410ddb95fe334972e8144c515822e88d840ea8e4  
cbad6ca965d5c320070b2f701f155740d2f434ce  

-----

