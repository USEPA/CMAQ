# Move Photolysis Rate Calculation to SCIPROC

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

## Brief Description
PHOT has been moved to SCIPROC for both the inline and the tabular method of calculating photolysis rates. It also adds variables to the cloud options that use the H<sub>2</sub>O<sub>2</sub> and HNO<sub>3</sub> photolysis rates calculated from PHOT. Finally, the *create_ebi* utility was updated to account for this structural change.

## Significance and Impact
The change allows cloud and aerosol modules to use results from the photolysis calculation, facilitates diagnosing the computational expense of photolysis module, improves profiling of CCTM, and simplifies debugging of CCTM when the photolysis module is causing run-time crashes.      

**Impact on Results:** None for the tabular method if using the Intel compiler. For the inline method, tests showed maximum differences of between 1-2 ppbV and 0.1&#8209;0.4 &#956;g&nbsp;m<sup>&#8209;3</sup> for ozone and fine mode sulfate over the 12-km continental U.S. domain. The maximum difference in sulfate occurred over the Northern Hemisphere domain occurred in China and India. The change is expected because the new location of PHOT within CCTM sets the gas and aerosol concentration for values prior to calling CLDPROC.

# Files Affected
*  phot.F, PHOT_MOD.F and PHOT_MET_DATA.F under CCTM/src/phot/inline and CCTM/src/phot/table
*  all module options under CCTM/src/cloud
*  all module options under CCTM/src/gas
