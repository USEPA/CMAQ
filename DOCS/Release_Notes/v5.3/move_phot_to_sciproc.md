# Move Photolysis Rate Calculation from CHEM to SCPROC

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
The pull request moves PHOT for the CHEM driver to sciproc for both in-line and tabular method for calculating photolysis rates. It also adds variables to the cloud options that use the H<sub>2</sub>O<sub>2</sub> and HNO<sub>3</sub> photolysis rates calculated from PHOT. Finally, the create_ebi utility was updated to account for this structural change.

## Significance and Impact
The change allows cloud and aerosol modules to use results from the photolysis calculation, more easily determining computational expense of photolysis module, improved profiling CCTM, and simpler debugging CCTM when the photolysis module is causing runtime crashes.      

Impact on Results: None for the tabular method if using Intel compiler. For inline method, tests showed maximum difference between 1-2 ppbV and 0.1-0.4 ugm<sup>3</sup> for ozone and fine mode sulfate over the 12X12 km<sup>2</sup> US continental domain. Maximum sulfate difference occurred China and India over the northern hemispheric domain. The change is expected because PHOT's new location set to the gas and aerosol concentration for values prior to calling CLDPROC.

# Files Affected
*  phot.F, PHOT_MOD.F and PHOT_MET_DATA.F under CCTM/src/phot/inline and CCTM/src/phot/table
*  all module options under CCTM/src/cloud
*  all module options under CCTM/src/gas
