# Remove floating point crashes from photolysis rate calculation when using Portland Group compiler.
 
**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
The changes remove crashes when the model is compiled using the pgi compiler with debug options. Floating point errors within the in-line calculation of photolysis rates cause the crashes because exponentials are being evaluated at very large negative REAL(8) numbers for the **TWOSTREAM_S** and **get_aggregate_optics** subroutines within the **PHOT_MOD.F** and **CLOUD_OPTICS.F** files, respectively. Code changes limit lowest value of the exponential argument to -709.090848126508 which corresponds to 9.0x10<sup>-307</sup> so exponentials evaluated below the limit are set to 9.0x10<sup>-307</sup> .

## Significance and Impact
Simulations over current CMAQ benchmark and CONUS domains show no differences when the model is compiled the intel version 17.0 and gcc version 6.1 compilers. When using the pgi version 17.4 compiler, concentration differences are much less then 0.1% from most species. Monoatomic chlorine, hypochlorous acid, and formyl chloride had isolated differences on the order of 10% for concentrations below 10<sup>-5</sup> to 10<sup>-8</sup> ppmV. Their locations were over the Gulf of Mexico and the Florida pennisula

## Files Affected

* CCTM/src/phot/inline/PHOT_MOD.F
* CCTM/src/phot/inline/CLOUD_OPTICS.F
