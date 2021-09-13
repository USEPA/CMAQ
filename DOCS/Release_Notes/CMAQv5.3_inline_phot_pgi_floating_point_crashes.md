# Removing sporadic floating point crashes from photolysis rate calculation with the PGI Fortran compiler.
 
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

## Brief Description
The changes remove sporadic crashes when CMAQ is compiled using the PGI compiler with debug options. Floating point errors in the inline calculation of photolysis rates occur when exponentials are evaluated at very large negative REAL(8) numbers for the **TWOSTREAM_S** and **get_aggregate_optics** subroutines in the **PHOT_MOD.F** and **CLOUD_OPTICS.F** files, respectively. These code changes limit the lowest value of the exponential argument to &#8209;709.090848126508, which corresponds to 9.0&nbsp;x&nbsp;10<sup>&#8209;307</sup>. Exponentials that are evaluated below the limit are set to 9.0&nbsp;x&nbsp;10<sup>&#8209;307</sup>.

## Significance and Impact
Simulations over the 12-km CMAQ Southeast benchmark and CONUS domains show no differences when the model is compiled the Intel version 17.0 and gcc version 6.1 Fortran compilers. When using the PGI version 17.4 Fortran compiler, concentration differences are much less than 0.1% for most species. Monoatomic chlorine, hypochlorous acid, and formyl chloride had differences on the order of 10% for concentrations below 10<sup>&#8209;5</sup> to 10<sup>&#8209;8</sup> ppmV in isolated locations over the Gulf of Mexico and the Florida peninsula.

## Files Affected

* CCTM/src/phot/inline/PHOT_MOD.F
* CCTM/src/phot/inline/CLOUD_OPTICS.F
