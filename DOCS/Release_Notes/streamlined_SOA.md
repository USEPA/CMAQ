# Streamlined SOA Module

[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency


## Brief Description
Several updates were introduced to the SOA mapping and partitioning algorithm to improve code clarity and increase robustness as greater complexity in the organic aerosol phase is accomodated. The new unified organic compound properties table centralizes the vapor pressure, enthalpy of vaporization, SOA yields and gas-phase counterpart so that this information can be easily referenced by novice and advanced users. The partitioning algorithm has also been generalized so that any number and combination of semivolatile and nonvolatile species can be supported in the same model run with no changes to the model infrastructure.

## Significance and Impact
There is no effect of this update on results and only minor effects on runtime. The update is expected to have a positive impact on user experience since all organic species and their properties are located in one centralized data table, making the code far more straightforward.

## Affected Files
CCTM/src/aero/aero6/AERO_DATA.F  
CCTM/src/aero/aero6/PRECURSOR_DATA.F  
CCTM/src/aero/aero6/SOA_DEFN.F  
CCTM/src/aero/aero6/aero_driver.F  
CCTM/src/aero/aero6/aero_subs.F  
CCTM/src/emis/emis/DUST_EMIS.F  
CCTM/src/emis/emis/SSEMIS.F  

## References

-----
## Internal Records:
#### Relevant Pull Requests:
d547345fa5ed17b378a1ba464dcdeb13796b04aa  
bec5fa35d40e782a6c1731e78b7787482d349d9b  
aeedc23e4f61397c7fd6111c917f1899b0d5943d  
5f4bb76429b06a76002809f34811ca3d39c69952  

-----

