### Remove compiler error using table option of phot module 
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**: 5.5  

**Description**: The pull request removes the compiler errors encountered when building CMAQ or WRF-CMAQ with the table option for calculating photolysis frequencies. The compiler error occurs because Megan biogenic emissions uses a FORTRAN module, `PHOT_MET_DATA`, only available in the inline option for phot.  The code replaces the `PHOT_MET_DATA` with `PHOT_MOD` . Both table and inline option have a PHOT_MOD FORTRAN module. The code fix made minor changes to both versions of PHOT_MOD.F so Megan has access to the needed data.

**Significance and Impact**: The pull request restores a supported option for building CCTM in the CMAQ version 5.5.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1054](https://github.com/USEPA/CMAQ/commit/93df9b26e93c61e72b974589a27798468b997af7) | [PR#1054](https://github.com/USEPA/CMAQ_Dev/pull/1054)  | 
 

### Calculating of Photolysis Frequencies: New Method for Aerosol Optical Properties  
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Science Update, Documentation, New Feature  
**Release Version/Date**:  CMAQv5.5

**Description**: The update accomplish the below goals regarding the optical properties used in the the inline option for calculating photolysis frequencies.  

1. Add a new method for determining the optical properties. The new method should better match properties determined by solving Mie Scattering Theory for spherical particles than the default method (FastOptics) but produce comparable model runtimes.
2. Simplify how the model runtime options set how aerosol optical properties are calculated. The change combines the two options currently used in one option.
3. Change the contents in the photolysis diagnostic files one and three. The change intends to add a way evaluate the optical propertes against observations or theory.  
  
**Significance and Impact**: The inline module for calculating photolysis frequencies has a new option for aerosol optics properties for their effect on the frequencies. The option better matches solving Mie Scattering Theory for uniformly mixed spherical aerosols but has a lower computational cost. The module's diagnostics provide an new output for evaluate model results. The update does not affect model result because FastOptics remain the default method.  

**References**:    
Andrews, E., Ogren, J. A., Kinne, S., and Samset, B.: Comparison of AOD, AAOD and column single scattering albedo from AERONET retrievals and in situ profiling measurements, Atmos. Chem. Phys., 17, 6041–6072, https://doi.org/10.5194/acp-17-6041-2017, 2017.  

Fast, J. D., Gustafson Jr., W. I., Easter, R. C., Zaveri, R. A., Barnard, J. C., Chapman, E. G., Grell, G. A. and Peckham, S. E. Evolution of ozone, particulates, and aerosol direct radiative forcing in the vicinity of Houston using a fully coupled meteorology-chemistry-aerosol model, J. Geophys. Res., 111, D21305, [https://doi.org/10.1029/2005JD00672](https://doi.org/10.1029/2005JD006721), 2006.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1034](https://github.com/USEPA/CMAQ/commit/8dbab2dd301bbc148c707ed0b1659df2b3c6e850) | [PR#1034](https://github.com/USEPA/CMAQ_Dev/pull/1034)  |  

### Correct sub-grid cloud effect on in-line photolysis frequencies
[[William T. Hutzell](mailto:hutzell.bill@epa.gov)], U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  

**Description**:  The pull request addresses a code error in the inline option for calculating photolysis frequencies. The option includes effects from sub-grid or convective clouds predicted by the ACM cloud algorithm. A code error causes incorrectly calculating total and cloud optical depths for grid cells that have convective clouds because the layer thicknesses are set to zero for the portion of optical depths below the model top. The result overestimates photolysis frequencies where convective clouds are predicted. 

**Significance and Impact**: Over the hemispheric domain using the cb6r5m_ae7_aq mechanism, a simulation from June 21 to July 1, 2018 show daily ozone concentrations change plus or minus several ppb at the surface on July 1<sup>st</sup>. The large changes were increases over central Asia.   
Over the 12US1 domain, simulations covered the same period and used boundary conditions from the corrected hemispheric simulations. The code correction predominately decreased daily ozone at the surface on  July 1<sup>st</sup> by several. The large decreases occurred over the southwestern US. The updated boundary conditions appear the likely cause for largest ozone decreases because they did not occur when using uncorrected boundary conditions.    

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1045](https://github.com/USEPA/CMAQ/commit/7a31e6c4dd12a45b8aebcc941ab784ca83e6603a) | [PR#1045](https://github.com/USEPA/CMAQ_Dev/pull/1045)  |   