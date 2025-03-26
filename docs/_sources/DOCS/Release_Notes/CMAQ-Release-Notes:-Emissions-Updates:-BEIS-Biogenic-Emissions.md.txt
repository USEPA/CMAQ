### BEIS Updates
[Jesse Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update and Bug Fix  
**Release Version/Date**: CMAQv5.4  
**Description**:  

Science updates:
1. BEIS in CMAQ is streamlined by removing the BIOSEASONS file and the BIOSW_YN and SUMMER_YN options.
 > Seasonality is now modeled using the 1 meter soil temperature following the WRF PX LSM implementation and was tested with WRF output using both NOAH and PX  LSM simulations. 
2. Canopy temperature and radiation environments are now modeled using the driving meteorological model’s representation of LAI rather than the LAI values in the B3GRD file.

Bug fixes: 
1. Solar radiation attenuation in the shaded portion of the canopy was using the direct beam photosynthetically active radiation (PAR) when the diffuse beam PAR attenuation coefficient should have been used. 
 > This update had little impact on the total emissions but did result in slightly higher emissions in the morning and evening transition periods for isoprene, methanol and MBO.
2. The fraction of solar radiation in the sunlit and shaded canopy layers, SOLSUN and SOLSHADE respectively were estimated using a planar surface. These should have been estimated based on the par intercepted by a hemispheric surface rather than a plane. 
 > This update can result in an earlier peak in leaf temperature, approximately up to an hour.
3. The quantum yield for isoprene emissions (ALPHA) was updated to the mean value in Niinemets et al. 2010a ( https://doi.org/10.1029/2010JG001436) and the integration coefficient (CL) was updated to yield 1 when PAR = 1000 following Niinemts et al 2010b ( https://doi.org/10.5194/bg-7-1809-2010).
 > This updated resulted in a slight reduction in isoprene, methanol, and MBO emissions.

BELD 6 updates:
1. Utilizes high resolution tree species and biomass data from Wilson et al. 2013a, and Wilson et al. 2013b. 
 > Scaled to mean grid cell leaf biomass using the allometric relationships in the U.S. Forest Service  Forest Inventory and Analysis (FIA) database allometric scaling (Jenkins et al. 2003)
2. Species names were changed from non-specific common names to scientific names
3. Tree species biogenic volatile organic carbon (BVOC) emission factors for tree species where taken from the NCAR Enclosure database similar to MEGAN 3
4. Switch from area based to the leaf biomass based emission factors to utilize the new high resolution data from the US Forest Service and to utilize units more commonly reported in the literature. 

**Significance and Impact**:   

The seasonality updates require one less input file and two fewer environment variables. Leaf off and leaf on conditions are now represented as a gradient rather than a binary variable and are in better agreement with long term means from the U.S.A. National Phenological Network observations (USANPN). This change did not have a large impact on BVOC emissions due to the temperature dependence on emissions. 
  
![image](https://user-images.githubusercontent.com/12100276/165358483-8df7e5fd-0241-46f5-963a-bae38bf28e16.png)
Comparison USANPN observations leaf onset observations for April 15th (top), the BEIS4 parameterization adopted from the WRF PX land surface scheme (bottom left), and the BEIS3 bioseasons file representation (bottom right)
 
The correction to the canopy light model resulted in similar emissions as BEIS3 but in much higher simulated CMAQ  isoprene, methanol, and MBO concentrations due to an increase in emissions during the morning and evening transition where the modeled boundary layer height is relatively low. This resulted in approximately a 30% increase in estimated isoprene concentrations in July. The updates to BELD increased isoprene emissions in the Southeast by  approximately another 20% and monoterpene emissions decreased by approximately 20% due to changes in emission factors and differences between the biomass estimates of Wilson et al. 2013 used in BELD 6 and the domain wide assumptions used in BELD 5.
 
![image](https://user-images.githubusercontent.com/12100276/165358526-0f2b989e-3ef2-441e-a32f-c42163bedcb8.png)
Daily AQS isoprene observations (grey) and July 2016 results from this merge with BELD6 emission factors (red), prior to this merge with BELD6 emission factors (blue), and the research branch with BELD5 emission factors (green)

**References**: 
Jenkins, J.C., Chajnocky, D.C., Heath, L.S., Birdsey, R.A., National-scale biomass estimators for United States Tree Species, Forest Science, 49(1), 12-35, https://doi.org/10.1093/forestscience/49.1.12, 2003
 
Niinemets, U., Copolovici, L., Huve, K., High within-canopy variations in isoprene emission potentials in temperate trees: Implications for predicting canopy-scale isoprene fluxes, J. Geophys. Res.  Biogeosci. G04029, ,https://doi.org/10.1029/2010JG001436, 2010a
 
Niinemets, U., Monson, R.K., Arneth, A.,  Ciccioli, P., Kesselmeier, J., Kuhn, U., Noe, S.M., Penuelas, J., Staudt, M., The leaf-level emission factor of volatile isoprenoids: caveats, model algorithms, response, shapes and scaling, Biogeosciences, 7, 1809-1832, https://doi.org/10.5194/bg-7-1809-2010, 2010b

Wiedinmyer, C., 2001. NCAR BVOC Enclosure Database. National Center for Atmospheric Research, Boulder, CO
 
Wilson, Barry Tyler; Lister, Andrew J.; Riemann, Rachel I.; Griffith, Douglas M. 2013a. Live tree species basal area of the contiguous United States (2000-2009). Newtown Square, PA: USDA Forest Service, Rocky Mountain Research Station. https://doi.org/10.2737/RDS-2013-0013
 
Wilson, Barry Tyler; Woodall, Christopher W.; Griffith, Douglas M. 2013b. Forest carbon stocks of the contiguous United States (2000-2009). Newtown Square, PA: U.S. Department of Agriculture, Forest Service, Northern Research Station. https://doi.org/10.2737/RDS-2013-0004

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#868](https://github.com/USEPA/CMAQ/commit/e5af32542cf7f60e16620857d1a20f85c0049528) | [PR#868](https://github.com/USEPA/CMAQ_Dev/pull/868)  | 
