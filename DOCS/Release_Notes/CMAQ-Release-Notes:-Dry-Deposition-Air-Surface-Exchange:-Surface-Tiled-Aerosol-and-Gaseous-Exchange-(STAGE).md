### Correction in an Underflow STAGE Emerson Aerosol Dry Deposition Option
[Jesse Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQ 5.5  
**Description**:  This pull request resolves an underflow issue in calculating the bounce correction term (R1) in the [Emerson et al. 2020](https://www.pnas.org/doi/10.1073/pnas.2014761117) (equation 2) aerosol dry deposition option in STAGE.   

**Significance and Impact**: This allows for CMAQ simulations using the STAGE Emersion aerosol dry deposition option when compiled with debug flags. This correction makes relatively small changes in the aerosol dry deposition velocity when the gravitation settling velocity is large. This can result in small changes in the aerosol deposition velocity but has no impact on model evaluations to the precision that is reported in the AMET tool.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1102](https://github.com/USEPA/CMAQ/commit/e54d26342569a1811195c4bb4e54579ae0192927) | [PR#1102](https://github.com/USEPA/CMAQ_Dev/pull/1102)  |


### STAGE Minor Bugfix
[Jesse Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQ 5.5  
**Description**:  
 This pull request address three issues in the STAGE deposition option.   
1. The units for Molar Vol in CMAQ_Control_STAGE.nml are incorrect and should be cm**3 mol-1
2. The diffusive volume in STAGE_MOD.F should be 20.1 cm**3 mol-1 rather than 22.4 following Fuller et al. 1966 and the [EPA Onsite toolbox](https://www3.epa.gov/ceampubl/learn2model/part-two/onsite/ed-background.html).
3. Added the attenuation of u<sub>*</sub> due to canopy elements to the deposition of aerosols to vegetated covered smooth surfaces.  

**Significance and Impact**: This pull request corrects a units typo in the  CMAQ_Control_STAGE.nml and the FSG diffusive volume for air in the diffusivity calculation. This results in a minor reduction in model estimated O<sub>3</sub> concentrations with the largest reductions (less than 1 ppb) occurring during the summer over forested land cover where the model typically overestimates AQS observed O<sub>3</sub>. The model differences are not likely to impact the general model evaluation.   

**References**:  [EPA Onsite toolbox](https://www3.epa.gov/ceampubl/learn2model/part-two/onsite/ed-background.html)  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1070](https://github.com/USEPA/CMAQ/commit/372037a5e32bca159c57fab7703de99c4530f7e3) | [PR#1070](https://github.com/USEPA/CMAQ_Dev/pull/1070)  | 


### Land Use and Deposition Species Mapping
[Jesse Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix, New Feature  
**Release Version/Date**: CMAQ 5.4  
**Description**:  
This update to STAGE has three primary parts: 
1. introduces the mapping of land use data from the meteorological model to an internal land use category similar as with the AQMEII 4 project. This mapping is defined in the CMAQ_Control_STAGE.nml and is fully customizable allowing the user to set the number and parameters for the land use types and define the meteorological models land use data. These parameters now include the NH3 and Hg compensation points for bidirectional options. This also provides the logic when mapping from land use specific deposition to high resolution land use data, e.g. MODIS.
2. Land use specific conductances are now normalized to the meteorological model’s grid average value. This results in little change when using WRF 3.8.1 with the PX land surface scheme but does change results with other versions of WRF and land surface schemes. When using WRF 4.1.2 using the PX land surface scheme in the EQUATES project, this resulted in a reduction in Maximum 8 hour O3 concentrations over the Eastern US by 1-3 ppb in July and little change in January. This resulted in an improvement in model performance. 
3. The mapping of dry deposition species to vertical diffusion species has been completely revised. This mapping is now done in the initialization DEPV_DEFN.F rather than at each sync step. This mapping is now editable without the need to recompile using the CMAQ_Control_STAGE namelist allowing users to add deposition processes to any modeled species. The mapping between vdiff and dep is stored in a derived data type and is also used to control the output of the optional grid and tiled deposition velocity files. 
4. This pull request corrected an error in the bidirectional NH3 exchange parameterization of the soil ammonium in solution for MODIS category 14 that was present when soil moisture in the first soil fell below the specified wilting point that resulted in excessively high NH3 emissions. This was only present when running using MODIS land use over the contiguous U.S. with bidirectional NH3 exchange turned on. 

**Significance and Impact**: Overall, these result in a reduction in model run time, particularly when the MOSAIC option is set, and improved model O3 performance and reduced NH3 error when compared against AMoN observations when using meteorology with MODIS land use. There is little impact on other model species. Land use and species-specific deposition parameters have been aggregated allowing for easier maintenance, improved transparency, and gives the user much more control over the governing deposition processes.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#847](https://github.com/USEPA/CMAQ/commit/16f959108268dd2a55e2271a26d5d89a9ec54914) | [PR#847](https://github.com/USEPA/CMAQ_Dev/pull/847)  | 

### Updates to Gaseous and Aerosol Dry Deposition
[Jesse Bash](mailto:bash.jesse@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug fix, New feature  
**Release Version/Date**: CMAQ 5.4  
**Description**:  

The STAGE option in CMAQ v5.5 now allows the user to specify key land use parameters for gaseous and aerosol dry deposition process using the CMAQ_Control_STAGE.nml name list. Default values have been populated using median observations from the TRY Plant Trait Database. These updates reduce the bias in summertime NH3 concentrations by approximately half.

Two aerosol deposition options have been added that better capture the observed relationship between the observed aerosol dry deposition velocity and particle diameter. The Emerson et al. 2020 option has been made the default in STAGE and results in PM2.5 concentrations similar to the STAGE CMAQ v5.3 scheme while the Pleim et al. 2022 option results in a higher rates of aerosol deposition in the Accumulation mode and results in lower ambient PM2.5 concentrations.  

The aerosol deposition options can be selected in the run script by setting the following environmental variables to Y:

`setenv CTM_STAGE_P22 N       #> Pleim et al. 2022 Aerosol deposition model [default: N]`

`setenv CTM_STAGE_E20 Y       #> Emerson et al. 2020 Aerosol deposition model [default: Y]`

`setenv CTM_STAGE_S22 N       #> CMAQ v5.3 Aerosol deposition model [default: N]`


This update to STAGE has 5 primary parts:
1.	The formulation for Calc_Rb_leaf in STAGE_FUNCTIONS.F has been revised to more closely follow the original derivation in Jensen and Hummelshoj 1995/1997 doi:10.1016/0168-1923(94)05083-I from the implementation in [Massad et al 2010](https://doi.org/10.5194/acp-10-10359-2010). This change was made due to typographical errors found Massad et al. 2010 and it was unclear if the remaining differences between Massad et al. 2010 and its cited references, Jensen and Hummelshoj 1995/1997, where intentional or in error.
2.	Leaf litter emission potentials were added for under canopy surfaces and stomatal NH3 emission potentials were updated based on AMoN site survey data extrapolated to the STAGE land use categories based [TRY Plant Trait Database](https://www.try-db.org/TryWeb/Home.php) leaf and soil litter nitrogen observations.
3.	The soil water-solid partitioning scheme was updated based on recent measurements conducted at NCSU.
4.	Land use specific parameters for aerosol deposition processes have now been moved to CMAQ_Control_STAGE.nml and the aerodynamic leaf width parameter has been updated based on TRY Plant Trait Database observations, assuming an oval leaf following [Campbell and Norman 1998](https://link.springer.com/book/10.1007/978-1-4612-1626-1).
5.	The CSU [Emerson et al. 2020](https://doi.org/10.1073/pnas.2014761117) and Pleim et al. 2022 aerosol deposition options are now included as a runtime option. The CSU model has been modified to smoothly scale from leaf off to leaf on conditions and vegetated to non-vegetated conditions for the CMAQ application. This includes the incorporation of a two layer aerosol deposition model and replacing the product of the empirical factor 3 and u* with the integration of LAI(z) u*(z) from 0 to the canopy top using the in-canopy attenuation coefficient of [Yi 2008](https://doi.org/10.1175/2007JAMC1667.1). This matches the empirical factor of 3 at an LAI of approximately 5.3. When LAI = 0 this returns the deposition velocity for non-vegetated surfaces. Despite these changes, CMAQ model results, with an aerosol standard deviation of 1, are similar to the figures presented in Emerson et al. 2020.

![image](https://user-images.githubusercontent.com/12100276/165372400-d045f7bd-c0c8-4780-9ce9-7c542cc2e4bb.png)
Deposition velocity as a function of particle diameter for the CSU model, M3Dry [PR#842](https://github.com/USEPA/CMAQ_Dev/pull/842), STAGE v5.3, and the STAGE-CSU modal parameterization [PR#883](https://github.com/USEPA/CMAQ_Dev/pull/883).

![image](https://user-images.githubusercontent.com/12100276/165372939-ccfa2c55-8a45-4604-9a62-31e0887baa5e.png)
Deposition pathways of the STAGE-CSU implementation.

**Significance and Impact**:
1.	This pull request revises the formula for Rb_leaf which results in lower resistances to vegetation with LAI > 4.6 and higher resistance to deposition otherwise. This primarily impacts HNO3 and other species with low/no canopy resistance. Model differences are small and typically less than 1 ppb.
2.	Changes 2 and 3 result in about a 15% increase in summertime NH3 concentrations over the CONUS domain with decreases in heavily agricultural areas. This results in a reduction in the model bias and error.

![image](https://user-images.githubusercontent.com/12100276/165373141-bac10392-8101-43fd-b322-3ebd566ef8d7.png)
July 2016 Evaluation against AMoN concentrations, AMoN observations (grey), the STAGE v5.4 update [PR#883](https://github.com/USEPA/CMAQ_Dev/pull/883) (red), and [PR#842](https://github.com/USEPA/CMAQ_Dev/pull/842) (blue)

3.	The Pleim et al. 2022 aerosol deposition options results are similar to [PR#842](https://github.com/USEPA/CMAQ_Dev/pull/842) while the Emerson et al. 2020 option results in a smaller reduction than Pleim et al. 2022 in ambient aerosol concentrations while still capturing the observed minimum in the aerosol deposition velocity. The Pleim et al. 2022 option was evaluated against a CONUS July 2016 simulation of a current build of M3Dry and the modeled PM was within 5% of each other. The differences in PM appear to be related to gaseous aerosol precursors due to differences in the gaseous deposition and BVOC emissions, the STAGE case used BEIS4/BELD6.

![image](https://user-images.githubusercontent.com/12100276/165373756-a34c24ab-4615-48ff-bc4d-fd82ef69ca64.png)
Monthly mean PM2.5 from M3Dry [PR#842](https://github.com/USEPA/CMAQ_Dev/pull/842) (left) and STAGE using the Pleim et al. 2022/M3Dry scheme (right).

![image](https://user-images.githubusercontent.com/12100276/165373889-1d5e8c98-f79a-4ba4-822b-1cd587e65aea.png)
Time series of PM2.5 from M3Dry [PR#842](https://github.com/USEPA/CMAQ_Dev/pull/842), black, and STAGE using the M3Dry aerosol deposition velocity, red, (left) and a scatter plot between the two parameterizations (right).

![image](https://user-images.githubusercontent.com/12100276/165374017-1e3a77be-6bf6-4b08-a58a-89252f4672b3.png)
Monthly mean PM2.5 from STAGE using the Emerson et al. 2020/CSU scheme (left) and STAGE using the Pleim et al. 2022/M3Dry scheme (right).

![image](https://user-images.githubusercontent.com/12100276/165374076-5c95bb86-a795-4ec9-9ad1-333c9f703050.png)
Time series of PM2.5 from STAGE using the CSU aerosol deposition velocity, black, and STAGE using the M3Dry aerosol deposition parameterization, red, (left) and a scatter plot between the two parameterizations (right).

![image](https://user-images.githubusercontent.com/12100276/165374388-27999a12-981f-4b15-b7f0-557a6471b691.png)
July 2016 stacked barplots. From left to right, AQS daily observations, STAGE with CSU aerosol deposition option, STAGE with M3Dry aerosol deposition option, STAGE with v5.3 aerosol deposition option

**References**: 
Campbell and Norman, An introduction to Environmental Biophysics, Springer New York, NY, https://doi.org/10.1007/978-1-4612-1626-1: 1998  

Emerson, E.W., Hodshire, A.L., DeBolt, H.M., Farmer, D.K., Revisiting particle dry deposition and its role in radiative effect estimates. Proceedings of the National Academy of Sciences, 117(42), 26076-26082, https://doi.org/10.1073/pnas.2014761117: 2020 

Jensen, N.O., Hummelshoj, P., Derivation of canopy resistance for water vapour fluxes over a spruce forest using a new technique for the viscous sublayer resistance, Agricultural and Forest Meteorology, 73 (3-4), 339-352, https://doi.org/10.1016/0168-1923(94)05083-I, 1995

Kattge, J., et. al.: TRY plant trait database – enhanced coverage and open access. Global Change Biology 26, 119 – 188,  https://doi.org/10.1111/gcb.14904: 2020

Massad, R.-S., Nemitz, E., Sutton. M.A., Review and parameterization of bi-directional ammonia exchange between vegetation and the atmosphere, Atmos. Chem. Phys., 10., 10359-10386, https://doi.org/10.5194/acp-10-10359-2010, 2010

Yi, X., Momentum Transfer within Canopies, J. Applied Meteorology and Climatology,47(1), 262-275, https://doi.org/10.1175/2007JAMC1667.1: 2008

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#883](https://github.com/USEPA/CMAQ/commit/bdcb56e60ca03248e4782fc296b855e6fdefd7c8) | [PR#883](https://github.com/USEPA/CMAQ_Dev/pull/883)  |  

