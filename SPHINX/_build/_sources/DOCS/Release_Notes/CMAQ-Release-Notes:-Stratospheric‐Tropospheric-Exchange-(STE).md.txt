# Stratospheric-Tropospheric Exchange

## Potential Vorticity Scaling of Ozone (PVO3) a Runtime Option

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency  

**Type of update**: Infrastructure Update 

**Release Version/Date**: CMAQv5.5  

**Description**: The update changes the build option for scaling ozone with potential vorticity in the upper troposphere to represent Stratosphere-Troposphere exchange (STE) to a runtime option. The change seeks to simplify building and running CCTM with the same chemical mechanism over different domains where using the process depends on the meteorological inputs or domain's size and location. Also, changes may ease reading model code by reducing the number of embedded CPP directives.
  
**Significance and Impact**: Update makes easier application of the same CCTM executable over different model cases where ozone scaling by potential vorticity is not always wanted. 
 
**References**: N/A  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1089](https://github.com/USEPA/CMAQ/commit/9d715843c1b2eba3d3662824b635d6b7b529917f) | [PR#1089](https://github.com/USEPA/CMAQ_Dev/pull/1089)  |  

## Removed dependency on coordinate system in PV-O3

[Rohit Mathur](mailto:mathur.rohit@epa.gov), U.S. Environmental Protection Agency  

**Type of update**: Improve consistency

**Release Version/Date**: October 6, 2023 / CMAQv5.5  

**Description**: The specification of the upper layers in which O3 mixing ratios are scaled based on space and time varying potential vorticity fields (PV) is based on a prescribed pressure level: currently less than 110hPa. The determination of these model levels in the current code however (i) has a dependence on the sigma-P coordinate; (2) assumes a constant surface pressure (1000 hPa); and (3) uses VGLVLS from the METCRO3D file header. These calculations are inconsistent with the hybrid coordinate system now adopted in both WRF and CMAQ simulations and can result in the application of the PV-scaling to model (pressure) levels not intended and/or inconsistent with the parameterization detailed in Xing et al. (2016). This update removes the dependence on the sigma-P coordinate and instead uses the pressure field from the METCRO3D file to check against the 110hPa threshold.  
  
**Significance and Impact**: The changes make the model consistent with the O3-PV parameterization detailed in Xing et al. (2016). For the current 44-layer configuration (Mathur et al., 2017), the existing approximations (VGLVLS and constant surface pressure) resulted in O3 being scaled by PV fields always in layers 41-44. For the hybrid coordinate and the 44-layer configuration, the mid-level pressure of layers 42 and above are less than the 110hPa threshold so the scaling is now applied to layers 42-44. Thus, there is no impact on O3 in the top three model layers, but O3 mixing ratios change between 300-100hPa. The difference in O3 mixing ratios within the boundary layer and at the surface are negligible and not impacted by this change for the current 44-layer configuration. The impact of model changes on comparisons with ozonesonde measurements at 4 sites across North America (Hilo, Trinidad Head, Boulder, and Huntsville) are illustrated in the figures below which compare mean vertical profiles at each site for all launches during Spring (March-April-May) and Summer (June-July-August) and further illustrate that the larger differences are confined to 300-100hPa and in general the updates help improve the model estimates relative to observations at these levels.  

![Mathur_SpringMAM_SummerJJA](https://github.com/user-attachments/assets/94b95e27-17ad-4b93-b929-cbc66687f963)
 
**References**: 
Xing, J., Mathur, R., Pleim, J., Hogrefe, C., Wang, J., Gan, C.-M., Sarwar, G., Wong, D. C., and McKeen, S.: Representing the effects of stratosphere–troposphere exchange on 3-D O3 distributions in chemistry transport models using a potential vorticity-based parameterization, Atmos. Chem. Phys., 16, 10865–10877, https://doi.org/10.5194/acp-16-10865-2016, 2016   

Mathur, R., Xing, J., Gilliam, R., Sarwar, G., Hogrefe, C., Pleim, J., Pouliot, G., Roselle, S., Spero, T. L., Wong, D. C., and Young, J. (2017). Extending the Community Multiscale Air Quality (CMAQ) modeling system to hemispheric scales: overview of process considerations and initial applications, Atmos. Chem. Phys., 17, 12449–12474, https://doi.org/10.5194/acp-17-12449-2017.



|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#980](https://github.com/USEPA/CMAQ/commit/f3b87b84768a249c10b472ee76d9a4e525db90c3) | [PR#980](https://github.com/USEPA/CMAQ_Dev/pull/980)  | 



