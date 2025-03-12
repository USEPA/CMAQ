### Revised dry dep flux for NH3
[Jon Pleim](mailto:pleim.jon@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  
**Release Version/Date**: CMAQv5.5    
**Description**:  
Changed the calculation of NH3 dry dep flux and NH3 emission flux.
Now the NH3 dry dep flux is calculated in ABFLUX by setting the ground and stomatal NH3 concentrations to zero. Emission flux is then the Net Flux - Dry Dep flux.  

Also, fixed error in cuticle resistance to ammonia when not using bi-directional flux.
 
**Significance and Impact**:
First part only changes NH3 and NH3_Emis output in CCTM_DRYDEP files.
Second part changes NH3 deposition when running without ammonia bi-directional exchange (ABFLUX). This change causes a minor increase in PM and decreases O3 (see figure below), providing a more accurate NH3 dry dep flux estimate.


<table>
<thead>
<tr>
<th><img width="100%" src="https://github.com/user-attachments/assets/55c33725-ff52-4d28-9093-7c26f545f02d"></th>
<th><img width="100%" src="https://github.com/user-attachments/assets/ad68c147-30af-40ee-87f0-6f9420dce0c5"></th>
</tr>
</table>

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1073](https://github.com/USEPA/CMAQ/commit/c58dbf7b0f60d4bd04188205236e664fab7902cd) | [PR#1073](https://github.com/USEPA/CMAQ_Dev/pull/1073)  |



### New Aerosol Deposition Model (aero_depv)  
[Jon Pleim](mailto:pleim.jon@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  
**Release Version/Date**: CMAQv5.4  
**Description**:  
A new aerosol deposition model has been developed and added to CMAQv5.4.  The model development, description, and testing are described in a new Journal article to be published in JAMES (Pleim et al, 2022).
The inspiration for this new model comes from comparisons of currently used aerosol dry deposition models to a compendia of published field measurement studies in various landscapes that show very poor agreement over a wide range of particle sizes.  The new aerosol dry deposition model that is a modification of the current model in CMAQv5.3 agrees much better with measured dry deposition velocities across particle sizes.  The key innovation is the addition of a second inertial impaction term for microscale obstacles such as leaf hairs, microscale ridges, and needleleaf edge effects.  
**Significance and Impact**:
The most significant effect of the new model is to increase the mass dry deposition of the accumulation mode aerosols in CMAQ.  Accumulation mode mass dry deposition velocities increase by almost an order of magnitude in forested areas with lesser increases for shorter vegetation.  Peak PM2.5 concentrations are reduced in some forested areas by up to 40% in CMAQ simulations.   
**References**: 
Pleim, J. E., Ran, L., Saylor, R. D., Willison, J., & Binkowski, F. S. (2022). A new aerosol dry deposition model for air quality and climate modeling. Journal of Advances in Modeling Earth Systems, 14, e2022MS003050. [https://doi. org/10.1029/2022MS003050](https://doi.%20org/10.1029/2022MS003050)

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#842](https://github.com/USEPA/CMAQ/commit/289701974ba9610cf92043e9f223fbbf0f888bbd) | [PR#842](https://github.com/USEPA/CMAQ_Dev/pull/842)  |
