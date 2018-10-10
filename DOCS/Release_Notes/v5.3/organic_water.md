# Uptake of water onto organic aerosol

**Author/P.O.C.:**, [Havala O. T. Pye](mailto:pye.havala@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The organic components of PM2.5 take up water, similar to the inorganic components, although to a lesser degree. In CMAQv5.2.1 and prior, this aerosol water was not accounted for.
In this work, we parameterized water uptake using a hygroscopicity parameter, kappa, for organic aerosol. Kappa was parameterized based on OM/OC
such that more oxygenated components promoted water uptake to a greater degree. 
This water uptake increases wet aerosol mass which changes the aerosol size and resulting deposition as well as light scattering. 
The effects of water on semivolatile organic paritioning are not included. ISORROPIA continues to be used for the inorganic water as in v5.2.1.

New species:  
AORGH2OJ, particulate water due to organic components in accumulation mode particles

## Significance and Impact
Adding uptake of water onto organic aerosol 
increased total particulate water by 60% in organic-rich regions such as the southeastern US in summer (Pye et al. 2017). 
The effects of water will not be seen directly in dry aerosol mass, but will affect deposition (by modifying aerosol size) and in situ measurements such as AOD.
Small secondary effects on dry PM2.5 mass (such as a 6% reduction in PM2.5 for southeast US in summer) due to deposition changes were observed in testing.                     

## Affected Files
CCTM/src/aero/aero7 (linked to aero6)   
AE\*aero7\*nml                     

## References
Pye, H. O. T., Murphy, B. N., Xu, L., Ng, N. L., Carlton, A. G., Guo, H., Weber, R., Vasilakos, P., Appel, K. W., Budisulistiorini, S. H., Surratt, J. D., Nenes, A., Hu, W., Jimenez, J. L., Isaacman-VanWertz, G., Misztal, P. K., and Goldstein, A. H.: On the implications of aerosol liquid water and phase separation for organic aerosol mass, Atmos. Chem. Phys., 17, 343-369, https://doi.org/10.5194/acp-17-343-2017, 2017.                       

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #335]



-----

