# Representing the Influence of Stratosphere-Troposphere Exchange on Simulated O3 Distributions 

**Author/P.O.C.:**, [Rohit Mathur](mailto:mathur.rohit@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description 

Though the role of cross-tropopause transport of O3 is acknowledged as a significant contributor to the tropospheric O3 budget, the distribution of O3 in the troposphere that originates from the stratosphere is still uncertain. Tightening O3 NAAQS and decreasing amounts of photo-chemically derived O3 due to continuously declining anthropogenic precursor emissions, now put greater emphasis on accurately characterizing the fraction of O3 in the troposphere, especially at the surface, that is of stratospheric origin.  This fraction varies spatially and seasonally in response to the tropopause height, with larger contributions episodically from deep intrusion events associated with weather patterns and frontal movement.  Potential vorticity (PV) has been shown to be a robust indicator of air mass exchange between the stratosphere and the troposphere with strong positive correlation with O3 and other trace species transported from the stratosphere to the upper troposphere (Danielsen, 1968). Numerous modeling studies have used this correlation to develop scaling factors that specify O3 in the modeled upper tropospheric-lower stratospheric (UTLS) based on estimated PV. The reported O3/PV ratios (e.g., Ebel et al, 1991; Carmichael et al, 1998; McCaffery et al, 2004; Mathur et al., 2008) however exhibit a wide range: 20-100 ppb/PVu (1 PV unit = 10-6 m2 K kg-1 s-1), as a function of location, altitude and season.  

To overcome these challenges and to develop a more robust representation of STE impacts, we have developed a dynamic O3-PV function based on 21-year ozonesonde records from World Ozone and Ultraviolet Radiation Data Centre (WOUDC) with corresponding PV values from WRF-CMAQ simulation across the northern hemisphere from 1990 to 2010. The result suggests strong spatial and seasonal variations of O3/PV ratios which exhibits large values in the upper layers and in high latitude regions, with highest values in spring and the lowest values in autumn over an annual cycle. The new generalized parameterization, detailed in Xing et al. (2016) can dynamically represent O3 in the UTLS across the northern hemisphere. The implementation of the new function in the hemispheric version of CMAQ significantly improves the model’s performance in the simulation of UTLS O3 in both magnitude and seasonality compared to observations, which then enables a more accurate simulation of the vertical distribution of O3 across the northern hemisphere (Xing et al., 2016). These can then be used to derive more realistic vertically and temporally varying LBCs for regional nested model calculations.  

In CMAQv5.2, this PV-based scaling of O3 in the model’s UTLS is invoked through calling the subroutine PVO3 in the subroutine SCIPROC.  In the current implementation, modelled O3 for all layers at pressure <110mb is scaled by the estimated PV using the dynamical function such that the scaling is dependent on latitude, altitude, and time. It is recommended that users pay attention to the vertical resolution employed in the discretization of the modelled vertical extent. Since O3 vertical profiles exhibit a strong gradient near the tropopause, inadequate vertical resolution near the tropopause can result in excessive artificial diffusion of O3 resulting in unrealistic high simulated O3 in the mid troposphere. This can then be entrained downwards through deep clouds and then to the boundary layer thereby unrealistically impacting predicted surface O3.   

## Affected Files:

driver/wrf/sciproc.F  
driver/yamo/sciproc.F  
pv_o3/pvo3.F  

## References: 

Carmichael, G. R., Uno, I., Phadnis, M. J., Zhang, Y., and Sunwoo, Y.: Tropospheric ozone production and transport in the springtime in east Asia. J. Geophys. Res., 103(D9), 10649-10671, 1998.  

Danielsen, E. F., Stratospheric-Tropospheric Exchange Based on Radioactivity, Ozone and Potential Vorticity, J. Atmos. Sci., 25, 502–518, 1968.   

Ebel, A., H. Haus, H. J. Jakobs, M. Laube, M. Memmesheimer, and A. Oberreuter: Simulation of ozone intrusion caused by a tropopause fold and cut-off low, Atmos. Environ., 25, 2131–2144, 1991.   

Mathur, R., Lin, H. M., McKeen, S., Kang, D., and Wong, D.: Three-dimensional model studies of exchange processes in the troposphere: use of potential vorticity to specify aloft O3 in regional models, Presented at the 7th Annual CMAS Conference, available at: https://www.cmascenter.org/conference/2008/slides/mathur_three-dimension_model_cmas08.ppt, 2008.   

McCaffery, S. J., S. A. McKeen, E.-Y. Hsie, D. D. Parrish, O. R. Cooper, J. S. Holloway, G. Hubler, F. C. Fehsenfeld, and M. Trainer: A case study of stratosphere-troposphere exchange during the 1996 North Atlantic Regional Experiment, J. Geophys. Res., 109, D14103, doi:10.1029/2003JD004007, 2004.   

Xing, J., R. Mathur, J. Pleim, C. Hogrefe, J. Wang, C.-M. Gan, G. Sarwar, D. Wong, and S. McKeen, Representing the effects of stratosphere-troposphere exchange on 3D O3 distributions in chemistry transport models using a potential vorticity based parameterization, Atmos. Chem. Phys., 16, 10865-10877, doi:10.5194/acp-16-10865-2016, 2016.   


-----
## Internal Records:

### Relevant Pull Requests: 
  [PR #31](https://github.com/usepa/cmaq_dev/pull/31)

### Commit IDs:
992729db506091be3ce80f5086d909e0ea15ae9f  
3dc45f1e9b2e9b35454ad51eb218e420fc57b701  
62e4165b45ef933f29b34d061e0a545c8cb8632e  
60647d3b104b09e2e0afa47f53fd7bb5083aa82a    
