### Correction to Species Tables for cb6r5m_ae7_aq
**Type of update**: Documentation update  
**Release Version/Date**: CMAQ version 5.5   
**Description:** [See release note under Carbon Bond 6 Mechanism](https://github.com/USEPA/CMAQ/wiki/CMAQ-Release-Notes:-Chemistry:-Carbon-Bond-6-Mechanism-(CB6)#correction-to-molecular-weight-of-hgiigas-in-species-tables)   

### Detailed Halogen Chemistry Update

[Golam Sarwar](sarwar.golam.email@epa.gov), U.S. Environmental Protection Agency  

**Type of update**: Science Update   

**Release Version/Date**: CMAQv5.4

**Description**:

Two different updates are implemented into the detailed halogen chemistry:

The first update:

Detailed halogen (bromine and iodine) chemistry was previously incorporated into the CB6r3 chemical mechanism and implemented into the CMAQ model (Sarwar et al., 2015; Sarwar et al., 2019). The combined chemical mechanism containing CB6r3 and detailed halogen chemistry is named as CB6r3m. Several halogen reactions and emissions estimates are updated.

• Existing CB6r3m includes several bromine reactions in cloud following Long et al., (2013, 2014). These bromine reactions in cloud are removed in the updated chemistry. The existing chemistry used the AQCHEM-KMT cloud module (kmtbr); the updated chemistry uses the standard cloud model (aq).
	
• Existing CB6r3m includes several heterogenous reactions of iodine and bromine species. CMAQ estimates total aerosol surface area which was used for calculating heterogeneous rate rates for these reactions. The chemistry is revised to use sea-salt surface area (Saiz-Lopez et al., 2014; Badia et al., 2019):

	V = 10-9× [ CNA / ρNA + CCL / ρCL + CMG / ρMG + CCA / ρCA + CK / ρK + 0.2514 CNA / ρSO4]

	S = V∙6/D_g ∙e^(0.5∙(logσ_g )^2 )

Where, V = volume of sea-salt aerosol, C = concentration, ρ = density, S = surface area of sea-salt aerosol, D_g  = particle diameter, σ_g  = geometric standard deviation, NA = sodium, Cl = chloride, SO4 = sulfate, MG = magnesium, CA = calcium, K = potasium, 0.2514 is the emission ratio of SO4/NA in sea-salt emission.

• Added heterogenous reactions of BrNO3 and BrNO2 following Fernandez et al. (2014). Reaction product yields used by Fernandez et al. (2014) are incorporated into heterogeneous rate constants (AEROSOL_CHEMISTRY.F). Revised product yields of the heterogenous reaction of HOBr following Fernandez et al. (2014).

	BrNO3 + ACl = BrCl + HNO3 

	BrNO3 + ABr = Br2 + HNO3

	BrNO2  + ACl = BrCl + HONO 

	BrNO2 + ABr = Br2 + HONO

	HOBr + ACl = BrCl 

	HOBr + ABr = Br2

• Added two gas-phase iodine reactions

	I2O2 = IO + IO (Ordonez et al., 2012)

	HOI + NO3 = IO + HNO3 (Saiz-Lopez et al., 2016)

• Existing CB6r3m contains heterogeneous loss of HI on aerosols following Sherwen et al. (2016a, 2016b) while other studies do not include such loss (Saiz-Lopez et al., 2014; Badia et al., 2019). This heterogeneous loss of HI is removed in the updated chemistry.

• Existing CB6r3m contains a reaction for CH3Br (CH3Br + OH = Br + FORM). The reaction was added for future expansion as the model does not include any CH3Br emissions. The reaction is now removed from the model.

• Existing model includes heterogeneous hydrolysis of ClNO3 and BrNO3 on J-mode aerosols; now they are added on both I- and J-mode aerosols.

• Existing model includes heterogeneous losses of I2O2, I2O3, I2O4 on J-mode aerosols; now they are added on both I- and J-mode aerosols.

• Halocarbon emissions are estimated using monthly climatological chlorophyll values from satellite. Chlorophyll values are used without any limitation. However, these values can be high in some coastal areas which can cause atomic iodine concentration to reach high levels in some coastal grid-cells. Chlorophyll values are now limited to a value of 1.0 following the procedure used in the Comprehensive Air quality Model with extensions (CAMx) (Yarwood et al., 2014). The following revised parametrization is used to estimate halocarbon emissions:

	EHC = 4.31×10-8 × AGC × fHC × fDP × chl-a  


where EHC = halocarbon emission rate, 4.31×10-8 represents a base emission rate, AGC = surface area of a grid-cell, fHC = a species-dependent emission factor, fDP = a diurnal profile factor based on the grid cell local hour peaking at noon, and chl-a = monthly climatological chl-a concentration from MODIS.

• Rosenbrock is the most efficient solver for halogen chemistry and is set as the default solver.

The second update:

Ramboll, the developer of the Carbon Bond chemical mechanism, recently updated the chemical mechanism (CB6r5) and implemented it into the Comprehensive Air quality Model with extensions (CAMx) (Yarwood et al., 2020). The Carbon Bond chemical mechanism in CMAQ  has recently been updated into CB6r5 (PR #731). Here, the updates related to CB6r5 are incorporated into the combined chemical mechanism containing the Carbon Bond chemical mechanism and the detailed halogen chemistry (CB6r5m_ae7_aq). In summary, Cb6r3m is updated into Cb6r5m keeping the detailed halogen chemistry updates contained in Cb6r3m. 

**Significance and Impact**:  

The first update:

Model simulations were completed with the existing and updated CB6r3m over the Northern Hemisphere for the year of 2016 (annual simulation). The update increases seasonal mean ozone over seawater and land areas compared to the previous chemistry (Figure 1). The impacts are higher in cooler months than those in warmer months. It affects Model Bias both at AQS (Figure 2) and CASTNET sites (Figure 3).


![image](https://user-images.githubusercontent.com/17162838/172228107-48e829aa-67bf-4662-9b8c-297a0ca3ad8b.png)

Figure 1: Impact of the halogen chemistry update on seasonal mean ozone.


![image](https://user-images.githubusercontent.com/17162838/172228347-ab452cfe-b4dd-4598-898a-d9473997ca9e.png)

Figure 2: A comparison of model predicted daily maximum 8-hr ozone with observed data (left) and monthly mean Model Bias (right) at AQS sites.


![image](https://user-images.githubusercontent.com/17162838/172228394-cfc82f81-3bd4-40ef-bb15-43a8aab2545f.png)

Figure 3: A comparison of model predicted daily maximum 8-hr ozone with observed data (left) and monthly mean Model Bias (right) at CASTNET sites.


The second update:

Two separate annual simulations were performed for 2016. One simulation used CB6r3m_ae7_aq and the other simulation used CB6r5m_ae7_aq. The update changes mean ozone in each season (Figure 1). It increases ozone in some areas while decreasing ozone over other areas. The update affects Model Bias both at AQS and CASTNET sites (Figure 2) by small margins.

![image](https://user-images.githubusercontent.com/17162838/172229418-c5350f8b-c5af-49d4-80d9-dc1e3c086159.png)

Figure 1: Impact of CB6r5m on seasonal mean ozone

![image](https://user-images.githubusercontent.com/17162838/172229459-9bda879d-8203-449b-a0ac-3e6f9ee828d9.png)

Figure 2: Impact of CB6r5m on mean Model Bias at AQS and CASTNET sites


References:
1.	Badia, A., Reeves, C. E. and Baker, A. R. and Saiz-Lopez, A. and Volkamer, R. and Koenig, T. K. and Apel, E. C. and Hornbrook, R. S. and Carpenter, L. J. and Andrews, S. J. and Sherwen, T. and von Glasow, R.  2019. Importance of reactive halogens in the tropical marine atmosphere: a regional modelling study using WRF-Chem. Atmos. Chem. Phys, 19, 3161-3189, https://doi.org/10.5194/acp-19-3161-2019.
2.	Fernandez, R. P.; Salawitch, R. J.; Kinnison, D. E.; Lamarque, J.-F.; Saiz-Lopez, A. Bromine partitioning in the tropical tropopause layer: implications for stratospheric injection. Atmospheric Chemistry and Physics, 2014, 14, 13391-13410. 
3.	Long, M. S.; Keene, W. C.; Easter, R.; Sander, R.; Kerkweg, A.; Erickson, D.; Liu, X.; Ghan, S., 2013. Implementation of the chemistry module MECCA (v2.5) in the modal aerosol version of the Community Atmosphere Model component (v3.6.33) of the Community Earth System Model. Geosci. Model Dev., 6, 255-262, https://doi.org/10.5194/gmd-6-255-2013, 2013.
4.	Long, M. S.; Keene, W. C.; Easter, R. C.; Sander, R.; Liu, X.; Kerkweg, A.; Erickson, D., 2014. Sensitivity of tropospheric chemical composition to halogen-radical chemistry using a fully coupled size-resolved multiphase chemistry–global climate system: halogen distributions, aerosol composition, and sensitivity of climate-relevant gases. Atmos. Chem. Phys., 2014, 14, 3397-3425.
5.	Ordóñez, C.; Lamarque, J.-F.; Tilmes, S.; Kinnison, D. E.; Atlas, E. L.; Blake, D. R.; Sousa Santos, G.; Brasseur, G.; Saiz-Lopez, A. Bromine and iodine chemistry in a global chemistry-climate model: description and evaluation of very short-lived oceanic sources. Atmospheric Chemistry & Physics, 2012, 12, 1423-1447.
6.	Saiz-Lopez, A.; Fernandez, R. P.; Ordóñez, C.; Kinnison, D. E.; Gómez Martín, J. C.; Lamarque, J.-F.; Tilmes, S. Iodine chemistry in the troposphere and its effect on ozone. Atmos. Chem. Phys., 2014, 14, 13119-13143.
7.	Saiz-Lopez, A., Plane, J. M. C., Cuevas, C. A., Mahajan, A. S., Lamarque, J.-F., and Kinnison, D. E.: Nighttime atmospheric chemistry of iodine, Atmos. Chem. Phys., 16, 15593–15604, https://doi.org/10.5194/acp-16-15593-2016, 2016.
8.	Sarwar, G., Gantt, B.; Schwede, D.; Foley, K.; Mathur, R.; Saiz-Lopez, A. Impact of enhanced ozone deposition and halogen chemistry on tropospheric ozone over the Northern Hemisphere, Environmental Science & Technology, 2015, 49(15):9203-9211.
9.	Sarwar, G., Gantt, B.; Foley, K.; Fahey, K.; Spero, T.; Kang, D.; Mathur, R.; Foroutan, H.; Xing, J.; Sherwen, T., Saiz-Lopez, A.: Influence of bromine and iodine chemistry on annual, seasonal, diurnal, and background ozone: CMAQ simulations over the Northern Hemisphere, Atmospheric Environment, 395-404, 2019.
10.	Sherwen, T., Evans, M. J., Carpenter, L. J., Andrews, S. J., Lidster, R. T., Dix, B., Koenig, T. K., Sinreich, R., Ortega, I., Volkamer, R., Saiz-Lopez, A., Prados-Roman, C., Mahajan, A. S., and Ordóñez, C.: Iodine's impact on tropospheric oxidants: a global model study in GEOS-Chem. Atmos. Chem. Phys., 2016, 16, 1161-1186.
11.	Sherwen, T., Schmidt, J. A., Evans, M. J., Carpenter, L. J., Großmann, K., Eastham, S. D., Jacob, D. J., Dix, B., Koenig, T. K., Sinreich, R., Ortega, I., Volkamer, R., Saiz-Lopez, A., Prados-Roman, C., Mahajan, A. S., and Ordóñez, C. Global impacts of tropospheric halogens (Cl, Br, I) on oxidants and composition in GEOS-Chem, Atmos. Chem. Phys., 2016, 16, 12239-12271.
12.	Yarwood, G.; Jung, J; Ou, N.; Emery, C., 2012. Improving CAMx performance in simulating ozone transport from the Gulf of Mexico, Final Report for the Texas Commission on Environmental Quality, Project No. 0626408I.
13. Yarwood, G.; Shi, Y.; Beardsley, R., 2020. Impact of CB6r5 mechanism changes on air pollutant modeling in Texas. Final Report for the Texas Commission on Environmental Quality, Work Order No. 582-20-11221-014.
14.	Amedro, D., Berasategui, M., Bunkan, A. J. C., Pozzer, A., Lelieveld, J., and Crowley, J. N.: Kinetics of the OH + NO2 reaction: effect of water vapour and new parameterization for global modelling, Atmos. Chem. Phys., 20, 3091–3105, https://doi.org/10.5194/acp-20-3091-2020, 2020.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#717](https://github.com/USEPA/CMAQ/commit/69bb1dd6fc76bdb1ca3042f951734d17e9ff6a9c) | [PR#717](https://github.com/USEPA/CMAQ_Dev/pull/717)  |
|[Merge for PR#738](https://github.com/USEPA/CMAQ/commit/cbe950a910c785aa24d047480c8a3c4a05d4c329) | [PR#738](https://github.com/USEPA/CMAQ_Dev/pull/738)  |