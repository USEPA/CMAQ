### Bug Fixes to Regional Atmospheric Chemistry Mechanism Version 2 (RACM2)

[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency  

**Type of update**: Bug Fix 

**Release Version/Date**: CMAQv5.4  

**Description**: 

Two different types of bugs are fixed in RACM2:

The first update:

CMAQv5.4 has two options for calculating in-line biogenic emissions: BEIS and MEGAN. For BEIS, CMAQ uses an emission profile for calculating in-line biogenic emissions. Xiaoyang Chen at Northeastern University notified that CMAQ (BEIS) does not generate any monoterpene emissions when in-line option is enabled with RACM2. Emission profile “B10RD” is currently used for RACM2; however, it does not contain correct mapping which results in no monoterpene emissions. An emission profile “B3V10” was generated when RACM2 was initially implemented in CMAQ which contains correct mapping of model species. The model is revised to remove emission profile “B10RD” and add the emission profile “B3V10”. Biogenic emissions calculation using MEGAN does not use this profile and works properly.

The second update:

RACM2 in CMAQv533 contains several bugs related to kinetics which are now corrected.
(1)	Rate constant for reaction #R40 is missing a negative sign (mech_racm2_ae6_aq.def)
(2)	Bill Stockwell identified that raw data files for HCHO photolysis contain errors and provided updated raw data files (HCHO_MOL_RACM2 and HCHO_RAD_RACM2)
(3)	CSQY_DATA.F contains an initialization problem (CSQY_DATA.F)

**Significance and Impact**: 

The first update:

Model tests were completed with the revised emissions profile. It generates correct biogenic emissions for RACM2 using BEIS.

The second update:

Model sensitivity runs were completed using the existing and updated models (by correcting kinetics information) for 10 days in summer. Ratios of predicted 10-day mean HCHO photolysis rate coefficients with the existing and updated models are shown in Figure 1. For HCHO photolysis, the updated model increases photolysis rate coefficients for the molecular channel [Figure 1(a)] while reducing the photolysis rate coefficients for the radical channel [Figure 1(b)]. 

![image](https://user-images.githubusercontent.com/17162838/172213812-1d5034fe-05fe-4f52-a31c-7dbab8b9b2b0.png)

Figure 1: (a) Ratio of mean photolysis rate coefficient for the molecular channel of HCHO photolysis (values with updated model / values with existing model) (b) ratio of mean photolysis rate coefficients for the radical channel of HCHO photolysis (values with updated model / values with existing model) 

Predicted 10-day mean ozone concentrations with the existing model are shown in Figure 2(a) and differences in predicted mean ozone concentrations with the updated and existing models are shown in Figure 2(b). It has mixed impacts on predicted ozone. The update increases ozone over some areas while reducing it over other areas. 

![image](https://user-images.githubusercontent.com/17162838/172213855-b2f40c1e-8d7b-42a4-9057-2a3d2bb520a4.png)

Figure 2: (a) Predicted 10-day mean ozone concentrations with the existing model (b) differences in predicted mean ozone concentrations with the updated and existing models (predictions  with updated model – predictions with existing model) 

Impacts on other model species are also small. For example, differences in predicted mean sulfate concentrations with the updated and existing models are shown in Figure 3(a) and differences in predicted mean aerosol nitrate concentrations with updated and existing models are shown in Figure 3(b). Impacts on sulfate and aerosol nitrate concentrations are small.

![image](https://user-images.githubusercontent.com/17162838/172213898-f0203dc9-e869-41e1-a43a-895634bbc7b2.png)

Figure 3: (a) Differences in predicted mean sulfate concentrations with the updated and existing models (predictions with updated model – predictions with existing model) (b) differences in predicted mean aerosol nitrate concentrations with the updated and existing models (predictions with updated model – predictions with existing model)

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#739](https://github.com/USEPA/CMAQ/commit/c6d10e71ce10c4b6d6ad2816da89c6ed977a06d4) | [PR#739](https://github.com/USEPA/CMAQ_Dev/pull/739)  |
|[Merge for PR#790](https://github.com/USEPA/CMAQ/commit/7eedde0e988a62e6ea9f8444927c7a9a82797731) | [PR#790](https://github.com/USEPA/CMAQ_Dev/pull/790)  | 