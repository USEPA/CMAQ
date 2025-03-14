### Correction to molecular weight of HGIIGAS in species tables

[Kristen Foley](mailto:foley.kristen@epa.gov), U.S. Environmental Protection Agency  
**Type of update:** Documentation Update   
**Release Version/Date:** CMAQv5.5  
**Description:** The molecular weight for HGIIGAS was incorrectly listed as 200.6 in the species tables for cb6r3_ae7_aq, cb6r5_ae7_aq, cb6r5hap_ae7_aq, cb6r5m_ae7_aq. This documentation can be found under CCTM/src/MECHS/README.md.  The molecular weight used in GC namelist files for these mechanisms is 271.5. The documentation in the species tables has been updated to be consistent with the namelist files (based on Donohoue et al.m 2005).  

This issue was first identified on the CMAS User forum by Shengpo (https://forum.cmascenter.org/t/error-of-the-molecular-weight-for-hgiigas/4673).  

**Significance and Impact**: Documentation updates only. No changes to model results.  

**References**:  
Deanna L. Donohoue, Dieter Bauer, and Anthony J. Hynes. The Journal of Physical Chemistry A 2005 109 (34), 7732-7741. DOI: 10.1021/jp051354l 

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1081](https://github.com/USEPA/CMAQ/commit/30de01b8e0303592b70439908a0b4b02708f881f) | [PR#1081](https://github.com/USEPA/CMAQ_Dev/pull/1081)  | 
 

### Carbon Bond Chemical Mechanism Version 6 Release 5  (CB6r5)

[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency  

**Type of update:** Science Update 

**Release Version/Date:** CMAQv5.4  

**Description:** 

Ramboll, the developer of the Carbon Bond chemical mechanism, recently updated the chemical mechanism (CB6r5) and implemented it into the Comprehensive Air quality Model with extensions (CAMx) (Yarwood et al., 2020). CMAQ currently uses CB6r3 chemical mechanism which is updated into CB6r5. Following changes are included in CB6r5.

• Updated rate constants for 41 reactions

• Updated photolysis rates for 6 reactions: formaldehyde (two channels), acetaldehyde, higher aldehyde, glyco-aldehyde, glyoxal

• Updated reaction products and yields for several reactions

• An additional reaction (Amedro et al., 2020): NO2 + OH + H2O = HNO3 + H2O; H2O is more effective as a third body than N2 or O2 and the reaction is more effective in humid regions.

• No changes in emissions are needed for CB6r5

**Significance and Impact**:

Model simulations were completed with the CB6r3 and CB6r5 over the continental United States for a winter (January) and a summer (July) month in 2016. The update increases monthly mean ozone in both month (Figure 1); however, it also decreases ozone over some areas by small margin. Overall, the impacts of the update on model predictions are small. The impacts are slightly larger in summer than those in winter. It affects Model Bias both at AQS and CASTNET sites (Figure 2) by small margins.

![image](https://user-images.githubusercontent.com/17162838/172222534-0323ac35-1cc7-490a-a43a-827062182504.png)

Figure 1: Impact of CB6r5 on monthly mean ozone

![image](https://user-images.githubusercontent.com/17162838/172222590-a19d6f4e-bba6-46e7-b9e4-4eff003c3f7f.png)

Figure 2: Impact of CB6r5 on monthly mean Model Bias at AQS and CASTNET sites

**References**:
1. Yarwood, G.; Shi, Y.; Beardsley, R., 2020. Impact of CB6r5 mechanism changes on air pollutant modeling in Texas. Final Report for the Texas Commission on Environmental Quality, Work Order No. 582-20-11221-014.
2. Amedro, D., Berasategui, M., Bunkan, A. J. C., Pozzer, A., Lelieveld, J., and Crowley, J. N.: Kinetics of the OH + NO2 reaction: effect of water vapour and new parameterization for global modelling, Atmos. Chem. Phys., 20, 3091–3105, https://doi.org/10.5194/acp-20-3091-2020, 2020.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#731](https://github.com/USEPA/CMAQ/commit/deb5b42cdd0041549a4aa5b8e6d069f2b75c203d) | [PR#731](https://github.com/USEPA/CMAQ_Dev/pull/731)  | 


### Simple Halogen Chemistry Update

[Golam Sarwar](sarwar.golam.email@epa.gov), U.S. Environmental Protection Agency  

**Type of update**: Science Update   

**Release Version/Date**: CMAQv5.4

**Description**: Several changes were made to the simple halogen chemistry which are described below:

First change:

A simple halogen mediated first order ozone loss was previously developed by using hemispheric CMAQ results obtained without and with detailed bromine/iodine chemistry. The detailed bromine/iodine chemistry has recently been updated and hemispheric model simulations were completed without and with the updated bromine/iodine chemistry for 2016. The simple halogen mediated first order ozone loss is re-derived using the annual hemispheric CMAQ results obtained without and with full bromine/iodine chemistry. The revised halogen mediated first-order rate constant for ozone loss:

k_O3 (P) = min⁡ ( 2.0×10E-06, 6.7006×10E-11 exp(10.7435×P)+ 3.4153×10E-8 exp(-0.6713×P) )

Where kO3 (s-1) is the first-order rate constant and P is the atmospheric pressure (atm). It is applied to grid-cells over oceanic areas. The revised halogen mediated first-order rate constant for ozone loss is lower than the previous value.

Second change:

Ocean files are generated using spatial allocator. Winston Hao of New York Department of Environmental Conservation reported that ocean file generated by the spatial allocator may occasionally contain some tiny (~1E-09) negative and positive values for SURF and OPEN near state borders. Simple halogen chemistry is activated when OPEN+SURF value in any grid-cell is positive (>0.0). The presence of tiny positive SURF and OPEN values activates the simple halogen chemistry over land and reduces ozone. CMAQ has a check for negative values of OPEN and SURF which are reset to 0.0 as follows (centeralized_io_module.F):

WHERE ( ocean .LT. 0.0 ) ocean = 0.0  ! ensure values are nonnegative
WHERE ( szone .LT. 0.0 ) szone = 0.0  ! ensure values are nonnegative

To avoid the activation of the simple halogen chemistry when tiny positive values are present, the existing checks are revised so that any negative and small positive values are reset to 0.0 as follows:

WHERE ( ocean .LT. 0.001 ) ocean = 0.0  ! ensure values are greater than 0.001
WHERE ( szone .LT. 0.001 ) szone = 0.0  ! ensure values are greater than 0.001 

Third change:

The condensed halogen chemistry is activated when OPEN+SURF value in any grid-cell > 0.001; otherwise it is inactive. When the condensed halogen chemistry is active, halogen mediated ozone loss occurs with a prescribed first order rate constant. In the existing implementation, the prescribed first order rate constant does not vary with the values of OPEN+SURF. Values of OPEN+SURF is 1.0 over open ocean; however, values can be less than 1.0 near coastal areas. In the updated implementation, the prescribed first order rate constant is multiplied by the value of OPEN+SURF to account for the halogen mediated ozone loss. The full extent of the halogen mediated ozone loss occurs over open ocean since OPEN+SURF = 1.0 over such areas. In contrast, impact of the halogen mediated ozone loss is reduced over coastal areas since OPEN+SURF < 1.0 over such areas. SEAICE can be present in some grid-cells. The presence of SEAICE was previously used to simply turn-on or turn-off the condensed halogen chemistry. It is now included in the calculation of the halogen mediated rate constant.

Existing implementation of the condensed halogen chemistry:

k = prescribed first order halogen mediated rate constant when OPEN+SURF > 0.001 and no SEAICE is present.
k = 0 when OPEN+SURF ≤ 0.001 or SEAICE is present

Updated implementation of the condensed halogen chemistry:

k = (OPEN + SURF - SEAICE) × prescribed first order halogen mediated rate constant when OPEN+SURF > 0.001
k = 0 when OPEN+SURF ≤ 0.001


**Significance and Impact**:  

First change:

Model sensitivity runs were completed using cb6r3_ae7_aq chemical mechanism with the existing and updated simple first order ozone loss for the continental US domain for a period of 9-days in summer (June 22-30, 2016). The revised simple first order ozone loss increases the average ozone over seawater and coastal areas by up to 1.5 ppbv. Impact is higher over seawater than over coastal area. Impact over the interior portion of the domain is negligible. 

![image](https://user-images.githubusercontent.com/2692799/167717635-e1aa5591-64ca-4a59-8de0-d7a8294f5051.png)  
**Figure 1: Impact of the updated simple halogen chemistry on O3**

Second change:

Model sensitivity runs were completed using the existing and updated checks for OPEN and SURF values for a 10-day period in summer. Model with updated checks for OPEN and SURF values has only small impacts on predicted results. The mean difference in O3 concentrations during the 10-day period are shown in Figure 2. Note that the ocean file used in this test does not contain any tiny positive values along state borders; hence the problem reported by a CMAQ user does not show up in the model results.

![image](https://user-images.githubusercontent.com/2692799/167717666-bf85d529-f378-4d20-83e8-02680a05c4c4.png)  
**Figure 2: Impact of using a threshold value of 0.001 for OPEN and SURF values on O3**

Third change:

Two different model simulations were completed using the existing and updated implementation of the condensed halogen chemistry for 10 days in summer (June 21 -  June 30, 2016). It employed 12-km horizontal grid resolution with 35 vertical layers. The difference in O3 concentrations (updated – existing implementation) is shown in Figure 3. Model with the updated implementation does not have any impact on O3 over open ocean. However, it increases O3 over coastal areas when OPEN+SURF  < 1.0. 

![image](https://user-images.githubusercontent.com/2692799/167717690-4e217cf9-5432-4f2d-8ed4-50a269f156e5.png)  
**Figure 3: Impact of the updated implementation of halogen chemistry on O3**

**References**:  

1.	Sarwar, G.; Gantt, B.; Foley, K.; Fahey, K.; Spero T. L.; Kang, D., Mathur, Rohit M., Hosein F.; Xing, J.; Sherwen, T.; Saiz-Lopez, A., 2019: Influence of bromine and iodine chemistry on annual, seasonal, diurnal, and background ozone: CMAQ simulations over the Northern Hemisphere, Atmospheric Environment, 213, 395-404.
2.	Sarwar, G.; Gantt, B.; Schwede, D.; Foley, K.; Mathur, M.; Saiz-Lopez, A., 2015: Impact of enhanced ozone deposition and halogen chemistry on tropospheric ozone over the Northern Hemisphere, Environmental Science & Technology, 49(15):9203-9211.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#712](https://github.com/USEPA/CMAQ/commit/33a020e2ad4ce8fbf0ce2982e2ab139017afd71f) | [PR#712](https://github.com/USEPA/CMAQ_Dev/pull/712)  |
|[Merge for PR#784](https://github.com/USEPA/CMAQ/commit/c197c5d98b0f3218092a2c2b2050b3b27c95f138) | [PR#784](https://github.com/USEPA/CMAQ_Dev/pull/784)  | 
|[Merge for PR#870](https://github.com/USEPA/CMAQ/commit/b26adbc5c00f1ebd5f2361c13cf82c437bac9ccd) | [PR#870](https://github.com/USEPA/CMAQ_Dev/pull/870)  |  


### DMS Chemistry

[Golam Sarwar](sarwar.golam.email@epa.gov), U.S. Environmental Protection Agency 

**Type of update**: New Feature  

**Release Version/Date**: CMAQv5.4

**Description**:  

Dimethyl sulfide (DMS) chemistry was previously added in the hemispheric CMAQ model. The details of the chemistry, emissions and their impact on model results over the Northern Hemisphere are described in Zhao et al. (2021). Here, DMS chemistry is combined with cb6r5 chemical mechanism and implemented into the regional CMAQ model. The DMS chemistry consists of 2 chemical reactions with OH, 1 reaction with NO3, and 1 reaction with Cl. 

DMS + OH = SO2 + MEO2 + FORM (abstraction channel)

DMS + OH = 0.75 × SO2 + 0.25 × MSA + MEO2 (addition channel)

DMS + NO3 = SO2 + HNO3 + MEO2 + FORM

DMS + Cl = 0.86 × SO2 + 0.14 × MSA + MEO2 + 0.45 × FORM + 0.45 × HCl + 0.55 ×ClO

These reactions produce SO2 which is then oxidized into sulfate via gas-phase and aqueous-phase chemical reactions. DMS emissions from ocean are calculated using the gas transfer velocity and climatological DMS concentrations in seawater reported by Lana et al. (2011).

**Significance and Impact**:  

Model sensitivity simulations were completed using cb6r5_ae7_aq chemical mechanism without and with the DMS chemistry over the continental US domain for January and July in 2016. DMS chemistry enhances SO2 over seawater and adjacent land areas by 0-45 pptV in January and 0-60 pptV in July (Figure 1). It  enhances sulfate over seawater and adjacent land areas by 0-0.2 μg/m3 in January and 0-0.7 μg/m3 in July (Figure 2). Impact over the interior portion of the modeling domain is generally small.

Impact of the DMS chemistry on model performance was calculated using data from all networks (Figure 3). It can affect Normalized Mean Bias for sulfate at CASTNET, CSN and IMPROVE networks. However, the impacts are generally small when all sites are considered for calculating Normalized Mean Bias. Impacts on Normalized Mean Bias can be higher in coastal areas. It’s impact on ozone is small (< ±0.3 ppb) and impact on model performance is negligible.


![image](https://user-images.githubusercontent.com/2692799/167717457-8bb2e2dc-e246-49d5-adaf-93a9283eca35.png)**Figure 1: (a) mean SO2 without DMS chemistry in January (b) impact of DMS chemistry on SO2 in January (c) mean SO2 without DMS chemistry in July (d) impact of DMS chemistry on SO2 in July**

![image](https://user-images.githubusercontent.com/2692799/167717484-b1c40b51-be9d-4fec-bb8d-d5e7c0ae1a3d.png)**Figure 2: (a) mean sulfate without DMS chemistry in January (b) impact of DMS chemistry on sulfate in January (c) mean sulfate without DMS chemistry in July (d) impact of DMS chemistry on sulfate in July**

![image](https://user-images.githubusercontent.com/2692799/167717507-36c144ad-1eef-4d26-a69c-7589d8dffa0d.png)**Figure 3: Normalized Mean Bias of sulfate without and with DMS chemistry (a) IMPROVE sites in January (b) CSN sites in January (c) CASTNET sites in January (d) IMPROVE sites in July (e) CSN sites in July (f) CASTNET sites in July**  

Existing ocean files will not work with the DMS chemistry; new ocean files with DMS concentrations in seawater are needed and can be generated using a new python based tool.

**References**:  

1. Zhao, J., Sarwar, G., Gantt, B., Foley, K., Kang, D., Fahey, K., Mathur, R., Henderson, B. H., Pye, H. O. T., Zhang, Y., Saiz-Lopez, A., 2021. Impact of dimethylsulfide chemistry on air quality over the Northern Hemisphere, Atmospheric Environment, 244, 117961:1-10.
2. Lana, A., Bell, T.G., Simó, R., Vallina, S.M., Ballabrera-Poy, J., Kettle, A.J., Dachs, J., Bopp, L., Saltzman, E.S., Stefels, J., Johnson, J.E., Liss, P.S., 2011. An updated climatology of surface dimethlysulfide concentrations and emission fluxes in the global ocean. Global Biogeochemical Cycles, 25, GB1004,doi:10.1029/2010GB003850.
