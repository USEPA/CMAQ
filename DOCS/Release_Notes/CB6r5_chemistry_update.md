# CB6r5 update

[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency

## Brief Description

Ramboll, the developer of the Carbon Bond chemical mechanism, recently updated the chemical mechanism (CB6r5) and implemented it into the Comprehensive Air quality Model with extensions (CAMx) (Yarwood et al., 2020). CMAQ currently uses CB6r3 chemical mechanism which is updated into CB6r5. Following changes are included in CB6r5.
* Updated rate constants for 41 reactions
* Updated photolysis rates for 6 reactions: formaldehyde (two channels), acetaldehyde, higher aldehyde, glyco-aldehyde, glyoxal
* Updated reaction products and yields for several reactions
* An additional reaction (Amedro et al., 2020): NO2 + OH + H2O = HNO3 + H2O; H2O is more effective as a third body than N2 or O2 and the reaction is more effective in humid regions.
* No changes in emissions are needed for CB6r5

## Significance and Impact

Model simulations were completed with the CB6r3 and CB6r5 over the continental United States for a winter (January) and a summer (July) month in 2016. The update increases monthly mean ozone in both month (Figure 1); however, it also decreases ozone over some areas by small margin. Overall, the impacts of the update on model predictions are small. The impacts are slightly larger in summer than those in winter. It affects Model Bias both at AQS and CASTNET sites (Figure 2) by small margins.

![Ozone](ozone_impact_cb6r5.jpg) 

Figure 1: Impact of CB6r5 on monthly mean ozone
 
![Model Bias](Model_Bias_with_cb6r5_and_cb6r3.jpg) 

Figure 2: Impact of CB6r5 on monthly mean Model Bias at AQS and CASTNET sites

## Affected Files

* CCTM/scripts/bldit_cctm.csh 
* CCTM/scripts/bldit_mech.csh

* CCTM/scripts/run_cctm_2011_12US1.csh 
* CCTM/scripts/run_cctm_2014_12US1.csh  
* CCTM/scripts/run_cctm_2015_HEMI.csh 
* CCTM/scripts/run_cctm_2016_12US1.csh 
* CCTM/scripts/run_cctm_Bench_2011_12SE1.csh 
* CCTM/scripts/run_cctm_Bench_2016_12SE1.csh 

* CCTM/src/MECHS/cb6r5_aq7_aq/AE_cb6r5_ae7_aq.nml
* CCTM/src/MECHS/cb6r5_aq7_aq/CSQY_DATA_cb6r5_ae7_aq 
* CCTM/src/MECHS/cb6r5_aq7_aq/EmissCtrl_cb6r5_ae7_aq.nml 
* CCTM/src/MECHS/cb6r5_aq7_aq/GC_cb6r5_ae7_aq.nml 
* CCTM/src/MECHS/cb6r5_aq7_aq/NR_cb6r5_ae7_aq.nml 
* CCTM/src/MECHS/cb6r5_aq7_aq/RXNS_DATA_MODULE.F90 
* CCTM/src/MECHS/cb6r5_aq7_aq/RXNS_FUNC_MODULE.F90 
* CCTM/src/MECHS/cb6r5_aq7_aq/SpecDef_Dep_cb6r5_ae7_aq.txt 
* CCTM/src/MECHS/cb6r5_aq7_aq/SpecDef_cb6r5_ae7_aq.txt 
* CCTM/src/MECHS/cb6r5_aq7_aq/SpecDef_cb6r5_ae7_aq_ELMO.txt 
* CCTM/src/MECHS/cb6r5_aq7_aq/mech_cb6r5_ae7_aq.def 
* CCTM/src/MECHS/cb6r5_aq7_aq/pa_cb6r5_ae7_aq.ctl 

* CCTM/src/gas/ebi_cb6r5_ae7_aq/DEGRADE_SETUP_TOX.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/degrade.F  
* CCTM/src/gas/ebi_cb6r5_ae7_aq/degrade_data.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/final_degrade.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/find_degraded.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/hrdata_mod.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/hrdriver.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/hrg1.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/hrg2.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/hrg3.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/hrg4.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/hrinit.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/hrprodloss.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/hrrates.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/hrsolver.F 
* CCTM/src/gas/ebi_cb6r5_ae7_aq/init_degrade.F 

* CCTM/src/MECHS/mechanism_information/cb6r5_ae7_aq/AE7_species_table.md 
* CCTM/src/MECHS/mechanism_information/cb6r5_ae7_aq/NR7_species_table.md 
* CCTM/src/MECHS/mechanism_information/cb6r5_ae7_aq/cb6r5_ae7_aq_species_table.md 
* CCTM/src/MECHS/mechanism_information/cb6r5_ae7_aq/mech_cb6r5_ae7_aq.md 

References:
1.	Yarwood, G.; Shi, Y.; Beardsley, R., 2020. Impact of CB6r5 mechanism changes on air pollutant modeling in Texas. Final Report for the Texas Commission on Environmental Quality, Work Order No. 582-20-11221-014.
2.	Amedro, D., Berasategui, M., Bunkan, A. J. C., Pozzer, A., Lelieveld, J., and Crowley, J. N.: Kinetics of the OH + NO2 reaction: effect of water vapour and new parameterization for global modelling, Atmos. Chem. Phys., 20, 3091–3105, https://doi.org/10.5194/acp-20-3091-2020, 2020.

# Internal Records
#### Relevant Pull Requests:
[PR #731](https://github.com/USEPA/CMAQ_Dev/pull/731)
#### Commit IDs:
2476d6897cb15320f06ae1c9342ce0eb0e37ab41
573bbc1481e553e07cd45a7bc73ab580cfcaa9d5
7727c043b47a8978cf0b1610998250ef5d3e7c35

-----------------------
