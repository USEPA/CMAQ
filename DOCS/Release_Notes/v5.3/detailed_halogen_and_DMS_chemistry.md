# Detailed halogen and DMS chemistry with CB6r3

**Author/P.O.C.:** [Golam Sarwar](mailto:sarwar.golam@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
Detailed halogen (bromine and iodine) chemistry was previously incorporated into the CB05 chemical mechanism. Now, the detailed halogen chemistry is combined with CB6r3 and implemented into the CMAQ model. It contains 38 gas-phase reactions and 8 heterogeneous reactions for bromine chemistry and 44 gas-phase reactions and 20 heterogeneous reactions for iodine chemistry. Dimethyl sulfide (DMS) chemistry is also combined with CB6r3 and implemented into the CMAQ model. It contains 7 gas-phase reactions involving DMS and oxidants. The combined chemical mechanism containing CB6r3, detailed halogen and DMS chemistry is named as CB6r3m. It is primarily intended for use in the hemispheric CMAQ model though it can be used in the regional version of the model.

## Significance and Impact
Model sensitivity runs were completed with CB6r3 (without detailed halogen and DMS chemistry) and CB6r3m (with detailed halogen and DMS chemistry) over the Northern Hemisphere for three months in 2015 (October-December). It reduces ozone by 3-15 ppbv (Figure 1) and increases sulfur dioxide by 20-160 pptv (Figure 2) and sulfate by 0.1-0.8 ug/m3 (Figure 3) over much of the seawater. It reduces ozone and increases sulfate over land by much smaller margins than over seawater.

![Ozone](ozone_impact.jpg) 
Figure 1: Impact of halogen chemistry on ozone (three-month average).

![SO2](so2_impact.jpg) 
Figure 2: Impact of halogen chemistry on sulfur dioxide (three-month average).

![SO4](sulfate_impact.jpg) 
Figure 3: Impact of halogen chemistry on sulfate (three-month average).



## Affected Files
CCTM/scripts/bldit_cctm.csh
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/AE_cb6r3m_ae7_kmtbr.nml
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/CSQY_DATA_cb6r3m_ae7_kmtbr
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/GC_cb6r3m_ae7_kmtbr.nml
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/NR_cb6r3m_ae7_kmtbr.nml
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/RXNS_DATA_MODULE.F90
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/RXNS_FUNC_MODULE.F90
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/SpecDef_Dep_cb6r3m_ae7_kmtbr.txt
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/SpecDef_cb6r3m_ae7_kmtbr.txt
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/mech_cb6r3m_ae7_kmtbr.def
CCTM/src/aero/aero6/AEROSOL_CHEMISTRY.F
CCTM/src/aero/aero6/AERO_DATA.F
CCTM/src/aero/aero6/SOA_DEFN.F
CCTM/src/cloud/acm_ae6/hlconst.F
CCTM/src/cloud/acm_ae7_kmtbr/AQ_DATA.F
CCTM/src/cloud/acm_ae7_kmtbr/acmcld.f
CCTM/src/cloud/acm_ae7_kmtbr/aq_map.F
CCTM/src/cloud/acm_ae7_kmtbr/aqchem_Function.F90
CCTM/src/cloud/acm_ae7_kmtbr/aqchem_Global.F90
CCTM/src/cloud/acm_ae7_kmtbr/aqchem_Initialize.F90
CCTM/src/cloud/acm_ae7_kmtbr/aqchem_Integrator.F90
CCTM/src/cloud/acm_ae7_kmtbr/aqchem_Jacobian.F90
CCTM/src/cloud/acm_ae7_kmtbr/aqchem_JacobianSP.F90
CCTM/src/cloud/acm_ae7_kmtbr/aqchem_LinearAlgebra.F90
CCTM/src/cloud/acm_ae7_kmtbr/aqchem_Model.F90
CCTM/src/cloud/acm_ae7_kmtbr/aqchem_Parameters.F90
CCTM/src/cloud/acm_ae7_kmtbr/aqchem_Precision.F90
CCTM/src/cloud/acm_ae7_kmtbr/aqchem_Rates.F90
CCTM/src/cloud/acm_ae7_kmtbr/aqchem_kmt.F90
CCTM/src/cloud/acm_ae7_kmtbr/cldproc_acm.F
CCTM/src/cloud/acm_ae7_kmtbr/convcld_acm.F
CCTM/src/cloud/acm_ae7_kmtbr/getalpha.F
CCTM/src/cloud/acm_ae7_kmtbr/hlconst.F
CCTM/src/cloud/acm_ae7_kmtbr/indexn.f
CCTM/src/cloud/acm_ae7_kmtbr/opwdep.F
CCTM/src/cloud/acm_ae7_kmtbr/rescld.F
CCTM/src/cloud/acm_ae7_kmtbr/scavwdep.F
CCTM/src/depv/m3dry/DEPVVARS.F
CCTM/src/emis/emis/BIOG_EMIS.F
CCTM/src/emis/emis/EMIS_DEFN.F
CCTM/src/emis/emis/MGEMIS.F
CCTM/src/emis/emis/SSEMIS.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/DEGRADE_SETUP_TOX.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/degrade.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/degrade_data.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/final_degrade.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/find_degraded.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/hrdata_mod.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/hrdriver.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/hrg1.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/hrg2.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/hrg3.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/hrg4.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/hrinit.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/hrprodloss.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/hrrates.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/hrsolver.F
CCTM/src/gas/ebi_cb6r3m_ae7_kmtbr/init_degrade.F
CCTM/src/vdiff/acm2_m3dry/ASX_DATA_MOD.F
UTIL/inline_phot_preproc/photolysis_CSQY_data/COHBR_JPL2010
UTIL/inline_phot_preproc/photolysis_CSQY_data/IBR_IUPAC10
UTIL/inline_phot_preproc/photolysis_CSQY_data/IONO2_06

## References
1.	Sarwar, G., Gantt, B.; Schwede, D.; Foley, K.; Mathur, R.; Saiz-Lopez, A. Impact of enhanced ozone deposition and halogen chemistry on tropospheric ozone over the Northern Hemisphere, Environmental Science & Technology, 2015, 49(15):9203-9211.
2.	Saiz-Lopez, A.; Fernandez, R. P.; Ordóñez, C.; Kinnison, D. E.; Gómez Martín, J. C.; Lamarque, J.-F.; Tilmes, S. Iodine chemistry in the troposphere and its effect on ozone. Atmos. Chem. Phys., 2014, 14, 13119-13143.
3.	Fernandez, R. P.; Salawitch, R. J.; Kinnison, D. E.; Lamarque, J.-F.; Saiz-Lopez, A. Bromine partitioning in the tropical tropopause layer: implications for stratospheric injection. Atmospheric Chemistry and Physics, 2014, 14, 13391-13410. 
4.	Sherwen, T., Evans, M. J., Carpenter, L. J., Andrews, S. J., Lidster, R. T., Dix, B., Koenig, T. K., Sinreich, R., Ortega, I., Volkamer, R., Saiz-Lopez, A., Prados-Roman, C., Mahajan, A. S., and Ordóñez, C.: Iodine's impact on tropospheric oxidants: a global model study in GEOS-Chem. Atmos. Chem. Phys., 2016, 16, 1161-1186.
5.	Sherwen, T., Schmidt, J. A., Evans, M. J., Carpenter, L. J., Großmann, K., Eastham, S. D., Jacob, D. J., Dix, B., Koenig, T. K., Sinreich, R., Ortega, I., Volkamer, R., Saiz-Lopez, A., Prados-Roman, C., Mahajan, A. S., and Ordóñez, C. Global impacts of tropospheric halogens (Cl, Br, I) on oxidants and composition in GEOS-Chem, Atmos. Chem. Phys., 2016, 16, 12239-12271.
6.	Yarwood, Y., Sakulyanontvittaya, T., Nopmongcol, O., and Koo, K. Ozone Depletion by Bromine and Iodine over the Gulf of Mexico, Final Report for the Texas Commission on Environmental Quality, November 2014.
7.	Sarwar, G., J. Xing, K. Fahey, K. Foley, D. Wong, R. Mathur, C. M. Gan, B. Gant, H. Simon, 2016. Dimethylsulfide chemistry: seasonal and spatial impacts on sulfate, Chapter 55, Clemens Mensink, George Kallos (ed.), Air Pollution Modeling and its Application XXV. Springer, 347-352. 



-----
## Internal Records:
#### Relevant Pull Requests:
[PR #362](https://github.com/usepa/cmaq_dev/pull/362)
#### Commit IDs:
e071d336de4e98b37e7d0d7b23b73f81f4daf79b
66f076fbb441e4d75e9ee55821da1cd63bf4a4d9
4e95eac60e9c78b27faf0c5e6d1c51b0b15cf7db
81edc0a978469bf9ea4e917f9de592921431134d

-----
