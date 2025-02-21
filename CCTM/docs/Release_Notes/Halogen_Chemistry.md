# Detailed halogen (bromine and iodine) chemistry

**Author/P.O.C.:**, [Golam Sarwar](mailto:sarwar.golam@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The CMAQ model now contains detailed halogen (bromine and iodine) chemistry. Sarwar et al. (2015) examined the impacts of halogen chemistry both with and without the photolysis of higher iodine oxides. The inclusion of the photolysis of higher iodine oxides substantially reduces ozone and is not included in the model. The halogen chemistry without the photolysis of higher iodine is included in the model.

The halogen chemistry in CMAQ follows the description of Sarwar et al. (2015) with the following three changes:  
1. Rate constants of the several iodine reactions in Sarwar et al. (2015) contained special expressions which have been replaced with rate constants from Sherwen et al. (2016).  
2. Sarwar et al. (2015) calculated photolysis rates of halogen species using ratios of other chemical species following the Comprehensive Air quality Model with extensions (CAMx; https://www.camx.com/). These photolysis rates are now directly calculated using absorption cross-section and quantum yield data.  
3. Br2 emissions are a function of sea-salt production rates that are calculated in the aerosol module of CMAQ. This method is different from Sarwar et al. (2015) that calculated Br2 emissions independent of the sea-salt production rates in the aerosol module of CMAQ.  

## Significance and Impact

Halogen chemistry reduces mean ozone by 2-6 ppbv over seawater and 2-4 ppbv over some areas of land. The inclusion of the halogen chemistry increases model runtime by > 25%.


## Affected Files:

MECHS/cb05eh51_ae6_aq/AE_cb05eh51_ae6_aq.nml   
MECHS/cb05eh51_ae6_aq/CSQY_DATA_cb05eh51_ae6_aq  
MECHS/cb05eh51_ae6_aq/GC_cb05eh51_ae6_aq.nml  
MECHS/cb05eh51_ae6_aq/NR_cb05eh51_ae6_aq.nml  
MECHS/cb05eh51_ae6_aq/RXNS_DATA_MODULE.F90   
MECHS/cb05eh51_ae6_aq/RXNS_FUNC_MODULE.F90   
MECHS/cb05eh51_ae6_aq/Species_Table_TR_0.nml   
MECHS/cb05eh51_ae6_aq/mech_CB05eh51.def   
gas/ebi_cb05eh51_ae6_aq/hrdata_mod.F   
gas/ebi_cb05eh51_ae6_aq/hrdriver.F   
gas/ebi_cb05eh51_ae6_aq/hrg1.F   
gas/ebi_cb05eh51_ae6_aq/hrg2.F   
gas/ebi_cb05eh51_ae6_aq/hrg3.F   
gas/ebi_cb05eh51_ae6_aq/hrg4.F   
gas/ebi_cb05eh51_ae6_aq/hrinit.F   
gas/ebi_cb05eh51_ae6_aq/hrprodloss.F   
gas/ebi_cb05eh51_ae6_aq/hrrates.F     
gas/ebi_cb05eh51_ae6_aq/hrsolver.F   
emis/emis/MGEMIS.F   
emis/emis/SSEMIS.F   
emis/emis/EMIS_DEFN.F   
vdiff/acm2/ASX_DATA_MOD.F   
vdiff/acm2/vdiffproc.F   
aero/aero6/AEROSOL_CHEMISTRY.F   
aero/aero6/SOA_DEFN.F   
cloud/acm_ae6/hlconst.F   
depv/m3dry/DEPVVARS.F   
ICL/fixed/filenames/FILES_CTM.EXT    


## References:

Sarwar, et al. Impact of enhanced ozone deposition and halogen chemistry on tropospheric ozone over the Northern Hemisphere, Environmental Science & Technology, 49(15):9203-9211, 2015.   

Sherwen et al.  Iodine’s impact on tropospheric oxidants: a global model study in GEOS-Chem, Atmospheric Chemistry & Physics, 16, 1161–1186, 2016.  

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #30](https://github.com/usepa/cmaq_dev/pull/30)

### Commit IDs:
992729db506091be3ce80f5086d909e0ea15ae9f  
3dc45f1e9b2e9b35454ad51eb218e420fc57b701  
62e4165b45ef933f29b34d061e0a545c8cb8632e  
60647d3b104b09e2e0afa47f53fd7bb5083aa82a    
