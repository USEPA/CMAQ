# Updates to the chlorine chemistry in CB6r3 in CMAQv5.3

[Deborah Luecken](mailto:luecken.deborah@epa.gov), U.S. Environmental Protection Agency

## Brief Description

Many of the reactions in the chlorine chemistry in CB6 have been updated for CMAQv5.3. Several new reactions have been added, including two heterogeneous reactions. Some reactions were updated either due to new information on reaction rates/products or changes in mechanism species. One new model species (chlorine nitrate) is added in CMAQv5.3. The chlorine chemistry in CMAQv5.3 is intended to be consistent with the chlorine chemistry in CAMx CB6r2 (Ramboll Environ, 2016). All versions of CB6 in CMAQv5.3, including the detailed halogen mechanism (*cb6r3m_ae7_kmtbr*), use the same chlorine chemistry.  An updated EBI solver is also developed for the revised mechanisms.  

* Reactions unchanged from CMAQv5.2:

      CL1, CL2, CL3, CL4, CL5, CL8, CL9, CL14-16, CL18-23, HET_H2NO3PIB, HET_H2NO3PJB, HET_H2NO3PKB 

* Reactions updated to match IUPAC, or CB6r2:

      CL6, CL10, CL11, CL13, CL17, CL24-27

* New reactions:

      CL7, CL12, CL28-31, HET_CLNO3_WAJ 
 
* Other new processes:

      Implemented deposition of ClO
      
## New Species
Chlorine nitrate (species CLNO3)



## Significance and Impact

Model sensitivity runs were conducted with the CMAQv5.2 and CMAQv5.3 versions of CB6r3 using a domain over the Northern Hemisphere for October 2015. The update to CB6r3 increases monthly mean ozone by <0.1&nbsp;ppbV compared to the previous version. The revision affects sulfate, nitrate, and ammonium only over a few isolated grid cells. The changes affect monthly mean sulfate by <0.03&nbsp;&#956;g&nbsp;m<sup>&#8209;3</sup>, nitrate by <0.06&nbsp;&#956;g&nbsp;m<sup>&#8209;3</sup>, and ammonium by <0.02&nbsp;&#956;g&nbsp;m<sup>&#8209;3</sup>. Overall, the impact of this change on model results is small.

## Files Affected

* CCTM/src/MECHS/cb6r3_ae7_aq/GC_cb6r3_ae7_aq.nml
* CCTM/src/MECHS/cb6r3_ae7_aq/RXNS_DATA_MODULE.F90
* CCTM/src/MECHS/cb6r3_ae7_aq/RXNS_FUNC_MODULE.F90
* CCTM/src/MECHS/cb6r3_ae7_aq/mech_cb6r3_ae7_aq.def
* CCTM/src/MECHS/cb6r3_ae7_aq/CSQY_DATA_cb6r3_ae7_aq

* CCTM/src/gas/ebi_cb6r3_ae7_aq/hrdriver.F
* CCTM/src/gas/ebi_cb6r3_ae7_aq/hrdata_mod.F
* CCTM/src/gas/ebi_cb6r3_ae7_aq/hrg1.F
* CCTM/src/gas/ebi_cb6r3_ae7_aq/hrg2.F
* CCTM/src/gas/ebi_cb6r3_ae7_aq/hrg3.F
* CCTM/src/gas/ebi_cb6r3_ae7_aq/hrg4.F
* CCTM/src/gas/ebi_cb6r3_ae7_aq/hrinit.F
* CCTM/src/gas/ebi_cb6r3_ae7_aq/hrprodloss.F
* CCTM/src/gas/ebi_cb6r3_ae7_aq/hrrates.F
* CCTM/src/gas/ebi_cb6r3_ae7_aq/hrsolver.F

* CCTM/src/MECHS/cb6r3_ae6_aq/GC_cb6r3_ae6_aq.nml
* CCTM/src/MECHS/cb6r3_ae6_aq/RXNS_DATA_MODULE.F90
* CCTM/src/MECHS/cb6r3_ae6_aq/RXNS_FUNC_MODULE.F90
* CCTM/src/MECHS/cb6r3_ae6_aq/mech_cb6r3_ae6_aq.def
* CCTM/src/MECHS/cb6r3_ae6_aq/CSQY_DATA_cb6r3_ae6_aq
* CCTM/src/gas/ebi_cb6r3_ae6_aq/hrdriver.F
* CCTM/src/gas/ebi_cb6r3_ae6_aq/hrdata_mod.F
* CCTM/src/gas/ebi_cb6r3_ae6_aq/hrg1.F
* CCTM/src/gas/ebi_cb6r3_ae6_aq/hrg2.F
* CCTM/src/gas/ebi_cb6r3_ae6_aq/hrg3.F
* CCTM/src/gas/ebi_cb6r3_ae6_aq/hrg4.F
* CCTM/src/gas/ebi_cb6r3_ae6_aq/hrinit.F
* CCTM/src/gas/ebi_cb6r3_ae6_aq/hrprodloss.F
* CCTM/src/gas/ebi_cb6r3_ae6_aq/hrrates.F
* CCTM/src/gas/ebi_cb6r3_ae6_aq/hrsolver.F

* CCTM/src/MECHS/cb6r3_ae7_aqkmt2/GC_cb6r3_ae7_aq.nml

## References

Dieber, G., George, C., Le Calve, S., Schweitzer, F., Mirabel, P., 2004.  Uptake study of ClONO<sub>2</sub> and BrONO<sub>2</sub> by Halide containing droplets, _Atmospheric Chemistry and Physics_, **4**, 1291-1299. 

IUPAC, 2017. Task Group on Atmospheric Chemical Kinetic Data Evaluation.  http://iupac.pole-ether.fr/ (accessed 10-2-17).

Ramboll Environ, 2016.  Users Guide Comprehensive Air Quality Model with Extensions, version 6.3, Novato, CA, http://www.camx.com/files/camxusersguide_v6-30.pdf

Yarwood, G., Rao, S., Yocke, M., Whitten, G.Z., 2005. Updates to the Carbon Bond Mechanism: CB05.  Final Report to the US EPA, RT-0400675. Yocke and Company, Novato, CA.

## Internal Records
#### Relevant Pull Requests:

[PR #359](https://github.com/USEPA/CMAQ_Dev/pull/359), [PR #342](https://github.com/USEPA/CMAQ_Dev/pull/342), [PR #447](https://github.com/USEPA/CMAQ_Dev/pull/447)



