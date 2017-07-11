<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch13_support.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_glossary.md)

<!-- END COMMENT -->

* * *

## Appendix A: CMAQ v5.2 Mechanism Table 
The table below lists the chemistry mechanisms available in CMAQ v5.2.  The entries in the MECHS Module column correspond to the `Mechanism` setting in the CMAQ build scripts. The entries in this column link to documentation for each mechanism.   The next two columns in the table define the aerosol and cloud modules that are compatible with each mechanism. The fourth column links to the mechanism definition, showing the details of the stoichiometry and kinetics of each mechanism, the last column links to the species tables from Appendix A.  See the [CMAQv5.2 release notes](https://github.com/USEPA/CMAQ/blob/5.2/CCTM/docs/Release_Notes/CMAQv5.2_Mechanisms.md) for additional information on photochemical mechanisms in CMAQ. 

|**MECHS Module**|**Aerosol Module**|**Cloud Module**|**Mechanism Definition**|**Species Table**|
|----------------------------------------------------------|------------------|-----------------------|--------------------|---------------------------|
| [cb05e51_ae6_aq](https://www.airqualitymodeling.org/index.php/CMAQ_v5.1_CB05_updates) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_cb05e51_ae6_aq.def](../../CCTM/src/MECHS/cb05e51_ae6_aq/mech_cb05e51_ae6_aq.def)|[**Table A1**](./Appendix_A/cb05e51_ae6_aq/cb05e51_ae6_aq_species_table.md)|
| [cb05e51_ae6nvPOA_aq](SemiVolPOA_pcSOA.md) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_cb05e51_ae6nvPOA_aq.def](../../CCTM/src/MECHS/cb05e51_ae6nvPOA_aq/mech_cb05e51_ae6nvPOA_aq.def) | |
| [cb05eh51_ae6_aq](Halogen_Chemistry.md) | aero6 | acm_ae6 or acm_ae6_kmt| [mech_cb05eh51_ae6_aq.def](../../CCTM/src/MECHS/cb05eh51_ae6_aq/mech_cb05eh51_ae6_aq.def) | |
| [cb05mp51_ae6_aq](https://www.airqualitymodeling.org/index.php/CMAQ_v5.1_cb05mp51_ae6_aq) | aero6 | acm_ae6_mp|[mech_cb05mp51_ae6_aq.def](../../CCTM/src/MECHS/cb05mp51_ae6_aq/mech_cb05mp51_ae6_aq.def) | |
| cb05tucl_ae6_aq [v5.0](https://www.airqualitymodeling.org/index.php/CMAQv5.0_Chemistry_Notes#CB05TUCL) [v5.1](https://www.airqualitymodeling.org/index.php/CMAQ_v5.1_CB05_updates#CB05tucl_Mechanism_Updates) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_cb05tucl_ae6_aq.def](../../CCTM/src/MECHS/cb05tucl_ae6_aq/mech_cb05tucl_ae6_aq.def) |[**Table A2**](./Appendix_A/cb05tucl/cb05tucl_species_table.md)|
| [cb6r3_ae6_aq](CB6_release_notes.md) | aero6 | acm_ae6 or acm_ae6_kmt| [mech_cb6r3_ae6_aq.def](../../CCTM/src/MECHS/cb6r3_ae6_aq/mech_cb6r3_ae6_aq.def) |[**Table A3**](./Appendix_A/cb6r3_ae6_aq/CB6_species_table.md)|
| [cb6r3_ae6nvPOA_aq](SemiVolPOA_pcSOA.md) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_cb6r3_ae6nvPOA_aq.def](../../CCTM/src/MECHS/cb6r3_ae6nvPOA_aq/mech_cb6r3_ae6nvPOA_aq.def) | |
| [racm2_ae6_aq](https://www.airqualitymodeling.org/index.php/CMAQv5.0.2_gas-phase_chemistry#New_Mechanism:_RACM2) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_racm2_ae6_aq.def](../../CCTM/src/MECHS/racm2_ae6_aq/mech_racm2_ae6_aq.def) |[**Table A4**](./Appendix_A/racm2_ae6_aq/racm2_ae6_aq_species_table.md)|
| [saprc07tb_ae6_aq](https://www.airqualitymodeling.org/index.php/CMAQv5.0_Chemistry_Notes#SAPRC07T) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_saprc07tb_ae6_aq.def](../../CCTM/src/MECHS/saprc07tb_ae6_aq/mech_saprc07tb_ae6_aq.def) |[**Table A5**](./Appendix_A/saprc07tb_ae6_aq/saprc07tb_ae6_aq_species_table.md)|
| [saprc07tc_ae6_aq](https://www.airqualitymodeling.org/index.php/CMAQv5.0_Chemistry_Notes#SAPRC07T) | aero6 | acm_ae6 or acm_ae6_kmt| [mech_saprc07tc_ae6_aq.def](../../CCTM/src/MECHS/saprc07tc_ae6_aq/mech_saprc07tc_ae6_aq.def) |[**Table A6**](./Appendix_A/saprc07tc_ae6_aq/saprc07tc_ae6_aq_species_table.md)|
| [saprc07tc_ae6nvPOA_aq](SemiVolPOA_pcSOA.md) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_saprc07tc_ae6nvPOA_aq.def](../../CCTM/src/MECHS/saprc07tc_ae6nvPOA_aq/mech_saprc07tc_ae6nvPOA_aq.def) | |
| [saprc07tic_ae6i_aq](https://www.airqualitymodeling.org/index.php/CMAQ_v5.1_SAPRC07tic_AE6i) | aero6 | acm_ae6|[mech_saprc07tic_ae6i_aq.def](../../CCTM/src/MECHS/saprc07tic_ae6i_aq/mech_saprc07tic_ae6i_aq.def) |[**Table A7**](./Appendix_A/saprc07tic_ae6i_aq/saprc07tic_ae6i_aq_species_table.md)|
| [saprc07tic_ae6i_aqkmti](https://www.airqualitymodeling.org/index.php/CMAQv5.1_Aqueous_Chemistry#Additional_options_associated_with_AQCHEM-KMT.28I.29) | aero6 | acm_ae6i_kmt|[mech_saprc07tic_ae6i_aqkmti.def](../../CCTM/src/MECHS/saprc07tic_ae6i_aqkmti/mech_saprc07tic_ae6i_aq.def) | |
| [saprc07tic_ae6invPOA_aq](SemiVolPOA_pcSOA.md) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_saprc07tic_ae6invPOA_aq.def](../../CCTM/src/MECHS/saprc07tic_ae6invPOA_aq/mech_saprc07tic_ae6invPOA_aq.def) | |
