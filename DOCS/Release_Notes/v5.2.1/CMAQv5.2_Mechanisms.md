# CMAQv5.2 Photochemical Mechanisms

**Author/P.O.C.:**, [Golam Sarwar] (mailto:sarwar.golam@epa.gov), National Exposure Research Laboratory, U.S. EPA

## Mechanism Definitions

Photochemical mechanisms define the chemical reactions that destroy or produce gas and aerosol species.  The modeled chemical species used in each mechanism are listed in three CMAQ files:
- GC namelist for the gas-phase species
- AE namelist for the aerosol species
- NR namelist for the non-reactive species
- TR namelist for tracer species

The photochemical mechanism does not define the reactions and species used in cloud chemistry.

The directories under $CMAQ_HOME/CCTM/src/MECHS contain files that define the chemical species and their reactions for each of the CMAQ chemistry mechanisms. Along with the GC, AE, and NR namelists, a set of other files are used to define the CMAQ mechanisms:
- The CSQY_DATA_*mechanism* file contains data used for in-line photolysis rates calculations
- The mech.def file lists the photochemical reactions.
- The RXNS.F90 files are machine-generated, Fortran source code implementations of the CMAQ mechanisms. The CMAQ utility CHEMMECH uses the mech.def file to generate the RXNS.F90 files for use in compiling the different CMAQ programs.
- The trac file contains the TR namelists for configuring CMAQ to run with inert tracer species.

## Chemistry Solvers

The directories under $CMAQ_HOME/CCTM/src/gas contain the Fortran source codes for numerical solvers to calculate species concentrations using the different CMAQ chemical mechanisms.

- The ros3 and smvgear solvers work for any mechanism and are based on Rosenbrock and Gear methods, respectively.
- The ebi_*mechanism* solver only works for the specified *mechanism*. They are optimized solvers based on a Backward Euler Iteration (EBI) method and analytical solutions.

## CMAQ v5.2 Mechanism Listing
The table below lists the chemistry mechanisms available in CMAQ v5.2.  The entries in the MECHS Module column correspond to the `Mechanism` setting in the CMAQ build scripts. The entries in this column link to documentation for each mechanism.   The next two columns in the table define the aerosol and cloud modules that are compatible with each mechanism. The last column links to the mechanism definition, showing the details of the stoichiometry and kinetics of each mechanism.

|**MECHS Module**|**Aerosol Module**|**Cloud Module**|**Mechanism Definition**|**Species Table**|
|---|---|---|---|---|
| [cb05e51_ae6_aq](https://www.airqualitymodeling.org/index.php/CMAQ_v5.1_CB05_updates) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_cb05e51_ae6_aq.def](../../src/MECHS/cb05e51_ae6_aq/mech_cb05e51_ae6_aq.def)|[species](../../../DOCS/User_Manual/Appendix_A/cb05e51_ae6_aq/cb05e51_ae6_aq_species_table.md)|
| [cb05e51_ae6nvPOA_aq](SemiVolPOA_pcSOA.md) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_cb05e51_ae6nvPOA_aq.def](../../src/MECHS/cb05e51_ae6nvPOA_aq/mech_cb05e51_ae6nvPOA_aq.def) | |
| [cb05eh51_ae6_aq](Halogen_Chemistry.md) | aero6 | acm_ae6 or acm_ae6_kmt| [mech_cb05eh51_ae6_aq.def](../../src/MECHS/cb05eh51_ae6_aq/mech_cb05eh51_ae6_aq.def) | |
| [cb05mp51_ae6_aq](https://www.airqualitymodeling.org/index.php/CMAQ_v5.1_cb05mp51_ae6_aq) | aero6 | acm_ae6_mp|[cb05mp51_ae6_aq.def](../../src/MECHS/cb05mp51_ae6_aq/cb05mp51_ae6_aq.def) | |
| cb05tucl_ae6_aq [v5.0](https://www.airqualitymodeling.org/index.php/CMAQv5.0_Chemistry_Notes#CB05TUCL) [v5.1](https://www.airqualitymodeling.org/index.php/CMAQ_v5.1_CB05_updates#CB05tucl_Mechanism_Updates) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_cb05tucl_ae6_aq.def](../../src/MECHS/cb05tucl_ae6_aq/mech_cb05tucl_ae6_aq.def) |[species](../../../DOCS/User_Manual/Appendix_A/cb05tucl/cb05tucl_species_table.md)|
| [cb05tump_ae6_aq](https://www.airqualitymodeling.org/index.php/CMAQ_v5.1_cb05tump_ae6_aq) | aero6 | acm_ae6_mp|[mech_cb05tump_ae6_aq.def](../../src/MECHS/cb05tump_ae6_aq/mech_cb05tump_ae6_aq.def) | |
| [cb6r3_ae6_aq](CB6_release_notes.md) | aero6 | acm_ae6 or acm_ae6_kmt| [mech_cb6r3_ae6_aq.def](../../src/MECHS/cb6r3_ae6_aq/mech_cb6r3_ae6_aq.def) |[species](../../../DOCS/User_Manual/Appendix_A/cb6r3_ae6_aq/CB6_species_table.md)|
| [cb6r3_ae6nvPOA_aq](SemiVolPOA_pcSOA.md) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_cb6r3_ae6nvPOA_aq.def](../../src/MECHS/cb6r3_ae6nvPOA_aq/mech_cb6r3_ae6nvPOA_aq.def) | |
| [racm2_ae6_aq](https://www.airqualitymodeling.org/index.php/CMAQv5.0.2_gas-phase_chemistry#New_Mechanism:_RACM2) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_racm2_ae6_aq.def](../../src/MECHS/racm2_ae6_aq/mech_racm2_ae6_aq.def) |[species](../../../DOCS/User_Manual/Appendix_A/racm2_ae6_aq/racm2_ae6_aq_species_table.md)|
| [saprc07tb_ae6_aq](https://www.airqualitymodeling.org/index.php/CMAQv5.0_Chemistry_Notes#SAPRC07T) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_saprc07tb_ae6_aq.def)](../../src/MECHS/saprc07tb_ae6_aq/mech_saprc07tb_ae6_aq.def) |[species](../../../DOCS/User_Manual/Appendix_A/saprc07tb_ae6_aq/saprc07tb_ae6_aq_species_table.md)|
| [saprc07tc_ae6_aq](https://www.airqualitymodeling.org/index.php/CMAQv5.0_Chemistry_Notes#SAPRC07T) | aero6 | acm_ae6 or acm_ae6_kmt| [mech_saprc07tc_ae6_aq.def](../../src/MECHS/saprc07tc_ae6_aq/mech_saprc07tc_ae6_aq.def) |[species](../../../DOCS/User_Manual/Appendix_A/saprc07tc_ae6_aq/saprc07tc_ae6_aq_species_table.md)|
| [saprc07tc_ae6nvPOA_aq](SemiVolPOA_pcSOA.md) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_saprc07tc_ae6nvPOA_aq.def](../../src/MECHS/saprc07tc_ae6nvPOA_aq/mech_saprc07tc_ae6nvPOA_aq.def) | |
| [saprc07tic_ae6i_aq](https://www.airqualitymodeling.org/index.php/CMAQ_v5.1_SAPRC07tic_AE6i) | aero6 | acm_ae6|[mech_saprc07tic_ae6i_aq.def](../../src/MECHS/saprc07tic_ae6i_aq/mech_saprc07tic_ae6i_aq.def) |[species](../../../DOCS/User_Manual/Appendix_A/saprc07tic_ae6i_aq/saprc07tic_ae6i_aq_species_table.md)|
| [saprc07tic_ae6i_aqkmti](https://www.airqualitymodeling.org/index.php/CMAQv5.1_Aqueous_Chemistry#Additional_options_associated_with_AQCHEM-KMT.28I.29) | aero6 | acm_ae6i_kmt|[mech_saprc07tic_ae6i_aqkmti.def](../../src/MECHS/saprc07tic_ae6i_aqkmti/mech_saprc07tic_ae6i_aq.def) | |
| [saprc07tic_ae6invPOA_aq](SemiVolPOA_pcSOA.md) | aero6 | acm_ae6 or acm_ae6_kmt|[mech_saprc07tic_ae6invPOA_aq.def](../../src/MECHS/saprc07tic_ae6invPOA_aq/mech_saprc07tic_ae6invPOA_aq.def) | |
