
### CMAQ Chemical Mechanisms

The CMAQ modeling system accounts for chemistry in three phases: a gas, aerosol, and cloud droplets. Several variations of the base gas-phase mechanisms, with and without chlorine, mercury, and toxic species chemistry, are distributed with CMAQ. Please consult the release notes for changes the mechanisms available in a specific version of CMAQ. 

Fortran modules and namelists define a chemical mechanisms for the CMAQ. Subdirectories of the $CMAQ_MODEL/CCTM/src/MECHS directory contain the files for each mechanism name. Namelists enable setting runtime options for a mechanism. Species namelists give their names, molecular weights and atmospheric processes (e.g., diffusion, cloud chemistry, deposition, advection). They also determine whether the concentrations and deposition values are written to output file. Emission control namelists define the emission inputs for model species. Two Fortran modules define the photochemistry for a mechanism. A data module specifies the reactions and parameters. A functions module initializes the photochemistry and calculates reaction rates constants. Because the two modules define photochemistry for a mechanism, an executable version of the CMAQ model has a fixed photochemistry. 
To modify or change photochemistry, requires modifying or replacing the modules then recompiling the model. The approach may not work because data hardcoded within the photochemistry solver changed, _if using an Euler Backward Interative (EBI) solver_, or because data used to calculate photolysis rates changed.

### Using predefined chemical mechanisms

To select a predefined mechanism configuration in CMAQ, set the *Mechanism* variable in the build scripts to the name of one of the mechanism directories located under $CMAQ_MODEL/CCTM/src/MECHS. The below table lists mechanisms available in this version of the CMAQ model.


|**Mechanism Name** | **Photochemistry**                                   | **Model Species<sup>1</sup>**    | **Cloud Chemistry Module<sup>2</sup>** |
| ----------------- | ---------------------------------------------------- | -------------------- | ---------------------- |
| cb6r3_ae7_aq      | [Carbon Bond 6 version r3 with aero7 treatment of SOA](mechanism_information/cb6r3_ae7_aq/mech_cb6r3_ae7_aq.md) |  [species table 1](mechanism_information/cb6r3_ae7_aq/cb6r3_ae7_aq_species_table.md)                 | acm_ae7          |
| cb6r3_ae7_kmt2    | [Carbon Bond 6 version r3 with aero7 treatment of SOA](mechanism_information/cb6r3_ae7_aq/mech_cb6r3_ae7_aqkmt2.md) | [species table 1](mechanism_information/cb6r3_ae7_aq/cb6r3_ae7_aqkmt2_species_table.md)                   | acm_ae7_kmt2          |
| cb6r3m_ae7_kmtbr  | [Carbon Bond 6 version r3 with aero7 treatment of SOA and DMS and marine halogen chemistry](mechanism_information/cb6r3_ae7_aq/mech_cb6r3m_ae7_aqkmtbr.md) | [species table 2](mechanism_information/cb6r3m_ae7_kmtbr/cb6r3m_ae7_kmtbr_species_table.md)                   | acm_ae7_kmtbr          |
| cb6r3_ae6_aq      | [Carbon Bond 6 version r3 with aero6 treatment of SOA](mechanism_information/cb6r3_ae7_aq/mech_cb6r3_ae6_aq.md) | [species table 3](mechanism_information/cb6r3_ae6_aq/cb6r3_ae6_aq_species_table.md)                   | acm_ae6          |
| cb6mp_ae6_aq      | [Carbon Bond 6 version r3 with air toxics and aero6 treatment of SOA](mechanism_information/cb6r3_ae7_aq/mech_cb6mp_ae6_aq.md) | [species table 4](mechanism_information/cb6mp_ae6_aq/cb6mp_ae6_aq_species_table.md)                   | acm_ae6          |
| racm2_ae6_aq      | [Regional Atmospheric Chemistry Mechanism version 2 with aero6 treatment of SOA](mechanism_information/racm2_ae6_aq/mech_racm2_ae6_aq.md) | [species table 5](mechanism_information/racm2_ae6_aq/racm2_ae6_aq_species_table.md)                   | acm_ae6          |
| saprc07tic_ae7i_aq | [State Air Pollution Research Center version 07tc with extended isoprene chemistry and aero7i treatment of SOA]( mechanism_information/saprc07tic_ae7i_aq/mech_saprc07tic_ae7i_aq.md) | [species table 6](mechanism_information/saprc07tic_ae7i_aq/saprc07tic_ae7i_aq_species_table.md)                   | acm_ae7          |
| saprc07tic_ae7i_aqkmt2 | [State Air Pollution Research Center version 07tc with extended isoprene chemistry and aero7i treatment of SOA](mechanism_information/saprc07tic_ae7i_aqkmt2/mech_saprc07tic_ae7i_aqkmt2.md)  | [species table 6](mechanism_information/saprc07tic_ae7i_aqkmt2/saprc07tic_ae7i_aqkmt2_species_table.md)                   | acm_ae7_kmt2          |
| saprc07tic_ae6i_aq | [State Air Pollution Research Center version 07tc with extended isoprene chemistry and aero6i treatment of SOA]( mechanism_information/saprc07tic_ae6i_aq/mech_saprc07tic_ae6i_aq.md) | [species table 7](mechanism_information/saprc07tic_ae6i_aq/saprc07tic_ae6i_aq_species_table.md)                   | acm_ae6          |
| saprc07tic_ae6i_aqkmti | [State Air Pollution Research Center version 07tc with extended isoprene chemistry and aero6i treatment of SOA](mechanism_information/saprc07tic_ae6i_aqkmti/mech_saprc07tic_ae6i_aqkmti.md)  | [species table 7](mechanism_information/saprc07tic_ae6i_aqkmti/saprc07tic_ae6i_aqkmti_species_table.md)                   | acm_ae6_kmti          |
| saprc07tc_ae6_aq | [State Air Pollution Research Center version 07tc with aero6 treatment of SOA](mechanism_information/saprc07tic_ae6i_aqkmti/mech_saprc07tc_ae6_aq.md)  | [species table 8](mechanism_information/saprc07tc_ae6_aq/saprc07tc_ae6_aq_species_table.md)                   | acm_ae6         |       

1. mechanisms can share the same model species but differ cloud chemistry
2. _kmt_  and _acm_ refers to the kinetic mass transfer to cloud droplets and the convective cloud/transport representation, respectively


### Creating or modifying a mechanism's photochemistry

Editing the Fortran modules is one way to make simple changes to photochemistry scheme. Complex changes or creating a new scheme requires 1) creating new namelists with a text editor (_if adding new model species_) and 2) using the CMAQ chemical mechanism utility, CHEMMECH, to produce the new Fortran modules. The CHEMMECH utility translates an ASCII file listing reactions for photochemistry into the Fortran modules used by CMAQ. For more information, consult the README file under $CMAQ_MODEL/UTIL/chemmech.
Creating new mechanism modules may not be the last steps for the CMAQ model to use the photochemistry update. If changes add a new photolysis rate(s), the inline_phot_preproc or jproc utility has to create CMAQ input file(s) for the photolysis module used. If CMAQ is using an EBI solver for photochemistry, the create_ebi utility has to be used to create a new solver. These three utilities use the mechanism data module produced by the CHEMMECH utility.  

### Using species namelist files

Species namelists define the four groups of model species: gas, aerosol, non-reactive, and tracer species simulated by the model.
The CMAQ model reads namelists to define processes that impact simulated concentrations. Species namelists can be used to apply uniform scaling factors by model species for several physical processes. Dry deposition of NO can be reduced by 50% by applying a factor of 0.5 to the dry deposition velocity for NO. Similarly, the boundary conditions of O<sub>3</sub> can be increased by 50% by applying a factor of 1.5. The gas, aerosol, and non-reactive define a specific mechanism. 
A tracer namelist is generally interchangable between mechanisms. They are employed in transport and deposition studies. 
Example tracer namelists are under $CMAQ_MODEL/CCTM/src/MECHS/trac0 (_the version most often used_) and $CMAQ_MODEL/CCTM/src/MECHS/trac1.

### Points to emphasize on chemical mechanisms

-   The Euler Backward Iterative (EBI) solver for photochemistry is hardcoded to the Fortran data module representing photochemistry and specific names in the species namelists. If either change, a new or different EBI solver source code is needed.
-   The Rosenbrock and SMVGEAR photochemistry solvers are not hardcoded the above files so they are more easily allow changing these files.

