
### CMAQ Chemical Mechanisms

The CMAQ modeling system accounts for chemistry in three phases: a gas, aerosol, and cloud droplets. Several variations of the base gas-phase mechanisms, with and without chlorine, mercury, and toxic species chemistry, are distributed with CMAQ. Please consult the release notes for changes the mechanisms available in a specific version of CMAQ. 

Fortran modules and namelists define a chemical mechanisms for the CMAQ. Subdirectories of the $CMAQ_MODEL/CCTM/src/MECHS directory contain the files for each mechanism name. Namelists enable setting runtime options for a mechanism. Species namelists give their names, molecular weights and atmospheric processes (e.g., diffusion, cloud chemistry, deposition, advection). They also determine whether the concentrations and deposition values are written to output file. An emission control namelists define the emission inputs for model species. Two Fortran modules define the photochemistry for a mechanism. A data module specifies the reactions and parameters. A functions module initializes the photochemistry and calculates reaction rates constants. Because the two modules define photochemistry for a mechanism, an executable version of the CMAQ model has a fixed photochemistry. To modify or change it, requires modifying or replacing the modules.

### Using predefined chemical mechanisms

To select a predefined mechanism configuration in CMAQ, set the *Mechanism* variable in the build scripts to the name of one of the mechanism directories located under $CMAQ_MODEL/CCTM/src/MECHS. Refer to the [CMAQv5.2 Photochemical Mechanisms release notes](https://github.com/USEPA/CMAQ/blob/5.2/CCTM/docs/Release_Notes/CMAQv5.2_Mechanisms.md) for the list of mechanisms available in CMAQv5.2.

### Creating or modifying a mechanism's photochemistry

Editing the Fortran modules is one way to make simple changed to photochemistry. Complex changes or creating new photochemistry scheme requires 1) using the CMAQ chemical mechanism utility, CHEMMECH, to produce the new Fortran modules and creating new namelist with a text editor. The CHEMMECH utility translates an ASCII files lists reaction into the two Fortran module used by CMAQ. It is a Fortran program that must be compiled before using. For more information, consult the README file under $CMAQ_MODEL/UTIL/chemmech.

### Using species namelist files

Species namelist files define the parameters of the gas, aerosol, non-reactive, and tracer species simulated by the model. The CMAQ programs read the namelist files during execution to define the sources and processes that impact the simulated concentrations of each of the model output species. The namelist files can be used to apply uniform scaling factors by model species for major model processes. For example, emissions of NO can be reduced by 50% across the board by applying a factor of 0.5 to the emissions scalar column of the gas-phase species namelist file. Similarly, the boundary conditions of O<sub>3</sub> can be increased by 50% by applying a factor of 1.5 to the boundary conditions scalar column of the gas-phase species namelist file.


When mechanisms are modified or created in CMAQ, new namelist files must be created that include the new species in the mechanism. As described above, the program CHEMMECH will generate namelist files from a mech.def mechanism definition file.  Alternatively, existing namelist files can be used as templates to guide the manual creation of new files.

### Further information on chemical mechanisms

-   The same chemical mechanism must be used for CCTM and all of the mechanism-dependent input processors that are part of the CMAQ system.
-   The Euler Backward Iterative (EBI) chemistry solver is mechanism-dependent. If a chemical mechanism is modified, then new EBI solver source code must be generated based on the mechanism definition. The CMAQ utility program CREATE_EBI reads the output from CHEMMECH to generate new EBI solver source code. 
-   The Rosenbrock and SMVGEAR solvers are mechanism-independent choices of chemistry solvers for the CCTM.
-   When adding new species to CMAQ, it is important to check that the sources of these new species into the modeling domain are accounted for correctly in the mechanism definition files. If species are added to the domain through the emissions files, the namelist files that define the mechanism species must contain these new species.

