# CMAQ Chemical Mechanism Processor
FORTRAN and c code that creates the RXNS modules for the CMAQ model version 5.2

This repository  contains the template bldrun script under script and code directory to generates the RXNS_DATA_MODULE.F90 and RXNS_FUNC_MODULE.F90 files
for CMAQ version 5.2.

To use this tool:

1) Compile the tool by modifying the bldit script in the scripts directory. Set Fortran compiler based on your system, save and run the script.

2) Modify the run script by setting the Mechanism that you are building.

3) Execute the run script and inspect the results under the output directory.

# Text from OGD Chapter 9:

### CMAQ Chemical Mechanisms

The CMAQ modeling system accounts for chemistry in three phases: a gas phase, aerosols (solid or liquid), and an aqueous phase. Refer to the release notes to find the gas‑phase chemistry mechanisms available in each version of CMAQ. Several variations of the base gas-phase mechanisms, with and without chlorine, mercury, and toxic species chemistry, are distributed with CMAQ. The modularity of CMAQ makes it possible to create or modify the gas-phase chemical mechanism.

Gas-phase chemical mechanisms are defined in CMAQ through Fortran source files. Located in subdirectories of the $CMAQ_MODEL/CCTM/src/MECHS directory (each correspond­ing to a mechanism name), these files define the source, reaction parameters, and atmospheric processes (e.g., diffusion, deposition, advection) of the various mechanism species. The species definitions for each mechanism are contained in namelist files that are read in during execution of the CMAQ programs. The CMAQ mechanism configuration is more similar to the science module configuration than to the horizontal grid or vertical layer configuration in that the mechanism is defined at compilation, resulting in executables that are hard-wired to a specific gas-phase mechanism. To change chemical mechanisms between simulations, a new executable that includes the desired mechanism configuration must be compiled.

### Using predefined chemical mechanisms

To select a predefined mechanism configuration in CMAQ, set the *Mechanism* variable in the build scripts to the name of one of the mechanism directories located under $CMAQ_MODEL/CCTM/src/MECHS. Refer to the [CMAQv5.2 Photochemical Mechanisms release notes](https://github.com/USEPA/CMAQ/blob/5.2/CCTM/docs/Release_Notes/CMAQv5.2_Mechanisms.md) for the list of mechanisms available in CMAQv5.2.

### Creating or modifying chemical mechanisms

Creating or modifying mechanisms in CMAQ requires the use of the CMAQ chemical mecha­nism compiler, CHEMMECH, to produce the required Fortran source (F90) and namelist files. CHEMMECH translates an ASCII mechanism listing to the F90 and namelist files required by CMAQ. Like all of the CMAQ preprocessors, CHEMMECH is a Fortran program that must be compiled prior to use. Distributed with a Makefile for compilation and run scripts for execution, CHEMMECH reads a mechanism definition (mech.def) file and outputs the mechanism F90 and namelist files. See Chapter 7 for a description of CHEMMECH.

To modify an existing mechanism, copy the mech.def file that is contained in one of the existing mechanism directories to a new directory and modify the mech.def file accordingly. Provide this modified mechanism definition file to CHEMMECH as input to produce the mechanism F90 and namleist files needed to compile CMAQ.  

To invoke this new mechanism in CMAQ, set the *Mechanism* variable in the CMAQ build scripts to the name of the new mechanism directory and compile new executables.

To create a new mechanism for CMAQ, follow a procedure similar to the above for modifying mechanisms. Use an existing mech.def file as a template to format the new mechanism for inclusion in CMAQ. After formatting the mechanism in the form of the mech.def file, provide this file as an input to CHEMMECH to create the required mechanism input files for CMAQ. Move the resulting mechanism files to a new directory under $CMAQ_MODEL/CCTM/src/MECHS. To invoke this new mechanism, set the *Mechanism* variable in the CMAQ build scripts to the name of the new mechanism directory and compile new executables.

### Using species namelist files

The species namelist files define the parameters of the gas, aerosol, non-reactive, and tracer species simulated by the model. The CMAQ programs read the namelist files during execution to define the sources and processes that impact the simulated concentrations of each of the model output species. The namelist files can be used to apply uniform scaling factors by model species for major model processes. For example, emissions of NO can be reduced by 50% across the board by applying a factor of 0.5 to the emissions scalar column of the gas-phase species namelist file. Similarly, the boundary conditions of O<sub>3</sub> can be increased by 50% by applying a factor of 1.5 to the boundary conditions scalar column of the gas-phase species namelist file.

See [Chapter 8](CMAQ_OGD_ch08_input_files.md) for a description of the format of the namelist file.

When mechanisms are modified or created in CMAQ, new namelist files must be created that include the new species in the mechanism. As described above, the program CHEMMECH will generate namelist files from a mech.def mechanism definition file.  Alternatively, existing namelist files can be used as templates to guide the manual creation of new files.

### Further information on chemical mechanisms

-   The same chemical mechanism must be used for CCTM and all of the mechanism-dependent input processors that are part of the CMAQ system.
-   The Euler Backward Iterative (EBI) chemistry solver is mechanism-dependent. If a chemical mechanism is modified, then new EBI solver source code must be generated based on the mechanism definition. The CMAQ utility program CREATE_EBI reads the output from CHEMMECH to generate new EBI solver source code. 
-   The Rosenbrock and SMVGEAR solvers are mechanism-independent choices of chemistry solvers for the CCTM.
-   When adding new species to CMAQ, it is important to check that the sources of these new species into the modeling domain are accounted for correctly in the mechanism definition files. If species are added to the domain through the emissions files, the namelist files that define the mechanism species must contain these new species.


#Text from OGD Chapter 7:
### Description

The program CHEMMECH generates mechanism source code files for all chemical mechanism-dependent CMAQ programs. Using an ASCII mechanism definition file as input, the Fortran program CHEMMECH creates all of the Fortran files that define the gas-phase chemical mechanisms for the CMAQ programs. The C-Shell script CSV2NML converts a comma-delimited text file that defines the processes (e.g., input as emissions, input through boundary conditions, transport, deposition) impacting the concentrations of each model species to a NAMELIST file for input to the CMAQ programs. In combination the Fortran source and NAMELIST files define chemical mechanisms in the CMAQ programs.

Implementing new mechanisms created by CHEMMECH and CSV2NML in the CMAQ programs is a two-step process. CHEMMECH generates the mechanism RXNS source files that must be used in the compilation of CMAQ source code into an executable. CSV2NML generates species NAMELIST files that are input to the CMAQ programs during execution. Care must be taken to ensure that the RXNS and NAMELIST files are consistent with each other in order to correctly update or implement new mechanisms in CMAQ.

CHEMMECH reads in a mechanism definition (mech.def) text file that lists the stoichiometry and kinetics of a photochemical reaction mechanism. The program converts the mech.def file to two RXNS files, RXNS_DATA_MODULE.F90 and RXNS_FUNC_MODULE.F90 that get compiled with the CMAQ source code into a new executable. The source files created by CHEMMECH must be manually moved to the correct directory location in the CMAQ source code directories to be available during compilation of the CMAQ programs. The mechanism files for CMAQ are found in $CMAQ_HOME/CCTM/src/MECHS/$Mechanism, where $Mechanism is the unique ID of a chemical mechanism (e.g., cb05e51_aq6_aq).

CSV2NML reads in a series of CSV files that define the processes that impact the concentrations of all of the CMAQ species. The CSV files are converted to NAMELIST files that are invoked at execution of the various CMAQ programs. Environment variables in the run scripts for ICON, BCON, and CCTM must be set to point to the NAMELIST files for a particular mechanism.

See [Chapter 9](CMAQ_OGD_ch09_grid_defn.md) for details on how to update existing mechanisms or create new mechanisms in CMAQ.

### Files, configuration, and environment variables

[Figure 7‑5](#Figure5-4) shows the input and output files and configuration options for CHEMMECH and CSV2NML. The full set of mechanism files required by the CMAQ programs is generated in two steps. In the first step, the program CHEMMECH is run with the mechanism definition file, mech.def, provided as input. The resulting RXNS files are then input to the CMAQ build scripts to compile CMAQ with a new chemical mechanism configuration. CSV2NML is used to convert the species definition files from CSV format to NAMELIST files. The NAMELIST files are used as inputs to the CMAQ programs ICON, BCON, or CCTM to define the processes that will impact each model species. Three NAMELIST files define the processes for gas-phase species (GC.nml), aerosol species (AE.nml), and nonreactive species (NR.nml).

<a id=Figure7-5></a>

![](./images/Figure7-5.png "Figure7-5.png")  
**Figure 7‑5. CHEMMECH and CSV2NML input and output files**

To implement a new mechanism in CMAQ, start with a mechanism definition (mech.def) file and CSV species files from an existing mechanism in the model. Edit the mech.def file to include the new reactions, species, and reaction rates and provide this new mech.def file as input to CHEMMECH. Edit the CSV species files to include the new species and provide these files as input to CSV2NML. Detailed examples of updating an existing mechanism and adding a new mechanism to CMAQ are provided in [Chapter 9](CMAQ_OGD_ch09_grid_defn.md). Neither CHEMMECH nor CSV2NML requires horizontal grid, vertical layer, or temporal settings.

#### CHEMMECH input files

<a id=Table7-9></a>

**Table 7-9. CHEMMECH input files**

|**File Name**|**Format**|**Description**|
|---------------|------|------------------------------------------------------|
|MCFL (mech.def)|ASCII|CMAQ mechanism definition file; photochemical mechanism listing with both mechanistic and kinetic information about all reactions that compose a chemical mechanism|

#### CHEMMECH output files

<a id=Table7-10></a>

**Table 7‑10. CHEMMECH output files**

|File Name|Format|Description|
|------------|----------|-----------------------------------------------------|
|RXCM.EXT|ASCII|Mechanism common INCLUDE file; lists all of the chemical mechanism variables and parameters|
|RXDT.EXT|ASCII|Mechanism data INCLUDE file; chemical mechanism definition formatted as DATA blocks to be read in as CMAQ source code|
|SPCS.EXT|ASCII|Species INCLUDE file; not used|

The location of the CHEMMECH output files is set in the run script by the variable Opath. To compile a version of the CMAQ programs that use the INCLUDE files created by CHEMMECH, these output INCLUDE files need to be moved to a new directory under the `$CMAQ_HOME/models/mechs/release` directory. Point the CMAQ build scripts to this new directory through the “Mechanism” variable.

### Compiling and Running

#### Compile Chemmech ####

To compile CHEMMECH, run the build script:

```
cd $CMAQ_HOME/UTIL/chemmech/scripts
./bldit_chemmech.csh [compiler] [version] |& tee bldit_chemmech.log
```

To port CHEMMECH to different compilers, change the compiler names, locations, and flags in the config_cmaq.csh script.

#### Run Chemmech ####

Set the run script settings according to the execution configuration variables described above. Run CHEMMECH using the following command:

```
cd $CMAQ_HOME/UTIL/chemmech/scripts
./run_chemmech.csh |& tee run_chemmech.log
```
