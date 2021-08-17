# CMAQ User's Guide


The CMAQ User's Guide is designed to support the installation, configuration, and execution of the [Community Multiscale Air Quality (CMAQ)](http://www.epa.gov/cmaq) model on Linux systems. CMAQ users should be comfortable with Linux scripting conventions and have some familiarity with the Fortran programming language. Users should also have some familiarity with atmospheric structure, and the physical and chemical processes that occur in the atmosphere. 

Note: While this User's Guide is packaged with the code when it is downloaded or cloned, users are encouraged to go [online](https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/README.md) for the latest version.

# Table of Contents

[CMAQ Tutorials:](Tutorials/README.md) A series of short tutorials provide practical examples of how to set up and run CMAQ. Tutorials include: 
* Running the CMAQ Test Case
* Running the CMAQ-ISAM Test Case
* Building CMAQ for GNU and Intel
* Building WRF-CMAQ for GNU
* Debugging tips
* Prescribing Emissions Using the DESID Module 
* Creating an OCEAN File
* Adding an Inert Tracer Species
* Creating IC/BCs from Seasonal Average Hemispheric CMAQ Output
* Modifying a CMAQ Chemical Mechanism

[Chapter 1 (Overview):](CMAQ_UG_ch01_overview.md) CMAQ background, features, requirements and support resources.

[Chapter 2 (Program Structure):](CMAQ_UG_ch02_program_structure.md) Overview of the programs included with the CMAQ system.

[Chapter 3 (Preparing Compute Environment):](CMAQ_UG_ch03_preparing_compute_environment.md) Hardware/software requirements for CMAQ default configuration.

[Chapter 4 (Model Inputs):](CMAQ_UG_ch04_model_inputs.md) Preprocessing tools included with the base code and descriptions of the CMAQ input files. 

[Chapter 5 (Running CMAQ):](CMAQ_UG_ch05_running_a_simulation.md) Obtain the CMAQ source codes, set-up your environment, run a simulation.

[Chapter 6 (Model Configuration Options):](CMAQ_UG_ch06_model_configuration_options.md) Configuration options for different CMAQ science modules.

[Chapter 7 (Model Outputs):](CMAQ_UG_ch07_model_outputs.md) Descriptions of the CMAQ output files.

[Chapter 8 (Analysis Tools):](CMAQ_UG_ch08_analysis_tools.md) Post-processing, visualization and evaluation tools for CMAQ.

[Chapter 9 (Process Analysis):](CMAQ_UG_ch09_process_analysis.md) Instrumented Models - Integrated Process Rates (IPR) and Integrated Reaction Rates (IRR).

[Chapter 10 (HDDM-3D):](CMAQ_UG_ch10_HDDM-3D.md) Instrumented Models - Decoupled Direct Method in Three Dimensions (DDM-3D).

[Chapter 11 (ISAM):](CMAQ_UG_ch11_ISAM.md) Instrumented Models - Integrated Source Apportionment Method (ISAM).

[Chapter 12 (Sulfur Tracking):](CMAQ_UG_ch12_sulfur_tracking.md) Instrumented Models - Sulfur Tracking Method (STM).

[Chapter 13 (WRF-CMAQ):](CMAQ_UG_ch13_WRF-CMAQ.md) WRF-CMAQ model for simulating interactions between chemistry and weather.

[Tables and Figures:](CMAQ_UG_tables_figures.md) List of Tables and Figures.

[Appendix A (Model Options):](Appendix/CMAQ_UG_appendixA_model_options.md) List of model options in configuration, compilation and run scripts.

[Appendix B (Emissions Control):](Appendix/CMAQ_UG_appendixB_emissions_control.md) How to use the Emissions Control Namelist for customization of emissions processing.

[Appendix C (Spatial Data):](Appendix/CMAQ_UG_appendixC_spatial_data.md) Information on how to create consistent geospatial data for CMAQ inputs.

[Appendix D (Parallel Implementation):](Appendix/CMAQ_UG_appendixD_parallel_implementation.md) An introduction to how data-parallelism can be applied in the CMAQ system to increase computational efficiency. 

[Appendix E (Configuring WRF):](Appendix/CMAQ_UG_appendixE_configuring_WRF.md) Configuring the Weather Research and Forecasting Model (WRF) for use with CMAQ

[Appendix F (Importing bugfixes):](Appendix/CMAQ_UG_appendixF_importing_bugfixes.md) Directions on importing bugfixes from CMAQ GitHub Issues page

***

CMAQ User's Guide (c) 2021<br>
